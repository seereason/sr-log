{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}

module SeeReason.Log
  ( -- * Configuration
    LogConfig(LogConfig, loggerConfig)
  , LoggerName(LoggerName)
  , LoggerConfig(LoggerConfig, logStack, logLevel)
  , LoggerSettings(..)
  , defaultLoggerConfig
  , defaultLogConfig

  , logger, loggerName, loggerLoc

    -- * Logging
  , alog
  , alogDrop
  , alogN
  , clog
  , logString

    -- * Locations
  , loc
  , srclocList
  , srcloc
  , srcloccol
  , srcfunloc
  , srclocs
  , putStrLnLoc
  , putStrLnLocs

    -- * Stack
  , getStack
  , compactStack
  , compactStackWith
  , parentFunc

  , thisFunction
  , thisLocation, ici
  , thisLocation'
  , thisLocation''

  , dropModuleFrames, dropPackageFrames

    -- * Elapsed time
#if 0
  , HasSavedTime(..)
  , alogElapsed
#endif
    -- * Re-exports
  , Priority(..)
  ) where

import Control.Lens ((.=), Lens', ix, preview, to, use, view)
import Control.Monad.Except (when)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Bool (bool)
import Data.Data (Data)
import Data.Default (Default(def))
import Data.List as List (intercalate, intersperse, isSuffixOf, uncons)
import Data.Map as Map (Map)
import Data.Maybe (fromMaybe)
import Data.SafeCopy (SafeCopy, safeGet, safePut)
import Data.Serialize (Serialize(get, put))
import Data.String (IsString(fromString))
import Data.Text (pack, Text)
import Data.Time (diffUTCTime, getCurrentTime, UTCTime)
import Data.Typeable (Typeable)
-- import Debug.Trace (trace)
import Extra.Lens (HasLens(hasLens))
import GHC.Generics (Generic)
import GHC.Stack (CallStack, callStack, fromCallSiteList, getCallStack, HasCallStack, prettyCallStack, SrcLoc(..))
import SeeReason.LogOrphans ()
import System.Log.Logger ({-getLevel,-} getLogger, {-getRootLogger,-} Logger, logL, Priority(..), rootLoggerName)
import Text.PrettyPrint.HughesPJClass (prettyShow)
import Text.Printf (printf)

import qualified Data.Function as Fn ((&))

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup((<>)))
#endif
#if MIN_VERSION_time(1,9,0)
import Data.Time.Format (formatTime, defaultTimeLocale)
#endif

mintercalate :: Monoid s => s -> [s] -> s
mintercalate x xs = mconcat (intersperse x xs)

-- | Verbosely format the location of the nth level up in a call stack
prettyLocN :: CallStack -> Int -> Maybe String
prettyLocN stack n = preview (to getCallStack . ix n . to (prettyLoc . snd)) stack

prettyLoc :: SrcLoc -> String
prettyLoc = prettyShow

topLoc :: (IsString s, Monoid s, HasCallStack) => s
topLoc = compactStack (take 2 $ dropModuleFrames $ getStack)

topLocs :: (IsString s, Monoid s, HasCallStack) => Int -> s
topLocs n = compactStack (take (n + 2) $ dropModuleFrames $ getStack)

putStrLnLoc :: (MonadIO m, HasCallStack) => String -> m ()
putStrLnLoc msg = liftIO $ putStrLn (msg <> " (" <> topLoc <> ")")

putStrLnLocs :: (MonadIO m, HasCallStack) => Int -> String -> m ()
putStrLnLocs n msg = liftIO $ putStrLn (msg <> " (" <> topLocs n <> ")")

-- | Compactly format a call stack.
srclocs :: (IsString s, Monoid s) => CallStack -> s
-- The space before the arrow allows the console to add line breaks.
-- The space after is omitted to make these line breaks more
-- consistent.
srclocs = mintercalate (fromString " →") . srclocList

-- | List of more compactly pretty printed CallStack location.
-- Reversed so main comes first.
srclocList :: IsString s => CallStack -> [s]
srclocList = fmap (fromString . srcloc . snd) . reverse . getCallStack

-- | Compactly format a source location
srcloc :: (IsString s, Semigroup s) => SrcLoc -> s
srcloc l = fromString (srcLocModule l) <> ":" <> fromString (show (srcLocStartLine l))

-- | Compactly format a source location with a function name
srcfunloc :: (IsString s, Semigroup s) => SrcLoc -> s -> s
srcfunloc l f = fromString (srcLocModule l) <> "." <> f <> ":" <> fromString (show (srcLocStartLine l))

-- | With start column
srcloccol :: (IsString s, Semigroup s) => SrcLoc -> s
srcloccol l = srcloc l <> ":" <> fromString (show (srcLocStartCol l))

-- | Drop the first element of a call stack and all subsequent frames
-- from the same module.
dropModuleFrames :: [(String, SrcLoc)] -> [(String, SrcLoc)]
dropModuleFrames [] = []
dropModuleFrames (frame1 : frames) =
  dropWhile (\frame ->
               srcLocPackage (snd frame) == srcLocPackage (snd frame1) &&
               srcLocModule (snd frame) == srcLocModule (snd frame1)) frames

-- | Drop the first element of a call stack and all subsequent frames
-- from the same package.  Don't use this in the interpeter, the
-- package is always main.
dropPackageFrames :: [(String, SrcLoc)] -> [(String, SrcLoc)]
dropPackageFrames [] = []
dropPackageFrames (frame1 : frames) =
  dropWhile (\frame ->
               srcLocPackage (snd frame) == srcLocPackage (snd frame1)) frames

-- | The first element of the result will be the call to 'getStack' and
-- the location from which it was called.
getStack :: HasCallStack => [(String, SrcLoc)]
getStack = getCallStack callStack

-- | Stack with main last.  Bottom frame includes the function name.
-- Top frame includes the column number.
compactStack :: forall s. (IsString s, Monoid s) => [(String, SrcLoc)] -> s
compactStack = mconcat . intersperse (" < " :: s) . compactLocs

compactLocs :: forall s. (IsString s, Monoid s) => [(String, SrcLoc)] -> [s]
compactLocs [] = ["(no CallStack)"]
compactLocs [(callee, l)] = [fromString callee, srcloccol l]
compactLocs [(_, l), (caller, _)] = [srcloccol l <> "." <> fromString caller]
compactLocs ((_, l) : more@((caller, _) : _)) =
  srcfunloc l (fromString caller) : stacktail (fmap snd more)
  where
    stacktail :: [SrcLoc] -> [s]
    stacktail [] = []
    -- Include the column number of the last item, it may help to
    -- figure out which caller is missing the HasCallStack constraint.
    stacktail [l'] = [srcloccol l']
    stacktail (l' : more') = srcloc l' : stacktail more'

logString :: HasCallStack => ([(String, SrcLoc)] -> [(String, SrcLoc)]) -> String -> String
logString fn msg =
  pre <> take (lim - len) msg <> suf
  where
    len = length pre + length suf
    callSite = take 2 allLocs
    pre = compactStack callSite <> " - "
    suf = if locs /= callSite then formattedStack locs else ""
    locs :: [(String, SrcLoc)]
    locs = fn allLocs
    allLocs :: [(String, SrcLoc)]
    allLocs = dropModuleFrames {-SrcLoc-} getStack
    lim =
#if defined(darwin_HOST_OS)
          2002
#else
          60000
#endif

formattedStack :: [(String, SrcLoc)] -> String
formattedStack locs =
  "\n    (" <> intercalate "\n     " stk <> ")"
  where
    stk = fmap (intercalate " > ") (groupsOf 5 (compactLocs (reverse locs)))

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n l =
  case splitAt n l of
    ([], []) -> []
    (l', l'') -> l' : groupsOf n l''

logStringOld  :: UTCTime -> UTCTime -> Priority -> String -> String
logStringOld prev time priority msg =
#if defined(darwin_HOST_OS)
  take 2002 $
#else
  take 60000 $
#endif
    unwords $ [timestring, fromMaybe "???" (prettyLocN callStack 1), "-", msg] <> bool [] ["(" <> show priority <> ")"] (priority == DEBUG)
    where timestring =
#if MIN_VERSION_time(1,9,0)
            formatTime defaultTimeLocale "%T%4Q"
#else
            (("elapsed: " <>) . (printf "%.04f" :: Double -> String) . fromRational . toRational)
#endif
              (diffUTCTime time prev)

type FunctionName = String
type Locs = [(FunctionName, SrcLoc)]

newtype LoggerName = LoggerName Text deriving (Generic, Eq, Ord, Show, Typeable, Data)
instance Serialize LoggerName where get = safeGet; put = safePut
instance SafeCopy LoggerName
instance IsString LoggerName where
  fromString = LoggerName . pack

data LogConfig =
  LogConfig
  { loggerConfig :: Map LoggerName LoggerConfig
  } deriving (Generic, Show, Eq, Ord, Data, Typeable)

instance Serialize LogConfig where get = safeGet; put = safePut
instance SafeCopy LogConfig

data LoggerConfig =
  LoggerConfig
  { logLevel :: Priority
  , logStack :: Bool -- ^ Include a compact stack trace
  , logLimit :: Maybe Int -- ^ Truncate messages at this length
  } deriving (Generic, Show, Eq, Ord, Data, Typeable)

instance Serialize LoggerConfig where get = safeGet; put = safePut
instance SafeCopy LoggerConfig

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig {loggerConfig = [(fromString rootLoggerName, defaultLoggerConfig)]}

defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig {logLevel = WARNING, logStack = False, logLimit = Just 400}

-- | Log message with a multi-line log stack on the next line
alogDrop :: (MonadIO m, HasCallStack) => (Locs -> Locs) -> Priority -> String -> m ()
alogDrop fn priority msg = do
  -- time <- getCurrentTime
  -- (liftIO . putStrLn . ("loggerLoc: " <>)) $ loggerLoc
  -- (liftIO . putStrLn . ("loggerName: " <>)) $ loggerName
  -- liftIO $ (putStrLn . ("level=" <>) . show . getLevel) =<< logger
  l <- liftIO logger
  liftIO $ logL l priority (logString fn msg)

data LoggerSettings =
  LoggerSettings
  { -- for clog and mlog
    showCallStack :: Bool
  , showFullStack :: Bool
#if 0
  , mapStack :: Locs -> Locs
#endif
    -- for mlog
  , saveTime :: Bool
  , showElapsed :: Bool
  , savedTime :: Maybe UTCTime
  } deriving Generic

instance Default LoggerSettings where
  def = LoggerSettings { showCallStack = False
                       , showFullStack = False
                       -- , mapStack = id
                       , saveTime = False
                       , showElapsed = False -- required MonadState
                       , savedTime = Nothing
                       }

-- | Normal log message with location
alog :: (MonadIO m, HasCallStack) => Priority -> String -> m ()
alog priority msg | priority >= WARNING = alogWithStack priority msg
alog priority msg = alogDrop (take 2) priority msg

instance HasLens LoggerSettings LoggerSettings where hasLens = id

-- | Log function with access to a settings record.
clog :: (MonadIO m, ?settings :: settings, HasLens settings LoggerSettings, HasCallStack) => Priority -> String -> m ()
clog priority msg | view (hasLens . to showFullStack) ?settings = alogWithStack priority msg
clog priority msg | view (hasLens . to showCallStack) ?settings = alogDrop id priority msg
clog priority msg | priority >= WARNING = alogDrop id priority msg
clog priority msg = alogDrop (take 2) priority msg

mlog :: forall s m. (MonadIO m, MonadState s m, HasLens s LoggerSettings, HasCallStack) => Priority -> String -> m ()
mlog priority msg = do
  let lns :: Lens' s LoggerSettings
      lns = hasLens
  loggerSettings <- use hasLens
  when (saveTime loggerSettings) $ do
         now <- liftIO getCurrentTime
         lns . #savedTime .= Just now
  when (showElapsed loggerSettings) $ do
    prev <- use (lns . #savedTime)
    now <- liftIO getCurrentTime
    lns . #savedTime .= Just now
    settings <- use lns
    let ?settings = settings
    clog priority
      (msg <>
       case (showElapsed loggerSettings, fmap (diffUTCTime now) prev) of
         (True, Just t) ->
           " (elapsed: " <>
#if MIN_VERSION_time(1,9,0)
           formatTime defaultTimeLocale "%T%4Q"
#else
           ((printf "%.04f" :: Double -> String) . fromRational . toRational)
#endif
           t <> ")"
         _ -> "")

alogWithStack :: (MonadIO m, HasCallStack) => Priority -> String -> m ()
alogWithStack priority msg =
  alogDrop (take 2) priority (msg <> "\n" <> prettyCallStack (fromCallSiteList $ dropModuleFrames $ dropModuleFrames $ getCallStack callStack))

-- | Truncate a string and add an ellipsis.
-- ellipsis :: Int -> String -> String
-- ellipsis n s = if Data.Foldable.length s > n + 3 then take n s <> "..." else s

-- | A logger based on the module name at the top of the stack,
-- excluding names from this module.  This is fragile, refactoring can
-- break it.
logger :: HasCallStack => IO Logger
logger = getLogger $ {-trace ("logger at: " <> loggerLoc) $-} loggerName

loggerName :: HasCallStack => String
loggerName =
  case dropModuleFrames getStack of
    [] -> rootLoggerName
    ((_, SrcLoc {..}) : _) -> srcLocModule

loggerLoc :: HasCallStack => String
loggerLoc =
  case dropModuleFrames getStack of
    [] -> rootLoggerName
    ((_, l) : _) -> prettyLoc l

-- trimmedStack :: HasCallStack => [([Char], SrcLoc)]
-- trimmedStack = take 2 (getCallStack callStack)

#if 0
class HasSavedTime s where savedTime :: Lens' s UTCTime
instance HasSavedTime UTCTime where savedTime = id

-- With elapsed time I think.
alogElapsed :: forall s m. (MonadIO m, HasSavedTime s, HasCallStack, MonadState s m) => Priority -> String -> m ()
alogElapsed priority msg = do
  level <- getLevel <$> liftIO (maybe getRootLogger getLogger (prettyLocN callStack 1))
  prev <- use savedTime
  time <- liftIO getCurrentTime
  l <- liftIO logger
  when (level <= Just priority) (savedTime .= time)
  liftIO $
    logL l priority $
      logStringOld prev time priority msg
#endif

alogN :: (MonadIO m, HasCallStack) => Int -> Priority -> String -> m ()
alogN depth priority msg = liftIO $ do
  -- time <- getCurrentTime
  l <- logger
  logL l priority (logString (callSiteN depth) msg)

type DropFn = Locs -> Locs

callSiteN :: HasCallStack => Int -> DropFn
callSiteN depth = take (depth + 1) . dropWhile (\(_, SrcLoc {srcLocModule = m}) -> isSuffixOf ".Log" m)
  where _ = callStack

-- from Base.SrcLoc

-- | This function creates a value which uniquely identifies the
-- location where it is invoked.  Note that it is easy to make the
-- mistake of using this inside a function, expecting unique keys
-- anywhere the function is called but instead getting the same key
-- everywhere.
loc :: HasCallStack => SrcLoc
loc = scrubLoc (snd here)

here :: HasCallStack => (String, SrcLoc)
here = head $ dropModuleFrames getStack

-- | The srcLocPackage for a symbol can vary depending on whether we
-- are using the compiler or the interpreter.  This erases the
-- differences, not sure what risks this might entail.
scrubLoc :: SrcLoc -> SrcLoc
scrubLoc l = l {srcLocPackage = "", srcLocFile = ""}

-- | Pretty print the location where this appears
thisLocation :: (HasCallStack, IsString s) => s
thisLocation = fromString $ prettyShow here

-- | Adds the function name
thisLocation' :: (HasCallStack, IsString s) => s
thisLocation' = fromString $ prettyframe here
  where
    prettyframe (function, SrcLoc{..}) = srcLocModule <> "." <> function <> ":" <> Prelude.show srcLocStartLine

-- | Adds the column number
thisLocation'' :: (HasCallStack, IsString s) => s
thisLocation'' = fromString $ prettyframe here
  where
    prettyframe (function, SrcLoc{..}) = srcLocModule <> "." <> function <> ":" <> Prelude.show srcLocStartLine <> ":" <> Prelude.show srcLocStartCol


-- | Pretty print the location where this appears
ici :: (HasCallStack, IsString s) => s
ici = thisLocation

-- | The function name appears in a pair with the location where it is
-- called, not where it is located.  For this reason we drop one
-- additional frame here, the one that contains the the function
-- "thisFunction".
thisFunction :: (HasCallStack, IsString s) => s
thisFunction = maybe "???" (fromString . fst . fst) $ List.uncons $ tail $ dropModuleFrames getStack

compactStackWith :: forall s. (IsString s, Monoid s) => (forall a. [a] -> [a]) -> [(String, SrcLoc)] -> s
compactStackWith f locs = compactStack ((f . drop 1) locs)

-- | Return the name of the parent (caller) of function @child@.
parentFunc :: HasCallStack => String -> String
parentFunc child =
  case (getStack Fn.&
        dropWhile ((/= child) . fst) Fn.&
        drop 1) of
    ((x@(_ : _), _) : _) -> x
    _ -> show getStack
