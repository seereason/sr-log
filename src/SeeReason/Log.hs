{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
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
  ( LogConfig(LogConfig, loggerConfig)
  , LoggerName(LoggerName)
  , LoggerConfig(LoggerConfig, logStack, logLevel)
  , defaultLoggerConfig
  , defaultLogConfig
  , alog
  , alogDrop
  , alogN
  , logString
  , getStack
  , compactStack
  , compactStackWith
  , parentFunc
  , loc
  , srclocList
  , srclocs
  , putStrLnLoc
  , putStrLnLocs

  -- * Pretty print
  , thisFunction
  , thisLocation, ici
  , thisLocation'
  , thisLocation''

    -- * Elapsed time
  , HasSavedTime(..)
  , alogElapsed
#if 0
  , dropPackageFrames
  , dropModuleFrames
  -- * Full stack formatting
  , prettyLoc
  , prettyLocN
  , topLoc
  , topLocs
  , locs
  -- * Compact stack formatting
  , srcloc
  , srcloccol
  , srcfunloc
  , srcfunloccol
  , compactStack'
  , compactLocs
  -- * Log string formatting
  , locDrop

    -- * from SeeReason.LogPure
  , callSiteOnly
  , logStringOld

    -- * from SeeReason.Log
  , loggerName, logger
  , alog2
  , alogs
  -- , alogG
  -- , alogH
  , printLoc
  , putLoc
    -- * from Base.Log
  , alogShow
#endif
#if 0
  , scrubLoc
  , locN
  -- * With Column numbers
  , srclocs'
  , srclocList'
  , srcloc'

  , locmsg
  , hlocmsg
  , locAttr
#endif
    -- * Re-exports
  , Priority(..)
  ) where

import Control.Lens ((.=), _2, Lens', ix, preview, to, use)
import Control.Monad.Except (when)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Bool (bool)
import Data.Data (Data)
import Data.Default (Default(def))
import Data.List as List (intercalate, intersperse, isPrefixOf, isSuffixOf, uncons)
import Data.Map as Map (Map)
import Data.Maybe (fromJust, fromMaybe)
import Data.SafeCopy (SafeCopy(version), safeGet, safePut)
import Data.Serialize (Serialize(get, put))
import Data.String (IsString(fromString))
import Data.Text (pack, Text)
import Data.Time (diffUTCTime, getCurrentTime, UTCTime)
import Data.Typeable (Typeable)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import GHC.Stack (CallStack, callStack, fromCallSiteList, getCallStack, HasCallStack, prettyCallStack, SrcLoc(..))
import SeeReason.LogOrphans ()
import System.IO (hPutStrLn, stderr)
import System.Log.Logger (getLevel, getLogger, getRootLogger, Logger, logL, Priority(..), rootLoggerName)
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), prettyShow, text )
import Text.Printf (printf)

import qualified Data.Function as Fn ((&))

#if 0
import Base.Alderon (dataSafe_)
import Base.Orphans ({-instance Pretty SrcLoc-})
import Base.Utils.Debug (traceWith)
import Control.Monad.Except (liftIO, MonadIO)
import Control.Lens as Lens (_2, ix, preview, to)
import Data.Maybe (fromJust)
import Data.String (IsString)
import GHC.Stack (CallStack, callStack, getCallStack, HasCallStack, SrcLoc(..))
import Data.String (fromString)
import SeeReason.Loc (srcloc, srclocs)
import System.IO (hPutStrLn, stderr)
import Text.PrettyPrint.HughesPJClass (prettyShow, text)
import Alderon.Alderon
#endif

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
prettyLoc SrcLoc{..} =
  foldr @[] (++) ""
    [ srcLocModule, ":"
    , show srcLocStartLine {-, ":"
    , show srcLocStartCol-} ]

topLoc :: (IsString s, Monoid s, HasCallStack) => s
topLoc = compactStack (take 2 $ dropModuleFrames $ getStack)

topLocs :: (IsString s, Monoid s, HasCallStack) => Int -> s
topLocs n = compactStack (take (n + 2) $ dropModuleFrames $ getStack)

putStrLnLoc :: (MonadIO m, HasCallStack) => String -> m ()
putStrLnLoc msg = liftIO $ putStrLn (msg <> " (" <> topLoc <> ")")

putStrLnLocs :: (MonadIO m, HasCallStack) => Int -> String -> m ()
putStrLnLocs n msg = liftIO $ putStrLn (msg <> " (" <> topLocs n <> ")")

-- | Verbosely format the full call stack starting at the nth level up.
locs :: CallStack -> Int -> String
locs stack n =
  (intercalate " -> " . fmap (prettyLoc . snd) . drop n . getCallStack) stack

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
srcloc loc = fromString (srcLocModule loc) <> ":" <> fromString (show (srcLocStartLine loc))

-- | Compactly format a source location with a function name
srcfunloc :: (IsString s, Semigroup s) => SrcLoc -> s -> s
srcfunloc loc f = fromString (srcLocModule loc) <> "." <> f <> ":" <> fromString (show (srcLocStartLine loc))

-- | With start column
srcloccol :: (IsString s, Semigroup s) => SrcLoc -> s
srcloccol loc = srcloc loc <> ":" <> fromString (show (srcLocStartCol loc))

srcfunloccol :: (IsString s, Semigroup s) => SrcLoc -> s -> s
srcfunloccol loc f = srcfunloc loc f <> ":" <> fromString (show (srcLocStartLine loc))

thisPackage :: String
thisPackage = "sr-log-"

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

-- | Build the prefix of a log message, after applying a function to
-- the call stack.
locDrop :: HasCallStack => ([(String, SrcLoc)] -> [(String, SrcLoc)]) -> String
locDrop fn = compactStack (fn getStack)
{-# DEPRECATED locDrop "Use compactStack (fn getStack)" #-}

-- | Stack with main last.  Bottom frame includes the function name.
-- Top frame includes the column number.
compactStack :: forall s. (IsString s, Monoid s) => [(String, SrcLoc)] -> s
compactStack = mconcat . intersperse (" < " :: s) . compactLocs

-- | 'compactStack' with a different arrow, it can cause problems.
compactStack' :: forall s. (IsString s, Monoid s) => [(String, SrcLoc)] -> s
compactStack' = mconcat . intersperse (" ← " :: s) . compactLocs

compactLocs :: forall s. (IsString s, Monoid s) => [(String, SrcLoc)] -> [s]
compactLocs [] = ["(no CallStack)"]
compactLocs [(callee, loc)] = [fromString callee, srcloccol loc]
compactLocs [(_, loc), (caller, _)] = [srcloccol loc <> "." <> fromString caller]
compactLocs ((_, loc) : more@((caller, _) : _)) =
  srcfunloc loc (fromString caller) : stacktail (fmap snd more)
  where
    stacktail :: [SrcLoc] -> [s]
    stacktail [] = []
    -- Include the column number of the last item, it may help to
    -- figure out which caller is missing the HasCallStack constraint.
    stacktail [loc'] = [srcloccol loc']
    stacktail (loc' : more') = srcloc loc' : stacktail more'

-- | Display the function and module where the logger was called
callSiteOnly = take 2
{-# DEPRECATED callSiteOnly "Use take 2" #-}

logString :: HasCallStack => ([(String, SrcLoc)] -> [(String, SrcLoc)]) -> String -> String
logString fn msg =
  pre <> take (lim - len) msg <> suf
  where
    len = length pre + length suf
    pre = compactStack (take 2 locs) <> " - "
    suf = if length locs > 2 then formattedStack locs else ""
    locs :: [(String, SrcLoc)]
    locs = fn $ dropModuleFrames {-LogPure-} $ dropModuleFrames {-SrcLoc-} getStack
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

-- deriving instance Data Priority
-- deriving instance Generic Priority
instance Serialize Priority where get = safeGet; put = safePut
instance SafeCopy Priority where version = 1
instance Pretty Priority where pPrint level = text (show level)

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
  l <- liftIO logger
  liftIO $ logL l priority (logString fn msg)

-- | Normal log message with location
alog :: (MonadIO m, HasCallStack) => Priority -> String -> m ()
alog priority msg | priority >= WARNING = alogWithStack priority msg
alog priority msg = alogDrop (take 2) priority msg

alog2 :: (MonadIO m, HasCallStack) => Priority -> String -> m ()
alog2 priority msg | priority >= WARNING = alogWithStack priority msg
alog2 priority msg = alogDrop (take 3) priority msg

alogWithStack :: (MonadIO m, HasCallStack) => Priority -> String -> m ()
alogWithStack priority msg =
  alogDrop (take 2) priority (msg <> "\n" <> prettyCallStack (fromCallSiteList $ dropModuleFrames $ dropModuleFrames $ getCallStack callStack))

alogs :: forall m. (MonadIO m, HasCallStack) => Priority -> [String] -> m ()
alogs priority msgs = alog priority (unwords msgs)

-- | Truncate a string and add an ellipsis.
-- ellipsis :: Int -> String -> String
-- ellipsis n s = if Data.Foldable.length s > n + 3 then take n s <> "..." else s

printLoc :: (Show a, HasCallStack, MonadIO m) => a -> m ()
printLoc x = putLoc >> liftIO (print x)

putLoc :: (HasCallStack, MonadIO m) => m ()
putLoc = liftIO (putStr (compactStack (take 2 getStack) <> " - "))

-- | A logger based on the module name at the top of the stack,
-- excluding names from this module.  This is fragile, refactoring can
-- break it.
logger :: HasCallStack => IO Logger
logger = getLogger $ traceWith (\name -> "logger at: " <> loggerLoc) $ loggerName

traceWith :: HasCallStack => (a -> String) -> a -> a
traceWith f a = trace (f a) a

loggerName :: HasCallStack => String
loggerName =
  case dropPackageFrames getStack of
    [] -> rootLoggerName
    ((_, SrcLoc {..}) : _) -> srcLocModule

loggerLoc :: HasCallStack => String
loggerLoc =
  case dropPackageFrames getStack of
    [] -> rootLoggerName
    ((_, loc) : _) -> prettyLoc loc

-- trimmedStack :: HasCallStack => [([Char], SrcLoc)]
-- trimmedStack = take 2 (getCallStack callStack)

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

alogN :: (MonadIO m, HasCallStack) => Int -> Priority -> String -> m ()
alogN depth priority msg = liftIO $ do
  -- time <- getCurrentTime
  l <- logger
  logL l priority (logString (callSiteN depth) msg)

type DropFn = Locs -> Locs

callSiteN :: HasCallStack => Int -> DropFn
callSiteN depth = take (depth + 1) . dropWhile (\(_, SrcLoc {srcLocModule = m}) -> isSuffixOf ".Log" m)
  where _ = callStack

trimmedStack :: HasCallStack => Locs
trimmedStack = take 2 (getCallStack callStack)

alogShow :: forall a m. (Show a, MonadIO m) => Priority -> String -> a -> m ()
alogShow priority prefix a = alog priority (prefix <> show a)

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

-- | Like loc, but looks n frames down into the stack.
locN :: HasCallStack => Int -> SrcLoc
locN n = fromJust $ preview (to getCallStack . ix n . _2) callStack

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

thisFunction :: (HasCallStack, IsString s) => s
thisFunction = maybe "???" (fromString . fst . fst) $ List.uncons $ dropModuleFrames getStack

-- | Compactly format a call stack.
srclocs' :: (IsString s, Monoid s) => CallStack -> s
-- The space before the arrow allows the console to add line breaks.
-- The space after is omitted to make these line breaks more
-- consistent.
srclocs' = mintercalate (fromString " →") . srclocList'

-- | List of more compactly pretty printed CallStack location.
-- Reversed so main comes first.
srclocList' :: IsString s => CallStack -> [s]
srclocList' = fmap (fromString . srcloc' . snd) . reverse . getCallStack

-- | Compactly format a source location
srcloc' :: (IsString s, Semigroup s) => SrcLoc -> s
srcloc' l = srcloc l <> ":" <> fromString (show (srcLocStartCol l))

locmsg :: (MonadIO m, HasCallStack) => String -> m ()
locmsg s = liftIO (putStrLn (s <> " (" <> maybe "???" srcloc (preview (to getCallStack . ix 0 . _2) callStack) <> ")"))

hlocmsg :: (MonadIO m, HasCallStack) => String -> m ()
hlocmsg s = liftIO (hPutStrLn stderr (s <> " (" <> maybe "???" srcloc (preview (to getCallStack . ix 0 . _2) callStack) <> ")"))

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

#if 0
locAttr :: HasCallStack => (forall a. [a] -> [a]) -> Attribute
locAttr f = dataSafe_ "loc" (compactStackWith (f . drop 1) getStack)
#endif
