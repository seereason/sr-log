{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module SeeReason.Log
  ( -- * Logging
    alog
  , alogWithStack
  , alogDrop
  , alogs
  , LogState(..)
  , alogG
  , alogH
  , printLoc
  , putLoc
  , locDrop
  , standardDrop
  , loc'
  , locs
  , srclocs
  , srclocList
  , logString
    -- * Elapsed time
  , HasSavedTime(..)
  , alog'
  , logString'
    -- * Re-exports
  , Priority(..)
  ) where

import Control.Lens((.=), ix, Lens', non, preview, to, use, view)
import Control.Monad.Except (when)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Bool (bool)
import Data.Cache (HasDynamicCache, mayLens)
import Data.Data (Data)
import Data.Default (Default(def))
import Data.Foldable
import Data.List (intercalate, intersperse, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize(get, put))
import Data.SafeCopy (SafeCopy, safeGet, safePut)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup((<>)))
#endif
import Data.String (IsString(fromString))
import Data.Time (diffUTCTime, getCurrentTime, UTCTime)
#if MIN_VERSION_time(1,9,0)
import Data.Time.Format (formatTime, defaultTimeLocale)
#endif
import Data.Typeable (Typeable)
-- import Extra.Orphans ({-instance Pretty SrcLoc-})
import GHC.Generics (Generic)
import GHC.Stack (CallStack, callStack, fromCallSiteList, getCallStack, HasCallStack, prettyCallStack, SrcLoc(..))
import GHC.Stack.Types (CallStack(..))
import System.Log.Logger (getLevel, getLogger, getRootLogger, Logger, logL, Priority(..))
import Text.PrettyPrint.HughesPJClass (prettyShow)
import Text.Printf (printf)

-- type DropFn = (String, SrcLoc) -> Bool
type DropFn = [(String, SrcLoc)] -> [(String, SrcLoc)]

alog :: (MonadIO m, HasCallStack) => Priority -> String -> m ()
alog priority msg | priority >= WARNING = alogWithStack priority msg
alog priority msg = liftIO $ do
  -- time <- getCurrentTime
  l <- logger
  logL l priority (logString standardDrop msg)

alogWithStack :: (MonadIO m, HasCallStack) => Priority -> String -> m ()
alogWithStack priority msg = liftIO $ do
  l <- logger
  logL l priority (logString standardDrop msg <> "\n" <> prettyCallStack (locDrop' standardDrop callStack))

alogDrop :: (MonadIO m, HasCallStack) => DropFn -> Priority -> String -> m ()
alogDrop fn priority msg = liftIO $ do
  -- time <- getCurrentTime
  l <- logger
  logL l priority (logString fn msg)

#if 0
logModule :: HasCallStack => String
-- Unfortunately(?) the call stack is not available inside a template
-- haskell splice.
logModule =
  $(case getCallStack callStack of
       [] -> error "callStack failed"
       ((_, SrcLoc {srcLocModule = m}) : _) -> lift =<< litE (stringL m))
#else
logModule :: String
logModule = "SeeReason.Log"
#endif

standardDrop :: HasCallStack => DropFn
standardDrop = dropWhile (\(_, SrcLoc {srcLocModule = m}) -> isSuffixOf ".Log" m)

alogs :: forall m. (MonadIO m, HasCallStack) => Priority -> [String] -> m ()
alogs priority msgs = alog priority (unwords msgs)

-- | Logger whose behavior is controlled by a record in the dynamic
-- cache.
alogH :: forall s m. (HasDynamicCache s, MonadState s m, MonadIO m, HasCallStack) => Priority -> String -> m ()
alogH priority msg = do
  LogState{..} <- use (mayLens @s @LogState . non def)
  (case trace of False -> alog; True -> alogWithStack)
    priority
    (bool msg (ellipsis 200 msg) short)

alogG :: forall s m. (HasDynamicCache s, MonadReader s m, MonadIO m, HasCallStack) => Priority -> String -> m ()
alogG priority msg = do
  LogState{..} <- view (mayLens @s @LogState . non def)
  (case trace of False -> alog; True -> alogWithStack)
    priority
    (bool msg (ellipsis 200 msg) short)

-- | Truncate a string and add an ellipsis.
ellipsis n s = if Data.Foldable.length s > n + 3 then take n s <> "..." else s

data LogState
  = LogState
    { trace :: Bool
    , short :: Bool
    } deriving (Eq, Ord, Show, Generic, Data, Typeable)

instance SafeCopy LogState
instance Serialize LogState where get = safeGet; put = safePut

instance Default LogState where
  def = LogState {trace = False, short = True}

logString  :: HasCallStack => DropFn -> String -> String
logString fn msg =
#if defined(darwin_HOST_OS)
  take 2002 $
#else
  take 60000 $
#endif
  locDrop fn <> " - " <> msg

printLoc :: (Show a, HasCallStack, MonadIO m) => a -> m ()
printLoc x = putLoc >> liftIO (print x)

putLoc :: (HasCallStack, MonadIO m) => m ()
putLoc = liftIO (putStr (locDrop standardDrop <> " - "))

-- | Get the portion of the stack before we entered this module.
trimmedStack :: HasCallStack => [([Char], SrcLoc)]
trimmedStack = standardDrop (getCallStack callStack)

-- | A logger based on the module name at the top of the stack.
logger :: HasCallStack => IO Logger
logger =
  case trimmedStack of
    [] -> getRootLogger
    ((_, SrcLoc {..}) : _) -> getLogger srcLocModule

-- | Format the location of the top level of the call stack, after
-- dropping matching stack frames.
locDrop :: HasCallStack => DropFn -> String
locDrop fn =
  case getCallStack (locDrop' fn callStack) of
    [] -> "(no CallStack)"
    [(_alog, SrcLoc {..})] -> srcLocModule <> ":" <> show srcLocStartLine
    ((_, SrcLoc {..}) : (f, _) : _) -> srcLocModule <> "." <> f <> ":" <> show srcLocStartLine

-- | Drop all matching frames from a 'CallStack'.
locDrop' :: HasCallStack => DropFn -> CallStack -> CallStack
locDrop' fn = fromCallSiteList . fn . getCallStack

-- | Format the location of the nth level up in a call stack
loc' :: CallStack -> Int -> Maybe String
loc' stack n =
  preview (to getCallStack . ix n . to prettyLoc) stack
  where
    prettyLoc (_s, SrcLoc {..}) =
      foldr (++) ""
        [ srcLocModule, ":"
        , show srcLocStartLine {-, ":"
        , show srcLocStartCol-} ]

-- | Format the full call stack starting at the nth level up.
locs :: CallStack -> Int -> String
locs stack n =
  (intercalate " -> " . fmap prettyLoc . drop n . getCallStack) stack
  where
    prettyLoc (_s, SrcLoc {..}) =
      foldr (++) ""
        [ srcLocModule, ":"
        , show srcLocStartLine {-, ":"
        , show srcLocStartCol-} ]

-- | Pretty print a CallStack even more compactly.
srclocs :: (HasCallStack, IsString s, Monoid s) => CallStack -> s
-- The space before the angle brackets allows the console to add line
-- breaks, no space after to make this formatting more consistent.
srclocs = mintercalate (fromString " →") . srclocList

-- | List of more compactly pretty printed CallStack location
srclocList :: (HasCallStack, IsString s) => CallStack -> [s]
srclocList = fmap (fromString . {-prettyShow-}(\loc -> srcLocModule loc <> ":" <> show (srcLocStartLine loc)) . snd) . reverse . getCallStack

mintercalate :: Monoid s => s -> [s] -> s
mintercalate x xs = mconcat (intersperse x xs)

class HasSavedTime s where savedTime :: Lens' s UTCTime
instance HasSavedTime UTCTime where savedTime = id

alog' :: forall s m. (MonadIO m, HasSavedTime s, HasCallStack, MonadState s m) => Priority -> String -> m ()
alog' priority msg = do
  level <- getLevel <$> liftIO (maybe getRootLogger getLogger (loc' callStack 1))
  prev <- use savedTime
  time <- liftIO getCurrentTime
  l <- liftIO logger
  when (level <= Just priority) (savedTime .= time)
  liftIO $
    logL l priority $
      logString' prev time priority msg

logString'  :: UTCTime -> UTCTime -> Priority -> String -> String
logString' prev time priority msg =
#if defined(darwin_HOST_OS)
  take 2002 $
#else
  take 60000 $
#endif
    unwords $ [timestring, fromMaybe "???" (loc' callStack 1), "-", msg] <> bool [] ["(" <> show priority <> ")"] (priority == DEBUG)
    where timestring =
#if MIN_VERSION_time(1,9,0)
            formatTime defaultTimeLocale "%T%4Q"
#else
            (("elapsed: " <>) . (printf "%.04f" :: Double -> String) . fromRational . toRational)
#endif
              (diffUTCTime time prev)
