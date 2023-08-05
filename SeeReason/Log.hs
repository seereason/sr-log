{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module SeeReason.Log
  ( -- * Logging
    alog
  , alog2
  , alogWithStack
  , alogDrop
  , alogs
  , LogState(..)
  , alogG
  , alogH
  , printLoc
  , putLoc
  , locDrop
  , locDrop'
  , callSiteOnly
  , callSitePlus
  , fullStack
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
import Data.Cache (HasDynamicCache, maybeLens)
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
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow)
import Text.Printf (printf)

-- type DropFn = (String, SrcLoc) -> Bool
type Locs = [(String, SrcLoc)]
type DropFn = Locs -> Locs

alogDrop :: (MonadIO m, HasCallStack) => (Locs -> Locs) -> Priority -> String -> m ()
alogDrop fn priority msg = liftIO $ do
  -- time <- getCurrentTime
  l <- logger
  logL l priority (logString fn msg)

alog :: (MonadIO m, HasCallStack) => Priority -> String -> m ()
alog priority msg | priority >= WARNING = alogWithStack priority msg
alog priority msg = alogDrop callSiteOnly priority msg

alog2 :: (MonadIO m, HasCallStack) => Priority -> String -> m ()
alog2 priority msg | priority >= WARNING = alogWithStack priority msg
alog2 priority msg = alogDrop callSitePlus priority msg

alogWithStack :: (MonadIO m, HasCallStack) => Priority -> String -> m ()
alogWithStack priority msg =
  alogDrop callSiteOnly priority (msg <> "\n" <> prettyCallStack (locDrop' fullStack callStack))

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

-- | Display the function and module where the logger was called
callSiteOnly :: HasCallStack => (Locs -> Locs)
callSiteOnly = take 2 . dropWhile (\(_, SrcLoc {srcLocModule = m}) -> isSuffixOf ".Log" m)

-- | Display one more stack level than 'callSiteOnly'
callSitePlus :: HasCallStack => (Locs -> Locs)
callSitePlus = take 3 . dropWhile (\(_, SrcLoc {srcLocModule = m}) -> isSuffixOf ".Log" m)

-- | Display the full call stack
fullStack :: HasCallStack => (Locs -> Locs)
fullStack = dropWhile (\(_, SrcLoc {srcLocModule = m}) -> isSuffixOf ".Log" m)

alogs :: forall m. (MonadIO m, HasCallStack) => Priority -> [String] -> m ()
alogs priority msgs = alog priority (unwords msgs)

-- | Logger whose behavior is controlled by a record in the dynamic
-- cache.
alogH :: forall s m. (HasDynamicCache s, MonadState s m, MonadIO m, HasCallStack) => Priority -> String -> m ()
alogH priority msg = do
  LogState{..} <- use (maybeLens @s @LogState . non def)
  (case trace of False -> alog2; True -> alogWithStack)
    priority
    (bool msg (ellipsis 1000 msg) short)

alogG :: forall s m. (HasDynamicCache s, MonadReader s m, MonadIO m, HasCallStack) => Priority -> String -> m ()
alogG priority msg = do
  LogState{..} <- view (maybeLens @s @LogState . non def)
  (case trace of False -> alog2; True -> alogWithStack)
    priority
    (bool msg (ellipsis 1000 msg) short)

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

logString :: HasCallStack => (Locs -> Locs) -> String -> String
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
putLoc = liftIO (putStr (locDrop callSiteOnly <> " - "))

-- | Get the portion of the stack before we entered this module.
trimmedStack :: HasCallStack => [([Char], SrcLoc)]
trimmedStack = callSiteOnly (getCallStack callStack)

-- | A logger based on the module name at the top of the stack.
logger :: HasCallStack => IO Logger
logger =
  case trimmedStack of
    [] -> getRootLogger
    ((_, SrcLoc {..}) : _) -> getLogger srcLocModule

-- | Format the location of the top level of the call stack, after
-- dropping matching stack frames.
locDrop :: HasCallStack => (Locs -> Locs) -> String
locDrop fn =
  case getCallStack (locDrop' fn callStack) of
    [] -> "(no CallStack)"
    [(f, loc)] -> srcLocModule loc <> "." <> f <> ":" <> show (srcLocStartLine loc)
    ((_, loc) : more@((f, _) : _)) ->
      -- Only the first location includes the function name
      intercalate " ← " {-" <- "-}
        (srcLocModule loc <> "." <> f <> ":" <> show (srcLocStartLine loc) :
         showLocs more)
    -- prs -> intercalate " ← " (showLocs prs)
  where
    showLocs :: [(String, SrcLoc)] -> [String]
    showLocs ((_, loc) : more@((f, _) : _)) =
      srcLocModule loc <> ":" <> show (srcLocStartLine loc) :
      showLocs more
    showLocs _ = []

-- | Drop all matching frames from a 'CallStack'.
locDrop' :: HasCallStack => (Locs -> Locs) -> CallStack -> CallStack
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

-- for sr-log?
srcloc :: (HasCallStack, IsString s, Pretty SrcLoc) => s
srcloc = fromString . prettyShow . snd . head . getCallStack $ callStack

-- | List of more compactly pretty printed CallStack location
srclocList :: (HasCallStack, IsString s) => CallStack -> [s]
srclocList = fmap (fromString . (\loc -> srcLocModule loc <> ":" <> show (srcLocStartLine loc)) . snd) . reverse . getCallStack

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
