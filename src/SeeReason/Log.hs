{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module SeeReason.Log
  ( module SeeReason.LogPure
  , alog
  , alog2
  , alogWithStack
  , alogDrop
  , alogs
  , LogState(..)
  -- , alogG
  -- , alogH
  , printLoc
  , putLoc
    -- * Elapsed time
  , HasSavedTime(..)
  , alog'
    -- * Re-exports
  , Priority(..)
  ) where

import Control.Lens((.=), Lens', use)
import Control.Monad.Except (when)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Data (Data)
import Data.Default (Default(def))
import Data.Foldable
import Data.Serialize (Serialize(get, put))
import Data.SafeCopy (SafeCopy, safeGet, safePut)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup((<>)))
#endif
import Data.Time (getCurrentTime, UTCTime)
#if MIN_VERSION_time(1,9,0)
import Data.Time.Format (formatTime, defaultTimeLocale)
#endif
import Data.Typeable (Typeable)
-- import Extra.Orphans ({-instance Pretty SrcLoc-})
import GHC.Generics (Generic)
import GHC.Stack (callStack, fromCallSiteList, getCallStack, HasCallStack, prettyCallStack, SrcLoc(..))
import SeeReason.LogPure
import SeeReason.SrcLoc
import System.Log.Logger (getLevel, getLogger, getRootLogger, Logger, logL, Priority(..))

type FunctionName = String
type Locs = [(FunctionName, SrcLoc)]

alogDrop :: (MonadIO m, HasCallStack) => (Locs -> Locs) -> Priority -> String -> m ()
alogDrop fn priority msg = liftIO $ do
  -- time <- getCurrentTime
  l <- logger
  logL l priority (logString fn msg)

alog :: (MonadIO m, HasCallStack) => Priority -> String -> m ()
alog priority msg | priority >= WARNING = alogWithStack priority msg
alog priority msg = alogDrop (take 2) priority msg

alog2 :: (MonadIO m, HasCallStack) => Priority -> String -> m ()
alog2 priority msg | priority >= WARNING = alogWithStack priority msg
alog2 priority msg = alogDrop (take 3) priority msg

alogWithStack :: (MonadIO m, HasCallStack) => Priority -> String -> m ()
alogWithStack priority msg =
  alogDrop (take 2) priority (msg <> "\n" <> prettyCallStack (fromCallSiteList $ dropThisPackageFrames $ getCallStack callStack))

alogs :: forall m. (MonadIO m, HasCallStack) => Priority -> [String] -> m ()
alogs priority msgs = alog priority (unwords msgs)

-- | Truncate a string and add an ellipsis.
ellipsis :: Int -> String -> String
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

printLoc :: (Show a, HasCallStack, MonadIO m) => a -> m ()
printLoc x = putLoc >> liftIO (print x)

putLoc :: (HasCallStack, MonadIO m) => m ()
putLoc = liftIO (putStr (compactStack (take 2 getStack) <> " - "))

-- | A logger based on the module name at the top of the stack,
-- excluding names from this module.  This is fragile, refactoring can
-- break it.
logger :: HasCallStack => IO Logger
logger =
  case getStack of
    [] -> getRootLogger
    ((_, SrcLoc {..}) : _) -> getLogger srcLocModule

trimmedStack :: HasCallStack => [([Char], SrcLoc)]
trimmedStack = take 2 (getCallStack callStack)

class HasSavedTime s where savedTime :: Lens' s UTCTime
instance HasSavedTime UTCTime where savedTime = id

alog' :: forall s m. (MonadIO m, HasSavedTime s, HasCallStack, MonadState s m) => Priority -> String -> m ()
alog' priority msg = do
  level <- getLevel <$> liftIO (maybe getRootLogger getLogger (prettyLocN callStack 1))
  prev <- use savedTime
  time <- liftIO getCurrentTime
  l <- liftIO logger
  when (level <= Just priority) (savedTime .= time)
  liftIO $
    logL l priority $
      logStringOld prev time priority msg
