{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}
{-# LANGUAGE UndecidableInstances #-}

module SeeReason.Log
  ( module SeeReason.LogPure
  , LoggerName(LoggerName)
  , LogConfig(LogConfig, loggerConfig)
  , LoggerConfig(LoggerConfig, logStack, logLevel)
  , defaultLogConfig
  , defaultLoggerConfig
  , alog
  , alog2
  , alogDrop
  , alogs
  -- , alogG
  -- , alogH
  , printLoc
  , putLoc
    -- * Elapsed time
  , HasSavedTime(..)
  , alogElapsed
    -- * Re-exports
  , Priority(..)
  ) where

import Control.Lens ((.=), Lens', use)
import Control.Monad.Except (when)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Data (Data)
import Data.Default (Default(def))
import Data.Map as Map (Map)
import Data.Serialize (Serialize(get, put))
import Data.SafeCopy (SafeCopy(version), safeGet, safePut)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup((<>)))
#endif
import Data.String (IsString(fromString))
import Data.Text (pack, Text)
import Data.Time (getCurrentTime, UTCTime)
#if MIN_VERSION_time(1,9,0)
import Data.Time.Format (formatTime, defaultTimeLocale)
#endif
import Data.Typeable (Typeable)
-- import Extra.Orphans ({-instance Pretty SrcLoc-})
import GHC.Generics (Generic)
import GHC.Stack (callStack, HasCallStack, fromCallSiteList, getCallStack, prettyCallStack, SrcLoc(..))
import SeeReason.LogPure
import SeeReason.SrcLoc
import System.Log.Logger (getLevel, getLogger, getRootLogger, Logger, logL, Priority(..), rootLoggerName)
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )

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
logger =
  case getStack of
    [] -> getRootLogger
    ((_, SrcLoc {..}) : _) -> getLogger srcLocModule

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
