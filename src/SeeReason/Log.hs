{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies, DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -Wno-redundant-constraints #-}

module SeeReason.Log
  ( -- * Configuration
    LogConfig(LogConfig, loggerConfig)
  , LoggerName(LoggerName)
  , LoggerConfig(LoggerConfig, logStack, logLevel)
  , defaultLoggerConfig
  , defaultLogConfig

  , logger, loggerName, loggerLoc

    -- * Logging
  , alog
  , alogDrop
  , alogN
  -- , LoggerSettings(..)
  -- , clog
  , logMessageLengthLimit
  , logString

    -- * Re-exports
  , Priority(..)
  ) where

-- import Control.Lens (to, view)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Data (Data)
-- import Data.Default (Default(def))
import Data.List as List (intercalate, isSuffixOf)
import Data.Map as Map (Map)
import Data.SafeCopy (SafeCopy)
import Data.Serialize (Serialize)
import Data.String (IsString(fromString))
import Data.Text (pack, Text)
-- import Data.Time ({-diffUTCTime, getCurrentTime,-} UTCTime)
-- import Extra.Lens (HasLens(hasLens))
import Extra.Serialize (SafeCopySerialize(..))
import Extra.SrcLoc (Call(Call, callee_, site_), callLocs, compactLocs, dropModuleFrames, funStack, getStack)
import GHC.Generics (Generic)
import GHC.Stack (callStack, HasCallStack, SrcLoc(..))
import System.Log.Logger ({-getLevel,-} getLogger, {-getRootLogger,-} Logger, logL, Priority(..), rootLoggerName)
import Text.PrettyPrint.HughesPJClass (pPrint)

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup((<>)))
#endif
#if MIN_VERSION_time(1,9,0)
-- import Data.Time.Format (formatTime, defaultTimeLocale)
#endif

logString :: (HasCallStack) => ([Call] -> [Call]) -> String -> String
logString fn msg = do
  -- Truncate the message to limit length, rather than the stack suffix
  pre <> take (logMessageLengthLimit - len) msg <> suf
  where
    len :: Int
    len = length pre + length suf
    callSite :: [Call]
    callSite = take 2 stk
    -- The prefix includes the call site
    pre :: String
    pre = funStack callSite <> " - "
    -- The suffix includes the call stack.  Omit if it is the same as
    -- the call site (maybe due to missing HasCallStack constraints.)
    suf :: String
    suf = if stk /= callSite then formattedStack stk else ""

    stk :: [Call]
    stk = fn (dropModuleFrames getStack)

logMessageLengthLimit :: Int
logMessageLengthLimit =
#if defined(darwin_HOST_OS)
          2002
#else
          60000
#endif

formattedStack :: [Call] -> String
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

newtype LoggerName = LoggerName Text deriving (Generic, Eq, Ord, Show, Data)
deriving via (SafeCopySerialize LoggerName) instance Serialize LoggerName
instance SafeCopy LoggerName
instance IsString LoggerName where
  fromString = LoggerName . pack

data LogConfig =
  LogConfig
  { loggerConfig :: Map LoggerName LoggerConfig
  } deriving (Generic, Show, Eq, Ord, Data)

deriving via (SafeCopySerialize LogConfig) instance Serialize LogConfig
instance SafeCopy LogConfig

data LoggerConfig =
  LoggerConfig
  { logLevel :: Priority
  , logStack :: Bool -- ^ Include a compact stack trace
  , logLimit :: Maybe Int -- ^ Truncate messages at this length
  } deriving (Generic, Show, Eq, Ord, Data)

deriving via (SafeCopySerialize LoggerConfig) instance Serialize LoggerConfig
instance SafeCopy LoggerConfig

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig {loggerConfig = [(fromString rootLoggerName, defaultLoggerConfig)]}

defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig {logLevel = WARNING, logStack = False, logLimit = Just 400}

-- | Log message with a multi-line log stack on the next line
alogDrop :: (MonadIO m, HasCallStack) => ([Call] -> [Call]) -> Priority -> String -> m ()
alogDrop fn priority msg = do
  -- time <- getCurrentTime
  -- (liftIO . putStrLn . ("loggerLoc: " <>)) $ loggerLoc
  -- (liftIO . putStrLn . ("loggerName: " <>)) $ loggerName
  -- liftIO $ (putStrLn . ("level=" <>) . show . getLevel) =<< logger
  l <- liftIO logger
  liftIO $ logL l priority (logString fn msg)

#if 0
data LoggerSettings =
  LoggerSettings
  { showCallStack :: Bool
  , showFullStack :: Bool
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

instance HasLens LoggerSettings LoggerSettings where hasLens = id

-- | Log function with access to a settings record.
clog :: (MonadIO m, ?settings :: settings, HasLens settings LoggerSettings, HasCallStack) => Priority -> String -> m ()
clog priority msg | view (hasLens . to showFullStack) ?settings = alogWithStack priority msg
clog priority msg | view (hasLens . to showCallStack) ?settings = alogDrop id priority msg
clog priority msg | priority >= WARNING = alogDrop id priority msg
clog priority msg = alogDrop (take 2) priority msg
#endif

-- | Normal log message with location
alog :: (MonadIO m, HasCallStack) => Priority -> String -> m ()
alog priority msg | priority >= WARNING = alogWithStack priority msg
alog priority msg = alogDrop (take 2) priority msg

alogWithStack :: (MonadIO m, HasCallStack) => Priority -> String -> m ()
alogWithStack priority msg =
  alogDrop (take 2) priority (msg <> "\n" <> callLocs)

-- | Truncate a string and add an ellipsis.
-- ellipsis :: Int -> String -> String
-- ellipsis n s = if Data.Foldable.length s > n + 3 then take n s <> "..." else s

-- | A logger based on the module name at the top of the stack,
-- excluding names from this module.  This is fragile, refactoring can
-- break it.
logger :: HasCallStack => IO Logger
logger = getLogger {-trace ("logger at: " <> loggerLoc) $-} loggerName

loggerName :: (HasCallStack) => String
loggerName =
  case dropModuleFrames getStack of
    [] -> rootLoggerName
    (Call{site_ = SrcLoc{..}} : _) -> srcLocModule

loggerLoc :: HasCallStack => String
loggerLoc =
  case dropModuleFrames getStack of
    [] -> rootLoggerName
    (Call{..} : _) -> show (pPrint site_)

alogN :: (MonadIO m, HasCallStack) => Int -> Priority -> String -> m ()
alogN depth priority msg = liftIO $ do
  -- time <- getCurrentTime
  l <- logger
  logL l priority (logString (callSiteN depth) msg)

type DropFn = [Call] -> [Call]

callSiteN :: HasCallStack => Int -> DropFn
callSiteN depth = take (depth + 1) . dropWhile (\Call{site_ = SrcLoc {srcLocModule = m}} -> isSuffixOf ".Log" m)
  where _ = callStack
