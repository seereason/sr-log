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

    -- * Elapsed time
#if 0
  , HasSavedTime(..)
  , alogElapsed
#endif
    -- * Re-exports
  , Priority(..)
  ) where

import Control.Lens (to, view)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Data (Data)
import Data.Default (Default(def))
import Data.List as List (intercalate, isSuffixOf)
import Data.Map as Map (Map)
import Data.SafeCopy (SafeCopy)
import Data.Serialize (Serialize)
import Data.String (IsString(fromString))
import Data.Text (pack, Text)
import Data.Time ({-diffUTCTime, getCurrentTime,-} UTCTime)
import Extra.Lens (HasLens(hasLens))
import Extra.Serialize (SafeCopySerialize(..))
import Extra.SrcLoc (Call(Call, callee_, site_), calls, compactLocs, compactStack, dropModuleFrames, getStack, sites)
import GHC.Generics (Generic)
import GHC.Stack (callStack, fromCallSiteList, getCallStack, HasCallStack, prettyCallStack, SrcLoc(..))
import SeeReason.LogOrphans ()
import System.Log.Logger ({-getLevel,-} getLogger, {-getRootLogger,-} Logger, logL, Priority(..), rootLoggerName)
import Text.PrettyPrint.HughesPJClass (pPrint)

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup((<>)))
#endif
#if MIN_VERSION_time(1,9,0)
import Data.Time.Format (formatTime, defaultTimeLocale)
#endif

logString :: (HasCallStack) => ([Call] -> [Call]) -> String -> String
logString fn msg = do
  pre allLocs <> take (lim - len allLocs) msg <> suf allLocs
  where
    len :: [Call] -> Int
    len fs = length (pre fs) + length (suf fs)
    callSite :: [Call] -> [Call]
    callSite fs = take 2 fs
    pre :: [Call] -> String
    pre fs = compactStack (callSite fs) <> " - "
    suf :: [Call] -> String
    suf fs = if locs fs /= callSite fs then formattedStack (locs fs) else ""
    locs :: [Call] -> [Call]
    locs fs = fn fs
    allLocs = dropModuleFrames getStack
    lim =
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

#if 0
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
#endif

type Locs = [Call]

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

#if 0
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
#endif

alogWithStack :: (MonadIO m, HasCallStack) => Priority -> String -> m ()
alogWithStack priority msg =
  alogDrop (take 2) priority (msg <> "\n" <> prettyCallStack (fromCallSiteList $ sites $ dropModuleFrames $ dropModuleFrames $ calls $ getCallStack callStack))

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
callSiteN depth = take (depth + 1) . dropWhile (\Call{site_ = SrcLoc {srcLocModule = m}} -> isSuffixOf ".Log" m)
  where _ = callStack
