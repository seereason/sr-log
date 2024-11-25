-- | On the client side we use hslogger's simple handlers to log to
-- stdout.

{-# LANGUAGE FlexibleInstances, RankNTypes, RecordWildCards, TemplateHaskell, TypeSynonymInstances #-}

module SeeReason.LogClient
  ( setClientLoggingLevel
  , setupClientLogger
  , HasSavedTime(..)
  , module SeeReason.Log
  ) where

import Control.Monad.Trans (MonadIO(..))
import SeeReason.Log
import GHC.Stack (HasCallStack)
import System.IO (stdout)
import System.Log.Formatter (tfLogFormatter)
import System.Log.Logger as Log (getLevel, getLogger, getRootLogger, Priority(..), saveGlobalLogger, setHandlers, setLevel)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (verboseStreamHandler)

setClientLoggingLevel :: HasCallStack => String -> Priority -> IO ()
setClientLoggingLevel name new = liftIO $ do
  logger <- getLogger name
  case getLevel logger of
    Just old | old == new -> pure ()
    _ -> do
      saveGlobalLogger (setLevel new $ logger)
      alog INFO
        ((case getLevel logger of
            Nothing -> "Setting ";
            Just _old -> "Changed ") <>
         name <> " logging level" <>
         (case getLevel logger of
            Nothing -> ""
            Just old -> " from " <> show old) <>
          " to " <> show new)

setupClientLogger :: (MonadIO m, HasCallStack) => Priority -> m ()
setupClientLogger lvl = liftIO $ do
  stdoutLog <- verboseStreamHandler stdout lvl

  saveGlobalLogger =<<
    (setLevel lvl .
     setHandlers [{-appLog,-} setFormatter stdoutLog (tfLogFormatter "%b %e %T%4Q" "$time: [$prio] $msg") ]) <$> getRootLogger
  alog lvl ("Setup client logging at level " ++ show lvl)
