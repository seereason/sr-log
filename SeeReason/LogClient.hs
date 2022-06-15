-- | On the client side we use hslogger's simple handlers to log to
-- stdout.

{-# LANGUAGE FlexibleInstances, RankNTypes, RecordWildCards, TemplateHaskell, TypeSynonymInstances #-}

module SeeReason.LogClient
  ( setClientLoggingLevel
  , setupClientLogger
  , HasSavedTime(..)
  , module SeeReason.Log
  ) where

import Control.Lens(ix, preview, to)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Trans (MonadIO(..))
import SeeReason.Log
import GHC.Stack (CallStack, getCallStack, HasCallStack, SrcLoc(..))
import System.IO (stdout)
import System.Log.Formatter (tfLogFormatter)
import System.Log.Logger as Log (getLevel, getLogger, getRootLogger, Logger(..), Priority(..), rootLoggerName, saveGlobalLogger, setHandlers, setLevel)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (verboseStreamHandler)

setClientLoggingLevel :: HasCallStack => String -> Priority -> IO ()
setClientLoggingLevel name new = liftIO $ do
  logger <- getLogger name
  case getLevel logger of
    Just old | old == new -> pure ()
    _ -> do
      saveGlobalLogger (setLevel new $ logger)
      alog ALERT
        ((case getLevel logger of
            Nothing -> "Setting";
            Just old -> "Changed") <>
         name <> " logging level" <>
         (case getLevel logger of
            Nothing -> ""
            Just old -> " from " <> show old) <>
          " to " <> show new)

setupClientLogger :: MonadIO m => Priority -> m ()
setupClientLogger lvl = liftIO $ do
  stdoutLog <- verboseStreamHandler stdout lvl

  saveGlobalLogger =<<
    (setLevel lvl .
     setHandlers [{-appLog,-} setFormatter stdoutLog (tfLogFormatter "%b %e %T%4Q" "$time: [$prio] $msg") ]) <$> getRootLogger
  alog lvl ("Setup client logging at level " ++ show lvl)
