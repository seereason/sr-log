-- | On the client side we use hslogger's simple handlers to log to
-- stdout.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SeeReason.LogClient
  ( setupClientLogger
  , setClientLoggingLevel
  , HasSavedTime(..)
  , module SeeReason.Log
  ) where

import SeeReason.Log
import GHC.Stack (HasCallStack)
import System.IO (stdout)
import System.Log.Formatter (tfLogFormatter)
import System.Log.Logger as Log (getLevel, getLogger, getRootLogger, Priority(..), saveGlobalLogger, setHandlers, setLevel)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (verboseStreamHandler)

setClientLoggingLevel ::
  HasCallStack
  => String
  -> Priority
  -> IO ()
setClientLoggingLevel name new = do
  logger <- getLogger name
  case getLevel logger of
    Just old | old == new -> pure ()
    Just old -> do
      alog INFO ("Changing logger " <> name <> " logging level from " <> show old <> " to " <> show new)
      saveGlobalLogger (setLevel new logger)
    Nothing -> do
      alog INFO ("Initializing logger " <> name <> " to " <> show new)
      saveGlobalLogger (setLevel new logger)

setupClientLogger ::
  HasCallStack
  => Priority
  -> IO ()
setupClientLogger lvl = do
  stdoutLog <- verboseStreamHandler stdout lvl

  saveGlobalLogger =<<
    (setLevel lvl .
     setHandlers [{-appLog,-} setFormatter stdoutLog (tfLogFormatter "%b %e %T%4Q" "$time: [$prio] $msg") ]) <$> getRootLogger
  alog lvl ("Setup client logging at level " ++ show lvl)
