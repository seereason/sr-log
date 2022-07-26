{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell #-}

module SeeReason.LogServer (
      printServerLogger
    , setupServerLogger
    , setServerLoggingLevel
    , Priority(..)
    -- , logException
    , module SeeReason.Log
    ) where

--import Config (LogMode(..))
import Control.Exception (try)
import Control.Monad.Trans (MonadIO(..))
import SeeReason.Log
import Language.Haskell.TH (ExpQ, Exp, Loc(..), location, pprint, Q)
import System.Directory (doesFileExist)
import System.Exit
import System.IO (hPutStrLn, stderr)
import System.Log.Handler.Syslog (openlog, Option(PID), Facility(..))
import System.Log.Logger (clearLevel, getLevel, getLogger, logM, Priority(..), rootLoggerName, saveGlobalLogger, setHandlers, setLevel, updateGlobalLogger)
import System.Process (readProcess)

  -- unwords [formatTimeCombined time, msg]

-- | To get logging in /var/log/appraisalscribe3.log, create that file and
-- modify this line in /etc/rsyslog.d/50-default.conf
-- @@
--    *.*;auth,authpriv.none -/var/log/syslog
-- @@
-- so it looks like this
-- @@
--    *.*;auth,authpriv.none;local1.none -/var/log/syslog
--    local1.* /var/log/appraisalscribe.log
-- @@
-- Then run "service rsyslog restart".
setupServerLogger :: Facility -> Priority -> IO ()
setupServerLogger facility lvl = do
  configureServerLogger
  appLog <- openlog "appraisalscribe3" [PID] facility lvl
  updateGlobalLogger rootLoggerName (setLevel lvl . setHandlers [appLog])

setServerLoggingLevel :: String -> Maybe Priority -> IO ()
setServerLoggingLevel name new = do
  logger <- getLogger name
  case getLevel logger of
    old | old == new -> pure ()
    _ -> do
      saveGlobalLogger (maybe clearLevel setLevel new $ logger)
      alog ALERT
        ((case getLevel logger of
            Nothing -> "Setting";
            Just old -> "Changed") <>
         (if name == rootLoggerName then "" else " logger " <> name) <>
         " server logging level" <>
         (case getLevel logger of
            Nothing -> ""
            Just old -> " from " <> show old) <>
          " to " <> show new)

printServerLogger :: MonadIO m => m ()
printServerLogger = liftIO $ do
  logger <- getLogger rootLoggerName
  putStrLn ("Server log level set to " ++ show (getLevel logger))

-- | This is written for debian - it needs to be adapted for nix.
configureServerLogger :: IO ()
configureServerLogger = do
  isNIXOS  <- doesFileExist "/etc/NIXOS"
  isDarwin <- doesFileExist "/etc/bashrc_Apple_Terminal"
  case (isNIXOS || isDarwin) of
    True -> do
      hPutStrLn stderr "On NIXOS/Darwin - skipping rsyslog config"
    False -> do
      let path = "/etc/rsyslog.d/30-appraisalscribe3-development.conf"
          text = unlines ["local2.* /var/log/appraisalscribe3-development.log", "& ~"]
      exists <- doesFileExist path
      case exists of
        True -> return ()
        False -> do
          result1 <- try (writeFile path text) :: IO (Either IOError ())
          case result1 of
            Left e -> do
              hPutStrLn stderr ("Could not create file " ++ path ++ " containing " ++ show text ++ ": " ++ show e)
              hPutStrLn stderr "Run this server with sudo to create it and restart the log daemon."
              exitWith $ ExitFailure 1
            Right () -> do
              hPutStrLn stderr $ "Success creating " ++ show path
              result2 <- try (readProcess "sudo" ["service", "rsyslog", "restart"] "") :: IO (Either IOError String)
              case result2 of
                Left e -> do
                  hPutStrLn stderr ("Could not restart rsyslog file " ++ path ++ " containing " ++ show text ++ ": " ++ show e)
                  exitWith $ ExitFailure 2
                Right _s -> do
                  hPutStrLn stderr "Success restarting rsyslog daemon"
                  exitWith ExitSuccess

{-
-- | Create an expression of type (MonadIO m => Priority -> m a -> m
-- a) that we can apply to an expression so that it catches, logs, and
-- rethrows any exception.
logException :: ExpQ
logException =
    [| \priority action ->
         action `catchError` (\e -> do
                                liftIO (logM (loc_module $__LOC__)
                                             priority
                                             ("Logging exception: " <> (pprint $__LOC__) <> " -> " ++ show e))
                                throwError e) |]

__LOC__ :: Q Exp
__LOC__ = TH.lift =<< location
-}
{-
logQ :: ExpQ
logQ = do
  loc <- location
  [|\priority message ->
       alog $(TH.lift (show (loc_module loc))) priority
         ($(TH.lift (show (loc_module loc) <> ":" <> show (fst (loc_start loc)))) <> " - " <> message)|]
-}
