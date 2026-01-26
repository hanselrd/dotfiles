module Dotfiles.Shell (readShellWithStdin, readShell) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger.CallStack (MonadLogger, logDebugN, logInfoN)
import Data.Text (pack)
import System.Exit (ExitCode)
import System.Process (readProcessWithExitCode)

readShellWithStdin :: (MonadIO m, MonadLogger m) => String -> String -> m (ExitCode, String, String)
readShellWithStdin cmd stdin = do
  (when $ not $ null stdin) $
    logDebugN $
      "cmdIn= " <> pack stdin

  logInfoN $ "cmdLine= " <> pack cmd

  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode "bash" ["--norc", "--noprofile", "-c", cmd] stdin

  (when $ not $ null stdout)
    $ mapM_
      (\x -> logDebugN $ "cmdOut= " <> pack x)
    $ lines stdout
  (when $ not $ null stderr)
    $ mapM_
      (\x -> logDebugN $ "cmdErr= " <> pack x)
    $ lines stderr
  logDebugN $ "cmdRc= " <> (pack $ show exitCode)

  pure (exitCode, stdout, stderr)

readShell :: (MonadIO m, MonadLogger m) => String -> m (ExitCode, String, String)
readShell = flip readShellWithStdin ""
