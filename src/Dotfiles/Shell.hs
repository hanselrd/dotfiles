module Dotfiles.Shell (readShellWithStdin, readShell) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger.CallStack (MonadLogger, logInfoN)
import Data.Text (pack)
import System.Exit (ExitCode)
import System.IO (BufferMode (..), Handle, hClose, hGetLine, hIsEOF, hPutStr, hSetBuffering)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import UnliftIO.Async (async, wait)

hStreamLogCapture :: (MonadIO m, MonadLogger m) => String -> Handle -> m [String]
hStreamLogCapture prefix stdH = loop []
  where
    loop acc = do
      eof <- liftIO $ hIsEOF stdH
      if eof
        then return $ reverse acc
        else do
          line <- liftIO $ hGetLine stdH
          logInfoN $
            pack prefix <> "= " <> pack line
          loop $ line : acc

readShellWithStdin :: (MonadUnliftIO m, MonadFail m, MonadLogger m) => String -> String -> m (ExitCode, String, String)
readShellWithStdin cmd stdin = do
  logInfoN $
    "cmdLine= " <> pack cmd

  (Just stdinH, Just stdoutH, Just stderrH, processH) <-
    liftIO $
      createProcess
        (proc "bash" ["--norc", "--noprofile", "-c", cmd])
          { std_in = CreatePipe,
            std_out = CreatePipe,
            std_err = CreatePipe
          }

  (when $ not $ null stdin) $ do
    logInfoN $
      "cmdIn= " <> pack stdin
    liftIO $
      hPutStr stdinH stdin

  liftIO $ hSetBuffering stdoutH LineBuffering
  liftIO $ hSetBuffering stderrH LineBuffering

  asyncStdout <- async $ hStreamLogCapture "cmdOut" stdoutH
  asyncStderr <- async $ hStreamLogCapture "cmdErr" stderrH

  liftIO $ hClose stdinH

  stdout <- wait asyncStdout
  stderr <- wait asyncStderr

  exitCode <- liftIO $ waitForProcess processH
  logInfoN $
    "cmdRc= " <> (pack $ show exitCode)

  return (exitCode, unlines stdout, unlines stderr)

readShell :: (MonadUnliftIO m, MonadFail m, MonadLogger m) => String -> m (ExitCode, String, String)
readShell = flip readShellWithStdin ""
