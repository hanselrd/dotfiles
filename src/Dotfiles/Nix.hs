module Dotfiles.Nix (nixosHosts, darwinHosts, homes, fakeHash) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger.CallStack (runNoLoggingT)
import Data.Text (pack, strip, unpack)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Dotfiles.Shell (readShell)
import System.Exit (ExitCode (..))
import System.IO (hFlush, hPutStrLn)
import System.IO.Temp (withSystemTempFile)
import System.IO.Unsafe (unsafePerformIO)

configurations :: String -> [String]
configurations name = unsafePerformIO $ do
  runNoLoggingT $ do
    (exitCode, stdout, _) <- readShell $ "nix eval .#" ++ name ++ "Configurations --apply \"x: builtins.attrNames x\" --json | jq -r \".[]\""

    if exitCode /= ExitSuccess
      then error $ "failed to evaluate " ++ name ++ " configurations"
      else
        return $
          lines stdout

nixosHosts :: [String]
nixosHosts = configurations "nixos"

darwinHosts :: [String]
darwinHosts = configurations "darwin"

homes :: [String]
homes = configurations "home"

fakeHash :: (MonadUnliftIO m, MonadFail m, MonadMask m) => m String
fakeHash = withSystemTempFile "fake-hash.txt" $ \path fileH -> do
  runNoLoggingT $ do
    liftIO $ do
      posixTime <- getPOSIXTime
      hPutStrLn fileH $ show posixTime
      hFlush fileH

    (exitCode, stdout, _) <- readShell $ "nix hash file " ++ path

    if exitCode /= ExitSuccess
      then error "failed to generate fake hash"
      else
        return $
          unpack $
            strip $
              pack stdout
