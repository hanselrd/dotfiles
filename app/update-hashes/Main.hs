module Main (main) where

import Control.Monad.Logger.CallStack (logDebugN)
import Control.Monad.Logger.Extras (colorize, logToStderr, runLoggerLoggingT)
import Data.Text (pack)
import qualified Dotfiles.Nix (darwinHosts, homes, nixosHosts)
import qualified Dotfiles.Shell (readShell, readShellWithStdin)

main :: IO ()
main = do
  let logger = colorize logToStderr
  flip runLoggerLoggingT logger $ do
    _ <- Dotfiles.Shell.readShell "echo \"Hello from update-hashes!\""
    _ <- Dotfiles.Shell.readShellWithStdin "wc -m" "hello"
    _ <- Dotfiles.Shell.readShellWithStdin "wc -c" "hello"
    logDebugN $ "nixosHosts= " <> (pack $ show Dotfiles.Nix.nixosHosts)
    logDebugN $ "darwinHosts= " <> (pack $ show Dotfiles.Nix.darwinHosts)
    logDebugN $ "homes= " <> (pack $ show Dotfiles.Nix.homes)
