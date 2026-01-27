module Main (main) where

import Control.Monad (forM_, void)
import Control.Monad.Logger.CallStack (logDebugN)
import Control.Monad.Logger.Extras (colorize, logToStderr, runLoggerLoggingT)
import Data.List.Split (splitOn)
import Data.Text (pack)
import qualified Dotfiles.Nix (darwinHosts, homes, nixosHosts)
import qualified Dotfiles.Shell (readShell, readShellWithStdin)
import Text.RawString.QQ

main :: IO ()
main = do
  let logger = colorize logToStderr
  flip runLoggerLoggingT logger $ do
    (_, stdout, _) <- Dotfiles.Shell.readShell [r|git grep -Po "[Hh]ash\s*=\s*\K(\"sha256-.{43}=\"|lib\.fakeHash)" -- ':!flake.lock'|]

    forM_
      (map (splitOn ":") $ lines stdout)
      $ \x -> Dotfiles.Shell.readShell $ "echo \"" ++ (x !! 0) ++ "\""

    void $ Dotfiles.Shell.readShellWithStdin "wc -c" "hello, world!"
    void $ Dotfiles.Shell.readShell [r|printf "hello\nworld\nfrom\nupdate-hashes"|]
    void $ Dotfiles.Shell.readShell [r|printf "hello\nworld\nfrom\nupdate-hashes\n"|]

    logDebugN $
      "nixosHosts= " <> (pack $ show Dotfiles.Nix.nixosHosts)
    logDebugN $
      "darwinHosts= " <> (pack $ show Dotfiles.Nix.darwinHosts)
    logDebugN $
      "homes= " <> (pack $ show Dotfiles.Nix.homes)

    void $ Dotfiles.Shell.readShell "nix run .#update-hashes -- --help"
    -- forM_
    --   Dotfiles.Nix.nixosHosts
    --   $ \x -> Dotfiles.Shell.readShell $ "nh os build . -H " ++ x ++ " --impure --no-nom"
    -- forM_
    --   Dotfiles.Nix.darwinHosts
    --   $ \x -> Dotfiles.Shell.readShell $ "nh darwin build . -H " ++ x ++ " --impure --no-nom"
    forM_
      Dotfiles.Nix.homes
      $ \x -> Dotfiles.Shell.readShell $ "nh home build . -c " ++ x ++ " --impure --no-nom"
