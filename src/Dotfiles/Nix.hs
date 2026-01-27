module Dotfiles.Nix (nixosHosts, darwinHosts, homes) where

import Control.Exception (assert)
import Control.Monad.Logger.CallStack (runNoLoggingT)
import Dotfiles.Shell (readShell)
import System.Exit (ExitCode (..))
import System.IO.Unsafe (unsafePerformIO)

configurations :: String -> [String]
configurations name = unsafePerformIO $ do
  runNoLoggingT $ do
    (exitCode, stdout, _) <- readShell $ "nix eval .#" ++ name ++ "Configurations --apply \"x: builtins.attrNames x\" --json | jq -r \".[]\""
    assert (exitCode == ExitSuccess) $
      return $
        lines stdout

nixosHosts :: [String]
nixosHosts = configurations "nixos"

darwinHosts :: [String]
darwinHosts = configurations "darwin"

homes :: [String]
homes = configurations "home"
