module Dotfiles.Nix
  ( system
  , nixosHosts
  , darwinHosts
  , homes
  , supportedNixosHosts
  , supportedDarwinHosts
  , supportedHomes
  , fakeHash
  )
where

import Control.Monad (filterM)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.String.Utils (strip)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Dotfiles.Shell (readShell)
import Flow
import System.Exit (ExitCode (..))
import System.IO (hFlush, hPutStrLn)
import System.IO.Temp (withSystemTempFile)
import System.IO.Unsafe (unsafePerformIO)

system :: String
system =
  unsafePerformIO <| do
    runNoLoggingT <| do
      (exitCode, stdout, _) <- readShell <| "nix eval --expr \"builtins.currentSystem\" --impure --raw"

      if exitCode /= ExitSuccess
        then error "failed to evaluate system"
        else
          return
            <| strip stdout

configurations :: String -> [String]
configurations name =
  unsafePerformIO <| do
    runNoLoggingT <| do
      (exitCode, stdout, _) <-
        readShell
          <| "nix eval .#" ++ name ++ "Configurations --apply \"x: builtins.attrNames x\" --json | jq -r \".[]\""

      if exitCode /= ExitSuccess
        then error <| "failed to evaluate " ++ name ++ " configurations"
        else
          return
            <| lines stdout

nixosHosts :: [String]
nixosHosts = configurations "nixos"

darwinHosts :: [String]
darwinHosts = configurations "darwin"

homes :: [String]
homes = configurations "home"

supportedConfigurations :: String -> [String]
supportedConfigurations name =
  unsafePerformIO <| do
    runNoLoggingT
      <| do
        filterM
          ( \x -> do
              (exitCode, stdout, _) <-
                readShell
                  <| "nix eval .#"
                    ++ name
                    ++ "Configurations."
                    ++ x
                    ++ ".config.nixpkgs.hostPlatform.system --impure --raw"

              if exitCode == ExitSuccess
                then
                  return
                    <| strip stdout == system
                else do
                  (exitCode, stdout, _) <-
                    readShell
                      <| "nix eval .#" ++ name ++ "Configurations." ++ x ++ ".config.nixpkgs.system --impure --raw"

                  if exitCode /= ExitSuccess
                    then error <| "failed to evaluate supported " ++ name ++ " configurations"
                    else
                      return
                        <| strip stdout == system
          )
      <| configurations name

supportedNixosHosts :: [String]
supportedNixosHosts = supportedConfigurations "nixos"

supportedDarwinHosts :: [String]
supportedDarwinHosts = supportedConfigurations "darwin"

supportedHomes :: [String]
supportedHomes = supportedConfigurations "home"

fakeHash :: (MonadFail m, MonadMask m, MonadUnliftIO m) => m String
fakeHash =
  withSystemTempFile "fake-hash.txt" <| \path fileH -> do
    runNoLoggingT <| do
      liftIO <| do
        posixTime <- getPOSIXTime
        hPutStrLn fileH <| show posixTime
        hFlush fileH

      (exitCode, stdout, _) <- readShell <| "nix hash file " ++ path

      if exitCode /= ExitSuccess
        then error "failed to generate fake hash"
        else
          return
            <| strip stdout
