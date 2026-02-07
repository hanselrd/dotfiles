module Main (main) where

import Control.Monad.Logger.CallStack (logDebugN)
import Control.Monad.Logger.Extras (colorize, logToStderr, runLoggerLoggingT)
import Data.Text (pack)
import qualified Dotfiles.Nix (darwinHosts, homes, nixosHosts, supportedDarwinHosts, supportedHomes, supportedNixosHosts, system)
import Flow

main :: IO ()
main = do
  let logger = colorize logToStderr

  flip runLoggerLoggingT logger <| do
    logDebugN "playground"

    logDebugN <| "nix.system= " <> pack Dotfiles.Nix.system
    logDebugN <| "nix.nixosHosts= " <> pack (show Dotfiles.Nix.nixosHosts)
    logDebugN <| "nix.darwinHosts= " <> pack (show Dotfiles.Nix.darwinHosts)
    logDebugN <| "nix.homes= " <> pack (show Dotfiles.Nix.homes)
    logDebugN <| "nix.supportedNixosHosts= " <> pack (show Dotfiles.Nix.supportedNixosHosts)
    logDebugN <| "nix.supportedDarwinHosts= " <> pack (show Dotfiles.Nix.supportedDarwinHosts)
    logDebugN <| "nix.supportedHomes= " <> pack (show Dotfiles.Nix.supportedHomes)

    logDebugN <|
      "<100= "
        <> ( [100, 99 ..]
               |> take @Int 5
               |> show
               |> pack
           )
