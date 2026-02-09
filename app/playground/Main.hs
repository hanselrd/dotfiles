module Main (main) where

import Control.Monad.Logger (logDebugN)
import Data.Text (pack)
import qualified Dotfiles.Application as DA (runApp)
import qualified Dotfiles.Nix as DN
  ( darwinHosts
  , homes
  , nixosHosts
  , supportedDarwinHosts
  , supportedHomes
  , supportedNixosHosts
  , system
  )
import Flow

main :: IO ()
main = do
  flip DA.runApp () <| do
    logDebugN <| "nix.system= " <> pack DN.system
    logDebugN <| "nix.nixosHosts= " <> pack (show DN.nixosHosts)
    logDebugN <| "nix.darwinHosts= " <> pack (show DN.darwinHosts)
    logDebugN <| "nix.homes= " <> pack (show DN.homes)
    logDebugN <| "nix.supportedNixosHosts= " <> pack (show DN.supportedNixosHosts)
    logDebugN <| "nix.supportedDarwinHosts= " <> pack (show DN.supportedDarwinHosts)
    logDebugN <| "nix.supportedHomes= " <> pack (show DN.supportedHomes)

    logDebugN
      <| "<100= "
        <> ( [100, 99 ..]
               |> take @Int 5
               |> show
               |> pack
           )
