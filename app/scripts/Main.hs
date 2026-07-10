module Main (main) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logDebugN)
import Control.Monad.Logger.Extras (colorize, logToStderr)
import Control.Monad.Shell (script)
import Data.Bits ((.|.))
import Data.Text (pack)
import qualified Data.Text.Lazy.IO as TIO (writeFile)
import qualified Dotfiles.Application as DA (runApp)
import qualified Dotfiles.Scripts as DS (nixChroot, nixConfig, nixInstall)
import Flow
import System.Posix.Files
  ( groupExecuteMode
  , groupReadMode
  , otherExecuteMode
  , otherReadMode
  , ownerModes
  , setFileMode
  )

main :: IO ()
main = do
  flip DA.runApp (colorize logToStderr) <| do
    let scripts =
          [ ("nix-config.sh", DS.nixConfig)
          , ("nix-chroot.sh", DS.nixChroot)
          , ("nix-install.sh", DS.nixInstall)
          ]

    forM_
      scripts
      <| \(name, script') -> do
        logDebugN
          <| "script= " <> (pack name)

        let scriptPath = "scripts/" ++ name

        liftIO
          <| TIO.writeFile scriptPath
          <| script script'

        liftIO
          <| setFileMode scriptPath
          <| ownerModes
            .|. groupReadMode
            .|. groupExecuteMode
            .|. otherReadMode
            .|. otherExecuteMode
