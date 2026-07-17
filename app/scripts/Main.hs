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
#if defined(SECRETS)
import qualified Dotfiles.Secrets.Scripts as DSS (workInit)
#endif

{- FOURMOLU_DISABLE -}
main :: IO ()
main = do
  flip DA.runApp (colorize logToStderr) <| do
    let scripts =
          [ ("scripts/nix-config.sh", DS.nixConfig)
          , ("scripts/nix-chroot.sh", DS.nixChroot)
          , ("scripts/nix-install.sh", DS.nixInstall)
#if defined(SECRETS)
          , ("secrets/modules/home/work/init2.sh", DSS.workInit)
#endif
          ]

    forM_
      scripts
      <| \(path, script') -> do
        logDebugN
          <| "script= " <> (pack path)

        liftIO
          <| TIO.writeFile path
          <| script script'

        liftIO
          <| setFileMode path
          <| ownerModes
            .|. groupReadMode
            .|. groupExecuteMode
            .|. otherReadMode
            .|. otherExecuteMode
{- FOURMOLU_ENABLE -}
