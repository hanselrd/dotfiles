module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logDebugN)
import Control.Monad.Logger.Extras (colorize, logToStderr)
import Data.Text (pack)
import qualified Dotfiles.Application as DA (runApp)
import Flow
import System.OsRelease

main :: IO ()
main = do
  flip DA.runApp (colorize logToStderr) <| do
    Just (OsRelease {..}) <- fmap osRelease <$> liftIO parseOsRelease
    logDebugN <| "name= " <> pack name
    logDebugN <| "id= " <> pack id
    logDebugN <| "pretty_name= " <> pack pretty_name
