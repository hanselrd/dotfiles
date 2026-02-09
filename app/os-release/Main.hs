module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logDebugN)
import Data.Text (pack)
import qualified Dotfiles.Application as DA (runApp)
import Flow
import System.OsRelease

main :: IO ()
main = do
  DA.runApp "os-release" () <| do
    Just (OsRelease {..}) <- fmap osRelease <$> liftIO parseOsRelease
    logDebugN <| "name= " <> pack name
    logDebugN <| "id= " <> pack id
    logDebugN <| "pretty_name= " <> pack pretty_name
