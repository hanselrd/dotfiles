module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger.CallStack (logDebugN)
import Control.Monad.Logger.Extras (colorize, logToStderr, runLoggerLoggingT)
import Data.Text (pack)
import Flow
import System.OsRelease

main :: IO ()
main = do
  let logger = colorize logToStderr

  flip runLoggerLoggingT logger <| do
    logDebugN "os-release"

    Just (OsRelease {..}) <- fmap osRelease <$> liftIO parseOsRelease
    logDebugN <| "name= " <> pack name
    logDebugN <| "id= " <> pack id
    logDebugN <| "pretty_name= " <> pack pretty_name
