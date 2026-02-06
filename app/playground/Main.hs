module Main (main) where

import Control.Monad.Logger.CallStack (logDebugN)
import Control.Monad.Logger.Extras (colorize, logToStderr, runLoggerLoggingT)
import Data.Text (pack)
import System.Info (os)

main :: IO ()
main = do
  let logger = colorize logToStderr

  flip runLoggerLoggingT logger $ do
    logDebugN "playground"

    logDebugN $ "os= " <> pack os
