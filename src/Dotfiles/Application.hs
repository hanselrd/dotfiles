module Dotfiles.Application (App (..), runApp) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, logDebugN)
import Control.Monad.Logger.Extras (colorize, logToStderr, runLoggerLoggingT)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Text (pack)
import Flow

newtype App r a = App
  { unApp :: LoggingT (ReaderT r IO) a
  }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadFail
    , MonadIO
    , MonadLogger
    , MonadMask
    , MonadReader r
    , MonadThrow
    , MonadUnliftIO
    )

runApp :: String -> r -> App r a -> IO a
runApp name r (App action) = do
  let logger = colorize logToStderr

  flip runReaderT r
    <| flip runLoggerLoggingT logger
    <| do
      logDebugN <| "app= " <> pack name

      action
