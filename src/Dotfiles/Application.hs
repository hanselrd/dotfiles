module Dotfiles.Application (App (..), runApp, runAppWithParser) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Logger.Extras (colorize, logToStderr, runLoggerLoggingT)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Flow
import Options.Applicative

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

runApp :: r -> App r a -> IO a
runApp r (App action) = do
  let logger = colorize logToStderr

  action
    |> flip runLoggerLoggingT logger
    |> flip runReaderT r

runAppWithParser :: App () (Parser r) -> App (Parser r) (ParserInfo r) -> App r a -> IO a
runAppWithParser rP rInfo action = do
  rP <- runApp () rP
  rInfo <- runApp rP rInfo
  r <- execParser rInfo

  runApp r action
