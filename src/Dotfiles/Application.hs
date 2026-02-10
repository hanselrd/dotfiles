module Dotfiles.Application (App (..), runAppWithParser, runApp) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Logger.Extras (colorize, logToStderr, runLoggerLoggingT)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Version (showVersion)
import Flow
import Options.Applicative
import Paths_dotfiles (version)
import System.Environment (getProgName)

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

runAppWithParser :: App r a -> App () (Parser r) -> IO a
runAppWithParser action rP = do
  rP <- runApp rP ()
  progName <- getProgName

  let rInfo =
        info
          ( rP
              <**> helper
              <**> (simpleVersioner <| unwords [progName, showVersion version])
          )
          ( fullDesc
              <> header ("Dotfiles " ++ progName)
              <> progDesc "a Dotfiles application"
              <> footer "(c) Dotfiles <hanselrd>"
          )

  r <- execParser rInfo

  runApp action r
  where
    logger = colorize logToStderr

    runApp action r =
      action
        |> unApp
        |> flip runLoggerLoggingT logger
        |> flip runReaderT r

runApp :: App r a -> r -> IO a
runApp action r =
  action
    |> ( flip runAppWithParser
           <| return
           <| pure r
       )
