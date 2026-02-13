module Dotfiles.Application
  ( RSApp
  , RApp
  , SApp
  , App
  , ParserInfoMod
  , runRSAppWithParser
  , runRSApp
  , runRAppWithParser
  , runRApp
  , runSApp'
  , runSApp
  , runApp
  )
where

import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Logger.Extras (Logger, runLoggerLoggingT)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Version (showVersion)
import Flow
import Options.Applicative
import Paths_dotfiles (version)
import System.Environment (getProgName)

newtype RSApp r s a = RSApp
  { unRSApp :: LoggingT (ReaderT (r, TVar s) IO) a
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
    , MonadReader (r, TVar s)
    , MonadThrow
    , MonadUnliftIO
    )

type RApp r a = RSApp r () a
type SApp s a = RSApp () s a
type App a = RApp () a

type ParserInfoMod a = (Parser a, InfoMod a)

runRSAppWithParser :: RSApp r s a -> Logger -> App (ParserInfoMod r) -> RApp r s -> IO a
runRSAppWithParser f logger rf sf = do
  progName <- getProgName

  (rP, rInfoMod) <- runApp rf logger

  let rInfo =
        info
          ( rP
              <**> helper
              <**> ( simpleVersioner
                       <| unwords [progName, showVersion version]
                   )
          )
          ( fullDesc
              <> header ("Dotfiles " ++ progName)
              <> progDesc "a Dotfiles application"
              <> footer "(c) Dotfiles <hanselrd>"
              <> rInfoMod
          )

  r <- execParser rInfo

  s <- runRApp sf logger r

  runRSApp f logger r s
  where
    runRSApp f logger r s = do
      sTVar <- liftIO <| newTVarIO s

      f
        |> unRSApp
        |> flip runLoggerLoggingT logger
        |> flip runReaderT (r, sTVar)

    runRApp f logger r = runRSApp f logger r ()
    runApp f logger = runRApp f logger ()

runRSApp :: RSApp r s a -> Logger -> r -> s -> IO a
runRSApp f logger r s = runRSAppWithParser f logger (return (pure r, mempty)) (return s)

runRAppWithParser :: RApp r a -> Logger -> App (ParserInfoMod r) -> IO a
runRAppWithParser f logger rf = runRSAppWithParser f logger rf (return ())

runRApp :: RApp r a -> Logger -> r -> IO a
runRApp f logger r = runRSApp f logger r ()

runSApp' :: SApp s a -> Logger -> App s -> IO a
runSApp' f logger sf = runRSAppWithParser f logger (return (pure (), mempty)) sf

runSApp :: SApp s a -> Logger -> s -> IO a
runSApp f logger s = runRSApp f logger () s

runApp :: App a -> Logger -> IO a
runApp f logger = runRApp f logger ()
