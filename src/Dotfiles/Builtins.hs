module Dotfiles.Builtins
  ( randomString
  , devicePartition
  , device
  , decryptSecret
  )
where

import Control.Monad (replicateM, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Data.String.Utils (strip)
import Dotfiles.Shell (readShell)
import Flow
import System.IO.Temp (emptySystemTempFile)
import System.Random (randomRIO)

randomString :: (MonadIO m) => Int -> m String
randomString length' = do
  let chars = ['a' .. 'z'] ++ ['0' .. '9']

  liftIO
    <| replicateM length'
    <| do
      index <- randomRIO (0, length chars - 1)
      return <| chars !! index

devicePartition :: (MonadFail m, MonadLogger m, MonadUnliftIO m) => FilePath -> m String
devicePartition path = do
  (_, stdout, _) <- readShell <| "findmnt -no source -T " ++ path

  return <| strip stdout

device :: (MonadFail m, MonadLogger m, MonadUnliftIO m) => FilePath -> m String
device path = do
  part <- devicePartition path
  (_, stdout, _) <- readShell <| "lsblk -dpno pkname " ++ part

  return <| strip stdout

decryptSecret :: (MonadFail m, MonadLogger m, MonadUnliftIO m) => FilePath -> FilePath -> m String
decryptSecret identity secret = do
  tmp <- liftIO <| emptySystemTempFile "decrypt-secret"

  void
    <| readShell
    <| unwords
      [ "age -d -i"
      , identity
      , "-o"
      , tmp
      , secret
      ]

  return tmp
