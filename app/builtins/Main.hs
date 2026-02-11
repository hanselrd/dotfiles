module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logDebugN)
import Control.Monad.Reader (ask)
import Data.String.Combinators (doubleQuotes)
import Data.String.Utils (strip)
import Data.Text (pack)
import qualified Dotfiles.Application as DA (App, runAppWithParser)
import qualified Dotfiles.Builtins as DB (decryptSecret, device, devicePartition, randomString)
import qualified Dotfiles.Shell as DS (readShell)
import Flow
import Options.Applicative

data Builtin
  = RandomString {length' :: Int}
  | DevicePartition {path :: FilePath}
  | Device {path :: FilePath}
  | DecryptSecret {identity :: FilePath, secret :: FilePath}
  deriving (Eq, Read, Show)

data Options = Options
  { builtin :: Builtin
  }
  deriving (Eq, Read, Show)

builtinP :: Parser Builtin
builtinP =
  subparser
    <| command
      "random-string"
      ( info
          ( ( RandomString
                <$> option
                  auto
                  ( long "length"
                      <> metavar "LENGTH"
                      <> help "Length of random string"
                  )
            )
              <**> helper
          )
          (progDesc "Generate random string of specified length")
      )
      <> command
        "device-partition"
        ( info
            ( ( DevicePartition
                  <$> strOption
                    ( long "path"
                        <> metavar "PATH"
                        <> help "Path to get device partition"
                    )
              )
                <**> helper
            )
            (progDesc "Device partition of path")
        )
      <> command
        "device"
        ( info
            ( ( Device
                  <$> strOption
                    ( long "path"
                        <> metavar "PATH"
                        <> help "Path to get device"
                    )
              )
                <**> helper
            )
            (progDesc "Device of path")
        )
      <> command
        "decrypt-secret"
        ( info
            ( ( DecryptSecret
                  <$> strOption
                    ( long "identity"
                        <> metavar "IDENTITY"
                        <> help "Identity to decrypt secret"
                    )
                  <*> strOption
                    ( long "secret"
                        <> metavar "SECRET"
                        <> help "Secret to decrypt"
                    )
              )
                <**> helper
            )
            (progDesc "Decrypt secret")
        )

optionsP :: DA.App () (Parser Options, Maybe (InfoMod Options))
optionsP = do
  return
    ( Options <$> builtinP
    , Just <| progDesc "Builtins for nix"
    )

main :: IO ()
main = do
  flip DA.runAppWithParser optionsP <| do
    opts <- ask
    logDebugN
      <| "opts= " <> pack (show opts)

    case opts.builtin of
      RandomString {..} -> do
        str <- DB.randomString length'

        liftIO
          <| putStr
          <| doubleQuotes str
      DevicePartition {..} -> do
        part <- DB.devicePartition path

        liftIO
          <| putStr
          <| doubleQuotes part
      Device {..} -> do
        dev <- DB.device path

        liftIO
          <| putStr
          <| doubleQuotes dev
      DecryptSecret {..} -> do
        path <- DB.decryptSecret identity secret

        liftIO
          <| putStr path
