module Main (main) where

import Control.Monad (forM, replicateM, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger.CallStack (logDebugN)
import Control.Monad.Logger.Extras (colorize, logToStderr, runLoggerLoggingT)
import Data.Text (pack, strip, unpack)
import qualified Dotfiles.Nix (homes)
import qualified Dotfiles.Shell (readShell)
import Flow
import Options.Applicative
import System.Directory (getHomeDirectory)
import System.Random (randomRIO)
import UnliftIO.Async (async, wait)

data Options = Options
  { home :: String,
    outDir :: FilePath
  }
  deriving (Show, Eq)

optionsP :: IO (Parser Options)
optionsP = do
  let chars = ['a' .. 'z'] ++ ['0' .. '9']
      hashLen = 5

  homeDir <- getHomeDirectory
  hash <-
    replicateM hashLen <| do
      index <- randomRIO (0, length chars - 1)
      return <| chars !! index

  let outDir = homeDir ++ "/.nix/x/" ++ hash

  return <|
    Options
      <$> strOption
        ( long "home"
            <> metavar "HOME"
            <> help "Home name"
            <> value "basic"
            <> showDefault
        )
      <*> strOption
        ( long "out-dir"
            <> metavar "OUT_DIR"
            <> help "Output directory"
            <> value outDir
            <> showDefault
        )

optionsInfo :: IO (ParserInfo Options)
optionsInfo = do
  optionsP <- optionsP

  return <|
    info
      (optionsP <**> helper)
      ( fullDesc
          <> progDesc "Eject home to output directory"
          <> header "Dotfiles Eject"
          <> footer "(c) Dotfiles <hanselrd>"
      )

main :: IO ()
main = do
  let logger = colorize logToStderr

  flip runLoggerLoggingT logger <| do
    logDebugN <|
      "homes= " <> pack (show Dotfiles.Nix.homes)

    opts <- liftIO <| optionsInfo >>= execParser
    logDebugN <|
      "opts= " <> pack (show opts)

    if not <| elem opts.home Dotfiles.Nix.homes
      then error <| opts.home ++ " is not a valid home configuration"
      else do
        (_, stdout, _) <-
          Dotfiles.Shell.readShell <|
            "nix build --no-link --print-out-paths .#homeConfigurations."
              ++ opts.home
              ++ ".activationPackage --impure"

        let path0 = unpack <| strip <| pack stdout
        logDebugN <|
          "path0= " <> pack path0

        homeDir <- liftIO <| getHomeDirectory
        (_, stdout, _) <-
          Dotfiles.Shell.readShell <|
            "readlink -f "
              ++ homeDir
              ++ "/.nix-profile"

        let path1 = unpack <| strip <| pack stdout
        logDebugN <|
          "path1= " <> pack path1

        (_, stdout, _) <-
          Dotfiles.Shell.readShell <|
            "nix-store -qR "
              ++ unwords [path0, path1]

        let deps = lines stdout
        logDebugN <|
          "deps= " <> pack (show deps)

        let storePrefix = "/nix/store/"
            sedString =
              "s@"
                ++ storePrefix
                ++ ".{"
                ++ show ((length opts.outDir) - (length storePrefix) + 1)
                ++ "}@"
                ++ opts.outDir
                ++ "/@g"
        logDebugN <|
          "sedString= " <> pack sedString

        asyncs <-
          forM
            deps
            <| \x -> do
              async <|
                Dotfiles.Shell.readShell <|
                  "find "
                    ++ x
                    ++ " | cpio -ov | sed -E '"
                    ++ sedString
                    ++ "' | cpio -idmv"
        mapM_ wait asyncs

        void <|
          Dotfiles.Shell.readShell <|
            "chmod -R u+w "
              ++ opts.outDir

        (_, stdout, _) <-
          Dotfiles.Shell.readShell <|
            "echo \""
              ++ path0
              ++ "\" | sed -E '"
              ++ sedString
              ++ "'"

        let path0New = unpack <| strip <| pack stdout
        logDebugN <|
          "path0New= " <> pack path0New

        (_, stdout, _) <-
          Dotfiles.Shell.readShell <|
            "echo \""
              ++ path1
              ++ "\" | sed -E '"
              ++ sedString
              ++ "'"

        let path1New = unpack <| strip <| pack stdout
        logDebugN <|
          "path1New= " <> pack path1New

        void <|
          Dotfiles.Shell.readShell <|
            "cp -a "
              ++ path0New
              ++ "/home-files/. "
              ++ homeDir
              ++ "/"

        void <|
          Dotfiles.Shell.readShell <|
            "ln -snfF "
              ++ path1New
              ++ " "
              ++ homeDir
              ++ "/.nix-profile"

        void <|
          Dotfiles.Shell.readShell <|
            "find "
              ++ path0New
              ++ "/home-files/ -type l > "
              ++ opts.outDir
              ++ "/manifest.txt"
