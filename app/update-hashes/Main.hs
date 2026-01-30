module Main (main) where

import Control.Monad (foldM_, forM_)
import Control.Monad.Logger.CallStack (LoggingT, logDebugN)
import Control.Monad.Logger.Extras (colorize, logToStderr, runLoggerLoggingT)
import Data.List.Split (splitOn)
import Data.Text (pack)
import qualified Dotfiles.Nix (darwinHosts, fakeHash, homes, nixosHosts)
import qualified Dotfiles.Shell (readShell)
import Text.RawString.QQ
import Text.Regex.TDFA

processCommand :: Int -> String -> LoggingT IO Int
processCommand 0 _ = return 0
processCommand count cmd = do
  (_, stdout, stderr) <- Dotfiles.Shell.readShell cmd

  let regex = [r|specified: (.*)[[:space:]]*got:    (.*)[[:space:]]|] :: String

      matches :: [[String]]
      matches = (stdout ++ stderr) =~ regex

  forM_
    matches
    $ \x -> do
      logDebugN $ "match= " <> pack (show x)

      let oldHash = x !! 1
          newHash = x !! 2

      Dotfiles.Shell.readShell $
        [r|git grep -Pl "[Hh]ash\s*=\s*\K(\"sha256-.{43}=\"|lib\.fakeHash)" -- ':!flake.lock'|]
          ++ " | xargs sed -i 's@"
          ++ oldHash
          ++ "@"
          ++ newHash
          ++ "@g'"

  if length matches == 0
    then return count
    else processCommand (count - (length matches)) cmd

main :: IO ()
main = do
  let logger = colorize logToStderr

  flip runLoggerLoggingT logger $ do
    (_, stdout, _) <- Dotfiles.Shell.readShell [r|git grep -Po "[Hh]ash\s*=\s*\K(\"sha256-.{43}=\"|lib\.fakeHash)" -- ':!flake.lock'|]

    forM_
      (map (splitOn ":") $ lines stdout)
      $ \x -> do
        let file = x !! 0
            lineNr = x !! 1
            oldHash = x !! 2

        newHash <- Dotfiles.Nix.fakeHash
        Dotfiles.Shell.readShell $
          "sed -i '"
            ++ lineNr
            ++ "s@"
            ++ oldHash
            ++ "@\""
            ++ newHash
            ++ "\"@' "
            ++ file

    let count = length $ lines stdout
        cmds =
          "nix run .#canary"
            : concat
              [ (map (\x -> "nh os build . -H " ++ x ++ " --impure --no-nom") Dotfiles.Nix.nixosHosts),
                (map (\x -> "nh darwin build . -H " ++ x ++ " --impure --no-nom") Dotfiles.Nix.darwinHosts),
                (map (\x -> "nh home build . -c " ++ x ++ " --impure --no-nom") Dotfiles.Nix.homes)
              ]

    foldM_ processCommand count cmds
