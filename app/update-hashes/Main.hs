module Main (main) where

import Control.Monad (foldM_, forM_)
import Control.Monad.Logger (logDebugN)
import Data.List.Split (splitOn)
import Data.Text (pack)
import qualified Dotfiles.Application as DA (App, runApp)
import qualified Dotfiles.Nix as DN
  ( fakeHash
  , supportedDarwinHosts
  , supportedHomes
  , supportedNixosHosts
  )
import qualified Dotfiles.Shell as DS (readShell)
import Flow
import Text.RawString.QQ
import Text.Regex.TDFA

processCommand :: Int -> String -> DA.App () Int
processCommand 0 _ = return 0
processCommand count cmd = do
  (_, stdout, stderr) <- DS.readShell cmd

  let regex = [r|specified: (.*)[[:space:]]*got:    (.*)[[:space:]]|] :: String

      matches :: [[String]]
      matches = (stdout ++ stderr) =~ regex

  forM_
    matches
    <| \x -> do
      logDebugN
        <| "match= " <> pack (show x)

      let oldHash = x !! 1
          newHash = x !! 2

      DS.readShell
        <| [r|git grep -Pl "[Hh]ash\s*=\s*\K(\"sha256-.{43}=\"|lib\.fakeHash)" -- ':!flake.lock'|]
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
  DA.runApp () <| do
    (_, stdout, _) <-
      DS.readShell [r|git grep -Po "[Hh]ash\s*=\s*\K(\"sha256-.{43}=\"|lib\.fakeHash)" -- ':!flake.lock'|]

    forM_
      (map (splitOn ":") <| lines stdout)
      <| \x -> do
        let file = x !! 0
            lineNr = x !! 1
            oldHash = x !! 2

        newHash <- DN.fakeHash
        DS.readShell
          <| "sed -i '"
            ++ lineNr
            ++ "s@"
            ++ oldHash
            ++ "@\""
            ++ newHash
            ++ "\"@' "
            ++ file

    let count = length <| lines stdout
        cmds =
          "nix run .#canary"
            : concat
              [ (map (\x -> "nh os build . -H " ++ x ++ " --impure --no-nom") DN.supportedNixosHosts)
              , (map (\x -> "nh darwin build . -H " ++ x ++ " --impure --no-nom") DN.supportedDarwinHosts)
              , (map (\x -> "nh home build . -c " ++ x ++ " --impure --no-nom") DN.supportedHomes)
              ]

    foldM_ processCommand count cmds
