module Main (main) where

import Control.Monad (foldM_, forM_, void)
import Control.Monad.Logger (logDebugN)
import Control.Monad.Logger.Extras (colorize, logToStderr)
import Data.List.Split (splitOn)
import Data.String.Combinators (doubleQuotes)
import Data.String.Utils (strip)
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

processCommand :: Int -> String -> DA.App Int
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
        <| [r|git grep -Pl "[Hh]ash\s*=\s*\K(\"sha256-.{43}=\"|lib\.fakeHash)"|]
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
  flip DA.runApp (colorize logToStderr) <| do
    let regex = [r|=\s*(.*)\s*;\s*#\s*github:(.*)\/(.*)\/(.*)|]

    (_, stdout, _) <-
      DS.readShell
        <| "git grep -Po \""
          ++ regex
          ++ [r|" | perl -nle 'print "$1:$2:$3:$4:$5:$6" if /(.*):(.*):\s*|]
          ++ regex
          ++ "/'"

    forM_
      (map (splitOn ":") <| lines stdout)
      <| \x -> do
        let file = x !! 0
            lineNr = x !! 1
            oldRev = x !! 2
            owner = x !! 3
            repo = x !! 4
            branch = x !! 5

        (_, stdout, _) <-
          DS.readShell
            <| "git ls-remote https://github.com/"
              ++ owner
              ++ "/"
              ++ repo
              ++ ".git "
              ++ branch
              ++ " | awk '{print $1}'"

        let newRev = doubleQuotes <| strip stdout

        if oldRev == newRev
          then return ()
          else
            void
              <| DS.readShell
              <| "sed -i '"
                ++ lineNr
                ++ "s/"
                ++ oldRev
                ++ "/"
                ++ newRev
                ++ "/' "
                ++ file

    (_, stdout, _) <-
      DS.readShell [r|git grep -Po "[Hh]ash\s*=\s*\K(\"sha256-.{43}=\"|lib\.fakeHash)"|]

    forM_
      (map (splitOn ":") <| lines stdout)
      <| \x -> do
        let file = x !! 0
            lineNr = x !! 1
            oldHash = x !! 2

        newHash <- fmap doubleQuotes DN.fakeHash
        DS.readShell
          <| "sed -i '"
            ++ lineNr
            ++ "s@"
            ++ oldHash
            ++ "@"
            ++ newHash
            ++ "@' "
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
