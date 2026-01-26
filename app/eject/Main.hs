module Main (main) where

import Options.Applicative

data Options = Options
  { home :: String,
    -- hashLen :: Int,
    outDir :: FilePath
  }
  deriving (Show, Eq)

optionsP :: Parser Options
optionsP =
  Options
    <$> strOption
      ( long "home"
          <> metavar "HOME"
          <> help "Home name"
          <> value "basic"
          <> showDefault
      )
    -- <*> option
    --   auto
    --   ( long "hash-len"
    --       <> metavar "HASH_LEN"
    --       <> help "Hash length"
    --       <> value 5
    --       <> showDefault
    --   )
    <*> strOption
      ( long "out-dir"
          <> metavar "OUT_DIR"
          <> help "Output directory"
          <> value "~/.nix/x/abcde"
          <> showDefault
      )

optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (optionsP <**> helper)
    ( fullDesc
        <> progDesc "Eject home to output directory"
        <> header "Dotfiles Eject"
        <> footer "(c) Dotfiles <hanselrd>"
    )

main :: IO ()
main = execParser optionsInfo >>= print
