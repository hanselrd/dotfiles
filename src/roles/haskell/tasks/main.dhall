let TaskPool/become = ../../../Lib/TaskPool/become.partial.dhall

let TaskPool/executeCommands =
      ../../../Lib/TaskPool/executeCommands.partial.dhall

let Privilege = ../../../Lib/Privilege/Enum.partial.dhall

let Shell = ../../../Lib/Shell/Enum.partial.dhall

in  TaskPool/become
      Privilege.User
      ( TaskPool/executeCommands
          Shell.Zsh
          [ "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | TMPDIR=/var/tmp BOOTSTRAP_HASKELL_NONINTERACTIVE=true sh"
          , "curl -sSL https://get.haskellstack.org | sh || true"
          , "ghcup upgrade"
          , "cabal update"
          , "stack update"
          , "ghcup install hls"
          , "cabal install ghcid brittany ormolu BNFC --overwrite-policy=always"
          ]
      )
