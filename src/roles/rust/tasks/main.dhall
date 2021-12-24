let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let TaskPool/become = ../../../Lib/TaskPool/become.partial.dhall

let TaskPool/concat = ../../../Lib/TaskPool/concat.partial.dhall

let TaskPool/createDirectories =
      ../../../Lib/TaskPool/createDirectories.partial.dhall

let TaskPool/executeCommands =
      ../../../Lib/TaskPool/executeCommands.partial.dhall

let Privilege = ../../../Lib/Privilege/Enum.partial.dhall

let Shell = ../../../Lib/Shell/Enum.partial.dhall

in  TaskPool/become
      Privilege.User
      ( TaskPool/concat
          [ Some
              (TaskPool/createDirectories [ Directory/toText Directory.Rust ])
          , Some
              ( TaskPool/executeCommands
                  Shell.Zsh
                  [ "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --no-modify-path -y"
                  , "rustup component add rls rust-analysis rust-src"
                  , "rustup default nightly"
                  , "rustup default stable"
                  , "rustup update"
                  , "cargo install cargo-tree cargo-edit cargo-feature cargo-expand wasm-pack || true"
                  , "rustfmt --print-config default ${Directory/toText
                                                        Directory.Rust}/rustfmt.toml"
                  ]
              )
          ]
      )
