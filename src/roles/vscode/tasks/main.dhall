let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let TaskPool/become = ../../../Lib/TaskPool/become.partial.dhall

let TaskPool/concat = ../../../Lib/TaskPool/concat.partial.dhall

let TaskPool/copyFiles = ../../../Lib/TaskPool/copyFiles.partial.dhall

let TaskPool/executeCommands =
      ../../../Lib/TaskPool/executeCommands.partial.dhall

let Privilege = ../../../Lib/Privilege/Enum.partial.dhall

let Shell = ../../../Lib/Shell/Enum.partial.dhall

in  TaskPool/become
      Privilege.User
      ( TaskPool/concat
          [ Some
              ( TaskPool/copyFiles
                  [ External/Prelude.Map.keyValue
                      (List Text)
                      (Directory/toText Directory.Vscode)
                      [ "keybindings.json", "settings.json" ]
                  ]
              )
          , Some
              ( TaskPool/executeCommands
                  Shell.Default
                  ( External/Prelude.List.map
                      Text
                      Text
                      ( \(extension : Text) ->
                          "code --install-extension ${extension} --force"
                      )
                      [ "agurodriguez.vscode-lbnf"
                      , "bungcip.better-toml"
                      , "dhall.dhall-lang"
                      , "dhall.vscode-dhall-lsp-server"
                      , "eamodio.gitlens"
                      , "EditorConfig.EditorConfig"
                      , "haskell.haskell"
                      , "IronGeek.vscode-env"
                      , "jkiviluoto.tws"
                      , "joaompinto.vscode-graphviz"
                      , "josef.rouge-theme"
                      , "justusadam.language-haskell"
                      , "matklad.rust-analyzer"
                      , "mnxn.lalrpop-highlight"
                      , "ms-python.python"
                      , "ms-python.vscode-pylance"
                      , "ms-toolsai.jupyter"
                      , "ms-toolsai.jupyter-keymap"
                      , "ms-toolsai.jupyter-renderers"
                      , "ms-vscode-remote.remote-containers"
                      , "ms-vscode-remote.remote-ssh"
                      , "ms-vscode-remote.remote-wsl"
                      , "ms-vscode-remote.vscode-remote-extensionpack"
                      , "ms-vscode.cmake-tools"
                      , "ms-vscode.cpptools"
                      , "naumovs.color-highlight"
                      , "PKief.material-icon-theme"
                      , "PolyMeilex.wgsl"
                      , "twxs.cmake"
                      , "vscodevim.vim"
                      , "wmaurer.change-case"
                      , "wwm.better-align"
                      , "zhuangtongfa.material-theme"
                      ]
                  )
              )
          ]
      )
