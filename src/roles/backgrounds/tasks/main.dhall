let Ansible =
      https://raw.githubusercontent.com/softwarefactory-project/dhall-ansible/0.2.2/package.dhall
        sha256:030d7d1b16172afde44843c6e950fcc3382a6653269e36a27ca1d06d75a631ff

let Prelude =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.0.0/Prelude/package.dhall
        sha256:46c48bba5eee7807a872bbf6c3cb6ee6c2ec9498de3543c5dcc7dd950e43999d

let Role/Enum = ../../../Lib/Role/Enum.partial.dhall

let Background/Enum = ../../../Lib/Background/Enum.partial.dhall

let Background/toText = ../../../Lib/Background/toText.partial.dhall

let Text/pathify = ../../../Lib/Text/pathify.partial.dhall

let env = ../../../../build/environment.dhall

let mkRoleTasks = ../../mkRoleTasks.partial.dhall

in  Prelude.List.concat
      Ansible.Task.Type
      [ mkRoleTasks
          Role/Enum.Backgrounds
          [ Prelude.Map.keyValue
              (List Text)
              (Text/pathify "${env.user_root_dir}/usr/local/share/backgrounds")
              ( Prelude.List.map
                  Background/Enum
                  Text
                  Background/toText
                  [ Background/Enum.One
                  , Background/Enum.Two
                  , Background/Enum.Three
                  , Background/Enum.Four
                  , Background/Enum.Five
                  , Background/Enum.Six
                  , Background/Enum.Seven
                  , Background/Enum.Eight
                  , Background/Enum.Nine
                  ]
              )
          ]
      ]
