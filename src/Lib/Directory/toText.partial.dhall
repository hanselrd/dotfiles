let Directory = ./Enum.partial.dhall

let env = ../../../build/environment.dhall

let Text/pathify = ../Text/pathify.partial.dhall

let toText
    : Directory -> Text
    = \(directory : Directory) ->
        merge
          { Background =
              Text/pathify "${env.user_root_dir}/usr/local/share/backgrounds"
          }
          directory

in  toText
