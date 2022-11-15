let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let System = ../../../Lib/System/Enum.partial.dhall

let System/equal = ../../../codegen/Lib/System/equal.partial.dhall

let TaskPool = ../../../Lib/TaskPool/Alias.partial.dhall

let TaskPool/copyFiles = ../../../Lib/TaskPool/copyFiles.partial.dhall

let PermissionMode = ../../../Lib/PermissionMode/Record.partial.dhall

let env = ../../../codegen/environment.partial.dhall

in  if    System/equal env.system System.Linux
    then  Some
            ( TaskPool/copyFiles
                (None PermissionMode.Type)
                [ External/Prelude.Map.keyValue
                    (List Text)
                    (Directory/toText Directory.Kernel)
                    [ "sysctl.conf" ]
                ]
            )
    else  None TaskPool.Type
