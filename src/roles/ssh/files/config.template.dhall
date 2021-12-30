let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Configuration = ../../../Lib/Configuration/Enum.partial.dhall

let Configuration/equal = ../../../codegen/Lib/Configuration/equal.partial.dhall

let env = ../../../codegen/environment.partial.dhall

in  ''
    ${External/Prelude.Text.default
        ( if    External/Prelude.Bool.not
                  (Configuration/equal env.configuration Configuration.Remote)
          then  Some
                  ''
                  PubkeyAcceptedKeyTypes +ssh-rsa
                  ''
          else  None Text
        )}
    Host 10.*.*.* 192.168.*.*
        StrictHostKeyChecking no
        UserKnownHostsFile /dev/null
    ''
