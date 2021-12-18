let Configuration/toText = ./codegen/Lib/Configuration/toText.partial.dhall

let env = ./codegen/environment.partial.dhall

let configurationText = Configuration/toText env.configuration

in  ''
    [all]
    ${configurationText} ansible_connection=local
    ''
