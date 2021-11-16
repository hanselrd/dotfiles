let Configuration/toText = ./Lib/Configuration/toText.partial.dhall

let env = ../build/environment.dhall

let configurationText = Configuration/toText env.configuration

in  ''
    [all]
    ${configurationText} ansible_connection=local
    ''
