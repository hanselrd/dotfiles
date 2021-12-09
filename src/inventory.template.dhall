let Configuration = ./Lib/Configuration/Enum.partial.dhall

let ConfigurationMeta = ./Lib/Configuration/EnumMeta.partial.dhall

let Enum/toText = ./Lib/Enum/toText.partial.dhall

let env = ../build/environment.dhall

let configurationText =
      Enum/toText Configuration ConfigurationMeta env.configuration

in  ''
    [all]
    ${configurationText} ansible_connection=local
    ''
