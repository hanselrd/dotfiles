let External/Prelude = ../Lib/External/Prelude.partial.dhall

in  External/Prelude.List.map
      Text
      ( External/Prelude.Map.Entry
          Text
          { `dedupe.partial.dhall` : Text
          , `equal.partial.dhall` : Text
          , `fromNatural.partial.dhall` : Text
          , `sort.partial.dhall` : Text
          , `toNatural.partial.dhall` : Text
          , `toText.partial.dhall` : Text
          , `values.partial.dhall` : Text
          }
      )
      ( \(enum : Text) ->
          External/Prelude.Map.keyValue
            { `dedupe.partial.dhall` : Text
            , `equal.partial.dhall` : Text
            , `fromNatural.partial.dhall` : Text
            , `sort.partial.dhall` : Text
            , `toNatural.partial.dhall` : Text
            , `toText.partial.dhall` : Text
            , `values.partial.dhall` : Text
            }
            enum
            { `dedupe.partial.dhall` =
                ''
                let ${enum} = ../../../Lib/${enum}/Enum.partial.dhall

                let ${enum}Meta = ../../../Lib/${enum}/EnumMeta.partial.dhall

                let Enum/dedupe = ../../../Lib/Enum/dedupe.partial.dhall

                in  Enum/dedupe ${enum} ${enum}Meta
                ''
            , `equal.partial.dhall` =
                ''
                let ${enum} = ../../../Lib/${enum}/Enum.partial.dhall

                let ${enum}Meta = ../../../Lib/${enum}/EnumMeta.partial.dhall

                let Enum/equal = ../../../Lib/Enum/equal.partial.dhall

                in  Enum/equal ${enum} ${enum}Meta
                ''
            , `fromNatural.partial.dhall` =
                ''
                let ${enum} = ../../../Lib/${enum}/Enum.partial.dhall

                let ${enum}Meta = ../../../Lib/${enum}/EnumMeta.partial.dhall

                let Enum/fromNatural = ../../../Lib/Enum/fromNatural.partial.dhall

                in  Enum/fromNatural ${enum} ${enum}Meta
                ''
            , `sort.partial.dhall` =
                ''
                let ${enum} = ../../../Lib/${enum}/Enum.partial.dhall

                let ${enum}Meta = ../../../Lib/${enum}/EnumMeta.partial.dhall

                let Enum/sort = ../../../Lib/Enum/sort.partial.dhall

                in  Enum/sort ${enum} ${enum}Meta
                ''
            , `toNatural.partial.dhall` =
                ''
                let ${enum} = ../../../Lib/${enum}/Enum.partial.dhall

                let ${enum}Meta = ../../../Lib/${enum}/EnumMeta.partial.dhall

                let Enum/toNatural = ../../../Lib/Enum/toNatural.partial.dhall

                in  Enum/toNatural ${enum} ${enum}Meta
                ''
            , `toText.partial.dhall` =
                ''
                let ${enum} = ../../../Lib/${enum}/Enum.partial.dhall

                let ${enum}Meta = ../../../Lib/${enum}/EnumMeta.partial.dhall

                let Enum/toText = ../../../Lib/Enum/toText.partial.dhall

                in  Enum/toText ${enum} ${enum}Meta
                ''
            , `values.partial.dhall` =
                ''
                let ${enum} = ../../../Lib/${enum}/Enum.partial.dhall

                let ${enum}Meta = ../../../Lib/${enum}/EnumMeta.partial.dhall

                let Enum/values = ../../../Lib/Enum/values.partial.dhall

                in  Enum/values ${enum} ${enum}Meta
                ''
            }
      )
      [ "Background"
      , "Configuration"
      , "Directory"
      , "Font"
      , "PackageFlag"
      , "PackageGroup"
      , "PackageManager"
      , "Permission"
      , "Privilege"
      , "Role"
      , "Shell"
      , "System"
      , "Theme"
      ]
