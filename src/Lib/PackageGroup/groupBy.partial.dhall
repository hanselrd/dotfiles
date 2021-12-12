let External/Prelude = ../External/Prelude.partial.dhall

let PackageGroup = ./Enum.partial.dhall

let PackageGroup/Metadata = ./Metadata/Record.partial.dhall

let PackageGroup/toMetadata = ./toMetadata.partial.dhall

let Package = ../Package/Record.partial.dhall

let Package/Flag = ../Package/Flag/Enum.partial.dhall

let Package/FlagMeta = ../Package/Flag/EnumMeta.partial.dhall

let Enum/equal = ../Enum/equal.partial.dhall

let groupBy
    : Optional Package/Flag -> List PackageGroup -> PackageGroup/Metadata.Type
    = \(packageFlag : Optional Package/Flag) ->
      \(xs : List PackageGroup) ->
        let xs =
              External/Prelude.List.map
                PackageGroup
                PackageGroup/Metadata.Type
                PackageGroup/toMetadata
                xs

        in  External/Prelude.List.foldLeft
              PackageGroup/Metadata.Type
              xs
              PackageGroup/Metadata.Type
              ( \(acc : PackageGroup/Metadata.Type) ->
                \(x : PackageGroup/Metadata.Type) ->
                      acc
                  //  { present =
                            acc.present
                          # External/Prelude.List.filter
                              Package.Type
                              ( \(package : Package.Type) ->
                                  merge
                                    { Some =
                                        \(packageFlag : Package/Flag) ->
                                          External/Prelude.Optional.any
                                            Package/Flag
                                            ( Enum/equal
                                                Package/Flag
                                                Package/FlagMeta
                                                packageFlag
                                            )
                                            package.flag
                                    , None =
                                        External/Prelude.Optional.null
                                          Package/Flag
                                          package.flag
                                    }
                                    packageFlag
                              )
                              x.present
                      , absent =
                            acc.absent
                          # External/Prelude.List.filter
                              Package.Type
                              ( \(package : Package.Type) ->
                                  merge
                                    { Some =
                                        \(packageFlag : Package/Flag) ->
                                          External/Prelude.Optional.any
                                            Package/Flag
                                            ( Enum/equal
                                                Package/Flag
                                                Package/FlagMeta
                                                packageFlag
                                            )
                                            package.flag
                                    , None =
                                        External/Prelude.Optional.null
                                          Package/Flag
                                          package.flag
                                    }
                                    packageFlag
                              )
                              x.absent
                      }
              )
              PackageGroup/Metadata::{=}

in  groupBy
