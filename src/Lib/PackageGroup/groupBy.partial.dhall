let External/Prelude = ../External/Prelude.partial.dhall

let PackageGroup = ./Enum.partial.dhall

let PackageGroup/Metadata = ./Metadata/Record.partial.dhall

let PackageGroup/toMetadata = ./toMetadata.partial.dhall

let Package = ../Package/Record.partial.dhall

let PackageFlag = ../PackageFlag/Enum.partial.dhall

let PackageFlag/equal = ../../codegen/Lib/PackageFlag/equal.partial.dhall

let groupBy
    : Optional PackageFlag -> List PackageGroup -> PackageGroup/Metadata.Type
    = \(packageFlag : Optional PackageFlag) ->
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
                                        \(packageFlag : PackageFlag) ->
                                          External/Prelude.Optional.any
                                            PackageFlag
                                            (PackageFlag/equal packageFlag)
                                            package.flag
                                    , None =
                                        External/Prelude.Optional.null
                                          PackageFlag
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
                                        \(packageFlag : PackageFlag) ->
                                          External/Prelude.Optional.any
                                            PackageFlag
                                            (PackageFlag/equal packageFlag)
                                            package.flag
                                    , None =
                                        External/Prelude.Optional.null
                                          PackageFlag
                                          package.flag
                                    }
                                    packageFlag
                              )
                              x.absent
                      }
              )
              PackageGroup/Metadata::{=}

in  groupBy
