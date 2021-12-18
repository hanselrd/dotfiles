let External/Prelude = ../Lib/External/Prelude.partial.dhall

let Role = ../Lib/Role/Enum.partial.dhall

let Role/isEnabled = ../Lib/Role/isEnabled.partial.dhall

let Role/toMetadata = ../Lib/Role/toMetadata.partial.dhall

let Role/toText = ../codegen/Lib/Role/toText.partial.dhall

let Role/values = ../codegen/Lib/Role/values.partial.dhall

in  External/Prelude.List.filterMap
      Role
      (External/Prelude.Map.Entry Text { meta : { `main.yml` : Text } })
      ( \(role : Role) ->
          if    Role/isEnabled role
          then  Some
                  ( External/Prelude.Map.keyValue
                      { meta : { `main.yml` : Text } }
                      (Role/toText role)
                      { meta.`main.yml`
                        =
                          External/Prelude.JSON.renderYAML
                            ( External/Prelude.JSON.object
                                ( toMap
                                    { dependencies =
                                        External/Prelude.JSON.array
                                          ( External/Prelude.List.map
                                              Role
                                              External/Prelude.JSON.Type
                                              ( External/Prelude.Function.compose
                                                  Role
                                                  Text
                                                  External/Prelude.JSON.Type
                                                  Role/toText
                                                  External/Prelude.JSON.string
                                              )
                                              ( Role/toMetadata role
                                              ).dependencies
                                          )
                                    }
                                )
                            )
                      }
                  )
          else  None
                  ( External/Prelude.Map.Entry
                      Text
                      { meta : { `main.yml` : Text } }
                  )
      )
      Role/values
