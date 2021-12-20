let External/Ansible = ../../../Lib/External/Ansible.partial.dhall

let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let TaskPool/executeCommands =
      ../../../Lib/TaskPool/executeCommands.partial.dhall

let Shell = ../../../Lib/Shell/Enum.partial.dhall

let TaskPool = ../../../Lib/TaskPool/Alias.partial.dhall

let TaskPool/become = ../../../Lib/TaskPool/become.partial.dhall

let TaskPool/build = ../../../Lib/TaskPool/build.partial.dhall

let TaskPool/concat = ../../../Lib/TaskPool/concat.partial.dhall

let TaskPool/when = ../../../Lib/TaskPool/when.partial.dhall

let PackageGroup/groupBy = ../../../Lib/PackageGroup/groupBy.partial.dhall

let Package = ../../../Lib/Package/Record.partial.dhall

let PackageFlag = ../../../Lib/PackageFlag/Enum.partial.dhall

let Privilege = ../../../Lib/Privilege/Enum.partial.dhall

let PackageGroup/values = ../../../codegen/Lib/PackageGroup/values.partial.dhall

let packages = PackageGroup/groupBy (None PackageFlag) PackageGroup/values

let aurPackages =
      PackageGroup/groupBy (Some PackageFlag.Aur) PackageGroup/values

in  TaskPool/concat
      [ Some
          ( TaskPool/build
              [ if    External/Prelude.Bool.not
                        ( External/Prelude.List.null
                            Package.Type
                            packages.present
                        )
                then  Some
                        External/Ansible.Task::{
                        , name = Some "Install packages"
                        , package = Some External/Ansible.Package::{
                          , name = "{{ item }}"
                          , state = "present"
                          }
                        , loop = Some "{{ packages }}"
                        , vars = Some
                            ( External/Ansible.Vars.object
                                ( toMap
                                    { packages =
                                        External/Ansible.Vars.array
                                          ( External/Prelude.List.map
                                              Text
                                              External/Ansible.Vars.Type
                                              External/Ansible.Vars.string
                                              ( External/Prelude.List.map
                                                  Package.Type
                                                  Text
                                                  ( \(package : Package.Type) ->
                                                      package.name
                                                  )
                                                  packages.present
                                              )
                                          )
                                    }
                                )
                            )
                        }
                else  None TaskPool.Entry
              , if    External/Prelude.Bool.not
                        ( External/Prelude.List.null
                            Package.Type
                            packages.absent
                        )
                then  Some
                        External/Ansible.Task::{
                        , name = Some "Remove packages"
                        , package = Some External/Ansible.Package::{
                          , name = "{{ item }}"
                          , state = "absent"
                          }
                        , loop = Some "{{ packages }}"
                        , vars = Some
                            ( External/Ansible.Vars.object
                                ( toMap
                                    { packages =
                                        External/Ansible.Vars.array
                                          ( External/Prelude.List.map
                                              Text
                                              External/Ansible.Vars.Type
                                              External/Ansible.Vars.string
                                              ( External/Prelude.List.map
                                                  Package.Type
                                                  Text
                                                  ( \(package : Package.Type) ->
                                                      package.name
                                                  )
                                                  packages.absent
                                              )
                                          )
                                    }
                                )
                            )
                        }
                else  None TaskPool.Entry
              ]
          )
      , Some
          ( TaskPool/when
              External/Ansible.Task::{
              , name = Some "Check if yay is installed"
              , stat = Some External/Ansible.Stat::{ path = "/usr/bin/yay" }
              }
              "st_yay"
              "st_yay.stat.exists"
              ( TaskPool/become
                  Privilege.User
                  ( TaskPool/concat
                      [ if    External/Prelude.Bool.not
                                ( External/Prelude.List.null
                                    Package.Type
                                    aurPackages.present
                                )
                        then  Some
                                ( TaskPool/executeCommands
                                    Shell.Default
                                    [ "yay -S ${External/Prelude.Text.concatSep
                                                  " "
                                                  ( External/Prelude.List.map
                                                      Package.Type
                                                      Text
                                                      ( \ ( package
                                                          : Package.Type
                                                          ) ->
                                                          package.name
                                                      )
                                                      aurPackages.present
                                                  )} --needed --noconfirm"
                                    ]
                                )
                        else  None TaskPool.Type
                      , if    External/Prelude.Bool.not
                                ( External/Prelude.List.null
                                    Package.Type
                                    aurPackages.absent
                                )
                        then  Some
                                ( TaskPool/executeCommands
                                    Shell.Default
                                    [ "yay -Rns ${External/Prelude.Text.concatSep
                                                    " "
                                                    ( External/Prelude.List.map
                                                        Package.Type
                                                        Text
                                                        ( \ ( package
                                                            : Package.Type
                                                            ) ->
                                                            package.name
                                                        )
                                                        aurPackages.absent
                                                    )} --unneeded --noconfirm"
                                    ]
                                )
                        else  None TaskPool.Type
                      ]
                  )
              )
          )
      ]
