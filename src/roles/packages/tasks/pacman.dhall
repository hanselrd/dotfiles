let External/Ansible = ../../../Lib/External/Ansible.partial.dhall

let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let TaskPool/executeCommands =
      ../../../Lib/TaskPool/executeCommands.partial.dhall

let TaskPool/update = ../../../Lib/TaskPool/update.partial.dhall

let PackageGroup/groupBy = ../../../Lib/PackageGroup/groupBy.partial.dhall

let Package = ../../../Lib/Package/Record.partial.dhall

let PackageFlag = ../../../Lib/PackageFlag/Enum.partial.dhall

let PackageGroup/values = ../../../codegen/Lib/PackageGroup/values.partial.dhall

let env = ../../../../build/environment.dhall

in  External/Prelude.List.concat
      External/Ansible.Task.Type
      [ [ External/Ansible.Task::{
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
                                    (\(package : Package.Type) -> package.name)
                                    ( PackageGroup/groupBy
                                        (None PackageFlag)
                                        PackageGroup/values
                                    ).present
                                )
                            )
                      }
                  )
              )
          }
        , External/Ansible.Task::{
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
                                    (\(package : Package.Type) -> package.name)
                                    ( PackageGroup/groupBy
                                        (None PackageFlag)
                                        PackageGroup/values
                                    ).absent
                                )
                            )
                      }
                  )
              )
          }
        , External/Ansible.Task::{
          , name = Some "Check if yay is installed"
          , stat = Some External/Ansible.Stat::{ path = "/usr/bin/yay" }
          , register = Some "st_yay"
          }
        ]
      , let presentPackages =
              External/Prelude.List.map
                Package.Type
                Text
                (\(package : Package.Type) -> package.name)
                ( PackageGroup/groupBy
                    (Some PackageFlag.Aur)
                    PackageGroup/values
                ).present

        in  TaskPool/update
              { become = Some True
              , become_user = Some env.user
              , when = Some "st_yay.stat.exists"
              }
              ( TaskPool/executeCommands
                  [ "yay -S ${External/Prelude.Text.concatSep
                                " "
                                presentPackages} --needed --noconfirm"
                  ]
                  True
              )
      , let absentPackages =
              External/Prelude.List.map
                Package.Type
                Text
                (\(package : Package.Type) -> package.name)
                ( PackageGroup/groupBy
                    (Some PackageFlag.Aur)
                    PackageGroup/values
                ).absent

        in  TaskPool/update
              { become = Some True
              , become_user = Some env.user
              , when = Some "st_yay.stat.exists"
              }
              ( TaskPool/executeCommands
                  [ "yay -Rns ${External/Prelude.Text.concatSep
                                  " "
                                  absentPackages} --unneeded --noconfirm"
                  ]
                  True
              )
      ]
