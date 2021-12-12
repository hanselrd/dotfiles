let External/Ansible = ../../../Lib/External/Ansible.partial.dhall

let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let TaskPool/executeCommands =
      ../../../Lib/TaskPool/executeCommands.partial.dhall

let PackageGroup = ../../../Lib/PackageGroup/Enum.partial.dhall

let PackageGroupMeta = ../../../Lib/PackageGroup/EnumMeta.partial.dhall

let PackageGroup/groupBy = ../../../Lib/PackageGroup/groupBy.partial.dhall

let Package = ../../../Lib/Package/Record.partial.dhall

let Package/Flag = ../../../Lib/Package/Flag/Enum.partial.dhall

let Enum/values = ../../../Lib/Enum/values.partial.dhall

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
                                        (None Package/Flag)
                                        ( Enum/values
                                            PackageGroup
                                            PackageGroupMeta
                                        )
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
                                        (None Package/Flag)
                                        ( Enum/values
                                            PackageGroup
                                            PackageGroupMeta
                                        )
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
      , External/Prelude.List.map
          External/Ansible.Task.Type
          External/Ansible.Task.Type
          ( \(task : External/Ansible.Task.Type) ->
                  task
              //  { become = Some True
                  , become_user = Some env.user
                  , when = Some "st_yay.stat.exists"
                  }
          )
          ( let presentPackages =
                  External/Prelude.List.map
                    Package.Type
                    Text
                    (\(package : Package.Type) -> package.name)
                    ( PackageGroup/groupBy
                        (Some Package/Flag.Aur)
                        (Enum/values PackageGroup PackageGroupMeta)
                    ).present

            in  TaskPool/executeCommands
                  [ "yay -S ${External/Prelude.Text.concatSep
                                " "
                                presentPackages} --needed --noconfirm"
                  ]
                  True
          )
      , External/Prelude.List.map
          External/Ansible.Task.Type
          External/Ansible.Task.Type
          ( \(task : External/Ansible.Task.Type) ->
                  task
              //  { become = Some True
                  , become_user = Some env.user
                  , when = Some "st_yay.stat.exists"
                  }
          )
          ( let absentPackages =
                  External/Prelude.List.map
                    Package.Type
                    Text
                    (\(package : Package.Type) -> package.name)
                    ( PackageGroup/groupBy
                        (Some Package/Flag.Aur)
                        (Enum/values PackageGroup PackageGroupMeta)
                    ).absent

            in  TaskPool/executeCommands
                  [ "yay -Rns ${External/Prelude.Text.concatSep
                                  " "
                                  absentPackages} --unneeded --noconfirm"
                  ]
                  True
          )
      ]
