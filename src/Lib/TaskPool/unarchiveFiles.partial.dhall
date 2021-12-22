let External/Ansible = ../External/Ansible.partial.dhall

let External/Prelude = ../External/Prelude.partial.dhall

let Prelude = ../Prelude.partial.dhall

let TaskPool = ./Alias.partial.dhall

let TaskPool/build = ./build.partial.dhall

let TaskPool/concat = ./concat.partial.dhall

let unarchiveFiles
    : External/Prelude.Map.Type Text (List Text) -> TaskPool.Type
    = \(map : External/Prelude.Map.Type Text (List Text)) ->
        let directories = External/Prelude.Map.keys Text (List Text) map

        let files =
              External/Prelude.List.concat
                Text
                (External/Prelude.Map.values Text (List Text) map)

        in  TaskPool/concat
              [ Some
                  ( TaskPool/build
                      [ if    External/Prelude.Bool.not
                                (External/Prelude.List.null Text directories)
                        then  Some
                                External/Ansible.Task::{
                                , name = Some
                                    "Create directory (or directories)"
                                , file = Some External/Ansible.File::{
                                  , path = "{{ item }}"
                                  , state = Some
                                      External/Ansible.File.state.directory
                                  }
                                , loop = Some "{{ directories }}"
                                , vars = Some
                                    ( External/Ansible.Vars.object
                                        ( toMap
                                            { directories =
                                                External/Ansible.Vars.array
                                                  ( External/Prelude.List.map
                                                      Text
                                                      External/Ansible.Vars.Type
                                                      ( External/Prelude.Function.compose
                                                          Text
                                                          Text
                                                          External/Ansible.Vars.Type
                                                          Prelude.Text.pathify
                                                          External/Ansible.Vars.string
                                                      )
                                                      directories
                                                  )
                                            }
                                        )
                                    )
                                }
                        else  None TaskPool.Entry
                      ]
                  )
              , Some
                  ( TaskPool/concat
                      [ if    External/Prelude.Bool.not
                                (External/Prelude.List.null Text files)
                        then  Some
                                ( TaskPool/build
                                    ( External/Prelude.List.map
                                        ( External/Prelude.Map.Entry
                                            Text
                                            (List Text)
                                        )
                                        (Optional TaskPool.Entry)
                                        ( \ ( entry
                                            : External/Prelude.Map.Entry
                                                Text
                                                (List Text)
                                            ) ->
                                            if    External/Prelude.Bool.not
                                                    ( External/Prelude.List.null
                                                        Text
                                                        entry.mapValue
                                                    )
                                            then  Some
                                                    External/Ansible.Task::{
                                                    , name = Some
                                                        "Unarchive file(s)"
                                                    , unarchive = Some External/Ansible.Unarchive::{
                                                      , src = "{{ item }}"
                                                      , dest =
                                                          Prelude.Text.pathify
                                                            "${entry.mapKey}"
                                                      }
                                                    , loop = Some "{{ files }}"
                                                    , vars = Some
                                                        ( External/Ansible.Vars.object
                                                            ( toMap
                                                                { files =
                                                                    External/Ansible.Vars.array
                                                                      ( External/Prelude.List.map
                                                                          Text
                                                                          External/Ansible.Vars.Type
                                                                          External/Ansible.Vars.string
                                                                          entry.mapValue
                                                                      )
                                                                }
                                                            )
                                                        )
                                                    , when = Some
                                                        "not ansible_check_mode"
                                                    }
                                            else  None TaskPool.Entry
                                        )
                                        map
                                    )
                                )
                        else  None TaskPool.Type
                      ]
                  )
              ]

in  unarchiveFiles
