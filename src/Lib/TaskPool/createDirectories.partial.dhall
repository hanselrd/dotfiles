let External/Ansible = ../External/Ansible.partial.dhall

let External/Prelude = ../External/Prelude.partial.dhall

let Prelude = ../Prelude.partial.dhall

let TaskPool = ./Alias.partial.dhall

let TaskPool/build = ./build.partial.dhall

let createDirectories
    : List Text -> TaskPool.Type
    = \(directories : List Text) ->
        TaskPool/build
          [ if    External/Prelude.Bool.not
                    (External/Prelude.List.null Text directories)
            then  Some
                    External/Ansible.Task::{
                    , name = Some "Create directory (or directories)"
                    , file = Some External/Ansible.File::{
                      , path = "{{ item }}"
                      , state = Some External/Ansible.File.state.directory
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

in  createDirectories
