let External/Ansible = ../External/Ansible.partial.dhall

let External/Prelude = ../External/Prelude.partial.dhall

let Role = ../Role/Enum.partial.dhall

let Role/toText = ../Role/toText.partial.dhall

let Text/pathify = ../Text/pathify.partial.dhall

let copyFiles
    : Role ->
      External/Prelude.Map.Type Text (List Text) ->
        List External/Ansible.Task.Type
    = \(role : Role) ->
      \(map : External/Prelude.Map.Type Text (List Text)) ->
        let roleText = Role/toText role

        let directories = External/Prelude.Map.keys Text (List Text) map

        in  External/Prelude.List.concat
              External/Ansible.Task.Type
              [ External/Prelude.List.unpackOptionals
                  External/Ansible.Task.Type
                  [ if    External/Prelude.Bool.not
                            (External/Prelude.List.null Text directories)
                    then  Some
                            External/Ansible.Task::{
                            , name = Some
                                "Create ${roleText} directory (or directories)"
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
                                                      Text/pathify
                                                      External/Ansible.Vars.string
                                                  )
                                                  directories
                                              )
                                        }
                                    )
                                )
                            }
                    else  None External/Ansible.Task.Type
                  ]
              , External/Prelude.List.unpackOptionals
                  External/Ansible.Task.Type
                  ( External/Prelude.List.map
                      (External/Prelude.Map.Entry Text (List Text))
                      (Optional External/Ansible.Task.Type)
                      ( \ ( entry
                          : External/Prelude.Map.Entry Text (List Text)
                          ) ->
                          if    External/Prelude.Bool.not
                                  ( External/Prelude.List.null
                                      Text
                                      entry.mapValue
                                  )
                          then  Some
                                  External/Ansible.Task::{
                                  , name = Some "Copy ${roleText} file(s)"
                                  , copy = Some External/Ansible.Copy::{
                                    , src = Some "{{ item }}"
                                    , dest =
                                        Text/pathify
                                          "${entry.mapKey}/{{ item }}"
                                    , force = Some True
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
                                  }
                          else  None External/Ansible.Task.Type
                      )
                      map
                  )
              ]

in  copyFiles
