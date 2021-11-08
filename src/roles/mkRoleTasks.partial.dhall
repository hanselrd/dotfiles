let Ansible =
      https://raw.githubusercontent.com/softwarefactory-project/dhall-ansible/0.2.2/package.dhall
        sha256:030d7d1b16172afde44843c6e950fcc3382a6653269e36a27ca1d06d75a631ff

let Prelude =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.0.0/Prelude/package.dhall
        sha256:46c48bba5eee7807a872bbf6c3cb6ee6c2ec9498de3543c5dcc7dd950e43999d

let Role/Enum = ../Lib/Role/Enum.partial.dhall

let Role/toText = ../Lib/Role/toText.partial.dhall

in  \(role : Role/Enum) ->
    \(outputs : Prelude.Map.Type Text (List Text)) ->
      let roleText = Role/toText role

      let directories = Prelude.Map.keys Text (List Text) outputs

      in  Prelude.List.concat
            Ansible.Task.Type
            [ Prelude.List.unpackOptionals
                Ansible.Task.Type
                [ if    Prelude.Bool.not (Prelude.List.null Text directories)
                  then  Some
                          Ansible.Task::{
                          , name = Some
                              "Create ${roleText} directory (or directories)"
                          , file = Some Ansible.File::{
                            , path = "{{ item }}"
                            , state = Some Ansible.File.state.directory
                            }
                          , loop = Some "{{ directories }}"
                          , vars = Some
                              ( Ansible.Vars.object
                                  ( toMap
                                      { directories =
                                          Ansible.Vars.array
                                            ( Prelude.List.map
                                                Text
                                                Ansible.Vars.Type
                                                Ansible.Vars.string
                                                directories
                                            )
                                      }
                                  )
                              )
                          }
                  else  None Ansible.Task.Type
                ]
            , Prelude.List.unpackOptionals
                Ansible.Task.Type
                ( Prelude.List.map
                    (Prelude.Map.Entry Text (List Text))
                    (Optional Ansible.Task.Type)
                    ( \(entry : Prelude.Map.Entry Text (List Text)) ->
                        if    Prelude.Bool.not
                                (Prelude.List.null Text entry.mapValue)
                        then  Some
                                Ansible.Task::{
                                , name = Some "Copy ${roleText} file(s)"
                                , copy = Some Ansible.Copy::{
                                  , src = Some "{{ item }}"
                                  , dest = "${entry.mapKey}/{{ item }}"
                                  , force = Some True
                                  }
                                , loop = Some "{{ files }}"
                                , vars = Some
                                    ( Ansible.Vars.object
                                        ( toMap
                                            { files =
                                                Ansible.Vars.array
                                                  ( Prelude.List.map
                                                      Text
                                                      Ansible.Vars.Type
                                                      Ansible.Vars.string
                                                      entry.mapValue
                                                  )
                                            }
                                        )
                                    )
                                }
                        else  None Ansible.Task.Type
                    )
                    outputs
                )
            ]
