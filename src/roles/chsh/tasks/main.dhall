let External/Ansible = ../../../Lib/External/Ansible.partial.dhall

let TaskPool/build = ../../../Lib/TaskPool/build.partial.dhall

let env = ../../../codegen/environment.partial.dhall

in  TaskPool/build
      [ Some
          External/Ansible.Task::{
          , name = Some "Change user shell"
          , user = Some External/Ansible.User::{
            , name = env.user
            , shell = Some "/bin/zsh"
            }
          }
      ]
