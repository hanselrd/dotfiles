let External/Ansible = ../External/Ansible.partial.dhall

let Entry = External/Ansible.Task.Type

in  { Type = List Entry, Entry }
