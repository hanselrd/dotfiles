let Role = ../Role.partial.dhall

in  { Type = { role : Role, enabled : Bool }, default = {=} }
