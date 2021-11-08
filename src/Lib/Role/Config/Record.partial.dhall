let Role/Enum = ../Enum.partial.dhall

in  { Type = { role : Role/Enum, enabled : Bool }, default = {=} }
