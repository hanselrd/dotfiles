let PackageFlag = ../PackageFlag/Enum.partial.dhall

in  { Type = { name : Text, flag : Optional PackageFlag }
    , default.flag = None PackageFlag
    }
