let Package/Flag = ./Flag/Enum.partial.dhall

in  { Type = { name : Text, flag : Optional Package/Flag }
    , default.flag = None Package/Flag
    }
