let Package/Flag = ./Enum.partial.dhall

let toNatural
    : Package/Flag -> Natural
    = \(packageFlag : Package/Flag) -> merge { Aur = 0, Repo = 1 } packageFlag

in  toNatural
