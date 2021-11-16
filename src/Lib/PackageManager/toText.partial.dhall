let PackageManager = ./Enum.partial.dhall

let toText
    : PackageManager -> Text
    = \(packageManager : PackageManager) ->
        merge { Pacman = "pacman", Dnf = "dnf", Apt = "apt" } packageManager

in  toText
