{ Function.flip = ./Prelude/Function/flip.partial.dhall
, List =
  { Indexed = ./Prelude/List/Indexed/Record.partial.dhall
  , contains = ./Prelude/List/contains.partial.dhall
  , packOptionals = ./Prelude/List/packOptionals.partial.dhall
  }
, Text =
  { pathify = ./Prelude/Text/pathify.partial.dhall
  , replaces = ./Prelude/Text/replaces.partial.dhall
  }
}
