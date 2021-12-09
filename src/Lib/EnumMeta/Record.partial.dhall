\(a : Type) ->
  { Type = { value : a, sort : Natural, text : Text, equal : a -> Bool }
  , default = { sort = 999, text = "" }
  }
