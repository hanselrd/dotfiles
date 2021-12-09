\(a : Type) ->
  { Type =
      { value : a
      , sort : Optional Natural
      , text : Optional Text
      , equal : a -> Bool
      }
  , default = { sort = None Natural, text = None Text }
  }
