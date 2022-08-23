\(a : Type) ->
  { Type =
      { value : a
      , sort : Optional Natural
      , skip : Bool
      , text : Optional Text
      , equal : a -> Bool
      }
  , default = { sort = None Natural, skip = False, text = None Text }
  }
