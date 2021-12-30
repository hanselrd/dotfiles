let flip
    : forall (a : Type) ->
      forall (b : Type) ->
      forall (c : Type) ->
      (a -> b -> c) ->
      b ->
      a ->
        c
    = \(a : Type) ->
      \(b : Type) ->
      \(c : Type) ->
      \(f : a -> b -> c) ->
      \(y : b) ->
      \(x : a) ->
        f x y

in  flip
