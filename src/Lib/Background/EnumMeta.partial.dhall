let External/Prelude = ../External/Prelude.partial.dhall

let Background = ./Enum.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let default =
      { One = False
      , Two = False
      , Three = False
      , Four = False
      , Five = False
      , Six = False
      , Seven = False
      , Eight = False
      , Nine = False
      }

let meta =
      { One = (EnumMeta Background)::{
        , value = Background.One
        , sort = Some 1
        , text = Some "0001.jpg"
        , equal =
            \(background : Background) ->
              merge (default // { One = True }) background
        }
      , Two = (EnumMeta Background)::{
        , value = Background.Two
        , sort = Some 2
        , text = Some "0002.jpg"
        , equal =
            \(background : Background) ->
              merge (default // { Two = True }) background
        }
      , Three = (EnumMeta Background)::{
        , value = Background.Three
        , sort = Some 3
        , text = Some "0003.jpg"
        , equal =
            \(background : Background) ->
              merge (default // { Three = True }) background
        }
      , Four = (EnumMeta Background)::{
        , value = Background.Four
        , sort = Some 4
        , text = Some "0004.jpg"
        , equal =
            \(background : Background) ->
              merge (default // { Four = True }) background
        }
      , Five = (EnumMeta Background)::{
        , value = Background.Five
        , sort = Some 5
        , text = Some "0005.jpg"
        , equal =
            \(background : Background) ->
              merge (default // { Five = True }) background
        }
      , Six = (EnumMeta Background)::{
        , value = Background.Six
        , sort = Some 6
        , text = Some "0006.jpg"
        , equal =
            \(background : Background) ->
              merge (default // { Six = True }) background
        }
      , Seven = (EnumMeta Background)::{
        , value = Background.Seven
        , sort = Some 7
        , text = Some "0007.jpg"
        , equal =
            \(background : Background) ->
              merge (default // { Seven = True }) background
        }
      , Eight = (EnumMeta Background)::{
        , value = Background.Eight
        , sort = Some 8
        , text = Some "0008.jpg"
        , equal =
            \(background : Background) ->
              merge (default // { Eight = True }) background
        }
      , Nine = (EnumMeta Background)::{
        , value = Background.Nine
        , sort = Some 9
        , text = Some "0009.jpg"
        , equal =
            \(background : Background) ->
              merge (default // { Nine = True }) background
        }
      }

let validate = assert : merge meta Background.One === meta.One

in  External/Prelude.Map.values Text (EnumMeta Background).Type (toMap meta)
