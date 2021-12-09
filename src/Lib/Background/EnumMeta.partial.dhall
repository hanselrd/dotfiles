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
        , sort = 1
        , text = "0001.jpg"
        , equal =
            \(background : Background) ->
              merge (default // { One = True }) background
        }
      , Two = (EnumMeta Background)::{
        , value = Background.Two
        , sort = 2
        , text = "0002.jpg"
        , equal =
            \(background : Background) ->
              merge (default // { Two = True }) background
        }
      , Three = (EnumMeta Background)::{
        , value = Background.Three
        , sort = 3
        , text = "0003.jpg"
        , equal =
            \(background : Background) ->
              merge (default // { Three = True }) background
        }
      , Four = (EnumMeta Background)::{
        , value = Background.Four
        , sort = 4
        , text = "0004.jpg"
        , equal =
            \(background : Background) ->
              merge (default // { Four = True }) background
        }
      , Five = (EnumMeta Background)::{
        , value = Background.Five
        , sort = 5
        , text = "0005.jpg"
        , equal =
            \(background : Background) ->
              merge (default // { Five = True }) background
        }
      , Six = (EnumMeta Background)::{
        , value = Background.Six
        , sort = 6
        , text = "0006.jpg"
        , equal =
            \(background : Background) ->
              merge (default // { Six = True }) background
        }
      , Seven = (EnumMeta Background)::{
        , value = Background.Seven
        , sort = 7
        , text = "0007.jpg"
        , equal =
            \(background : Background) ->
              merge (default // { Seven = True }) background
        }
      , Eight = (EnumMeta Background)::{
        , value = Background.Eight
        , sort = 8
        , text = "0008.jpg"
        , equal =
            \(background : Background) ->
              merge (default // { Eight = True }) background
        }
      , Nine = (EnumMeta Background)::{
        , value = Background.Nine
        , sort = 9
        , text = "0009.jpg"
        , equal =
            \(background : Background) ->
              merge (default // { Nine = True }) background
        }
      }

let validate = assert : merge meta Background.One === meta.One

in  External/Prelude.Map.values Text (EnumMeta Background).Type (toMap meta)
