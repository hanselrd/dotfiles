let Background = ./Enum.partial.dhall

let toText
    : Background -> Text
    = \(background : Background) ->
        merge
          { One = "0001.jpg"
          , Two = "0002.jpg"
          , Three = "0003.jpg"
          , Four = "0004.jpg"
          , Five = "0005.jpg"
          , Six = "0006.jpg"
          , Seven = "0007.jpg"
          , Eight = "0008.jpg"
          , Nine = "0009.jpg"
          }
          background

in  toText
