let External/Prelude = ../External/Prelude.partial.dhall

let Role/Enum = ./Enum.partial.dhall

let Role/equal = ./equal.partial.dhall

let Role/toMetadata = ./toMetadata.partial.dhall

let Accumulator =
      { edges : External/Prelude.Map.Type Role/Enum Role/Enum
      , start : List Role/Enum
      , sorted : List Role/Enum
      }

let sort
    : List Role/Enum -> List Role/Enum
    = \(xs : List Role/Enum) ->
        let edges =
              External/Prelude.List.concatMap
                Role/Enum
                (External/Prelude.Map.Entry Role/Enum Role/Enum)
                ( \(role : Role/Enum) ->
                    let dependencies = (Role/toMetadata role).dependencies

                    in  External/Prelude.List.map
                          Role/Enum
                          (External/Prelude.Map.Entry Role/Enum Role/Enum)
                          ( \(dependency : Role/Enum) ->
                              { mapKey = dependency, mapValue = role }
                          )
                          dependencies
                )
                xs

        let start =
              External/Prelude.List.filter
                Role/Enum
                ( \(role : Role/Enum) ->
                    External/Prelude.List.fold
                      (External/Prelude.Map.Entry Role/Enum Role/Enum)
                      edges
                      Bool
                      ( \ ( edge
                          : External/Prelude.Map.Entry Role/Enum Role/Enum
                          ) ->
                        \(acc : Bool) ->
                              External/Prelude.Bool.not
                                (Role/equal edge.mapValue role)
                          &&  acc
                      )
                      True
                )
                xs

        let indices =
              External/Prelude.List.generate
                (   External/Prelude.List.length Role/Enum xs
                  + External/Prelude.List.length
                      (External/Prelude.Map.Entry Role/Enum Role/Enum)
                      edges
                )
                Natural
                (External/Prelude.Function.identity Natural)

        in  ( External/Prelude.List.fold
                Natural
                indices
                Accumulator
                ( \(index : Natural) ->
                  \(acc : Accumulator) ->
                    let maybeNode =
                          External/Prelude.List.head Role/Enum acc.start

                    in  if    External/Prelude.Bool.not
                                ( External/Prelude.Optional.null
                                    Role/Enum
                                    maybeNode
                                )
                        then  let acc =
                                        acc
                                    //  { start =
                                            External/Prelude.List.drop
                                              1
                                              Role/Enum
                                              acc.start
                                        }

                              let acc =
                                        acc
                                    //  { sorted =
                                              acc.sorted
                                            # External/Prelude.Optional.toList
                                                Role/Enum
                                                maybeNode
                                        }

                              let dependentEdges =
                                    External/Prelude.List.filter
                                      ( External/Prelude.Map.Entry
                                          Role/Enum
                                          Role/Enum
                                      )
                                      ( \ ( edge
                                          : External/Prelude.Map.Entry
                                              Role/Enum
                                              Role/Enum
                                          ) ->
                                          merge
                                            { None = False
                                            , Some =
                                                \(role : Role/Enum) ->
                                                  Role/equal edge.mapKey role
                                            }
                                            maybeNode
                                      )
                                      acc.edges

                              in  External/Prelude.List.fold
                                    ( External/Prelude.Map.Entry
                                        Role/Enum
                                        Role/Enum
                                    )
                                    dependentEdges
                                    Accumulator
                                    ( \ ( dependentEdge
                                        : External/Prelude.Map.Entry
                                            Role/Enum
                                            Role/Enum
                                        ) ->
                                      \(acc : Accumulator) ->
                                        let acc =
                                                  acc
                                              //  { edges =
                                                      let p =
                                                            External/Prelude.List.partition
                                                              ( External/Prelude.Map.Entry
                                                                  Role/Enum
                                                                  Role/Enum
                                                              )
                                                              ( \ ( edge
                                                                  : External/Prelude.Map.Entry
                                                                      Role/Enum
                                                                      Role/Enum
                                                                  ) ->
                                                                      Role/equal
                                                                        dependentEdge.mapKey
                                                                        edge.mapKey
                                                                  &&  Role/equal
                                                                        dependentEdge.mapValue
                                                                        edge.mapValue
                                                              )
                                                              acc.edges

                                                      in    p.false
                                                          # External/Prelude.List.drop
                                                              1
                                                              ( External/Prelude.Map.Entry
                                                                  Role/Enum
                                                                  Role/Enum
                                                              )
                                                              p.true
                                                  }

                                        let hasIncomingEdges =
                                              External/Prelude.Bool.not
                                                ( External/Prelude.List.null
                                                    ( External/Prelude.Map.Entry
                                                        Role/Enum
                                                        Role/Enum
                                                    )
                                                    ( External/Prelude.List.partition
                                                        ( External/Prelude.Map.Entry
                                                            Role/Enum
                                                            Role/Enum
                                                        )
                                                        ( \ ( edge
                                                            : External/Prelude.Map.Entry
                                                                Role/Enum
                                                                Role/Enum
                                                            ) ->
                                                            Role/equal
                                                              dependentEdge.mapValue
                                                              edge.mapValue
                                                        )
                                                        acc.edges
                                                    ).true
                                                )

                                        in  if    External/Prelude.Bool.not
                                                    hasIncomingEdges
                                            then      acc
                                                  //  { start =
                                                            acc.start
                                                          # [ dependentEdge.mapValue
                                                            ]
                                                      }
                                            else  acc
                                    )
                                    acc
                        else  acc
                )
                { edges, start, sorted = External/Prelude.List.empty Role/Enum }
            ).sorted


in  sort
