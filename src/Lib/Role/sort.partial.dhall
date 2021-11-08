let External/Prelude = ../External/Prelude.partial.dhall

let Role = ./Enum.partial.dhall

let Role/equal = ./equal.partial.dhall

let Role/toMetadata = ./toMetadata.partial.dhall

let Accumulator =
      { edges : External/Prelude.Map.Type Role Role
      , start : List Role
      , sorted : List Role
      }

let sort
    : List Role -> List Role
    = \(xs : List Role) ->
        let edges =
              External/Prelude.List.concatMap
                Role
                (External/Prelude.Map.Entry Role Role)
                ( \(role : Role) ->
                    let dependencies = (Role/toMetadata role).dependencies

                    in  External/Prelude.List.map
                          Role
                          (External/Prelude.Map.Entry Role Role)
                          ( \(dependency : Role) ->
                              { mapKey = dependency, mapValue = role }
                          )
                          dependencies
                )
                xs

        let start =
              External/Prelude.List.filter
                Role
                ( \(role : Role) ->
                    External/Prelude.List.fold
                      (External/Prelude.Map.Entry Role Role)
                      edges
                      Bool
                      ( \(edge : External/Prelude.Map.Entry Role Role) ->
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
                (   External/Prelude.List.length Role xs
                  + External/Prelude.List.length
                      (External/Prelude.Map.Entry Role Role)
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
                    let maybeNode = External/Prelude.List.head Role acc.start

                    in  if    External/Prelude.Bool.not
                                (External/Prelude.Optional.null Role maybeNode)
                        then  let acc =
                                        acc
                                    //  { start =
                                            External/Prelude.List.drop
                                              1
                                              Role
                                              acc.start
                                        }

                              let acc =
                                        acc
                                    //  { sorted =
                                              acc.sorted
                                            # External/Prelude.Optional.toList
                                                Role
                                                maybeNode
                                        }

                              let dependentEdges =
                                    External/Prelude.List.filter
                                      (External/Prelude.Map.Entry Role Role)
                                      ( \ ( edge
                                          : External/Prelude.Map.Entry Role Role
                                          ) ->
                                          merge
                                            { None = False
                                            , Some =
                                                \(role : Role) ->
                                                  Role/equal edge.mapKey role
                                            }
                                            maybeNode
                                      )
                                      acc.edges

                              in  External/Prelude.List.fold
                                    (External/Prelude.Map.Entry Role Role)
                                    dependentEdges
                                    Accumulator
                                    ( \ ( dependentEdge
                                        : External/Prelude.Map.Entry Role Role
                                        ) ->
                                      \(acc : Accumulator) ->
                                        let acc =
                                                  acc
                                              //  { edges =
                                                      let p =
                                                            External/Prelude.List.partition
                                                              ( External/Prelude.Map.Entry
                                                                  Role
                                                                  Role
                                                              )
                                                              ( \ ( edge
                                                                  : External/Prelude.Map.Entry
                                                                      Role
                                                                      Role
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
                                                                  Role
                                                                  Role
                                                              )
                                                              p.true
                                                  }

                                        let hasIncomingEdges =
                                              External/Prelude.Bool.not
                                                ( External/Prelude.List.null
                                                    ( External/Prelude.Map.Entry
                                                        Role
                                                        Role
                                                    )
                                                    ( External/Prelude.List.partition
                                                        ( External/Prelude.Map.Entry
                                                            Role
                                                            Role
                                                        )
                                                        ( \ ( edge
                                                            : External/Prelude.Map.Entry
                                                                Role
                                                                Role
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
                { edges, start, sorted = External/Prelude.List.empty Role }
            ).sorted

in  sort
