module Solutions.Day9 exposing (solution)

import Array exposing (Array)
import Types exposing (GetSolution)


type alias Depths =
    { values : Array Int
    , width : Int
    }


type alias AdjacencyCheck =
    { value : Int
    , neighbours : List Int
    }


parseInput : String -> Depths
parseInput input =
    let
        lines =
            String.lines input

        height =
            List.length lines

        values =
            String.join "" lines
                |> String.split ""
                |> List.filterMap String.toInt
                |> Array.fromList
    in
    { values = values
    , width = Array.length values // height
    }


createAdjacencies : Depths -> Array AdjacencyCheck
createAdjacencies { values, width } =
    Array.indexedMap
        (\i value ->
            let
                onLeftEdge =
                    modBy width i == 0

                onRightEdge =
                    modBy width (i + 1) == 0

                neighbourIndices =
                    if onLeftEdge then
                        -- left edge - ignore left neighbour
                        [ i + 1, i - width, i + width ]

                    else if onRightEdge then
                        -- right edge - ignore right neighbour
                        [ i - 1, i - width, i + width ]

                    else
                        [ i - 1, i + 1, i - width, i + width ]
            in
            { value = value
            , neighbours = List.filterMap (\n -> Array.get n values) neighbourIndices
            }
        )
        values


findLowPoints : Array AdjacencyCheck -> Array Int
findLowPoints checks =
    Array.filter
        (\{ value, neighbours } ->
            List.all (\n -> n > value) neighbours
        )
        checks
        |> Array.map .value


riskLevelSum : Array Int -> String
riskLevelSum lowPoints =
    Array.toList lowPoints
        |> List.map ((+) 1)
        |> List.sum
        |> String.fromInt


part1 : String -> String
part1 input =
    parseInput input
        |> createAdjacencies
        |> findLowPoints
        |> riskLevelSum


solution : GetSolution
solution input =
    { part1 = Just (part1 input)
    , part2 = Nothing
    }
