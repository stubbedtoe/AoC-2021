module Solutions.Day9 exposing (solution)

import Array exposing (Array)
import Dict exposing (Dict)
import Test.Html.Event exposing (check)
import Types exposing (GetSolution)


type alias Depths =
    { values : Array Int
    , width : Int
    }


type alias AdjacencyCheck =
    { value : Int
    , index : Int
    , neighbours : List Int
    , basinIndex : Maybe Int
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
                neighbourIndices =
                    getNeighbourIndices i width
            in
            { value = value
            , neighbours = List.filterMap (\n -> Array.get n values) neighbourIndices
            , basinIndex = Nothing
            , index = i
            }
        )
        values


createAdjacenciesPart2 : Depths -> Array AdjacencyCheck
createAdjacenciesPart2 { values, width } =
    Array.indexedMap
        (\i value ->
            { value = value
            , neighbours = getNeighbourIndices i width
            , basinIndex = Nothing
            , index = i
            }
        )
        values


isLowPoint : AdjacencyCheck -> Bool
isLowPoint { value, neighbours } =
    List.all (\n -> n > value) neighbours


getNeighbourIndices : Int -> Int -> List Int
getNeighbourIndices index width =
    let
        onLeftEdge =
            modBy width index == 0

        onRightEdge =
            modBy width (index + 1) == 0
    in
    if onLeftEdge then
        -- left edge - ignore left neighbour
        [ index + 1, index - width, index + width ]

    else if onRightEdge then
        -- right edge - ignore right neighbour
        [ index - 1, index - width, index + width ]

    else
        [ index - 1, index + 1, index - width, index + width ]


updateWithBasinIndex : Int -> AdjacencyCheck -> AdjacencyCheck
updateWithBasinIndex index check =
    { check | basinIndex = Just index }


findLowPoints : Array AdjacencyCheck -> Array AdjacencyCheck
findLowPoints checks =
    Array.filter isLowPoint checks


floodSelfAndNeighbours : Int -> Int -> Array AdjacencyCheck -> Array AdjacencyCheck
floodSelfAndNeighbours basinIndex index checks =
    case Array.get index checks of
        Just check ->
            -- let
            --     _ =
            --         Debug.log "in flood" (String.fromInt check.value)
            -- in
            if check.value /= 9 && check.basinIndex == Nothing then
                -- let
                --     _ =
                --         Debug.log "update and recurse" (String.fromInt check.value)
                -- in
                List.foldl
                    (floodSelfAndNeighbours basinIndex)
                    (Array.set index (updateWithBasinIndex basinIndex check) checks)
                    check.neighbours

            else
                -- let
                --     _ =
                --         Debug.log "not updating" (String.fromInt check.value)
                -- in
                checks

        Nothing ->
            checks


floodBasins : Array AdjacencyCheck -> Array AdjacencyCheck
floodBasins checks =
    Array.foldl
        (\lowpoint acc ->
            floodSelfAndNeighbours lowpoint.index lowpoint.index acc
        )
        checks
        (findLowPoints checks)


gatherBasins : Array AdjacencyCheck -> Dict Int Int
gatherBasins checks =
    Array.foldl
        (\check acc ->
            case check.basinIndex of
                Just i ->
                    Dict.update i
                        (\maybeNum ->
                            case maybeNum of
                                Just num ->
                                    Just (num + 1)

                                Nothing ->
                                    Just 1
                        )
                        acc

                Nothing ->
                    acc
        )
        Dict.empty
        checks


getTopThreeBasins : Dict Int Int -> String
getTopThreeBasins basins =
    -- let
    --     _ =
    --         Debug.log "basins" basins
    -- in
    List.sort (Dict.values basins)
        |> List.reverse
        |> List.take 3
        |> List.product
        |> String.fromInt


riskLevelSum : Array AdjacencyCheck -> String
riskLevelSum lowPoints =
    Array.toList lowPoints
        |> List.map .value
        |> List.map ((+) 1)
        |> List.sum
        |> String.fromInt


part1 : String -> String
part1 input =
    parseInput input
        |> createAdjacencies
        |> findLowPoints
        |> riskLevelSum


part2 : String -> String
part2 input =
    parseInput input
        |> createAdjacenciesPart2
        |> floodBasins
        |> gatherBasins
        |> getTopThreeBasins


solution : GetSolution
solution input =
    { part1 = Just (part1 input)
    , part2 = Just (part2 input)
    }
