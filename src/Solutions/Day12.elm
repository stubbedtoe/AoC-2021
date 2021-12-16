module Solutions.Day12 exposing (solution)

import Dict exposing (Dict)
import Input.Day1 exposing (input)
import Set exposing (Set)
import Types exposing (GetSolution, Part(..))


type CaveSize
    = Large
    | Small


type alias Id =
    String


type alias Cave =
    { id : Id
    , connections : Set Id
    , size : CaveSize
    }


type alias CaveMapping =
    Dict Id Cave


caveSizeFromId : Id -> CaveSize
caveSizeFromId id =
    if String.toUpper id == id then
        Large

    else
        Small


createConnection : Id -> Id -> CaveMapping -> CaveMapping
createConnection cave1 cave2 mapping =
    Dict.update cave1
        (\maybeCave ->
            case maybeCave of
                Just cave ->
                    Just { cave | connections = Set.insert cave2 cave.connections }

                Nothing ->
                    let
                        size =
                            caveSizeFromId cave1
                    in
                    Just
                        { id = cave1
                        , size = size
                        , connections = Set.fromList [ cave2 ]
                        }
        )
        mapping


parseLine : String -> CaveMapping -> CaveMapping
parseLine line mapping =
    case String.split "-" line of
        from :: to :: [] ->
            createConnection from to mapping
                |> createConnection to from

        _ ->
            mapping


parseInput : String -> CaveMapping
parseInput input =
    List.foldl
        parseLine
        Dict.empty
        (String.lines input)


smallCavesJustOnce : List Id -> Bool
smallCavesJustOnce visited =
    let
        smallVisited =
            List.filter (\id -> caveSizeFromId id == Small) visited
    in
    Set.fromList smallVisited
        |> Set.size
        |> (==) (List.length smallVisited)


findPossibleNext : Part -> List Id -> List Id -> List Cave -> List Cave
findPossibleNext part oldVisited newVisited caves =
    case part of
        Part1 ->
            List.filter (\{ size, id } -> size == Large || not (List.member id oldVisited)) caves

        Part2 ->
            List.filter (\{ size, id } -> size == Large || not (List.member id oldVisited) || (id /= "start" && smallCavesJustOnce newVisited)) caves


findPaths : Part -> CaveMapping -> List Id -> Cave -> Int
findPaths part mapping visited cave =
    if cave.id == "end" then
        -- let
        --     _ =
        --         Debug.log "path" (List.reverse (cave.id :: visited))
        -- in
        1

    else
        let
            newVisited =
                cave.id :: visited

            possibleNext =
                Set.toList cave.connections
                    |> List.filterMap (\id -> Dict.get id mapping)
                    |> findPossibleNext part visited newVisited
        in
        List.map
            (findPaths part mapping newVisited)
            possibleNext
            |> List.sum


getAnswer : Part -> CaveMapping -> String
getAnswer part mapping =
    case Dict.get "start" mapping of
        Just cave ->
            findPaths part mapping [] cave
                |> String.fromInt

        Nothing ->
            "error - start not found"


solution : GetSolution
solution input =
    let
        mapping =
            parseInput input
    in
    { part1 = Just (getAnswer Part1 mapping)
    , part2 = Just (getAnswer Part2 mapping)
    }
