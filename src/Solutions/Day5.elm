module Solutions.Day5 exposing (solution)

import Dict exposing (Dict)
import Html.Attributes exposing (start)
import Types exposing (GetSolution)
import Utils


type alias Position =
    { x : Int
    , y : Int
    }


type alias VentDefinition =
    { start : Position
    , end : Position
    }


type alias VentLine =
    List String


type alias Grid =
    Dict String Int


type LineType
    = Horizontal
    | Vertical


type Part
    = Part1
    | Part2


positionToString : Position -> String
positionToString { x, y } =
    String.fromInt x ++ "," ++ String.fromInt y


constructDiagonalLine : Position -> Position -> VentLine
constructDiagonalLine start end =
    let
        xRange =
            if start.x < end.x then
                List.range start.x end.x

            else
                List.range end.x start.x
                    |> List.reverse

        yRange =
            if start.y < end.y then
                List.range start.y end.y

            else
                List.range end.y start.y
                    |> List.reverse
    in
    List.map2
        (\x y ->
            { x = x, y = y }
        )
        xRange
        yRange
        |> List.map positionToString


constructStraightLine : LineType -> Int -> Int -> Int -> VentLine
constructStraightLine kind const start end =
    let
        -- _ =
        --     Debug.log (String.join " " [ "const", String.fromInt const, "start", String.fromInt start, "end", String.fromInt end ]) ""
        from =
            List.minimum [ start, end ]
                |> Maybe.withDefault start

        to =
            List.maximum [ start, end ]
                |> Maybe.withDefault end
    in
    List.range from to
        |> List.map
            (\var ->
                case kind of
                    Horizontal ->
                        { x = var, y = const }

                    Vertical ->
                        { x = const, y = var }
            )
        |> List.map positionToString


isVentLine : Part -> VentDefinition -> Maybe VentLine
isVentLine part { start, end } =
    if start.x == end.x then
        Just (constructStraightLine Vertical start.x start.y end.y)

    else if start.y == end.y then
        Just (constructStraightLine Horizontal start.y start.x end.x)

    else if part == Part2 && abs (start.y - end.y) == abs (start.x - end.x) then
        Just (constructDiagonalLine start end)

    else
        Nothing


setInGrid : VentLine -> Grid -> Grid
setInGrid line grid =
    List.foldl
        (\curr acc ->
            Dict.update curr
                (\m ->
                    case m of
                        Just i ->
                            Just (i + 1)

                        Nothing ->
                            Just 1
                )
                acc
        )
        grid
        line


setLinesInGrid : List VentLine -> Grid
setLinesInGrid lines =
    List.foldl setInGrid Dict.empty lines


getAnswerPart1 : Grid -> String
getAnswerPart1 grid =
    Dict.values grid
        |> List.filter (\i -> i >= 2)
        |> List.length
        |> String.fromInt


parseLine : String -> Maybe VentDefinition
parseLine line =
    case String.split " -> " line of
        [ start, end ] ->
            let
                startPos =
                    case Utils.intAfterSplit "," start of
                        x :: y :: [] ->
                            { x = x, y = y }

                        _ ->
                            { x = 0, y = 0 }

                endPos =
                    case Utils.intAfterSplit "," end of
                        x :: y :: [] ->
                            { x = x, y = y }

                        _ ->
                            { x = 0, y = 0 }
            in
            Just { start = startPos, end = endPos }

        _ ->
            Nothing


getVentLines : Part -> String -> String
getVentLines part input =
    String.lines input
        |> List.filterMap parseLine
        |> List.filterMap (isVentLine part)
        |> setLinesInGrid
        |> getAnswerPart1


solution : GetSolution
solution input =
    { part1 = Just (getVentLines Part1 input)
    , part2 = Just (getVentLines Part2 input)
    }
