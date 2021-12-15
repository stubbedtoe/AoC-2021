module Solutions.Day10 exposing (solution)

import Array
import Types exposing (GetSolution)


type Bracket
    = Round
    | Square
    | Curly
    | Angle


type LineResult
    = Valid
    | Incomplete (List Bracket)
    | Corrupt Bracket


calculateScore : List Bracket -> Int
calculateScore brackets =
    List.map
        (\b ->
            case b of
                Round ->
                    3

                Square ->
                    57

                Curly ->
                    1197

                Angle ->
                    25137
        )
        brackets
        |> List.sum


consumeLine : String -> LineResult
consumeLine line =
    String.toList line
        |> List.foldl
            (\char result ->
                case result of
                    Incomplete stack ->
                        consumeChar char stack

                    _ ->
                        result
            )
            (Incomplete [])
        |> cleanLineResult


consumeChar : Char -> List Bracket -> LineResult
consumeChar char stack =
    case char of
        '(' ->
            Incomplete (Round :: stack)

        '[' ->
            Incomplete (Square :: stack)

        '{' ->
            Incomplete (Curly :: stack)

        '<' ->
            Incomplete (Angle :: stack)

        ')' ->
            matchHead Round stack

        ']' ->
            matchHead Square stack

        '}' ->
            matchHead Curly stack

        '>' ->
            matchHead Angle stack

        _ ->
            Incomplete stack


matchHead : Bracket -> List Bracket -> LineResult
matchHead bracket stack =
    case stack of
        head :: tail ->
            if head == bracket then
                Incomplete tail

            else
                Corrupt bracket

        [] ->
            Corrupt bracket


cleanLineResult : LineResult -> LineResult
cleanLineResult result =
    case result of
        Incomplete [] ->
            Valid

        _ ->
            result


filterCorrupt : List LineResult -> List Bracket
filterCorrupt results =
    List.foldl
        (\result brackets ->
            case result of
                Corrupt b ->
                    b :: brackets

                _ ->
                    brackets
        )
        []
        results


findCompletionStringScore : LineResult -> Maybe Int
findCompletionStringScore result =
    case result of
        Incomplete stack ->
            List.foldl
                (\b score ->
                    case b of
                        Round ->
                            (score * 5) + 1

                        Square ->
                            (score * 5) + 2

                        Curly ->
                            (score * 5) + 3

                        Angle ->
                            (score * 5) + 4
                )
                0
                stack
                |> Just

        _ ->
            Nothing


findMiddleScore : List Int -> Maybe String
findMiddleScore unsorted =
    let
        middle =
            List.length unsorted // 2
    in
    List.sort unsorted
        |> Array.fromList
        |> Array.get middle
        |> Maybe.andThen (\i -> Just (String.fromInt i))


part1 : String -> String
part1 input =
    String.lines input
        |> List.map consumeLine
        |> filterCorrupt
        |> calculateScore
        |> String.fromInt


part2 : String -> Maybe String
part2 input =
    String.lines input
        |> List.map consumeLine
        |> List.filterMap findCompletionStringScore
        |> findMiddleScore


solution : GetSolution
solution input =
    { part1 = Just (part1 input)
    , part2 = part2 input
    }
