module Day3 exposing (solution)

import Day3input
import Types exposing (Solution)


type alias GammaEplison =
    { gamma : String
    , epsilon : String
    }


binaryToInt : String -> Int
binaryToInt binary =
    String.split "" binary
        |> List.filterMap String.toInt
        |> List.map2 (\power num -> (2 ^ power) * num) (List.reverse (List.range 0 (String.length binary - 1)))
        |> List.sum


gammaEpilsonToAnswer : GammaEplison -> String
gammaEpilsonToAnswer { gamma, epsilon } =
    binaryToInt gamma
        * binaryToInt epsilon
        |> String.fromInt


oneFromMany : GammaEplison -> List GammaEplison -> GammaEplison
oneFromMany current gammaEpsilons =
    case gammaEpsilons of
        { gamma, epsilon } :: rest ->
            oneFromMany
                { current
                    | gamma = current.gamma ++ gamma
                    , epsilon = current.epsilon ++ epsilon
                }
                rest

        [] ->
            current


getGammaEpsilon : List Char -> GammaEplison
getGammaEpsilon column =
    let
        sum =
            List.filterMap (\c -> String.toInt (String.fromChar c)) column
                |> List.sum

        half =
            List.length column // 2
    in
    if sum > half then
        { gamma = "1", epsilon = "0" }

    else
        { gamma = "0", epsilon = "1" }


listFromArray : List Char -> Int -> Int -> List Char
listFromArray allChars jump skip =
    List.map2
        (\i c -> ( i, c ))
        (List.range 0 (List.length allChars - 1))
        allChars
        |> List.filter (\( i, _ ) -> modBy jump i == skip)
        |> List.map (\( _, c ) -> c)


part1 : String
part1 =
    let
        asStringList =
            String.lines Day3input.input

        stringLength =
            case asStringList of
                x :: _ ->
                    String.length x

                [] ->
                    0

        asCharList =
            String.join "" asStringList
                |> String.toList
    in
    List.range 0 (stringLength - 1)
        |> List.map (\skip -> listFromArray asCharList stringLength skip)
        |> List.map getGammaEpsilon
        |> oneFromMany { gamma = "", epsilon = "" }
        |> gammaEpilsonToAnswer



-- getOxegenGeneratorRating : List GammaEplison -> List (List Char) -> List Char
-- getOxegenGeneratorRating gammaEpsilons binaries =
--     case binaries of
--         [] ->
--             []
--         binary::[] ->
--             binary
--         _ ->
--             let newBinaries =
--                 List.filter
-- part2 : String
-- part2 =
--     let
--         asStringList =
--             String.lines Day3input.input
--         stringLength =
--             case asStringList of
--                 x :: _ ->
--                     String.length x
--                 [] ->
--                     0
--         asCharList =
--             String.join "" asStringList
--                 |> String.toList
--     in
--     List.range 0 (stringLength - 1)
--         |> List.map (\skip -> listFromArray asCharList stringLength skip)
--         |> List.map getGammaEpsilon
--         |> oneFromMany { gamma = "", epsilon = "" }
--         |> gammaEpilsonToAnswer


solution : Solution
solution =
    { part1 = Just part1
    , part2 = Nothing
    }
