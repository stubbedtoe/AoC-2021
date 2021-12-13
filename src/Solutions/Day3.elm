module Solutions.Day3 exposing (solution)

import Array
import Types exposing (GetSolution)


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
                |> toFloat

        half =
            List.length column
                |> toFloat
                |> (\i -> i / 2)
    in
    if sum >= half then
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


part1 : String -> String
part1 input =
    let
        asStringList =
            String.lines input

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


getLifeSupportRating : Int -> Int -> String
getLifeSupportRating o2GeneratorRating co2ScrubberRating =
    o2GeneratorRating
        * co2ScrubberRating
        |> String.fromInt


getRating : (GammaEplison -> String) -> List (List Char) -> Int -> Maybe Int
getRating which binaries index =
    case binaries of
        [] ->
            Nothing

        binary :: [] ->
            String.fromList binary
                |> binaryToInt
                |> Just

        binary :: _ ->
            let
                stringLength =
                    List.length binary

                asStringList =
                    List.map String.fromList binaries

                asCharList =
                    String.join "" asStringList
                        |> String.toList

                gammaEpsilon : GammaEplison
                gammaEpsilon =
                    listFromArray asCharList stringLength index
                        |> getGammaEpsilon

                newBinaries : List (List Char)
                newBinaries =
                    List.filter
                        (\b ->
                            Array.fromList b
                                |> (\a ->
                                        case Array.get index a of
                                            Just c ->
                                                String.fromChar c == which gammaEpsilon

                                            Nothing ->
                                                False
                                   )
                        )
                        binaries
            in
            case newBinaries of
                bin :: [] ->
                    String.fromList bin
                        |> binaryToInt
                        |> Just

                _ ->
                    getRating which newBinaries (index + 1)


part2 : String -> Maybe String
part2 input =
    let
        rows =
            String.lines input
                |> List.map String.toList

        o2GeneratorRating =
            getRating .gamma rows 0

        co2ScrubberRating =
            getRating .epsilon rows 0
    in
    case ( o2GeneratorRating, co2ScrubberRating ) of
        ( Just generator, Just scrubber ) ->
            Just (getLifeSupportRating generator scrubber)

        _ ->
            Nothing


solution : GetSolution
solution input =
    { part1 = Just (part1 input)
    , part2 = part2 input
    }
