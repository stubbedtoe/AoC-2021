module Solutions.Day8 exposing (solution)

import Dict exposing (Dict)
import Set exposing (Set)
import Types exposing (GetSolution)


type alias EncodedSegment =
    Char


type alias DecodedSegment =
    Char


type alias Possibilities =
    Dict EncodedSegment (Set DecodedSegment)


type alias Part2Parsed =
    { inputs : List (List EncodedSegment)
    , outputs : List (List EncodedSegment)
    }


encoded : List EncodedSegment
encoded =
    [ 'a', 'b', 'c', 'd', 'e', 'f', 'g' ]


decoded : List DecodedSegment
decoded =
    [ 'A', 'B', 'C', 'D', 'E', 'F', 'G' ]


initialPossibilities : Possibilities
initialPossibilities =
    List.map (\en -> ( en, Set.fromList decoded )) encoded
        |> Dict.fromList


zero : Set DecodedSegment
zero =
    Set.fromList [ 'A', 'B', 'C', 'E', 'F', 'G' ]


one : Set DecodedSegment
one =
    Set.fromList [ 'C', 'F' ]


two : Set DecodedSegment
two =
    Set.fromList [ 'A', 'C', 'D', 'E', 'G' ]


three : Set DecodedSegment
three =
    Set.fromList [ 'A', 'C', 'D', 'F', 'G' ]


four : Set DecodedSegment
four =
    Set.fromList [ 'B', 'C', 'D', 'F' ]


five : Set DecodedSegment
five =
    Set.fromList [ 'A', 'B', 'D', 'F', 'G' ]


six : Set DecodedSegment
six =
    Set.fromList [ 'A', 'B', 'D', 'E', 'F', 'G' ]


seven : Set DecodedSegment
seven =
    Set.fromList [ 'A', 'C', 'F' ]


eight : Set DecodedSegment
eight =
    Set.fromList decoded


nine : Set DecodedSegment
nine =
    Set.fromList [ 'A', 'B', 'C', 'D', 'F', 'G' ]


insertCodeIntoPossbilities : List EncodedSegment -> Set DecodedSegment -> Possibilities -> Possibilities
insertCodeIntoPossbilities code decode poss =
    List.foldl
        (\char p ->
            Dict.update char
                (\maybeExisting ->
                    case maybeExisting of
                        Just existing ->
                            Just (Set.intersect existing decode)

                        Nothing ->
                            Just decode
                )
                p
        )
        poss
        code


fillPossibilities : Possibilities -> List (List EncodedSegment) -> Possibilities
fillPossibilities possibilities codes =
    let
        newPossibilities =
            List.foldl
                (\code poss ->
                    case List.length code of
                        2 ->
                            insertCodeIntoPossbilities code one poss

                        3 ->
                            insertCodeIntoPossbilities code seven poss

                        4 ->
                            insertCodeIntoPossbilities code four poss

                        _ ->
                            poss
                )
                possibilities
                codes
    in
    if newPossibilities == possibilities then
        newPossibilities

    else
        fillPossibilities newPossibilities codes


removeOneFromOtherPossibilities : Possibilities -> Possibilities
removeOneFromOtherPossibilities possibilities =
    let
        oneFound =
            List.filter (\set -> set == one) (Dict.values possibilities)
                |> List.length
                |> (==) 2
    in
    if oneFound then
        Dict.toList possibilities
            |> List.map
                (\( key, value ) ->
                    if not (value == one) then
                        ( key, Set.diff value one )

                    else
                        ( key, value )
                )
            |> Dict.fromList

    else
        possibilities


removeSingletonFromOtherPossibilities : Possibilities -> Possibilities
removeSingletonFromOtherPossibilities possibilities =
    let
        singleton =
            List.filter (\set -> Set.size set == 1) (Dict.values possibilities)
                |> List.head
                |> Maybe.withDefault Set.empty
    in
    Dict.toList possibilities
        |> List.map
            (\( key, value ) ->
                if not (value == singleton) then
                    ( key, Set.diff value singleton )

                else
                    ( key, value )
            )
        |> Dict.fromList


containsOneOf : List EncodedSegment -> EncodedSegment -> EncodedSegment -> Maybe (List EncodedSegment)
containsOneOf code a b =
    if xor (List.member a code) (List.member b code) then
        Just code

    else
        Nothing


distinguishBetweenTwo : EncodedSegment -> EncodedSegment -> List (List EncodedSegment) -> Possibilities -> Possibilities
distinguishBetweenTwo containedInTarget missingFromTarget codes possibilities =
    let
        both =
            Set.fromList [ containedInTarget, missingFromTarget ]

        confusion =
            List.filter (\( _, set ) -> set == both) (Dict.toList possibilities)
                |> List.map Tuple.first

        codesContainTarget =
            case confusion of
                some_or_other :: other_or_some :: [] ->
                    List.foldl
                        (\code maybe ->
                            case maybe of
                                Just c ->
                                    Just c

                                Nothing ->
                                    if (List.length code == 6) && (codeToDigit possibilities code == "8") then
                                        containsOneOf code some_or_other other_or_some

                                    else
                                        Nothing
                        )
                        Nothing
                        codes

                _ ->
                    Nothing
    in
    case codesContainTarget of
        Just code ->
            case confusion of
                some_or_other :: other_or_some :: [] ->
                    if List.member some_or_other code then
                        Dict.insert some_or_other (Set.singleton containedInTarget) possibilities
                            |> Dict.insert other_or_some (Set.singleton missingFromTarget)

                    else
                        Dict.insert some_or_other (Set.singleton missingFromTarget) possibilities
                            |> Dict.insert other_or_some (Set.singleton containedInTarget)

                _ ->
                    possibilities

        Nothing ->
            possibilities


removePairFromOtherPossibilities : Possibilities -> Possibilities
removePairFromOtherPossibilities possibilities =
    let
        pairs =
            List.filter (\set -> Set.size set == 2 && not (set == one)) (Dict.values possibilities)

        pairFound =
            case pairs of
                pair1 :: pair2 :: [] ->
                    pair1 == pair2

                _ ->
                    False

        pair =
            if pairFound then
                List.head pairs
                    |> Maybe.withDefault Set.empty

            else
                Set.empty
    in
    if pairFound then
        Dict.toList possibilities
            |> List.map
                (\( key, value ) ->
                    if not (value == pair) then
                        ( key, Set.diff value pair )

                    else
                        ( key, value )
                )
            |> Dict.fromList

    else
        possibilities


codeToDigit : Possibilities -> List EncodedSegment -> String
codeToDigit possibilities code =
    let
        decode =
            List.foldl
                (\c set ->
                    Dict.get c possibilities
                        |> Maybe.withDefault Set.empty
                        |> Set.union set
                )
                Set.empty
                code
    in
    if decode == zero then
        "0"

    else if decode == one then
        "1"

    else if decode == two then
        "2"

    else if decode == three then
        "3"

    else if decode == four then
        "4"

    else if decode == five then
        "5"

    else if decode == six then
        "6"

    else if decode == seven then
        "7"

    else if decode == eight then
        "8"

    else if decode == nine then
        "9"

    else
        "error"


solve : Possibilities -> Part2Parsed -> Maybe Int
solve possibilities parsed =
    let
        codes =
            parsed.inputs ++ parsed.outputs

        new_possibilities =
            fillPossibilities possibilities codes
                |> removeOneFromOtherPossibilities
                |> removeSingletonFromOtherPossibilities
                |> removePairFromOtherPossibilities
                |> distinguishBetweenTwo 'B' 'D' codes
                |> distinguishBetweenTwo 'F' 'C' codes
                |> distinguishBetweenTwo 'G' 'E' codes
    in
    if possibilities == new_possibilities then
        List.map (codeToDigit possibilities) parsed.outputs
            |> String.join ""
            |> String.toInt

    else
        solve new_possibilities parsed


knownLineOutputs : List String -> Int
knownLineOutputs outputs =
    List.filter
        (\o ->
            case String.length o of
                2 ->
                    True

                3 ->
                    True

                4 ->
                    True

                7 ->
                    True

                _ ->
                    False
        )
        outputs
        |> List.length


part1 : List (List String) -> String
part1 outputs =
    List.map knownLineOutputs outputs
        |> List.sum
        |> String.fromInt


parseOutputsOnly : String -> List (List String)
parseOutputsOnly input =
    String.lines input
        |> List.filterMap
            (\s ->
                case String.split " | " s of
                    _ :: outputs :: [] ->
                        Just (String.split " " outputs)

                    _ ->
                        Nothing
            )


parsePartTwo : String -> List Part2Parsed
parsePartTwo input =
    String.lines input
        |> List.filterMap
            (\s ->
                case String.split " | " s of
                    inputs :: outputs :: [] ->
                        let
                            ins =
                                String.split " " inputs
                                    |> List.map String.toList

                            outs =
                                String.split " " outputs
                                    |> List.map String.toList
                        in
                        Just
                            { inputs = ins
                            , outputs = outs
                            }

                    _ ->
                        Nothing
            )



-- testInput : String
-- testInput =
--     "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"


solution : GetSolution
solution input =
    let
        outputs =
            parseOutputsOnly input

        parsedForPart2 =
            parsePartTwo input
    in
    { part1 = Just (part1 outputs)
    , part2 =
        List.filterMap (solve initialPossibilities) parsedForPart2
            |> List.sum
            |> String.fromInt
            |> Just
    }
