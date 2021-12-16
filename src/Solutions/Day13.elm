module Solutions.Day13 exposing (solution)

import Input.Day1 exposing (input)
import Set exposing (Set)
import Types exposing (GetSolution)
import Utils


type alias Dot =
    ( Int, Int )


type alias Paper =
    Set Dot


type Fold
    = Horizontal Int
    | Vertical Int


type alias Instructions =
    ( Paper, List Fold )


parseDots : List String -> Paper
parseDots lines =
    List.map
        (\line ->
            case Utils.intAfterSplit "," line of
                x :: y :: [] ->
                    ( x, y )

                _ ->
                    let
                        _ =
                            Debug.log "error in parseDots" line
                    in
                    ( -1, -1 )
        )
        lines
        |> Set.fromList


parseFolds : List String -> List Fold
parseFolds lines =
    List.map
        (\line ->
            case String.split "=" line of
                direction :: value :: [] ->
                    let
                        num =
                            Maybe.withDefault -1 (String.toInt value)
                    in
                    if String.endsWith "y" direction then
                        Horizontal num

                    else if String.endsWith "x" direction then
                        Vertical num

                    else
                        let
                            _ =
                                Debug.log "parse direction" direction
                        in
                        Horizontal -1

                _ ->
                    let
                        _ =
                            Debug.log "error in parseFold" line
                    in
                    Vertical -1
        )
        lines


makeFold : Fold -> Paper -> Paper
makeFold fold dots =
    case fold of
        Horizontal num ->
            let
                ( aboveTheFold, belowTheFold ) =
                    Set.partition (\( _, y ) -> y < num) dots
            in
            Set.map
                (\( x, y ) ->
                    let
                        newY =
                            num - (y - num)
                    in
                    ( x, newY )
                )
                belowTheFold
                |> Set.union aboveTheFold

        Vertical num ->
            let
                ( leftOfFold, rightOfFold ) =
                    Set.partition (\( x, _ ) -> x < num) dots
            in
            Set.map
                (\( x, y ) ->
                    let
                        newX =
                            num - (x - num)
                    in
                    ( newX, y )
                )
                rightOfFold
                |> Set.union leftOfFold


parseInput : String -> Instructions
parseInput input =
    case Utils.splitOnEmptyLine (String.lines input) of
        dots :: folds :: [] ->
            ( parseDots dots, parseFolds folds )

        _ ->
            let
                _ =
                    Debug.log "parse" "unexpeted split on empty line"
            in
            ( Set.empty, [] )


part1 : Instructions -> Maybe String
part1 ( dots, folds ) =
    case folds of
        first :: _ ->
            makeFold first dots
                |> Set.size
                |> String.fromInt
                |> Just

        _ ->
            Nothing


part2 : Instructions -> String
part2 ( dots, folds ) =
    let
        paper =
            List.foldl
                makeFold
                dots
                folds

        maxX =
            Set.toList paper
                |> List.map Tuple.first
                |> List.maximum
                |> Maybe.withDefault 1

        maxY =
            Set.toList paper
                |> List.map Tuple.second
                |> List.maximum
                |> Maybe.withDefault 1
    in
    List.map
        (\y ->
            List.map
                (\x ->
                    if Set.member ( x, y ) paper then
                        "#"

                    else
                        "."
                )
                (List.range 0 maxX)
                |> String.join ""
        )
        (List.range 0 maxY)
        |> String.join "\n"



-- ####..##..#..#..##..#..#.###..####..##.
-- #....#..#.#.#..#..#.#.#..#..#....#.#..#
-- ###..#....##...#....##...###....#..#...
-- #....#.##.#.#..#....#.#..#..#..#...#.##
-- #....#..#.#.#..#..#.#.#..#..#.#....#..#
-- #.....###.#..#..##..#..#.###..####..###
-- F    G    K    C    K    B    Z    G


solution : GetSolution
solution input =
    let
        instructions =
            parseInput input
    in
    { part1 = part1 instructions
    , part2 = Just (part2 instructions)
    }
