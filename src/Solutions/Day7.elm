module Solutions.Day7 exposing (solution)

import Dict exposing (diff)
import Input.Day1 exposing (input)
import Types exposing (GetSolution, Part(..))
import Utils


getAverage : List Int -> Int
getAverage nums =
    let
        sum =
            List.map toFloat nums
                |> List.sum

        length =
            List.length nums
                |> toFloat
    in
    sum
        / length
        |> round


calcCost : Int -> Int
calcCost diff =
    if diff <= 1 then
        diff

    else
        calcCost (diff - 1) + diff


getCost : Part -> List Int -> Int -> Int
getCost part nums pos =
    case part of
        Part1 ->
            List.map ((+) pos) nums
                |> List.map abs
                |> List.sum

        Part2 ->
            List.map ((-) pos) nums
                |> List.map abs
                |> List.map calcCost
                |> List.sum


step : Part -> (Int -> Int -> Int) -> List Int -> Int -> Int -> Int
step part direction nums prevPos prevCost =
    let
        newPos =
            direction prevPos 1

        newCost =
            getCost part nums newPos
    in
    if newCost < prevCost then
        step part direction nums newPos newCost

    else
        prevCost


getAnswer : Part -> List Int -> Maybe String
getAnswer part nums =
    let
        avg =
            getAverage nums

        avgCost =
            getCost part nums avg
    in
    List.minimum
        [ step part (-) nums avg avgCost
        , step part (+) nums avg avgCost
        ]
        |> Maybe.andThen (\n -> Just (String.fromInt n))


solution : GetSolution
solution input =
    let
        nums =
            Utils.intAfterSplit "," input
    in
    { part1 = getAnswer Part1 nums
    , part2 = getAnswer Part2 nums
    }
