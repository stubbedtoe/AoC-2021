module Solutions.Day1 exposing (solution)

import Types exposing (GetSolution)
import Utils


solution : GetSolution
solution input =
    { part1 = Just (part1 input)
    , part2 = Just (part2 input)
    }


countPreceedBySmaller : List Int -> Int -> Int
countPreceedBySmaller list count =
    case list of
        prev :: curr :: rest ->
            if prev < curr then
                countPreceedBySmaller (curr :: rest) (count + 1)

            else
                countPreceedBySmaller (curr :: rest) count

        _ ->
            count


sumOfThree : List Int -> List Int -> List Int
sumOfThree list current =
    case list of
        a :: b :: c :: rest ->
            sumOfThree (b :: c :: rest) (List.append current [ a + b + c ])

        _ ->
            current


part1 : String -> String
part1 input =
    let
        nums =
            Utils.intsFromLines input

        increasedFromPrevious =
            countPreceedBySmaller nums 0
    in
    String.fromInt increasedFromPrevious


part2 : String -> String
part2 input =
    let
        nums =
            Utils.intsFromLines input

        windowOfThree =
            sumOfThree nums []

        increasedFromPrevious =
            countPreceedBySmaller windowOfThree 0
    in
    String.fromInt increasedFromPrevious
