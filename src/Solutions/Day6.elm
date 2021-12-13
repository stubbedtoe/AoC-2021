module Solutions.Day6 exposing (solution)

import Dict exposing (Dict)
import Types exposing (GetSolution)
import Utils



-- type alias LaternFish =
--     Int
-- passDay : LaternFish -> List LaternFish
-- passDay days =
--     if days == 0 then
--         [ 6, 8 ]
--     else
--         [ days - 1 ]
-- dayForAll : List LaternFish -> List LaternFish
-- dayForAll fish =
--     List.concatMap passDay fish


{-| Recalculate starting day as if we just spawned, ie. 9
days to next spawn.
-}
population : Int -> Int -> Int
population days initial =
    populationAfter (days + 8 - initial)


{-| Calculate population after N days.
First spawn after 9 days, then spawn every 7 days.
Basically, this is just a memoized version of:
popAfter : Int -> Int
popAfter days =
if days < 9 then
1
else
popAfter (days - 7) + popAfter (days - 9)
Does it remind you of recursive Fibonacci maybe?
-}
populationAfter : Int -> Int
populationAfter days =
    populationAfterMem Dict.empty days
        |> Tuple.first


populationAfterMem : Dict Int Int -> Int -> ( Int, Dict Int Int )
populationAfterMem mem d =
    case Dict.get d mem of
        Just v ->
            ( v, mem )

        Nothing ->
            if d < 9 then
                ( 1, Dict.insert d 1 mem )

            else
                let
                    ( v1, mem1 ) =
                        populationAfterMem mem (d - 7)

                    ( v2, mem2 ) =
                        populationAfterMem mem1 (d - 9)
                in
                ( v1 + v2
                , Dict.insert d (v1 + v2) mem2
                )



-- nDays : Int -> List LaternFish -> String
-- nDays days fish =
--     List.range 1 (days // 8)
--         |> List.foldl
--             (\day fishes ->
--                 let
--                     _ =
--                         Debug.log ("day " ++ String.fromInt (day * 8) ++ " of ") (String.fromInt days)
--                 in
--                 List.concatMap after8Days fishes
--             )
--             fish
--         |> List.length
--         |> String.fromInt
-- after8Days : LaternFish -> List LaternFish
-- after8Days fish =
--     case fish of
--         8 ->
--             [ 0 ]
--         7 ->
--             [ 6, 8 ]
--         6 ->
--             [ 5, 7 ]
--         5 ->
--             [ 4, 6 ]
--         4 ->
--             [ 3, 5 ]
--         3 ->
--             [ 2, 4 ]
--         2 ->
--             [ 1, 3 ]
--         1 ->
--             [ 0, 2 ]
--         _ ->
--             []


part1 : List Int -> Maybe String
part1 nums =
    List.map (population 80) nums
        |> List.sum
        |> String.fromInt
        |> Just


part2 : List Int -> Maybe String
part2 nums =
    List.map (population 256) nums
        |> List.sum
        |> String.fromInt
        |> Just


solution : GetSolution
solution input =
    let
        nums =
            Utils.intAfterSplit "," input
    in
    { part1 = part1 nums
    , part2 = part2 nums
    }
