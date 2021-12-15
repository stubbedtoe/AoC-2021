module Solutions.Day11 exposing (solution)

import Array exposing (Array)
import Input.Day1 exposing (input)
import Types exposing (GetSolution)
import Utils


type alias Octopus =
    { energy : Int
    , neighbours : List Int
    , index : Int
    , hasFlashed : Bool
    }


type alias Grid =
    { octopi : Array Octopus
    , numFlashes : Int
    , stepsComplete : Int
    }


getNeighbourIndices : Int -> List Int
getNeighbourIndices index =
    let
        left =
            index - 1

        right =
            index + 1

        above =
            index - 10

        below =
            index + 10

        aboveLeft =
            index - 11

        aboveRight =
            index - 9

        belowLeft =
            index + 9

        belowRight =
            index + 11
    in
    -- assumes a 10 x 10 grid
    if modBy 10 index == 0 then
        -- on Left side
        [ right, aboveRight, belowRight, above, below ]

    else if modBy 10 (index + 1) == 0 then
        -- on Right side
        [ left, aboveLeft, belowLeft, above, below ]

    else
        [ left, aboveLeft, belowLeft, right, aboveRight, belowRight, above, below ]


haveAllFlashed : Grid -> Bool
haveAllFlashed grid =
    Array.toList grid.octopi
        |> List.map .energy
        |> List.sum
        |> (==) 0


parseInput : String -> Grid
parseInput input =
    { octopi =
        String.lines input
            |> List.concatMap (Utils.intAfterSplit "")
            |> Array.fromList
            |> Array.indexedMap
                (\i level ->
                    { energy = level
                    , neighbours = getNeighbourIndices i
                    , index = i
                    , hasFlashed = False
                    }
                )
    , numFlashes = 0
    , stepsComplete = 0
    }


increaseAndMaybeFlash : Int -> Grid -> Grid
increaseAndMaybeFlash index grid =
    case Array.get index grid.octopi of
        Just octopus ->
            let
                newEnergy =
                    octopus.energy + 1

                newOctopus =
                    { octopus | energy = newEnergy }
            in
            if newEnergy > 9 && octopus.hasFlashed == False then
                List.foldl
                    increaseAndMaybeFlash
                    { grid
                        | octopi = Array.set index { newOctopus | hasFlashed = True } grid.octopi
                        , numFlashes = grid.numFlashes + 1
                    }
                    octopus.neighbours

            else
                { grid | octopi = Array.set index newOctopus grid.octopi }

        Nothing ->
            grid


reset : Grid -> Grid
reset grid =
    let
        newOctopi =
            Array.map
                (\octopus ->
                    if octopus.hasFlashed then
                        { octopus | hasFlashed = False, energy = 0 }

                    else
                        octopus
                )
                grid.octopi
    in
    { grid | octopi = newOctopi }


part1Steps : Grid -> String
part1Steps grid =
    if grid.stepsComplete == 100 then
        grid.numFlashes
            |> String.fromInt

    else
        let
            newGrid =
                List.foldl
                    increaseAndMaybeFlash
                    grid
                    (List.range 0 (Array.length grid.octopi - 1))
                    |> reset
        in
        part1Steps { newGrid | stepsComplete = grid.stepsComplete + 1 }


part2Steps : Grid -> String
part2Steps grid =
    if haveAllFlashed grid then
        grid.stepsComplete
            |> String.fromInt

    else
        let
            newGrid =
                List.foldl
                    increaseAndMaybeFlash
                    grid
                    (List.range 0 (Array.length grid.octopi - 1))
                    |> reset
        in
        part2Steps { newGrid | stepsComplete = grid.stepsComplete + 1 }


solution : GetSolution
solution input =
    let
        grid =
            parseInput input
    in
    { part1 = Just (part1Steps grid)
    , part2 = Just (part2Steps grid)
    }
