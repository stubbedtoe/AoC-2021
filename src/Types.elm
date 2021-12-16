module Types exposing (..)


type alias Solution =
    { part1 : Maybe String
    , part2 : Maybe String
    }


type alias GetSolution =
    String -> Solution


type DaysComplete
    = Day1
    | Day2
    | Day3
    | Day4
    | Day5
    | Day6
    | Day7
    | Day8
    | Day9
    | Day10
    | Day11
    | Day12


type InputType
    = Test
    | Input


type Part
    = Part1
    | Part2
