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


type InputType
    = Test
    | Input
