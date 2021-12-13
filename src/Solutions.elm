module Solutions exposing (..)

import Solutions.Day1
import Solutions.Day2
import Solutions.Day3
import Solutions.Day4
import Solutions.Day5
import Solutions.Day6
import Types exposing (DaysComplete(..), GetSolution)


getSolution : DaysComplete -> GetSolution
getSolution day =
    case day of
        Day1 ->
            Solutions.Day1.solution

        Day2 ->
            Solutions.Day2.solution

        Day3 ->
            Solutions.Day3.solution

        Day4 ->
            Solutions.Day4.solution

        Day5 ->
            Solutions.Day5.solution

        Day6 ->
            Solutions.Day6.solution
