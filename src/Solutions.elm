module Solutions exposing (..)

import Solutions.Day1
import Solutions.Day10
import Solutions.Day11
import Solutions.Day12
import Solutions.Day13
import Solutions.Day2
import Solutions.Day3
import Solutions.Day4
import Solutions.Day5
import Solutions.Day6
import Solutions.Day7
import Solutions.Day8
import Solutions.Day9
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

        Day7 ->
            Solutions.Day7.solution

        Day8 ->
            Solutions.Day8.solution

        Day9 ->
            Solutions.Day9.solution

        Day10 ->
            Solutions.Day10.solution

        Day11 ->
            Solutions.Day11.solution

        Day12 ->
            Solutions.Day12.solution

        Day13 ->
            Solutions.Day13.solution
