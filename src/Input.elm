module Input exposing (..)

import Input.Day1
import Input.Day10
import Input.Day11
import Input.Day12
import Input.Day13
import Input.Day2
import Input.Day3
import Input.Day4
import Input.Day5
import Input.Day6
import Input.Day7
import Input.Day8
import Input.Day9
import Types exposing (DaysComplete(..), InputType(..))


getInput : DaysComplete -> InputType -> String
getInput day input =
    case day of
        Day1 ->
            case input of
                Test ->
                    Input.Day1.test

                Input ->
                    Input.Day1.input

        Day2 ->
            case input of
                Test ->
                    Input.Day2.test

                Input ->
                    Input.Day2.input

        Day3 ->
            case input of
                Test ->
                    Input.Day3.test

                Input ->
                    Input.Day3.input

        Day4 ->
            case input of
                Test ->
                    Input.Day4.test

                Input ->
                    Input.Day4.input

        Day5 ->
            case input of
                Test ->
                    Input.Day5.test

                Input ->
                    Input.Day5.input

        Day6 ->
            case input of
                Test ->
                    Input.Day6.test

                Input ->
                    Input.Day6.input

        Day7 ->
            case input of
                Test ->
                    Input.Day7.test

                Input ->
                    Input.Day7.input

        Day8 ->
            case input of
                Test ->
                    Input.Day8.test

                Input ->
                    Input.Day8.input

        Day9 ->
            case input of
                Test ->
                    Input.Day9.test

                Input ->
                    Input.Day9.input

        Day10 ->
            case input of
                Test ->
                    Input.Day10.test

                Input ->
                    Input.Day10.input

        Day11 ->
            case input of
                Test ->
                    Input.Day11.test

                Input ->
                    Input.Day11.input

        Day12 ->
            case input of
                Test ->
                    Input.Day12.test

                Input ->
                    Input.Day12.input

        Day13 ->
            case input of
                Test ->
                    Input.Day13.test

                Input ->
                    Input.Day13.input
