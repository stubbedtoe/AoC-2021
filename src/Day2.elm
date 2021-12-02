module Day2 exposing (solution)

import Day2input
import Types exposing (Solution)


type Command
    = Forward Int
    | Down Int
    | Up Int


type alias Position =
    { depth : Int
    , horizontal : Int
    }


type alias PositionWithAim =
    { depth : Int
    , horizontal : Int
    , aim : Int
    }


positionToPart1 : Position -> String
positionToPart1 { depth, horizontal } =
    String.fromInt (depth * horizontal)


positionToPart2 : PositionWithAim -> String
positionToPart2 { depth, horizontal } =
    String.fromInt (depth * horizontal)


calculatePosition : Position -> List Command -> Position
calculatePosition current commands =
    case current of
        { depth, horizontal } ->
            case commands of
                next :: rest ->
                    case next of
                        Forward n ->
                            calculatePosition { current | horizontal = horizontal + n } rest

                        Down n ->
                            calculatePosition { current | depth = depth + n } rest

                        Up n ->
                            calculatePosition { current | depth = depth - n } rest

                [] ->
                    current


calculatePositionWithAim : PositionWithAim -> List Command -> PositionWithAim
calculatePositionWithAim current commands =
    case current of
        { depth, horizontal, aim } ->
            case commands of
                next :: rest ->
                    case next of
                        Forward n ->
                            calculatePositionWithAim { current | horizontal = horizontal + n, depth = depth + (aim * n) } rest

                        Down n ->
                            calculatePositionWithAim { current | aim = aim + n } rest

                        Up n ->
                            calculatePositionWithAim { current | aim = aim - n } rest

                [] ->
                    current


parseLine : String -> Maybe Command
parseLine line =
    case String.split " " line of
        cmd :: num :: [] ->
            case String.toInt num of
                Just n ->
                    case cmd of
                        "forward" ->
                            Just (Forward n)

                        "down" ->
                            Just (Down n)

                        "up" ->
                            Just (Up n)

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


parseInput : List Command
parseInput =
    String.lines Day2input.input
        |> List.filterMap parseLine


part1 : String
part1 =
    parseInput
        |> calculatePosition { depth = 0, horizontal = 0 }
        |> positionToPart1


part2 : String
part2 =
    parseInput
        |> calculatePositionWithAim { depth = 0, horizontal = 0, aim = 0 }
        |> positionToPart2


solution : Solution
solution =
    { part1 = Just part1
    , part2 = Just part2
    }
