module Solutions.Day2 exposing (solution)

import Types exposing (GetSolution)


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
    case commands of
        next :: rest ->
            case next of
                Forward n ->
                    calculatePosition { current | horizontal = current.horizontal + n } rest

                Down n ->
                    calculatePosition { current | depth = current.depth + n } rest

                Up n ->
                    calculatePosition { current | depth = current.depth - n } rest

        [] ->
            current


calculatePositionWithAim : PositionWithAim -> List Command -> PositionWithAim
calculatePositionWithAim current commands =
    case commands of
        next :: rest ->
            case next of
                Forward n ->
                    let
                        newDepth =
                            current.depth + (current.aim * n)
                    in
                    calculatePositionWithAim { current | horizontal = current.horizontal + n, depth = newDepth } rest

                Down n ->
                    calculatePositionWithAim { current | aim = current.aim + n } rest

                Up n ->
                    calculatePositionWithAim { current | aim = current.aim - n } rest

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


parseInput : String -> List Command
parseInput input =
    String.lines input
        |> List.filterMap parseLine


part1 : String -> String
part1 input =
    parseInput input
        |> calculatePosition { depth = 0, horizontal = 0 }
        |> positionToPart1


part2 : String -> String
part2 input =
    parseInput input
        |> calculatePositionWithAim { depth = 0, horizontal = 0, aim = 0 }
        |> positionToPart2


solution : GetSolution
solution input =
    { part1 = Just (part1 input)
    , part2 = Just (part2 input)
    }
