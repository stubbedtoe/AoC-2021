module Utils exposing (intAfterSplit, intsFromLines, splitOnEmptyLine)


intsFromLines : String -> List Int
intsFromLines lines =
    String.lines lines
        |> List.filterMap String.toInt


intAfterSplit : String -> String -> List Int
intAfterSplit sep line =
    String.split sep line
        |> List.filterMap String.toInt


splitOnEmptyLine : List String -> List (List String)
splitOnEmptyLine lines =
    List.foldr
        (\line { current, finished } ->
            if String.isEmpty line then
                { current = []
                , finished = current :: finished
                }

            else
                { current = line :: current
                , finished = finished
                }
        )
        { current = [], finished = [] }
        lines
        |> (\{ current, finished } -> current :: finished)
