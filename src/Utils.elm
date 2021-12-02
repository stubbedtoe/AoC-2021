module Utils exposing (intsFromLines)


intsFromLines : String -> List Int
intsFromLines lines =
    String.lines lines
        |> List.filterMap String.toInt
