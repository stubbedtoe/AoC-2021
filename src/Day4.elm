module Day4 exposing (parseInput, solution)

import Array exposing (Array)
import Day4input
import Types exposing (Solution)
import Utils


type alias BingoSquare =
    { value : Int
    , checked : Bool
    }


type alias BingoCard =
    { squares : List BingoSquare
    , width : Int
    , height : Int
    }


type alias Bingo =
    { card : Maybe BingoCard
    , num : Int
    , previousNums : List Int
    , otherCards : List BingoCard
    , currentNums : List Int
    }


type RoundResult
    = Success Bingo
    | Failure (List BingoCard)


type alias ParsedInput =
    { numbers : List Int
    , cards : List BingoCard
    }


getRowFromIndex : BingoCard -> Int -> List BingoSquare
getRowFromIndex { squares, width } start =
    List.drop start squares
        |> List.take width


getColumnFromIndex : BingoCard -> Int -> List BingoSquare
getColumnFromIndex { squares, height } start =
    Array.fromList squares
        |> Array.toIndexedList
        |> List.filter (\( i, _ ) -> modBy height i == start)
        |> List.map (\( _, s ) -> s)


getRows : BingoCard -> List (List BingoSquare)
getRows card =
    List.range 0 (card.height - 1)
        |> List.map (\i -> getRowFromIndex card i)


getColumns : BingoCard -> List (List BingoSquare)
getColumns card =
    List.range 0 (card.width - 1)
        |> List.map (\i -> getColumnFromIndex card i)


markNumberOnCard : Int -> BingoCard -> BingoCard
markNumberOnCard num card =
    { card
        | squares =
            List.map
                (\square ->
                    if square.value == num then
                        { square | checked = True }

                    else
                        square
                )
                card.squares
    }


isLineFull : List BingoSquare -> Bool
isLineFull line =
    List.all (\{ checked } -> checked == True) line


isCardFull : BingoCard -> Bool
isCardFull card =
    let
        isRowFull =
            getRows card
                |> List.any isLineFull

        isColumnFull =
            getColumns card
                |> List.any isLineFull
    in
    isRowFull || isColumnFull


checkCardsAfterNumber : Int -> Bingo -> Bingo
checkCardsAfterNumber num current =
    List.foldl
        (\card bingo ->
            if isCardFull card then
                { bingo
                    | card = Just card
                    , num = num
                    , previousNums = bingo.currentNums
                    , otherCards = List.filter (\c -> not (isCardFull c)) bingo.otherCards
                }

            else
                bingo
        )
        current
        current.otherCards


markNumberOnAllCards : Int -> Bingo -> Bingo
markNumberOnAllCards num current =
    let
        newCards =
            List.map (markNumberOnCard num) current.otherCards
    in
    checkCardsAfterNumber num { current | otherCards = newCards, currentNums = current.currentNums ++ [ num ] }


playBingoPart1 : List Int -> List BingoCard -> Bingo
playBingoPart1 nums cards =
    List.foldl
        (\num bingo ->
            case bingo.card of
                Just card ->
                    bingo

                Nothing ->
                    markNumberOnAllCards num bingo
        )
        { card = Nothing, num = 0, otherCards = cards, currentNums = [], previousNums = [] }
        nums


playBingoPart2 : List Int -> List BingoCard -> Bingo
playBingoPart2 nums cards =
    let
        allNumsCalled =
            List.foldl
                markNumberOnAllCards
                { card = Nothing, num = 0, otherCards = cards, currentNums = [], previousNums = [] }
                nums
    in
    List.foldl
        markNumberOnAllCards
        { card = Nothing, num = 0, otherCards = cards, currentNums = [], previousNums = [] }
        allNumsCalled.previousNums


bingoToString : Bingo -> String
bingoToString { card, num } =
    case card of
        Just bingoCard ->
            List.filter (\{ checked } -> checked == False) bingoCard.squares
                |> List.map .value
                |> List.sum
                |> (*) num
                |> String.fromInt

        Nothing ->
            "No card completed :("


parseBingoCard : List String -> BingoCard
parseBingoCard lines =
    let
        squares =
            List.map (String.replace "  " " ") lines
                |> List.map String.trim
                |> List.map (\line -> Utils.intAfterSplit " " line)
                |> List.concatMap (List.map (\i -> { value = i, checked = False }))

        height =
            List.length lines

        width =
            List.length squares // height
    in
    { squares = squares
    , width = width
    , height = height
    }


parseCards : List String -> List BingoCard
parseCards lines =
    Utils.splitOnEmptyLine lines
        |> List.map parseBingoCard


parseInput : String -> ParsedInput
parseInput input =
    case String.lines input of
        numbers :: _ :: rest ->
            { numbers = Utils.intAfterSplit "," numbers
            , cards = parseCards rest
            }

        _ ->
            { numbers = [], cards = [] }


part1 : String
part1 =
    parseInput Day4input.input
        |> (\{ numbers, cards } ->
                playBingoPart1 numbers cards
                    |> bingoToString
           )


part2 : String
part2 =
    parseInput Day4input.input
        |> (\{ numbers, cards } ->
                playBingoPart2 numbers cards
                    |> bingoToString
           )


solution : Solution
solution =
    { part1 = Just part1
    , part2 = Just part2
    }
