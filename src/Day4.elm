module Day4 exposing (parseInput, solution)

import Array
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


type alias WinningCard =
    { card : BingoCard
    , num : Int
    }


type alias Bingo =
    { cards : List WinningCard
    , otherCards : List BingoCard
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
        |> List.map (\i -> getRowFromIndex card (i * card.width))


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
                { cards = bingo.cards ++ [ { card = card, num = num } ]
                , otherCards = List.filter (\c -> c /= card) bingo.otherCards
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
    checkCardsAfterNumber num { current | otherCards = newCards }


playBingo : ParsedInput -> Bingo
playBingo { numbers, cards } =
    List.foldl
        markNumberOnAllCards
        { cards = [], otherCards = cards }
        numbers


bingoToString : Maybe WinningCard -> String
bingoToString maybeWinningCard =
    case maybeWinningCard of
        Just { card, num } ->
            List.filter (\{ checked } -> checked == False) card.squares
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


solution : Solution
solution =
    let
        bingo =
            parseInput Day4input.input
                |> playBingo

        part1Card =
            List.head bingo.cards

        part2Card =
            List.reverse bingo.cards
                |> List.head
    in
    { part1 = Just (bingoToString part1Card)
    , part2 = Just (bingoToString part2Card)
    }
