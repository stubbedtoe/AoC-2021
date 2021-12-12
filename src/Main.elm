module Main exposing (..)

import Browser
import Day1
import Day2
import Day3
import Day4
import Html exposing (Html, button, div, p, text)
import Html.Events exposing (onClick)
import Types exposing (Solution)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type Day
    = Day Int



-- MODEL


type alias Model =
    Maybe Solution


daysCompleted : List Day
daysCompleted =
    [ Day 1
    , Day 2
    , Day 3
    , Day 4
    ]


init : Model
init =
    Nothing



-- UPDATE


type Msg
    = Select Day


update : Msg -> Model -> Model
update msg _ =
    case msg of
        Select (Day num) ->
            case num of
                1 ->
                    Just Day1.solution

                2 ->
                    Just Day2.solution

                3 ->
                    Just Day3.solution

                4 ->
                    Just Day4.solution

                _ ->
                    Nothing



-- VIEW


printPart : Maybe String -> String
printPart =
    Maybe.withDefault "No answer yet"


printSolution : Maybe Solution -> Html Msg
printSolution solution =
    case solution of
        Nothing ->
            p [] [ text "No day selected" ]

        Just { part1, part2 } ->
            div []
                [ p [] [ text ("Part 1: " ++ printPart part1) ]
                , p [] [ text ("Part 2: " ++ printPart part2) ]
                ]


makeButton : Day -> Html Msg
makeButton day =
    case day of
        Day num ->
            button [ onClick (Select day) ] [ text (String.fromInt num) ]


view : Model -> Html Msg
view model =
    div []
        (List.append
            (List.map makeButton daysCompleted)
            [ printSolution model ]
        )
