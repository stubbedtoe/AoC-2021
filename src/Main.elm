module Main exposing (..)

import Browser
import Html exposing (Html, br, button, div, option, p, select, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import Input
import Solutions
import Solutions.Day1 exposing (solution)
import Types exposing (DaysComplete(..), InputType(..), Solution)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type Day
    = Day Int



-- MODEL


type alias Model =
    { solution : Maybe Solution
    , inputType : InputType
    , day : Maybe DaysComplete
    }


daysCompleted : List ( String, DaysComplete )
daysCompleted =
    [ ( "1", Day1 )
    , ( "2", Day2 )
    , ( "3", Day3 )
    , ( "4", Day4 )
    , ( "5", Day5 )
    , ( "6", Day6 )
    , ( "7", Day7 )
    , ( "8", Day8 )
    , ( "9", Day9 )
    , ( "10", Day10 )
    , ( "11", Day11 )
    , ( "12", Day12 )
    , ( "13", Day13 )
    ]


init : Model
init =
    { solution = Nothing
    , day = Nothing
    , inputType = Test
    }



-- UPDATE


type Msg
    = Select DaysComplete
    | UpdateInput String
    | Solve


update : Msg -> Model -> Model
update msg model =
    case msg of
        Solve ->
            case model.day of
                Just day ->
                    { model | solution = Just (Solutions.getSolution day (Input.getInput day model.inputType)) }

                Nothing ->
                    { model | solution = Nothing }

        Select day ->
            update Solve { model | day = Just day }

        UpdateInput inputType ->
            case inputType of
                "test" ->
                    update Solve { model | inputType = Test }

                "input" ->
                    update Solve { model | inputType = Input }

                _ ->
                    model



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


makeButton : Maybe DaysComplete -> ( String, DaysComplete ) -> Html Msg
makeButton daySelected ( label, day ) =
    button
        [ onClick (Select day)
        , style "background-color"
            (case daySelected of
                Just d ->
                    if d == day then
                        "yellow"

                    else
                        "white"

                Nothing ->
                    "white"
            )
        ]
        [ text label ]


selectInput : Html Msg
selectInput =
    select [ onInput UpdateInput, style "margin-top" "2rem" ]
        [ option [ value "test" ] [ text "test" ]
        , option [ value "input" ] [ text "input" ]
        ]


view : Model -> Html Msg
view model =
    div []
        (List.append
            (List.map (makeButton model.day) daysCompleted)
            [ br [] []
            , selectInput
            , printSolution model.solution
            ]
        )
