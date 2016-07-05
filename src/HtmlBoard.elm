module Main exposing (..)

import Html exposing (Html)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Board exposing (Board)


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    Board


init : ( Model, Cmd Msg )
init =
    ( Board.makeEmpty 10 10, Cmd.none )



-- UPDATE


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg board =
    case msg of
        Tick newTime ->
            ( Board.vivify 1 1 board, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (5 * second) Tick



-- VIEW


makeSquare tx ty status =
    let
        colour =
            if status then
                "#FFFFFF"
            else
                "#000000"
    in
        rect [ x (toString (tx * 60)), y (toString (ty * 60)), width "60", height "60", rx "5", ry "5", fill colour ] []


view : Model -> Html Msg
view board =
    let
        flattenedBoard =
            Board.flatten board

        toSquare ( col, row, status ) =
            makeSquare col row status
    in
        svg [ viewBox "0 0 1000 1000", width "1000px" ]
            (List.map
                toSquare
                flattenedBoard
            )
