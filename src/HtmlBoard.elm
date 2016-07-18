module Main exposing (..)

import Html exposing (Html, div, h1, text, img, button)
import Html.Events exposing (onClick)
import Html.Attributes
import Html.App as App
import Svg exposing (svg, rect)
import Svg.Attributes exposing (..)
import Time exposing (Time, second, millisecond)
import Board exposing (Board)
import BoardSpec exposing (fromSpec, trimSpec)
import Arrangements exposing (stabalisingToRepeating)
import Random
import Array
import Task


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    ( Board, Int )


{-| This is available as shmookey/cmd-extra but inline here to comprehend
-}
message : msg -> Cmd msg
message x =
    Task.perform identity identity (Task.succeed x)



-- UPDATE


init : ( Model, Cmd Msg )
init =
    ( ( Board.makeEmpty 10 10, 500 ), message GenerateRandomBoard )


type Msg
    = Tick Time
    | Pause
    | GenerateRandomBoard
    | SpeedDelta Int
    | NewBoard Board


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( board, speed ) =
    let
        randomBoardGenerator =
            Random.map Board.fromList (Random.list 10 (Random.list 10 Random.bool))
    in
        case msg of
            Pause ->
                ( ( board
                  , if speed > 0 then
                        0
                    else
                        500
                  )
                , Cmd.none
                )

            SpeedDelta delta ->
                ( ( board, speed + delta ), Cmd.none )

            Tick newTime ->
                let
                    newBoard =
                        if speed > 0 then
                            Board.nextGen board
                        else
                            board
                in
                    ( ( newBoard, speed ), Cmd.none )

            GenerateRandomBoard ->
                ( ( board, speed ), Random.generate NewBoard randomBoardGenerator )

            NewBoard board ->
                ( ( board, speed ), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions ( board, speed ) =
    Time.every (toFloat speed * millisecond) Tick



-- VIEW


makeSquare tx ty status =
    let
        widthMultiplier =
            50

        heightMultiplier =
            50

        colour =
            if status then
                "#00000F"
            else
                "#EEEEEE"
    in
        rect
            [ x (toString (tx * widthMultiplier))
            , y (toString (ty * heightMultiplier))
            , width (toString widthMultiplier)
            , height (toString heightMultiplier)
            , rx "5"
            , ry "5"
            , fill colour
            ]
            []


view : Model -> Html Msg
view ( board, speed ) =
    let
        flattenedBoard =
            Board.flatten board

        toSquare ( col, row, status ) =
            makeSquare col row status
    in
        div
            [ Html.Attributes.style
                [ ( "margin-left", "2em" )
                ]
            ]
            [ h1 []
                [ text
                    "Game of life in elm"
                ]
            , img
                [ Html.Attributes.src "tree.png"
                , Html.Attributes.style
                    [ ( "position", "absolute" )
                    , ( "right", "0" )
                    , ( "top", "0" )
                    , ( "opacity", "0.1" )
                    ]
                ]
                []
            , svg
                [ viewBox "0 0 500 500", width "500px" ]
                (List.map
                    toSquare
                    flattenedBoard
                )
            , div []
                [ button [ onClick Pause ] [ text "Pause" ]
                , button [ onClick (SpeedDelta (-50)) ] [ text "-" ]
                , text <| toString speed
                , button [ onClick (SpeedDelta 50) ] [ text "+" ]
                , button [ onClick GenerateRandomBoard ] [ text "Randomize" ]
                ]
            ]
