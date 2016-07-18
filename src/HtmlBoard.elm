module Main exposing (..)

import Html exposing (Html, div, span, h1, text, img, button)
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
import String


config =
    { rows = 50
    , cols = 60
    , boardWidth = 500
    , boardHeight = 600
    , cellRounding = 8
    , deadColour = "#00000F"
    , aliveColour = "#EEEEEE"
    , speedDelta = 50
    , defaultSpeed = 500
    }


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { board : Board
    , speed : Int
    , paused : Bool
    }


{-| This is available as shmookey/cmd-extra but inline here to comprehend
-}
message : msg -> Cmd msg
message x =
    Task.perform identity identity (Task.succeed x)



-- UPDATE


init : ( Model, Cmd Msg )
init =
    ( { board = Board.makeEmpty config.cols config.rows
      , speed = config.defaultSpeed
      , paused = False
      }
    , message GenerateRandomBoard
    )


type Msg
    = Tick Time
    | Pause
    | Step
    | GenerateRandomBoard
    | SpeedDelta Int
    | NewBoard Board


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        randomBoardGenerator =
            Random.list config.cols Random.bool
                |> Random.list config.rows
                |> Random.map Board.fromList
    in
        case msg of
            Pause ->
                ( { model | paused = not model.paused }, Cmd.none )

            SpeedDelta delta ->
                ( { model | speed = model.speed + delta }, Cmd.none )

            Tick newTime ->
                let
                    cmd =
                        if model.paused then
                            Cmd.none
                        else
                            message Step
                in
                    ( model, cmd )

            Step ->
                ( { model | board = Board.nextGen model.board }, Cmd.none )

            GenerateRandomBoard ->
                ( model, Random.generate NewBoard randomBoardGenerator )

            NewBoard newBoard ->
                ( { model | board = newBoard }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { speed } =
    Time.every (toFloat speed * millisecond) Tick



-- VIEW


makeSquare tx ty status =
    let
        cellWidth =
            config.boardWidth // config.cols

        cellHeight =
            config.boardHeight // config.rows

        colour =
            if status then
                config.deadColour
            else
                config.aliveColour
    in
        rect
            [ x <| toString <| tx * cellWidth
            , y <| toString <| ty * cellHeight
            , width <| toString cellWidth
            , height <| toString cellHeight
            , rx <| toString config.cellRounding
            , ry <| toString config.cellRounding
            , fill colour
            ]
            []


view : Model -> Html Msg
view { board, speed, paused } =
    let
        flattenedBoard =
            Board.flatten board

        toSquare ( col, row, status ) =
            makeSquare col row status

        viewPort =
            [ 0
            , 0
            , config.boardWidth
            , config.boardHeight
            ]
    in
        div []
            [ svg
                [ viewBox <| String.join " " (List.map toString viewPort)
                , width <| (toString config.boardWidth) ++ "px"
                , height <| (toString config.boardHeight) ++ "px"
                ]
                (List.map
                    toSquare
                    flattenedBoard
                )
            , div []
                [ if paused then
                    span []
                        [ button [ onClick Pause ] [ text "Play" ]
                        , button [ onClick Step ] [ text "Step" ]
                        ]
                  else
                    button [ onClick Pause ] [ text "Pause" ]
                , button [ onClick (SpeedDelta (-config.speedDelta)) ] [ text "-" ]
                , text <|
                    (if paused then
                        "Paused"
                     else
                        toString speed
                    )
                , text <| " height " ++ (toString <| config.boardHeight // config.rows)
                , text <| " width " ++ (toString <| config.boardWidth // config.cols)
                , button [ onClick (SpeedDelta config.speedDelta) ] [ text "+" ]
                , button [ onClick GenerateRandomBoard ] [ text "Randomize" ]
                ]
            ]
