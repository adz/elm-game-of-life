module Main exposing (..)

import Html exposing (Html, div, span, h1, text, img, button)
import Html.Events exposing (onClick)
import Html
import Svg exposing (svg, rect)
import Svg.Attributes exposing (..)
import Time exposing (Time, second, millisecond)
import Board exposing (Board)
import Random
import Task
import String


-- Try out different pre-defined boards as specifications
-- import Arrangements exposing (stabalisingToRepeating)
-- import BoardSpec exposing (fromSpec, trimSpec)


config =
    { rows = 60
    , cols = 80
    , boardWidth = 800
    , boardHeight = 600
    , cellRounding = 3
    , deadColour = "#EEEEEE"
    , aliveColour = "rgba(71,161,77,0.6)"
    , speedDelta = 50
    , defaultSpeed = 500
    }


main : Program Never Model Msg
main =
    Html.program
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
    Task.perform identity (Task.succeed x)



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


makeSquare : Int -> Int -> Bool -> Svg.Svg msg
makeSquare tx ty status =
    let
        cellWidth =
            config.boardWidth // config.cols

        cellHeight =
            config.boardHeight // config.rows

        colour =
            if status then
                config.aliveColour
            else
                config.deadColour
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

        viewBoxParams =
            [ 0
            , 0
            , config.boardWidth
            , config.boardHeight
            ]

        stringifyViewPort =
            String.join " " << List.map toString

        toolbar =
            [ text "Speed: "
            , button [ onClick (SpeedDelta (-config.speedDelta)) ] [ text "-" ]
            , text <| toString speed
            , button [ onClick (SpeedDelta config.speedDelta) ] [ text "+" ]
            , button [ onClick GenerateRandomBoard ] [ text "Randomize" ]
            , if paused then
                span []
                    [ button [ onClick Pause ] [ text "Play" ]
                    , button [ onClick Step ] [ text "Step" ]
                    ]
              else
                button [ onClick Pause ] [ text "Pause" ]
            ]
    in
        div []
            [ div [] toolbar
            , svg
                [ viewBox <| stringifyViewPort viewBoxParams
                , width <| (toString config.boardWidth) ++ "px"
                , height <| (toString config.boardHeight) ++ "px"
                ]
                (List.map
                    toSquare
                    flattenedBoard
                )
            ]
