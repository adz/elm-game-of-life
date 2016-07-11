module Main exposing (..)

import Html exposing (Html, div, h1, text, img)
import Html.Attributes
import Html.App as App
import Svg exposing (svg, rect)
import Svg.Attributes exposing (..)
import Time exposing (Time, second, millisecond)
import Board exposing (Board)
import BoardSpec exposing (fromSpec, trimSpec)


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


stabalisingToRepeating =
    """
    ...........
    .....*.....
    ......*....
    ....***....
    ...........
    ...........
    ....*......
    ....*......
    ....*......
    ...........
    ...........
    """


stabalisingToStatic =
    """
    ...........
    .....*.....
    ......*....
    ....***....
    ...........
    ..*........
    ....*......
    ....*......
    ....*......
    ...........
    ...........
    """


init : ( Model, Cmd Msg )
init =
    ( fromSpec <| trimSpec stabalisingToRepeating, Cmd.none )


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg board =
    case msg of
        Tick newTime ->
            ( Board.nextGen board, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (500 * millisecond) Tick



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
view board =
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
                [ viewBox "0 0 1000 1000", width "1000px" ]
                (List.map
                    toSquare
                    flattenedBoard
                )
            ]
