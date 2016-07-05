module BoardTests exposing (..)

import ElmTest exposing (Test, suite, equals, assertEqual, test)
import Board
import BoardSpec exposing (trimSpec, fromSpec, toSpec)
import Array exposing (fromList)
import Maybe exposing (Maybe(Just, Nothing))
import List


{-| A set of simple board specifications: laid out as text for readability
-}
empty2By3 =
    trimSpec """..
                ..
                ..
                """


filled2By3 =
    trimSpec """**
                **
                **
                """


tests : Test
tests =
    suite "Board construction tests"
        [ testSingleCell
        , testEmpty
        , testFilled
        , testToSpec
        , testToSpecFilled
        , testKill
        , testVivify
        , testFlatten
        ]


testSingleCell =
    test "Single cell board"
        (assertEqual (Board.makeEmpty 1 1) (fromSpec "."))


testEmpty =
    Board.makeEmpty 2 3 `equals` fromSpec empty2By3


testFilled =
    let
        filledBoard =
            fromSpec filled2By3
    in
        suite "Test all filled"
            [ Board.get 0 0 filledBoard `equals` True
            , Board.get 1 0 filledBoard `equals` True
            , Board.get 0 1 filledBoard `equals` True
            , Board.get 1 1 filledBoard `equals` True
            , Board.get 0 2 filledBoard `equals` True
            , Board.get 1 2 filledBoard `equals` True
            , Board.get 9000 9000 filledBoard `equals` False
            ]


testKill =
    let
        filledBoard =
            Board.kill 1 1 (fromSpec filled2By3)
    in
        suite "Test killed correctly filled"
            [ Board.get 0 0 filledBoard `equals` True
            , Board.get 1 0 filledBoard `equals` True
            , Board.get 0 1 filledBoard `equals` True
            , Board.get 1 1 filledBoard `equals` False
            , Board.get 0 2 filledBoard `equals` True
            , Board.get 1 2 filledBoard `equals` True
            ]


testVivify =
    let
        filledBoard =
            Board.vivify 1 1 (fromSpec empty2By3)
    in
        suite "Test killed correctly filled"
            [ Board.get 0 0 filledBoard `equals` False
            , Board.get 1 0 filledBoard `equals` False
            , Board.get 0 1 filledBoard `equals` False
            , Board.get 1 1 filledBoard `equals` True
            , Board.get 0 2 filledBoard `equals` False
            , Board.get 1 2 filledBoard `equals` False
            ]


testToSpec =
    empty2By3 `equals` (toSpec (Board.makeEmpty 2 3))


testFlatten =
    (Board.flatten <| Board.makeEmpty 2 3)
        `equals`
            [ ( 0, 0, False )
            , ( 1, 0, False )
            , ( 0, 1, False )
            , ( 1, 1, False )
            , ( 0, 2, False )
            , ( 1, 2, False )
            ]


testToSpecFilled =
    (toSpec <| fromSpec filled2By3) `equals` filled2By3
