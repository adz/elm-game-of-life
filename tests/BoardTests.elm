module BoardTests exposing (tests)

import Test exposing (Test, describe, test)
import Expect
import Board
import BoardSpec exposing (trimSpec, fromSpec, toSpec)


{-| A set of simple board specifications: laid out as text for readability
-}
specs : { empty2By3 : String, filled2By3 : String }
specs =
    { empty2By3 =
        trimSpec """
                ..
                ..
                ..
                """
    , filled2By3 =
        trimSpec """
                **
                **
                **
                """
    }


tests : Test
tests =
    describe "All tests"
        [ testBoardConstruction ]


testBoardConstruction : Test
testBoardConstruction =
    describe "Board construction"
        [ test "Making an empty single cell board" <|
            \_ ->
                Board.makeEmpty 1 1
                    |> Expect.equal (fromSpec ".")
        , test "Making a empty 2x3 board" <|
            \_ -> Expect.equal (Board.makeEmpty 2 3) (fromSpec specs.empty2By3)
        , let
            filledBoard =
                fromSpec specs.filled2By3
          in
            testBoard filledBoard
                [ ( 0, 0 )
                , ( 1, 0 )
                , ( 0, 1 )
                , ( 1, 1 )
                , ( 0, 2 )
                , ( 1, 2 )
                ]
                []
        ]


testBoard : Board.Board -> List ( Int, Int ) -> List ( Int, Int ) -> Test
testBoard board living dead =
    let
        fetchAt x y =
            Board.get x y board

        expectAliveAt x y isAlive =
            \_ -> Expect.equal (fetchAt x y) isAlive

        testAliveAt isAlive ( x, y ) =
            test
                ("Expected at x:"
                    ++ toString x
                    ++ ",y:"
                    ++ toString y
                    ++ " to be "
                    ++ toString isAlive
                )
                (expectAliveAt x y isAlive)
    in
        describe "Test all filled" <|
            (List.map
                (testAliveAt True)
                living
            )
                ++ (List.map (testAliveAt False) dead)



-- TODO
-- , test "Off board returns dead" <|
--     expectAliveAt 9000 9000 False


testBoardSeralization : Test
testBoardSeralization =
    describe "Board serialization"
        [ test "toSpec from empty" <|
            \_ -> Expect.equal specs.empty2By3 (toSpec (Board.makeEmpty 2 3))
        , test "toSpec from filled" <|
            \_ -> Expect.equal (toSpec <| fromSpec specs.filled2By3) specs.filled2By3
        , test "flatten" <|
            \_ ->
                Expect.equal (Board.flatten <| Board.makeEmpty 2 3)
                    [ ( 0, 0, False )
                    , ( 1, 0, False )
                    , ( 0, 1, False )
                    , ( 1, 1, False )
                    , ( 0, 2, False )
                    , ( 1, 2, False )
                    ]
        ]


testMutations : Test
testMutations =
    describe "Killing & vivifying cells" <|
        [ testKill
        , testVivify
        ]


testKill : Test
testKill =
    let
        deadAt11 =
            Board.kill 1 1 (fromSpec specs.filled2By3)
    in
        testBoard
            deadAt11
            [ ( 0, 0 )
            , ( 1, 0 )
            , ( 0, 1 )
            , ( 0, 2 )
            , ( 1, 1 )
            ]
            [ ( 1, 1 ) ]


testVivify : Test
testVivify =
    let
        aliveAt11 =
            Board.vivify 1 1 (fromSpec specs.empty2By3)
    in
        testBoard
            aliveAt11
            [ ( 1, 1 ) ]
            [ ( 0, 0 )
            , ( 1, 0 )
            , ( 0, 1 )
            , ( 0, 2 )
            , ( 1, 1 )
            ]


testGeneration : Test
testGeneration =
    describe "Generating new boards" <|
        [ test "Simple row of three" <|
            \_ -> Expect.equal (Board.nextGen <| fromSpec "***") (fromSpec ".*.")
        , test "An L in a square" <|
            \_ -> Expect.equal (toSpec <| (Board.nextGen <| fromSpec ".*\n**")) (toSpec <| (fromSpec "**\n**"))
        , test "Simple column of two" <|
            \_ -> Expect.equal (toSpec <| (Board.nextGen <| fromSpec "*\n.")) (toSpec <| (fromSpec ".\n."))
        , testGenGlider
        ]


testGenGlider : Test
testGenGlider =
    let
        spec =
            fromSpec << trimSpec

        glider =
            spec """
       .*...
       ..*..
       ***..
       ....."""

        gliderNext =
            spec """
       .....
       .**..
       .**..
       .*..."""
    in
        test "Generate a glider" <|
            \_ -> Expect.equal (toSpec <| Board.nextGen glider) (toSpec gliderNext)
