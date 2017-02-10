module BoardTests exposing (tests)

import Test exposing (Test, describe, test)
import Expect
import Board exposing (Board)
import BoardSpec exposing (trimSpec, fromSpec, toSpec)


{-| A set of simple board specifications: laid out as text for readability
-}
specs : { empty2By3 : String, filled2By3 : String }
specs =
    { empty2By3 =
        trimSpec """..
                    ..
                    .."""
    , filled2By3 =
        trimSpec """**
                    **
                    **"""
    }


boards : { empty2By3 : Board, filled2By3 : Board }
boards =
    { empty2By3 = fromSpec specs.empty2By3, filled2By3 = fromSpec specs.filled2By3 }


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
            \_ -> Expect.equal (Board.makeEmpty 2 3) boards.empty2By3
        , testBoardContents boards.filled2By3
            [ ( 0, 0 )
            , ( 1, 0 )
            , ( 0, 1 )
            , ( 1, 1 )
            , ( 0, 2 )
            , ( 1, 2 )
            ]
            -- off board grid considered dead
            [ ( 9000, 90000 ) ]
        ]


testBoardContents : Board -> List ( Int, Int ) -> List ( Int, Int ) -> Test
testBoardContents board living dead =
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


testBoardSeralization : Test
testBoardSeralization =
    describe "Board serialization"
        [ test "toSpec produces expected string for empty boards" <|
            \_ -> Expect.equal specs.empty2By3 (toSpec <| Board.makeEmpty 2 3)
        , test "toSpec produces expected string for filled boards" <|
            \_ -> Expect.equal specs.filled2By3 (toSpec <| boards.filled2By3)
        , test "flatten produces a list of positions with living status" <|
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
        testBoardContents
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
            Board.vivify 1 1 boards.empty2By3
    in
        testBoardContents
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
