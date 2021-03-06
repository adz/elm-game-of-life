module Board exposing (..)

import Array
import List


-- Game of life rules:
-- --------------------------------
-- if alive:
--   0,1 => die (under populated)
--   2,3 => live
--   _   => die (overpopulated)
--
-- if dead:
--   3 => come back to live
--   _ => stay dead


{-| A board is an array of arrays containing booleans.
  | If a cell is alive, then the boolean is True
-}
type alias Board =
    Array.Array (Array.Array Bool)


type alias Pos =
    ( Int, Int )


{-| Convert array of arrays to list of lists
-}
asList : Board -> List (List Bool)
asList =
    Array.toList >> List.map Array.toList


fromList : List (List Bool) -> Board
fromList =
    List.map Array.fromList >> Array.fromList


{-| Flatten board to list of tuples containing col, row and cell
-}
flatten : Board -> List ( Int, Int, Bool )
flatten board =
    let
        rowToTuple rowIndex row =
            List.indexedMap (\colIndex cell -> ( colIndex, rowIndex, cell )) row

        boardTuplified =
            List.indexedMap rowToTuple (asList board)
    in
        List.concatMap identity boardTuplified


{-| Take a list and generate all possible permutaions of pairs of members
-}
pairPermutations : List a -> List ( a, a )
pairPermutations xs =
    let
        pairUp x ys =
            List.map (\y -> ( x, y )) ys
    in
        List.map (\x -> pairUp x xs) xs
            |> List.concat


{-| Calculate number of neighbours alive
-}
neighbours : Pos -> Board -> Int
neighbours pos board =
    let
        ( col, row ) =
            pos

        offsetAt ( x, y ) =
            ( col + x, row + y )

        neighbourPositions =
            pairPermutations [ -1, 0, 1 ]
                |> List.map offsetAt
                |> List.filter ((/=) pos)

        neighbourCells =
            List.map (\position -> get position board) neighbourPositions
    in
        List.filter identity neighbourCells
            |> List.length


{-| Produce a next generation board
-}
nextGen : Board -> Board
nextGen board =
    let
        shouldLiveOn pos =
            List.member (neighbours pos board) [ 2, 3 ]

        shouldReproduce pos =
            (neighbours pos board) == 3

        visitCell isAlive pos =
            if isAlive then
                shouldLiveOn pos
            else
                shouldReproduce pos

        mapRow rowIndex row =
            Array.indexedMap (\colIndex cell -> visitCell cell ( colIndex, rowIndex )) row

        mapBoard board =
            Array.indexedMap (\rowIndex row -> mapRow rowIndex row) board
    in
        mapBoard board


{-| Get a cell at position colNum / rowNum
-}
get : Pos -> Board -> Bool
get ( colNum, rowNum ) board =
    Array.get rowNum board
        |> Maybe.andThen (Array.get colNum)
        |> Maybe.withDefault False


{-| Make a new board of size cols/rows
-}
makeEmpty : Int -> Int -> Board
makeEmpty cols rows =
    Array.repeat rows (Array.repeat cols False)
