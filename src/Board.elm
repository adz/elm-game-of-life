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
    List.map Array.toList << Array.toList


fromList : List (List Bool) -> Board
fromList =
    (List.map Array.fromList) >> Array.fromList


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


pairPermutations : List a -> List ( a, a )
pairPermutations xs =
    let
        pairPermutations2 xs ys =
            case ( xs, ys ) of
                ( head :: xs_left, ys ) ->
                    (List.map ((,) head) ys) ++ pairPermutations2 xs_left ys

                ( xs, ys ) ->
                    []
    in
        pairPermutations2 xs xs


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
                |> List.filter ((/=) ( 0, 0 ))
                |> List.map offsetAt

        neighbourCells =
            List.map (\position -> get position board) neighbourPositions

        valueOf cell =
            if cell then
                1
            else
                0
    in
        List.map valueOf neighbourCells |> List.sum


{-| Produce a next generation board
-}
nextGen : Board -> Board
nextGen board =
    let
        shouldKill pos =
            not <| List.member (neighbours pos board) [ 2, 3 ]

        shouldReproduce pos =
            (neighbours pos board) == 3

        visitCell cell pos newBoard =
            if cell && shouldKill pos then
                kill pos newBoard
            else if not cell && shouldReproduce pos then
                vivify pos newBoard
            else
                newBoard

        foldRow rowIndex row newBoard =
            Tuple.second <|
                Array.foldl
                    (\cell ( colIndex, newBoard2 ) -> ( colIndex + 1, visitCell cell ( colIndex, rowIndex ) newBoard2 ))
                    ( 0, newBoard )
                    row
    in
        Tuple.second <|
            Array.foldl
                (\row ( rowIndex, newBoard ) -> ( rowIndex + 1, foldRow rowIndex row newBoard ))
                ( 0, board )
                board


{-| Get a cell at position colNum / rowNum
-}
get : Pos -> Board -> Bool
get ( colNum, rowNum ) board =
    Array.get rowNum board
        |> Maybe.andThen (Array.get colNum)
        |> Maybe.withDefault False


{-| Put a cell at position colNum / rowNum
-}
put : Pos -> Board -> Bool -> Board
put pos board status =
    let
        ( colNum, rowNum ) =
            pos

        newRow row =
            Array.set colNum status row

        makeNewRow =
            Maybe.map newRow (Array.get rowNum board)

        maybeMakeNewRow =
            (Maybe.withDefault (Array.fromList []) makeNewRow)
    in
        Array.set rowNum maybeMakeNewRow board


{-| Kill cell at position col, row
-}
kill : Pos -> Board -> Board
kill pos board =
    put pos board False


{-| Vivify (bring to life) cell at position col, row
-}
vivify : Pos -> Board -> Board
vivify pos board =
    put pos board True


{-| Make a new board of size cols/rows
-}
makeEmpty : Int -> Int -> Board
makeEmpty cols rows =
    Array.repeat rows (Array.repeat cols False)
