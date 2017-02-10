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


{-| Calculate number of neighbours alive
-}
neighbours : Int -> Int -> Board -> Int
neighbours col row board =
    let
        neighbourCells =
            [ get (col - 1) row board
            , get (col - 1) (row - 1) board
            , get col (row - 1) board
            , get (col + 1) (row - 1) board
            , get (col + 1) row board
            , get (col + 1) (row + 1) board
            , get col (row + 1) board
            , get (col - 1) (row + 1) board
            ]

        valueOf cell =
            if cell then
                1
            else
                0
    in
        List.sum <| List.map valueOf neighbourCells


{-| Produce a next generation board
-}
nextGen : Board -> Board
nextGen board =
    let
        shouldKill colIndex rowIndex =
            not <| List.member (neighbours colIndex rowIndex board) [ 2, 3 ]

        shouldReproduce colIndex rowIndex =
            (neighbours colIndex rowIndex board) == 3

        visitCell cell colIndex rowIndex newBoard =
            if cell && shouldKill colIndex rowIndex then
                kill colIndex rowIndex newBoard
            else if not cell && shouldReproduce colIndex rowIndex then
                vivify colIndex rowIndex newBoard
            else
                newBoard

        foldRow rowIndex row newBoard =
            Tuple.second <|
                Array.foldl
                    (\cell ( colIndex, newBoard2 ) -> ( colIndex + 1, visitCell cell colIndex rowIndex newBoard2 ))
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
get : Int -> Int -> Board -> Bool
get colNum rowNum board =
    let
        getRow =
            Array.get rowNum

        getCol =
            Array.get colNum

        getCell =
            getRow board |> Maybe.andThen getCol
    in
        Maybe.withDefault False getCell


{-| Put a cell at position colNum / rowNum
-}
put : Int -> Int -> Board -> Bool -> Board
put colNum rowNum board status =
    let
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
kill : Int -> Int -> Board -> Board
kill colNum rowNum board =
    put colNum rowNum board False


{-| Vivify (bring to life) cell at position col, row
-}
vivify : Int -> Int -> Board -> Board
vivify colNum rowNum board =
    put colNum rowNum board True


{-| Make a new board of size cols/rows
-}
makeEmpty : Int -> Int -> Board
makeEmpty cols rows =
    Array.repeat rows (Array.repeat cols False)
