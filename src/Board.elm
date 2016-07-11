module Board exposing (..)

import Array
import String
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


asList : Board -> List (List Bool)
asList =
    List.map Array.toList << Array.toList


flatten : Board -> List ( Int, Int, Bool )
flatten board =
    let
        rowToTuple rowIndex row =
            List.indexedMap (\colIndex cell -> ( colIndex, rowIndex, cell )) row

        boardTuplified =
            List.indexedMap rowToTuple (asList board)
    in
        List.concatMap identity boardTuplified


neighbours : Int -> Int -> Board
neighbours col row board =
    [ get (col - 1) row board
    , get (col - 1) (row - 1) board
    , get col (row - 1) board
    , get (col + 1) row board
    , get (col + 1) (row + 1) board
    , get col (row + 1) board
    ]


nextGen : Board -> Board
nextGen board =
    let
        shouldKill colIndex rowIndex =
            case neighbours colIndex rowIndex board of
                2 ->
                    True

                3 ->
                    True

                _ ->
                    False

        reproduce colIndex rowIndex =
            if neighbours colIndex rowIndex board == 6 then
                True
            else
                False

        mapRow rowIndex row =
            Array.indexedMap
                (\colIndex cell ->
                    if cell && shouldKill then
                        Board.kill
                    else
                        boar
                )
                row
    in
        Array.indexedMap mapRow board


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
            getRow board `Maybe.andThen` getCol
    in
        Maybe.withDefault False getCell


put : Int -> Int -> Board -> Bool -> Board
put colNum rowNum board status =
    let
        newRow row =
            Array.set colNum status row

        makeNewRow =
            Maybe.map newRow (Array.get rowNum board)
    in
        Array.set rowNum (Maybe.withDefault (Array.fromList []) makeNewRow) board


kill : Int -> Int -> Board -> Board
kill colNum rowNum board =
    put colNum rowNum board False


vivify : Int -> Int -> Board -> Board
vivify colNum rowNum board =
    put colNum rowNum board True


makeEmpty : Int -> Int -> Board
makeEmpty cols rows =
    Array.repeat rows (Array.repeat cols False)
