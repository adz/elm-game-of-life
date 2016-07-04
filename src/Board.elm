module Board exposing (..)

import Array
import String
import List


{-| A board is an array of arrays containing booleans. If a cell is alive,
then the boolean is True
-}
type alias Board =
    Array.Array (Array.Array Bool)


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
