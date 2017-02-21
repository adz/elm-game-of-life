module BoardSpec exposing (..)

import Array
import String
import List
import Board exposing (Board)


trimSpec : String -> String
trimSpec =
    let
        onlyLinesWithContent =
            List.filter (not << String.isEmpty)

        trimLines =
            List.map String.trim
    in
        String.lines >> trimLines >> onlyLinesWithContent >> String.join "\n"


fromSpec : String -> Board
fromSpec =
    let
        rowStringToRow =
            String.toList >> List.map charToCell >> Array.fromList

        charToCell =
            (==) '*'
    in
        String.lines >> List.map rowStringToRow >> Array.fromList


toSpec : Board -> String
toSpec =
    let
        rowArrayToString =
            Array.toList >> List.map cellToChar >> String.join ""

        cellToChar cell =
            if cell then
                "*"
            else
                "."
    in
        Array.toList >> List.map rowArrayToString >> String.join "\n"
