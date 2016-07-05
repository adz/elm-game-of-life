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
        String.join "\n" << onlyLinesWithContent << trimLines << String.lines


fromSpec : String -> Board
fromSpec =
    let
        rowStringToRow =
            String.toList >> List.map charToCell >> Array.fromList

        charToCell =
            (==) '*'
    in
        Array.fromList << List.map rowStringToRow << String.lines


toSpec : Board -> String
toSpec board =
    let
        rowArrayToString =
            Array.toList >> List.map cellToChar >> String.join ""

        cellToChar cell =
            if cell then
                "*"
            else
                "."
    in
        String.join "\n" <| List.map rowArrayToString <| Array.toList board
