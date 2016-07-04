module Main exposing (..)

import ElmTest exposing (..)
import BoardTests


tests : Test
tests =
    BoardTests.tests


main : Program Never
main =
    runSuite tests
