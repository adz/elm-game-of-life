port module Main exposing (..)

import BoardTests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit BoardTests.tests


port emit : ( String, Value ) -> Cmd msg
