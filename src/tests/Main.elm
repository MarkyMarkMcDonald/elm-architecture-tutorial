port module Main exposing (..)

import SetsTests
import CardsDecoderTests
import Test.Runner.Node exposing (run)
import Test exposing (..)
import Json.Encode exposing (Value)


main : Program Value
main =
    run emit (Test.concat [ SetsTests.all, CardsDecoderTests.all ])


port emit : ( String, Value ) -> Cmd msg
