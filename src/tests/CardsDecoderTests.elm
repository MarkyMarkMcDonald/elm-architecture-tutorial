module CardsDecoderTests exposing (..)

import Test exposing (..)
import Expect
import String
import Card exposing (..)
import Sets exposing (attributesSameOrUnique)
import CardsDecoder
import Json.Decode as JsonDecoder
import Result


all : Test
all =
    describe "Mapping Cards"
        [ test "" <|
            \() ->
                let
                    decoder =
                        CardsDecoder.decoder

                    cardsAsString =
                        """
                        {"cards":[{"color":"Red","shape":"Squiggle","number":"One"}]}
                        """

                    actualResult =
                        Result.withDefault ({ cards = [] }) (JsonDecoder.decodeString decoder cardsAsString)

                    expectedResult =
                        { cards = [ { color = Red, shape = Squiggle, number = One } ]
                        }
                in
                    Expect.true ""
                        ((==)
                            actualResult
                            expectedResult
                        )
        ]
