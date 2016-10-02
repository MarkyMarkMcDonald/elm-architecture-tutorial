module SetsTests exposing (..)

import Test exposing (..)
import Expect
import String
import Card exposing (..)
import Sets exposing (attributesSameOrUnique)


all : Test
all =
    describe "Sets"
        [ describe "Valid"
            [ test "All of the same" <|
                \() ->
                    Expect.true "Sets can have attributes with all the same properties"
                        (Sets.attributesSameOrUnique
                            [ { number = One, shape = Diamond, color = Red }
                            , { number = One, shape = Diamond, color = Red }
                            , { number = One, shape = Diamond, color = Red }
                            ]
                        )
            , test "All of unique" <|
                \() ->
                    Expect.true "Sets can have attributes with all unique properties"
                        (Sets.attributesSameOrUnique
                            [ { number = One, shape = Diamond, color = Red }
                            , { number = Two, shape = Oval, color = Blue }
                            , { number = Three, shape = Squiggle, color = Green }
                            ]
                        )
            , test "Varying of same and unique" <|
                \() ->
                    Expect.true "Sets can have attributes with all unique or all same properties"
                        (Sets.attributesSameOrUnique
                            [ { number = One, shape = Diamond, color = Red }
                            , { number = Two, shape = Diamond, color = Blue }
                            , { number = Three, shape = Diamond, color = Green }
                            ]
                        )
            ]
        , describe "Invalid"
            [ test "One but not all duplicates" <|
                \() ->
                    Expect.false "One but not all duplicates for color"
                        (Sets.attributesSameOrUnique
                            [ { number = One, shape = Diamond, color = Blue }
                            , { number = Two, shape = Squiggle, color = Blue }
                            , { number = Three, shape = Oval, color = Red }
                            ]
                        )
            , test "One but not all duplicates" <|
                \() ->
                    Expect.false "One but not all duplicates another attribute"
                        (Sets.attributesSameOrUnique
                            [ { number = One, shape = Diamond, color = Blue }
                            , { number = Two, shape = Diamond, color = Blue }
                            , { number = Three, shape = Oval, color = Blue }
                            ]
                        )
            ]
        ]
