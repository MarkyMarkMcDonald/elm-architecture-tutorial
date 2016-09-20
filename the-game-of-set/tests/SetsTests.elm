module SetsTests exposing (..)

import Test exposing (..)
import Expect
import String

import Card exposing (..)
import Sets exposing(isValid)

all : Test
all =
    describe "Sets" [
        describe "Valid"
            [ test "All of the same color" <|
                \() ->
                    Expect.true "Sets can be the same color" (Sets.isValid
                                              [ { number = One, shape = Diamond, color = Red }
                                              , { number = One, shape = Diamond, color = Red }
                                              , { number = One, shape = Diamond, color = Red } ])
            , test "All of different colors" <|
                \() ->
                    Expect.true "Sets can be all different colors" (Sets.isValid
                                              [ { number = One, shape = Diamond, color = Blue }
                                              , { number = One, shape = Diamond, color = Green }
                                              , { number = One, shape = Diamond, color = Red } ])
            ]
        ,
        describe "Invalid" [
            test "Duplicate but not unique colors" <|
                \() ->
                    Expect.false "Sets must be the same color" (Sets.isValid
                                              [ { number = One, shape = Diamond, color = Blue }
                                              , { number = One, shape = Diamond, color = Blue }
                                              , { number = One, shape = Diamond, color = Red } ])]
    ]



