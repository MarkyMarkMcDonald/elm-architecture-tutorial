module CardsDecoder exposing (decoder)

import Json.Decode as Json exposing (..)
import GameModel exposing (Model)
import Card exposing (Card, Shape(..), Number(..), Color(..))


type alias CardsRecord =
    { cards : List Card }


decoder : Json.Decoder { cards : List Card }
decoder =
    object1 CardsRecord
        ("cards" := Json.list cardDecoder)


cardDecoder : Json.Decoder Card
cardDecoder =
    object3 Card
        (("shape" := string) `andThen` decodeShape)
        (("number" := string) `andThen` decodeNumber)
        (("color" := string) `andThen` decodeColor)



-- Color


decodeColor : String -> Json.Decoder Color
decodeColor color =
    succeed (colorMapping color)


colorMapping : String -> Color
colorMapping color =
    case color of
        "Red" ->
            Red

        "Blue" ->
            Blue

        _ ->
            Green



-- Shape


decodeShape : String -> Json.Decoder Shape
decodeShape shape =
    succeed (shapeMapping shape)


shapeMapping : String -> Shape
shapeMapping shape =
    case shape of
        "Squiggle" ->
            Squiggle

        "Diamond" ->
            Diamond

        _ ->
            Oval



-- Number


decodeNumber : String -> Json.Decoder Number
decodeNumber number =
    succeed (numberMapping number)


numberMapping : String -> Number
numberMapping number =
    case number of
        "One" ->
            One

        "Two" ->
            Two

        _ ->
            Three
