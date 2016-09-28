module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String exposing (repeat)
import Card exposing (Card, Color(..), Shape(..), Number(..))
import Selectable exposing (..)
import Sets exposing (isValid)
import Debug exposing (..)
import ListReplacement exposing (fromIf)
import LocalStorage exposing (..)
import Json.Encode
import Task


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


sampleStateJson =
    text <|
        Json.Encode.encode 0
            (Json.Encode.object
                [ ( "cards", Json.Encode.list <| List.map cardEncoder cards )
                , ( "deck", Json.Encode.list <| List.map cardEncoder deck )
                ]
            )


cardEncoder : Card -> Json.Encode.Value
cardEncoder card =
    Json.Encode.object
        [ ( "color", Json.Encode.string <| toString card.color )
        , ( "number", Json.Encode.string <| toString card.number )
        , ( "shape", Json.Encode.string <| toString card.shape )
        ]


type alias Model =
    { cards : List (SelectableCard), deck : List (Card) }


type alias SelectableCard =
    Selectable Card



-- MODEL


init : ( Model, Cmd Msg )
init =
    ( { cards = []
      , deck = []
      }
    , Task.perform
        (\error -> LoadedFromStorage { cards = [], deck = [] })
        (\result -> LoadedFromStorage result)
        (Task.succeed { cards = List.map unselected cards, deck = deck })
    )


deck : List Card
deck =
    [ { shape = Oval, number = One, color = Red }
    , { shape = Oval, number = One, color = Green }
    , { shape = Oval, number = One, color = Red }
    , { shape = Squiggle, number = Two, color = Blue }
    , { shape = Diamond, number = One, color = Green }
    , { shape = Squiggle, number = Three, color = Blue }
    , { shape = Squiggle, number = One, color = Green }
    , { shape = Oval, number = Two, color = Blue }
    , { shape = Oval, number = Two, color = Red }
    ]


cards : List Card
cards =
    [ { shape = Diamond, number = Three, color = Red }
    , { shape = Oval, number = Two, color = Green }
    , { shape = Diamond, number = One, color = Red }
    , { shape = Squiggle, number = Two, color = Blue }
    , { shape = Diamond, number = One, color = Green }
    , { shape = Squiggle, number = Three, color = Blue }
    , { shape = Squiggle, number = One, color = Green }
    , { shape = Oval, number = Two, color = Blue }
    , { shape = Oval, number = Two, color = Red }
    ]



-- UPDATE


type Msg
    = ToggleSelect Int
    | LoadedFromStorage Model


update : Msg -> Model -> ( Model, Cmd a )
update message model =
    case message of
        ToggleSelect id ->
            ( updateSelectionsANDSetStatus id model, Cmd.none )

        LoadedFromStorage state ->
            ( state, Cmd.none )


updateSelectionsANDSetStatus : Int -> Model -> Model
updateSelectionsANDSetStatus index model =
    let
        updatedModel =
            { model | cards = applyAtIndex index toggle model.cards }
    in
        let
            { cards, deck } =
                updatedModel
        in
            if isAValidSet <| Selectable.selected cards then
                let
                    { items, source } =
                        ListReplacement.fromIf .selected { items = cards, source = List.map unselected deck }
                in
                    { cards = items, deck = List.map .item source }
            else
                updatedModel


isAValidSet : List Card -> Bool
isAValidSet cards =
    List.length cards == 3 && Sets.isValid cards


applyAtIndex : Int -> (a -> a) -> List a -> List a
applyAtIndex indexToSendTo action elements =
    let
        applyIfMatching index element =
            if index == indexToSendTo then
                action element
            else
                element
    in
        List.indexedMap applyIfMatching elements



-- VIEW


view : Model -> Html Msg
view model =
    let
        cards =
            List.indexedMap viewIndexedCard model.cards
    in
        div [] cards


viewIndexedCard : Int -> SelectableCard -> Html Msg
viewIndexedCard id selectable =
    span
        [ onClick (ToggleSelect id)
        , style
            [ ( "border", selectableBorder selectable )
            , ( "display", "inline-block" )
            ]
        ]
        [ Card.view selectable.item
        ]


selectableBorder : Selectable a -> String
selectableBorder selectable =
    if selectable.selected then
        "1px solid red"
    else
        "1px solid black"
