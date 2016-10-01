module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String exposing (repeat)
import Card exposing (Card, init)
import Selectable exposing (..)
import Sets exposing (isValid)
import Debug exposing (..)
import ListReplacement exposing (fromIf)
import LocalStorage exposing (..)
import Json.Encode
import Task
import Shuffling exposing (shuffle)
import Random


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


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
        (\error -> LoadedFromServer { cards = [], deck = [] })
        (\result -> LoadedFromServer result)
        (
        let
        shuffledCards = [0..26] |> Shuffling.shuffle (Random.initialSeed 31415) |> List.map Card.init
        deck = shuffledCards |> List.drop 12
        cards = shuffledCards |> List.take 12 |> List.map unselected
        in
        Task.succeed { cards = cards, deck = deck }
        )
    )


-- UPDATE


type Msg
    = ToggleSelect Int
    | LoadedFromServer Model


update : Msg -> Model -> ( Model, Cmd a )
update message model =
    case message of
        ToggleSelect id ->
            ( updateSelectionsANDSetStatus id model, Cmd.none )

        LoadedFromServer state ->
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
    div [] (List.indexedMap viewIndexedCard model.cards)


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
