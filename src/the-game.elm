module Main exposing (..)

import Html exposing (Html, div, span)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Card exposing (Card)
import Selectable exposing (Selectable)
import Sets exposing (attributesSameOrUnique)
import ListReplacement exposing (fromIf)
import Task exposing (Task)
import Shuffling
import Random


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


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
        (initLocalGame)
    )


initLocalGame : Task Never Model
initLocalGame =
    let
        shuffledCards =
            [0..26] |> Shuffling.shuffle (Random.initialSeed 31415) |> List.map Card.init

        deck =
            shuffledCards |> List.drop 12

        cards =
            shuffledCards |> List.take 12 |> List.map Selectable.unselected
    in
        Task.succeed { cards = cards, deck = deck }



-- UPDATE


type Msg
    = ToggleSelect Int
    | LoadedFromServer Model


update : Msg -> Model -> ( Model, Cmd a )
update message model =
    case message of
        ToggleSelect id ->
            let
                state =
                    toggleAt id model
            in
                if isAValidSet state.cards then
                    ( replaceSet id state, Cmd.none )
                else
                    ( state, Cmd.none )

        LoadedFromServer state ->
            ( state, Cmd.none )


toggleAt : Int -> Model -> Model
toggleAt index model =
    { model | cards = applyAtIndex index Selectable.toggle model.cards }


replaceSet : Int -> Model -> Model
replaceSet index { cards, deck } =
    let
        { items, source } =
            ListReplacement.fromIf .selected { items = cards, source = List.map Selectable.unselected deck }
    in
        { cards = items, deck = List.map .item source }


isAValidSet : List SelectableCard -> Bool
isAValidSet cards =
    let
        selectedCards =
            cards |> List.filter .selected
    in
        (List.length selectedCards == 3)
            && Sets.attributesSameOrUnique (List.map .item selectedCards)


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
