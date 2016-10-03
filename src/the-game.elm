module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Card exposing (Card)
import Selectable exposing (Selectable)
import Sets
import ListReplacement
import Task exposing (Task)
import Shuffling
import Random
import Http
import Json.Decode exposing (Decoder)
import GameModel exposing (Model, SelectableCard)
import CardsDecoder


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


init : ( Model, Cmd Msg )
init =
    let
        server =
            Just "http://localhost:3000"
    in
        ( { cards = []
          , deck = []
          , server = server
          }
        , Task.perform
            (\error -> LoadedFromServer { cards = [], deck = [], server = server })
            (\result -> LoadedFromServer result)
            (case server of
                Just location ->
                    initServerGame location

                Nothing ->
                    initLocalGame
            )
        )


initServerGame : String -> Task Http.Error Model
initServerGame server =
    Task.map
        (\cardsRecord ->
            { deck = []
            , cards = List.map Selectable.unselected cardsRecord.cards
            , server = Just server
            }
        )
        (Http.get CardsDecoder.decoder (server ++ "/games/1"))


initLocalGame : Task Http.Error Model
initLocalGame =
    let
        shuffledCards =
            [0..26] |> Shuffling.shuffle (Random.initialSeed 31415) |> List.map Card.init

        deck =
            shuffledCards |> List.drop 12

        cards =
            shuffledCards |> List.take 12 |> List.map Selectable.unselected
    in
        Task.succeed { cards = cards, deck = deck, server = Nothing }



-- UPDATE


type Msg
    = ToggleSelect Int
    | SetChosen
    | LoadedFromServer Model


update : Msg -> Model -> ( Model, Cmd a )
update message model =
    case message of
        ToggleSelect index ->
            update SetChosen (toggleAt index model)

        SetChosen ->
            if validSetSelected model.cards then
                ( replaceSet model, Cmd.none )
            else
                ( model, Cmd.none )

        LoadedFromServer state ->
            ( state, Cmd.none )


toggleAt : Int -> Model -> Model
toggleAt index model =
    { model | cards = applyAtIndex index Selectable.toggle model.cards }


replaceSet : Model -> Model
replaceSet ({ cards, deck } as model) =
    let
        { items, source } =
            ListReplacement.fromIf .selected { items = cards, source = List.map Selectable.unselected deck }
    in
        { model | cards = items, deck = List.map .item source }


validSetSelected : List SelectableCard -> Bool
validSetSelected cards =
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
