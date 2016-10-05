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
import Json.Encode as Json
import GameModel exposing (Model, SelectableCard)
import CardsDecoder
import WebSocket


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


server =
    Just "localhost:3000"



-- MODEL


init : ( Model, Cmd Msg )
init =
    ( { cards = []
      , deck = []
      }
    , Task.perform
        (\error -> LoadedFromServer { cards = [], deck = [] })
        (\result -> LoadedFromServer result)
        (case server of
            Just location ->
                initServerGame location

            Nothing ->
                initLocalGame
        )
    )


initServerGame : String -> Task Http.Error Model
initServerGame serverUrl =
    Task.map
        (\cardsRecord ->
            { deck = []
            , cards = List.map Selectable.unselected cardsRecord.cards
            }
        )
        (Http.get CardsDecoder.decoder ("http://" ++ serverUrl ++ "/games/1"))


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
        Task.succeed { cards = cards, deck = deck }



-- SUBSCRIPTIONS


decodeWebSocketResponse : String -> Msg
decodeWebSocketResponse response =
    let
        result =
            Json.Decode.decodeString CardsDecoder.decoder response
    in
        case result of
            Ok state ->
                LoadedFromServer
                    { deck = []
                    , cards = List.map Selectable.unselected state.cards
                    }

            Err _ ->
                LoadedFromServer { cards = [], deck = [] }


subscriptions : Model -> Sub Msg
subscriptions model =
    case server of
        Just serverUrl ->
            WebSocket.listen ("ws://" ++ serverUrl ++ "/games/1/board_updates") decodeWebSocketResponse

        Nothing ->
            Sub.none



-- UPDATE

type alias Move = List Card

type Msg
    = ToggleSelect Int
    | SetChosen
    | LoadedFromServer Model
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ToggleSelect index ->
            update SetChosen (toggleAt index model)

        SetChosen ->
            if validSetSelected model.cards then
                case server of
                    Just serverUrl ->
                        ( model, sendMoveToServer serverUrl (currentMove model))

                    Nothing ->
                        ( replaceSet model, Cmd.none )
            else
                ( model, Cmd.none )

        LoadedFromServer state ->
            ( state, Cmd.none )

        Noop ->
            (model, Cmd.none)

currentMove : Model -> Move
currentMove ({cards} as model) =
    cards |> List.filter .selected |> List.map .item

sendMoveToServer : String -> Move -> Cmd Msg
sendMoveToServer serverUrl (move) =
    let
    body = Http.string <| Json.encode 2 (Json.list <| List.map Card.cardEncoder move)
    movesUrl = "http://" ++ serverUrl ++ "/games/1/moves"
    in
    Task.perform
        (always Noop)
        (always Noop)
        (Http.post Json.Decode.string movesUrl body)


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
