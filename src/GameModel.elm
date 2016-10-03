module GameModel exposing (Model, SelectableCard)

import Selectable exposing (Selectable)
import Card exposing (Card)


type alias Model =
    { cards : List (SelectableCard), deck : List (Card), server : Maybe String }


type alias SelectableCard =
    Selectable Card
