import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String exposing (repeat)
import Card exposing (Color (..), Shape (..), Number (..))
import Selectable exposing (..)
import Sets exposing (isValid)
import Debug exposing (..)

main =
  Html.beginnerProgram
    { model = init
    , view = view
    , update = update
    }

type alias Model =
  { cards : List (SelectableCard)
  , validSetSelected : Bool
  }

type alias SelectableCard = Selectable Card.Model

-- MODEL

init : Model
init = { cards = List.map unselected cards
       , validSetSelected = False
       }


cards : List Card.Model
cards = [ { shape = Diamond, number = Three, color = Red }
        , { shape = Oval, number = Two, color = Green }
        , { shape = Diamond, number = One, color = Red }
        , { shape = Squiggle, number = Two, color = Blue }
        , { shape = Diamond, number = One, color = Green }
        , { shape = Squiggle, number = Three, color = Blue }
        , { shape = Squiggle, number = One, color = Green }
        , { shape = Oval, number = Two, color = Blue }
        , { shape = Oval, number = Two, color = Blue }
        ]

-- UPDATE

type Msg
    = ToggleSelect Int

update : Msg -> Model -> Model
update message model =
    case message of
        ToggleSelect id ->
            updateSelectionsANDSetStatus id model

updateSelectionsANDSetStatus : Int -> Model -> Model
updateSelectionsANDSetStatus index model =
    let updatedCards = applyAtIndex index toggle model.cards in
        { model | cards = updatedCards
        , validSetSelected = isAValidSet <| (Selectable.selected updatedCards)
        }

isAValidSet : List Card.Model -> Bool
isAValidSet cards =
    List.length cards == 3 && Sets.isValid cards

applyAtIndex : Int -> (a -> a) -> List a -> List a
applyAtIndex indexToSendTo action elements =
    let
        applyIfMatching index element =
            if index == indexToSendTo then action element
            else element
    in
        List.indexedMap applyIfMatching elements

-- VIEW

view : Model -> Html Msg
view model =
    let cards = List.indexedMap viewIndexedCard model.cards in
      div [] (cards ++ [div [] [text ("Set status" ++ toString(model.validSetSelected))]])

viewIndexedCard : Int -> SelectableCard -> Html Msg
viewIndexedCard id selectable =
  span [
        onClick (ToggleSelect id),
        style [ ("border", selectableBorder selectable)
              , ("display", "inline-block")
              ]
      ] [
        Card.view selectable.item
      ]

selectableBorder : Selectable a -> String
selectableBorder selectable =
    if selectable.selected then "1px solid red"
    else "1px solid black"
