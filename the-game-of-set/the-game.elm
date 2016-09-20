import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String exposing (repeat)
import Card exposing (Color (..), Shape (..), Number (..))
import Selectable exposing (..)

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
        , { shape = Oval, number = Two, color = Blue }
        , { shape = Diamond, number = One, color = Red }
        , { shape = Squiggle, number = Two, color = Blue }
        , { shape = Oval, number = One, color = Blue }
        , { shape = Squiggle, number = Three, color = Blue }
        , { shape = Diamond, number = One, color = Blue }
        , { shape = Squiggle, number = Two, color = Green }
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
        , validSetSelected = isAValidSet updatedCards
        }

isAValidSet : List SelectableCard -> Bool
isAValidSet cards =
    let
        filteredSelected = List.filter selected cards
        filtered = List.map .item filteredSelected
    in
        List.length filtered == 3
          && allSameOrDifferent .shape filtered
          && allSameOrDifferent .number filtered
          && allSameOrDifferent .color filtered

allSameOrDifferent : (a -> b) -> List a -> Bool
allSameOrDifferent func list =
    let
        list' = List.map func list
    in
        (allSame list') || (allDifferent list')

allSame : List a -> Bool
allSame list =
    case list of
        first :: tail -> List.all (\other -> other == first) tail
        [] -> True

allDifferent : List a -> Bool
allDifferent list =
    case list of
        first :: tail -> (List.all (\other -> other /= first) tail) && (allDifferent tail)
        [] -> True

selected : Selectable item -> Bool
selected selectable = selectable.selected

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

