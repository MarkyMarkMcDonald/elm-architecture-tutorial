import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String exposing (repeat)

main =
  Html.beginnerProgram
    { model = init
    , view = view
    , update = update
    }

type alias Model =
  { cards : List Card
  }

type alias Card =
    {
        shape: Shape,
        number: Int,
        color: Color,
        selected: Bool
    }

type Color = Red | Blue | Purple

type Shape = Diamond | Oval | Rectangle

asciiArt : Shape -> String
asciiArt shape =
    case shape of
        Diamond -> "♢"
        Oval -> "◎"
        Rectangle -> "▭"

-- MODEL

init : Model
init =
  Model [ {shape = Diamond, number = 3, color = Red, selected = False}
    , {shape = Oval, number = 2, color = Blue, selected = True}
    , {shape = Rectangle, number = 1, color = Purple}
    , {shape = Oval, number = 3, color = Red}
    , {shape = Oval, number = 2, color = Blue}
    , {shape = Rectangle, number = 3, color = Red}
    , {shape = Rectangle, number = 3, color = Purple}
    , {shape = Rectangle, number = 1, color = Red}
    , {shape = Diamond, number = 3, color = Red}
    , {shape = Rectangle, number = 3, color = Red}
    , {shape = Oval, number = 2, color = Blue}
    , {shape = Diamond, number = 3, color = Purple}
  ]

-- UPDATE

type CardMsg = SelectCard
type Msg
    = SelectCard Int CardMsg

update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectCard id xxx ->
            List.map (selectHelper id) model

-- VIEW

view : Model -> Html msg
view model =
    let cards = List.map viewIndexedCounter model.cards in
      div [] cards

renderCard : Card -> Html msg
renderCard card =
    div [
        onClick SelectCard,
        style
                [   ("color", toString card.color)
                  , ("width", "150px")
                  , ("height", "100px")
                  , ("line-height", "100px")
                  , ("font-size", "50px")
                  , ("border", "1px solid black")
                  , ("display", "inline-block")
                ]
    ] [ text (repeat card.number (asciiArt card.shape) ) ]

viewIndexedCounter : Int -> Card -> Html Msg
viewIndexedCounter {id, model} =
  Html.map (SelectCard id) (renderCard model)

selectHelper : Int -> Card -> Card
selectHelper card =
    card