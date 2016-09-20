import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)

main =
  App.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

-- MODEL
type alias Model = Int

model : Model
model = 0

-- UPDATE
type Msg
  = Increment
  | Decrement

update : Msg -> Model -> Model
update msg counter =
  case msg of
    Increment ->
      counter + 1

    Decrement ->
      counter - 1

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]
