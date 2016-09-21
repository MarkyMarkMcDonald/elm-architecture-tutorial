import Card exposing (Color (..), Shape (..))
import Html exposing (..)
import Html.App as Html

main =
  Html.beginnerProgram
    { model = { shape = Diamond, number = 3, color = Red, selected = False }
    , view = Card.view
    , update = Card.update
    }