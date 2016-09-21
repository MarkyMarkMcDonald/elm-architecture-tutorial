module Selectable exposing (..)

type alias Selectable a =
    { item: a
    , selected: Bool
    }

unselected : item -> Selectable item
unselected item =
    { item = item
    , selected = False
    }

toggle : Selectable item -> Selectable item
toggle selectable = { selectable | selected = not selectable.selected}

selected : List (Selectable a) -> List a
selected items = items |> List.filter .selected |> List.map .item