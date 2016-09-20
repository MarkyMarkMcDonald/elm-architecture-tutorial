module Sets exposing (isValid)

import Card exposing (Model, Color (..) )

isValid : List Card.Model -> Bool
isValid cards =
     cards |> List.map .color |> any [allSame, allUnique]

any : List (List a -> Bool) -> List a -> Bool
any listPredicates list =
    List.any (\listPredicate -> listPredicate list) listPredicates

allUnique : List a -> Bool
allUnique items =
    case items of
        front :: tail -> firstItemIsUnique items && allUnique tail
        [] -> True

firstItemIsUnique items =
    case items of
        front :: tail -> List.all (\item -> item /= front) tail
        [] -> True

allSame : List a -> Bool
allSame items = case items of
    front :: tail -> List.all ((==) front) tail && allSame tail
    [] -> True
