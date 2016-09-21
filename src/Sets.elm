module Sets exposing (isValid)

import Card exposing (Model, Color (..), attributes)

isValid : List Card.Model -> Bool
isValid cards =
     List.all isTrue (attributeChecks cards)

attributeChecks : List (Card.Model) -> List (Bool)
attributeChecks cards =
    List.map (allSameOrDifferent cards) Card.attributes

isTrue = ((==) True)

allSameOrDifferent : List (Card.Model) -> (Card.Model -> a) -> Bool
allSameOrDifferent cards attribute =
    cards |> List.map attribute |> any [allSame, allUnique]

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