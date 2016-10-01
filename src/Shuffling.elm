module Shuffling exposing (shuffle)

import List
import Random


shuffle : Random.Seed -> List Int -> List Int
shuffle seed list =
    let
        ( n, seed1 ) =
            Random.step (Random.int 0 ((List.length list) - 1)) seed

        nth =
            List.head (List.drop n list)

        before =
            List.take n list

        after =
            List.drop (n + 1) list
    in
        case nth of
            Just val ->
                [ val ] ++ (shuffle seed1 (before ++ after))

            Nothing ->
                []
