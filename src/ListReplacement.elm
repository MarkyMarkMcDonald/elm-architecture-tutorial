module ListReplacement exposing (fromIf)


fromIf : (a -> Bool) -> { items : List a, source : List a } -> { items : List a, source : List a }
fromIf shouldReplace { items, source } =
    let
        result =
            fromIfHelper shouldReplace { items = items, source = source, output = [] }
    in
        { items = result.output, source = result.source }


fromIfHelper : (a -> Bool) -> { items : List a, source : List a, output : List a } -> { items : List a, source : List a, output : List a }
fromIfHelper shouldReplace { items, source, output } =
    let
        replaceFrom =
            fromIfHelper shouldReplace
    in
        case items of
            [] ->
                { items = items, source = source, output = output }

            inputItem :: rest ->
                if shouldReplace inputItem then
                    case source of
                        [] ->
                            replaceFrom { items = rest, source = source, output = output }

                        sourceItem :: sourceTail ->
                            replaceFrom { items = rest, source = sourceTail, output = output ++ [ sourceItem ] }
                else
                    replaceFrom { items = rest, source = source, output = output ++ [ inputItem ] }
