module ListReplacement exposing (fromIf)

fromIf : (a -> Bool) -> { input: List a, source: List a, output: List a} -> { input: List a, source: List a, output: List a}
fromIf shouldReplace { input, source, output } =
    let replaceFrom = fromIf shouldReplace in
    case input of
      [] -> { input = input, source = source, output = output }
      inputItem :: rest ->
          if shouldReplace inputItem then
              case source of
                  [] -> replaceFrom { input = rest, source = source, output = output }
                  sourceItem :: sourceTail -> replaceFrom { input = rest, source = sourceTail, output = output ++ [sourceItem] }
         else
              replaceFrom { input = rest, source = source, output = output ++ [inputItem] }