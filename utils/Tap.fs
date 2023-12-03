module Utils.Tap

let tap sideEffect x =
    sideEffect x
    x

