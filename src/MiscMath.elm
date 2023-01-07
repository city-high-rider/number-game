module MiscMath exposing (..)


factorial : Int -> Int
factorial n =
    if n == 0 then
        1

    else if n < 0 then
        0

    else
        n * factorial (n - 1)


isRound : Float -> Bool
isRound x =
    x == (x |> round |> toFloat)


square : Float -> Float
square x =
    x * x


half : Float -> Float
half x =
    x / 2
