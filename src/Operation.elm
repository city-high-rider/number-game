module Operation exposing (..)

type Operation
    = FloatFunction (Float -> Float)
    | IntFunction (Int -> Int)
