-- this is the module for the level type and its associated functions


module Level exposing (Level)


type alias Level =
    { id : LevelId
    , startNumber : Float
    , goalNumber : Float
    , minMovesToPass : Int
    -- level operators are the things that you can do to your number in the 
    -- level, like incrementing or decrementing it.
    , availableOperators : List (Float -> Float)
    }



-- we make the ID a custom type instead of just storing it as an integer
-- so it's less likely that we pass an identifier of a different data structure
-- that also happens to be an integer type.


type LevelId
    = LevelId Int



