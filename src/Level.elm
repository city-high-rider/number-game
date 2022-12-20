-- this is the module for the level type and its associated functions


module Level exposing (Level, levelIdToString, levelsDecoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)


type alias Level =
    { id : LevelId
    , difficulty : String
    , startNumber : Float
    , goalNumber : Float
    , minMovesToPass : Int

    -- level operators are the things that you can do to your number in the
    -- level, like incrementing or decrementing it.
    , availableOperations : List Operation
    }



-- this custom type is nessecary because our level might have functions that deal with
-- both ints or floats, like incrementing, or only with ints, like a
-- factorial function. We also might want to add some function later with
-- two inputs or something.


type Operation
    = FloatFunction (Float -> Float)
    | IntFunction (Int -> Int)



-- we make the ID a custom type instead of just storing it as an integer
-- so it's less likely that we pass an identifier of a different data structure
-- that also happens to be an integer type.


type LevelId
    = LevelId Int



-- level Id to string function, used for redirecting the player to a url


levelIdToString : LevelId -> String
levelIdToString (LevelId id) =
    String.fromInt id



-- json decoder to convert json data to level data type


levelsDecoder : Decoder (List Level)
levelsDecoder =
    Decode.list levelDecoder


levelDecoder : Decoder Level
levelDecoder =
    Decode.succeed Level
        |> required "id" levelIdDecoder
        |> required "difficulty" Decode.string
        |> required "startNumber" Decode.float
        |> required "endNumber" Decode.float
        |> required "minMovesToPass" Decode.int
        -- here is the issue : there is no guarantee that the json for the levels will have
        -- valid functions listed, maybe there is a typo or something.
        -- so we might have to return an empty list. Later on we can reject the level
        -- if the list of available operations is empty.
        |> required "operators" operationsDecoder


levelIdDecoder : Decoder LevelId
levelIdDecoder =
    Decode.map LevelId Decode.int


operationsDecoder : Decoder (List Operation)
operationsDecoder =
    -- Our json for the levels will have a list called operators, that 
    -- contains a bunch of strings which correspond to operations we can
    -- perform on our number. Unfortunately, we cannot guarantee that 
    -- the strings in that json list will match to an operation. 
    -- (see stringToOperation.) Therefore we will have a list of
    -- "Maybe Operation", and we must filter out the Nothings.
    -- that is (in theory) what this decoder should do.
    Decode.map (List.filterMap stringToOperation) (Decode.list Decode.string)


stringToOperation : String -> Maybe Operation
stringToOperation inp =
    case inp of
        "increment" ->
            Just (FloatFunction ((+) 1))

        "decrement" ->
            Just (FloatFunction ((-) 1))

        "double" ->
            Just (FloatFunction ((*) 2))

        _ ->
            Nothing
