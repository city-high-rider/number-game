-- this is the module for the level type and its associated functions


module Level exposing (Level, LevelId, isLevelOperationsNotEmpty, levelDecoder, levelIdParser, levelIdToString, levelsDecoder)

import Inscribed exposing (Inscribed(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Operation exposing (Operation(..))
import Url.Parser exposing (Parser, custom, int, map)


type alias Level =
    { id : LevelId
    , difficulty : String
    , startNumber : Float
    , goalNumber : Float
    , minMovesToPass : Int

    -- level operations are the things that you can do to your number in the
    -- level, like incrementing or decrementing it.
    , operations : List (Inscribed Operation)
    }



-- this custom type is nessecary because our level might have functions that deal with
-- both ints or floats, like incrementing, or only with ints, like a
-- factorial function. We also might want to add some function later with
-- two inputs or something.
-- todo : look into using the "number" type (https://package.elm-lang.org/packages/elm/core/latest/Basics#+)


isLevelOperationsNotEmpty : Level -> Bool
isLevelOperationsNotEmpty level =
    not <|
        List.isEmpty level.operations



-- we make the ID a custom type instead of just storing it as an integer
-- so it's less likely that we pass an identifier of a different data structure
-- that also happens to be an integer type.


type LevelId
    = LevelId Int



-- level Id to string function, used for redirecting the player to a url


levelIdToString : LevelId -> String
levelIdToString (LevelId id) =
    String.fromInt id


levelIdParser : Parser (LevelId -> a) a
levelIdParser =
    custom "LEVELID" <|
        \levelId ->
            Maybe.map LevelId (String.toInt levelId)



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


operationsDecoder : Decoder (List (Inscribed Operation))
operationsDecoder =
    -- Our json for the levels will have a list called operators, that
    -- contains a bunch of strings which correspond to operations we can
    -- perform on our number. Unfortunately, we cannot guarantee that
    -- the strings in that json list will match to an operation.
    -- (see string To Operation) Therefore we will have a list of
    -- "Maybe Inscribed Operation", and we must filter out the Nothings.
    -- that is (in theory) what this decoder should do.
    -- the operations are wrapped in InscribedData so they have name
    -- that is useful for display purposes later, namely the buttons
    -- in the play page
    Decode.map (List.filterMap stringToOperation) (Decode.list Decode.string)


stringToOperation : String -> Maybe (Inscribed Operation)
stringToOperation inp =
    case inp of
        "increment" ->
            Just (InscribedData (FloatFunction ((+) 1)) inp)

        "decrement" ->
            Just (InscribedData (FloatFunction ((+) -1)) inp)

        "double" ->
            Just (InscribedData (FloatFunction ((*) 2)) inp)

        "factorial" ->
            Just (InscribedData (IntFunction factorial) inp)

        _ ->
            Nothing


factorial : Int -> Int
factorial n =
    if (n == 0) then
        1
    else
        n * factorial (n - 1)
