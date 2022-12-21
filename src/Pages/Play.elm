module Pages.Play exposing (..)

import Level exposing (Level, LevelId)
import RemoteData exposing (RemoteData(..), WebData)
import Html exposing (h3, text, Html)
import Http
import Level exposing (levelIdToString, levelDecoder)
import RemoteData exposing (isSuccess)



-- model and init


type alias Model =
    { currentLevel : WebData Level
    -- it does not make sense to store the current number in the level,
    -- as the level data type is only supposed to contain information about
    -- the level itself. It makes more sense to have the current 
    -- number be part of the play page. However, it has to be of type
    -- webdata, since we only initialise it when the level is succesfully 
    -- loaded.
    , currentNumber : WebData Float
    }



-- the main function will pass us the level id which the route
-- module extracted


init : LevelId -> ( Model, Cmd Msg )
init lvlId =
    let
        initialModel : Model
        initialModel =
            { currentLevel = Loading
            , currentNumber = Loading
            }
    in
    ( initialModel, loadLevel lvlId )

-- view

view : Model -> Html Msg
view _ =
    h3 [] [text "Placeholder"]

-- update and msg

type Msg =
    ReceivedLevel (WebData Level)

loadLevel : LevelId -> Cmd Msg
loadLevel lvlId =
    Http.get
    { url = ( "http://localhost:5019/levels/" ++ levelIdToString lvlId ) 
    , expect = Http.expectJson (RemoteData.fromResult >> ReceivedLevel) levelDecoder
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReceivedLevel levelData ->
            -- Here we want to set our model's current number to the initial 
            -- number stored inside the level, only if we have succesfully 
            -- loaded it. We want to set the model's current level to 
            -- the webdata we received, regardless of whether or not the 
            -- level was loaded so that we can display an error if need be.
            -- doing this with pattern matching is awkward, so I resorted
            -- to trying the update function in remotedata library.
            let 
                (currentNumber, cmd) =
                    RemoteData.update initCurrentNumber levelData
            in
            ({model | currentNumber = currentNumber, currentLevel = levelData}, cmd)

initCurrentNumber : Level -> (Float, Cmd Msg)
initCurrentNumber levelData =
    (levelData.startNumber, Cmd.none)


