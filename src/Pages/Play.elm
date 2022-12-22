-- todo : handle rounding explicitly, i.e let the player have a button for
-- it, add win condition


module Pages.Play exposing (..)

import ErrorViewing exposing (viewError)
import Html exposing (Html, button, div, h2, h3, p, strong, text)
import Html.Events exposing (onClick)
import Http
import Inscribed exposing (BindableFunction(..), Inscribed, bind, makeBindable)
import Level exposing (Level, LevelId, levelDecoder, levelIdToString)
import Operation exposing (Operation)
import RemoteData exposing (RemoteData(..), WebData, isSuccess)



-- model and init


type alias Model =
    { currentLevel : WebData Level

    -- it does not make sense to store the current number in the level,
    -- as the level data type is only supposed to contain information about
    -- the level itself. It makes more sense to have the current
    -- number be part of the play page. However, it has to be of type
    -- webdata, since we only initialise it when the level is succesfully
    -- loaded. it is also inscribed so that we can log user action
    , currentNumber : WebData (Inscribed Float)
    , moves : Int
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
            , moves = 0
            }
    in
    ( initialModel, loadLevel lvlId )



-- view


view : Model -> Html Msg
view model =
    -- theoretically the currentNumber should always be successfully
    -- loaded if the level is because we used remotedata update function
    -- to initialise it, but we still have to write case expressions for
    -- scenarios where it isn't
    case ( model.currentLevel, model.currentNumber ) of
        ( NotAsked, _ ) ->
            h3 [] [ text "You haven't asked for a level to be loaded." ]

        ( Loading, _ ) ->
            h3 [] [ text "Loading level... please wait" ]

        ( Failure reason, _ ) ->
            viewError reason

        ( Success level, Success number ) ->
            viewLevel level number model.moves

        ( _, _ ) ->
            h3 [] [ text "Something went wrong..." ]


viewLevel : Level -> Inscribed Float -> Int -> Html Msg
viewLevel level number moves =
    div []
        [ h2 [] [ text ("Level " ++ levelIdToString level.id) ]
        , h3 [] [ text ("Goal number : " ++ String.fromFloat level.goalNumber) ]
        , h3 [] [ text ("Current number : " ++ String.fromFloat (Inscribed.extractValue number)) ]
        , displayWinCondition level moves number
        , operationButtons level.operations
        , actionHistory number
        ]


displayWinCondition : Level -> Int -> Inscribed Float -> Html Msg
displayWinCondition level currentMoves (Inscribed.InscribedData currentNumber _) =
    if currentNumber == level.goalNumber && currentMoves <= level.minMovesToPass then
        div []
            [ displayMoves currentMoves
                level.minMovesToPass
            , displayWinScreen
            ]

    else if currentNumber /= level.goalNumber && currentMoves > level.minMovesToPass then
        div []
            [ displayMoves currentMoves
                level.minMovesToPass
            , displayLoseScreen
            ]

    else
        displayMoves currentMoves level.minMovesToPass


displayMoves : Int -> Int -> Html Msg
displayMoves currentMoves allowedMoves =
    div []
        [ strong [] [ text ("Remaining moves : " ++ String.fromInt (allowedMoves - currentMoves)) ]
        ]


displayWinScreen : Html Msg
displayWinScreen =
    div []
        [ h3 [] [ text "You won!" ]
        , button [] [ text "Next ->" ]
        ]


displayLoseScreen : Html Msg
displayLoseScreen =
    div []
        [ h3 [] [ text "You have no more moves left" ]
        , p [] [ text "Reload the page to retry" ]
        ]



-- here we take the list of the operations loaded in the level, and turn
-- them all into buttons for display


operationButtons : List (Inscribed Operation) -> Html Msg
operationButtons operations =
    div [] (List.map inscribedOperationToButton operations)


inscribedOperationToButton : Inscribed Operation -> Html Msg
inscribedOperationToButton inOp =
    -- for the text, we just extract the "name" of the function
    -- when the button is clicked, we send the operation with the name
    -- to the update function, so that we can change the current number and
    -- log the change
    button [ onClick (PerformOperation inOp) ] [ text <| Inscribed.extractMessage inOp ]


actionHistory : Inscribed Float -> Html Msg
actionHistory inFloat =
    div []
        [ h3 [] [ text "Your actions so far : " ]
        , p [] [ text <| Inscribed.extractMessage inFloat ]
        ]



-- update and msg


type Msg
    = ReceivedLevel (WebData Level)
    | PerformOperation (Inscribed Operation)


loadLevel : LevelId -> Cmd Msg
loadLevel lvlId =
    Http.get
        { url = "http://localhost:5019/levels/" ++ levelIdToString lvlId
        , expect = Http.expectJson (RemoteData.fromResult >> ReceivedLevel) levelDecoder
        }


initCurrentNumber : Level -> ( Inscribed Float, Cmd Msg )
initCurrentNumber levelData =
    ( Inscribed.InscribedData levelData.startNumber "", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
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
                ( currentNumber, cmd ) =
                    RemoteData.update initCurrentNumber levelData
            in
            ( { model | currentNumber = currentNumber, currentLevel = levelData }, cmd )

        PerformOperation inscribedOperation ->
            let
                -- First, we turn the operation into a bindable function
                bindable =
                    makeBindable inscribedOperation
            in
            -- now we check that the number is loaded
            case ( model.currentNumber, bindable ) of
                ( RemoteData.Success inscFloat, fn ) ->
                    -- now we check the bindable function to see if it
                    -- accepts floats or integers
                    case fn of
                        FloatFunction ffn ->
                            -- if it's a float, we can just chuck our number in.
                            -- we do have to wrap it into a remotedata type though
                            ( { model | currentNumber = RemoteData.succeed (bind ffn inscFloat) }, Cmd.none )

                        IntFunction ifn ->
                            -- if it's an int, we round our float, put it through,
                            -- convert the result to float, then remotedata succeed
                            -- that and save it
                            let
                                roundedFloat =
                                    Inscribed.map round inscFloat

                                result =
                                    bind ifn roundedFloat

                                resultToFloat =
                                    Inscribed.map toFloat result
                            in
                            ( { model | currentNumber = RemoteData.succeed resultToFloat }, Cmd.none )

                ( _, _ ) ->
                    ( model, Cmd.none )


