-- todo : handle rounding explicitly, i.e let the player have a button for
-- it, add win condition


module Pages.Play exposing (..)

import ColorScheme
import Element exposing (Element, alignTop, centerX, column, el, fill, fillPortion, height, layout, mouseOver, padding, paddingEach, paragraph, spaceEvenly, width)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import ErrorViewing exposing (viewError)
import Html exposing (Html, button, div, h2, h3, p, strong, text)
import Html.Events exposing (onClick)
import Http
import Inscribed exposing (BindableFunction(..), Inscribed, bind, executeOperation, makeBindable)
import Level exposing (Level, LevelId, levelDecoder, levelIdToString)
import LoadingScreen
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
    , operationError : Maybe String
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
            , operationError = Nothing
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
            LoadingScreen.viewLoadingScreen

        ( Failure reason, _ ) ->
            ErrorViewing.viewError reason

        ( Success level, Success number ) ->
            viewLevel level model number

        ( _, _ ) ->
            h3 [] [ text "Something went wrong..." ]


viewLevel : Level -> Model -> Inscribed Float -> Html Msg
viewLevel level model currentNumber =
    Element.layout [ Element.Background.color ColorScheme.veryDark ] <|
        Element.column [ width fill, height fill ]
            [ viewLevelInfo level model.moves
            , actionHistory currentNumber model.operationError
            , Element.row [ width fill, spaceEvenly ]
                [ displayButtons level.operations
                , displayWinState level model.moves (Inscribed.extractValue currentNumber)
                ]
            ]


displayWinState : Level -> Int -> Float -> Element Msg
displayWinState level currentMoves currentNumber =
    let
        screen : Element Msg
        screen =
            if level.goalNumber == currentNumber && currentMoves <= level.availableMoves then
                winScreen

            else if currentMoves >= level.availableMoves && level.goalNumber /= currentNumber then
                failScreen

            else
                Element.none
    in
    Element.row
        [ padding 5
        , height fill
        , width <| fillPortion 2
        , Element.Border.width 2
        , Element.Border.color ColorScheme.neutral
        ]
        [ Element.column [ width <| fillPortion 1, Element.Font.size 25 ]
            [ paragraph [ Element.Font.color ColorScheme.red ]
                [ el [] (Element.text ("Goal : " ++ String.fromFloat level.goalNumber))
                ]
            , paragraph [ Element.Font.color ColorScheme.light ]
                [ el [] (Element.text ("Current : " ++ String.fromFloat currentNumber))
                ]
            ]
        , screen
        ]


winScreen : Element Msg
winScreen =
    let
        buttonLabel =
            el [ padding 5, centerX ]
                (Element.text "Next Level ->")
    in
    Element.column [ width fill ]
        [ Element.paragraph
            [ Element.Font.size 22
            , Element.Font.color ColorScheme.light
            , padding 5
            , centerX
            , width fill
            ]
            [ el [] (Element.text "You won!") ]
        , Element.Input.button
            [ Element.Font.color ColorScheme.red
            , Element.Background.color ColorScheme.neutral
            , Element.Border.width 2
            , Element.Border.color ColorScheme.red
            , padding 5
            , width fill
            , mouseOver
                [ Element.Background.color ColorScheme.red
                , Element.Font.color ColorScheme.neutral
                ]
            ]
            { onPress = Just GotoNextLevel, label = buttonLabel }
        ]


failScreen : Element Msg
failScreen =
    Element.column []
        [ Element.paragraph
            [ Element.Font.size 22
            , Element.Font.color ColorScheme.red
            ]
            [ el [] (Element.text "The level is no longer winnable, reload to try again.") ]
        ]


displayButtons : List (Inscribed Operation) -> Element Msg
displayButtons operations =
    Element.column [ padding 5, width <| fillPortion 1, Element.scrollbarY ] <|
        List.map inscribedOperationToButton operations


viewLevelInfo : Level -> Int -> Element msg
viewLevelInfo level movesLeft =
    let
        levelTitleText : String
        levelTitleText =
            "Level " ++ levelIdToString level.id ++ " | " ++ level.difficulty

        movesLeftText : String
        movesLeftText =
            "Moves made : " ++ String.fromInt movesLeft ++ "/" ++ String.fromInt level.availableMoves
    in
    Element.row [ width fill, spaceEvenly, padding 10, Element.Font.size 30 ]
        [ Element.link
            [ Element.Border.width 2
            , Element.Border.color ColorScheme.red
            , Element.Background.color ColorScheme.neutral
            , Element.Font.color ColorScheme.red
            , Element.padding 5
            , mouseOver
                [ Element.Background.color ColorScheme.red
                , Element.Font.color ColorScheme.neutral
                ]
            ]
            { url = "/levels"
            , label = el [] (Element.text "<- Back")
            }
        , el [ Element.Font.color ColorScheme.red ] (Element.text levelTitleText)
        , el [ Element.Font.alignRight, Element.Font.color ColorScheme.neutral ] (Element.text movesLeftText)
        ]


viewOperationError : Maybe String -> Element Msg
viewOperationError err =
    case err of
        Nothing ->
            Element.none

        Just reason ->
            Element.column []
                [ Element.paragraph [] [ el [] (Element.text "The operation failed !") ]
                , Element.paragraph [] [ el [] (Element.text ("Reason : " ++ reason)) ]
                ]



-- here we take the list of the operations loaded in the level, and turn
-- them all into buttons for display


inscribedOperationToButton : Inscribed Operation -> Element Msg
inscribedOperationToButton inOp =
    -- for the text, we just extract the "name" of the function
    -- when the button is clicked, we send the operation with the name
    -- to the update function, so that we can change the current number and
    -- log the change
    Element.Input.button
        [ Element.Border.width 2
        , Element.Border.color ColorScheme.red
        , Element.Background.color ColorScheme.neutral
        , Element.Font.color ColorScheme.red
        , mouseOver
            [ Element.Background.color ColorScheme.red
            , Element.Font.color ColorScheme.neutral
            ]
        , width fill
        ]
        { onPress = Just <| PerformOperation inOp
        , label =
            el [ padding 5, centerX ]
                (Element.text <| Inscribed.extractMessage inOp)
        }


actionHistory : Inscribed Float -> Maybe String -> Element Msg
actionHistory inFloat operationError =
    let
        -- We want to show the error, if there is one
        textToShow : String
        textToShow =
            Maybe.withDefault (Inscribed.extractMessage inFloat) operationError

        heading : Element msg
        heading =
            case operationError of
                Nothing ->
                    paragraph [ Element.Font.color ColorScheme.light ] [ el [] (Element.text "Your actions: ") ]

                Just _ ->
                    paragraph [ Element.Font.color ColorScheme.red ] [ el [] (Element.text "Error!") ]
    in
    Element.column [ width fill, paddingEach { top = 80, bottom = 0, left = 5, right = 5 } ]
        [ Element.row [ centerX ]
            [ heading ]
        , Element.row
            [ width fill
            , height <| Element.px 250
            , padding 10
            , Element.Border.width 2
            , Element.Border.color ColorScheme.accent
            , Element.scrollbarY
            ]
            [ paragraph [ Element.Font.color ColorScheme.neutral, alignTop ]
                [ el [] (Element.text <| textToShow)
                ]
            ]
        ]



-- update and msg


type Msg
    = ReceivedLevel (WebData Level)
    | PerformOperation (Inscribed Operation)
    | GotoNextLevel


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
            ( { model
                | currentNumber = currentNumber
                , currentLevel = levelData
                , moves = 0
              }
            , cmd
            )

        PerformOperation inscribedOperation ->
            case model.currentNumber of
                RemoteData.Success number ->
                    let
                        newNumber =
                            executeOperation inscribedOperation number

                        newMoves =
                            model.moves + 1
                    in
                    case newNumber of
                        Ok val ->
                            ( { model
                                | currentNumber = RemoteData.succeed val
                                , moves = newMoves
                                , operationError = Nothing
                              }
                            , Cmd.none
                            )

                        Err error ->
                            ( { model | operationError = Just error }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotoNextLevel ->
            let
                -- (attempt to) get the id of the next level
                levelId =
                    RemoteData.toMaybe model.currentLevel
                        |> Maybe.map (.id >> Level.incrementId)
            in
            case levelId of
                Nothing ->
                    -- if it fails, we do nothing.
                    ( model, Cmd.none )

                Just id ->
                    -- if we did get an ID, increase it by one, then try to
                    -- load the level
                    -- resetting the number and moves is handled in the
                    -- loadlevel message
                    ( { model | moves = 0, currentLevel = RemoteData.Loading }
                    , loadLevel id
                    )
