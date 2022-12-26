module Pages.LevelList exposing (Model, Msg, init, update, view)

import Browser.Navigation exposing (Key)
import ColorScheme
import Element exposing (centerX, centerY, column, fill, layout, paragraph, width)
import Element.Background exposing (color)
import Element.Font
import ErrorViewing exposing (viewError)
import Html exposing (Html, a, button, div, h3, p, table, td, text, th, tr)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Level exposing (Level, isLevelOperationsNotEmpty, levelIdToString, levelsDecoder)
import RemoteData exposing (RemoteData(..), WebData)



-- model and init


type alias Model =
    { levels : WebData (List Level)
    , navKey : Key
    }


init : Key -> ( Model, Cmd Msg )
init key =
    let
        initialModel =
            Model Loading key
    in
    ( initialModel, fetchLevels )



-- view function


view : Model -> Html Msg
view model =
    case model.levels of
        NotAsked ->
            div []
                [ h3 [] [ text "Nobody's asked for the levels to be loaded yet!" ]
                , button [ onClick ManualLevelLoad ] [ text "Load levels" ]
                ]

        Loading ->
            loadingView

        Success levels ->
            viewLevels levels

        Failure error ->
            viewError error


loadingView : Html Msg
loadingView =
    layout [ Element.Background.color ColorScheme.veryDark ] <|
        Element.row [ width fill ]
            [ column [ centerX ]
                [ paragraph
                    [ Element.Font.color ColorScheme.red
                    , centerX
                    , Element.padding 10
                    , Element.Font.size 25
                    ]
                    [ Element.text "Loading... please wait" ]
                ]
            ]


viewLevels : List Level -> Html Msg
viewLevels levels =
    Element.layout [ Element.Background.color ColorScheme.veryDark ] <|
        Element.table
            [ Element.Font.color ColorScheme.neutral
            , Element.padding 10
            , centerX
            ]
            { data = levels
            , columns =
                [ { header =
                        paragraph [ Element.Font.color ColorScheme.red ]
                            [ Element.text "Level ID" ]
                  , width = fill
                  , view =
                        \level ->
                            paragraph [ Element.padding 3 ]
                                [ Element.text <| Level.levelIdToString level.id
                                ]
                  }
                , { header =
                        paragraph [ Element.Font.color ColorScheme.red ]
                            [ Element.text "Level Difficulty " ]
                  , width = fill
                  , view =
                        \level ->
                            paragraph [ Element.padding 3 ]
                                [ Element.text <| level.difficulty
                                ]
                  }
                , { header = Element.none
                  , width = fill
                  , view =
                        \level ->
                            Element.link []
                                { url = "levels/" ++ Level.levelIdToString level.id
                                , label =
                                    paragraph [ Element.Font.color ColorScheme.accent ]
                                        [ Element.text "Play" ]
                                }
                  }
                ]
            }



-- update and Msg


fetchLevels : Cmd Msg
fetchLevels =
    Http.get
        { url = "http://localhost:5019/levels"
        , expect = Http.expectJson (RemoteData.fromResult >> ReceivedLevels) levelsDecoder
        }


type Msg
    = ManualLevelLoad
    | ReceivedLevels (WebData (List Level))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ManualLevelLoad ->
            ( { model | levels = Loading }, fetchLevels )

        ReceivedLevels levels ->
            ( { model | levels = levels }, Cmd.none )
