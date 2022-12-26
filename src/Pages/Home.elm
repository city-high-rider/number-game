-- this is a home page with some info about the site


module Pages.Home exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav exposing (Key)
import ColorScheme exposing (..)
import Element exposing (alignLeft, alignRight, centerX, column, el, fill, layout, mouseOver, none, paragraph, rgb, row, text, width)
import Element.Background
import Element.Border
import Element.Font exposing (color, size)
import Element.Input exposing (button)
import Html exposing (Html, button, div, h2, h3, p, text)
import Html.Events exposing (onClick)
import Route exposing (Route(..), pushUrl)
import Url exposing (Url)



-- model and init


type alias Model =
    { navKey : Nav.Key
    }


init : Nav.Key -> ( Model, Cmd Msg )
init navkey =
    ( Model navkey, Cmd.none )



-- view function


view : Model -> Html Msg
view _ =
    layout [ Element.Background.color ColorScheme.veryDark ] <|
        column [ width fill ]
            [ row [ centerX ]
                [ paragraph [ Element.padding 10, color ColorScheme.red ]
                    [ el [ size 35 ] (Element.text "Number game | Home") ]
                ]
            , paragraph [ Element.padding 20, color ColorScheme.light ]
                [ column [ Element.padding 10 ]
                    [ el [ size 28 ] (Element.text "How to play: ")
                    , Element.text "You are given a starting number and a goal number."
                    , Element.text "You are also given a set of operations that you can perform on the number. The objective is to set your number equal to the goal number."
                    , paragraph []
                        [ Element.text "However, you only have a "
                        , el [ color ColorScheme.red ]
                            (Element.text "limited amount of moves")
                        , Element.text " to finish the level, so think carefully!"
                        ]
                    ]
                ]
            , Element.Input.button [ Element.padding 20 ]
                { onPress = Just GotoLevelsPage
                , label =
                    el
                        [ Element.padding 6
                        , color ColorScheme.red
                        , size 22
                        , Element.Background.color ColorScheme.neutral
                        , Element.Border.solid
                        , Element.Border.color ColorScheme.red
                        , Element.Border.width 2
                        , mouseOver
                            [ color ColorScheme.neutral
                            , Element.Background.color ColorScheme.red
                            ]
                        ]
                        (Element.text "Play !")
                }
            ]



-- update and msg


type Msg
    = GotoLevelsPage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotoLevelsPage ->
            ( model, Route.pushUrl LevelViewRoute model.navKey )
