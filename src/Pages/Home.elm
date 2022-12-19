-- this is a home page with some info about the site


module Pages.Home exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav exposing (Key)
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
    div []
        [ h2 [] [ text "Number game" ]
        , h3 [] [ text "How to play : " ]

        -- elm formmater turns this into a massive one line string
        , p [] [ text "You are given a starting number and an ending number, as well as \n     buttons that perform arithmetic on your number. the goal is to get to the \n     ending number in the least amount of moves. There is a minimum amount of moves\n     required to pass the level." ]
        , button [ onClick GotoLevelsPage ] [ text "View all levels" ]
        ]



-- update and msg


type Msg
    = GotoLevelsPage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotoLevelsPage ->
            ( model, Route.pushUrl LevelViewRoute model.navKey )
