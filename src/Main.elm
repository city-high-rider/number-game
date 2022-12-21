module Main exposing (..)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation as Nav exposing (Key)
import Html exposing (Html, div, h3, p, text)
import Pages.Home as HomePageFile exposing (Model, Msg, init, update, view)
import Pages.LevelList as LevelListPageFile exposing (Model, Msg, init, update, view)
import Route exposing (Route(..), urlToRoute)
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = LinkChanged
        }



-- model and init function


type alias Model =
    { page : Page
    , route : Route
    , navKey : Nav.Key
    }


type Page
    = NotFoundPage
    | HomePage HomePageFile.Model
    | LevelListPage LevelListPageFile.Model



-- init function


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        model =
            { page = NotFoundPage
            , route = urlToRoute url
            , navKey = key
            }
    in
    initCurrentPage ( model, Cmd.none )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( initialModel, initialCommands ) =
    -- here we need to first parse the model route to get a page
    -- then we have to load the corresponding page init function
    -- and set our model's page to the appropriate one, plus map any
    -- initial commands that our loaded pages may have
    let
        ( loadedPage, initalMappedPageCmds ) =
            case initialModel.route of
                NotFoundRoute ->
                    ( NotFoundPage, Cmd.none )

                HomeRoute ->
                    let
                        ( loadedPageModel, loadedPageCmds ) =
                            HomePageFile.init initialModel.navKey
                    in
                    ( HomePage loadedPageModel, Cmd.map HomePageMsg loadedPageCmds )

                LevelViewRoute ->
                    let
                        ( loadedPageModel, loadedPageCmds ) =
                            LevelListPageFile.init initialModel.navKey
                    in
                    ( LevelListPage loadedPageModel, Cmd.map LevelListPageMsg loadedPageCmds )
    in
    ( { initialModel | page = loadedPage }, Cmd.batch [ initialCommands, initalMappedPageCmds ] )



-- view


viewPage : Model -> Html Msg
viewPage model =
    case model.page of
        NotFoundPage ->
            notFoundView

        HomePage homePageModel ->
            HomePageFile.view homePageModel
                |> Html.map HomePageMsg

        LevelListPage listPageModel ->
            LevelListPageFile.view listPageModel
                |> Html.map LevelListPageMsg


view : Model -> Document Msg
view model =
    { title = "Number game"
    , body = [ viewPage model ]
    }


notFoundView : Html Msg
notFoundView =
    h3 [] [ text "The page you requested was not found !" ]



-- update and msg


type Msg
    = HomePageMsg HomePageFile.Msg
    | LevelListPageMsg LevelListPageFile.Msg
    | LinkClicked UrlRequest
    | LinkChanged Url



-- in this function if we are on a page and receive its corresponding
-- message, we call its update function and save the new model in main and
-- run any commands
-- It also handles URL changes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( HomePageMsg homePageMessage, HomePage homePageModel ) ->
            let
                ( updatedPageModel, updatedPageCmd ) =
                    HomePageFile.update homePageMessage homePageModel
            in
            ( { model | page = HomePage updatedPageModel }, Cmd.map HomePageMsg updatedPageCmd )

        ( LevelListPageMsg listPageMessage, LevelListPage listPageModel ) ->
            let
                ( updatedPageModel, updatedPageCmd ) =
                    LevelListPageFile.update listPageMessage listPageModel
            in
            ( { model | page = LevelListPage updatedPageModel }, Cmd.map LevelListPageMsg updatedPageCmd )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External urlString ->
                    ( model, Nav.load urlString )

        ( LinkChanged url, _ ) ->
            let
                newRoute =
                    Route.urlToRoute url
            in
            initCurrentPage ( { model | route = newRoute }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )
