module Pages.LevelList exposing (Model, Msg, init, update, view)

import Browser.Navigation exposing (Key)
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
            h3 [] [ text "Loading levels, please wait..." ]

        Success levels ->
            viewLevels levels

        Failure error ->
            viewError error


viewLevels : List Level -> Html Msg
viewLevels levels =
    let
        tableHeader =
            tr []
                [ th [] [ text "Level ID" ]
                , th [] [ text "Difficulty" ]
                ]

        -- if you check the operations decoder in src/Level, you will see
        -- that it's possible for a level's operation list to be empty,
        -- making it practically unbeatable. We want to filter out
        -- any levels like this
        filteredLevels =
            List.filter isLevelOperationsNotEmpty levels
    in
    table []
        (tableHeader :: List.map levelToTableRow filteredLevels)


levelToTableRow : Level -> Html Msg
levelToTableRow level =
    tr []
        [ td [] [ text (levelIdToString level.id) ]
        , td [] [ text level.difficulty ]
        , a [ href ("/levels/" ++ levelIdToString level.id) ] [ text "Play" ]
        ]


viewError : Http.Error -> Html Msg
viewError error =
    div []
        [ h3 [] [ text "There was an error loading the content !" ]
        , p [] [ text ("Reason : " ++ httpErrorToString error) ]
        ]


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message



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
