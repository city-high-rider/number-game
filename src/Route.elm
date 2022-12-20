-- the idea in this module is to convert our url routes from strings into
-- custom types, so that we are less likely to make a mistake while doing
-- routing


module Route exposing (..)

import Browser.Navigation as Nav exposing (Key)
import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)



-- This contains the pages that we can route to


type Route
    = NotFoundRoute
    | HomeRoute
    | LevelViewRoute



-- route parser


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map HomeRoute top
        , map LevelViewRoute (s "levels")
        ]



-- Url to route


urlToRoute : Url -> Route
urlToRoute url =
    Maybe.withDefault NotFoundRoute <|
        parse routeParser url



-- route to string


routeToString : Route -> String
routeToString route =
    case route of
        NotFoundRoute ->
            "/NotFoundPage"

        HomeRoute ->
            "/"

        LevelViewRoute ->
            "/levels"



-- push url command that allows other pages to redirect


pushUrl : Route -> Nav.Key -> Cmd msg
pushUrl route navKey =
    Nav.pushUrl navKey (routeToString route)
