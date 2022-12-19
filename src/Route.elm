-- the idea in this module is to convert our url routes from strings into
-- custom types, so that we are less likely to make a mistake while doing
-- routing


module Route exposing (..)

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
