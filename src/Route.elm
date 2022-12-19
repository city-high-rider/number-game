-- the idea in this module is to convert our url routes from strings into
-- custom types, so that we are less likely to make a mistake while doing
-- routing


module Route exposing (..)

import Url exposing (Url)
import Url.Parser exposing (Parser, map, s, oneOf, parse, top)



-- This contains the pages that we can route to


type Route
    = NotFoundPage
    | HomePage
    | LevelViewPage


-- route parser


routeParser : Parser ( Route -> a ) a
routeParser =
    oneOf
        [ map HomePage top
        , map LevelViewPage (s "levels")
        ]



-- Url to route


urlToRoute : Url -> Route
urlToRoute url =
    Maybe.withDefault NotFoundPage <|
        parse routeParser url
