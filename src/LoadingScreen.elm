module LoadingScreen exposing (..)

import ColorScheme
import Element exposing (centerX, column, fill, layout, paragraph, row, width)
import Element.Background
import Element.Font
import Html exposing (Html)


viewLoadingScreen : Html msg
viewLoadingScreen =
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
