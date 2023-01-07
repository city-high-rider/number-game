module ErrorViewing exposing (httpErrorToString, viewError)

import ColorScheme
import Element exposing (centerX, el, fill, layout, paragraph, text, width)
import Element.Background
import Element.Font
import Html exposing (Html, div, h3, p)
import Http


viewError : Http.Error -> Html msg
viewError error =
    let
        errorText : String
        errorText =
            httpErrorToString error
    in
    layout [ Element.Background.color ColorScheme.veryDark ] <|
        Element.column [ width fill ]
            [ Element.row [ centerX ]
                [ paragraph [ Element.Font.size 32 ]
                    [ el [ Element.Font.color ColorScheme.accent ] (text "There was an error!")
                    ]
                ]
            , paragraph [ Element.Font.size 25 ]
                [ el [ Element.Font.color ColorScheme.light ] (text errorText)
                ]
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
