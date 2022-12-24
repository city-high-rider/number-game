module ErrorViewing exposing (httpErrorToString, viewError)

import Html exposing (Html, div, h3, p, text)
import Http


viewError : Http.Error -> Html msg
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
