module Main exposing (..)

import Browser
import Day09
import Html


type alias Model =
    String


type Msg
    = Increment
    | Decrement


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    ""


view : Model -> Html.Html msg
view _ =
    Html.pre []
        [ viewDay 1 (Day01.highestCalories Day01.rawData) (Day01.topNCalories Day01.rawData 3)
        ]


update : Msg -> Model -> Model
update msg data =
    data


viewDay : Int -> String -> String -> Html.Html msg
viewDay num ans1 ans2 =
    Html.div []
        [ Html.h2 [] [ Html.text ("Day " ++ String.fromInt num) ]
        , Html.p []
            [ Html.text ans1
            , Html.br [] []
            , Html.text ans2
            ]
        ]
