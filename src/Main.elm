module Main exposing (..)

import Array
import Browser
import Day09
import Dict
import Grid exposing (Grid)
import Html
import Html.Events
import List.Extra as LE
import Set


type alias Model =
    { iteration : Int
    , puzzleModel : Day09.Model
    }


type Msg
    = MsgNext
    | MsgPrev


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    { iteration = 0
    , puzzleModel = Day09.part1
    }


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.button [ Html.Events.onClick MsgPrev ] [ Html.text "Prev" ]
        , Html.button [ Html.Events.onClick MsgNext ] [ Html.text "Next" ]
        , viewGrid model
        , visitsView model
        ]


visitsView model =
    let
        numMoves =
            model.puzzleModel.tailMoves
                |> Set.size
                |> String.fromInt
    in
    Html.div [] [ Html.text numMoves ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        MsgPrev ->
            { model | iteration = model.iteration - 1 }

        MsgNext ->
            { model | iteration = model.iteration + 1 }


drawMark : ( ( Int, Int ), Int ) -> Char
drawMark ( _, x ) =
    if x > 0 then
        'x'

    else
        '.'


viewGrid : Model -> Html.Html Msg
viewGrid model =
    let
        _ =
            Debug.log ">" model.puzzleModel.history

        historyArray =
            Array.fromList (List.reverse model.puzzleModel.history)

        historyItem =
            Array.get model.iteration historyArray

        grid =
            Grid.repeat 6 6 '.'
                |> Grid.rows
                |> Array.toList
                |> List.map Array.toList
                |> List.map String.fromList
                |> List.map (\s -> Html.pre [] [ Html.text s ])
    in
    case historyItem of
        Just item ->
            Html.div []
                (viewGriddy item)

        Nothing ->
            Html.div [] [ Html.text "fucked it mate" ]


viewGriddy : List Day09.Coordinate -> List (Html.Html Msg)
viewGriddy historyRope =
    let
        grid =
            historyRope
                |> LE.indexedFoldl
                    (\i coord ->
                        let
                            ch =
                                case i of
                                    0 ->
                                        'H'

                                    1 ->
                                        '1'

                                    2 ->
                                        '2'

                                    3 ->
                                        '3'

                                    4 ->
                                        '4'

                                    5 ->
                                        '5'

                                    6 ->
                                        '6'

                                    7 ->
                                        '7'

                                    8 ->
                                        '8'

                                    9 ->
                                        '9'

                                    _ ->
                                        'T'
                        in
                        Grid.set coord ch
                    )
                    (Grid.repeat 6 5 '.')
    in
    grid
        |> Grid.rows
        |> Array.toList
        |> List.map Array.toList
        |> List.reverse
        |> List.map String.fromList
        |> List.map (\s -> Html.pre [] [ Html.text s ])



--  List.map (\row -> Html.pre [] [ Html.text (String.fromList row) ])
