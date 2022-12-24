module Day09 exposing (..)

import Dict exposing (Dict)
import List.Extra as LE
import Set


type Direction
    = Up
    | Down
    | Left
    | Right


type Move
    = Move Direction Int


type alias Coordinate =
    ( Int, Int )


type alias Grid =
    Dict ( Int, Int ) Int


parseDirection : String -> Maybe Direction
parseDirection dir =
    case dir of
        "R" ->
            Just Right

        "L" ->
            Just Left

        "U" ->
            Just Up

        "D" ->
            Just Down

        _ ->
            Nothing


createMove : Direction -> Int -> Move
createMove direction distance =
    Move direction distance


pairs : Int -> Int -> List ( ( Int, Int ), Int )
pairs x y =
    List.range 0 x
        |> List.map (\xx -> ( ( xx, y ), 0 ))


generateGrid : Int -> Int -> Grid
generateGrid x y =
    List.range 0 y
        |> List.map (pairs x)
        |> List.concat
        |> Dict.fromList


parseInstruction : List String -> Maybe Move
parseInstruction bits =
    case bits of
        [ inputDirection, inputDistance ] ->
            Maybe.map2
                createMove
                (parseDirection inputDirection)
                (String.toInt inputDistance)

        _ ->
            Nothing


up : Coordinate -> Coordinate
up ( a, b ) =
    ( a, b + 1 )


down : Coordinate -> Coordinate
down ( a, b ) =
    ( a, b - 1 )


right : Coordinate -> Coordinate
right ( a, b ) =
    ( a + 1, b )


left : Coordinate -> Coordinate
left ( a, b ) =
    ( a - 1, b )


type alias Model =
    { head : Coordinate
    , tail : List Coordinate
    , tailMoves : Set.Set Coordinate
    , history : List (List Coordinate)
    }


doSteps : Model -> (Model -> Model) -> Int -> Model
doSteps model fn steps =
    case steps of
        0 ->
            model

        _ ->
            doSteps (fn model) fn (steps - 1)


foldTail follower ( leader, list ) =
    let
        newPostin =
            calculateMove leader follower
    in
    ( newPostin, newPostin :: list )


move : (Coordinate -> Coordinate) -> Model -> Model
move moveFn model =
    let
        newHead =
            moveFn model.head

        ( lastTail, newTail ) =
            List.foldl foldTail ( newHead, [] ) model.tail
    in
    { model
        | head = newHead
        , tail = List.reverse newTail
        , tailMoves = Set.insert lastTail model.tailMoves
    }


recordHistory : Model -> Model
recordHistory model =
    let
        lastSequence =
            model.head :: model.tail
    in
    { model | history = lastSequence :: model.history }


doMove : Move -> Model -> Model
doMove (Move direction steps) model =
    let
        directionFn =
            case direction of
                Up ->
                    move up

                Down ->
                    move down

                Left ->
                    move left

                Right ->
                    move right
    in
    doSteps model directionFn steps
        |> recordHistory


diffs : Coordinate -> Coordinate -> Coordinate
diffs ( tx, ty ) ( fx, fy ) =
    ( fx - tx, fy - ty )



-- |> LE.groupsOf 5
-- |> LE.transpose
-- |> List.reverse
-- |> Debug.log ">>>"
-- |> printIt
-- |> Debug.log ""
-- |> List.filter ((/=) 0)
-- |> List.length
-- |> (+) 1
-- (4,3) - (2,2)  = (-2, -1)    -> (3,3) tx + 1
-- (4,3) - (2,4)  = (-2,  1)    -> (3,3)
-- (4,3) - (2,3)  = (-2,  0)    -> (3,3)
-- (4,3) - (6,4)  = ( 2,  1)    -> (5,3) tx - 1
-- (4,3) - (6,2)  = ( 2, -1)    -> (5,3)
-- (4,3) - (6,3)  = ( 2,  0)    -> (5,3)
-- (4,3) - (3,1)  = (-1, -2)    -> (4,2) ty - 1
-- (4,3) - (5,1)  = ( 1, -2)    -> (4,2)
-- (4,3) - (4,1)  = ( 0, -2)    -> (4,2)
-- (4,3) - (3,5) = (-1,  2)    -> (4,4) ty + 1
-- (4,3) - (5,5) = ( 1,  2)    -> (4,4)
-- [(2,2), (2,4), (2,3),(6,4),(6,2),(6,3),(3,1),(5,1),(4,1),(3,5),(5,5),(4,5)]-- (4,3) - (4,5) = ( 0,  2)    -> (4,4)


calculateMove : Coordinate -> Coordinate -> Coordinate
calculateMove (( tx, ty ) as target) follower =
    let
        ( x, y ) =
            diffs target follower
    in
    if x == -2 && y == -2 then
        ( tx - 1, ty - 1 )

    else if x == -2 && y == 2 then
        ( tx - 1, ty + 1 )

    else if x == 2 && y == 2 then
        ( tx + 1, ty + 1 )

    else if x == 2 && y == -2 then
        ( tx + 1, ty - 1 )

    else if x == -2 then
        ( tx - 1, ty )

    else if x == 2 then
        ( tx + 1, ty )

    else if y == -2 then
        ( tx, ty - 1 )

    else if y == 2 then
        ( tx, ty + 1 )

    else
        follower


part1 =
    let
        instructions =
            rawData
                |> String.trim
                |> String.lines
                |> List.map String.words
                |> List.filterMap parseInstruction

        start =
            ( 0, 0 )

        initBoard =
            { head = start
            , tail = List.repeat 9 ( 0, 0 )
            , tailMoves = Set.singleton ( 0, 0 )
            , history = []
            }
    in
    instructions
        -- |> List.take 1
        |> List.foldl doMove initBoard



-- |> Debug.log ">"
-- |> .tailMoves
-- -- |> Debug.log ">"
-- |> Set.size


part2 =
    rawDataTest
        |> String.trim
        |> String.lines


rawDataTest : String
rawDataTest =
    String.trim """
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"""


rawDataTest2 : String
rawDataTest2 =
    String.trim """
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
"""


rawData : String
rawData =
    String.trim """
L 1
R 1
L 1
U 1
R 2
U 1
D 2
R 2
U 1
D 2
U 2
L 2
D 1
U 1
D 2
L 2
D 1
L 2
D 2
L 1
U 2
R 2
L 1
D 2
L 2
R 2
D 2
L 1
U 1
R 1
U 1
L 1
D 1
R 1
U 2
D 2
R 1
U 1
R 1
L 1
U 1
R 1
D 1
L 1
U 1
R 1
D 1
U 1
D 1
U 1
R 2
L 2
D 2
L 2
R 1
U 2
L 2
R 1
U 1
R 2
D 2
R 2
L 2
U 2
R 2
D 2
R 1
U 1
L 2
R 1
U 1
D 2
U 2
D 1
R 2
L 2
D 2
L 2
R 2
U 1
R 2
L 2
D 2
L 1
D 1
R 2
L 1
U 2
R 1
U 2
D 1
U 2
D 1
L 1
R 2
D 2
R 2
U 2
R 1
U 2
R 2
U 1
R 1
D 2
U 2
R 2
U 1
R 2
D 2
R 1
L 1
U 3
L 2
U 1
L 2
U 1
D 1
U 2
D 1
U 2
R 2
L 1
U 3
R 1
U 1
R 3
L 3
D 3
L 2
U 1
D 3
U 1
R 1
L 1
R 2
L 1
R 3
L 3
D 1
R 2
L 3
U 3
D 1
L 2
U 2
D 2
L 2
U 1
R 1
L 2
R 3
L 3
D 3
R 2
D 3
U 1
L 3
R 1
U 2
L 1
D 3
L 1
U 1
D 3
L 3
U 2
D 1
U 2
L 3
R 1
U 2
D 2
R 3
D 3
U 2
D 3
L 3
R 3
L 1
U 3
L 1
D 1
L 2
D 2
R 1
L 3
R 2
L 3
R 3
L 1
D 1
U 3
L 2
R 2
L 2
D 3
U 3
L 3
U 3
D 1
U 2
R 2
U 1
D 3
L 3
R 1
L 3
U 3
D 1
L 3
D 2
R 1
U 1
R 1
D 2
L 2
U 1
L 2
R 2
L 2
D 3
U 3
D 4
L 3
D 4
R 3
L 2
U 3
R 3
U 1
R 4
U 2
D 1
R 3
L 1
R 1
D 3
R 2
D 1
R 2
L 2
U 2
D 1
R 1
U 2
R 3
L 2
D 1
U 3
R 2
D 4
L 3
U 1
R 1
D 2
U 1
R 3
L 4
D 2
L 1
U 3
L 2
U 3
R 1
D 1
L 2
R 4
D 4
U 1
R 3
U 3
R 1
U 3
L 2
U 3
L 2
D 4
U 1
R 1
L 2
R 2
D 2
L 2
D 2
U 4
D 4
L 2
U 2
D 3
L 4
U 4
D 4
R 1
D 4
R 1
D 1
L 1
R 1
L 1
D 2
L 4
D 2
R 3
L 1
D 2
U 3
L 2
U 3
R 4
D 4
U 1
L 1
U 4
L 4
U 3
D 2
R 3
L 1
D 3
U 3
R 1
L 3
U 1
D 1
R 1
U 3
R 2
L 1
D 1
L 3
U 1
D 1
L 2
R 3
U 4
L 5
D 5
R 5
D 3
U 3
D 1
L 5
R 2
U 1
L 2
D 1
L 1
D 4
R 1
U 1
L 1
U 1
D 3
L 4
D 1
U 5
L 1
R 5
L 2
R 4
D 5
U 2
D 3
U 4
L 1
R 2
U 4
L 4
D 3
R 1
U 1
R 3
D 3
U 5
L 3
D 3
U 5
R 4
L 1
D 4
U 1
L 1
R 4
D 5
L 5
D 3
R 5
U 3
R 2
D 2
U 2
D 5
R 5
D 5
R 2
D 5
U 2
D 2
R 1
L 5
D 2
U 2
R 5
U 1
D 5
U 5
L 2
D 5
L 1
R 4
U 4
D 1
L 5
U 4
L 1
R 3
L 3
R 3
L 3
D 3
R 4
U 5
L 2
D 3
U 2
L 1
R 5
U 1
L 3
U 4
L 4
D 4
R 1
D 4
R 5
D 5
U 5
R 5
L 3
D 3
U 2
R 5
U 5
R 2
D 5
R 1
U 2
D 5
L 3
R 5
U 5
R 4
D 6
L 4
U 4
L 6
R 2
L 4
U 5
L 1
U 6
D 4
R 3
U 2
R 1
D 5
L 4
U 2
R 6
U 1
L 2
D 6
U 1
R 2
L 3
R 2
L 5
U 1
R 3
D 3
L 4
R 5
U 1
D 3
R 6
D 5
R 4
D 3
U 2
D 4
R 5
U 4
R 2
L 2
U 5
D 5
U 6
D 4
U 5
L 5
R 1
D 3
R 3
L 3
D 3
L 3
U 6
R 5
D 6
U 5
L 1
R 4
L 1
U 4
L 5
D 5
U 4
D 3
R 3
L 4
R 4
U 6
L 5
D 2
U 3
R 4
U 5
L 5
D 6
R 5
D 5
L 3
U 6
L 3
U 5
D 2
U 4
R 4
U 3
D 1
R 2
L 6
R 2
L 3
U 5
L 1
R 4
D 2
U 5
D 2
R 4
U 6
R 6
L 6
U 6
L 4
R 5
D 3
U 1
D 6
L 1
D 3
L 4
D 6
U 3
D 2
L 2
U 5
D 2
U 5
D 6
R 4
D 1
U 1
D 5
U 1
L 7
U 3
R 3
U 5
D 2
R 7
D 6
L 7
R 6
L 3
R 3
U 1
L 4
D 4
L 6
R 1
U 4
L 3
R 3
D 3
L 2
D 7
U 7
D 7
R 2
L 6
D 2
R 6
D 6
U 5
L 4
U 3
D 6
U 3
D 1
L 7
U 7
L 4
R 3
D 6
U 2
L 6
D 3
U 7
D 2
R 7
D 4
L 4
R 4
U 2
D 3
U 2
L 4
R 4
L 4
D 6
R 4
U 1
R 7
D 7
U 7
R 6
D 4
U 2
D 4
L 5
U 4
D 6
R 4
U 2
R 1
U 4
D 5
R 1
D 7
R 4
U 1
D 4
U 6
L 6
R 1
D 1
R 1
D 6
R 6
D 3
U 1
D 5
R 4
L 1
D 6
R 6
L 8
U 1
D 7
R 4
L 2
R 6
U 7
L 2
D 5
R 6
D 3
L 4
R 5
U 7
L 3
U 8
L 1
U 6
L 8
R 7
D 2
U 3
R 4
L 8
D 4
L 1
U 1
D 7
U 8
R 7
D 2
L 5
U 6
D 7
R 4
L 1
R 3
U 1
R 2
U 1
D 7
R 6
U 7
D 6
L 2
D 8
U 4
R 8
L 5
D 6
L 7
U 7
R 8
L 5
D 5
R 4
L 5
U 8
D 4
U 6
D 2
U 5
R 7
L 8
U 7
R 3
L 8
R 6
D 4
R 2
D 6
R 5
D 2
U 6
L 4
U 8
R 3
U 1
L 6
R 1
L 3
R 1
U 3
L 8
D 5
U 6
L 8
D 6
R 2
U 6
R 4
L 8
D 1
R 6
D 8
U 6
D 6
L 3
U 4
D 2
L 2
D 7
L 5
U 3
R 8
D 6
R 5
L 8
U 5
L 9
R 7
D 5
R 2
U 5
L 3
D 9
R 4
U 1
L 3
R 5
L 7
R 8
D 1
U 4
L 4
R 3
U 8
L 8
D 8
U 9
R 6
U 3
R 2
U 8
D 9
R 3
L 2
U 9
R 9
L 8
D 2
R 1
L 8
U 7
R 3
L 1
R 9
L 4
R 2
U 3
D 8
R 5
U 9
L 9
R 1
D 5
R 5
U 4
L 9
R 6
D 5
L 2
R 6
U 1
D 5
L 3
R 1
U 5
R 4
D 4
U 8
D 2
U 6
R 6
U 5
R 3
U 5
R 5
L 8
D 9
L 7
R 1
U 7
R 5
U 6
R 8
U 2
R 2
U 8
R 7
D 3
R 6
L 8
R 4
L 8
R 8
U 8
R 3
L 3
U 1
L 9
D 9
R 9
U 4
R 1
L 3
D 4
U 5
R 7
D 6
L 9
U 7
L 4
R 8
U 8
D 4
L 5
D 9
U 2
D 9
R 4
D 8
R 8
D 8
U 9
L 6
R 6
L 2
U 9
L 4
U 10
L 1
R 10
D 3
U 6
R 5
D 3
R 4
L 4
U 7
D 9
R 2
D 10
U 9
L 7
D 4
R 2
L 10
D 5
R 5
U 9
R 4
L 7
R 5
D 3
R 6
D 8
R 6
D 10
R 7
U 4
D 5
U 6
L 3
D 3
U 10
R 10
U 10
D 9
R 6
D 2
R 9
D 8
R 7
U 10
R 4
U 6
R 7
U 2
R 5
U 6
L 4
U 8
L 6
D 8
R 2
U 2
L 4
U 4
R 5
U 10
D 8
U 7
R 5
D 3
L 2
R 9
U 10
L 1
U 5
L 1
D 3
L 9
R 9
U 2
D 3
U 8
R 10
L 2
U 9
D 4
U 4
D 10
L 4
D 3
R 3
L 2
U 8
L 4
D 6
R 2
L 3
D 6
L 4
D 5
L 7
U 9
L 3
D 4
R 6
L 10
U 5
L 3
R 3
D 6
U 1
L 3
R 5
D 8
R 11
U 2
L 11
R 7
U 8
R 4
D 4
U 6
D 4
U 9
D 5
R 9
D 9
U 6
D 1
U 3
L 4
U 3
D 11
U 11
R 4
L 3
U 5
D 1
R 4
U 6
D 4
U 3
D 8
L 7
R 8
L 9
R 3
D 4
U 6
L 2
U 6
R 3
D 8
R 1
L 5
U 8
D 3
R 8
L 8
U 3
R 2
U 11
L 9
U 4
D 10
L 7
D 1
U 2
D 7
R 4
L 2
U 8
L 5
U 5
D 1
L 11
R 4
L 7
U 2
D 9
L 4
U 11
R 8
L 1
D 10
U 5
R 4
L 8
U 8
D 11
U 8
D 3
R 4
U 8
R 2
U 3
R 9
D 4
L 2
D 4
L 4
U 4
R 5
U 4
L 10
U 4
L 4
R 8
D 5
L 2
U 5
D 9
U 10
D 6
R 9
D 4
R 10
U 4
D 12
R 7
L 12
R 8
L 11
U 1
D 4
L 1
D 7
R 1
L 5
R 9
L 3
D 7
L 2
R 6
U 2
D 2
U 9
L 4
U 11
D 4
R 7
D 7
L 11
U 11
L 1
D 9
U 6
R 3
D 3
L 9
R 7
L 6
D 6
R 11
D 10
U 11
R 1
U 2
R 9
L 6
U 7
D 5
U 7
R 4
U 12
R 2
D 1
L 7
R 5
D 6
R 1
D 11
R 9
L 6
U 11
D 7
L 7
R 9
L 7
D 12
U 7
D 12
R 3
L 2
R 10
D 1
L 9
R 3
D 6
R 11
U 2
L 2
U 3
R 3
L 6
U 3
R 1
L 5
R 7
U 2
D 8
L 9
U 7
L 9
D 12
U 3
D 9
U 1
L 10
D 9
L 11
U 6
R 9
U 10
L 5
R 11
U 3
R 9
D 12
U 1
D 11
R 12
U 1
D 7
L 1
U 7
L 5
U 8
L 2
R 6
U 13
R 3
D 8
U 2
L 5
U 7
L 7
U 13
L 3
R 5
D 1
L 9
U 2
L 7
R 9
U 12
R 9
L 13
R 1
U 11
R 4
U 6
D 6
R 13
D 13
U 5
L 9
U 2
D 9
R 12
D 13
U 9
D 10
U 11
R 11
U 12
D 5
L 1
R 2
U 2
D 5
U 1
D 8
R 8
L 1
U 9
L 11
D 10
R 1
D 6
U 5
D 11
L 13
D 2
R 1
L 1
R 7
L 1
U 2
D 6
U 4
L 7
U 8
D 7
L 11
U 5
D 5
R 1
D 6
R 1
U 9
L 11
D 6
U 10
D 10
R 2
U 13
L 5
U 9
L 13
U 7
L 3
D 9
L 13
R 5
L 5
U 5
L 13
U 10
L 1
R 10
L 8
R 13
U 13
R 7
D 6
R 4
L 5
U 3
D 6
R 13
U 9
L 10
U 13
R 3
D 8
L 8
U 5
L 9
U 4
D 11
L 14
U 11
R 1
D 7
U 4
R 11
U 3
R 2
U 11
L 6
R 2
D 5
L 1
U 1
L 11
R 5
U 10
D 12
R 3
L 6
R 3
D 9
L 4
D 11
L 11
D 4
R 10
D 3
L 12
D 8
U 7
R 2
D 1
L 5
U 11
L 9
R 4
U 4
D 11
R 9
D 3
U 6
L 4
U 4
L 8
U 6
L 14
R 13
D 5
R 5
U 5
R 2
U 8
L 12
R 2
U 12
D 4
R 3
L 5
D 11
R 7
U 9
D 10
L 1
D 4
U 4
D 1
R 9
L 10
R 11
L 8
D 10
R 10
L 3
R 1
L 9
D 4
L 8
R 7
U 12
R 5
L 10
R 7
U 6
R 7
U 8
D 1
U 1
L 1
R 2
D 14
U 4
D 5
R 2
D 1
R 5
D 6
R 3
L 12
U 11
D 1
L 7
R 10
U 2
R 6
U 11
R 12
U 2
R 4
L 1
D 7
U 2
D 9
U 12
D 11
U 2
L 4
R 10
U 13
R 2
L 8
R 13
L 11
R 13
L 13
R 5
D 2
U 10
D 14
L 8
U 11
R 1
L 15
U 13
L 4
D 9
L 1
U 14
R 10
D 2
R 15
L 12
R 15
U 15
L 3
U 1
L 1
R 9
D 11
L 9
D 3
U 12
R 1
D 7
L 8
R 10
D 2
L 4
D 2
U 12
D 12
L 15
U 3
D 13
U 13
D 3
R 4
L 7
D 6
U 7
D 7
R 2
D 2
R 10
L 12
U 14
D 1
U 5
L 8
R 1
U 5
D 8
U 1
D 15
U 13
L 15
R 1
L 10
U 14
D 15
R 10
L 13
D 8
L 7
U 11
R 7
U 10
D 15
L 1
U 5
L 2
R 6
L 4
U 15
R 13
U 4
R 7
U 4
R 10
L 3
U 10
R 10
D 13
R 11
L 12
D 12
U 7
R 8
D 14
L 4
R 4
L 8
D 6
R 8
L 5
U 6
L 7
U 14
D 14
R 14
L 9
D 5
U 14
R 16
L 4
R 14
U 3
D 10
L 5
R 9
D 4
R 14
L 16
D 13
L 4
R 13
D 5
R 5
U 1
R 6
L 15
D 16
R 15
D 11
R 11
D 6
U 7
L 10
D 7
U 12
R 6
L 2
U 7
L 8
U 14
L 5
U 6
L 10
R 16
L 13
D 13
U 7
R 9
L 8
U 3
D 6
R 2
L 4
R 15
D 12
U 7
D 4
U 2
L 10
D 4
L 11
D 12
R 5
L 13
U 16
L 12
R 3
L 16
R 14
L 8
R 7
U 7
L 7
D 12
R 2
D 5
R 14
U 9
D 15
U 2
L 10
D 1
U 13
L 2
R 13
D 5
U 1
D 2
L 7
D 14
U 12
D 8
U 10
L 3
R 1
U 14
R 8
L 12
R 8
U 2
R 1
D 14
U 5
D 11
R 16
U 1
R 13
D 2
U 14
R 11
D 12
U 17
R 12
L 6
U 4
R 16
D 4
R 2
D 1
U 6
D 11
R 8
L 7
R 3
U 3
L 14
R 11
L 9
U 9
L 2
R 6
U 11
R 9
L 15
U 7
R 6
D 15
U 11
L 5
U 2
R 8
D 13
L 5
U 6
R 13
D 15
U 11
L 10
R 16
L 14
U 15
R 3
L 9
U 11
L 6
R 7
L 6
D 16
U 8
R 2
U 14
D 14
R 15
D 2
L 16
D 14
U 17
D 6
L 16
D 7
R 2
U 17
D 2
U 2
D 15
U 5
R 9
L 10
D 16
U 1
L 5
U 6
R 10
D 11
R 1
U 7
R 13
U 6
R 2
L 11
D 13
U 8
D 4
R 15
U 8
R 14
L 17
U 5
L 12
U 11
L 14
R 12
L 2
D 10
L 14
D 10
L 11
U 15
D 7
U 5
R 10
U 4
R 14
U 3
D 9
L 6
D 10
L 1
U 4
L 15
R 4
U 13
D 18
R 15
L 3
U 17
R 5
L 3
D 3
L 1
U 10
L 3
R 5
U 13
D 2
R 17
D 3
U 4
L 15
D 5
L 18
U 7
D 7
U 10
L 18
D 14
U 18
L 11
D 3
L 15
D 14
L 16
R 2
D 8
L 12
U 11
R 12
D 2
L 3
R 12
U 7
L 9
D 8
U 7
L 10
D 2
U 10
D 16
L 17
R 13
L 12
R 18
L 6
U 2
D 6
L 15
D 8
U 14
R 12
L 8
R 6
L 17
U 5
D 4
L 8
U 16
R 8
U 12
L 1
R 6
D 11
L 10
U 13
R 4
L 9
R 7
D 1
L 17
R 4
L 13
R 8
D 14
U 3
R 10
L 18
R 11
U 3
D 5
U 2
L 14
R 12
D 12
R 11
U 5
D 17
R 2
D 6
L 13
D 4
L 5
R 7
U 7
L 12
D 16
U 6
L 1
R 18
D 4
R 11
D 10
L 11
D 2
L 1
D 5
U 2
R 19
D 10
U 14
R 1
U 11
R 15
D 2
L 15
U 19
R 3
L 2
D 12
R 2
L 15
R 8
D 3
R 8
D 11
R 17
D 5
R 16
L 8
R 12
L 5
R 14
D 3
R 19
D 4
R 18
L 4
U 12
D 7
L 8
R 11
U 14
L 12
U 17
L 1
D 5
R 8
L 18
U 16
L 15
D 13
U 18
R 11
L 7
R 19
L 15
U 10
L 4
R 9
U 7
R 8
L 16
R 8
L 17
D 8
L 17
D 18
U 9
D 2
L 6
U 6
L 7
D 18
R 1
U 13
L 2
U 1
R 8
U 11
D 14
R 6
U 13
R 2
D 11
L 5
U 12
D 7
U 4
R 4
D 19
R 17
L 10
U 5
L 1
U 4
L 6
D 11
L 16
D 9
R 16
L 3
R 1
D 3
L 5
U 18
D 13
R 10
U 5
D 4
U 17
R 4
D 18
U 1
"""
