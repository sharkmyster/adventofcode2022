module Day10 exposing (..)

import List.Extra as LE


type alias Model =
    { registerX : Int
    , cycleCount : Int
    , signalStrengths : List Int
    , pixels : List Char
    }


init =
    { registerX = 1
    , cycleCount = 1
    , signalStrengths = []
    , pixels = []
    }


type Command
    = Noop
    | Add Int


parseInstructions : List String -> Maybe Command
parseInstructions command =
    case command of
        [ "addx", n ] ->
            Just (Add (Maybe.withDefault 0 (String.toInt n)))

        [ "noop" ] ->
            Just Noop

        _ ->
            Nothing


encode : Command -> List (Int -> Int) -> List (Int -> Int)
encode command acc =
    case command of
        Add x ->
            (+) x :: identity :: acc

        Noop ->
            identity :: acc


cases : List number
cases =
    [ 20, 60, 100, 140, 180, 220 ]


isTouching : Int -> Int -> Bool
isTouching crtPos xPos =
    [ xPos - 1, xPos, xPos + 1 ]
        |> List.any ((==) crtPos)


computeInstruction : (Int -> Int) -> Model -> Model
computeInstruction command model =
    let
        newStrengths =
            if List.any ((==) model.cycleCount) cases then
                (model.registerX * model.cycleCount) :: model.signalStrengths

            else
                model.signalStrengths

        crtPos =
            modBy 40 (model.cycleCount - 1)

        pixel =
            if isTouching crtPos model.registerX then
                '#'

            else
                '.'
    in
    { model
        | signalStrengths = newStrengths
        , registerX = command model.registerX
        , cycleCount = model.cycleCount + 1
        , pixels = pixel :: model.pixels
    }


processInput =
    rawData
        |> String.lines
        |> List.map String.words
        |> List.filterMap parseInstructions
        |> List.foldl encode []
        |> List.reverse
        |> List.foldl computeInstruction init


part1 : Int
part1 =
    processInput
        |> .signalStrengths
        |> List.sum


part2 : List String
part2 =
    processInput
        |> .pixels
        |> List.reverse
        |> LE.groupsOf 40
        |> List.map String.fromList


rawDataTest : String
rawDataTest =
    String.trim """
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
"""


rawData : String
rawData =
    String.trim """
noop
addx 5
noop
noop
noop
addx 1
addx 2
addx 5
addx 2
addx 5
noop
noop
noop
noop
noop
addx -12
addx 18
addx -1
noop
addx 3
addx 5
addx -5
addx 7
noop
addx -36
addx 18
addx -16
noop
noop
noop
addx 5
addx 2
addx 5
addx 2
addx 13
addx -6
addx -4
addx 5
addx 2
addx 4
addx -3
addx 2
noop
addx 3
addx 2
addx 5
addx -40
addx 25
addx -22
addx 25
addx -21
addx 5
addx 3
noop
addx 2
addx 19
addx -10
addx -4
noop
addx -4
addx 7
noop
addx 3
addx 2
addx 5
addx 2
addx -26
addx 27
addx -36
noop
noop
noop
noop
addx 4
addx 6
noop
addx 12
addx -11
addx 2
noop
noop
noop
addx 5
addx 5
addx 2
noop
noop
addx 1
addx 2
addx 5
addx 2
addx 1
noop
noop
addx -38
noop
addx 9
addx -4
noop
noop
addx 7
addx 10
addx -9
addx 2
noop
addx -9
addx 14
addx 5
addx 2
addx -24
addx 25
addx 2
addx 5
addx 2
addx -30
addx 31
addx -38
addx 7
noop
noop
noop
addx 1
addx 21
addx -16
addx 8
addx -4
addx 2
addx 3
noop
noop
addx 5
addx -2
addx 5
addx 3
addx -1
addx -1
addx 4
addx 5
addx -38
noop
    """
