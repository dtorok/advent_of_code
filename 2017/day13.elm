module Day13 exposing (part1, part2)

import TestRun
import Dict exposing (Dict)


part1 : TestRun.Test
part1 =
  { title = "Day 13: Packet Scanners - Part 1"
  , solver = solver
  , testCases =
    [ ( smallInput, "24")
    , ( bigInput, "1844")
    ]
  }

part2 : TestRun.Test
part2 =
  { title = "Day 13: Packet Scanners - Part 2"
  , solver = identity
  , testCases =
    [ ("", "")
    ]
  }

-- SOLVER
---------
solver : String -> String
solver input = input
  |> parseInput -- List (Int, Int)
  |> buildFirewalls -- Firewalls
  |> sendPacket
  |> toString

type Dir = Up | Down
type alias Firewalls = Dict Int Layer
type alias Layer =
  { range : Int
  , scanPos : Int
  , scanDir : Dir
  }

fwNewLayer : Int -> Layer
fwNewLayer range =
  { range = range
  , scanPos = 0
  , scanDir = Down
  }

fwStep : Firewalls -> Firewalls
fwStep fws = Dict.map fwStepLayer fws

fwStepLayer : Int -> Layer -> Layer
fwStepLayer _ layer =
  let
      step : Int -> Dir -> Int
      step curr dir =
        case dir of
          Up -> curr - 1
          Down -> curr + 1
  in
    case layer.scanDir of
      Down ->
        if layer.scanPos < layer.range - 1 then
          { layer | scanPos = layer.scanPos + 1 }
        else if layer.range > 1 then
          { layer | scanPos = layer.scanPos - 1, scanDir = Up }
        else
          layer
      Up ->
        if layer.scanPos > 0 then
          { layer | scanPos = layer.scanPos - 1 }
        else if layer.range > 1 then
          { layer | scanPos = layer.scanPos + 1, scanDir = Down }
        else
          layer



buildFirewalls : List (Int, Int) -> Firewalls
buildFirewalls layerList =
  let
    builder : (Int, Int) -> Firewalls -> Firewalls
    builder (depth, range) firewalls =
      Dict.insert depth (fwNewLayer range) firewalls
  in
    List.foldl builder Dict.empty layerList

sendPacket : Firewalls -> Int
sendPacket fws =
  let
    maxDepth : Int
    maxDepth = fws
      |> Dict.keys
      |> List.maximum
      |> Maybe.withDefault 0

    packetPositions : List Int
    packetPositions = List.range 0 maxDepth

    -- update : Firewalls -> Firewalls
    -- update fws = Dict.map fwStepLayer fws

    run : Int -> (Firewalls, Int) -> (Firewalls, Int)
    run pos (fws, sev) =
      case Dict.get pos fws of
        Just layer ->
          let
            extraSev = if layer.scanPos == 0 then pos * layer.range else 0
            newSev = sev + extraSev
          in
            (fwStep fws, newSev)
        Nothing ->
          (fwStep fws, sev)
  in
    packetPositions
      |> List.foldl run (fws, 0)
      |> Tuple.second


-- PARSER
---------
parseInput : String -> List (Int, Int)
parseInput input = input
  |> String.trim
  |> String.split "\n" -- List String
  |> List.map parseLine -- List (Int, Int)

parseLine : String -> (Int, Int)
parseLine line = line
  |> String.trim
  |> String.split ": " -- List String
  |> List.map String.toInt -- List (Result Int)
  |> List.map (Result.mapError (\e -> Debug.crash("Unable to parse '" ++ line ++ "': " ++ e))) -- List (Result Int)
  |> List.filterMap Result.toMaybe -- List Int
  |> (\values -> case values of
        depth :: range :: [] -> (depth, range)
        _ -> Debug.crash("Parsing error to tuple: " ++ (toString values))
  )

-- INPUT
--------
smallInput : String
smallInput = """
0: 3
1: 2
4: 4
6: 4
"""

bigInput : String
bigInput = """
0: 4
1: 2
2: 3
4: 4
6: 8
8: 5
10: 6
12: 6
14: 10
16: 8
18: 6
20: 9
22: 8
24: 6
26: 8
28: 8
30: 12
32: 12
34: 12
36: 12
38: 10
40: 12
42: 12
44: 14
46: 8
48: 14
50: 12
52: 14
54: 14
58: 14
60: 12
62: 14
64: 14
66: 12
68: 12
72: 14
74: 18
76: 17
86: 14
88: 20
92: 14
94: 14
96: 18
98: 18
"""