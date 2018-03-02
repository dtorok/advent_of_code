module Day13 exposing (part1, part2)

import TestRun
import Dict exposing (Dict)


part1 : TestRun.Test
part1 =
  { title = "Day 13: Packet Scanners - Part 1"
  , solver = solver1
  , testCases =
    [ ( smallInput, "24")
    , ( bigInput, "1844")
    ]
  }

part2 : TestRun.Test
part2 =
  { title = "Day 13: Packet Scanners - Part 2"
  , solver = solver2optimized
  , testCases =
    [ (smallInput, "10")
    , (bigInput, "3897604")
    ]
  }

-- SOLVER
---------
solver1 : String -> String
solver1 input = input
  |> parseInput -- List (Int, Int)
  |> buildFirewalls -- Firewalls
  |> sendPacketOptimized 0 -- (Int, Bool)
  |> Tuple.second -- Int
  |> toString -- String

solver2naive : String -> String
solver2naive input = input
  |> parseInput -- List (Int, Int)
  |> buildFirewalls -- Firewalls
  |> sendPacketInvisibleNaive -- Int
  |> toString -- String

solver2optimized : String -> String
solver2optimized input = input
  |> parseInput -- List (Int, Int)
  |> buildFirewalls -- Firewalls
  |> sendPacketInvisibleOptimized -- Int
  |> toString -- String

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

sendPacketNaive : Firewalls -> (Bool, Int)
sendPacketNaive fws =
  let
    maxDepth : Int
    maxDepth = fws
      |> Dict.keys
      |> List.maximum
      |> Maybe.withDefault 0

    packetPositions : List Int
    packetPositions = List.range 0 maxDepth

    run : Int -> (Firewalls, Bool, Int) -> (Firewalls, Bool, Int)
    run pos (fws, caught, sev) =
      -- Debug.log ((toString pos) ++ ": \n" ++ (firewallsToString pos fws)) <|
      case Dict.get pos fws of
        Just layer ->
          let
            extraSev = if layer.scanPos == 0 then pos * layer.range else 0
            newSev = sev + extraSev

            currentCaught = layer.scanPos == 0
            newCaught = caught || currentCaught
          in
            (fwStep fws, newCaught, newSev)
        Nothing ->
          (fwStep fws, caught, sev)
  in
    packetPositions
      |> List.foldl run (fws, False, 0)
      |> (\(_, caught, sev) -> (caught, sev) )

sendPacketOptimized : Int -> Firewalls -> (Bool, Int)
sendPacketOptimized delay fws =
  let
    run : Int -> Layer -> (Bool, Int) -> (Bool, Int)
    run p layer (caught, sev) =
      if (p + delay) % ((layer.range - 1) * 2) == 0 then
        (True, sev + (layer.range * p))
      else
        (caught, sev)
  in
    Dict.foldl run (False, 0) fws

sendPacketInvisibleNaive : Firewalls -> Int
sendPacketInvisibleNaive fws =
  let
    isZeroState : Firewalls -> Bool
    isZeroState fws = fws
      |> Dict.values
      |> List.map .scanPos
      |> List.sum
      |> (\num -> num == 0)

    trySending : Int -> Firewalls -> Int
    trySending delay fws =
      if delay > 0 && isZeroState fws then
        Debug.crash ("Couldn't find a path...")
      else
        let
          (caught, _) =
            -- Debug.log ("*** sending packet after " ++ (toString delay) ++ " ps:") <|
            sendPacketNaive fws
        in
          if not caught then
            delay
          else
            trySending (delay + 1) (fwStep fws)
  in
    trySending 0 fws

sendPacketInvisibleOptimized : Firewalls -> Int
sendPacketInvisibleOptimized fws =
  let
    run : Int -> Int
    run delay =
      let
        (caught, _) = sendPacketOptimized delay fws
      in
        if caught then
          run (delay + 1)
        else
          delay
  in
    run 0

-- DEBUG
--------
firewallsToString : Int -> Firewalls -> String
firewallsToString packetPos fws =
  let
    maxDepth : Int
    maxDepth = fws
      |> Dict.keys
      |> List.maximum
      |> Maybe.withDefault 0

    maxRange : Int
    maxRange = fws
      |> Dict.values
      |> List.map .range
      |> List.maximum
      |> Maybe.withDefault 0

    layerPosToString : Int -> (Int, Maybe Layer) -> String
    layerPosToString p (layerPos, maybeLayer) =
      case maybeLayer of
        Just layer ->
          let
            (l, r) =
              if layerPos == packetPos && p == 0 then
                ("(", ")")
              else if p >= layer.range then
                (" ", " ")
              else
                ("[", "]")
          in
            if layer.scanPos == p then
              l ++ "X" ++ r
            else
              l ++ " " ++ r
        Nothing ->
          let
            (l, r) =
              if layerPos == packetPos && p == 0 then
                ("(", ")")
              else
                (".", ".")
          in
            l ++ "." ++ r

    lineToString : Int -> String
    lineToString p = List.range 0 maxDepth
      |> List.map (\i -> (i, Dict.get i fws))
      |> List.map (layerPosToString p)
      |> String.join " "

  in
    List.range 0 maxRange
      |> List.map lineToString
      |> String.join "\n"


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