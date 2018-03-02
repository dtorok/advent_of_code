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
  , solver = solver2
  , testCases =
    [ (smallInput, "10")
    -- , (bigInput, "3897604") -- too long to run
    ]
  }

-- SOLVER
---------
solver1 : String -> String
solver1 input = input
  |> parseInput -- Firewalls
  |> sendPacket 0 -- (Int, Bool)
  |> Tuple.second -- Int
  |> toString -- String

solver2 : String -> String
solver2 input = input
  |> parseInput -- Firewalls
  |> sendPacketInvisible -- Int
  |> toString -- String

type alias Firewalls = Dict Int Int

sendPacket : Int -> Firewalls -> (Bool, Int)
sendPacket delay fws =
  let
    run : Int -> Int -> (Bool, Int) -> (Bool, Int)
    run p range (caught, sev) =
      let
        arrivesAt = p + delay
        roundtripTime = (range - 1) * 2
      in
        if arrivesAt % roundtripTime == 0 then
          (True, sev + (range * p))
        else
          (caught, sev)
  in
    Dict.foldl run (False, 0) fws

sendPacketInvisible : Firewalls -> Int
sendPacketInvisible fws =
  let
    run : Int -> Int
    run delay =
      let
        (caught, _) = sendPacket delay fws
      in
        if caught then
          run (delay + 1)
        else
          delay
  in
    run 0

-- PARSER
---------
parseInput : String -> Firewalls
parseInput input = input
  |> String.trim
  |> String.split "\n" -- List String
  |> List.map parseLine -- List (Int, Int)
  |> Dict.fromList

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