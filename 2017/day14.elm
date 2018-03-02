module Day14 exposing (part1, part2)

import TestRun
import Day10 exposing (knotHash)

part1 : TestRun.Test
part1 =
  { title = "Day 14: Disk Defragmentation - Part 1"
  , solver = solver
  , testCases =
    [ ( "flqrgnkx", "8108")
    , ( "wenycdww", "8226")
    ]
  }

part2 : TestRun.Test
part2 =
  { title = "Day 14: Disk Defragmentation - Part 2"
  , solver = identity
  , testCases =
    [ ("", "")
    ]
  }


-- SOLVER
---------
solver : String -> String
solver input = List.range 0 127
  |> List.map (\i -> input ++ "-" ++ (toString i)) -- List String
  |> List.map knotHash -- List String
  |> List.map String.toList -- List (List Char)
  |> List.map (List.map hexToNumBits) -- List (List Int)
  |> List.map (List.sum) -- List Int
  |> List.sum -- Int
  |> toString -- String


hexToNumBits : Char -> Int
hexToNumBits ch =
  case ch of
    '0' -> 0 -- '0000'
    '1' -> 1 -- '0001'
    '2' -> 1 -- '0010'
    '3' -> 2 -- '0011'
    '4' -> 1 -- '0100'
    '5' -> 2 -- '0101'
    '6' -> 2 -- '0110'
    '7' -> 3 -- '0111'
    '8' -> 1 -- '1000'
    '9' -> 2 -- '1001'
    'a' -> 2 -- '1010'
    'b' -> 3 -- '1011'
    'c' -> 2 -- '1100'
    'd' -> 3 -- '1101'
    'e' -> 3 -- '1110'
    'f' -> 4 -- '1111'
    _ -> Debug.crash ("Invalid character: " ++ (toString ch))