module Day14 exposing (part1, part2)

import TestRun
import Array exposing (Array)
import Dict exposing (Dict)

import Day10 exposing (knotHash)

part1 : TestRun.Test
part1 =
  { title = "Day 14: Disk Defragmentation - Part 1"
  , solver = solver1
  , testCases =
    [ ( "flqrgnkx", "8108")
    , ( "wenycdww", "8226")
    ]
  }

part2 : TestRun.Test
part2 =
  { title = "Day 14: Disk Defragmentation - Part 2"
  , solver = solver2
  , testCases =
    [ ("flqrgnkx", "1242")
    , ("wenycdww", "1128")
    ]
  }


-- SOLVER
---------
solver1 : String -> String
solver1 input = List.range 0 127
  |> List.map (\i -> input ++ "-" ++ (toString i)) -- List String
  |> List.map knotHash -- List String
  |> List.map String.toList -- List (List Char)
  |> List.map (List.map hexToNumBits) -- List (List Int)
  |> List.map (List.sum) -- List Int
  |> List.sum -- Int
  |> toString -- String

solver2 : String -> String
solver2 input = List.range 0 127
  |> List.map (\i -> input ++ "-" ++ (toString i)) -- List String
  |> List.map knotHash -- List String
  |> List.map String.toList -- List (List Char)
  |> List.map (List.map hexToBits) -- List (List (List Bool))
  |> List.map (List.concat) -- List (List Bool)
  |> discFromList -- Disc
  |> countRegions
  |> toString

type alias Disc = Array (Array Bool)

discFromList : List (List Bool) -> Disc
discFromList lists = lists
  |> List.map Array.fromList -- List (Array Bool)
  |> Array.fromList -- Array (Array Bool) == Disc

discUsedAt : (Int, Int) -> Disc -> Bool
discUsedAt (r, c) disc = disc
  |> Array.get r -- Maybe (Array Bool)
  |> Maybe.andThen (Array.get c) -- Maybe Bool
  |> Maybe.withDefault False

discCoords : Disc -> List (Int, Int)
discCoords disc =
  let
    mapRow : Int -> Int -> Bool -> (Int, Int)
    mapRow r c _ = (r, c)

    mapDisc : Int -> Array Bool -> Array (Int, Int)
    mapDisc r row = row
      |> Array.indexedMap (mapRow r)
  in
    disc
      |> Array.indexedMap mapDisc
      |> Array.map Array.toList
      |> Array.toList
      |> List.concat

countRegions : Disc -> Int
countRegions disc =
  let
    visitRegion : Int -> (Int, Int) -> Dict (Int, Int) Int -> Dict (Int, Int) Int
    visitRegion num (r, c) visited =
      if Dict.member (r, c) visited then
        visited
      else if not (discUsedAt (r, c) disc) then
        visited
      else
        visited
          |> Dict.insert (r, c) num
          |> visitRegion num (r - 1, c)
          |> visitRegion num (r, c + 1)
          |> visitRegion num (r + 1, c)
          |> visitRegion num (r, c - 1)


    countRegions : (Int, Int) -> (Int, Dict (Int, Int) Int) -> (Int, Dict (Int, Int) Int)
    countRegions (r, c) (cnt, visited) =
      if not (discUsedAt (r, c) disc) then
        (cnt, visited)
      else if Dict.member (r, c) visited then
        (cnt, visited)
      else
        ( cnt + 1, visitRegion (cnt + 1) (r, c) visited)

    debug : (Int, Dict (Int, Int) Int) -> (Int, Dict (Int, Int) Int)
    debug (num, visited) =
      Debug.log ("countRegions2:\n" ++ (printRegions disc visited)) (num, visited)
  in
    disc
      |> discCoords
      |> List.foldl countRegions (0, Dict.empty)
      |> Tuple.first

hexToBits : Char -> List Bool
hexToBits ch =
  let
    bits =
      case ch of
        '0' -> "0000"
        '1' -> "0001"
        '2' -> "0010"
        '3' -> "0011"
        '4' -> "0100"
        '5' -> "0101"
        '6' -> "0110"
        '7' -> "0111"
        '8' -> "1000"
        '9' -> "1001"
        'a' -> "1010"
        'b' -> "1011"
        'c' -> "1100"
        'd' -> "1101"
        'e' -> "1110"
        'f' -> "1111"
        _ -> Debug.crash ("Invalid character: " ++ (toString ch))
  in
    bits
      |> String.toList
      |> List.map (\c -> c == '1')

hexToNumBits : Char -> Int
hexToNumBits ch = ch
  |> hexToBits -- List Bool
  |> List.filter identity
  |> List.length



-- DEBUG
--------
printRegions : Disc -> Dict (Int, Int) Int -> String
printRegions disc visited =
  let
    printCell : Int -> Int -> Bool -> String
    printCell r c used =
      if not used then
        " . "
      else
        case Dict.get (r, c) visited of
          Nothing -> Debug.crash ("A coord that's used but not visited?? (" ++ (toString r) ++ ", " ++ (toString c) ++ ")")
          Just num -> " " ++ (toString num) ++ " "

    printRow : Int -> Array Bool -> String
    printRow r row = row
      |> Array.indexedMap (printCell r) -- Array String
      |> Array.toList -- List String
      |> String.join "\t" -- String
  in
    disc
      |> Array.indexedMap printRow -- Array String
      |> Array.toList -- List String
      |> String.join "\n"
