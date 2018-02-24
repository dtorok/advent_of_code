module Day6 exposing (part1, part2)

import TestRun
import Regex
import Dict exposing (Dict)
import Array exposing (Array)

part1 : TestRun.Test
part1 =
  { title = "Day 6: Memory Reallocation - Part 1"
  , solver = solver steps
  , testCases =
    [ ( smallInput, "5")
    , ( bigInput, "14029")
    ]
  }

part2 : TestRun.Test
part2 =
  { title = "Day 6: Memory Reallocation - Part 2"
  , solver = solver loopSize
  , testCases =
    [ ( smallInput, "4")
    , ( bigInput, "2765")
    ]
  }


-- SOLVER
---------
solver : ((Int, Int) -> Int) -> String -> String
solver resultTransformer input = input
  |> parseBlocks -- Array Int
  |> runRedistribution 0 Dict.empty -- (Int, Int)
  |> resultTransformer
  |> toString

steps : (Int, Int) -> Int
steps (cachedCounter, counter) = counter

loopSize : (Int, Int) -> Int
loopSize (cachedCounter, counter) = counter - cachedCounter


-- HELPERS
----------
runRedistribution : Int -> Dict String Int -> Array Int -> (Int, Int)
runRedistribution counter cache blocks =
  case Dict.get (toString blocks) cache of
    Just cachedCounter -> (cachedCounter, counter)
    Nothing ->
      let
        (fullPos, fullValue) = findFullestBlock blocks
        newBlocks =
          redistribute
            (fullPos + 1)
            fullValue
            (Array.set fullPos 0 blocks)
      in
        runRedistribution
          (counter + 1)
          (Dict.insert (toString blocks) counter cache)
          newBlocks

findFullestBlock : Array Int -> (Int, Int)
findFullestBlock blocks =
  let
      f : Int -> Int -> Int -> List Int -> (Int, Int)
      f currMinPos currMinVal pos bl =
        case bl of
          x :: xs ->
            if currMinVal == -1 || currMinVal < x
            then f pos x (pos + 1) xs
            else f currMinPos currMinVal (pos + 1) xs
          [] ->
            (currMinPos, currMinVal)
  in
    f -1 -1 0 (Array.toList blocks)

redistribute : Int -> Int -> Array Int -> Array Int
redistribute pos value blocks =
  if value <= 0 then blocks
  else
    let
      len = Array.length blocks
      currPos = if pos >= len then 0 else pos
      currVal = blocks
        |> Array.get currPos
        |> Maybe.withDefault 0
    in
      redistribute
        (currPos + 1)
        (value - 1)
        (Array.set currPos (currVal + 1) blocks)

-- PARSER
---------
parseBlocks : String -> Array Int
parseBlocks input = input
  |> Regex.split Regex.All (Regex.regex "\\s+") -- List String
  |> List.map String.toInt -- List Result
  |> List.filterMap Result.toMaybe -- List Int
  |> Array.fromList -- Array Int


-- INPUT
--------
smallInput : String
smallInput = "0 2 7 0"

bigInput : String
bigInput = "10 3 15 10  5  15  5  15  9  2  5  8  5  2  3  6"
