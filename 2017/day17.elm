module Day17 exposing (part1, part2)

import TestRun
import Dict exposing (Dict)

part1 : TestRun.Test
part1 =
  { title = "Day 17: Spinlock - Part 1"
  , solver = solver1
  , testCases =
    [ ( "3,2017", "638")
    , ( "303,2017", "1971")
    ]
  }

part2 : TestRun.Test
part2 =
  { title = "Day 17: Spinlock - Part 2"
  , solver = solver2
  , testCases =
    [ ("3,10", "9")
    , ("303,50000000", "17202899")
    ]
  }

-- SOLVER
---------
solver1 : String -> String
solver1 input =
  let
    (stepCount, numRounds) = parseInput input
  in
    List.range 1 numRounds
      |> List.foldl (runRound stepCount) (0, ringNew)
      |> (\(curr, ring) -> unsafeGet numRounds ring)
      |> toString

solver2 : String -> String
solver2 input =
  let
    (stepCount, numRounds) = parseInput input

    folder : Int -> (Int, Int, Int) -> (Int, Int, Int)
    folder i (len, curr, result) =
      let
        newLen = len + 1
        newCurr = (curr + stepCount) % len + 1
        newResult = if newCurr == 1 then i else result
      in
        (newLen, newCurr, newResult)

    f : Int -> (Int, Int, Int) -> Int
    f num (len, curr, result) =
      if num < numRounds then
        f
          (num + 1)
          (folder num (len, curr, result))
      else
        result
  in
    toString <| f 1 (1, 0, -1)

type alias Ring = Dict Int Int

runRound : Int -> Int -> (Int, Ring) -> (Int, Ring)
runRound stepCount newVal (current, ring) =
  let
    nomrStepCount = stepCount % (Dict.size ring)

    f : Int -> Int -> Int
    f _ curr = ringNext ring curr

    newCurrent = List.foldl f current (List.range 1 stepCount)
  in
    (newVal, ringInsertRight newCurrent newVal ring)

ringNew : Ring
ringNew = Dict.empty
  |> Dict.insert 0 0

ringNext : Ring -> Int -> Int
ringNext ring item =
  unsafeGet item ring

unsafeGet : comparable -> Dict comparable a -> a
unsafeGet k dict =
  case Dict.get k dict of
    Just x -> x
    Nothing -> Debug.crash ("invalid index (" ++ (toString k) ++ ") in " ++ (toString dict))

ringInsertRight : Int -> Int -> Ring -> Ring
ringInsertRight index value ring = ring
  |> Dict.insert value (unsafeGet index ring)
  |> Dict.insert index value

-- PARSER
---------
parseInput : String -> (Int, Int)
parseInput input = input
  |> String.split ","
  |> List.map String.toInt
  |> List.map (Result.mapError (\e -> Debug.crash("Couldn't parse ints: " ++ (toString e) ++ " " ++ (toString input))))
  |> List.map (Result.withDefault 0)
  |> list2Tuple

list2Tuple : List Int -> (Int, Int)
list2Tuple l =
  case l of
    a :: b :: xs -> (a, b)
    _ -> Debug.crash ("Not enough values for tuple: " ++ (toString l))
