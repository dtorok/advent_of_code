module Day17 exposing (part1, part2)

import TestRun
import Dict exposing (Dict)

part1 : TestRun.Test
part1 =
  { title = "Day 17: Spinlock - Part 1"
  , solver = solver
  , testCases =
    [ ( "3", "638")
    , ( "303", "1971")
    ]
  }

part2 : TestRun.Test
part2 =
  { title = "Day 17: Spinlock - Part 2"
  , solver = identity
  , testCases =
    [ ("", "")
    ]
  }

-- SOLVER
---------
solver : String -> String
solver input =
  let
    stepCount : Int
    stepCount = Result.withDefault 0 (String.toInt input)
  in
    List.range 1 2017
      |> List.foldl (runRound stepCount) (0, ringNew)
      |> (\(curr, ring) -> unsafeGet 2017 ring)
      |> toString


type alias Ring = Dict Int Int

runRound : Int -> Int -> (Int, Ring) -> (Int, Ring)
runRound stepCount newVal (current, ring) =
  let
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
