module Day10 exposing (part1, part2)

import TestRun
import Array exposing (Array)


part1 : TestRun.Test
part1 =
  { title = "Day 10: Knot Hash - Part 1"
  , solver = solver
  , testCases =
    [ ( "5;3,4,1,5", "12" )
    , ( "256;88,88,211,106,141,1,78,254,2,111,77,255,90,0,54,205", "11375" )
    ]
  }

part2 : TestRun.Test
part2 =
  { title = "Day 10: Knot Hash - Part 2"
  , solver = identity
  , testCases =
    [ ("?", "?")
    ]
  }

-- SOLVER
---------
solver : String -> String
solver input =
  let
    (len, data) = parseInput input
    ring = ringNew len
  in ring
  |> solve 0 0 data -- Ring
  |> Array.toList -- List Int
  |> List.take 2 -- List Int
  |> List.product -- Int
  |> toString -- String


solve : Int -> Int -> List Int -> Ring -> Ring
solve pos skip data ring =
  case data of
    x :: xs ->
      let
        newPos = pos + x + skip
        newSkip = skip + 1
        newRing = ringDataReverse pos x ring
      in
        solve newPos newSkip xs newRing
    [] -> ring

-- RING
-------
type alias Ring = Array Int

ringNew : Int -> Ring
ringNew size = List.range 0 (size - 1) |> Array.fromList

ringDataGet : Int -> Ring -> Int
ringDataGet index data =
  let i = ringDataNormIndex data index
  in data
    |> Array.get i
    |> Maybe.withDefault -1

ringDataSet : Int -> Int -> Ring -> Ring
ringDataSet index value data =
  let i = ringDataNormIndex data index
  in Array.set i value data

ringDataSwap : Int -> Int -> Ring -> Ring
ringDataSwap i1 i2 data = data
  |> ringDataSet i1 (ringDataGet i2 data)
  |> ringDataSet i2 (ringDataGet i1 data)

ringDataNormIndex : Ring -> Int -> Int
ringDataNormIndex data index = index % (Array.length data)

ringDataReverse : Int -> Int -> Ring -> Ring
ringDataReverse index len data =
  if len <= 1 then
    data
  else
    data
      |> ringDataSwap index (index + len - 1)
      |> ringDataReverse (index + 1) (len - 2)

-- PARSER
---------
parseInput : String -> (Int, List Int)
parseInput input =
  case String.split ";" input of
    len :: data :: [] ->
      (parseLength len, parseData data)
    _ ->
      (0, [])

parseLength : String -> Int
parseLength len = len
  |> String.toInt -- Maybe Int
  |> Result.withDefault 0 -- Int

parseData : String -> List Int
parseData input = input
  |> String.split "," -- List String
  |> List.map String.toInt -- List (Result Int)
  |> List.map (Result.mapError (\e -> Debug.crash "Unable to parse number?" ++ (toString e))) -- List (Result Int)
  |> List.filterMap Result.toMaybe -- List Int

