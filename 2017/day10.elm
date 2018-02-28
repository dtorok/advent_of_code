module Day10 exposing (part1, part2)

import TestRun
import Array exposing (Array)
import Char
import Bitwise


part1 : TestRun.Test
part1 =
  { title = "Day 10: Knot Hash - Part 1"
  , solver = solver1
  , testCases =
    [ ( "5;3,4,1,5", "12" )
    , ( "256;88,88,211,106,141,1,78,254,2,111,77,255,90,0,54,205", "11375" )
    ]
  }

part2 : TestRun.Test
part2 =
  { title = "Day 10: Knot Hash - Part 2"
  , solver = solver2
  , testCases =
    [ ("", "a2582a3a0e66e6e86e3812dcb672a272")
    , ("AoC 2017", "33efeb34ea91902bb2f59c9920caa6cd")
    , ("1,2,3", "3efbe78a8d82f29979031a4aa0b16a9d")
    , ("1,2,4", "63960835bcdc130f0b66d7ff4f6a5a8e")
    , ("88,88,211,106,141,1,78,254,2,111,77,255,90,0,54,205", "e0387e2ad112b7c2ef344e44885fe4d8")
    ]
  }

-- SOLVER
---------
solver1 : String -> String
solver1 input =
  let
    (len, data) = parseInput input
    ring = ringNew len
  in ring
  |> knotRound 0 0 data -- Ring
  |> Array.toList -- List Int
  |> List.take 2 -- List Int
  |> List.product -- Int
  |> toString -- String

solver2 : String -> String
solver2 input =
  let
    data =
      input
        |> parseAscii -- LIst Int
        |> (\x -> x ++ [17, 31, 73, 47, 23]) -- List Int
        |> List.repeat 64 -- List (List Int)
        |> List.concat -- List Int
    ring = ringNew 256
  in ring
    |> knotRound 0 0 data -- Ring
    |> Array.toList -- List Int
    |> encode
    |> List.map toHex
    |> String.concat

knotRound : Int -> Int -> List Int -> Ring -> Ring
knotRound pos skip data ring =
  case data of
    x :: xs ->
      let
        newPos = pos + x + skip
        newSkip = skip + 1
        newRing = ringDataReverse pos x ring
      in
        knotRound newPos newSkip xs newRing
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

-- HELPERS
----------
encode : List Int -> List Int
encode data =
  if (List.length data) == 0 then
    []
  else
    let
        starter = Bitwise.xor 0 0
        item = data
          |> List.take 16
          |> List.foldl Bitwise.xor starter
        rest = List.drop 16 data
    in
      item :: (encode rest)

toHex : Int -> String
toHex num =
  let
    charMap = Array.fromList ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']

    num1 = num % 16
    num10 = num // 16

    hex1 = Array.get num1 charMap |> Maybe.withDefault '0'
    hex10 = Array.get num10 charMap |> Maybe.withDefault '0'
  in
    String.fromList (hex10 :: hex1 :: [])

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

parseAscii : String -> List Int
parseAscii input = input
  |> String.toList -- List Char
  |> List.map Char.toCode -- List Int

