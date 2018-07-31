module Day21 exposing (part1, part2, transpose, split, join, rotateCcw, flipH, flipV)

import Dict exposing (Dict)
import TestRun


part1 : TestRun.Test
part1 =
  { title = "Day 21: Fractal Art - Part 1"
  , solver = solver1
  , testCases =
    [ ( bigInput, "147")
    ]
  }

part2 : TestRun.Test
part2 =
  { title = "Day 21: Fractal Art - Part 2"
  , solver = solver2
  , testCases =
    [ (bigInput, "1936582")
    ]
  }

-- SOLVER
---------
solver1 : String -> String
solver1 = solver 5

solver2 : String -> String
solver2 = solver 18

solver : Int -> String -> String
solver num input = input
  |> parseInput
  |> (\repl -> runner num repl initMatrix)
  |> countChar '#'
  |> toString

-- MODELS
---------

type alias Matrix a = List (List a)

initMatrix : Matrix Char
initMatrix =
  [ [ '.', '#', '.' ]
  , [ '.', '.', '#' ]
  , [ '#', '#', '#' ] ]

runner : Int -> Dict String (Matrix Char) -> Matrix Char -> Matrix Char
runner num replacements matrix =
  if num == 0 then
    matrix
  else
    let
      boxsize =
        if List.length matrix % 2 == 0
          then 2
          else 3
    in
      matrix
        |> split boxsize
        |> List.map (List.map (findReplacement replacements))
        |> join
        |> runner (num - 1) replacements

findReplacement : Dict String (Matrix Char) -> Matrix Char -> Matrix Char
findReplacement replacements matrix =
  case Dict.get (matrix2str matrix) replacements of
    Just to -> to
    _ -> matrix

matrix2str : Matrix Char -> String
matrix2str matrix = matrix
  |> List.map String.fromList
  |> String.join "/"

countChar : Char -> Matrix Char -> Int
countChar ch matrix = matrix -- List (List Char)
  |> List.map (List.filter (\c -> c == ch)) -- List (List Char)
  |> List.map (List.length) -- List Int
  |> List.sum -- Int

-- MATRIX HELPERS
-----------------
flipV : Matrix a -> Matrix a
flipV matrix = List.reverse matrix

flipH : Matrix a -> Matrix a
flipH matrix = List.map List.reverse matrix

rotateCcw : Int -> Matrix a -> Matrix a
rotateCcw n matrix =
  if n == 0 then
    matrix
  else matrix
  |> rotateCcw (n - 1)
  |> transpose
  |> flipV

transpose : Matrix a -> Matrix a
transpose matrix =
  let
    len = case matrix of
      x :: xs -> List.length x
      _ -> 0
    base = List.repeat len []
    toEnd a l = l ++ [a]

    f : List a -> Matrix a -> Matrix a
    f row m = List.map2 toEnd row m
  in
    List.foldl f base matrix

split : Int -> Matrix a -> Matrix (Matrix a)
split n matrix =
  matrix
    |> List.map (group n)
    |> group n
    |> List.map transpose

join : Matrix (Matrix a) -> Matrix a
join matrix =
  matrix
    |> List.map transpose
    |> List.concat
    |> List.map (List.concat)


-- HELPERS
----------
toTuple : List a -> (a, a)
toTuple l = case l of
  x :: y :: [] -> (x, y)
  _ -> Debug.crash("Incorrect input data")

group : Int -> List a -> List (List a)
group n l =
  if List.length l > 0 then
    (List.take n l) :: (group n (List.drop n l))
  else
    []

-- PARSER
---------
parseInput : String -> Dict String (Matrix Char)
parseInput input = input
  |> String.trim -- String
  |> String.split "\n" -- List String
  |> List.map parseRule -- List (List (Matrix, Matrix))
  |> List.concat -- List (Matrix, Matrix)
  |> Dict.fromList -- Dict (Matrix, Matrix)

parseRule : String -> List (String, Matrix Char)
parseRule rule = rule
  |> String.split " => "
  |> List.map parseMatrix
  |> toTuple
  |> multiplyRule


parseMatrix : String -> Matrix Char
parseMatrix input = input
  |> String.trim
  |> String.split "/"
  |> List.map String.toList

multiplyRule : (Matrix Char, Matrix Char) -> List (String, Matrix Char)
multiplyRule (from, to) = List.map (\ f -> (matrix2str (f from), to)) <|
  [ identity
  , flipH
  , flipV
  , rotateCcw 1
  , rotateCcw 1 << flipH
  , rotateCcw 1 << flipV
  , rotateCcw 2
  , rotateCcw 2 << flipH
  , rotateCcw 2 << flipV
  , rotateCcw 3
  , rotateCcw 3 << flipH
  , rotateCcw 3 << flipV
  ]

-- INPUT
--------
smallInput = """
../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#
"""

bigInput = """
../.. => .../.#./.#.
#./.. => .../#../#..
##/.. => #.#/.#./.#.
.#/#. => ##./##./...
##/#. => .##/###/#..
##/## => .##/#../##.
.../.../... => .#.#/###./##.#/###.
#../.../... => #.#./..#./..../#.#.
.#./.../... => #.##/..#./.#.#/####
##./.../... => ###./.#../####/##..
#.#/.../... => ...#/####/#.##/...#
###/.../... => .#../..#./#..#/..#.
.#./#../... => ###./.##./#.../..#.
##./#../... => #.#./...#/..../.###
..#/#../... => ..../..../##../#..#
#.#/#../... => ..#./#..#/.#../..##
.##/#../... => ##../.#.#/.##./...#
###/#../... => ..../#.../#..#/#..#
.../.#./... => ##.#/#.#./#.../#..#
#../.#./... => ..#./#.#./.##./....
.#./.#./... => ..##/#.../..../###.
##./.#./... => .#../...#/.##./.#.#
#.#/.#./... => ...#/#..#/.#../.###
###/.#./... => ###./.###/##.#/#.##
.#./##./... => ##.#/##../..##/..##
##./##./... => #.##/.###/.##./###.
..#/##./... => ##.#/.##./..##/####
#.#/##./... => ####/####/#.##/.#..
.##/##./... => ####/.#../####/#..#
###/##./... => #.#./..#./###./..#.
.../#.#/... => #.../..../.#../#.##
#../#.#/... => ..#./###./####/..#.
.#./#.#/... => #.##/.#../##.#/#.#.
##./#.#/... => ###./.###/###./##..
#.#/#.#/... => ...#/.##./.#.#/#.##
###/#.#/... => ####/#.../###./###.
.../###/... => ..##/#.##/.#../.#..
#../###/... => ..../.###/.#.#/...#
.#./###/... => #.##/.#.#/.#.#/.##.
##./###/... => #..#/.#.#/#.##/#.#.
#.#/###/... => #.../##../#.##/##.#
###/###/... => .#../.#../.###/..#.
..#/.../#.. => ...#/.##./.##./####
#.#/.../#.. => ##.#/##../#.#./.#..
.##/.../#.. => #..#/.##./####/.#..
###/.../#.. => ..../..../..##/..##
.##/#../#.. => ..##/.##./#..#/###.
###/#../#.. => ##.#/#..#/#.../#..#
..#/.#./#.. => #..#/##.#/.##./#..#
#.#/.#./#.. => .#../####/..##/#.##
.##/.#./#.. => ###./#..#/.##./###.
###/.#./#.. => ####/###./##../..##
.##/##./#.. => #.../####/...#/####
###/##./#.. => .#../#.##/.##./####
#../..#/#.. => .#../####/#.../....
.#./..#/#.. => .#.#/...#/.###/.#.#
##./..#/#.. => ..##/#..#/#..#/....
#.#/..#/#.. => .###/.#.#/.##./#.#.
.##/..#/#.. => ...#/#.##/#.../..##
###/..#/#.. => #.##/#.##/...#/#.##
#../#.#/#.. => #..#/..##/.#../.###
.#./#.#/#.. => #.##/..../.##./.#..
##./#.#/#.. => #.#./..#./.#.#/.#..
..#/#.#/#.. => ...#/#..#/###./##..
#.#/#.#/#.. => ##.#/##.#/.#.#/.#..
.##/#.#/#.. => #..#/#..#/##../.#..
###/#.#/#.. => #.##/..##/##.#/....
#../.##/#.. => ##.#/.##./...#/.#.#
.#./.##/#.. => .##./.###/###./.#.#
##./.##/#.. => #.#./#.##/..##/.#..
#.#/.##/#.. => ..#./.##./..##/.#..
.##/.##/#.. => ##../..##/#..#/#...
###/.##/#.. => ###./#..#/##.#/..#.
#../###/#.. => .###/#.../####/#.#.
.#./###/#.. => #.#./.###/#..#/....
##./###/#.. => ..#./.#.#/#.../#...
..#/###/#.. => ...#/..#./##../#..#
#.#/###/#.. => .#.#/###./.#../##..
.##/###/#.. => .#../###./..#./##..
###/###/#.. => .#../..##/#.../#...
.#./#.#/.#. => ##.#/..../##../.#..
##./#.#/.#. => #.../#.##/.###/#.##
#.#/#.#/.#. => ...#/..##/##.#/#.##
###/#.#/.#. => ...#/.#.#/###./#..#
.#./###/.#. => ...#/...#/##../#.##
##./###/.#. => ###./###./.#.#/..##
#.#/###/.#. => ..../#..#/..##/#..#
###/###/.#. => .#.#/.#.#/##../.###
#.#/..#/##. => .##./..#./##../....
###/..#/##. => ####/...#/.#.#/#...
.##/#.#/##. => ..#./...#/###./.#..
###/#.#/##. => ..../.#../.#../#.#.
#.#/.##/##. => .##./..../#.../.#.#
###/.##/##. => ..../#..#/...#/#...
.##/###/##. => #.##/##.#/#.../..#.
###/###/##. => .#../.###/###./##.#
#.#/.../#.# => .#.#/..../#..#/.#..
###/.../#.# => ##../#.##/##.#/..#.
###/#../#.# => .#.#/..../.#.#/.###
#.#/.#./#.# => ...#/..../##.#/#...
###/.#./#.# => ####/.###/#.#./#.##
###/##./#.# => #..#/.###/...#/###.
#.#/#.#/#.# => #.##/...#/.###/.##.
###/#.#/#.# => #.../.#.#/.#.#/.###
#.#/###/#.# => ##.#/##../###./#...
###/###/#.# => .##./.###/.#../..##
###/#.#/### => #.##/###./#..#/#..#
###/###/### => #.../..../#..#/#...
"""
