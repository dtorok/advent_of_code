module Day21 exposing (part1, part2, transpose, split, join, rotateCcw, flipH, flipV)

import TestRun


part1 : TestRun.Test
part1 =
  { title = "Day 21: Fractal Art - Part 1"
  , solver = solver
  , testCases =
    [ ( bigInput, "147")
    ]
  }

part2 : TestRun.Test
part2 =
  { title = "Day 21: Fractal Art - Part 2"
  , solver = identity
  , testCases =
    [ ("", "")
    ]
  }

-- SOLVER
---------
solver : String -> String
solver input = input
  |> parseInput
  |> (\repl -> solver1 5 repl initMatrix)
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

solver1 : Int -> List (Matrix a, Matrix a)-> Matrix a -> Matrix a
solver1 num replacements matrix =
  if num == 0 then
    matrix
  else
    let
      -- newmatrix = solver1 matrix (num - 1) replacements
      boxsize =
        if List.length matrix % 2 == 0
          then 2
          else 3
    in
      matrix
        |> split boxsize
        |> List.map (List.map (findReplacement replacements))
        |> join
        |> solver1 (num - 1) replacements

findReplacement : List (Matrix a, Matrix a) -> Matrix a -> Matrix a
findReplacement replacements matrix =
  case replacements of
    (from, to) :: xs ->
      if testReplacement from matrix then
        to
      else
        findReplacement xs matrix
    [] -> matrix

testReplacement : Matrix a -> Matrix a -> Bool
testReplacement ref matrix = False
  || matrix == (ref)
  || matrix == (ref |> flipH)
  || matrix == (ref |> flipV)
  || matrix == (ref |> rotateCcw 1)
  || matrix == (ref |> rotateCcw 1 |> flipH)
  || matrix == (ref |> rotateCcw 1 |> flipV)
  || matrix == (ref |> rotateCcw 2)
  || matrix == (ref |> rotateCcw 2 |> flipH)
  || matrix == (ref |> rotateCcw 2 |> flipV)
  || matrix == (ref |> rotateCcw 3)
  || matrix == (ref |> rotateCcw 3 |> flipH)
  || matrix == (ref |> rotateCcw 3 |> flipV)

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
parseInput : String -> List (Matrix Char, Matrix Char)
parseInput input = input
  |> String.trim
  |> String.split "\n"
  |> List.map parseRule

parseRule : String -> (Matrix Char, Matrix Char)
parseRule rule = rule
  |> String.split " => "
  |> List.map parseMatrix
  |> toTuple

parseMatrix : String -> Matrix Char
parseMatrix input = input
  |> String.trim
  |> String.split "/"
  |> List.map String.toList

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
