module Day21 exposing (part1, part2, transpose, split, join, rotateCcw, flipH, flipV)

import TestRun


part1 : TestRun.Test
part1 =
  { title = "Day 21: Fractal Art - Part 1"
  , solver = solver
  , testCases =
    [ ( "?", "?")
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
solver = identity

-- MODELS
---------

type alias Matrix a = List (List a)

-- MATRIX HELPERS
-----------------
flipV : Matrix a -> Matrix a
flipV matrix = List.reverse matrix

flipH : Matrix a -> Matrix a
flipH matrix = List.map List.reverse matrix

rotateCcw : Matrix a -> Matrix a
rotateCcw matrix = matrix
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
parseMatricies : String -> (Matrix Char, Matrix Char)
parseMatricies input = input
  |> String.split " => "
  |> List.map parseMatrix
  |> toTuple

parseMatrix : String -> Matrix Char
parseMatrix input = input
  |> String.trim
  |> String.split "/"
  |> List.map String.toList
