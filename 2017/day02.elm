module Day02 exposing (part1, part2)

import TestRun
import Regex


part1 : TestRun.Test
part1 =
  { title = "Day 2: Corruption Checksum - Part 1"
  , solver = solver calcMaxDiff
  , testCases =
    [ ( smallMatrixPart1, "18")
    , ( bigMatrix, "45158")
    ]
  }

part2 : TestRun.Test
part2 =
  { title = "Day 2: Corruption Checksum - Part 2"
  , solver = solver calcEvenlyDividable
  , testCases =
    [ ( smallMatrixPart2, "9")
    , ( bigMatrix, "294")
    ]
  }


-- SOLVERS
----------

solver : (List Int -> Int) -> String -> String
solver calc input = input
  |> parseMatrix
  |> List.map calc
  |> List.sum
  |> toString

calcMaxDiff : List Int -> Int
calcMaxDiff l =
  let
    max = (List.maximum l) |> Maybe.withDefault 0
    min = (List.minimum l) |> Maybe.withDefault 0
  in
    max - min

tryDivide : Int -> Int -> Maybe Int
tryDivide x y =
  let
    (s, b) =
      if x < y then (x, y)
      else (y, x)
  in
    if b % s == 0 then
      Just (b // s)
    else
      Nothing

calcEvenlyDividable : List Int -> Int
calcEvenlyDividable l =
  case l of
    x :: xs ->
      let results = xs
          |> List.filterMap (tryDivide x)
      in
        case results of
          x :: xs -> x
          [] -> calcEvenlyDividable xs
    [] -> -1 -- shouldn't happen by description


-- PARSER
---------

parseCell : String -> Int
parseCell cell = cell
  |> String.toInt
  |> Result.withDefault 0

parseRow : String -> List Int
parseRow row = row
  |> String.trim
  |> Regex.split (Regex.All) (Regex.regex "\\s+")
  |> List.map parseCell

parseMatrix : String -> List (List Int)
parseMatrix matrix = matrix
  |> String.trim
  |> String.split "\n"
  |> List.map parseRow


-- TEST DATA
------------

smallMatrixPart1 : String
smallMatrixPart1 = """
5 1 9 5
7 5 3
2 4 6 8
"""

smallMatrixPart2 : String
smallMatrixPart2 = """
5 9 2 8
9 4 7 3
3 8 6 5
"""

bigMatrix : String
bigMatrix = """
86 440 233 83 393 420 228 491 159 13 110 135 97 238 92 396
3646 3952 3430 145 1574 2722 3565 125 3303 843 152 1095 3805 134 3873 3024
2150 257 237 2155 1115 150 502 255 1531 894 2309 1982 2418 206 307 2370
1224 343 1039 126 1221 937 136 1185 1194 1312 1217 929 124 1394 1337 168
1695 2288 224 2667 2483 3528 809 263 2364 514 3457 3180 2916 239 212 3017
827 3521 127 92 2328 3315 1179 3240 695 3144 3139 533 132 82 108 854
1522 2136 1252 1049 207 2821 2484 413 2166 1779 162 2154 158 2811 164 2632
95 579 1586 1700 79 1745 1105 89 1896 798 1511 1308 1674 701 60 2066
1210 325 98 56 1486 1668 64 1601 1934 1384 69 1725 992 619 84 167
4620 2358 2195 4312 168 1606 4050 102 2502 138 135 4175 1477 2277 2226 1286
5912 6261 3393 431 6285 3636 4836 180 6158 6270 209 3662 5545 204 6131 230
170 2056 2123 2220 2275 139 461 810 1429 124 1470 2085 141 1533 1831 518
193 281 2976 3009 626 152 1750 1185 3332 715 1861 186 1768 3396 201 3225
492 1179 154 1497 819 2809 2200 2324 157 2688 1518 168 2767 2369 2583 173
286 2076 243 939 399 451 231 2187 2295 453 1206 2468 2183 230 714 681
3111 2857 2312 3230 149 3082 408 1148 2428 134 147 620 128 157 492 2879
"""
