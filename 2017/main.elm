module AdventOfCode2017 exposing (main)

import Html exposing (..)

import Test
import Day1
import Day2
import Day3

main : Html msg
main = div []
  [ h1 [] [ text "Advent of code 2017" ]
  , Test.test Day3.part1
  , Test.test Day2.part1
  , Test.test Day2.part2
  , Test.test Day1.part1
  , Test.test Day1.part2
  ]

testMain : Html msg
testMain = ['1']
  |> String.fromList
  |> String.toInt
  |> toString
  |> text
