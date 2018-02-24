module AdventOfCode2017 exposing (main)

import Day1
import Test
import Html exposing (..)

main : Html msg
main = div []
  [ h1 [] [ text "Advent of code 2017" ]
  , Test.test Day1.part1
  , Test.test Day1.part2
  ]

testMain : Html msg
testMain = ['1']
  |> String.fromList
  |> String.toInt
  |> toString
  |> text
