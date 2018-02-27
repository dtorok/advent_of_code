module AdventOfCode2017 exposing (main)

import Html exposing (..)

import TestRun
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10

main : Html msg
main = div []
  [ h1 [] [ text "Advent of code 2017" ]
  , TestRun.test Day10.part1
  , TestRun.test Day10.part2
  , TestRun.test Day9.part1
  , TestRun.test Day9.part2
  , TestRun.test Day8.part1
  , TestRun.test Day8.part2
  , TestRun.test Day7.part1
  , TestRun.test Day7.part2
  , TestRun.test Day6.part1
  , TestRun.test Day6.part2
  , TestRun.test Day5.part1
  , TestRun.test Day5.part2
  , TestRun.test Day4.part1
  , TestRun.test Day4.part2
  , TestRun.test Day3.part1
  , TestRun.test Day3.part2
  , TestRun.test Day2.part1
  , TestRun.test Day2.part2
  , TestRun.test Day1.part1
  , TestRun.test Day1.part2
  ]

testMain : Html msg
testMain = ['1']
  |> String.fromList
  |> String.toInt
  |> toString
  |> text
