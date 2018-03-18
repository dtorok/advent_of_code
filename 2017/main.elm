module AdventOfCode2017 exposing (main)

import Html exposing (..)

import TestRun
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18

main : Html msg
main = div []
  [ h1 [] [ text "Advent of code 2017" ]
  , TestRun.test Day18.part1
  , TestRun.test Day18.part2
  , TestRun.test Day17.part1
  , TestRun.test Day17.part2
  , TestRun.test Day16.part1
  , TestRun.test Day16.part2
  , TestRun.test Day15.part1
  , TestRun.test Day15.part2
  , TestRun.test Day14.part1
  , TestRun.test Day14.part2
  , TestRun.test Day13.part1
  , TestRun.test Day13.part2
  , TestRun.test Day12.part1
  , TestRun.test Day12.part2
  , TestRun.test Day11.part1
  , TestRun.test Day11.part2
  , TestRun.test Day10.part1
  , TestRun.test Day10.part2
  , TestRun.test Day09.part1
  , TestRun.test Day09.part2
  , TestRun.test Day08.part1
  , TestRun.test Day08.part2
  , TestRun.test Day07.part1
  , TestRun.test Day07.part2
  , TestRun.test Day06.part1
  , TestRun.test Day06.part2
  , TestRun.test Day05.part1
  , TestRun.test Day05.part2
  , TestRun.test Day04.part1
  , TestRun.test Day04.part2
  , TestRun.test Day03.part1
  , TestRun.test Day03.part2
  , TestRun.test Day02.part1
  , TestRun.test Day02.part2
  , TestRun.test Day01.part1
  , TestRun.test Day01.part2
  ]

testMain : Html msg
testMain = ['1']
  |> String.fromList
  |> String.toInt
  |> toString
  |> text
