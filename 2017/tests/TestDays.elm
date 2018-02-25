module TestDays exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, test, describe)

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import TestRun


runTest : TestRun.Test -> Test
runTest t =
    let
        run : (String, String) -> Test
        run (input, expected) =
            let
                result = t.solver input
            in
                test input <| \_ -> Expect.equal result expected
    in
        describe t.title (List.map run t.testCases)

suite : Test
suite =
    describe "Running tests for each days"
        ( List.map runTest
            [ Day1.part1
            , Day1.part2
            , Day2.part1
            , Day2.part2
            , Day3.part1
            , Day3.part2
            , Day4.part1
            , Day4.part2
            , Day5.part1
            , Day5.part2
            , Day6.part1
            , Day6.part2
            , Day7.part1
            , Day7.part2
            ]
        )

