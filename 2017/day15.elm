module Day15 exposing (part1, part2)

import TestRun


part1 : TestRun.Test
part1 =
  { title = "Day 15: Dueling Generators - Part 1"
  , solver = solver
  , testCases = []
    -- [ ( "65,8921", "588")
    -- , ( "618,814", "577")
    -- ]
  }

part2 : TestRun.Test
part2 =
  { title = "Day 15: Dueling Generators - Part 2"
  , solver = identity
  , testCases =
    [ ("", "")
    ]
  }

-- SOLVER
---------
solver : String -> String
solver input = input
  |> parseInput -- (Int, Int)
  |> (\(a, b) -> (genNew a 16807, genNew b 48271)) -- (Generator, Generator)
  -- |> compareGens 10
  |> compareGens 40000000
  |> toString

type alias Generator =
  { current : Int
  , factor : Int
  }

genNew : Int -> Int -> Generator
genNew current factor =
  { current = current
  , factor = factor
  }

genStep : Generator -> Generator
genStep gen =
  { gen | current = (gen.current * gen.factor) % 2147483647 }

genLow16 : Generator -> Int
genLow16 gen = gen.current % 65536

compareGens : Int -> (Generator, Generator) -> Int
compareGens stepCount gens =
  let
    step : Int -> (Generator, Generator) -> Int -> Int
    step stepCount (genA, genB) cnt =
      if stepCount > 0 then
        let
          gensMatch = (genLow16 genA) == (genLow16 genB)
          newCnt = if gensMatch then cnt + 1 else cnt

          newGenA = genStep genA
          newGenB = genStep genB
        in
          -- Debug.log ("step: " ++ (toString (genA, genB)))<|
          step (stepCount - 1) (newGenA, newGenB) newCnt
      else
        cnt
  in
    step stepCount gens 0


-- PARSER
---------
parseInput : String -> (Int, Int)
parseInput input =
  let
    list2tuple : List Int -> (Int, Int)
    list2tuple l =
      case l of
        a :: b :: [] -> (a, b)
        _ -> Debug.crash ("Invalid input: " ++ (toString l))
  in
    input
      |> String.split "," -- List String
      |> List.map String.toInt -- List (Result Int)
      |> List.map Result.toMaybe -- List (Maybe Int)
      |> List.map (Maybe.withDefault 0) -- List Int
      |> list2tuple -- (Int, Int)
