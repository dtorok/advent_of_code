module Day3 exposing (part1)

import Test

part1 : Test.Test
part1 =
  { title = "Day 3: Spiral Memory - Part 1"
  , solver = solver
  , testCases =
    [ ( "1", "0")
    , ( "12", "3")
    , ( "23", "2")
    , ( "1024", "31")
    , ( "312051", "430")
    ]
  }

-- SOLVER
---------

type Dir
  = Up
  | Left
  | Down
  | Right

type alias Model =
  { x : Int
  , y : Int
  , round : Int
  , dir : Dir
  , number : Int
  , step : Int
}

solver : String -> String
solver input =
  let
    num = String.toInt input |> Result.withDefault 0
    initModel =
      { x = 0
      , y = 0
      , round = 1
      , step = 0
      , dir = Right
      , number = 1
    }
    endModel = walkThrough initModel num
    distance = calcDistance initModel endModel
  in
    toString distance

walkThrough : Model -> Int -> Model
walkThrough model target =
  if model.number == target then
    model
  else
    walkThrough (update model) target

update : Model -> Model
update m =
  let
    len = m.round * 2 - 1
    onSide = m.step < len - 1
    newModel =
      case m.dir of
        Up ->
          if onSide then
            { m
              | y = m.y - 1
              , step = m.step + 1 }
          else
            { m
              | x = m.x - 1
              , step = 1
              , dir = Left }
        Left ->
          if onSide then
            { m
              | x = m.x - 1
              , step = m.step + 1 }
          else
            { m
              | y = m.y + 1
              , step = 1
              , dir = Down }
        Down ->
          if onSide then
            { m
              | y = m.y + 1
              , step = m.step + 1 }
          else
            { m
              | x = m.x + 1
              , step = 1
              , dir = Right }
        Right ->
          if onSide then
            { m
              | x = m.x + 1
              , step = m.step + 1 }
          else
            { m
              | x = m.x + 1
              , step = 1
              , dir = Up
              , round = m.round + 1 }
  in
    { newModel | number = newModel.number + 1 }

calcDistance : Model -> Model -> Int
calcDistance mA mB =
  let
    dX = abs (mA.x - mB.x)
    dY = abs (mA.y - mB.y)
  in
    dX + dY

abs : Int -> Int
abs a = if a >= 0 then a else (-a)




















