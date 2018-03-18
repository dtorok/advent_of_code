module Day03 exposing (part1, part2)

import TestRun
import Dict exposing (Dict)

part1 : TestRun.Test
part1 =
  { title = "Day 3: Spiral Memory - Part 1"
  , solver = solver stepNumberUpdater calcDistance
  , testCases =
    [ ( "1", "0")
    , ( "12", "3")
    , ( "23", "2")
    , ( "1024", "31")
    -- , ( "312051", "430") -- too long to run
    ]
  }

part2 : TestRun.Test
part2 =
  { title = "Day 3: Spiral Memory - Part 2"
  , solver = solver neighborNumberUpdater getNumber
  , testCases =
    [ ( "312051", "312453")
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

type alias Cache =
  Dict String Model

type alias NumberUpdater =
  (Cache -> Model -> Model)

solver : NumberUpdater -> (Model -> Model -> Int) -> String -> String
solver numberUpdater transformer input =
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
    cache = Dict.empty |> addToCache initModel
    endModel = walkThrough numberUpdater cache initModel num
    result = transformer initModel endModel
  in
    toString result

stepNumberUpdater : Cache -> Model -> Model
stepNumberUpdater cache m =
    { m | number = m.number + 1 }

neighborNumberUpdater : Cache -> Model -> Model
neighborNumberUpdater cache m =
  let
    dirs =
      [ (  1,  0 )
      , (  1, -1 )
      , (  0, -1 )
      , ( -1, -1 )
      , ( -1,  0 )
      , ( -1,  1 )
      , (  0,  1 )
      , (  1,  1 ) ]

    neighborNumberSum = dirs -- directions
      |> List.map (\(vx, vy) -> (m.x + vx, m.y + vy)) -- coords
      |> List.map (\(x, y) -> makeCacheKey x y) -- cache keys
      |> List.filterMap (\key -> Dict.get key cache) -- neighbor models
      |> List.map (\model -> model.number) -- neighbor models' numbers
      |> List.sum -- sum of neighbor models' numbers
  in
    { m | number = neighborNumberSum }

walkThrough : NumberUpdater -> Cache -> Model -> Int -> Model
walkThrough numberUpdater cache model target =
  if model.number >= target then
    model
  else
    let
      newModel = model
        |> update
        |> numberUpdater cache
      key = makeCacheKey newModel.x newModel.y
      newCache = addToCache newModel cache
    in
      walkThrough numberUpdater newCache newModel target

makeCacheKey : Int -> Int -> String
makeCacheKey x y = (toString x) ++ "," ++ (toString y)

addToCache : Model -> Cache -> Cache
addToCache model cache =
  let
    key = makeCacheKey model.x model.y
  in
    Dict.insert key model cache

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
    newModel

calcDistance : Model -> Model -> Int
calcDistance m0 m1 =
  let
    dX = abs (m0.x - m1.x)
    dY = abs (m0.y - m1.y)
  in
    dX + dY

getNumber : Model -> Model -> Int
getNumber m0 m1 = m1.number

abs : Int -> Int
abs a = if a >= 0 then a else (-a)




















