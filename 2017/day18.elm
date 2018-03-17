module Day18 exposing (part1, part2)

import TestRun

import Array exposing (Array)
import Dict exposing (Dict)


part1 : TestRun.Test
part1 =
  { title = "Day 18: Duet - Part 1"
  , solver = solver
  , testCases =
    [ ( smallInput, "4")
    , ( bigInput, "8600")
    ]
  }

part2 : TestRun.Test
part2 =
  { title = "Day 18: Duet - Part 2"
  , solver = identity
  , testCases =
    [ ("", "")
    ]
  }

-- SOLVER
---------
solver : String -> String
solver input = input
  |> parseInput
  |> newEx
  |> run
  |> .recovered
  |> Maybe.withDefault -1
  |> toString

type alias Reg = String
type RegOrInt = Reg Reg | Int Int
type Instruction
  = Set Reg RegOrInt
  | Add Reg RegOrInt
  | Mul Reg RegOrInt
  | Mod Reg RegOrInt
  | Jgz Reg RegOrInt
  | Sound RegOrInt
  | Recover Reg

type alias Executor =
  { instrs : Array Instruction
  , pc : Int
  , regs : Dict Reg Int
  , sound : Maybe Int
  , recovered : Maybe Int
  }

run : Executor -> Executor
run ex =
  if isJust ex.recovered then
    ex
  else
    case Array.get ex.pc ex.instrs of
      Just instr ->
        run (execute instr ex)
      Nothing ->
        ex

execute : Instruction -> Executor -> Executor
execute instruction ex =
  case instruction of
    Set reg roi ->
      { ex
      | regs = Dict.insert reg (get roi ex) ex.regs
      , pc = ex.pc + 1 }
    Add reg roi ->
      { ex
      | regs = regChg reg roi (+) ex
      , pc = ex.pc + 1 }
    Mul reg roi ->
      { ex
      | regs = regChg reg roi (*) ex
      , pc = ex.pc + 1 }
    Mod reg roi ->
      { ex
      | regs = regChg reg roi (%) ex
      , pc = ex.pc + 1 }
    Jgz reg roi ->
      if regGet reg ex > 0 then
        { ex
        | pc = ex.pc + (get roi ex) }
      else
        { ex
        | pc = ex.pc + 1 }
    Sound roi ->
      { ex
      | sound = Just (get roi ex)
      , pc = ex.pc + 1 }
    Recover reg ->
      if regGet reg ex > 0 then
        { ex
        | recovered = ex.sound
        , pc = ex.pc + 1 }
      else
        { ex
        | pc = ex.pc + 1 }

get : RegOrInt -> Executor -> Int
get regOrInt ex =
  case regOrInt of
    Reg reg -> regGet reg ex
    Int int -> int

regGet : Reg -> Executor -> Int
regGet reg ex =
  Maybe.withDefault 0 (Dict.get reg ex.regs)

regSet : Reg -> Int -> Executor -> Executor
regSet reg val ex =
  { ex | regs = Dict.insert reg val ex.regs }

regChg : Reg -> RegOrInt -> (Int -> Int -> Int) -> Executor -> Dict Reg Int
regChg reg roi f ex =
  let
    regVal = regGet reg ex
    roiVal = get roi ex
    result = f regVal roiVal
  in
    Dict.insert reg result ex.regs


newEx : Array Instruction -> Executor
newEx instrs =
  { instrs = instrs
  , pc = 0
  , regs = Dict.empty
  , sound = Nothing
  , recovered = Nothing
  }

isJust : Maybe a -> Bool
isJust a =
  case a of
    Just _ -> True
    Nothing -> False

-- PARSER
---------
parseInput : String -> Array Instruction
parseInput input = input
  |> String.trim -- String
  |> String.split "\n" -- List String
  |> List.map String.trim -- List String
  |> List.map parseInstruction -- List Instruction
  |> Array.fromList -- Array Instruction

parseInstruction : String -> Instruction
parseInstruction input =
  let
    cmd = input
      |> String.left 3 -- String
    args = input
      |> String.dropLeft 4 -- String
      |> String.split " " -- List String
  in
    case String.left 3 input of
      "set" -> args |> with2ops Set reg regOrInt
      "add" -> args |> with2ops Add reg regOrInt
      "mul" -> args |> with2ops Mul reg regOrInt
      "mod" -> args |> with2ops Mod reg regOrInt
      "jgz" -> args |> with2ops Jgz reg regOrInt
      "snd" -> args |> with1op Sound regOrInt
      "rcv" -> args |> with1op Recover reg
      _ -> Debug.crash ("invalid input: " ++ input)

reg : String -> String
reg = identity

int : String -> Int
int s = crashIfError (String.toInt s)

regOrInt : String -> RegOrInt
regOrInt s =
  case String.toInt s of
    Ok num -> Int num
    Err _ -> Reg s

with1op : (a -> Instruction) -> (String -> a) -> List String -> Instruction
with1op constructor mapperA args =
  case args of
    a :: [] -> constructor (mapperA a)
    _ -> Debug.crash ("Invalid parameter count: " ++ (toString constructor) ++ " " ++ (toString args))

with2ops : (a -> b -> Instruction) -> (String -> a) -> (String -> b) -> List String -> Instruction
with2ops constructor mapperA mapperB args =
  case args of
    a :: b :: [] -> constructor (mapperA a) (mapperB b)
    _ -> Debug.crash ("Invalid parameter count: " ++ (toString constructor) ++ " " ++ (toString args))

crashIfError : Result String res -> res
crashIfError result =
  case result of
    Err s -> Debug.crash s
    Ok x -> x


-- INPUT
--------
smallInput : String
smallInput = """
set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2
"""

bigInput : String
bigInput = """
set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 735
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19
"""