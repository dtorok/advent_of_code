module Day18 exposing (part1, part2)

import TestRun

import Array exposing (Array)
import Dict exposing (Dict)


part1 : TestRun.Test
part1 =
  { title = "Day 18: Duet - Part 1"
  , solver = solver1
  , testCases =
    [ ( smallInput1, "4")
    , ( bigInput, "8600")
    ]
  }

part2 : TestRun.Test
part2 =
  { title = "Day 18: Duet - Part 2"
  , solver = solver2
  , testCases =
    [ (smallInput2, "3")
    , (bigInput, "7239")
    ]
  }

-- SOLVER
---------
solver1 : String -> String
solver1 input = input
  |> parseInput
  |> newPr 0
  |> run
  |> .queueOut
  |> List.reverse
  |> List.head
  |> Maybe.withDefault -1
  |> toString

solver2 : String -> String
solver2 input = input
  |> parseInput
  |> newPrs
  |> runPrograms
  |> .program2
  |> .counterOut
  |> toString

type alias Reg = String
type RegOrInt = Reg Reg | Int Int
type Instruction
  = Set Reg RegOrInt
  | Add Reg RegOrInt
  | Mul Reg RegOrInt
  | Mod Reg RegOrInt
  | Jgz RegOrInt RegOrInt
  | Send RegOrInt
  | Receive Reg

type alias Programs =
  { program1 : Program
  , program2 : Program
  }

type alias Program =
  { instrs : Array Instruction
  , pc : Int
  , regs : Dict Reg Int
  , queueIn : List Int
  , queueOut : List Int
  , counterOut : Int
  }

runPrograms : Programs -> Programs
runPrograms prs =
  let
    pr1 = prs.program1
    pr1_ran = run pr1

    pr2 = prs.program2
    pr2_queuein_updated = { pr2 | queueIn = pr2.queueIn ++ pr1_ran.queueOut }
    pr2_ran = run pr2_queuein_updated

    pr1_queuein_updated = { pr1_ran | queueIn = pr1_ran.queueIn ++ pr2_ran.queueOut }

    pr1_new =
      { pr1_queuein_updated
      | queueOut = []
      , counterOut = pr1_queuein_updated.counterOut + (List.length pr1_queuein_updated.queueOut) }
    pr2_new =
      { pr2_ran
      | queueOut = []
      , counterOut = pr2_ran.counterOut + (List.length pr2_ran.queueOut) }

    newPrs =
      { prs
      | program1 = pr1_new
      , program2 = pr2_new }
  in
    if List.isEmpty pr1_new.queueIn && List.isEmpty pr2_new.queueIn then
      newPrs
    else
      runPrograms newPrs

run : Program -> Program
run pr =
  case Array.get pr.pc pr.instrs of
    Just instr ->
      let
        mPr = execute instr pr
      in
        case mPr of
          Just newPr ->
            run newPr
          Nothing ->
            pr -- blocking on rcv
    Nothing ->
      { pr | queueIn = [] } -- pc out of bounds

execute : Instruction -> Program -> Maybe Program
execute instruction ex =
  case instruction of
    Set reg roi -> Just
      { ex
      | regs = Dict.insert reg (get roi ex) ex.regs
      , pc = ex.pc + 1 }
    Add reg roi -> Just
      { ex
      | regs = regChg reg roi (+) ex
      , pc = ex.pc + 1 }
    Mul reg roi -> Just
      { ex
      | regs = regChg reg roi (*) ex
      , pc = ex.pc + 1 }
    Mod reg roi -> Just
      { ex
      | regs = regChg reg roi (%) ex
      , pc = ex.pc + 1 }
    Jgz reg roi -> Just <|
      if get reg ex > 0 then
        { ex
        | pc = ex.pc + (get roi ex) }
      else
        { ex
        | pc = ex.pc + 1 }
    Send roi -> Just
      { ex
      | queueOut = ex.queueOut ++ [get roi ex]
      , pc = ex.pc + 1 }
    Receive reg ->
      case ex.queueIn of
        x :: xs -> Just
          { ex
          | regs = Dict.insert reg x ex.regs
          , queueIn = xs
          , pc = ex.pc + 1 }
        [] ->
          Nothing

get : RegOrInt -> Program -> Int
get regOrInt ex =
  case regOrInt of
    Reg reg -> regGet reg ex
    Int int -> int

regGet : Reg -> Program -> Int
regGet reg ex =
  Maybe.withDefault 0 (Dict.get reg ex.regs)

regSet : Reg -> Int -> Program -> Program
regSet reg val ex =
  { ex | regs = Dict.insert reg val ex.regs }

regChg : Reg -> RegOrInt -> (Int -> Int -> Int) -> Program -> Dict Reg Int
regChg reg roi f ex =
  let
    regVal = regGet reg ex
    roiVal = get roi ex
    result = f regVal roiVal
  in
    Dict.insert reg result ex.regs


newPrs : Array Instruction -> Programs
newPrs instrs =
  { program1 = newPr 0 instrs
  , program2 = newPr 1 instrs }

newPr : Int -> Array Instruction -> Program
newPr pid instrs =
  { instrs = instrs
  , pc = 0
  , regs = Dict.fromList [("p", pid)]
  , queueIn = []
  , queueOut = []
  , counterOut = 0
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

    (snd, rcv) =
        (Send, Receive)

  in
    case String.left 3 input of
      "set" -> args |> with2ops Set reg regOrInt
      "add" -> args |> with2ops Add reg regOrInt
      "mul" -> args |> with2ops Mul reg regOrInt
      "mod" -> args |> with2ops Mod reg regOrInt
      "jgz" -> args |> with2ops Jgz regOrInt regOrInt
      "snd" -> args |> with1op Send regOrInt
      "rcv" -> args |> with1op Receive reg
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
smallInput1 : String
smallInput1 = """
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

smallInput2 : String
smallInput2 = """
snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d
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