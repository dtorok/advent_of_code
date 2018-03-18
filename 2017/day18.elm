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
    -- [ (smallInput2, "3")
    [ (bigInput, "?")
    ]
  }

-- SOLVER
---------
solver1 : String -> String
solver1 input = input
  |> parseInput SendParser
  |> newPr 0
  |> run
  |> .queueOut
  |> List.reverse
  |> List.head
  |> Maybe.withDefault -1
  |> toString

solver2 : String -> String
solver2 input = input
  |> parseInput SendParser
  |> newPrs
  |> runPrograms 0
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
  | Sound RegOrInt
  | Recover Reg
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
  , sound : Maybe Int
  , recovered : Maybe Int
  , queueIn : List Int
  , queueOut : List Int
  , counterOut : Int
  }

runPrograms : Int -> Programs -> Programs
runPrograms counter prs =
  let
    pr1_1 = -- withStats "before run" counter <|
      prs.program1
    pr1_2 = -- withStats "after run" counter <|
      run pr1_1

    pr2_1 = prs.program2
    pr2_2 = { pr2_1 | queueIn = Debug.log ("queueIn 2 " ++ (toString counter)) <| pr2_1.queueIn ++ pr1_2.queueOut }
    pr2_3 = -- withStats 1 <|
      run pr2_2

    pr1_3 = { pr1_2 | queueIn = Debug.log ("queueIn 1 " ++ (toString counter)) <| pr1_2.queueIn ++ pr2_3.queueOut }

    pr1_x =
      { pr1_3
      | queueOut = []
      , counterOut = pr1_3.counterOut + (List.length pr1_3.queueOut) }
    pr2_x =
      { pr2_3
      | queueOut = []
      , counterOut = pr2_3.counterOut + (List.length pr2_3.queueOut) }

    newPrs =
      { prs
      | program1 = pr1_x
      , program2 = pr2_x }
  in
    if List.isEmpty pr1_x.queueIn && List.isEmpty pr2_x.queueIn then
      --Debug.log "end" <|
      newPrs
    else
      if counter > 200 then
        newPrs
      else
        --Debug.log ("round" ++ (fullStats newPrs)) <|
        runPrograms (counter + 1) newPrs

withStats : String -> Int -> Program -> Program
withStats msg pid prs =
  Debug.log (msg ++ " " ++ (stats pid prs)) prs

fullStats : Programs -> String
fullStats prs =
  "\n  " ++ (stats 0 prs.program1) ++ "\n  " ++ (stats 1 prs.program2) ++ "\n  "

stats : Int -> Program -> String
stats pidNum pr =
  let
    queueInLen = toString <| List.length pr.queueIn
    queueInSum = toString <| List.sum pr.queueIn
    pc = toString <| pr.pc
    pid = toString <| pidNum
  in
    "pid: " ++ pid ++ " queueIn: " ++ queueInLen ++ "/" ++ queueInSum


run : Program -> Program
run pr =
  case Array.get pr.pc pr.instrs of
    Just instr ->
      let
        mPr = -- Debug.log "execute" <|
          execute instr pr
      in
        case mPr of
          Just newPr ->
            run newPr
          Nothing ->
            pr -- blocking on rcv
    Nothing ->
      { pr | queueIn = [] } -- pc out of bounds


-- run : Program -> Program
-- run ex =
--   if isJust ex.recovered then
--     ex
--   else
--     case Array.get ex.pc ex.instrs of
--       Just instr ->
--         run (execute instr ex)
--       Nothing ->
--         ex

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
      | regs = -- Debug.log ("mul " ++ (toString (regGet reg ex)) ++ " " ++ (toString (get roi ex))) <|
          regChg reg roi (*) ex
      , pc = ex.pc + 1 }
    Mod reg roi -> Just
      { ex
      | regs = regChg reg roi (%) ex
      , pc = ex.pc + 1 }
    Jgz reg roi -> Just <|
      if get reg ex > 0 then
        { ex
        | pc = -- Debug.log ("jgz from " ++ (toString ex.pc)) <|
            ex.pc + (get roi ex) }
      else
        { ex
        | pc = ex.pc + 1 }
    Sound roi -> Just
      { ex
      | sound = Just (get roi ex)
      , pc = ex.pc + 1 }
    Recover reg -> Just <|
      if regGet reg ex > 0 then
        { ex
        | recovered = ex.sound
        , pc = ex.pc + 1 }
      else
        { ex
        | pc = ex.pc + 1 }
    Send roi -> Just
      { ex
      | queueOut = -- Debug.log "after send" <|
          ex.queueOut ++ [get roi ex]
      , pc = ex.pc + 1 }
    Receive reg ->
      case ex.queueIn of
        x :: xs -> Just
          { ex
          | regs = Dict.insert reg x ex.regs
          , queueIn = -- Debug.log "after receive"
              xs
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
  , sound = Nothing
  , recovered = Nothing
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
type ParserType = SoundParser | SendParser

parseInput : ParserType -> String -> Array Instruction
parseInput parserType input = input
  |> String.trim -- String
  |> String.split "\n" -- List String
  |> List.map String.trim -- List String
  |> List.map (parseInstruction parserType) -- List Instruction
  |> Array.fromList -- Array Instruction

parseInstruction : ParserType -> String -> Instruction
parseInstruction parserType input =
  let
    cmd = input
      |> String.left 3 -- String

    args = input
      |> String.dropLeft 4 -- String
      |> String.split " " -- List String

    (snd, rcv) =
      if parserType == SoundParser then
        (Sound, Recover)
      else
        (Send, Receive)

  in
    case String.left 3 input of
      "set" -> args |> with2ops Set reg regOrInt
      "add" -> args |> with2ops Add reg regOrInt
      "mul" -> args |> with2ops Mul reg regOrInt
      "mod" -> args |> with2ops Mod reg regOrInt
      "jgz" -> args |> with2ops Jgz regOrInt regOrInt
      "snd" -> args |> with1op snd regOrInt
      "rcv" -> args |> with1op rcv reg
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