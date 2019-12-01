import Test.HUnit
import Day01


testCalcFuel :: Test
testCalcFuel = 
    let 
        test :: Int -> Int -> Test
        test a b = TestCase $ assertEqual ("calcFuel " ++ (show a)) (calcFuel a) b
    in TestList
        [ test 12 2
        , test 14 2
        , test 1969 654
        , test 100756 33583 ]

testCalcFuelFull :: Test
testCalcFuelFull = 
    let 
        test :: Int -> Int -> Test
        test a b = TestCase $ assertEqual ("calcFuelFull " ++ (show a)) (calcFuelFull a) b
    in TestList
        [ test 12 2
        , test 14 2
        , test 1969 966
        , test 100756 50346 ]

testSolve1 :: String -> String -> String -> Test
testSolve1 name input expectedOutput = 
    TestCase $ assertEqual name (solve1 input) expectedOutput

testSolve2 :: String -> String -> String -> Test
testSolve2 name input expectedOutput = 
    TestCase $ assertEqual name (solve2 input) expectedOutput

main :: IO ()
main = do
    input <- readFile "Day01.input"
    example <- readFile "Day01.example"
    
    runTestTT $ testCalcFuel
    runTestTT $ testCalcFuelFull

    runTestTT $ testSolve1 "example1" example "34241"
    runTestTT $ testSolve1 "task1" input "3313655"
    runTestTT $ testSolve2 "task2" input "4967616"

    return ()
