import Test.HUnit
import Day02

testSolve1 name p input expected = TestCase $ assertEqual name (solve1 p input) expected
testSolve2 name input expected = TestCase $ assertEqual name (solve2 input) expected

testSolve1Examples :: Test
testSolve1Examples = 
    TestList 
        [ testSolve1 "test1" id "1,9,10,3,2,3,11,0,99,30,40,50" "3500"
        , testSolve1 "test2" id "1,0,0,0,99" "2"
        , testSolve1 "test3" id "2,3,0,3,99" "2"
        , testSolve1 "test4" id "2,4,4,5,99,0" "2"
        , testSolve1 "test5" id "1,1,1,4,99,5,6,0,99" "30"
        ]

testSolve1Real :: String -> Test
testSolve1Real input =
    testSolve1 "real" prepare input "3895705"

testSolve2Real :: String -> Test
testSolve2Real input =
    testSolve2 "real" input "6417"

main :: IO ()
main = do
    input <- readFile "Day02.input"
    
    runTestTT $ testSolve1Examples
    runTestTT $ testSolve1Real input

    runTestTT $ testSolve2Real input

    return ()
