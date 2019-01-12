import Data.List

-- solution 1
-------------
solve1 :: String -> String
solve1 d = show $ cnt2 * cnt3
  where
    cnt2 = length . filter (hasRepetition 2) . lines $ d
    cnt3 = length . filter (hasRepetition 3) . lines $ d

hasRepetition :: Int -> String -> Bool
hasRepetition num = (> 0) . length . filter (== num) . map length . group . sort

-- solution 2
-------------
solve2 :: String -> String
solve2 input = show
             . commons
             . head
             . filter (diff 1)
             . pair
             $ (ids, ids)
  where ids = lines input

pair :: ([String], [String]) -> [(String, String)]
pair (xs, ys) = [(x, y) | x <- xs, y <- ys, x /= y]

diff :: Int -> (String, String) -> Bool
diff d (a, b) = d == length [x | (x, y) <- zip a b, x /= y]

commons :: (String, String) -> String
commons (a, b) = [x | (x, y) <- zip a b, x == y]

-- main
------------
solve :: String -> String -> (String -> String)-> IO ()
solve filename prefix solver = do
  d <- readFile filename
  putStr $ prefix ++ ": " ++ (solver d) ++ "\n"

main :: IO ()
main = do
  solve "day02.input" "02/01" solve1
  solve "day02.input" "02/02" solve2
