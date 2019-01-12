import qualified Data.Set as Set

solve :: String -> String -> (String -> String)-> IO ()
solve filename prefix solver = do
  d <- readFile filename
  putStr $ prefix ++ ": " ++ (solver d) ++ "\n"

parse :: String -> [Int]
parse = map read
      . map (dropWhile (== '+'))
      . filter (/= "")
      . lines

solve1 :: String -> String
solve1 = show
       . sum
       . parse

solve2 :: String -> String
solve2 = show
       . untilTwice Set.empty 0
       . cycle
       . parse
  where
    untilTwice :: Set.Set Int -> Int -> [Int] -> Int
    untilTwice cache acc (x:xs)
      | Set.member acc' cache = acc'
      | otherwise             = untilTwice cache' acc' xs
      where
        acc' = acc + x
        cache' = Set.insert acc' cache

main :: IO ()
main = do
  solve "day01.input" "01/01" solve1
  solve "day01.input" "01/02" solve2
