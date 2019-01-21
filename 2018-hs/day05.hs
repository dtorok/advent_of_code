import Utils
import Data.Char
import Data.List
import qualified Data.Set as S

import Debug.Trace

pairs :: Char -> Char -> Bool
pairs a b =
  (toUpper a == toUpper b) &&
  (isUpper a && isLower b || isLower a && isUpper b)

reducer :: String -> String
reducer [] = ""
reducer (c:cs) = case reducer cs of
  ""     -> c:""
  (r:"") -> c:r:""
  (r:rs) -> if pairs r c then rs else (c:r:rs)

solve1 :: String -> String
solve1 = show . length . reducer . strip

solve2 :: String -> String
solve2 input = show . head . sort . map solvewithout $ allunits
  where
    solvewithout :: Char -> Int
    solvewithout c = length . reducer . filter (\i -> toUpper i /= c) . strip $ input
    allunits :: String
    allunits = S.toList . S.fromList . map toUpper . strip $ input

main :: IO ()
main = do
  solve "../2018/day5.input" "05/01" solve1
  solve "../2018/day5.input" "05/02" solve2
