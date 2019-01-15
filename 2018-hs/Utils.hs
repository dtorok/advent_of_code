module Utils (parseNextNumber, range, solve, parse) where

import Text.ParserCombinators.ReadP
import qualified Data.List as L
import Data.Char

solve :: String -> String -> (String -> String)-> IO ()
solve filename prefix solver = do
  d <- readFile filename
  putStr $ prefix ++ ": " ++ (solver d) ++ "\n"

parseNextNumber :: ReadP Int
parseNextNumber = do
  munch (not . isNumber)
  num <- read <$> munch1 isNumber
  return num

parse :: ReadS a -> [String] -> [a]
parse parser lines = L.map (fst . head) . L.map parser $ lines

range :: Int -> Int -> [Int]
range from len = L.take len $ iterate (+1) from
