module Utils (parseNextNumber, range, solve, parse, parseInt, parseDate, parseTime, parseDateTime, (<<<), (>>>)) where

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

parseInt :: ReadP Int
parseInt = do
  i <- munch1 isNumber
  return $ read i

parseDate :: ReadP (Int, Int, Int)
parseDate = do
  (y:m:d:[]) <- sepBy (munch1 isNumber) (char '-')
  return (read y, read m, read d)

parseTime :: ReadP (Int, Int)
parseTime = do
  (h:m:[]) <- sepBy (munch1 isNumber) (char ':')
  return (read h, read m)

parseDateTime :: ReadP (Int, Int, Int, Int, Int)
parseDateTime = do
  (y, mo, d) <- parseDate
  char ' '
  (h, mi) <- parseTime
  return (y, mo, d, h, mi)

(<<<) :: ReadP a -> ReadP b -> ReadP a
a <<< b = do
  ret <- a
  _ <- b
  return ret

(>>>) :: ReadP a -> ReadP b -> ReadP b
a >>> b = do
  _ <- a
  ret <- b
  return ret



parse :: ReadS a -> [String] -> [a]
parse parser lines = L.map (fst . head) . L.map parser $ lines

range :: Int -> Int -> [Int]
range from len = L.take len $ iterate (+1) from
