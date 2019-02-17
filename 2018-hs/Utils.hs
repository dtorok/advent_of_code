module Utils (parseNextNumber, range, solve, parse, parseInt, parseDate, parseTime, parseDateTime, (<<<), (>>>), largestInMap, largestInMapWith, strip, sHead) where

import Text.ParserCombinators.ReadP
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Char
import Debug.Trace

solve :: String -> String -> (String -> String)-> IO ()
solve filename prefix solver = do
  d <- readFile filename
  putStr $ prefix ++ ": " ++ (solver d) ++ "\n"

isNum :: Char -> Bool
isNum c = isNumber c || c == '-'

parseNextNumber :: ReadP Int
parseNextNumber = do
  munch (not . isNum)
  num <- read <$> munch1 isNum
  return num

parseInt :: ReadP Int
parseInt = do
  i <- munch1 isNum
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

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parse :: ReadS a -> [String] -> [a]
parse parser lines = L.map (fst . head) . L.map parser $ lines

range :: Int -> Int -> [Int]
range from len = L.take len $ iterate (+1) from

largestInMap :: Ord a => M.Map k a -> (k, a)
largestInMap = L.head . L.reverse . L.sortOn snd . M.toList

largestInMapWith :: Ord b => (a -> b) -> M.Map k a -> (k, a)
largestInMapWith f = L.head . L.reverse . L.sortOn (f . snd) . M.toList

sHead :: [a] -> Maybe a
sHead [] = Nothing
sHead (x:_) = Just x
