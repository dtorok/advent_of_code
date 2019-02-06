import Utils

import Text.ParserCombinators.ReadP
import Data.Maybe
import Debug.Trace


data LicenseNode = LicenseNode
  { numChildren :: Int
  , numMeta :: Int
  , children :: [LicenseNode]
  , metadata :: [Int]
  } deriving (Show)

parseLicenseNode :: ReadP LicenseNode
parseLicenseNode = do
  numChildren <- parseNextNumber
  numMeta     <- parseNextNumber
  children    <- count numChildren parseLicenseNode
  metadata    <- count numMeta parseNextNumber

  return LicenseNode
    { numChildren = numChildren
    , numMeta     = numMeta
    , children    = children
    , metadata    = metadata }

at :: [a] -> Int -> Maybe a
at [] _ = Nothing
at (x:_) 0 = Just x
at xs i = at (drop i xs) 0

sumMetadata :: LicenseNode -> Int
sumMetadata l = sumMeta + sumRecurMeta
  where
    sumMeta = sum . metadata $ l
    sumRecurMeta = sum . map sumMetadata . children $ l

calcValue :: LicenseNode -> Int
calcValue l
  | numChildren l == 0 = sumMetadata l
  | otherwise          = sum . map calcValue . mapMaybe findChild . metadata $ l
    where
      findChild i = (children l) `at` (i - 1)

solve1 :: String -> String
solve1 input = show . sumMetadata . fst . head . readP_to_S parseLicenseNode $ input

solve2 :: String -> String
solve2 input = show . calcValue . fst . head . readP_to_S parseLicenseNode $ input

main = do
  solve "../2018/day8.input" "08/01" solve1
  solve "../2018/day8.input" "08/02" solve2
