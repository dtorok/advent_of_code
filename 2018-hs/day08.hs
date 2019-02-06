import Utils

import Text.ParserCombinators.ReadP
import Data.Maybe
import Debug.Trace


data LicenseNode = LicenseNode [LicenseNode] [Int] deriving (Show)

parseLicenseNode :: ReadP LicenseNode
parseLicenseNode = do
  numChildren <- parseNextNumber
  numMeta     <- parseNextNumber
  children    <- count numChildren parseLicenseNode
  metadata    <- count numMeta parseNextNumber

  return $ LicenseNode children metadata

at :: [a] -> Int -> Maybe a
at [] _ = Nothing
at (x:_) 0 = Just x
at xs i = at (drop i xs) 0

sumMetadata :: LicenseNode -> Int
sumMetadata (LicenseNode children metadata) =
  (sum metadata) + (sum (map sumMetadata children))

calcValue :: LicenseNode -> Int
calcValue (LicenseNode [] metadata) = sum metadata
calcValue (LicenseNode children metadata) =
  sum . map calcValue . mapMaybe findChild $ metadata
    where
      findChild i = children `at` (i - 1)

solve1 :: String -> String
solve1 input = show . sumMetadata . fst . head . readP_to_S parseLicenseNode $ input

solve2 :: String -> String
solve2 input = show . calcValue . fst . head . readP_to_S parseLicenseNode $ input

main = do
  solve "../2018/day8.input" "08/01" solve1
  solve "../2018/day8.input" "08/02" solve2
