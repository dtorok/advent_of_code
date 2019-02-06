import Utils

import Text.ParserCombinators.ReadP

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

sumMetadata :: LicenseNode -> Int
sumMetadata l = sumMeta + sumRecurMeta
  where
    sumMeta = sum . metadata $ l
    sumRecurMeta = sum . map sumMetadata . children $ l

solve1 :: String -> String
solve1 input = show . sumMetadata . fst . head . readP_to_S parseLicenseNode $ input

main = solve "../2018/day8.input" "08/01" solve1
