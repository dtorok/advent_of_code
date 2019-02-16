import Utils

import qualified Data.Map.Strict as Map
import Debug.Trace

powerLevel :: Int -> (Int, Int) -> Int
powerLevel serial (x, y) = (hundreds ((rackId * y + serial) * rackId)) - 5
  where
    rackId = x + 10
    hundreds n = (n `div` 100) `mod` 10

powerLevel3x3 :: Int -> (Int, Int) -> Int
powerLevel3x3 serial (x, y) = sum . map (powerLevel serial) $ (,) <$> [x..x+2] <*> [y..y+2]

solve1 :: Int -> (Int, Int)
solve1 serial = fst . largestInMap . Map.fromList . map calcPower $ (,) <$> [1..298] <*> [1..298]
  where
    calcPower coord = (coord, powerLevel3x3 serial coord)

main :: IO ()
main = putStrLn . show $ solve1 3463
