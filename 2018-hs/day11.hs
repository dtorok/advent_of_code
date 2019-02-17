import Utils

import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace

type PowerMap = Map.Map (Int, Int) Int
type PowerMaps = Map.Map Int PowerMap


powerLevel :: Int -> (Int, Int) -> Int
powerLevel serial (x, y) = (hundreds ((rackId * y + serial) * rackId)) - 5
  where
    rackId = x + 10
    hundreds n = (n `div` 100) `mod` 10

powerLevel3x3 :: Int -> (Int, Int) -> Int
powerLevel3x3 serial (x, y) = sum . map (powerLevel serial) $ (,) <$> [x..x+2] <*> [y..y+2]

powerLevelNxN :: Int -> Int -> PowerMaps -> (Int, Int) -> Int
powerLevelNxN serial 0 powermaps coord = 0
powerLevelNxN serial 1 powermaps coord = powerLevel serial coord
powerLevelNxN serial n powermaps (x,y) = traceShow n $ current + topleft + leftsub + rightsub - midsub
  where
    current  = powerLevel serial (x,y)
    topleft  = getPowerSum 1       (x - n + 1, y - n + 1) powermaps
    leftsub  = getPowerSum (n - 1) (x - 1, y) powermaps
    rightsub = getPowerSum (n - 1) (x, y - 1) powermaps
    midsub   = getPowerSum (n - 2) (x - 1, y - 1) powermaps

getPowerSum :: Int -> (Int, Int) -> PowerMaps -> Int
getPowerSum size coord =
  fromJust . Map.lookup coord . fromJust . Map.lookup size

addPowerMap :: Int -> PowerMaps -> Int -> PowerMaps
addPowerMap serial powermaps size = Map.insert size powermap powermaps
  where
    powermap = Map.fromList . map calcPowerLevel $ (,) <$> [size..300] <*> [size..300]
    calcPowerLevel c = (c, powerLevelNxN serial size powermaps c)

solve1 :: Int -> (Int, Int)
solve1 serial = fst . largestInMap . Map.fromList . map calcPower $ (,) <$> [1..298] <*> [1..298]
  where
    calcPower coord = (coord, powerLevel3x3 serial coord)

solve2 :: Int -> (Int, Int, Int)
solve2 serial = trans . largestInMapWith snd . Map.map largestInMap . foldl (addPowerMap serial) Map.empty $ [0..300]
  where
    -- transform coord from bottom-right to top-left
    trans (size, ((x, y), pl)) = (x - size + 1, y - size + 1, size)

main :: IO ()
main = do
  putStrLn . show $ solve1 3463
  putStrLn . show $ solve2 3463
