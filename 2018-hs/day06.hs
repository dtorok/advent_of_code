import Utils

import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as Map
import Data.List
import Debug.Trace

data Coord = Coord
  { x :: Int
  , y :: Int
  } deriving (Show, Eq, Ord)

data Info = Info
  { size :: Int
  , infinite :: Bool
  } deriving Show

coord :: ReadS Coord
coord = readP_to_S $ Coord <$> parseNextNumber <*> parseNextNumber

mkBB :: [Coord] -> (Coord, Coord)
mkBB cs = (Coord minx miny, Coord maxx maxy)
  where
    minx = minimum . map x $ cs
    miny = minimum . map y $ cs
    maxx = maximum . map x $ cs
    maxy = maximum . map y $ cs

isOutside :: (Coord, Coord) -> Coord -> Bool
isOutside (Coord minx miny, Coord maxx maxy) (Coord x y) =
  x <= minx || x >= maxx || y <= miny || y >= maxy

mkCoords :: String -> [Coord]
mkCoords = parse coord . lines

distance :: Coord -> Coord -> Int
distance a b = abs (x a - x b) + abs (y a - y b)

mkArea :: (Coord, Coord) -> [Coord]
mkArea (tl, br) = Coord <$> [x tl .. x br] <*> [y tl .. y br]

solve1 :: String -> String
solve1 input = show . size . head . reverse . sortOn size . filter (\i -> infinite i == False) . map snd . Map.toList . foldl buildCmap emptyCmap $ area
  where
    coords = mkCoords input
    bb = mkBB coords
    area = mkArea bb
    emptyCmap = Map.fromList . map (\c -> (c, Info 0 False)) $ coords

    buildCmap :: Map.Map Coord Info -> Coord -> Map.Map Coord Info
    buildCmap cmap cArea =
      case closestCoords of
        (c:[]) -> addToMap cmap c
        otherwise -> cmap
      where
        outside :: Bool
        outside = isOutside bb cArea

        around :: [(Coord, Int)]
        around = sortOn snd . zip coords . map (distance cArea) $ coords

        minD :: Int
        minD = snd . head $ around

        closestCoords :: [Coord]
        closestCoords = map fst . filter ((== minD) . snd) $ around

        addToMap :: Map.Map Coord Info -> Coord -> Map.Map Coord Info
        addToMap cm c = Map.adjust (\i -> Info (size i + 1) (infinite i || outside)) c cm


main :: IO ()
main = do
  solve "../2018/day6.input" "06/01" solve1
