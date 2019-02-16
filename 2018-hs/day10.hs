import Utils

import Text.ParserCombinators.ReadP
import qualified Data.List as L
import Debug.Trace

data Point = Point Int Int Int Int deriving (Show)

instance Ord Point where
  (<=) (Point x1 y1 _ _) (Point x2 y2 _ _)
    | y1 == y2  = x1 <= x2
    | otherwise = y1 <= y2

instance Eq Point where
  (==) (Point x1 y1 _ _) (Point x2 y2 _ _) = x1 == x2 && y1 == y2

parsePoint :: ReadS Point
parsePoint = readP_to_S $ Point
  <$> string "position=<" >>> parseNextNumber
  <*> string ", " >>> parseNextNumber
  <*> string "> velocity=<" >>> parseNextNumber
  <*> string ", " >>> parseNextNumber

getx :: Point -> Int
getx (Point x _ _ _ ) = x

gety :: Point -> Int
gety (Point _ y _ _ ) = y

bb :: [Point] -> ((Int, Int), (Int, Int))
bb points = ((minx, miny), (maxx - minx, maxy - miny))
  where
    xs = map getx points
    ys = map gety points
    minx = minimum xs
    miny = minimum ys
    maxx = maximum xs
    maxy = maximum ys

step :: Point -> Point
step (Point x y vx vy) = Point (x + vx) (y + vy) vx vy

draw :: [Point] -> String
draw points = go sorted (minx, miny) "\n"
  where
    sorted = L.nub . L.sort $ points
    ((minx, miny), (w, h)) = bb points
    go [] _ result = reverse result
    go points@((Point px py _ _):xs) (x, y) result
      | x > minx + w       = go points (minx, y + 1) ('\n' : result)
      | x == px && y == py = go xs     (x + 1, y)    ('X'  : result)
      | y > miny + h       = undefined
      | otherwise          = go points (x + 1, y)    ('.'  : result)

moveStars :: Int -> Int -> [Point] -> (Int, [Point])
moveStars w i points | w == 0    = moveStars (fst . snd . bb $ points) (i+1) points
                     | w >= w'   = moveStars w' (i+1) points'
                     | otherwise = (i - 1, points)
  where
    points' = map step points
    w' = fst . snd . bb $ points'

solve1 :: String -> String
solve1 = draw . snd . moveStars 0 0 . parse parsePoint . lines

solve2 :: String -> String
solve2 = show . fst . moveStars 0 0 . parse parsePoint . lines

main :: IO ()
main = do
  solve "../2018/day10.input" "10/01" solve1
  solve "../2018/day10.input" "10/02" solve2
