import Utils

import Text.ParserCombinators.ReadP
import qualified Data.List as L
import Debug.Trace

data V = V { y :: Int, x:: Int} deriving (Eq, Ord)
data Star = Star V V

parseStar :: ReadS Star
parseStar = readP_to_S $ do
  x  <- string "position=<" >>> parseNextNumber
  y  <- string ", " >>> parseNextNumber
  vx <- string "> velocity=<" >>> parseNextNumber
  vy <- string ", " >>> parseNextNumber
  return $ Star (V y x) (V vy vx)

coord :: Star -> V
coord (Star v _) = v

bb :: [V] -> (V, V)
bb vectors = (V miny minx, V (maxy - miny) (maxx - minx))
  where
    xs = map x vectors
    ys = map y vectors
    minx = minimum xs
    miny = minimum ys
    maxx = maximum xs
    maxy = maximum ys

step :: Star -> Star
step (Star (V y x) (V vy vx)) = Star (V (y + vy) (x + vx)) (V vy vx)

draw :: [V] -> String
draw vectors = go sorted (minx, miny) "\n"
  where
    sorted               = L.nub . L.sort $ vectors
    (V miny minx, V h w) = bb vectors

    go [] _ result         = reverse result
    go vs@((V py px):xs) (x, y) result
      | y > miny + h       = undefined
      | x > minx + w       = go vs (minx, y + 1) ('\n' : result)
      | x == px && y == py = go xs (x + 1, y)    ('X'  : result)
      | otherwise          = go vs (x + 1, y)    ('.'  : result)

moveStars :: Int -> Int -> [Star] -> (Int, [Star])
moveStars w i stars | w == 0    = moveStars (x . snd . bb . map coord $ stars) (i+1) stars
                    | w >= w'   = moveStars w' (i+1) stars'
                    | otherwise = (i - 1, stars)
  where
    stars' = map step stars
    w' = x . snd . bb . map coord $ stars'

solve1 :: String -> String
solve1 = draw . map coord . snd . moveStars 0 0 . parse parseStar . lines

solve2 :: String -> String
solve2 = show . fst . moveStars 0 0 . parse parseStar . lines

main :: IO ()
main = do
  solve "../2018/day10.input" "10/01" solve1
  solve "../2018/day10.input" "10/02" solve2
