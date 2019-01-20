import Utils

import qualified Data.Map.Strict as M
import qualified Data.List as L
import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.Char

data Event = ShiftBegins Int Int
           | FallsAsleep Int
           | WakesUp Int deriving (Show, Eq, Ord)

-- type MinMap = M.Map (Int, Int) Int
type Mins = M.Map Int Int
type MinMap = M.Map Int Mins

parseMin :: ReadP Int
parseMin = do
  (_, _, _, _, mi) <- between (char '[') (char ']') parseDateTime
  return mi

parseShiftBegins :: ReadP Event
parseShiftBegins = ShiftBegins
  <$> parseMin
  <*> string " Guard #" >>> parseInt

parseFallsAsleep :: ReadP Event
parseFallsAsleep = FallsAsleep
  <$> parseMin <<< string " falls asleep"

parseWakesUp :: ReadP Event
parseWakesUp = WakesUp
  <$> parseMin <<< string " wakes up"

parseEvent :: ReadP Event
parseEvent = parseShiftBegins +++ parseFallsAsleep +++ parseWakesUp

parseEvents :: [String] -> [Event]
parseEvents = map $ fst . head . readP_to_S parseEvent

addMinsToMinMap :: Int -> Int -> Int -> MinMap -> MinMap
addMinsToMinMap gid from to minmap = L.foldl updateMinMap minmap [from..to]
  where
    updateMinMap :: MinMap -> Int -> MinMap
    updateMinMap minmap mi = M.alter (updateGuard mi) gid minmap

    updateGuard :: Int -> Maybe Mins -> Maybe Mins
    updateGuard mi = Just . M.alter updateMins mi . fromMaybe M.empty

    updateMins :: Maybe Int -> Maybe Int
    updateMins = Just . maybe 1 (+1)

buildMinMap :: (Int, Int) -> MinMap -> [Event] -> MinMap
buildMinMap _ minmap [] = minmap
buildMinMap (lastgid, lastmi) minmap (x:xs) = f x xs
  where
    f (ShiftBegins mi gid) = buildMinMap (gid, mi) minmap
    f (FallsAsleep mi)     = buildMinMap (lastgid, mi) minmap
    f (WakesUp mi)         = buildMinMap (lastgid, mi) minmap'
      where
        minmap' = addMinsToMinMap lastgid lastmi (mi-1) minmap

createMinMap :: [Event] -> MinMap
createMinMap = buildMinMap (-1, 0) M.empty

parseToMinMap :: String -> MinMap
parseToMinMap = createMinMap . parseEvents . L.sort . lines

solve1 :: String -> String
solve1 input = show $ (gid, mi, gid * mi)
  where
    gid = fst . largestInMap . M.map (sum . M.elems) $ minmap
    mi = fst . largestInMap . fromJust . M.lookup gid $ minmap
    minmap = parseToMinMap input

solve2 :: String -> String
solve2 input = show $ (gid, mi, gid * mi)
  where
    gid = fst . largestInMap . M.map (snd . largestInMap) $ minmap
    mi = fst . largestInMap . fromJust . M.lookup gid $ minmap
    minmap = parseToMinMap input

main :: IO ()
main = do
  solve "../2018/day4.input" "04/01" solve1
  solve "../2018/day4.input" "04/02" solve2
