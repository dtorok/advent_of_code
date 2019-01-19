import Utils

import Text.ParserCombinators.ReadP
import Data.Char


data Event = ShiftBegins Int Int
           | FallsAsleep Int
           | WakesUp Int deriving (Show, Eq, Ord)

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
parseEvent = choice
  [ parseShiftBegins
  , parseFallsAsleep
  , parseWakesUp
  ]

main :: IO ()
main = putStr . show $ readP_to_S parseEvent <$> input

input :: [String]
input =
  [ "[1518-11-01 00:00] Guard #10 begins shift"
  , "[1518-11-01 00:05] falls asleep"
  , "[1518-11-01 00:25] wakes up"
  , "[1518-11-01 00:30] falls asleep"
  , "[1518-11-01 00:55] wakes up"
  , "[1518-11-01 23:58] Guard #99 begins shift"
  ]
