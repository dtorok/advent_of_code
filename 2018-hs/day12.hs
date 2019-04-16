import Utils

import Text.ParserCombinators.ReadP
import Data.Char
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable
import Debug.Trace

data Plants = Plants Int (Seq.Seq Char) deriving Show
type Rules = Map.Map String Char

parseRule :: ReadP (String, Char)
parseRule = (,)
  <$> count 5 get <<< string " => "
  <*> get

-- parseInput :: ReadP (String, [(String, Char)])
parseInput :: ReadP (Plants, Rules)
parseInput = do
  plantsString <- string "initial state: " >>> many (char '.' +++ char '#') <<< string "\n\n"
  rulesList <- many1 (parseRule <<< char '\n') <<< eof

  let plants = Plants 0 (Seq.fromList plantsString)
  let rules = Map.fromList rulesList

  return (plants, rules)

emptypot :: Char -> Bool
emptypot c = c == '.'

extend :: Plants -> Plants
extend (Plants z p) = Plants z' p'
  where
    z' = z - 5
    p' = (Seq.fromList ".....") Seq.>< p Seq.>< (Seq.fromList ".....")

reduce :: Plants -> Plants
reduce (Plants z p) = Plants z' p'
  where
    z' = z + Seq.length (Seq.takeWhileL emptypot p)
    p' = Seq.dropWhileL emptypot . Seq.dropWhileR emptypot $ p

calcResult :: Plants -> Int
calcResult (Plants z p) = go z (Foldable.toList p) 0
  where
    go _ [] res = res
    go i ('.':xs) res = go (i + 1) xs res
    go i ('#':xs) res = go (i + 1) xs (res + i)

solve1 :: Int -> String -> String
solve1 gens input = show . calcResult . foldl growGen initplants $ [1..gens]
  where
    (initplants, rules) = fst . head . readP_to_S parseInput $ input

    growGen :: Plants -> Int -> Plants
    growGen plants i = (snd . traceShowId . (,) i) . reduce . growPlants . extend $ plants

    growPlants :: Plants -> Plants
    growPlants (Plants z p) = Plants z' p'
      where
        z' = z + 2
        p' = grow Seq.empty p

    grow :: Seq.Seq Char -> Seq.Seq Char -> Seq.Seq Char
    grow result pots@(p Seq.:<| ps)
      | Seq.length pots < 5 = result
      | otherwise           = grow result' ps
        where
          result' = result Seq.|> Map.findWithDefault '.' pattern rules
          pattern = Foldable.toList . Seq.take 5 $ pots

main :: IO ()
main = do
  solve "../2018/day12.input" "12/01" (solve1 20)
  solve "../2018/day12.input" "12/02" (solve1 500)
  putStrLn . show . calcResult $ Plants (50000000000 - 95) (Seq.fromList "#.#.....#.#.....#.#.....#.#.....#.#.....#.#.....#.#............#.#.....#.#.....#.#.....#.#.....#.#.....#.#.....#.#.....#.#......#.#.....#.#........#.#.....#.#.......#.#...#.#.....#.#......#.#")
