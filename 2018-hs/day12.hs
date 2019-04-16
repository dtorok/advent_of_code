import Utils

import Text.ParserCombinators.ReadP
import Data.Char
import qualified Data.Map.Strict as Map
import qualified Data.Foldable as Foldable
import Debug.Trace

data Plants = Plants Int String deriving Show
type Rules = Map.Map String Char


parseRule :: ReadP (String, Char)
parseRule = (,)
  <$> count 5 get <<< string " => "
  <*> get

parseInput :: ReadP (Plants, Rules)
parseInput = do
  plantsString <- string "initial state: " >>> many (char '.' +++ char '#') <<< string "\n\n"
  rulesList <- many1 (parseRule <<< char '\n') <<< eof

  let plants = Plants 0 plantsString
  let rules = Map.fromList rulesList

  return (plants, rules)

emptypot :: Char -> Bool
emptypot c = c == '.'

extend :: Plants -> Plants
extend (Plants z p) = Plants z' p'
  where
    z' = z - 5
    p' = "....." ++ p

reduce :: Plants -> Plants
reduce (Plants z p) = Plants z' p'
  where
    z' = z + length (takeWhile emptypot p)
    p' = dropWhile emptypot p

calcResult :: Plants -> Int
calcResult (Plants z p) = go z p 0
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
        p' = reverse . grow "" $ p

    grow :: String -> String -> String
    grow result ('.':'.':'.':'.':'.':[]) = result
    grow result (a:b:c:d:[]) = grow result (a:b:c:d:'.':[])
    grow result (a:b:c:d:e:xs) = grow (curr : result) (b:c:d:e:xs)
      where
        curr = Map.findWithDefault '.' (a:b:c:d:e:[]) rules

main :: IO ()
main = do
  solve "../2018/day12.input" "12/01" (solve1 20)
  solve "../2018/day12.input" "12/02" (solve1 500)
  putStrLn . show . calcResult $ Plants (50000000000 - 95) "#.#.....#.#.....#.#.....#.#.....#.#.....#.#.....#.#............#.#.....#.#.....#.#.....#.#.....#.#.....#.#.....#.#.....#.#......#.#.....#.#........#.#.....#.#.......#.#...#.#.....#.#......#.#"
