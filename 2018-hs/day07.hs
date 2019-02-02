import Utils

import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
-- import Debug.Trace

type Step = (Char, Char)
type Deps = Map.Map Char (Set.Set Char)

parseStep :: ReadS Step
parseStep = readP_to_S $ (,) <$> string "Step " >>> get <*> string " must be finished before step " >>> get

mkDeps :: [Step] -> Deps
mkDeps steps = foldl addDep Map.empty steps
  where
    addDep :: Deps -> Step -> Deps
    addDep deps (first, second) =
        Map.insertWith (Set.union) first Set.empty
      $ Map.insertWith (Set.union) second (Set.singleton first)
      $ deps

solve1 :: String -> String
solve1 input = show . go "" . mkDeps . parse parseStep . lines $ input
  where
    go :: String -> Deps -> String
    go result deps
      | Map.null deps = reverse result
      | otherwise     = go (next:result) deps'
        where
          next :: Char
          next = minimum . Map.keys . Map.filter (== Set.empty) $ deps

          deps' :: Deps
          deps' = Map.delete next
                $ Map.map (Set.delete next)
                $ deps

main :: IO ()
main = do
  solve "../2018/day7.input" "07/01" solve1
