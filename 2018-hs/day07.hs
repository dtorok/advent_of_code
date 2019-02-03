import Utils

import Text.ParserCombinators.ReadP
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char
import Debug.Trace

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

readyToDo :: Deps -> [Char]
readyToDo deps = Map.keys . Map.filter (== Set.empty) $ deps

cleanDep :: Char -> Deps -> Deps
cleanDep d = Map.map (Set.delete d)

solveGen :: Int -> String -> String
solveGen num input = show . go "" Map.empty 0 . mkDeps . parse parseStep . lines $ input
  where
    go :: String -> Map.Map Char Int -> Int -> Deps -> (String, Int)
    go result work ts deps
      | Map.null work && Map.null deps = (reverse result, ts - 1)
      | otherwise = go result' work' (ts + 1) deps'
        where
          workLength :: Char -> Int
          workLength c = 60 + ord c - ord 'A' + 1

          finished :: [Char]
          finished = Map.keys . Map.filter (<= ts) $ work

          depsWoFinished :: Deps
          depsWoFinished = foldl (flip cleanDep) deps finished

          workWoFinished :: Map.Map Char Int
          workWoFinished = foldl (flip Map.delete) work finished

          toStart :: [Char]
          toStart = take (num - Map.size work + length finished) . readyToDo $ depsWoFinished

          workToStart :: Map.Map Char Int
          workToStart = Map.fromList . map (\c -> (c, ts + workLength c)) $ toStart

          work' :: Map.Map Char Int
          work' = workWoFinished `Map.union` workToStart

          deps' :: Deps
          deps' = foldl (flip Map.delete) depsWoFinished toStart

          result' :: String
          result' = (reverse . sort $ finished) ++ result

main :: IO ()
main = do
  solve "../2018/day7.input" "07/01" (solveGen 1)
  solve "../2018/day7.input" "07/02" (solveGen 5)
