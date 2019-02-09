import Utils

import Data.Sequence
import qualified Data.Map.Strict as Map


rotate :: Int -> Seq a -> Seq a
rotate i s = (back >< front)
  where
    (front, back) = Data.Sequence.splitAt (i `mod` Data.Sequence.length s) s

solution :: Int -> Int -> Int
solution players maxMarble = maxScore . Prelude.foldl go (singleton 0, Map.empty) $ [1..maxMarble]
  where
    go :: (Seq Int, Map.Map Int Int) -> Int -> (Seq Int, Map.Map Int Int)
    go (board, scores) curr
      | curr `mod` 23 == 0 = handle23
      | otherwise          = handleNormal
      where
        handleNormal = (curr <| (rotate 2 board), scores)
        handle23 = (board', scores')
          where
            pl = (curr - 1) `mod` players
            scores' = Map.insertWith (+) pl (curr + addScore) scores
            (addScore :<| board') = rotate (-7) board

    maxScore :: (Seq Int, Map.Map Int Int) -> Int
    maxScore (_, scores) = maximum (Map.elems scores)


main :: IO()
main = do
  putStrLn . show $ solution 9 25
  putStrLn . show $ solution 10 1618
  putStrLn . show $ solution 13 7999
  putStrLn . show $ solution 17 1104
  putStrLn . show $ solution 21 6111
  putStrLn . show $ solution 30 5807
  putStrLn . show $ solution 405 70953
  putStrLn . show $ solution 405 (70953 * 100)
