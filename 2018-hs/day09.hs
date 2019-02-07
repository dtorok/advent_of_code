import qualified Data.Map as Map

import Debug.Trace

type Board = ([Int], [Int], Int)

norm :: Board -> Board
norm ([], ys, size) = (reverse ys, [], size)
norm board          = board

cw1 :: Board -> Board
cw1 (x:xs, ys, size) = norm (xs, x:ys, size)

ccw1 :: Board -> Board
ccw1 (xs, [], size) = ccw1 ([], reverse xs, size)
ccw1 (xs, y:ys, size) = (y:xs, ys, size)

cw :: Int -> Board -> Board
cw 0 board = board
cw n board = cw (n - 1) (cw1 board)

ccw :: Int -> Board -> Board
ccw 0 board = board
ccw n board = ccw (n - 1) (ccw1 board)

init_board :: Board
init_board = ([0], [], 1)

add :: Int -> Board -> Board
add x (xs, ys, size) = (x:xs, ys, size + 1)

remove_current :: Board -> Board
remove_current (x:[], ys, size) = (reverse ys, [], size - 1)
remove_current (x:xs, ys, size) = (xs, ys, size - 1)

current :: Board -> Int
current (x:_, _, _) = x
-- current board = current . r $ board

s (a, b, _) = length a + length b
showProg curr
  | curr `mod` 10000 == 0 = traceShowId curr
  | otherwise         = curr

solution :: Int -> Int -> Int
-- solution players rounds = maxScore $ foldl go (init_board, Map.empty, 0) [1..rounds]
solution players rounds = go (init_board, Map.empty, 0) 1
  where
    go :: (Board, Map.Map Int Int, Int) -> Int -> Int
    go context@(_, _, maxScore) curr
      | curr == rounds     = maxScore
      | curr `mod` 23 == 0 = go (handle23 context curr) (curr + 1)
      | otherwise          = go (handleNormal context curr) (curr + 1)

    handleNormal context@(board, score, maxScore) curr =
      (add curr . cw 2 $ board, score, maxScore)

    handle23 context@(board, score, maxScore) curr =
      (board', score', maxScore')
        where
          pl = (curr-1) `mod` players
          score' = Map.insertWith (+) pl (curr + current board7ccw) score
          board7ccw = ccw 7 board
          board' = remove_current board7ccw
          maxScore' = max maxScore ((Map.!) score' pl)

    maxScore :: (Board, Map.Map Int Int, Int) -> Int
    maxScore (_, _, maxScore) = maxScore

main :: IO ()
main = do
  putStrLn . show $ solution 9 25
  putStrLn . show $ solution 10 1618
  putStrLn . show $ solution 13 7999
  putStrLn . show $ solution 17 1104
  putStrLn . show $ solution 21 6111
  putStrLn . show $ solution 30 5807
  putStrLn . show $ solution 405 70953
  putStrLn . show $ solution 405 (70953 * 100)
