module Day02 (solve1, solve2, prepare) where

    import Data.List.Split
    import Data.Map.Strict ((!))
    import qualified Data.Map.Strict as M

    type Mem = M.Map Int Int
    type Prepare = Mem -> Mem

    -- COMPUTER
    -----------
    compute :: Int -> Mem -> Int
    compute index mem =
        case mem ! index of
            99 -> mem ! 0
            1 -> compute (index + 4) (operate (+))
            2 -> compute (index + 4) (operate (*))
        where 
            operate :: (Int -> Int -> Int) -> Mem
            operate f = M.insert to (f op1 op2) mem
                where
                    op1 = mem ! (mem ! (index + 1))
                    op2 = mem ! (mem ! (index + 2))
                    to = mem ! (index + 3)

    -- MEMORY PREPARATION
    ---------------------
    prepareWith :: Int -> Int -> Prepare
    prepareWith a b = M.insert 1 a . M.insert 2 b

    prepare :: Prepare
    prepare = prepareWith 12 2

    -- PARSER
    ---------
    parseMem :: String -> Mem
    parseMem = M.fromList . zip [0..] . map read . splitOn ","

    -- SOLVERS
    ----------    
    solve1 :: Prepare -> String -> String
    solve1 p = show . compute 0 . p . parseMem

    solve2 :: String -> String
    solve2 = show . checkWords ((,) <$> [0..99] <*> [0..99]) . parseMem
        where
            checkWords :: [(Int, Int)] -> Mem -> Int
            checkWords ((a, b):xs) mem =
                case compute 0 (prepareWith a b mem) of
                    19690720 -> 100 * a + b
                    _ -> checkWords xs mem
                    