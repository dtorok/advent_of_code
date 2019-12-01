module Day01 (calcFuel, calcFuelFull, solve1, solve2) where

    -- CALCULATORS
    --------------
    calcFuel :: Int -> Int
    calcFuel val = max 0 (val `div` 3 - 2)

    calcFuelFull :: Int -> Int
    calcFuelFull val = case (calcFuel val) of
        0 -> 0
        x -> x + (calcFuelFull x)

    -- SOLVERS
    ----------
    solve :: (Int -> Int) -> String -> String
    solve f = show . sum . map f . map (read :: String -> Int) . lines

    solve1 :: String -> String
    solve1 = solve calcFuel

    solve2 :: String -> String
    solve2 = solve calcFuelFull
