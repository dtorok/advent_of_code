import Utils

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Text.ParserCombinators.ReadP


data Claim = Claim Int Int Int Int Int deriving (Show, Eq, Ord)
type ClaimMap = M.Map (Int, Int) [Claim]

claims :: ReadS Claim
claims = readP_to_S $ Claim
  <$> parseNextNumber -- id
  <*> parseNextNumber -- left
  <*> parseNextNumber -- top
  <*> parseNextNumber -- width
  <*> parseNextNumber -- height

coords :: Claim -> [(Int, Int)]
coords (Claim _ l t w h) = (,) <$> range t h <*> range l w

addClaimToMap :: ClaimMap -> Claim -> ClaimMap
addClaimToMap stats claim = update stats (coords claim)
  where
    update st [] = st
    update st (c:cs) = update st' cs
      where
        current = M.findWithDefault [] c st
        st' = M.insert c (claim : current) st

createClaimMap :: [Claim] -> ClaimMap
createClaimMap = L.foldl addClaimToMap M.empty

parseToClaimMap :: String -> ClaimMap
parseToClaimMap = createClaimMap . parse claims . lines

solve1 :: String -> String
solve1 input = show . length . M.filter ((> 1) . length) . parseToClaimMap $ input

solve2 :: String -> String
solve2 input = show $ allClaims `S.difference` overlappingClaims
  where
    claimMap :: ClaimMap
    claimMap = parseToClaimMap input

    toClaimSet :: ClaimMap -> S.Set Claim
    toClaimSet = S.fromList . concat . M.elems

    allClaims :: S.Set Claim
    allClaims = toClaimSet claimMap

    overlappingClaims :: S.Set Claim
    overlappingClaims = toClaimSet . M.filter ((> 1) . length) $ claimMap


main :: IO ()
main = do
  solve "../2018/day3.input" "03/01" solve1
  solve "../2018/day3.input" "03/02" solve2
