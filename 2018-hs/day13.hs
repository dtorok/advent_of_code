import Utils
import Prelude hiding (lookup, Right, Left)
import Data.Map.Strict (Map, insert, empty, (!), fromList, toList, findWithDefault, lookup, keys)
import qualified Data.List as L
import qualified Data.Maybe as Maybe

type Size = (Int, Int)
type Coord = (Int, Int)

-- Cart
data CartDir = Up | Right | Down | Left deriving (Eq, Ord, Show)
data CartLastTurn = CartTurnLeft | CartStraight | CartTurnRight deriving Show
data Cart = Cart Coord CartDir CartLastTurn deriving Show

-- Road
data Road = H | V | TL | TR | X deriving (Eq, Ord, Show)

-- Game
type Roads = Map Coord Road
type Carts = [Cart]
data Game = Game Size Roads Carts


---- Helpers

---- Parsers
-- parseCart
cartDirMap :: Map Char CartDir
cartDirMap = fromList 
    [ ('^', Up)
    , ('>', Right)
    , ('v', Down)
    , ('<', Left)
    ]

cartDirMapRev :: Map CartDir Char
cartDirMapRev = reverseMap cartDirMap

parseCartDir :: Char -> Maybe CartDir
parseCartDir c = lookup c cartDirMap 

parseCart :: (Coord, Char) -> Maybe Cart
parseCart (coord, c) = do
    cartDir <- parseCartDir c
    return $ Cart coord cartDir CartTurnLeft

parseCarts :: [[Char]] -> Carts
parseCarts matrix = Maybe.mapMaybe parseCart (coordMatrix matrix)


-- parseRoad
roadMap :: Map Char Road
roadMap = fromList
    [ ('|' , H)
    , ('-' , V)
    , ('+' , X)
    , ('/' , TR)
    , ('\\', TL)
    ]

roadMapCart :: Map Char Road
roadMapCart = fromList
    [ ('^' , H)
    , ('>' , V)
    , ('v' , H)
    , ('<' , V)
    ]

roadMapRev :: Map Road Char
roadMapRev = reverseMap roadMap

-- Alright, this is blackmagic, but I can't come up with a better version now
-- Checks c in roadMap, and if it's not there, it takes from roadMapCart, or returns Nothing
parseRoad :: Char -> Maybe Road
parseRoad c = maybe (lookup c roadMapCart) Just (lookup c roadMap)

parseRoads :: [[Char]] -> Roads
parseRoads matrix = fromList $ Maybe.mapMaybe f (coordMatrix matrix)
    where
        f (coord, c) = 
            do
                road <- parseRoad c
                return $ (coord, road)

parseGame :: String -> Game
parseGame input = 
    let
        matrix = lines input
        size = (0, 0) -- I think I won't need this
        roads = parseRoads matrix
        carts = parseCarts matrix
    in
        Game size roads carts


---- Printing
--
printCell :: Roads -> Carts -> Coord -> Char
printCell roads carts coord =
     case (getCart carts coord, getRoad roads coord) of
        (Just (Cart _ d _), _) -> cartDirMapRev ! d
        (_,         Just road) -> roadMapRev ! road
        _ -> ' '

printGame :: Game -> String
printGame (Game _ roads carts) = '\n' : printMatrix
    where
        printMatrix :: String
        printMatrix = L.concat . L.intersperse "\n" $ map printRow [0..h]
        printRow :: Int -> String
        printRow c = map (printCell roads carts) ((,) <$> [c] <*> [0..w])
        h = L.maximum . map fst . keys $ roads
        w = L.maximum . map snd . keys $ roads
        


instance Show Game where
    show = printGame



---- Stepper
step :: Game -> Game
step (Game size roads carts) = Game size roads carts'
    where
        carts' :: [Cart]
        carts' = map (stepCart roads) carts

stepCart :: Roads -> Cart -> Cart
stepCart roads (Cart coord d lturn) = Cart coord' d' lturn'
    where
        coord' = stepCoord d coord
        road' = fromJust (getRoad roads coord')
        d' lturn' = stepDir road' d lturn

stepCoord :: CartDir -> Coord -> Coord
stepCoord d (r, c) =
    case d of
        Left -> (r, c - 1)
        Down -> (r + 1, c)
        Right -> (r, c + 1)
        Up -> (r - 1, c)

stepDir :: Road -> CartDir -> CartLastTurn -> CartDir -> CarLastTurn
stepDir road d lturn = case (road, d, lturn) of
    (

-- stepCart :: Roads -> Cart -> Cart
-- stepCart roads cart@(Cart coord _ _) = 
--     case (getRoad roads coord) of
--         Just H -> stepCartH cart
--         Just V -> stepCartV cart
--         Just X -> stepCartX cart
--         Just TL -> stepCartTL cart
--         Just TR -> stepCartTR cart

stepCartH :: Cart -> Cart
stepCartH (Cart coord d lturn) =
    case d of
        

stepCartV :: Cart -> Cart
stepCartV = id

stepCartX :: Cart -> Cart
stepCartX = id

stepCartTL :: Cart -> Cart
stepCartTL = id

stepCartTR :: Cart -> Cart
stepCartTR = id





-- helpers
--
coordMatrix :: [[a]] -> [(Coord, a)]
coordMatrix m =
    let
        f (r, row) = map (g r) (zip [0..] row)
        g r (c, item) = ((r, c), item)
    in
        concatMap f (zip [0..] m)

getCart :: Carts -> Coord -> Maybe Cart
getCart carts coord = L.find (\(Cart cc _ _) -> cc == coord) carts

getRoad :: Roads -> Coord -> Maybe Road
getRoad roads coord = lookup coord roads

-- reverseMap
reverseMap :: (Ord b) => Map a b -> Map b a
reverseMap = fromList . map (\(a, b) -> (b, a)) . toList

nextInList :: Eq a => [a] -> a -> a
nextInList xs

---- solver
solve1 :: String -> String
solve1 = show . step . parseGame

main :: IO ()
main = solve "../2018/day13.example" "13/01" solve1
-- main = putStr "ok"
