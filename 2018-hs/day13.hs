import Utils
import Data.Map.Strict (Map, empty, insert, (!)) --, insert, empty, (!), fromList, toList, findWithDefault, lookup)
import qualified Data.Map.Strict as M
import Data.List (sortOn)


-- DATA
-------
data RoadSegment = H | V | TL | TR | X deriving (Show, Eq)
type Coord = (Int, Int)
type CartDir = Coord
type Road = Map Coord RoadSegment
type Game = (Road, [Cart])

data Cart = Cart 
    { cartCoord :: Coord
    , cartDir   :: CartDir
    , nextTurns  :: [(Coord -> Coord)]
    }

instance Show Cart where
    show c = show (cartCoord c) ++ " - " ++ show (cartDir c)

-- DIRECTIONS
-------------
up :: Coord
up = (-1, 0)

right :: Coord
right = (0, 1)

down :: Coord
down = (1, 0)

left :: Coord
left = (0, -1)

add :: Coord -> Coord -> Coord
add (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

turnRight :: CartDir -> CartDir
turnRight (r, c) = (c, -r)

turnLeft :: CartDir -> CartDir
turnLeft (r, c) = (-c, r)

-- PARSER
---------
parseRoadSegment :: Char -> Maybe RoadSegment
parseRoadSegment '-'  = Just H
parseRoadSegment '>'  = Just H
parseRoadSegment '<'  = Just H
parseRoadSegment '|'  = Just V
parseRoadSegment 'v'  = Just V
parseRoadSegment '^'  = Just V
parseRoadSegment '/'  = Just TR
parseRoadSegment '\\' = Just TL
parseRoadSegment '+'  = Just X
parseRoadSegment _    = Nothing

parseCartDir :: Char -> Maybe CartDir
parseCartDir '^' = Just up                                                                                                                                                                                                                                                                                                                                      
parseCartDir '>' = Just right
parseCartDir 'v' = Just down
parseCartDir '<' = Just left
parseCartDir _   = Nothing

parseCart :: (Coord, CartDir) -> Cart
parseCart (coord, cartDir) = Cart { cartCoord = coord, cartDir = cartDir, nextTurns = take 99999 (cycle [turnLeft, id, turnRight])}

parseRoad :: String -> Road
parseRoad = M.mapMaybe parseRoadSegment . rowsToMap

parseCarts :: String -> [Cart]
parseCarts = map parseCart . M.toList . M.mapMaybe parseCartDir . rowsToMap

parseGame :: String -> Game
parseGame input = (parseRoad input, parseCarts input)

-- CRASHES
----------
isCartAt :: Coord -> [Cart] -> Bool
isCartAt c carts = any id $ map f carts
    where
        f :: Cart -> Bool
        f cart = cartCoord cart == c

crashCoord :: [Cart] -> Maybe Coord
crashCoord (a:[]) = Nothing
crashCoord (a:b:xs) =
    if cartCoord a == cartCoord b then
        Just (cartCoord a)
    else crashCoord (b:xs)

removeCartsAt :: Coord -> [Cart] -> [Cart]
removeCartsAt c carts = filter f carts
    where
        f :: Cart -> Bool
        f cart = cartCoord cart /= c

-- MOVE CARTS
-------------
turnCartAt :: RoadSegment -> Cart -> Cart
turnCartAt rs cart = cart { cartDir = cd', nextTurns = nt'}
    where
        cd = cartDir cart
        nt = nextTurns cart

        cd' = 
            if rs == TR then
                if cd `elem` [up, down] then turnRight cd
                else turnLeft cd
            else if rs == TL then
                if cd `elem` [up, down] then turnLeft cd
                else turnRight cd
            else if rs == X then
                (head nt) cd
            else
                cd
        
        nt' = 
            if rs == X then
                drop 1 nt
            else
                nt

turnCart :: Road -> Cart -> Cart
turnCart road cart = turnCartAt (road ! (cartCoord cart)) cart

moveCart :: Cart -> Cart
moveCart cart = cart { cartCoord = (cartCoord cart) `add` (cartDir cart) }

moveAndRemoveCarts :: [Cart] -> [Cart] -> [Cart]
moveAndRemoveCarts result [] = result
moveAndRemoveCarts result (x:xs) = moveAndRemoveCarts result' xs' 
    where
        x' = moveCart x
        cx' = cartCoord x'
        xs' =
            if isCartAt cx' xs then
                removeCartsAt cx' xs
            else
                xs
        
        result' = 
            if isCartAt cx' result then
                removeCartsAt cx' result
            else if isCartAt cx' xs then
                result
            else
                x' : result

moveCarts :: [Cart] -> [Cart] -> [Cart]
moveCarts _ = map moveCart

-- SOLUTIONS
------------
update :: ([Cart] -> [Cart] -> [Cart]) -> Game -> Game
update fMoveCarts (road, carts) =
    (road, map (turnCart road) . fMoveCarts [] . sortOn cartCoord $ carts)

game1 :: Game -> Coord
game1 (road, carts) = 
    case crashCoord carts of
        Just c -> c
        Nothing -> game1 g'
    where
        g' = update moveCarts (road, carts)

game2 :: Game -> Coord
game2 (road, c:[]) = cartCoord c
game2 (road, carts) = game2 g'
    where
        g' = 
            case crashCoord carts of
                Just c -> (road, removeCartsAt c carts)
                Nothing -> update moveAndRemoveCarts (road, carts)
            
        
solve1 :: String -> String
solve1 = show . game1 . parseGame

solve2 :: String -> String
solve2 = show . game2 . parseGame

main :: IO ()
main = do
    solve "../2018/day13.input" "13/01" solve1
    solve "../2018/day13.input" "13/02" solve2
