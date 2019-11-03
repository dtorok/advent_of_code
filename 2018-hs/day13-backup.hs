import Utils
import Prelude hiding (lookup)
-- import Data.Matrix (Matrix, fromLists, (!), setElem)
-- import Data.Sequence (Seq, index, adjust, update, mapWithIndex)
import Data.Map.Strict (Map, insert, empty, (!), fromList, toList, findWithDefault, lookup)
import Data.Maybe (isJust, fromJust)
import Data.List (elemIndex)

import Debug.Trace

---- Models
-- Helper
type Coord = (Int, Int)
type Size = (Int, Int)

-- Cart
data CartDir = U | R | D | L deriving (Eq, Ord, Show)
data CartLastTurn = CartTurnLeft | CartStraight | CartTurnRight deriving Show
data Cart = Cart CartDir CartLastTurn deriving Show

-- Road
data Road = H | V | TL | TR | X deriving (Eq, Ord, Show)

-- Cell
type Cell = (Maybe Road, Maybe Cart)

-- Board
type Board = Map Coord Cell

-- Game
data Game = Game Size Board


---- Helpers
-- reverseMap
reverseMap :: (Ord b) => Map a b -> Map b a
reverseMap = fromList . map (\(a, b) -> (b, a)) . toList

---- Parsers
-- parseCart
cartDirMap :: Map Char CartDir
cartDirMap = fromList 
    [ ('^', U)
    , ('>', R)
    , ('v', D)
    , ('<', L)
    ]

cartDirMapRev :: Map CartDir Char
cartDirMapRev = reverseMap cartDirMap

parseCartDir :: Char -> Maybe CartDir
parseCartDir c = lookup c cartDirMap 

parseCart :: Char -> Maybe Cart
parseCart c = do
    cartDir <- parseCartDir c
    return $ Cart cartDir CartTurnLeft

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

-- parseCell
parseCell :: Char -> Cell
parseCell c = (parseRoad c, parseCart c)

-- parseGame
parseInput :: String -> Game
parseInput input = Game size board
  where
    inputMatrix = lines input
    size = (rows, cols)
      where
        rows = length inputMatrix
        cols = maximum (map length inputMatrix)
    board = foldl parseRow empty . zip [0..] $ inputMatrix
      where
        parseRow :: Board -> (Int, String) -> Board
        parseRow board (r, row) = foldl parseItem board . zip [0..] $ row
          where
            parseItem :: Board -> (Int, Char) -> Board
            parseItem board (c, item) = insert (r, c) (parseCell item) board

---- Printing
-- printCell
printCell :: Cell -> Char
printCell (_, Just (Cart cartDir _)) = cartDirMapRev ! cartDir
printCell (Just road, Nothing) = roadMapRev ! road
printCell (Nothing, Nothing) = ' '

printGame :: Game -> String
printGame (Game (rows, cols) board) = unlines ("\n\n": (map printRow [0..rows-1]))
    where
        printRow r = map (printCell' r) [0..cols-1]
        printCell' r c = printCell (getCell board (r, c)) -- (findWithDefault (Nothing, Nothing) (r, c) board)

instance Show Game where
    show = printGame

---- Stepper
step :: Game -> Game
step (Game (rows, cols) board) = Game (rows, cols) board'
  where
    board' = foldl stepCell board ((,) <$> [0..rows-1] <*> [0..cols-1])

    stepCell :: Board -> Coord -> Board
    stepCell board coord = 
        if isCart board coord
            then stepCart board coord 
            else board
    
    isCart :: Board -> Coord -> Bool
    isCart board coord = isJust . snd $ getCell board coord

    stepCart :: Board -> Coord -> Board
    stepCart board coord = 
        let
            cart = getCart board coord
            coord' = traceShowId $ moveCart coord cart
            cart' = turnCart board coord' cart
        in
            addCart coord' cart' . removeCart coord $ board

getCell :: Board -> Coord -> Cell
getCell board coord = findWithDefault (Nothing, Nothing) coord board

getRoad :: Board -> Coord -> Road
getRoad board coord = case board ! coord of
    (Just road, _) -> road

getCart :: Board -> Coord -> Cart
getCart board coord = case board ! coord of
    (_, Just cart) -> cart

addCart :: Coord -> Cart -> Board -> Board
addCart coord cart board = insert coord (road, Just cart) board
    where
        (road, _) = board ! coord
        
removeCart :: Coord -> Board -> Board
removeCart coord board = insert coord (road, Nothing) board
    where
        (road, _) = board ! coord

moveCart :: Coord -> Cart -> Coord
moveCart (r, c) (Cart dir _) = case dir of
    U -> (r - 1, c)
    R -> (r, c + 1)
    D -> (r + 1, c)
    L -> (r, c - 1)

turnCart :: Board -> Coord -> Cart -> Cart
turnCart board coord cart@(Cart dir lt) = 
    let
        road = getRoad board coord
        left = [U, L, D, R, U]
        turnLeft d = left !! ((fromJust $ elemIndex d left) + 1)
        right = reverse left
        turnRight d = right !! ((fromJust $ elemIndex d right) + 1)

        dir' = case lt of
            CartTurnLeft -> turnLeft dir
            CartTurnRight -> turnRight dir
            CartStraight -> dir
        lt' = case lt of
            CartTurnLeft -> CartStraight
            CartStraight -> CartTurnRight
            CartTurnRight -> CartTurnLeft
        cart' = Cart dir' lt'
    in
        case (road, dir) of
            (X, _) -> cart'
            (TL, D) -> Cart (turnLeft dir) lt
            (TL, U) -> Cart (turnLeft dir) lt
            (TL, R) -> Cart (turnRight dir) lt
            (TL, L) -> Cart (turnRight dir) lt
            (TR, U) -> Cart (turnRight dir) lt
            (TR, D) -> Cart (turnRight dir) lt
            (TR, L) -> Cart (turnLeft dir) lt
            (TR, R) -> Cart (turnLeft dir) lt
            _ -> cart


-- newCoord :: Board Board -> Coord -> Coord
-- newCoord board coord = go (board ! coord)
--     where
--         go (Just H, Just U) (c, r) = 



solve1 :: String -> String
solve1 input = show $ foldl (\g _ -> step . traceShowId $ g) (parseInput input) [0..13]
-- solve1 = show . step . traceShowId . step . traceShowId . step . traceShowId . step . traceShowId . parseInput
-- solve1 input = show . step . traceShowId . h . traceShowId . f . traceShowId . g . traceShowId . parseInput $ input
    where
        test game = (printGame $ game) ++ (printGame $ (f . g) game)
        f (Game size board) = Game size (removeCart (0, 2) board)
        g (Game size board) = Game size (addCart (0, 3) (getCart board (0, 2)) board)
        h (Game size board) = Game size board'
            where
                coord = (3, 9)
                coord' = moveCart coord cart
                cart = getCart board coord
                cart' = turnCart board coord' cart
                board' = removeCart coord . addCart coord' cart' $ board

main :: IO ()
main = solve "../2018/day13.example" "13/01" solve1
