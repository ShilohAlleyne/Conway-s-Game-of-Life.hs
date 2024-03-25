module Main where

import Control.Concurrent (threadDelay)
import System.Console.ANSI (setCursorPosition)
import qualified Data.Set as S

-- Constants
gridsize :: Int
gridsize = 50

-- Initial grid
grid :: [[Int]]
grid = replicate gridsize (replicate gridsize 0)

-- Helper Functions
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex _ _ [] = []
replaceAtIndex n item ls 
    | n < 0 || n >= length ls = ls
    | otherwise = a ++ (item:b) where (a, _ : b) = splitAt n ls

getCell :: Int -> Int -> [[Int]] -> Int
getCell x y g = (g !! y) !! x

isInGrid :: Int -> Int -> Bool
isInGrid x y
    | x < 0 || y < 0 = False -- Can't have negative coords
    | x > gridsize - 1 || y > gridsize - 1 = False -- Can't have coords outside the grid
    | otherwise = True

--------------------------------------------------[Game Logic]------------------------------------------------
spawn :: [[Int]] -> (Int, Int, Int) -> [[Int]]
spawn g (x, y, s) = replaceAtIndex y (replaceAtIndex x s (g !! y)) g

getAliveCells :: [[Int]] -> [(Int, Int)]
getAliveCells g = [(x, y) | x <- [0..length g - 1], y <- [0..length (head g) - 1], getCell x y g == 1]

countNieghbours :: Int -> Int -> [[Int]] -> Int
countNieghbours x y g = sum [if isInGrid (x + dx) (y + dy) then getCell (x + dx) (y + dy) g else 0 | 
                             dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]

getNCoords :: (Int, Int) -> [(Int, Int)]
getNCoords (x, y) = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], isInGrid (x+dx) (y + dy)]

aliveRules :: Int -> Int -> [[Int]] -> Int
aliveRules x y g
  | countNieghbours x y g > 3 = 0 -- Over Population
  | countNieghbours x y g < 2 = 0 -- Under Population
  | otherwise = 1

deadRules :: Int -> Int -> [[Int]] -> Int
deadRules x y g
  | countNieghbours x y g == 3 = 1 -- Birth
  | otherwise = 0

rules :: [[Int]] -> (Int, Int) -> Int
rules g (x, y)  
    | getCell x y g == 1 = aliveRules x y g
    | otherwise = deadRules x y g

renderGeneration :: [[Int]] -> [[Int]]
renderGeneration cells =
    let aliveCells = S.fromList $ concatMap getNCoords (getAliveCells cells)
        coords     = [(x, y, rules cells (x, y)) | (x, y) <- S.toList aliveCells]
     in foldl spawn grid coords

------------------------------------------------[IO Functions]---------------------------------------------
addBorder :: [[Char]] -> [[Char]]
addBorder g = border : map (\ row -> '|' : row ++ "|") g ++ [border]
    where border = '+' : replicate (length (head g)) '-' ++ "+"

cellToChar :: Int -> Char
cellToChar c = case c of
                 0 -> ' '
                 1 -> '#'
                 _ -> ' '

charBuffer :: [[Int]] -> [[Char]]
charBuffer = map (map cellToChar)

pprint :: [[Char]] -> IO ()
pprint array = mapM_ putStrLn [unwords [char : "" | char <- row] | row <- array]

runGame :: [[Int]] -> Int -> IO ()
runGame _ 0 = return ()
runGame g r = do
    setCursorPosition 0 0
    pprint $ addBorder $ charBuffer g
    threadDelay 125000 -- wait for 1 second
    runGame (renderGeneration g) (r-1)

main :: IO ()
main = do

    let gliderGunCells = [
          -- left block
          (0, 10, 1),
          (1, 10, 1),
          (0, 11, 1),
          (1, 11, 1),
          -- Egg thing
          (10, 10, 1),
          (10, 11, 1),
          (10, 12, 1),
          (11, 13, 1),
          (12, 14, 1),
          (13, 14, 1),
          (15, 13, 1),
          (16, 12, 1),
          (16, 11, 1),
          (16, 10, 1),
          (17, 11, 1),
          (14, 11, 1),
          (15, 9, 1),
          (13, 8, 1),
          (12, 8, 1),
          (11, 9, 1),
          -- right wing
          (20, 10, 1),
          (20, 9, 1),
          (20, 8, 1),
          (21, 10, 1),
          (21, 9, 1),
          (21, 8, 1),
          (22, 7, 1),
          (24, 7, 1),
          (24, 6, 1),
          (22, 11, 1),
          (24, 11, 1),
          (24, 12, 1),
          -- right block
          (34, 8, 1),
          (34, 9, 1),
          (35, 8, 1),
          (35, 9, 1)]
        
        cells = foldl spawn grid gliderGunCells
    
    runGame cells 5000
