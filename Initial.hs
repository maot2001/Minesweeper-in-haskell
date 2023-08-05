module Initial where

import System.Random
import Data.List

import State

-- Metodos Auxiliares

generateCoords :: Int -> Int -> [[(Int, Int)]]
generateCoords w h = [[(x, y) | y <- [0..h-1]] | x <- [0..w-1]]

---------------------

-- Ubicar minas

minePos :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
minePos 0 _ _ = []
minePos _ _ [] = []
minePos n seconds myList = [x] ++ minePos (n - 1) (seconds + 1) newList
  where
    ind = randomRs (0, (length myList - 1)) (mkStdGen (seconds)) !! 0
    x = myList !! ind
    newList = filter (\y -> y /= x) myList   

---------------                   

-- Numeros del tablero

getNeighbors :: [[Int]] -> (Int, Int) -> [(Int, Int)]
getNeighbors values (x, y) = filter isValid neighbors
  where
    neighbors = [(x-1,y-1),(x,y-1),(x+1,y-1),
                 (x-1,y),          (x+1,y),
                 (x-1,y+1),(x,y+1),(x+1,y+1)]
    isValid (x', y') = x' >= 0 && x' < length values &&
                       y' >= 0 && y' < length (head values) &&
                       values !! x' !! y' /= -1

tuplesToCountedTuples :: [(Int, Int)] -> [((Int, Int), Int)]
tuplesToCountedTuples tuples = map (\group -> (head group, length group)) (group (sort tuples))

correctValue :: [[Int]] -> ((Int, Int), Int) -> [[Int]]
correctValue matrix ((x,y),v) = 
  let (top, row:bottom) = splitAt x matrix
      (left, _:right) = splitAt y row
  in top ++ [left ++ [v] ++ right] ++ bottom

----------------------

initialGame :: Int -> GameState -> GameState
initialGame seconds GameState { sideOfSquare = s, numberOfMines = n } = GameState
 { board = board, gameState = InProgress, values = final, sideOfSquare = s, numberOfMines = n }  
  where
    board = replicate s (replicate s Covered)
    minePositions = minePos n seconds (concat $ generateCoords s s)
    newBoard = updated_matrix (replicate s (replicate s 0)) (-1) minePositions
    neighbors = concat $ map (getNeighbors newBoard) minePositions
    final = foldl' correctValue newBoard $ tuplesToCountedTuples neighbors