module Input where

import Graphics.Gloss.Interface.Pure.Game
import Data.List
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.IO.Unsafe (unsafePerformIO)

import State
import Initial

-- Para desbloquear los valores adyacentes a una Uncovered 0

getNeighborsPlus :: Board -> [[Int]] -> (Int, Int) -> [(Int, Int)]
getNeighborsPlus board values (x, y) = filter isValid neighbors
  where
    neighbors = [(x-1,y-1),(x,y-1),(x+1,y-1),
                 (x-1,y),          (x+1,y),
                 (x-1,y+1),(x,y+1),(x+1,y+1)]
    isValid (x', y') = x' >= 0 && x' < length values &&
                       y' >= 0 && y' < length (head values) &&
                       board !! x' !! y' /= Uncovered &&
                       board !! x' !! y' /= Flagged
                  
clean :: [[Int]] -> [(Int, Int)] -> [(Int, Int)]
clean values coords = filter cleaner coords
  where cleaner (x, y) = if values !! x !! y == 0 then True else False

zeroAdyacents :: Board -> [[Int]] -> [(Int, Int)] -> Board
zeroAdyacents board values [] = board
zeroAdyacents board values coords = zeroAdyacents newB values (nub newList)
  where
    newB = updated_matrix board Uncovered coords
    newcoords = clean values coords
    newList = concat $ map (getNeighborsPlus board values) newcoords

------------------------------------------------------------

-- Obtener casilla de click

getCellIndices :: (Float, Float) -> (Int, Int) -> (Int, Int)
getCellIndices (x, y) (w, h) = (floor (y / cellSize) + (div h 2), floor (x / cellSize) + (div w  2))

---------------------------

-- Finalizar juego

endGame :: GameState -> GameResult -> GameState
endGame (GameState board _ values side mines) result =
  let newBoard = [[if cell == Covered && values !! rowIndex !! colIndex == -1 then Mine else cell | (colIndex, cell) <- zip [0..] row] | (rowIndex, row) <- zip [0..] board]
  in GameState newBoard result values side mines

------------------

handleIn :: Event -> GameState -> GameState
handleIn (EventKey (MouseButton LeftButton) Down _ (x, y)) gameState =
  case gameState of
    GameState board InProgress values side mines ->
      let 
          width = length board
          height = length (head board)
          (rowIndex, colIndex) = getCellIndices (x, y) (width, height)
          cellState = board !! rowIndex !! colIndex
          newCellState = case cellState of
            Covered -> if values !! rowIndex !! colIndex == -1
              then Mine                                                             
              else Uncovered
            _ -> cellState
          newBoard = updated_matrix board newCellState [(rowIndex, colIndex)]
          newBoardTwo = zeroAdyacents board values [(rowIndex, colIndex)]
          newGameState = GameState newBoard InProgress values side mines
          newGameStateTwo = GameState newBoardTwo InProgress values side mines
      in if newCellState == Mine                                                      
        then endGame newGameState Lost
        else if all (\row -> all (\cell -> cell /= Covered || cell == Flagged) row) newBoard        
          then endGame newGameState Won
        else if values !! rowIndex !! colIndex == 0
          then newGameStateTwo
          else newGameState
    _ -> gameState
handleIn (EventKey (MouseButton RightButton) Down _ (x, y)) gameState =
  case gameState of
    GameState board InProgress values side mines ->
      let 
          width = length board
          height = length (head board)
          (rowIndex, colIndex) = getCellIndices (x, y) (width, height)
          cellState = board !! rowIndex !! colIndex
          newCellState = case cellState of
            Covered -> Flagged
            Flagged -> Covered
            _ -> cellState
          newBoard = updated_matrix board newCellState [(rowIndex, colIndex)]
          newGameState = GameState newBoard InProgress values side mines
      in if all (\row -> all (\cell -> cell /= Covered || cell == Flagged) row) newBoard        
        then endGame newGameState Won
        else newGameState
    _ -> gameState
handleIn (EventKey (Char c) Down _ _) GameState { gameState = Retry, sideOfSquare = s, numberOfMines = n}
  | c == 's' = GameState { gameState = Retry, sideOfSquare = min 40 (s + 1), numberOfMines = n}
  | c == 'd' = GameState { gameState = Retry, sideOfSquare = max 1 (s - 1), numberOfMines = n}
  | c == 'x' = GameState { gameState = Retry, sideOfSquare = s, numberOfMines = n + 1}
  | c == 'c' = GameState { gameState = Retry, sideOfSquare = s, numberOfMines = max 1 (n - 1)}
  | c == 'a' = initialGame seconds GameState {gameState = Retry , sideOfSquare = s, numberOfMines = n}
  where
    seconds = round $ utctDayTime (unsafePerformIO getCurrentTime)
handleIn (EventKey (Char c) Down _ _) GameState { gameState = Won, sideOfSquare = s, numberOfMines = n}
  | c == 'a' = GameState { gameState = Retry, sideOfSquare = s, numberOfMines = n}
  | c == 'z' = GameState { gameState = Retry }
handleIn (EventKey (Char c) Down _ _) GameState { gameState = Lost, sideOfSquare = s, numberOfMines = n}
  | c == 'a' = GameState { gameState = Retry, sideOfSquare = s, numberOfMines = n}
  | c == 'z' = GameState { gameState = Retry }
handleIn _ gameState = gameState