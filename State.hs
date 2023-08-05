module State where

import Data.List
import Graphics.Gloss
import System.IO.Unsafe

-- Tipos de datos

data CellState = Covered | Uncovered | Flagged | Mine deriving (Eq, Show)
data GameResult = InProgress | Won | Lost | Retry deriving (Eq, Show)
type Board = [[CellState]]

data GameState = GameState
    { board :: Board,
      gameState :: GameResult,
      values :: [[Int]],
      sideOfSquare :: Int,
      numberOfMines :: Int
    } deriving (Eq, Show)

-----------------

-- Modificador general

updated_matrix :: [[a]] -> a -> [(Int, Int)] -> [[a]]
updated_matrix matrix cellState tuples = foldl' (\acc (x, y) -> updateMatrix acc x y cellState) matrix tuples
  where
    updateMatrix matrix x y cst = take x matrix ++ [take y (matrix !! x) ++ [cst] ++ drop (y + 1) (matrix !! x)] ++ drop (x + 1) matrix

----------------------

-- Dimensiones de pantalla
screenSide :: Int
screenSide = 800
cellSize :: Float
cellSize = 20
margin :: Float
margin = 10

--------------------------

-- Colores de pantalla

uncoveredColor :: Color
uncoveredColor = makeColor 0.678 0.847 0.902 1.0
coveredColor :: Color
coveredColor = makeColor 0.098 0.098 0.439 1.0
flaggedColor :: Color
flaggedColor = red
mineColor :: Color
mineColor = black
textColor :: Color
textColor = red

----------------------

-- Imagenes de pantalla

loadFont :: IO Picture
loadFont = do
  img <- loadBMP "font.bmp"
  return $ scale 2.0 2.0 img
playFont :: Picture
playFont = unsafePerformIO loadFont

loadFlag :: IO Picture
loadFlag = do
  img <- loadBMP "flag.bmp"
  return $ scale 0.05 0.05 img
playFlag :: Picture
playFlag = unsafePerformIO loadFlag

loadMine :: IO Picture
loadMine = do
  img <- loadBMP "mine.bmp"
  return $ scale 0.04 0.04 img
playMine :: Picture
playMine = unsafePerformIO loadMine

-----------------------

draw :: GameState -> Picture
draw GameState { board = board, gameState = InProgress, values = values } = 
  pictures [playFont, translate (-boardOffsetX) (-boardOffsetY) $ pictures $ map drawRow (zip board [0..])]
  where
    w = length board
    width = if (mod w 2 == 0) then w else w - 1
    boardOffsetX = fromIntegral width * cellSize / 2
    boardOffsetY = fromIntegral width * cellSize / 2
    drawRow :: ([CellState], Int) -> Picture
    drawRow (row, rowIndex) = pictures $ map (drawCell rowIndex) (zip row [0..])
    drawCell :: Int -> (CellState, Int) -> Picture
    drawCell rowIndex (cellState, colIndex) = translate x y $ pictures [cellShape, cellContent]
      where
        x = margin + cellSize * fromIntegral colIndex
        y = margin + cellSize * fromIntegral rowIndex
        cellShape = color (if cellState == Covered then coveredColor else uncoveredColor) $ rectangleSolid cellSize cellSize
        cellContent = case cellState of
          Covered -> blank
          Uncovered -> case values !! rowIndex !! colIndex of
            -1 -> color mineColor $ circleSolid (cellSize / 2)
            0 -> blank
            n -> color mineColor $ translate (-5) (-7.5) $ scale 0.15 0.15 $ text $ show n
          Flagged -> playFlag
          Mine -> playMine
draw GameState { board = board, gameState = Won } = pictures [playFont,
  translate (-400) 200 $ scale tam tam $ color textColor $ text ("You Win!!!"),
  translate (-400) 0 $ scale tam tam $ color textColor $ text ("Press A to Retry!"),
  translate (-400) (-200) $ scale tam tam $ color textColor $ text ("Press Z to Exit!")]
  where
    w = length board
    width = if (mod w 2 == 0) then w else w - 1
    tam = fromIntegral width / (cellSize * 4)
draw GameState { board = board, gameState = Lost, values = values } = pictures [playFont,
  translate (-boardOffsetX) (-boardOffsetY) $ pictures $ map drawRow (zip board [0..]),
  translate pos3 pos1 $ scale tam1 tam1 $ color textColor $ text ("Game Over!"),
  translate (pos3 - 200) pos2 $ scale tam2 tam2 $ color textColor $ text ("Press A to Retry!"),
  translate (pos3 + 200) pos2 $ scale tam2 tam2 $ color textColor $ text ("Press Z to Exit!")]
  where
    w = length board
    width = if (mod w 2 == 0) then w else w - 1
    pos1 = fromIntegral width * cellSize / 2 + 30
    pos2 = - 50 - fromIntegral width * cellSize / 2 
    pos3 = - fromIntegral width * cellSize / 3
    tam1 = fromIntegral width / (cellSize * 4)
    tam2 = fromIntegral width / (cellSize * 5)
    boardOffsetX = fromIntegral width * cellSize / 2
    boardOffsetY = fromIntegral width * cellSize / 2
    drawRow :: ([CellState], Int) -> Picture
    drawRow (row, rowIndex) = pictures $ map (drawCell rowIndex) (zip row [0..])
    drawCell :: Int -> (CellState, Int) -> Picture
    drawCell rowIndex (cellState, colIndex) = translate x y $ pictures [cellShape, cellContent]
      where
        x = margin + cellSize * fromIntegral colIndex
        y = margin + cellSize * fromIntegral rowIndex
        cellShape = color (if cellState == Covered then coveredColor else uncoveredColor) $ rectangleSolid cellSize cellSize
        cellContent = case cellState of
          Covered -> blank
          Uncovered -> case values !! rowIndex !! colIndex of
            -1 -> color mineColor $ circleSolid (cellSize / 2)
            0 -> blank
            n -> color mineColor $ translate (-5) (-7.5) $ scale 0.15 0.15 $ text $ show n
          Flagged -> playFlag
          Mine -> playMine
draw GameState { gameState = Retry , sideOfSquare = s, numberOfMines = m} = pictures [playFont,
  translate (-400) 300 $ scale tam tam $ color textColor $ text ("Side of the square: " ++ show (s)),
  translate (-400) 230 $ scale tam tam $ color textColor $ text ("Press S to up"),
  translate (-400) 160 $ scale tam tam $ color textColor $ text ("  or D to down"),
  translate (-400) 10 $ scale tam tam $ color textColor $ text ("Number of mines: " ++ show (m)),
  translate (-400) (-60) $ scale tam tam $ color textColor $ text ("Press X to up"),
  translate (-400) (-130) $ scale tam tam $ color textColor $ text ("  or C to down"),
  translate (-400) (-280) $ scale tam tam $ color textColor $ text ("Press A to start")]
  where
    w = s
    width = if (mod w 2 == 0) then w else w - 1
    tam = fromIntegral width / (cellSize * 4)

update :: Float -> GameState -> GameState
update _ gameState = gameState 