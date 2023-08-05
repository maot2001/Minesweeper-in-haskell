import Graphics.Gloss

import Initial
import Input
import State

initial :: GameState
initial = GameState { gameState = Retry, sideOfSquare = 10, numberOfMines = 10 }

window :: Display
window = InWindow "MyMi" (screenSide, screenSide) (0, 0)

main :: IO ()
--play :: Display -> Color -> Int -> world -> (world -> Picture) -> (Event -> world -> world) (Float -> world -> world)
main = play window white 10 initial draw handleIn update