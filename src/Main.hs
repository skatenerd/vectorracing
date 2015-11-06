{-# LANGUAGE FlexibleContexts #-}

module Main where

import UI.NCurses
import Data.Maybe
import Control.Monad
import Control.Monad.State
import System.Posix.Unistd
import GameTypes
import Geometry
import qualified Rendering as R
import Car
import Ai

import Control.Monad.Loops
import Control.Monad.Reader

data GameConfig = GameConfig { getCourse :: Course }

main :: IO ()
main = runCurses $ void (runStateT top startGameState)

startCarState = CarState (Point 8 (-2)) (Vector 0 0) (Point 8 (-2))
startAIState = CarState (Point 7 2) (Vector 0 0) (Point 7 2)
startGameState = GameState {humanState = startCarState, aiState = startAIState, quitted = False}
thecourse = Course { path = [
  (Point 8 0),
  (Point 38 0),
  (Point 46 8),
  (Point 46 18),
  (Point 38 26),
  (Point 8 26),
  (Point 0 18),
  (Point 0 8),
  (Point 8 0)],
  obstacles=[]}
hardCodedConfig = GameConfig thecourse

top = do
  lift $ setEcho False
  w <- lift defaultWindow
  (rows, cols) <- lift screenSize
  lift $ updateWindow w $ do
      moveCursor (rows - 1) 0
      drawString $ "Enter Move:"
  lift render
  untilM (runReaderT (gameLoop w rows) hardCodedConfig) (crash (getCourse hardCodedConfig)) >> showMessage w

showMessage w = do
  sad <- lift $ newWindow 10 50 10 10
  lift $ updateWindow sad $ do
      moveCursor 0 0
      drawString $ "GAME OVER"
  lift render
  lift $ getEvent sad $ Just 3000
  lift $ updateWindow sad $ do
      moveCursor 1 0
      drawString $ "ENTER TO QUIT"
  lift render
  lift $ awaitEnter w

awaitEnter w = untilM render $ do
  key <- getEvent w Nothing
  return $ (key == Just (EventSpecialKey KeyEnter)) || (key == Just (EventCharacter '\n'))

crash course = do
  gamestate <- get
  return $ (quitted gamestate) || hitsCourse (lastSegmentTravelled (humanState gamestate)) course

makeCID Dust = newColorID ColorYellow ColorBlack 1
makeCID Car = newColorID ColorCyan ColorBlack 3
makeCID Earth = newColorID ColorGreen ColorBlack 4
makeCID Wall = newColorID ColorYellow ColorBlack 5
makeCID Opponent = newColorID ColorRed ColorBlack 6

drawCharacter w (character, color) = do
   cid <- makeCID color --newColorID ColorBlue ColorGreen 1
   updateWindow w $ do
     setColor cid
     drawString [character]

drawColorfulString characters w = forM_ characters $ drawCharacter w

gameLoop w rows = do
  c <- lift $ lift $ getCharInput w
  course <- asks getCourse
  case c of
      Quit -> do
        oldstate <- get
        put oldstate {quitted = True}
      MoveDirection d -> do
        oldstate <- get
        let newstate = updateState oldstate d course
        put newstate
        lift $ lift $ do
          cid <- makeCID Dust --newColorID ColorBlue ColorGreen 1
          updateWindow w $ do
            clear
            moveCursor (rows - 1) 0
            drawString $ "Enter Move: " ++ (show c)
            moveCursor 0 0
          drawColorfulString (R.render newstate course) w
        lift $ lift render

updateHumanState state delta = state { humanState = takeCarTurn (humanState state) delta }
updateAIState state delta = state { aiState = takeCarTurn (aiState state) delta }

updateState s humanDirection course = let aIdirection = getMove course (aiState s)--head history
                                          withHumanMove = updateHumanState s humanDirection
                                          withAIMove = updateAIState withHumanMove aIdirection
                                      in withAIMove

getCharInput w = loop where
  loop = do
    ev <- getEvent w Nothing
    case ev of
        Nothing -> loop
        Just (EventCharacter 'q') -> return Quit
        Just (EventSpecialKey KeyUpArrow) -> return $ MoveDirection Up
        Just (EventSpecialKey KeyDownArrow) -> return $ MoveDirection Down
        Just (EventSpecialKey KeyLeftArrow) -> return $ MoveDirection LLeft
        Just (EventSpecialKey KeyRightArrow) -> return $ MoveDirection RRight
        _ -> loop
