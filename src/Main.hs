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

startCarState = CarState (Point (-10) (-10)) (Vector 0 0) (Point (-10) (-10))
startAIState = CarState (Point (-12) (-12)) (Vector 0 0) (Point (-12) (-12))
startGameState = GameState {humanState = startCarState, aiState = startAIState, quitted = False}
thecourse = Course { path = [(Point (-10) (-10)), (Point 0 0), (Point 10 2), (Point 35 (-5)), (Point 45 15)], obstacles=[]}
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
        lift $ lift $ updateWindow w $ do
            moveCursor (rows - 1) 0
            drawString $ replicate 40 ' '--hack...
            moveCursor (rows - 1) 0
            drawString $ "Enter Move: " ++ (show c)
            moveCursor (rows - 1) 12
            moveCursor 0 0
            drawString $ R.render newstate course
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
