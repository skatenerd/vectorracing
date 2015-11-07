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
import Course

import Control.Monad.Loops
import Control.Monad.Reader
import Data.List
import Data.Function

data GameConfig = GameConfig { getCourse :: Course }

main :: IO ()
main = runCurses $ void (runStateT top startGameState)

startCarState = CarState [Point 8 (-2), Point 8 (-2)]
startAIState = CarState [Point 7 2, Point 7 2]
startGameState = GameState {humanState = startCarState, aiState = startAIState, quitted = False}
thecourse = makeCourse [
  (Point 8 0),
  (Point 38 0),
  (Point 46 8),
  (Point 46 18),
  (Point 38 26),
  (Point 8 26),
  (Point 0 18),
  (Point 0 8),
  (Point 7 0),
  (Point 8 0)]
  []
hardCodedConfig = GameConfig thecourse

top = do
  lift $ setEcho False
  w <- lift defaultWindow
  (rows, cols) <- lift screenSize
  lift $ updateWindow w $ do
      moveCursor (rows - 1) 0
      drawString $ "Enter Move:"
  lift render
  untilM (runReaderT (gameLoop w rows) hardCodedConfig) (over (getCourse hardCodedConfig)) >> showEndState w (getCourse hardCodedConfig) >> gameOverMessage w (getCourse hardCodedConfig)

drawCourse course state w = do
  updateWindow w $ do
    moveCursor 0 0
  --let clumped = group (R.renderInto state course 25 43)
  let clumped = group (R.render state course)
  forM_ clumped (\s -> drawColorfulString w (map fst s) (snd $ head s))
  render

showEndState w course = do
  endstate <- get
  lift $ drawCourse course endstate w

gameOverMessage w course = do
  sad <- lift $ newWindow 10 50 10 10
  lift $ updateWindow sad $ do
      moveCursor 0 0
      drawString $ "GAME OVER\n"
  endState <- get
  let humanFinished = (humanWon course endState)
      aiFinished = (aiWon course endState)
      tie = humanFinished && aiFinished
      humanVictory = humanFinished && not aiFinished
      aiVictory = aiFinished && not humanFinished
  when humanVictory $ lift $ updateWindow sad $ do
    drawString "YOU BEAT THE COMPUTER!!!\n"
  when aiVictory $ lift $ updateWindow sad $ do
    drawString "YOU LOST.  TO A ROBOT\n"
  when tie $ lift $ updateWindow sad $ do
    drawString "A TIE? IMPOSSIBLE!\n"
  when (hitsCourse (lastSegmentTravelled (humanState endState)) course) $ lift $ updateWindow sad $ do
    drawString "YOU CRASHED, IDIOT\n"
  lift render
  lift $ getEvent sad $ Just 3000
  lift $ updateWindow sad $ do
      moveCursor 4 0
      drawString $ "ENTER TO QUIT"
  lift render
  lift $ awaitEnter w

awaitEnter w = untilM render $ do
  key <- getEvent w Nothing
  return $ (key == Just (EventSpecialKey KeyEnter)) || (key == Just (EventCharacter '\n'))

over course = do
  gamestate <- get
  return $ (quitted gamestate) || hitsCourse (lastSegmentTravelled (humanState gamestate)) course || someoneWon course gamestate

playerWon course playerState = totalProgress >= (length $ pointsAlong course)
                               where totalProgress = progress course (positionHistory playerState)

someoneWon course gameState =playerWon course (aiState gameState) || playerWon course (humanState gameState)
aiWon course gameState = playerWon course (aiState gameState)
humanWon course gameState = playerWon course (humanState gameState)

makeCID Dust = newColorID ColorYellow ColorBlack 1
makeCID Car = newColorID ColorCyan ColorBlack 3
makeCID Earth = newColorID ColorGreen ColorGreen 4
makeCID Wall = newColorID ColorYellow ColorBlack 5
makeCID Opponent = newColorID ColorRed ColorBlack 6
makeCID Road = newColorID ColorBlack ColorBlack 7
makeCID Finish = newColorID ColorBlue ColorBlue 8
textColor = newColorID ColorBlack ColorWhite 9

drawColorfulString w string color = do
   cid <- makeCID color --newColorID ColorBlue ColorGreen 1
   updateWindow w $ do
     setColor cid
     drawString string

gameLoop w rows = do
  course <- asks getCourse
  oldstate <- get
  lift $ lift $ drawCourse course oldstate w
  c <- lift $ lift $ getCharInput w
  case c of
      Quit -> do
        oldstate <- get
        put oldstate {quitted = True}
      MoveDirection d -> do
        lift $ lift $ do
          cid <- textColor
          updateWindow w $ do
            setColor cid
            moveCursor (rows - 1) 0
            clearLine
            drawString $ "Enter Move: " ++ (show c)
          render
        oldstate <- get
        let newstate = updateState oldstate d course
        put newstate

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
