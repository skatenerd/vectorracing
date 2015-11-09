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
import Configs
import CursesWrappers
import Scoring

import Control.Monad.Loops
import Control.Monad.Reader
import Data.List
import Data.Function


main :: IO ()
main = void $ runCurses $ top

top = do
  setEcho False
  w <- defaultWindow
  updateWindow w $ do
    drawString "Choose a course!"
  render
  let startHeight = 3
  configIndex <- getCourseSelection startHeight
  let config = configs !! (fromIntegral configIndex)
      startState = startStateFromConfig config
      course = getCourse config
  (rows, cols) <- screenSize
  updateWindow w $ do
      moveCursor (rows - 1) 0
      drawString $ "Enter Move:"
  render
  (runStateT ((untilM (gameLoop w rows course) (over course)) >> (showEndState w course) >> (gameOverMessage w course))
    startState)

showEndState w course = do
  endstate <- get
  lift $ drawCourse course endstate w

over course = do
  gamestate <- get
  return $ (quitted gamestate) || hitsCourse (lastSegmentTravelled (humanState gamestate)) course || someoneWon course gamestate

gameLoop w rows course = do
  oldstate <- get
  lift $ drawCourse course oldstate w
  c <- lift $ getCharInput w
  case c of
      Quit -> do
        oldstate <- get
        put oldstate {quitted = True}
      MoveDirection d -> do
        lift $ do
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

updateState s humanDirection course = let aIdirection = getMove course (aiState s)
                                          withHumanMove = updateHumanState s humanDirection
                                          withAIMove = updateAIState withHumanMove aIdirection
                                      in withAIMove
