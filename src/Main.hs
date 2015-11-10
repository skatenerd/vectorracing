{-# LANGUAGE FlexibleContexts #-}
module Main where

import UI.NCurses
import Control.Monad.State
import Control.Monad.Reader
import GameTypes
import Car
import Ai
import Configs
import CursesWrappers
import Scoring

import Control.Monad.Loops

main :: IO ()
main = void $ runCurses $ top

data Readme = Readme { aiDifficulty :: Integer, course :: Course}

top = do
  setEcho False
  w <- defaultWindow
  render
  let startHeight = 3
  configIndex <- getCourseSelection startHeight
  let config = configs !! (fromIntegral configIndex)
      startState = startStateFromConfig config
      course = getCourse config
  (rows, cols) <- screenSize
  let theconfig = Readme { aiDifficulty = 5, course = course}
  updateWindow w $ do
      moveCursor (rows - 1) 0
      drawString $ "Enter Move:"
  render
  let gameBody = ((untilM (gameLoop w) (over course)) >> (showEndState w) >> farewell w)
      runnable = (runReaderT
                   (runStateT gameBody startState)
                   theconfig)
  runnable

farewell w = do
  endState <- get
  course <- asks course
  lift $ lift $ (gameOverMessage w course endState)

showEndState w = do
  endstate <- get
  course <- asks course
  lift $ lift $ drawCourse course endstate w
  return endstate

over course = do
  gamestate <- get
  return $ (quitted gamestate) || hitsCourse (lastSegmentTravelled (humanState gamestate)) course || someoneWon course gamestate

gameLoop w = do
  course <- asks course
  oldstate <- get
  lift $ lift $ drawCourse course oldstate w
  c <- lift $ lift $ getCharInput w
  case c of
      Quit -> do
        put oldstate {quitted = True}
      MoveDirection d -> do
        lift $ lift $ do
          cid <- textColor
          (rows, _) <- screenSize
          updateWindow w $ do
            setColor cid
            moveCursor (rows - 1) 0
            clearLine
            drawString $ "Enter Move: " ++ (show c)
          render
        newState <- updateState d course
        put newState

updateState humanDirection course = do
  s <- get
  aiDepth <- asks aiDifficulty
  let updateHumanState state delta = state { humanState = takeCarTurn (humanState state) delta }
      updateAIState state delta = state { aiState = takeCarTurn (aiState state) delta }
      aIdirection = getMove course (aiState s) aiDepth
      withHumanMove = updateHumanState s humanDirection
      withAIMove = updateAIState withHumanMove aIdirection
  return withAIMove

