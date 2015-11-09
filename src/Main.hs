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

data GameConfig = GameConfig { getCourse :: Course, getStartAIState :: CarState, getStartCarState :: CarState }

main :: IO ()
main = void $ runCurses $ top

--startGameState = GameState {humanState = startCarState, aiState = startAIState, quitted = False}
roundCourse = GameConfig { getCourse = makeCourse [
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
  [],
  getStartCarState = CarState [Point 8 (-2), Point 8 (-2)],
  getStartAIState = CarState [Point 7 2, Point 7 2] }

zigzagCourse = GameConfig { getCourse = makeCourse [(Point (-10) (-10)), (Point 0 0), (Point 10 2), (Point 35 (-5)), (Point 65 12)] [],
                            getStartCarState = CarState [Point (-11) (-11), Point (-11) (-11)],
                            getStartAIState = CarState [Point (-12) (-12), Point (-12) (-12)] }

configs = [roundCourse, zigzagCourse]

top = do
  setEcho False
  w <- defaultWindow
  updateWindow w $ do
    drawString "Choose a course!"
  render
  courseSelectWindow <- newWindow thumbnailHeight thumbnailWidth 0 0 -- this could get pushed down into getCourseSelection
  let startHeight = 3
  configIndex <- getCourseSelection courseSelectWindow 0 startHeight
  closeWindow courseSelectWindow
  let config = configs !! (fromIntegral configIndex)
      startState = startStateFromConfig config
      course = getCourse config
  (rows, cols) <- screenSize
  updateWindow w $ do
      moveCursor (rows - 1) 0
      drawString $ "Enter Move:"
  render
  --untilM (runStateT (gameLoop w rows course) startState) (over course) >>
  --  showEndState w course >>
  --    gameOverMessage w course
  (runStateT ((untilM (gameLoop w rows course) (over course)) >> (showEndState w course) >> (gameOverMessage w course))
    startState)
  --  showEndState w course >>
  --    gameOverMessage w course
thumbnailWidth = 30
thumbnailHeight = 20
thumbnailBuffer = 3
thumbnailOuterSize = thumbnailWidth + thumbnailBuffer
arrowString=
  "/\\\n\
  \||\n\
  \||\n\
  \||\n\
  \||\n"

drawArrow canvas courseIndex startHeight = do
  cid <- textColor
  updateWindow canvas $ do
    moveWindow (thumbnailHeight + 2 + startHeight) (courseIndex * thumbnailOuterSize)
    moveCursor 0 0
    setColor cid
    drawString $ arrowString

startStateFromConfig config = GameState { quitted = False, humanState = (getStartCarState config), aiState = (getStartAIState config) }

drawThumbnails startHeight = forM (zip [0..] configs) drawThumbnail
  where drawThumbnail (courseIndex, config) = do window <- newWindow 50 50 startHeight (courseIndex * thumbnailOuterSize)
                                                 condenseAndDraw  (R.renderInto (startStateFromConfig config) (getCourse config) thumbnailWidth thumbnailHeight) window
                                                 closeWindow window


data UserSelectCommand = SelectToLeft | SelectToRight | FinalizeSelection
getCourseSelection w courseIndex startHeight = do
  drawArrow w courseIndex startHeight
  render
  drawThumbnails startHeight
  render
  drawArrow w courseIndex startHeight
  render
  courseAction <- awaitCourseSelectCommand w
  case courseAction of
    SelectToLeft -> getCourseSelection w (mod (courseIndex + 1) (fromIntegral $ length configs)) startHeight
    SelectToRight -> getCourseSelection w (mod (courseIndex - 1) (fromIntegral $ length configs)) startHeight
    FinalizeSelection-> return (mod courseIndex (fromIntegral (length configs)))

awaitCourseSelectCommand w = untilJust getInput
  where inputToCommand (Just (EventSpecialKey KeyEnter)) = Just FinalizeSelection
        inputToCommand (Just (EventCharacter '\n')) = Just FinalizeSelection
        inputToCommand (Just (EventSpecialKey KeyRightArrow)) = Just SelectToRight
        inputToCommand (Just (EventSpecialKey KeyLeftArrow)) = Just SelectToLeft
        inputToCommand _ = Nothing
        getInput = do
          input <- getEvent w Nothing
          return $ inputToCommand input


drawCourse course state w = do
  updateWindow w $ do
    moveCursor 0 0
  condenseAndDraw (R.render state course) w
  --let clumped = group (R.render state course)
  --forM_ clumped (\s -> drawColorfulString w (map fst s) (snd $ head s))
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

condenseAndDraw colorfulString w = do
  let clumped = group (colorfulString)
  forM_ clumped (\s -> drawColorfulString w (map fst s) (snd $ head s))

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
