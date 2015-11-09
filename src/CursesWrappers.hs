{-# LANGUAGE FlexibleContexts #-}
module CursesWrappers where
import Data.List

import UI.NCurses
import Control.Monad.State
import Control.Monad.Loops
import qualified Rendering as R
import GameTypes
import Configs
import Car
import Scoring

data UserSelectCommand = SelectToLeft | SelectToRight | FinalizeSelection

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



makeCID Dust = newColorID ColorYellow ColorBlack 1
makeCID Car = newColorID ColorCyan ColorBlack 3
makeCID Earth = newColorID ColorGreen ColorGreen 4
makeCID Wall = newColorID ColorYellow ColorBlack 5
makeCID Opponent = newColorID ColorRed ColorBlack 6
makeCID Road = newColorID ColorBlack ColorBlack 7
makeCID Finish = newColorID ColorBlue ColorBlue 8
textColor = newColorID ColorBlack ColorWhite 9

getCourseSelection startHeight = do
  courseSelectWindow <- newWindow thumbnailHeight thumbnailWidth 0 0 -- this could get pushed down into getCourseSelection
  answer <- go 0 courseSelectWindow
  closeWindow courseSelectWindow
  return answer
    where go courseIndex courseSelectWindow = do
                 drawArrow courseSelectWindow courseIndex startHeight
                 render
                 drawThumbnails startHeight
                 render
                 drawArrow courseSelectWindow courseIndex startHeight
                 render
                 courseAction <- awaitCourseSelectCommand courseSelectWindow
                 case courseAction of
                   SelectToLeft -> go (mod (courseIndex + 1) (fromIntegral $ length configs)) courseSelectWindow
                   SelectToRight -> go (mod (courseIndex - 1) (fromIntegral $ length configs)) courseSelectWindow
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
  render

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

condenseAndDraw colorfulString w = do
  let clumped = group (colorfulString)
  forM_ clumped (\s -> drawColorfulString w (map fst s) (snd $ head s))

drawThumbnails startHeight = forM (zip [0..] configs) drawThumbnail
  where drawThumbnail (courseIndex, config) = do window <- newWindow 50 50 startHeight (courseIndex * thumbnailOuterSize)
                                                 condenseAndDraw  (R.renderInto (startStateFromConfig config) (getCourse config) thumbnailWidth thumbnailHeight) window
                                                 closeWindow window


drawColorfulString w string color = do
   cid <- makeCID color --newColorID ColorBlue ColorGreen 1
   updateWindow w $ do
     setColor cid
     drawString string

awaitEnter w = untilM render $ do
  key <- getEvent w Nothing
  return $ (key == Just (EventSpecialKey KeyEnter)) || (key == Just (EventCharacter '\n'))

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
