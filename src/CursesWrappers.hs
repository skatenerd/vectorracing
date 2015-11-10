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
import qualified Rendering as R
import Data.List.Split

data UserSelectCommand = SelectToLeft | SelectToRight | FinalizeSelection

floorDivide :: Integer -> Integer -> Integer
floorDivide top bottom = floor $ (fromIntegral top) / (fromIntegral bottom)

thumbnailBuffer = 3

thumbnailDimensions = do
  (rows, cols) <- screenSize
  let width = floorDivide cols $ fromIntegral (length configs)
      height = floorDivide rows 2
      outerSize = width + thumbnailBuffer
  return (width, height, outerSize)

arrowString=
  "/\\\n\
  \||\n\
  \||\n"

drawArrow canvas courseIndex startHeight = do
  cid <- textColor
  (width, height, outerWidth) <- thumbnailDimensions
  let spaceBetweenArrowAndThumbnail = 2
  updateWindow canvas $ do
    moveWindow (height + spaceBetweenArrowAndThumbnail + startHeight) (courseIndex * outerWidth)
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
  arrowWindow <- newWindow (arrowHeight + 1) (arrowWidth + 1) 0 0 -- not sure why we need this (+1) here...
  answer <- go 0 arrowWindow
  closeWindow arrowWindow
  return answer
    where go courseIndex arrowWindow = do
                 updateWindow arrowWindow clear
                 w <- defaultWindow
                 updateWindow w $ do
                   moveCursor 0 0
                   drawString "Choose a course!"
                 drawArrow arrowWindow courseIndex startHeight
                 drawThumbnails startHeight
                 render
                 courseAction <- awaitCourseSelectCommand arrowWindow
                 case courseAction of
                   SelectToLeft -> go (mod (courseIndex + 1) (fromIntegral $ length configs)) arrowWindow
                   SelectToRight -> go (mod (courseIndex - 1) (fromIntegral $ length configs)) arrowWindow
                   FinalizeSelection-> return (mod courseIndex (fromIntegral (length configs)))
          arrowWidth = fromIntegral $ length $ head $ splitOn "\n" arrowString
          arrowHeight = fromIntegral $ length $ splitOn "\n" arrowString

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
  (rows, cols) <- screenSize
  updateWindow w $ do
    moveCursor 0 0
  condenseAndDraw (R.renderInto state course (min (cols - 3) ((R.courseWidth course))) (min (rows - 3) (R.courseHeight course))) w -- try to cram the rendering into the size of your screen
  render

gameOverMessage w course endState = do
  sad <- newWindow 10 50 10 10
  updateWindow sad $ do
      moveCursor 0 0
      drawString $ "GAME OVER\n"
  let humanFinished = (humanWon course endState)
      aiFinished = (aiWon course endState)
      tie = humanFinished && aiFinished
      humanVictory = humanFinished && not aiFinished
      aiVictory = aiFinished && not humanFinished
  when humanVictory $ updateWindow sad $ do
    drawString "YOU BEAT THE COMPUTER!!!\n"
  when aiVictory $ updateWindow sad $ do
    drawString "YOU LOST.  TO A ROBOT\n"
  when tie $ updateWindow sad $ do
    drawString "A TIE? IMPOSSIBLE!\n"
  when (hitsCourse (lastSegmentTravelled (humanState endState)) course) $ updateWindow sad $ do
    drawString "YOU CRASHED, IDIOT\n"
  render
  getEvent sad $ Just 3000
  updateWindow sad $ do
      moveCursor 4 0
      drawString $ "ENTER TO QUIT"
  render
  awaitEnter w

condenseAndDraw colorfulString w = do
  let clumped = group (colorfulString)
  forM_ clumped (\s -> drawColorfulString w (map fst s) (snd $ head s))

drawThumbnails startHeight = forM (zip [0..] configs) drawThumbnail
  where drawThumbnail (courseIndex, config) = do (width, height, outerSize) <- thumbnailDimensions
                                                 window <- newWindow
                                                          (height + 1)
                                                          (width + 1)
                                                          startHeight
                                                          (courseIndex * outerSize)
                                                 condenseAndDraw  (R.renderInto (startStateFromConfig config) (getCourse config) width height) window
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
