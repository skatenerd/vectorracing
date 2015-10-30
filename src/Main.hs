module Main where

import UI.NCurses
import Data.Maybe
import Control.Monad
import Control.Monad.State
import GameTypes
import Geometry
import qualified Rendering as R
--import Course
import Car
import Ai

--import Data.Maybe
--import Data.List

--import Control.Monad.Loops
--import Control.Monad.State
--import Control.Monad.Reader

main :: IO ()
main = runCurses $ void (runStateT top startGameState)

startCarState = CarState (Point (-10) (-10)) (Vector 0 0) (Point (-10) (-10))
startAIState = CarState (Point (-12) (-12)) (Vector 0 0) (Point (-12) (-12))
startGameState = GameState {humanState = startCarState, aiState = startAIState}
thecourse = Course { path = [(Point (-10) (-10)), (Point 0 0), (Point 10 2), (Point 35 (-5)), (Point 45 15)], obstacles=[]}

top = do
  lift $ setEcho False
  w <- lift defaultWindow
  (rows, cols) <- lift screenSize
  lift $ updateWindow w $ do
      moveCursor (rows - 1) 0
      drawString $ "Enter Move:"
  lift render
  gameLoop w rows

gameLoop w rows = do
  c <- lift $ getCharInput w
  case c of
      Quit -> return ()
      MoveDirection d -> do
        oldstate <- get
        --put $ c
        --newstuff <- get
        let newstate = updateState oldstate d
        put newstate
        lift $ updateWindow w $ do
            moveCursor (rows - 1) 0
            drawString $ replicate 40 ' '--hack...
            moveCursor (rows - 1) 0
            drawString $ "Enter Move: " ++ (show c)
            moveCursor (rows - 1) 12
            moveCursor 0 0
            drawString $ R.render newstate thecourse
        lift render
        unless (c == Quit) $ gameLoop w rows

data Command = MoveDirection Direction | Quit deriving (Show, Eq)


updateHumanState state delta = state { humanState = takeCarTurn (humanState state) delta }
updateAIState state delta = state { aiState = takeCarTurn (aiState state) delta }

updateState s humanDirection = let aIdirection = getMove thecourse (aiState s)--head history
                                   withHumanMove = updateHumanState s humanDirection
                                   withAIMove = updateAIState withHumanMove aIdirection
                               in withAIMove

--incstate = do
--  now <- get
--  --liftIO $ print now
--  theConfig <- lift ask
--  userInput <- liftIO promptInput
--  let delta = inputToDelta userInput
--  let aIdirection = getMove (getCourse theConfig) (aiState now)--head history
--      withHumanMove = updateHumanState now delta
--      withAIMove = updateAIState withHumanMove aIdirection
--  put $ withAIMove
--  newnow <- get
--  liftIO $ putStrLn $ render newnow $ getCourse theConfig

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

--data GameConfig = GameConfig { getCourse :: Course }
--
--
--
--coast car = let new_position = translate (position car) (velocity car)
--            in CarState new_position (velocity car)
--
--
--distanceToShape p shape = minimum (map (distanceToSegment p) (getWalls shape))
--
--hitsShape segment shape = any (segmentIntersects segment) $ getWalls shape
--
---- a "shape" is a collection of points whose order doesn't matter
--getWalls shape = [makeSegment x y | x <- shape, y <- shape]
--
--inputToDelta "up" = Up
--inputToDelta "upright" = UpRight
--inputToDelta "upleft" = UpLeft
--inputToDelta "left" = LLeft
--inputToDelta "right" = RRight
--inputToDelta "downright" = DownRight
--inputToDelta "downleft" = DownLeft
--inputToDelta "down" = Down
--inputToDelta _ = Up
--
--
--promptInput = let valid_inputs = ["up", "upright", "upleft", "left", "right", "downright", "downleft", "down"]
--              in do
--                putStrLn "\n\nPlease enter a direction"
--                userInput <- getLine
--                putStrLn userInput
--                if elem userInput valid_inputs
--                  then return userInput
--                  else do
--                    putStrLn "Please enter a legal input"
--                    print valid_inputs
--                    promptInput
--
--
--
--
--incstate :: StateT GameState (ReaderT GameConfig IO) ()
--incstate = do
--  now <- get
--  --liftIO $ print now
--  theConfig <- lift ask
--  userInput <- liftIO promptInput
--  let delta = inputToDelta userInput
--  let aIdirection = getMove (getCourse theConfig) (aiState now)--head history
--      withHumanMove = updateHumanState now delta
--      withAIMove = updateAIState withHumanMove aIdirection
--  put $ withAIMove
--  newnow <- get
--  liftIO $ putStrLn $ render newnow $ getCourse theConfig
--
--crash = do
--  gamestate <- get
--  theConfig <- lift ask
--  return $ hitsCourse (lastSegmentTravelled (humanState gamestate)) (getCourse theConfig)
--
--
--squareBarrier = [[Point 3 3, Point 3 5, Point 5 5, Point 5 3], [Point (-8) (-8), Point (-8) 8], [Point (-8) 8, Point 8 8]]
--
--
---- TODO: try reading some config values from a file...
---- TODO: use someting ncurses-like instead of reprinting everything
--theconfig = GameConfig Course {path = thecourse, obstacles = squareBarrier}
--main = putStrLn "\n\n\nWelcome!!!" >> (runReaderT (runStateT (untilM incstate crash) startGameState) $ theconfig)
