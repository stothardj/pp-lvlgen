{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Random
import Control.Monad.State.Lazy
import Data.List

data Color = Red | Green | Blue
           deriving (Show, Eq)
data Pos = Pos { posX :: Int, posY :: Int}
           deriving (Show, Eq)
data Dimensions = Dimensions { dimX :: Int, dimY :: Int}
                deriving Show
data Direction = DirUp | DirDown | DirLeft | DirRight
               deriving Show
data BoxAction = BoxMove | BoxDisappear
               deriving (Show, Eq)
data GoalAction = GoalDisappear
                deriving (Show, Eq)

data Box = Box { _boxPos :: Pos
               , _boxColor :: Color
               , _boxActions :: [BoxAction] }
         deriving Show
$(makeLenses ''Box)
data Goal = Goal { _goalPos :: Pos
                 , _goalColor :: Color
                 , _goalActions :: [GoalAction] }
          deriving Show
$(makeLenses ''Goal)
data Wall = Wall { wallPos :: Pos }
          deriving Show
data GameLevel = GameLevel { _lvlBoxes :: [Box]
                           , _lvlGoals :: [Goal]
                           , _lvlWalls :: [Wall]
                           , _lvlDimensions :: Dimensions}
               deriving Show
$(makeLenses ''GameLevel)

class Colored a where
  getColor :: a -> Color
instance Colored Box where
  getColor = _boxColor
instance Colored Goal where
  getColor = _goalColor

class Positioned a where
  getPos :: a -> Pos
instance Positioned Box where
  getPos = _boxPos
instance Positioned Goal where
  getPos = _goalPos
instance Positioned Wall where
  getPos = wallPos

isGameOver :: GameLevel -> Bool
isGameOver = null . _lvlGoals

inBounds :: Positioned a => Dimensions -> a -> Bool
inBounds (Dimensions dx dy) box = x >= 0 && x < dx && y >= 0 && y < dy
  where
    p = getPos box
    x = posX p
    y = posY p

clearOfPositioned
  :: (Positioned a, Positioned a1) => a -> [a1] -> Bool
clearOfPositioned p ps = (getPos p) `notElem` (map getPos ps)

movePos :: Direction -> Pos -> Pos
movePos dir (Pos x y) = case dir of
                         DirLeft -> Pos (pred x) y
                         DirRight -> Pos (succ x) y
                         DirUp -> Pos x (pred y)
                         DirDown -> Pos x (succ y)

moveBox :: Direction -> Box -> Box
moveBox dir = boxPos %~ movePos dir

reachedGoal' :: Pos -> Color -> [Pos] -> [Color] -> Bool
reachedGoal' p c ps cs = anyOf folded (== (p,c)) $ zip ps cs

reachedGoal
  :: (Positioned a, Positioned a1, Colored a, Colored a1) =>
     a -> [a1] -> Bool
reachedGoal box goals =
  reachedGoal' (getPos box) (getColor box) (map getPos goals) (map getColor goals)

determineBoxActions :: Direction -> GameLevel -> Box -> [BoxAction]
determineBoxActions dir lvl box
  | reachedGoal box (lvl^.lvlGoals) = [BoxDisappear]
  | not $ inBounds (lvl^.lvlDimensions) movedBox = []
  | not $ clearOfPositioned movedBox (lvl^.lvlWalls) = []
  | not $ clearOfPositioned movedBox (lvl^.lvlBoxes) = []
  | otherwise = [BoxMove]
  where movedBox = moveBox dir box

tagBoxActions :: Direction -> GameLevel -> Box -> Box
tagBoxActions dir lvl box = box & boxActions .~ newActions
  where newActions = determineBoxActions dir lvl box

determineGoalActions :: GameLevel -> Goal -> [GoalAction]
determineGoalActions lvl goal
  | reachedGoal goal (lvl^.lvlBoxes) = [GoalDisappear]
  | otherwise = []

tagGoalActions :: GameLevel -> Goal -> Goal
tagGoalActions lvl goal = goal & goalActions .~ newActions
  where newActions = determineGoalActions lvl goal

determineActions :: Direction -> GameLevel -> GameLevel
determineActions dir lvl = lvl
                           & lvlBoxes %~ map (tagBoxActions dir lvl)
                           & lvlGoals %~ map (tagGoalActions lvl)

applyBoxActions :: Direction -> GameLevel -> (GameLevel, Bool)
applyBoxActions dir lvl = (lvl & lvlBoxes .~ immobile ++ movedBoxes, null mobile)
  where shouldKeepBox = noneOf (boxActions.folded) (==BoxDisappear)
        remaining = lvl^.lvlBoxes^..folded.filtered shouldKeepBox
        mobile = remaining^..folded.filtered (anyOf (boxActions.folded) (==BoxMove))
        immobile = remaining^..folded.filtered (noneOf (boxActions.folded) (==BoxMove))
        movedBoxes = map (moveBox dir) mobile

applyGoalActions :: GameLevel -> GameLevel
applyGoalActions lvl = lvl & lvlGoals %~ (^..folded.filtered shouldKeepGoal)
  where shouldKeepGoal = noneOf (goalActions.folded) (==GoalDisappear)

applyActions :: Direction -> GameLevel -> (GameLevel, Bool)
applyActions dir lvl = (bothApplied, doneMoving)
  where (boxApplied, doneMoving) = applyBoxActions dir lvl
        bothApplied = applyGoalActions boxApplied

stepLevel :: Direction -> State GameLevel Bool
stepLevel dir = do lvl <- get
                   let withActions = determineActions dir lvl
                       (newLvl, doneMoving) = applyActions dir withActions
                   put newLvl
                   return doneMoving

vertical :: [Direction]
vertical = [DirUp, DirDown]

horizontal :: [Direction]
horizontal = [DirLeft, DirRight]

choose :: MonadRandom m => [a] -> m a
choose ls = liftM (ls !!) $ getRandomR (0, (length ls) - 1)

interleave :: [a] -> [a] -> [a]
interleave a b = concat . transpose $ [a,b]

randPath :: MonadRandom m => m [Direction]
randPath = do
  numToDrop <- getRandomR (0,1)
  path <- sequence $ interleave (repeat $ choose vertical) (repeat $ choose horizontal)
  return $ drop numToDrop path

main = return ()
