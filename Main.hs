{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Random
import Control.Monad.State.Lazy
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

data Color = Red | Green | Blue
           deriving (Show, Eq)
data Pos = Pos { posX :: Int, posY :: Int}
           deriving (Show, Eq, Ord)
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
data Wall = Wall { _wallPos :: Pos }
          deriving Show
$(makeLenses ''Wall)
data GameLevel = GameLevel { _lvlBoxes :: [Box]
                           , _lvlGoals :: [Goal]
                           , _lvlWalls :: [Wall]
                           , _lvlDimensions :: Dimensions}
               deriving Show
$(makeLenses ''GameLevel)

class Colored a where
  color :: Functor f => (Color -> f Color) -> a -> f a
instance Colored Box where
  color = boxColor
instance Colored Goal where
  color = goalColor

class Positioned a where
  pos :: Functor f => (Pos -> f Pos) -> a -> f a
instance Positioned Box where
  pos = boxPos
instance Positioned Goal where
  pos = goalPos
instance Positioned Wall where
  pos = wallPos

isGameOver :: GameLevel -> Bool
isGameOver = null . _lvlGoals

inBounds :: Positioned a => Dimensions -> a -> Bool
inBounds (Dimensions dx dy) box = x >= 0 && x < dx && y >= 0 && y < dy
  where
    p = box^.pos
    x = posX p
    y = posY p

clearOfPositioned :: (Positioned a, Positioned a1) => a -> [a1] -> Bool
clearOfPositioned p ps = (p^.pos) `notElem` (map (^.pos) ps)

movePos :: Direction -> Pos -> Pos
movePos dir (Pos x y) = case dir of
                         DirLeft -> Pos (pred x) y
                         DirRight -> Pos (succ x) y
                         DirUp -> Pos x (pred y)
                         DirDown -> Pos x (succ y)

moveBox :: Direction -> Box -> Box
moveBox dir = boxPos %~ movePos dir

reachedGoal :: (Positioned a, Positioned a1, Colored a, Colored a1) => a -> [a1] -> Bool
reachedGoal box goals = any (== (p,c)) $ zip ps cs
  where p = box^.pos
        c = box^.color
        ps = map (^.pos) goals
        cs = map (^.color) goals

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
                           & lvlBoxes.mapped %~ (tagBoxActions dir lvl)
                           & lvlGoals.mapped %~ (tagGoalActions lvl)

applyBoxActions :: Direction -> GameLevel -> (GameLevel, Bool)
applyBoxActions dir lvl = (lvl & lvlBoxes .~ immobile ++ movedBoxes, null mobile)
  where shouldKeepBox = noneOf (boxActions.folded) (==BoxDisappear)
        remaining = lvl^.lvlBoxes^..folded.filtered shouldKeepBox
        mobile = remaining^..folded.filtered (anyOf (boxActions.folded) (==BoxMove))
        immobile = remaining^..folded.filtered (noneOf (boxActions.folded) (==BoxMove))
        movedBoxes = map (moveBox dir) mobile

applyGoalActions :: GameLevel -> GameLevel
applyGoalActions = lvlGoals %~ (^..folded.filtered shouldKeepGoal)
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

moveOnLevel :: Direction -> GameLevel -> GameLevel
moveOnLevel dir = execState $ iterateUntil id (stepLevel dir)

simulatePath ::  GameLevel -> [Direction] -> GameLevel
simulatePath = foldl' (flip moveOnLevel)

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

randPos :: MonadRandom m => Dimensions -> m Pos
randPos dim = Pos <$> getRandomR (0, dimX dim) <*> getRandomR (0, dimY dim)

randPoss' :: MonadRandom m => Dimensions -> StateT (Set Pos) m Int
randPoss' dim = do ps <- get
                   np <- lift (randPos dim)
                   let nps = Set.insert np ps
                   put nps
                   return $ Set.size nps

randPoss :: MonadRandom m => Dimensions -> Int -> m (Set Pos)
randPoss dim n = execStateT (iterateUntil (>= n) (randPoss' dim)) Set.empty

main = return ()
