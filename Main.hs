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
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq

data Color = Red | Green | Blue
           deriving (Show, Eq, Ord, Enum)
data Pos = Pos { posX :: Int, posY :: Int}
           deriving (Show, Eq, Ord)
data Dimensions = Dimensions { dimX :: Int, dimY :: Int}
                deriving (Show, Ord, Eq)
data Direction = DirUp | DirDown | DirLeft | DirRight
               deriving Show
data BoxAction = BoxMove | BoxDisappear
               deriving (Show, Eq, Ord)
data GoalAction = GoalDisappear
                deriving (Show, Eq, Ord)

data Box = Box { _boxPos :: Pos
               , _boxColor :: Color
               , _boxActions :: [BoxAction] }
         deriving (Show, Ord, Eq)
$(makeLenses ''Box)
data Goal = Goal { _goalPos :: Pos
                 , _goalColor :: Color
                 , _goalActions :: [GoalAction] }
          deriving (Show, Ord, Eq)
$(makeLenses ''Goal)
data Wall = Wall { _wallPos :: Pos }
          deriving (Show, Ord, Eq)
$(makeLenses ''Wall)
data GameLevel = GameLevel { _lvlBoxes :: [Box]
                           , _lvlGoals :: [Goal]
                           , _lvlWalls :: [Wall]
                           , _lvlDimensions :: Dimensions}
               deriving (Show, Ord, Eq)
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

simulatePath :: GameLevel -> [Direction] -> GameLevel
simulatePath = foldl' (flip moveOnLevel)

solve' :: Set GameLevel -> Seq ([Direction], GameLevel) -> Maybe [Direction]
solve' seen q
  | Seq.null q = Nothing
  | isGameOver lvl = Just dirs
  | otherwise =  solve'  nextseen nextq
  where cur = q `Seq.index` 0
        (dirs, lvl) = cur
        nq = Seq.drop 1 q
        f (d, l) nd = (nd:d, moveOnLevel nd l)
        nlvls = map (f cur) allDirections
        nextseen = Set.insert lvl seen
        nextq = if (lvl `Set.member` seen) then nq else nq >< Seq.fromList nlvls

solve :: GameLevel -> Maybe [Direction]
solve lvl = solve' s q
  where s = Set.empty
        q = Seq.singleton ([], lvl)

allDirections :: [Direction]
allDirections = vertical ++ horizontal

vertical :: [Direction]
vertical = [DirUp, DirDown]

horizontal :: [Direction]
horizontal = [DirLeft, DirRight]

choose :: MonadRandom m => [a] -> m a
choose ls = liftM (ls !!) $ getRandomR (0, (length ls) - 1)

uniqueRandom' :: (Monad m, Ord a1) => Set a1 -> m a1 -> Int -> m (Set a1)
uniqueRandom' accum _ 0 = return accum
uniqueRandom' accum r n = do
  item <- r
  let newN = if item `Set.member` accum then n else n-1
  uniqueRandom' (Set.insert item accum) r newN

uniqueRandom :: (Monad m, Ord a1) => m a1 -> Int -> m (Set a1)
uniqueRandom = uniqueRandom' Set.empty

repeatRandom :: Monad m => m t -> Int -> m [t]
repeatRandom r n = sequence $ replicate n r

sample :: (Ord a1, MonadRandom m) => [a1] -> Int -> m (Set a1)
sample ls = uniqueRandom (choose ls)

interleave :: [a] -> [a] -> [a]
interleave a b = concat . transpose $ [a,b]

-- MonadRandom's bind is strict, therefore we can't return an infinite list easily
randPath :: MonadRandom m => Int -> m [Direction]
randPath n = do
  numToDrop <- getRandomR (0,1)
  path <- sequence $ interleave
          (replicate n (choose vertical)) (replicate n (choose horizontal))
  return $ take n . drop numToDrop $ path

randPos :: MonadRandom m => Dimensions -> m Pos
randPos dim = Pos <$> getRandomR (0, dimX dim) <*> getRandomR (0, dimY dim)

allColors :: [Color]
allColors = [Red ..]

size :: Dimensions -> Int
size dim = dimX dim * dimY dim

-- Generates a possible level. Not guarenteed to be solveable, that must be checked
genPossibleLevel :: MonadRandom m => Int -> Dimensions -> m GameLevel
genPossibleLevel numBoxes dimensions = do
  let pathDist = 20
      numWalls = size dimensions `div` 5
  boxPositions <- liftM Set.toList (uniqueRandom (randPos dimensions) numBoxes)
  colors <- repeatRandom (choose allColors) numBoxes
  let boxes = zipWith (\ p c -> Box p c []) boxPositions colors
  wallPositions <- liftM ((\\ boxPositions) . Set.toList)
                   (uniqueRandom (randPos dimensions) numWalls)
  let walls = map Wall wallPositions
  path <- liftM (take pathDist) (randPath pathDist)
  let levelAfterBoxesMoved = simulatePath (GameLevel boxes [] walls dimensions) path
      finalBoxes = levelAfterBoxesMoved^.lvlBoxes
      toGoal b = Goal (b^.boxPos) (b^.boxColor) []
      goals = map toGoal finalBoxes
  return $ GameLevel boxes goals walls dimensions

genLevel :: MonadRandom m => Int -> Dimensions -> m (GameLevel, [Direction])
genLevel numBoxes dimensions = do
  lvl <- genPossibleLevel numBoxes dimensions
  let solution = solve lvl
  case solution of
   Just sol -> return (lvl, sol)
   Nothing -> genLevel numBoxes dimensions

main = return ()
