module Main where

import System.IO
import Blaze.ByteString.Builder (Builder, toByteStringIO)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Control.Concurrent (ThreadId, killThread, myThreadId, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, takeMVar)
import Control.Monad (forM_, sequence, when)
import qualified Data.ByteString.Char8 as B
import Data.List (sort, sortBy, zip4)
import Data.Maybe
import Data.Monoid (mappend, mempty)
-- import System.Posix.Signals (Handler(Catch), installHandler, sigINT)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Random
import VM

type Predicate = (Cost, Object, Point) -> Bool
type Range = (Int, Int)
-- print the dijkstra graph
cMAX = 2147483647
myPrint c x = 
    let str = if x ==  cMAX then "X" else show x in 
    let str2 = if c then "\n" else "" in
    putStr (str ++ str2)

-- where do you want to go today? specify it using p, this function will find a place
-- TODO: change (Object, Point) to Maybe Point
chooseGoal :: State -> CostTable -> Predicate -> Range -> Point -> (Object, Point)
chooseGoal state costs predicate (w, h) robot =
      let l = sort $ clean [ ((getCost costs (i, j)), (get state (i, j)), (i, j)) | i <- [1..w], j<- [1..h]] in 
      select l
   where 
      select [] = (ORobot, robot)
      -- take first one --- maybe all instead?
      select ((_,object,position):xs) = (object, position)
      clean = (filter predicate) . (filter (\(costs, _, _) -> costs < cMAX))

-- find a goal and a path there
findGoalWithPath :: State -> CostTable -> Point -> Predicate -> [Move]
findGoalWithPath state costs robot predicate = 
  let 
    range = getWorldSize state
    (object, goal) = 
      chooseGoal state costs predicate range robot
  in
    if object /= ORobot then findPath state costs robot goal else []

-- checks if the moves kills the robot      
testMoves s m = 
    let s' = makeMoves s m in
    if (getCondition s') /= CLose then (True, s') else (False, s)
    
-- finds the moves that do not kill robot
testMovesList s [] = (False, s, [])
testMovesList s (m:ms) = let (b, s') = testMoves s m in if b then (b, s', m) else testMovesList s (ms)

-- to complicated
myFind _ [] = []
myFind g ((p, m):xs) = if p==g then [m] else myFind g xs 
 
 
-- run :: MVar Builder -> State -> [Move] -> [Int] -> IO (Int, [Move])
-- main function
run s0 ps ms = goDijkstra s0 ps ms []
  where
    goDijkstra s (p:ps) (m:ms) prefix = do
      let r = getRobotPoint s
      let (wx, wy) = getWorldSize s
      let c = buildCostTable s r
--      flip mapM_ [1..wy] $ \y -> 
--        flip mapM_ [1..wx] $ \x -> myPrint (x == wx) $ getCost c (x,  wy+1-y)
-- several sequences of moves
        --find lambda!
      let moves = findGoalWithPath s c r (\(fc, ft, fp) -> isLambda s fp || isLift s fp)
      -- probably wrong, but i am too tired / jmi
      let rocks = findMoveRocks s 
      let (t, goal) = chooseGoal s c (\(fc, ft, fp) -> let myRocks = filter (\(p, m) -> p==fp) rocks in myRocks /= []) (getWorldSize s) r
      let mak1 = if t /= ORobot then findPath s c r goal else []
      let mak2 = if t /= ORobot then myFind goal rocks else []
      let movesComak = mak1++mak2
      -- /probably wrong
      --find trampolina! hop hop
      let moves2 = findGoalWithPath s c r (\(fc, ft, fp) -> isTrampoline s fp || isRazor s fp)
      
      -- find earth!
      let moves3 = findGoalWithPath s c r (\(fc, ft, fp) -> isEarth s fp)
      -- small probability of doing nothing
      let all = if (length prefix) < p && (p `mod` 20 == 0) then [] else [moves, moves2, movesComak, moves3]
      -- some default moves
      let (b, s', answer) =  testMovesList s $ filter (\x -> x /= [])  $ all ++ [[m], [MRight], [MLeft], [MDown], [MUp]]
--      dump s' 
      let result = prefix ++ pruneCycles s answer
--    print result
--      hFlush stdout
      -- CHANGE 153! use deadlock detection!!
      if  (getCondition s')/= CNone || (moves == [] && length(result)>153) 
        then return ((getScore s') , result) 
        else goDijkstra s' ps ms result
--      modifyMVar_ resultV $ \result ->
--        return (result `mappend` fromString answer)
--      goRandom s' ms (prefix++answer)
-- mietek's function
handleInterrupt :: MVar Builder -> ThreadId -> IO ()
handleInterrupt resultV mainT = do
  result <- takeMVar resultV
  toByteStringIO B.putStrLn result
  killThread mainT

-- todo: find cycles larger than one move
pruneCycles :: State -> [Move] -> [Move]
pruneCycles = pruneWaits

pruneWaits :: State -> [Move] -> [Move]
pruneWaits s [] = []
pruneWaits s (m:ms) = 
  if s' == s then rest else (m : rest)
    where s' = makeOneMove s m
          rest = pruneWaits s ms

-- initialize random values
prepareRun n input = do
  seed <- newStdGen
  let ms  = randomRs (1, 4) seed 
  let ps  = randomRs (0, n) seed
  result <- run input ps $ map intToMove ms
  return result
  where
       intToMove :: Int -> Move
       intToMove 1 = MRight
       intToMove 2 = MLeft
       intToMove 3 = MUp
       intToMove  _  = MDown

data Verbosity = MoveSequence | Dump deriving (Eq, Ord, Show)
  
main :: IO ()
main = do
--  resultV <- newMVar mempty
--  mainT <- myThreadId
--  _ <- installHandler sigINT (Catch (handleInterrupt resultV mainT)) Nothing
  input <- B.getContents
  let runs = [prepareRun (100-i) (new input) | i<-([1..400]::[Int])]
  -- TODO: Store results one by one in resultV
  results <- fmap (sortBy (flip compare)) (sequence runs)
  let (maxScore, maxMoves) = head results
  -- TODO: Output using Builder/ByteString
  args <- getArgs
  when (args == ["-v"]) $
    forM_ results $ \(score, moves) ->
      hPutStrLn stderr (show score ++ " " ++ map fromMove moves)
  putStrLn (map fromMove maxMoves)
