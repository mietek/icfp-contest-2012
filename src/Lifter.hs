module Main where

import Blaze.ByteString.Builder (Builder, toByteStringIO)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Control.Concurrent (ThreadId, killThread, myThreadId, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, takeMVar)
import Control.Monad (forM_, sequence, when)
import qualified Data.ByteString.Char8 as B
import Data.List (sort, sortBy, zip4)
import Data.Monoid (mappend, mempty)
-- import System.Posix.Signals (Handler(Catch), installHandler, sigINT)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Random (newStdGen, randomRs)

import VM
import Utils


-- print the dijkstra graph
cMAX = 2147483647
myPrint c x = 
    let str = if x ==  cMAX then "X" else show x in 
    let str2 = if c then "\n" else "" in
    putStr (str ++ str2)

printCT c s = 
   let (wx, wy) = getWorldSize s in 
     flip mapM_ [1..wy] $ \y -> 
       flip mapM_ [1..wx] $ \x -> myPrint (x == wx) $ getCost c (x,  wy+1-y)

f :: Int -> Move
f 1 = MRight
f 2 = MLeft
f 3 = MUp
f 4 = MWait
f  _  = MDown

-- where do you want to go today? specify it using p, this function will find a place
chooseGoal s c p (x, y) r =
      let l = sort $ clean [ ((getCost c (i, j)), (get s (i, j)), (i, j)) | i <- [1..x], j<- [1..y]] in 
      select l
   where 
      select [] = (ORobot, r)
      -- take first one --- maybe all instead?
      select ((_,t,x):xs) = (t, x)
      clean = (filter p) . (filter (\(c, _, _) -> c < cMAX))

-- find a goal and a path there
findA s c r p = 
      let (t, goal) = chooseGoal s c p (getWorldSize s) r in
      if t /= ORobot then findPath s c r goal else []

-- checks if the moves kills the robot      
testMoves s m = 
    let s' = makeMoves s m in
    if (getCondition s') /= CLose then (True, s') else (False, s)
    
-- finds the moves that do not kill robot
testMovesList s _ [] l = l
testMovesList s prefix (m:ms) l = 
   let (b, s') = testMoves s m in 
   let goodMoves = if b then l ++ [(s', prefix++m)] else l in
   testMovesList s prefix ms goodMoves

-- to complicated
myFind _ [] = []
myFind g ((p, m):xs) = if p==g then [m] else myFind g xs 
 
getSomePossibilities s c r p m steps = all ++ [[m]]
   where
-- several sequences of moves
      --find lambda!
      moves = findA s c r (\(fc, ft, fp) -> isLambda s fp || isLift s fp)
      -- probably wrong, but i am too tired / jmi
      rocks = findMoveRocks s 
      (t, goal) = chooseGoal s c (\(fc, ft, fp) -> let myRocks = filter (\(p, m) -> p==fp) rocks in myRocks /= []) (getWorldSize s) r
      mak1 = if t /= ORobot then findPath s c r goal else []
      mak2 = if t /= ORobot then myFind goal rocks else []
      movesComak = mak1++mak2
      -- /probably wrong
      --find trampolina! hop hop
      moves2 = findA s c r (\(fc, ft, fp) -> isTrampoline s fp || isRazor s fp)
      -- find earth!
      moves3 = findA s c r (\(fc, ft, fp) -> isEarth s fp)
      -- small probability of doing nothing
      all = if p then [] else [moves, moves2, movesComak, moves3]
 
-- run :: MVar Builder -> State -> [Move] -> [Int] -> IO (Int, [Move])
-- main function
goDijkstra (s,steps,prefix) (p:ps) (m:ms) queue = do
      let r = getRobotPoint s
      let c = buildCostTable s r
      printCT c s 
      let possibilities = filter (\x -> x /= [])  $ getSomePossibilities s c r p m steps
      let steps' = if length(possibilities) <= 1 then steps+50 else steps+1
      let moves = if possibilities == [] then [[MRight], [MLeft], [MDown], [MUp]] else possibilities
      let ((s', result):answers) =  testMovesList s prefix moves []
      --      dump s' 
      if  (getCondition s')/= CNone || steps' > 1000
        then return (((getScore s') , result), queue)
        else goDijkstra (s',steps', result) ps ms  (queue ++ answers)


-- mietek's function
handleInterrupt :: MVar Builder -> ThreadId -> IO ()
handleInterrupt resultV mainT = do
  result <- takeMVar resultV
  toByteStringIO B.putStrLn result
  killThread mainT

-- initialize random values
prepareRun :: Int -> State -> IO [(Int, [Move])]
prepareRun n input = do
  seed <- newStdGen
  let ms  = map f $ randomRs (1, 5) seed 
  let ps  = map (\x -> if x == 1 then True else False) $ randomRs (1, n) seed
  (result, rest) <- goDijkstra (input,0,[]) ps ms []
  return [result]

data Verbosity = MoveSequence | Dump deriving (Eq, Ord, Show)
  
main :: IO ()
main = do
--  resultV <- newMVar mempty
--  mainT <- myThreadId
--  _ <- installHandler sigINT (Catch (handleInterrupt resultV mainT)) Nothing
  rawInput <- B.getContents
  let input = new rawInput
--  let runs = [prepareRun i input | i<-([1..400]::[Int])]
  runs <- prepareRun 1000 input
  -- TODO: Store results one by one in resultV
  let results = sortBy (flip compare) runs
  let (maxScore, maxMoves) = head results
  -- TODO: Output using Builder/ByteString
  args <- getArgs
  when (args == ["-v"]) $
--    forM_ results $ \(score, moves) ->
--      hPutStrLn stderr (show score ++ " " ++ map fromMove moves)
	dump $ makeMoves input maxMoves
  putStrLn (map fromMove maxMoves)
