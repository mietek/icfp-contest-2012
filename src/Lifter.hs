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
import System.IO (hPutStrLn, hPutStr, stderr)
import System.IO (hPrint)
import System.Random (newStdGen, randomRs)
import qualified Data.Map as M
import VM
import Utils


-- print the dijkstra graph
cMAX = 2147483647

maxSteps = 2000

myPrint c x =
    let str = if x ==  cMAX then "X" else show x in
    let str2 = if c then "\n" else "" in
    hPutStr stderr (str ++ str2)

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
testMovesList _ s _ [] l = l
testMovesList steps s prefix (m:ms) l =
   let (b, s') = testMoves s m in
   let goodMoves = if b then l ++ [(s', steps, prefix++m)] else l in
   testMovesList steps s prefix ms goodMoves

-- to complicated
myFind _ [] = []
myFind g ((p, m):xs) = if p==g then [m] else myFind g xs

getSomePossibilities s c r p m steps = all ++ [[m]]
   where
-- several sequences of moves
      --find lambda!
      moves = findA s c r (\(_, _, fp) -> isLambda s fp || isLift s fp)
      -- probably wrong, but i am too tired / jmi

      rocks = findMoveRocks s
      (t, goal) = chooseGoal s c (\(_, _, fp) -> let myRocks = filter (\(p', _) -> p'==fp) rocks in myRocks /= []) (getWorldSize s) r
      mak1 = if t /= ORobot then findPath s c r goal else []
      mak2 = if t /= ORobot then myFind goal rocks else []
      movesComak = mak1++mak2
      -- /probably wrong
      --find trampolina! hop hop
      moves2 = findA s c r (\(_, _, fp) -> isTrampoline s fp || isRazor s fp)
      -- find earth!
      moves3 = findA s c r (\(_, _, fp) -> isEarth s fp)
      -- small probability of doing nothing
      all = if p then [] else [moves, moves2, movesComak, moves3]

-- run :: MVar Builder -> State -> [Move] -> [Int] -> IO (Int, [Move])
-- main function
goDijkstra (p:ps) (m:ms) queue (bestScore, bestMoves) (s,steps,prefix)  = do
      let r = getRobotPoint s
      let c = buildCostTable s r
      printCT c s
      let possibilities = filter (\x -> x /= [])  $ getSomePossibilities s c r p m steps
      let steps' = if length(possibilities) <= 1 then steps+50 else steps+1
      let moves = if possibilities == [] then [[MRight], [MLeft], [MDown], [MUp]] else possibilities
      let tMoves = testMovesList steps s prefix moves []
      if tMoves == []
          then return ((0, [MAbort]), queue)
          else
              let ((s', _, result):answers) =  testMovesList steps s prefix moves [] in

              let score = getScore s' in
      --      dump s'
              if  (getCondition s')/= CNone || steps' > maxSteps
                 then return (((getScore s') , result), queue)
                 else goDijkstra ps ms  (queue ++ answers) (if bestScore>score then (bestScore, bestMoves) else (score , result)) (s',steps', result)

-- mietek's function
handleInterrupt :: MVar Builder -> ThreadId -> IO ()
handleInterrupt resultV mainT = do
  result <- takeMVar resultV
  toByteStringIO B.putStrLn result
  killThread mainT

-- <<<<<<< HEAD
-- refine :: [(State, Int, [Move])] -> [(State, Int, [Move])]
-- refine x = take maxSteps $ sortBy (\(s1, _, m1) -> (
--                        \(s2, _, m2) -> (
--                           if (getScore s1)-(length m1) < (getScore s2)+(length m2) then LT else GT
--                    ))) x
-- =======
refine :: [(State,Int,[Move])] -> [(State,Int,[Move])]
refine = map getMin . M.elems . atSamePos where
    atSamePos = foldr (\ a@(st,x,ms) acc ->
                           let cpos = getRobotPoint st
                           in if cpos `M.member` acc
                               then M.update (Just .(a:)) cpos acc
                              else M.insert cpos [a] acc) M.empty
    getMin = head . sortBy criterium
    criterium (st1,_,ms1) (st2,_,ms2)
        | getScore st1 >= getScore st2 = LT
        | getCollectedLambdaCount st1 >= getCollectedLambdaCount st2 = LT
        | getBlockedLambdas st1 <= getBlockedLambdas st2 = LT
        | length ms1 <= length ms2 = LT
        | getRobotHealt st1 >= getRobotHealt st2 = LT
        | otherwise = GT


-- initialize random values
prepareRun :: Int -> Int -> [(State, Int, [Move])] -> [(Int, [Move])] -> IO [(Int, [Move])]
prepareRun _ _ [] results = do return results
prepareRun d n (x:xs) previousResults = do
  seed <- newStdGen
  let ms  = map f $ randomRs (1, 5) seed
  let ps  = map (\x -> x == 1) $ randomRs (1, n) seed
  (result, rest) <- goDijkstra ps ms [] (0, [MAbort]) x
  let rest' = refine $ xs ++ rest
--  print $ length rest'
  if d == 0 || rest' == []
      then return $ result:previousResults
      else prepareRun (d-1) n rest' $ result:previousResults

data Verbosity = MoveSequence | Dump deriving (Eq, Ord, Show)

main :: IO ()
main = do
--  resultV <- newMVar mempty
--  mainT <- myThreadId
--  _ <- installHandler sigINT (Catch (handleInterrupt resultV mainT)) Nothing
  rawInput <- B.getContents
  let input = new rawInput
--  let runs = [prepareRun i input | i<-([1..400]::[Int])]
  runs <- prepareRun 5000 500 [(input, 0, [])] []
  -- TODO: Store results one by one in resultV
  let results = sortBy (flip compare) runs
  let (_, maxMoves) = head results
  -- TODO: Output using Builder/ByteString
  args <- getArgs
  when (args == ["-v"]) $
--    forM_ results $ \(score, moves) ->
--      hPutStrLn stderr (show score ++ " " ++ map fromMove moves)
	dump $ makeMoves input maxMoves
  putStrLn (map fromMove maxMoves)
