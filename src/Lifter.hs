module Main where

import System.IO
import Blaze.ByteString.Builder (Builder, toByteStringIO)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Control.Concurrent (ThreadId, killThread, myThreadId, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, takeMVar)
import qualified Data.ByteString.Char8 as B
import Data.List (sort, zip4)
import Data.Monoid (mappend, mempty)
-- import System.Posix.Signals (Handler(Catch), installHandler, sigINT)
import System.Random
import VM

cMAX = 2147483647

myPrint c x = 
    let str = if x ==  cMAX then "X" else show x in 
    let str2 = if c then "\n" else "" in
    putStr (str ++ str2)

chooseGoal s c p (x, y) r =
      let l = sort $ clean [ ((getCost c (i, j)), (get s (i, j)), (i, j)) | i <- [1..x], j<- [1..y]] in 
      select l
   where 
      select [] = (ORobot, r)
      select ((_,t,x):xs) = (t, x)
      clean = (filter p) . (filter (\(c, _, _) -> c < cMAX))

findA s c r p = 
      let (t, goal) = chooseGoal s c p (getWorldSize s) r in
      if t /= ORobot then findPath s c r goal else []

testMoves s m = 
    let s' = makeMoves s m in
    if (getCondition s') /= CLose then (True, s') else (False, s)
    
testMovesList s [] = (False, s, [])
testMovesList s (m:ms) = let (b, s') = testMoves s m in if b then (b, s', m) else testMovesList s (ms)

    
-- run :: MVar Builder -> State -> [Move] -> [Int] -> IO (Int, [Move])
run s0 ps ms = goDijkstra s0 ps ms []
  where
    goDijkstra s (p:ps) (m:ms) prefix = do
      let r = getRobotPoint s
      let (wx, wy) = getWorldSize s
      let c = buildCostTable s r
--      flip mapM_ [1..wy] $ \y -> 
--        flip mapM_ [1..wx] $ \x -> myPrint (x == wx) $ getCost c (x,  wy+1-y)
      let moves = findA s c r (\(fc, ft, fp) -> isLambda s fp || isLift s fp)
      let moves2 = findA s c r (\(fc, ft, fp) -> isTrampoline s fp || isRazor s fp)
      let moves3 = findA s c r (\(fc, ft, fp) -> isEarth s fp)
      -- small probability of doing nothing
      let all = if (length prefix) < p && (p `mod` 3 == 0) then [] else [moves, moves2, moves3]
      let (b, s', answer) =  testMovesList s $ filter (\x -> x /= [])  $ all ++ [[m], [MRight], [MLeft], [MDown], [MUp]]
--      dump s' 
      let result = prefix ++ answer
--      print result
--      hFlush stdout
      if  (getCondition s')/= CNone || (moves == [] && length(result)>153) 
        then return ((getScore s') , result) 
        else goDijkstra s' ps ms result
--      modifyMVar_ resultV $ \result ->
--        return (result `mappend` fromString answer)
--      goRandom s' ms (prefix++answer)

handleInterrupt :: MVar Builder -> ThreadId -> IO ()
handleInterrupt resultV mainT = do
  result <- takeMVar resultV
  toByteStringIO B.putStrLn result
  killThread mainT

prepareRun n input = do
  seed <- newStdGen
  let ms  = randomRs (1, 4) seed 
  let ps  = randomRs (0, n) seed
  result <- run input ps $ map f ms
  return result
  where
       f :: Int -> Move
       f 1 = MRight
       f 2 = MLeft
       f 3 = MUp
       f  _  = MDown

  
main :: IO ()
main = do
--  resultV <- newMVar mempty
--  mainT <- myThreadId
--  _ <- installHandler sigINT (Catch (handleInterrupt resultV mainT)) Nothing
  input <- B.getContents
  let all = [prepareRun (100-i) (new input) | i<-([1..100]::[Int])] 
  mapM_(\x -> print =<< x) all