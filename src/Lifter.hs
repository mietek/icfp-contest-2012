module Main where

import System.IO
import Blaze.ByteString.Builder (Builder, toByteStringIO)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Control.Concurrent (ThreadId, killThread, myThreadId, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, takeMVar)
import qualified Data.ByteString.Char8 as B
import Data.Monoid (mappend, mempty)
-- import System.Posix.Signals (Handler(Catch), installHandler, sigINT)
import System.Random
import VM

cMAX = 2147483647

myPrint c x = 
    let str = if x ==  cMAX then "X" else show x in 
    let str2 = if c then "\n" else "" in
    putStr (str ++ str2)

chooseGoal s c (x, y) r =
      let l = clean [ ((getCost c (i, j)), (get s (i, j)), (i, j)) | i <- [1..x], j<- [1..y]] in 
      select l
   where 
      select [] = (ORobot, r)
      select ((_,t,x):xs) = (t, x)
      clean = filter (\ (c, t, _) -> (t == OLambda || t == OOpenLift) && c< cMAX)
      
run :: MVar Builder -> State -> [Move]-> IO ()
run resultV s0 l = goRandom s0 l []
  where
    goRandom s (m:ms) prefix = do
      let r = getRobotPoint s
      let (wx, wy) = getWorldSize s
      let c = buildCostTable s r
--      flip mapM_ [1..wx] $ \x -> 
--        flip mapM_ [1..wy] $ \y -> myPrint (y == wy) $ getCost c (x,  wy+1-y)
      let (t, goal) = chooseGoal s c (wx, wy) r
      let answer = if t /= ORobot then findPath s c r goal else [m]
      let s' = makeMoves s answer
      let result = prefix ++ answer
      print (length(result), result)
      dump s'
      if  (getCondition s')/= CNone || (t == ORobot && length(result)>123) 
        then print result 
        else goRandom s' ms result
--      modifyMVar_ resultV $ \result ->
--        return (result `mappend` fromString answer)
--      goRandom s' ms (prefix++answer)

handleInterrupt :: MVar Builder -> ThreadId -> IO ()
handleInterrupt resultV mainT = do
  result <- takeMVar resultV
  toByteStringIO B.putStrLn result
  killThread mainT

main :: IO ()
main = do
  resultV <- newMVar mempty
  mainT <- myThreadId
--  _ <- installHandler sigINT (Catch (handleInterrupt resultV mainT)) Nothing
  input <- B.getContents
  seed <- newStdGen
  let ms  = randomRs ('a', 'd') seed
  run resultV (new input) $ map f ms
  where
       f 'a' = MRight
       f 'b' = MLeft
       f 'c' = MUp
       f  _  = MDown
