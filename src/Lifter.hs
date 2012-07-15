module Main where

import Blaze.ByteString.Builder (Builder, toByteStringIO)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Control.Concurrent (ThreadId, killThread, myThreadId, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, takeMVar)
import qualified Data.ByteString.Char8 as B
import Data.Monoid (mappend, mempty)
import System.Posix.Signals (Handler(Catch), installHandler, sigINT)
import System.Random
import VM


run :: MVar Builder -> State -> [Char]-> IO ()
run resultV s0 l = goRandom s0 l
  where
    goRandom s (m:ms) = do
      dump s
      let s' = makeOneMove_ s m
      modifyMVar_ resultV $ \result ->
        return (result `mappend` fromChar m)
      threadDelay 500000
      goRandom s' ms

handleInterrupt :: MVar Builder -> ThreadId -> IO ()
handleInterrupt resultV mainT = do
  result <- takeMVar resultV
  toByteStringIO B.putStrLn result
  killThread mainT

main :: IO ()
main = do
  resultV <- newMVar mempty
  mainT <- myThreadId
  _ <- installHandler sigINT (Catch (handleInterrupt resultV mainT)) Nothing
  input <- B.getContents
  seed <- newStdGen
  run resultV (new input) $ map f (randomRs ('a', 'd') seed)
  where
       f 'a' = 'R'
       f 'b' = 'L'
       f 'c' = 'U'
       f 'd' = 'D'
