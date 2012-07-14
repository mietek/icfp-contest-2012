module Main where

import Blaze.ByteString.Builder (Builder, toByteStringIO)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Control.Concurrent (ThreadId, killThread, myThreadId, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, takeMVar)
import qualified Data.ByteString.Char8 as B
import Data.Monoid (mappend, mempty)
import System.Posix.Signals (Handler(Catch), installHandler, sigINT)

import VM


run :: MVar Builder -> State -> IO ()
run resultV s0 = goRight s0
  where
    goRight s = do
      dump s
      let s' = makeOneMove s 'R'
      modifyMVar_ resultV $ \result ->
        return (result `mappend` fromChar 'R')
      threadDelay 2000000
      goRight s'

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
  run resultV (new input)
