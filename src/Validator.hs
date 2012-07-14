module Main where

import Control.Monad (foldM_)
import System.Environment (getArgs)

import VM


data Verbosity = Score | FinalDump | AllDumps deriving (Eq, Ord, Show)


runWithFile :: Verbosity -> String -> IO ()
runWithFile v path = do
  s0 <- newFromFile path
  moves <- getContents
  case v of
    Score     -> print (getScore (makeMoves s0 moves))
    FinalDump -> dump (makeMoves s0 moves)
    AllDumps  -> foldM_ dumpAndMove s0 moves
      where
        dumpAndMove s m = do
          let s' = makeOneMove s m
          dump s'
          return s'

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path]         -> runWithFile Score path
    ["-v", path]   -> runWithFile Score path
    ["-vv", path]  -> runWithFile FinalDump path
    ["-vvv", path] -> runWithFile AllDumps path
    _ -> putStrLn "Usage: echo <moves> | ./validator [-v|-vv|-vvv] <map>"
