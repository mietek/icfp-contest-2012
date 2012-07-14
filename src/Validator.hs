module Main where

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
    AllDumps  -> dumpAndMove s0 moves
      where
        dumpAndMove _ [] = return ()
        dumpAndMove s (m : ms) = do
          let s1 = makeOneMove s m
          if s1 /= s
            then do
              dump s1
              dumpAndMove s1 ms
            else return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path]         -> runWithFile Score path
    ["-v", path]   -> runWithFile Score path
    ["-vv", path]  -> runWithFile FinalDump path
    ["-vvv", path] -> runWithFile AllDumps path
    _ -> putStrLn "Usage: echo <moves> | ./validator [-v|-vv|-vvv] <map>"
