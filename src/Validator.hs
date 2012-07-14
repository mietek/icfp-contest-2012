module Main where

import System.Environment (getArgs)

import VM


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-vv", path] -> runWithFile path
    [path] -> runWithFile path
    _ -> putStrLn "Usage: echo <moves> | ./validator [-vv] <map>"


runWithFile :: String -> IO ()
runWithFile path = do
  s <- newFromFile path
  moves <- getContents
  dump (makeMoves s moves)
