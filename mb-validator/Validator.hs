module Main where

import System.Environment (getArgs)

import VM


main :: IO ()
main = do
  [_, path] <- getArgs
  s <- newFromFile path
  moves <- getLine
  shortDump (makeMoves s moves)
