module Main where

import System.Environment (getArgs)

import VM


main :: IO ()
main = do
  [_, path] <- getArgs
  s <- newFromFile path
  moves <- getContents
  shortDump (makeMoves s moves)
