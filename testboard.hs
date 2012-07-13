module Main where


import Board.Map
import Board.Class
import Types
import qualified Data.ByteString.Char8 as B

main = do
  b <- readBoardIO :: IO MBoard

  printBoard b

  print $ getObject (1,1) b

  print $ getObject (5,4) b
  let b' = setObject Wall (5,4) b
  print $ getObject (5,4) b'
  print $ fst b'
  printBoard b'
