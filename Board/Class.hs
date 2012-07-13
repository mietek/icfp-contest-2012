{-# LANGUAGE TypeFamilies , NoMonomorphismRestriction #-}

module Board.Class where

import Types
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B


class  Board a where
    type Idx

-- board `io`
    showBoard   :: a -> ByteString
    readBoard   :: ByteString -> a

    getObject    :: Idx -> a -> Maybe Object
    updateObject :: (Object -> Maybe Object) -> Idx -> a -> a

    setObject   :: Object -> Idx  -> a -> a
    setObject o = updateObject (const (Just o))

readBoardIO ::  Board a => IO a
readBoardIO = fmap readBoard B.getContents

printBoard :: Board a => a -> IO ()
printBoard = B.putStrLn . showBoard


-- updateBoard :: Move -> Idx -> a  -> a
-- updateBoard

-- canMove     :: Move -> Idx -> a  -> Bool
-- isFinished  :: a -> Bool
-- isLoosed    :: a -> Bool
-- isStuck     :: a -> Bool
