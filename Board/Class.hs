{-# LANGUAGE TypeFamilies , NoMonomorphismRestriction, FlexibleContexts  #-}

module Board.Class where

import Types
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Prelude hiding (Either(..))
import Control.Arrow ((&&&))

class  Show Idx => Board a where
    type Idx

-- board `io`
    showBoard   :: a -> ByteString
    readBoard   :: ByteString -> a

    getObject    :: Idx -> a -> Maybe Object
    updateObject :: (Object -> Maybe Object) -> Idx -> a -> a

    setObject   :: Object -> Idx  -> a -> a
    setObject o = updateObject (const (Just o))

    getObject' :: Idx -> a -> Object
    getObject' idx = maybe (error $ "canMove: no object at " ++ show idx) id . getObject idx

    getCurrentPos :: a -> Idx

readBoardIO ::  Board a => IO a
readBoardIO = fmap readBoard B.getContents

printBoard :: Board a => a -> IO ()
printBoard = B.putStrLn . showBoard


-- updateBoard :: Move -> Idx -> a  -> a
-- updateBoard mov idx b =


--canMove :: Board a => Move -> Idx -> a  -> (Bool,Effect Idx)
canMove m idx b =
  let idx' = evalMoves m idx
  in case getObject' idx' b of
       Empty -> (True,None)
       Earth -> (True,Eat)
       Lambda -> (True,Pick idx')
       OLift  -> (True,Win)
       Rock | m `elem` [Left,Right] ->
            case getObject' (evalMoves m idx') b of
              Empty -> (True,if m == Left then RockL idx' else RockR idx')
              _     -> (False,None)


isFinished :: Board a => a -> Bool
isFinished = (==RL) . uncurry getObject' . (getCurrentPos &&& id)
-- isLoosed    :: a -> Bool

-- isStuck   :: Board a =>  a -> Bool
-- isStuck b = all (\m -> not$ fst$canMove m idx b) mvs where
--    mvs = map toEnum [0..3]
--    idx = getCurrentPos b
