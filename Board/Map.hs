{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances,TypeFamilies    #-}
module Board.Map where

import Board.Class
import Types

import Data.Map (Map)
import qualified Data.Map as M

import Data.List (sortBy,unfoldr)
import qualified Data.ByteString.Char8 as B

type Pos = Position -- Y x X
type MBoard = (Pos,Map Pos Object) -- Size x Board

foo :: MBoard -> [(Pos,Object)]
foo = M.toList . snd

sf (x,y) (a,b) | x < a && y == b = LT
sf (x,y) (a,b) | y > b = LT
sf (x,y) (a,b) = GT


showMap :: MBoard -> B.ByteString
showMap ((_,n),b)  = B.concat . map (B.pack . myshow ) . sortBy (\x y -> fst x `compare`  fst y) $ M.toList b where
        myshow ((_,k),e) = show e ++ if k == n-1 then "\n" else ""


readMap :: B.ByteString -> MBoard
readMap bs = ((,) (n,m)) . M.fromList . concat . zipWith go [0..] $ lns  where
    lns = B.lines bs
    (n,m) = (length lns, B.length $ head lns)
    go i = zipWith (\x e -> ((i,x),e)) [0..] .  B.foldr (\c a -> readObj c  : a) []

instance Board MBoard where
    type Idx = Pos

    showBoard = showMap
    readBoard = readMap

    getObject (k,l) ((n,m),b) = M.lookup (n-k,l-1) b
    updateObject f (k,l) (s@(n,_),b) = (s,M.update f (n-k,l-1) b)




--readMap