{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances,TypeFamilies    #-}
module Board.Map where

import Board.Class
import Types

import Data.Map (Map)
import qualified Data.Map as M

import Data.List (sortBy,find)
import qualified Data.ByteString.Char8 as B

type Pos = Position -- Y x X
type MBoard = (Pos,Pos,Map Pos Object) -- CurrentPosition x Size x Board


showMap :: MBoard -> B.ByteString
showMap (_,(_,n),b)  = B.concat . map (B.pack . myshow ) . sortBy (\x y -> fst x `compare`  fst y) $ M.toList b where
        myshow ((_,k),e) = show e ++ if k == n-1 then "\n" else ""


readMap :: B.ByteString -> MBoard
readMap bs = mkBoard . concat . zipWith go [0..] $ lns  where
    lns = B.lines bs
    s@(n,m) = (length lns, B.length $ head lns)
    go i = zipWith (\x e -> ((i,x),e)) [0..] .  B.foldr (\c a -> readObj c  : a) []
    mkBoard ls =
        let beg = maybe (1,1) fst $ find ((==Robot) . snd)
        in (beg,s,M.fromList ls)

instance Board MBoard where
    type Idx = Pos

    showBoard = showMap
    readBoard = readMap

    getObject (k,l) (_,(n,m),b) = M.lookup (n-k,l-1) b
    updateObject f (k,l) (_,s@(n,_),b) = (s,M.update f (n-k,l-1) b)

    getCurrentPos (p,_,_) = p



--readMap