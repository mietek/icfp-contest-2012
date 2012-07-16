module Utils where

import Control.Arrow ((***))
import Data.List (sort, zip4)
import Data.Maybe (catMaybes)
import System.Random (newStdGen, randomRs)

import VM



findPath :: State -> CostTable -> Point -> Point -> [Move]
findPath s ct from0 to = map reverseMove (loop to [])
  where
    loop from path
      | from == from0 = path
      | (getDist ct from) <= minDist = []
      | otherwise = loop step (move : path)
        where
          moves = [MLeft, MRight, MUp, MDown]
          steps = map (imagineStep s from) moves
          dists = map (getDist ct) steps
          costs = map (getCost ct) steps
          minDist = head $ sort $ dists
          minCost = head $ sort $ costs
          (cost, dist, step, move) = head $ sort $ (filter (\(x,y,_,m) -> y == minDist) (zip4 costs dists steps moves))


iterateBoard :: State -> (Point -> Object -> [a] ) -> [a]
iterateBoard s f = iter (1,1) [] where
    (m,n) = succ *** id  $ getWorldSize s
    iter (x,y) acc | x == m && y == n = acc
    iter (x,y) acc | x == m = iter (1,y+1) acc
    iter p@(x,y) acc = iter (x+1,y) (f p (get s p) ++ acc)

findRocks ::  State -> [Point]
findRocks s =
  iterateBoard s $ \p o ->
    if isRock s p || isHORock s p then [p] else []

findMoveRocks :: State -> [(Point,Move)]
findMoveRocks s = concatMap moveable $  findRocks s where
    moveable p = catMaybes $ zipWith check [MRight,MLeft,MUp,MDown] $ cycle [p]
    check MRight (x,y) | isEmpty s (x+1,y) && isEnterable s (x+1,y) =
                      Just ((x-1,y) ,MRight)
    check MLeft (x,y) | isEmpty s (x-1,y) && isEnterable s (x+1,y) =
                      Just ((x-1,y),MLeft)
    check MUp (x,y)   | isEmpty s (x,y+1) && isEnterable s (x,y-1) =
                      Just ((x,y-1) ,MDown)
    check MDown (x,y) | isEmpty s (x,y-1) && isEnterable s (x,y+1) =
                      Just ((x,y+1) ,MDown)
    check _ _ = Nothing

evalMoves :: Move -> Point -> Point
evalMoves MLeft (x,y) = (pred x ,y)
evalMoves MRight (x,y) = (succ x ,y)
evalMoves MUp (x,y) = (x ,succ y )
evalMoves MDown (x,y) = (x ,pred y )
evalMoves MWait (x,y) = (x ,y)
evalMoves MAbort (x,y) = undefined -- halt ?


canMove :: State -> Point -> Bool
canMove s p =any (uncurry (&&) . (isEnterable s *** isSafe s). flip evalMoves p) moves where
    moves = [MDown,MLeft,MRight,MUp]

willDeadLock :: State -> Move -> Point -> Bool
willDeadLock s m p = canMove s' $ getRobotPoint s' where
   s' = flip makeOneMove m $ imagineRobotAt s p
