module Types where

import Prelude hiding (Either(..))

data Object = Robot | Lambda | Rock | Wall | CLift | OLift | Earth | Empty
  deriving (Enum,Eq)

instance Show Object where
    show Rock   = "*"
    show Lambda = "\\"
    show Robot  = "R"
    show Wall   = "#"
    show CLift  = "L"
    show Earth  = "."
    show Empty  = " "

readObj :: Char -> Object
readObj 'R'  = Robot
readObj '*'  = Rock
readObj '#'  = Wall
readObj '\\' = Lambda
readObj 'L'  = CLift
readObj '.'  = Earth
readObj ' '  = Empty

instance Read Object where
    readsPrec n ('R':rest)  = [(Robot,rest)]
    readsPrec n ('*':rest)  = [(Rock,rest)]
    readsPrec n ('#':rest)  = [(Wall,rest)]
    readsPrec n ('\\':rest) = [(Lambda,rest)]
    readsPrec n ('L':rest)  = [(CLift,rest)]
    readsPrec n ('.':rest)  = [(Earth,rest)]
    readsPrec n (' ':rest)  = [(Empty,rest)]


data Move = Left | Right | Down | Up |  Wait | Abort
  deriving (Enum,Eq)

instance Show Move where
    show Left  = "L"
    show Right = "R"
    show Down  = "D"
    show Up    = "U"
    show Wait  = "W"
    show Abort = "A"

instance Read Move where
    readsPrec n ('L':rest) = [(Left,rest)]
    readsPrec n ('R':rest) = [(Right,rest)]
    readsPrec n ('D':rest) = [(Down,rest)]
    readsPrec n ('U':rest) = [(Up,rest)]
    readsPrec n ('W':rest) = [(Wait,rest)]
    readsPrec n ('A':rest) = [(Abort,rest)]



type Position = (Int,Int)

evalMoves :: Move -> Position -> Position
evalMoves Left  (x,y) = (x - 1 ,y)
evalMoves Right (x,y) = (x + 1 ,y)
evalMoves Up (x,y)    = (x ,y + 1)
evalMoves Down (x,y)  = (x ,y - 1 )
evalMoves Wait (x,y)  = (x  ,y)
evalMoves Abort (x,y) = undefined -- halt ?
