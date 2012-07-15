{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module VM where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.List (sort, zip4)
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.C.String (CString, castCharToCChar, castCCharToChar, withCString)
import Foreign.C.Types (CChar (..), CLong (..))
import Foreign.Marshal.Alloc (alloca, finalizerFree)
import Foreign.Marshal.Utils (toBool)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (catMaybes)

import Control.Arrow

data CState
type CStatePtr = Ptr CState
type CStateFPtr = ForeignPtr CState
data State = State !(CStateFPtr)

instance Eq State where
  (==) = equal

unwrapState :: State -> (CStatePtr -> IO a) -> a
unwrapState (State sfp) action =
  unsafePerformIO (withForeignPtr sfp action)

wrapState :: CStatePtr -> IO State
wrapState sp = do
  sfp <- newForeignPtr finalizerFree sp
  return (State sfp)

getInt :: (CStatePtr -> CLong) -> State -> Int
getInt action s =
  unwrapState s $ \sp ->
    return (fromEnum (action sp))

getIntPair :: (CStatePtr -> Ptr CLong -> Ptr CLong -> IO ()) -> State -> (Int, Int)
getIntPair action s =
  unwrapState s $ \sp ->
    alloca $ \ap ->
      alloca $ \bp -> do
        action sp ap bp
        a <- peek ap
        b <- peek bp
        return (fromEnum a, fromEnum b)


data LiftState = Closed | Open deriving (Eq, Ord, Show)

data Object = ORobot | OWall | ORock | OLambda | OLift LiftState
            | OEarth | OEmpty | OBeard | ORazor | OTrampoline Trampoline
            | OTarget Target | OHoRock
  deriving (Eq, Ord)

instance Show Object where
  show object = [fromObject object]

fromObject :: Object -> Char
fromObject ORobot                   = 'R'
fromObject OWall                    = '#'
fromObject ORock                    = '*'
fromObject OLambda                  = '\\'
fromObject (OLift Closed)           = 'L'
fromObject (OLift Open)             = 'O'
fromObject OEarth                   = '.'
fromObject OEmpty                   = ' '
fromObject OBeard                   = 'W'
fromObject ORazor                   = '!'
fromObject OHoRock                  = '@'
fromObject (OTrampoline trampoline) = fromTrampoline trampoline
fromObject (OTarget target)         = fromTarget target

toObject :: Char -> Object
toObject 'R'                = ORobot
toObject '#'                = OWall
toObject '*'                = ORock
toObject '\\'               = OLambda
toObject 'L'                = OLift Closed
toObject 'O'                = OLift Open
toObject '.'                = OEarth
toObject ' '                = OEmpty
toObject 'W'                = OBeard
toObject '!'                = ORazor
toObject '@'                = OHoRock
toObject c
  | isValidTrampolineChar c = OTrampoline (toTrampoline c)
  | isValidTargetChar c     = OTarget (toTarget c)
toObject _    = undefined


data Trampoline = TA | TB | TC | TD | TE | TF | TG | TH | TI deriving (Enum, Eq, Ord)

instance Show Trampoline where
  show trampoline = [fromTrampoline trampoline]

firstTrampolineChar :: Char
firstTrampolineChar = 'A'

lastTrampolineChar :: Char
lastTrampolineChar = 'I'

isValidTrampolineChar :: Char -> Bool
isValidTrampolineChar c = c >= firstTrampolineChar && c <= lastTrampolineChar

fromTrampoline :: Trampoline -> Char
fromTrampoline trampoline = toEnum (fromEnum firstTrampolineChar + fromEnum trampoline)

toTrampoline :: Char -> Trampoline
toTrampoline c
  | isValidTrampolineChar c = toEnum (fromEnum c - fromEnum firstTrampolineChar)
toTrampoline _ = undefined


data Target = T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 deriving (Enum, Eq, Ord)

instance Show Target where
  show target = [fromTarget target]

firstTargetChar :: Char
firstTargetChar = '1'

lastTargetChar :: Char
lastTargetChar = '9'

isValidTargetChar :: Char -> Bool
isValidTargetChar c = c >= firstTargetChar && c <= lastTargetChar

fromTarget :: Target -> Char
fromTarget target = toEnum (fromEnum firstTargetChar + fromEnum target)

toTarget :: Char -> Target
toTarget c
  | isValidTargetChar c = toEnum (fromEnum c - fromEnum firstTargetChar)
toTarget _ = undefined


data Move = MLeft | MRight | MUp | MDown | MWait | MAbort deriving (Eq, Ord)

instance Show Move where
  show move = [fromMove move]

fromMove :: Move -> Char
fromMove MLeft  = 'L'
fromMove MRight = 'R'
fromMove MUp    = 'U'
fromMove MDown  = 'D'
fromMove MWait  = 'W'
fromMove MAbort = 'A'

reverseMove :: Move -> Move
reverseMove MLeft  = MRight
reverseMove MRight = MLeft
reverseMove MUp    = MDown
reverseMove MDown  = MUp
reverseMove MWait  = MWait
reverseMove MAbort = MAbort


data Condition = CNone | CWin | CLose | CAbort deriving (Eq, Ord)

instance Show Condition where
  show condition = [fromCondition condition]

fromCondition :: Condition -> Char
fromCondition CNone  = 'N'
fromCondition CWin   = 'W'
fromCondition CLose  = 'L'
fromCondition CAbort = 'A'

toCondition :: Char -> Condition
toCondition 'N' = CNone
toCondition 'W' = CWin
toCondition 'L' = CLose
toCondition 'A' = CAbort
toCondition _   = undefined


type Size = (Int, Int)
type Point = (Int, Int)


foreign import ccall unsafe "libvm.h new"
  cNew :: CLong -> Ptr CChar -> IO CStatePtr

foreign import ccall unsafe "libvm.h new_from_file"
  cNewFromFile :: CString -> IO CStatePtr

foreign import ccall unsafe "libvm.h equal"
  cEqual :: CStatePtr -> CStatePtr -> Bool

foreign import ccall unsafe "libvm.h dump"
  cDump :: CStatePtr -> IO ()

foreign import ccall unsafe "libvm.h get_world_size"
  cGetWorldSize :: CStatePtr -> Ptr CLong -> Ptr CLong -> IO ()

foreign import ccall unsafe "libvm.h get_robot_point"
  cGetRobotPoint :: CStatePtr -> Ptr CLong -> Ptr CLong -> IO ()

foreign import ccall unsafe "libvm.h get_lift_point"
  cGetLiftPoint :: CStatePtr -> Ptr CLong -> Ptr CLong -> IO ()

foreign import ccall unsafe "libvm.h get_water_level"
  cGetWaterLevel :: CStatePtr -> CLong

foreign import ccall unsafe "libvm.h get_flooding_rate"
  cGetFloodingRate :: CStatePtr -> CLong

foreign import ccall unsafe "libvm.h get_robot_waterproofing"
  cGetRobotWaterproofing :: CStatePtr -> CLong

foreign import ccall unsafe "libvm.h get_used_robot_waterproofing"
  cGetUsedRobotWaterproofing :: CStatePtr -> CLong

foreign import ccall unsafe "libvm.h get_beard_growth_rate"
  cGetBeardGrowthRate :: CStatePtr -> CLong

foreign import ccall unsafe "libvm.h get_razor_count"
  cGetRazorCount :: CStatePtr -> CLong

foreign import ccall unsafe "libvm.h get_lambda_count"
  cGetLambdaCount :: CStatePtr -> CLong

foreign import ccall unsafe "libvm.h get_collected_lambda_count"
  cGetCollectedLambdaCount :: CStatePtr -> CLong

foreign import ccall unsafe "libvm.h get_trampoline_count"
  cGetTrampolineCount :: CStatePtr -> CLong

foreign import ccall unsafe "libvm.h get_trampoline_point"
  cGetTrampolinePoint :: CStatePtr -> CChar -> Ptr CLong -> Ptr CLong -> CChar

foreign import ccall unsafe "libvm.h get_target_point"
  cGetTargetPoint :: CStatePtr -> CChar -> Ptr CLong -> Ptr CLong -> CChar

foreign import ccall unsafe "libvm.h get_trampoline_target"
  cGetTrampolineTarget :: CStatePtr -> CChar -> Ptr CChar -> CChar

foreign import ccall unsafe "libvm.h get_move_count"
  cGetMoveCount :: CStatePtr -> CLong

foreign import ccall unsafe "libvm.h get_score"
  cGetScore :: CStatePtr -> CLong

foreign import ccall unsafe "libvm.h get_condition"
  cGetCondition :: CStatePtr -> CChar

foreign import ccall unsafe "libvm.h safe_get"
  cGet :: CStatePtr -> CLong -> CLong -> CChar

foreign import ccall unsafe "libvm.h make_one_move"
  cMakeOneMove :: CStatePtr -> CChar -> IO CStatePtr

foreign import ccall unsafe "libvm.h make_moves"
  cMakeMoves :: CStatePtr -> CString -> IO CStatePtr

foreign import ccall unsafe "libvm.h update_world_ignoring_robot"
  cUpdateWorldIgnoringRobot :: CStatePtr -> IO CStatePtr

foreign import ccall unsafe "libvm.h imagine_robot_at"
  cImagineRobotAt :: CStatePtr -> CLong -> CLong -> IO CStatePtr

foreign import ccall unsafe "libvm.h is_enterable"
  cIsEnterable :: CStatePtr -> CLong -> CLong -> CChar

foreign import ccall unsafe "libvm.h is_safe"
  cIsSafe :: CStatePtr -> CLong -> CLong -> CChar


new :: ByteString -> State
new input =
  unsafePerformIO $
    unsafeUseAsCStringLen input $ \(i, il) -> do
      sp <- cNew (toEnum il) i
      wrapState sp

newFromFile :: String -> IO State
newFromFile path =
  withCString path $ \p -> do
    sp <- cNewFromFile p
    wrapState sp

equal :: State -> State -> Bool
equal s1 s2 =
  unwrapState s1 $ \sp1 ->
    unwrapState s2 $ \sp2 ->
      return (return (cEqual sp1 sp2))

dump :: State -> IO ()
dump s =
  unwrapState s $ \sp ->
    return (cDump sp)

getWorldSize :: State -> Size
getWorldSize = getIntPair cGetWorldSize

getRobotPoint :: State -> Point
getRobotPoint = getIntPair cGetRobotPoint

getLiftPoint :: State -> Point
getLiftPoint = getIntPair cGetLiftPoint

getWaterLevel :: State -> Int
getWaterLevel = getInt cGetWaterLevel

getFloodingRate :: State -> Int
getFloodingRate = getInt cGetFloodingRate

getRobotWaterproofing :: State -> Int
getRobotWaterproofing = getInt cGetRobotWaterproofing

getUsedRobotWaterproofing :: State -> Int
getUsedRobotWaterproofing = getInt cGetUsedRobotWaterproofing

getBeardGrowthRate :: State -> Int
getBeardGrowthRate = getInt cGetBeardGrowthRate

getRazorCount :: State -> Int
getRazorCount = getInt cGetRazorCount

getLambdaCount :: State -> Int
getLambdaCount = getInt cGetLambdaCount

getCollectedLambdaCount :: State -> Int
getCollectedLambdaCount = getInt cGetCollectedLambdaCount

getTrampolineCount :: State -> Int
getTrampolineCount = getInt cGetTrampolineCount

getTrampolinePoint_ :: State -> Char -> Maybe Point
getTrampolinePoint_ s trampoline =
  unwrapState s $ \sp ->
    alloca $ \xp ->
      alloca $ \yp -> do
        let ok = toBool (cGetTrampolinePoint sp (castCharToCChar trampoline) xp yp)
        if ok
          then do
            x <- peek xp
            y <- peek yp
            return (Just (fromEnum x, fromEnum y))
          else return Nothing

getTrampolinePoint :: State -> Trampoline -> Maybe Point
getTrampolinePoint s trampoline = getTrampolinePoint_ s (fromTrampoline trampoline)

getTargetPoint_ :: State -> Char -> Maybe Point
getTargetPoint_ s target =
  unwrapState s $ \sp ->
    alloca $ \xp ->
      alloca $ \yp -> do
        let ok = toBool (cGetTargetPoint sp (castCharToCChar target) xp yp)
        if ok
          then do
            x <- peek xp
            y <- peek yp
            return (Just (fromEnum x, fromEnum y))
          else return Nothing

getTargetPoint :: State -> Target -> Maybe Point
getTargetPoint s target = getTargetPoint_ s (fromTarget target)

getTrampolineTarget_ :: State -> Char -> Maybe Char
getTrampolineTarget_ s trampoline =
  unwrapState s $ \sp ->
    alloca $ \tp -> do
      let ok = toBool (cGetTrampolineTarget sp (castCharToCChar trampoline) tp)
      if ok
        then do
          target <- peek tp
          return (Just (castCCharToChar target))
        else return Nothing

getTrampolineTarget :: State -> Trampoline -> Maybe Target
getTrampolineTarget s trampoline = fmap toTarget (getTrampolineTarget_ s (fromTrampoline trampoline))

getMoveCount :: State -> Int
getMoveCount = getInt cGetMoveCount

getScore :: State -> Int
getScore = getInt cGetScore

getCondition :: State -> Condition
getCondition s =
  unwrapState s $ \sp ->
    return (toCondition (castCCharToChar (cGetCondition sp)))

get :: State -> Point -> Object
get s (x, y) =
  unwrapState s $ \sp ->
    return (toObject (castCCharToChar (cGet sp (toEnum x) (toEnum y))))

is :: (Object -> Bool) -> State -> Point -> Bool
is check s pt = check (get s pt)

isRobot :: State -> Point -> Bool
isRobot = is (== ORobot)

isWall :: State -> Point -> Bool
isWall = is (== OWall)

isRock :: State -> Point -> Bool
isRock = is (== ORock)

isHoRock :: State -> Point -> Bool
isHoRock = is ( == OHoRock)

isLambda :: State -> Point -> Bool
isLambda = is (== OLambda)

isLift :: State -> Point -> Bool
isLift = is check
  where
    check (OLift _) = True
    check _ = False

isEarth :: State -> Point -> Bool
isEarth = is (== OEarth)

isEmpty :: State -> Point -> Bool
isEmpty = is (== OEmpty)

isBeard :: State -> Point -> Bool
isBeard = is (== OBeard)

isRazor :: State -> Point -> Bool
isRazor = is (== ORazor)

isTrampoline :: State -> Point -> Bool
isTrampoline = is check
  where
    check (OTrampoline _) = True
    check _ = False

isTarget :: State -> Point -> Bool
isTarget = is check
  where
    check (OTarget _) = True
    check _ = False

makeOneMove_ :: State -> Char -> State
makeOneMove_ s0 move =
  unwrapState s0 $ \sp0 -> do
    sp <- cMakeOneMove sp0 (castCharToCChar move)
    wrapState sp

makeOneMove :: State -> Move -> State
makeOneMove s0 move = makeOneMove_ s0 (fromMove move)

makeMoves_ :: State -> String -> State
makeMoves_ s0 moves =
  unwrapState s0 $ \sp0 -> do
    withCString moves $ \ms -> do
      sp <- cMakeMoves sp0 ms
      wrapState sp

makeMoves :: State -> [Move] -> State
makeMoves s0 moves = makeMoves_ s0 (map fromMove moves)

updateWorldIgnoringRobot :: State -> State
updateWorldIgnoringRobot s0 =
  unwrapState s0 $ \sp0 -> do
    sp <- cUpdateWorldIgnoringRobot sp0
    wrapState sp

imagineRobotAt :: State -> Point -> State
imagineRobotAt s0 (x, y) =
  unwrapState s0 $ \sp0 -> do
    sp <- cImagineRobotAt sp0 (toEnum x) (toEnum y)
    wrapState sp

getStep :: State -> Move -> Point
getStep s move = getRobotPoint (makeOneMove s move)

imagineStep :: State -> Point -> Move -> Point
imagineStep s from move = getStep (imagineRobotAt s from) move

isEnterable :: State -> Point -> Bool
isEnterable s (x, y) =
  unwrapState s $ \sp ->
    return (toBool (cIsEnterable sp (toEnum x) (toEnum y)))

isSafe :: State -> Point -> Bool
isSafe s (x, y) =
  unwrapState s $ \sp ->
    return (toBool (cIsSafe sp (toEnum x) (toEnum y)))


data CCostTable
type CCostTablePtr = Ptr CCostTable
type CCostTableFPtr = ForeignPtr CCostTable
data CostTable = CostTable !(CCostTableFPtr)

type Cost = Int


foreign import ccall unsafe "libvm.h build_cost_table"
  cBuildCostTable :: CStatePtr -> CLong -> CLong -> IO CCostTablePtr

foreign import ccall unsafe "libvm.h safe_get_cost"
  cGetCost :: CCostTablePtr -> CLong -> CLong -> CLong
foreign import ccall unsafe "libvm.h safe_get_dist"
  cGetDist :: CCostTablePtr -> CLong -> CLong -> CLong


buildCostTable :: State -> Point -> CostTable
buildCostTable s (x, y) =
  unwrapState s $ \sp -> do
    ctp <- cBuildCostTable sp (toEnum x) (toEnum y)
    ctfp <- newForeignPtr finalizerFree ctp
    return (CostTable ctfp)

getCost :: CostTable -> Point -> Cost
getCost (CostTable ctfp) (x, y) =
  unsafePerformIO $
    withForeignPtr ctfp $ \ctp ->
      return (fromEnum (cGetCost ctp (toEnum x) (toEnum y)))

getDist :: CostTable -> Point -> Cost
getDist (CostTable ctfp) (x, y) =
  unsafePerformIO $
    withForeignPtr ctfp $ \ctp ->
      return (fromEnum (cGetDist ctp (toEnum x) (toEnum y)))


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
    if isRock s p || isHoRock s p then [p] else []

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
canMove s p =any (isEnterable s . flip evalMoves p) moves where
    moves = [MDown,MLeft,MRight,MUp]

-- willDeadLock :: State -> Move -> Point -> Bool
-- willDeadLock s m = canMove s' $ getRobotPoint s'
--   s' = imagineStep s p
