{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module VM where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.C.String (CString, castCharToCChar, castCCharToChar, withCString)
import Foreign.C.Types (CChar (..), CLong (..))
import Foreign.Marshal.Alloc (alloca, finalizerFree)
import Foreign.Marshal.Utils (toBool)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)


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


data Object = ORobot | OWall | ORock | OLambda | OClosedLift | OOpenLift | OEarth | OEmpty deriving (Eq, Ord)

instance Show Object where
  show object = [fromObject object]

fromObject :: Object -> Char
fromObject ORobot      = 'R'
fromObject OWall       = '#'
fromObject ORock       = '*'
fromObject OLambda     = '\\'
fromObject OClosedLift = 'L'
fromObject OOpenLift   = 'O'
fromObject OEarth      = '.'
fromObject OEmpty      = ' '

toObject :: Char -> Object
toObject 'R'  = ORobot
toObject '#'  = OWall
toObject '*'  = ORock
toObject '\\' = OLambda
toObject 'L'  = OClosedLift
toObject 'O'  = OOpenLift
toObject '.'  = OEarth
toObject ' '  = OEmpty
toObject _    = undefined


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

foreign import ccall unsafe "libvm.h get"
  cGet :: CStatePtr -> CLong -> CLong -> CChar

foreign import ccall unsafe "libvm.h make_one_move"
  cMakeOneMove :: CStatePtr -> CChar -> IO CStatePtr

foreign import ccall unsafe "libvm.h make_moves"
  cMakeMoves :: CStatePtr -> CString -> IO CStatePtr


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

getLambdaCount :: State -> Int
getLambdaCount = getInt cGetLambdaCount

getCollectedLambdaCount :: State -> Int
getCollectedLambdaCount = getInt cGetCollectedLambdaCount

getTrampolineCount :: State -> Int
getTrampolineCount = getInt cGetTrampolineCount

getTrampolinePoint :: State -> Char -> Maybe Point
getTrampolinePoint s trampoline =
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

getTargetPoint :: State -> Char -> Maybe Point
getTargetPoint s target =
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

getTrampolineTarget :: State -> Char -> Maybe Char
getTrampolineTarget s trampoline =
  unwrapState s $ \sp ->
    alloca $ \tp -> do
      let ok = toBool (cGetTrampolineTarget sp (castCharToCChar trampoline) tp)
      if ok
        then do
          target <- peek tp
          return (Just (castCCharToChar target))
        else return Nothing

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

makeOneMove :: State -> Char -> State
makeOneMove s0 move =
  unwrapState s0 $ \sp0 -> do
    sp <- cMakeOneMove sp0 (castCharToCChar move)
    wrapState sp

makeOneMove' :: State -> Move -> State
makeOneMove' s0 move = makeOneMove s0 (fromMove move)

makeMoves :: State -> String -> State
makeMoves s0 moves =
  unwrapState s0 $ \sp0 -> do
    withCString moves $ \ms -> do
      sp <- cMakeMoves sp0 ms
      wrapState sp

makeMoves' :: State -> [Move] -> State
makeMoves' s0 moves = makeMoves s0 (map fromMove moves)
