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
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)


data CState
type CStatePtr = Ptr CState
type CStateFPtr = ForeignPtr CState
data State = State !(CStateFPtr) deriving (Eq, Ord, Show)

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
    return (fromEnum (cGetMoveCount sp))

getIntPair :: (CStatePtr -> Ptr CLong -> Ptr CLong -> IO ()) -> State -> (Int, Int)
getIntPair action s =
  unwrapState s $ \sp ->
    alloca $ \ap ->
      alloca $ \bp -> do
        action sp ap bp
        a <- peek ap
        b <- peek bp
        return (fromEnum a, fromEnum b)


data Object = ORobot | OWall | ORock | OLambda | OClosedLift | OOpenLift | OEarth | OEmpty deriving (Eq, Ord, Show)

toObject :: Char -> Object
toObject 'R'  = ORobot
toObject '#'  = OWall
toObject '*'  = ORock
toObject '\\' = OLambda
toObject 'L'  = OClosedLift
toObject 'O'  = OOpenLift
toObject '.'  = OEarth
toObject ' '  = OEmpty


data Move = MLeft | MRight | MUp | MDown | MWait | MAbort deriving (Eq, Ord, Show)

fromMove :: Move -> Char
fromMove MLeft  = 'L'
fromMove MRight = 'R'
fromMove MUp    = 'U'
fromMove MDown  = 'D'
fromMove MWait  = 'W'
fromMove MAbort = 'A'


data Condition = CNone | CWin | CLose | CAbort deriving (Enum, Eq, Ord, Show)


type Size = (Int, Int)
type Point = (Int, Int)


foreign import ccall unsafe "libvm.h new"
  cNew :: CLong -> Ptr CChar -> IO CStatePtr

foreign import ccall unsafe "libvm.h new_from_file"
  cNewFromFile :: CString -> IO CStatePtr

foreign import ccall unsafe "libvm.h dump"
  cDump :: CStatePtr -> IO ()

foreign import ccall unsafe "libvm.h get_world_size"
  cGetWorldSize :: CStatePtr -> Ptr CLong -> Ptr CLong -> IO ()

foreign import ccall unsafe "libvm.h get_robot_point"
  cGetRobotPoint :: CStatePtr -> Ptr CLong -> Ptr CLong -> IO ()

foreign import ccall unsafe "libvm.h get_lift_point"
  cGetLiftPoint :: CStatePtr -> Ptr CLong -> Ptr CLong -> IO ()

foreign import ccall unsafe "libvm.h get_lambda_count"
  cGetLambdaCount :: CStatePtr -> CLong

foreign import ccall unsafe "libvm.h get_move_count"
  cGetMoveCount :: CStatePtr -> CLong

foreign import ccall unsafe "libvm.h get_condition"
  cGetCondition :: CStatePtr -> CChar

foreign import ccall unsafe "libvm.h lookup_point"
  cLookupPoint :: CStatePtr -> CLong -> CLong -> CChar

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

getLambdaCount :: State -> Int
getLambdaCount = getInt cGetLambdaCount

getMoveCount :: State -> Int
getMoveCount = getInt cGetMoveCount

getCondition :: State -> Condition
getCondition s =
  unwrapState s $ \sp ->
    return (toEnum (fromEnum (castCCharToChar (cGetCondition sp))))

lookupPoint :: State -> Point -> Object
lookupPoint s (x, y) =
  unwrapState s $ \sp ->
    return (toObject (castCCharToChar (cLookupPoint sp (toEnum x) (toEnum y))))

makeOneMove :: State -> Move -> State
makeOneMove s0 move =
  unwrapState s0 $ \sp0 -> do
    sp <- cMakeOneMove sp0 (castCharToCChar (fromMove move))
    wrapState sp

makeMoves :: State -> [Move] -> State
makeMoves s0 moves =
  unwrapState s0 $ \sp0 -> do
    withCString (map fromMove moves) $ \ms -> do
      sp <- cMakeMoves sp0 ms
      wrapState sp
