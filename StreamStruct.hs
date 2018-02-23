{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module StreamStruct where

import           Declarations
import           Val

-- Structure used to store Stream Tuples ---

data StreamStructure a =
    Object a
  | Tuple  [StreamStructure a]
  | List [StreamStructure a]
  deriving Eq

instance Show a => Show (StreamStructure a) where
  show (Object x) = show x
  show (Tuple xs) = "(" ++ foldl1 (\x y -> x ++ "," ++ y) (map show xs) ++ ")"

fromTuple (Tuple s) = s
fromObject (Object o) = o
fromList (List l) = l
toObject = Object
toTuple = Tuple
toList = List

test1 :: StreamStructure Stream1
test1 = Tuple [Object (Const 0 (I 5)),Object (Const 0 (I 4))]

test1' :: ((Stream1, Stream1), Stream1)
test1' = ((Const 0 (I 5), Const 0 (I 3)), Const 0 (I 2))


-- StreamTuple is used as input and output to a function
-- the function to be simulated must have the type (StreamTuple -> StreamTuple)
class StreamTuple a where
  fromStreamStructure :: StreamStructure Stream1 -> a
  toStreamStructure :: a -> StreamStructure Stream1
  applyToTuple :: (Stream1 -> [Val]) -> a -> [[Val]]

instance StreamTuple Stream1 where
  fromStreamStructure = fromObject
  toStreamStructure = toObject
  applyToTuple f x1 = [f x1]

instance StreamTuple (Stream a) where
  fromStreamStructure = wrapStream . fromObject
  toStreamStructure = toObject . unwrapStream
  applyToTuple f x = applyToTuple f $ unwrapStream x

--instance (StreamTuple a) => StreamTuple [a] where
--  fromStreamStructure (List xs) = map fromStreamStructure xs
--  toStreamStructure = List . (map toStreamStructure)

instance (StreamTuple a1, StreamTuple a2) => StreamTuple (a1, a2) where
  fromStreamStructure (Tuple [x1,x2]) = (fromStreamStructure x1, fromStreamStructure x2)
  toStreamStructure (x1, x2) = Tuple [toStreamStructure x1, toStreamStructure x2]
  applyToTuple f (x1, x2) = applyToTuple f x1 ++ applyToTuple f x2

instance (StreamTuple a1, StreamTuple a2, StreamTuple a3) => StreamTuple (a1, a2, a3) where
  fromStreamStructure (Tuple [x1,x2,x3]) = (fromStreamStructure x1, fromStreamStructure x2, fromStreamStructure x3)
  toStreamStructure (x1, x2, x3) = Tuple [toStreamStructure x1, toStreamStructure x2, toStreamStructure x3]
  applyToTuple f (x1, x2, x3) = applyToTuple f x1 ++ applyToTuple f x2 ++ applyToTuple f x3

instance (StreamTuple a1, StreamTuple a2, StreamTuple a3, StreamTuple a4) => StreamTuple (a1, a2, a3, a4) where
  fromStreamStructure (Tuple [x1,x2,x3,x4]) = (fromStreamStructure x1, fromStreamStructure x2, fromStreamStructure x3, fromStreamStructure x4)
  toStreamStructure (x1, x2, x3, x4) = Tuple [toStreamStructure x1, toStreamStructure x2, toStreamStructure x3, toStreamStructure x4]
  applyToTuple f (x1, x2, x3, x4) = applyToTuple f x1 ++ applyToTuple f x2 ++ applyToTuple f x3 ++ applyToTuple f x4


-- ValTuple are the input values - converted into Constant Streams
class ValTuple a where
  toStructure :: [a] -> StreamStructure Stream1


instance ValTuple Val where
  toStructure vs = Object (ConstStream 0 vs (getStreamType vs))

instance (ValTuple a1, ValTuple a2) => ValTuple (a1, a2) where
  toStructure vs = Tuple [toStructure x1, toStructure x2]
    where
      (x1, x2) = untwine2 vs

instance (ValTuple a1, ValTuple a2, ValTuple a3) => ValTuple (a1, a2, a3) where
  toStructure vs = Tuple [toStructure x1, toStructure x2, toStructure x3]
    where
      (x1, x2, x3) = untwine3 vs

instance (ValTuple a1, ValTuple a2, ValTuple a3, ValTuple a4) => ValTuple (a1, a2, a3, a4) where
  toStructure vs = Tuple [toStructure x1, toStructure x2, toStructure x3, toStructure x4]
    where
      (x1, x2, x3, x4) = untwine4 vs

entwine2 :: (ValTuple a1, ValTuple a2) => ([a1], [a2]) -> [(a1, a2)]
entwine2 ([x1], [x2])     = [(x1,x2)]
entwine2 (x1:xx1, x2:xx2) = (x1, x2):entwine2(xx1,xx2)

untwine2 :: (ValTuple a1, ValTuple a2) => [(a1, a2)] -> ([a1], [a2])
untwine2 [(x1, x2)] = ([x1], [x2])
untwine2 ((x1, x2):vs) = (x1:xx1, x2:xx2)
  where
    (xx1, xx2) = untwine2 vs

untwine3 :: (ValTuple a1, ValTuple a2, ValTuple a3) => [(a1, a2, a3)] -> ([a1], [a2], [a3])
untwine3 [(x1, x2, x3)] = ([x1], [x2], [x3])
untwine3 ((x1, x2, x3):vs) = (x1:xx1, x2:xx2, x3:xx3)
  where
    (xx1, xx2, xx3) = untwine3 vs

untwine4 :: (ValTuple a1, ValTuple a2, ValTuple a3, ValTuple a4) => [(a1, a2, a3, a4)] -> ([a1], [a2], [a3], [a4])
untwine4 [(x1, x2, x3, x4)] = ([x1], [x2], [x3], [x4])
untwine4 ((x1, x2, x3, x4):vs) = (x1:xx1, x2:xx2, x3:xx3, x4:xx4)
  where
    (xx1, xx2, xx3, xx4) = untwine4 vs

-- looks at the first element of the list to determine what stream type should be formed.  Assumes that all inputs are of the same type.
getStreamType :: [Val] -> StreamType
getStreamType (I x:vs) = IType
getStreamType (B x:vs) = BType


-- retrieve an Integer from a Val
getInt :: Val -> Int
getInt (I x) = x
getInt _     = error "non integer value"

-- retrieve a Bool from a Val
getBool :: Val -> Bool
getBool (B b) = b
getBool _     = error "non bool value"

-- the square of an integer x * x
square :: Int -> Int
square x = x * x
