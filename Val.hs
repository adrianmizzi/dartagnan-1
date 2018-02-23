{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Val (Val(I, B), mapWithVal, zipWithVal, applyToVal, applyTo2Val) where

data Val = I Int | B Bool -- | L ValList
  deriving (Ord, Eq, Show)

-- type ValList = [Val]

instance Num Val where
  I n1 + I n2 = I (n1+n2)
  negate (I n) = I (negate n)
  fromInteger n = I (fromIntegral n)
  I n1 * I n2 = I (n1 * n2)
  abs (I n) = I (abs n)
  signum (I n) = I (signum n)

class Valable a where
  toVal :: a -> Val
  fromVal :: Val -> a

instance Valable Bool where
  toVal = B
  fromVal (B b) = b

instance Valable Int where
  toVal = I
  fromVal (I i) = i

class ApplyToVal a b where
  mapWithVal :: (a -> b) -> [Val] -> [Val]
  zipWithVal :: (a -> a -> b) -> [Val] -> [Val] -> [Val]
  applyToVal :: (a -> b) -> Val -> Val
  applyTo2Val :: (a -> a -> b) -> Val -> Val -> Val

instance ApplyToVal Int Bool where
  mapWithVal f = map ((B . f) . fromVal)
  zipWithVal f xs ys = map B $ zipWith f (map fromVal xs) (map fromVal ys)
  applyToVal f x = toVal $ f $ fromVal x
  applyTo2Val f x y = toVal $ f (fromVal x) (fromVal y)

instance ApplyToVal Int Int where
  mapWithVal f = map ((I . f) . fromVal)
  zipWithVal f xs ys = map I $ zipWith f (map fromVal xs) (map fromVal ys)
  applyToVal f x = toVal $ f $ fromVal x
  applyTo2Val f x y = toVal $ f (fromVal x) (fromVal y)

instance ApplyToVal Bool Bool where
  mapWithVal f = map ((B . f) . fromVal)
  zipWithVal f xs ys = map B $ zipWith f (map fromVal xs) (map fromVal ys)
  applyToVal f x = toVal $ f $ fromVal x
  applyTo2Val f x y = toVal $ f (fromVal x) (fromVal y)

instance ApplyToVal Bool Int where
  mapWithVal f = map ((I . f) . fromVal)
  zipWithVal f xs ys = map I $ zipWith f (map fromVal xs) (map fromVal ys)
  applyToVal f x = toVal $ f $ fromVal x
  applyTo2Val f x y = toVal $ f (fromVal x) (fromVal y)
