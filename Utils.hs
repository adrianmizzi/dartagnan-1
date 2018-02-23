{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Utils where

import           Declarations
import           Val


instance Num (Stream Int) where
  s1 + s2 = add (s1, s2)
  s1 - s2 = sub (s1, s2)
  s1 * s2 = mul (s1, s2)
  abs s   = ifThenElse (s .<. 0, (neg s, s))
  fromInteger n = constant (fromIntegral n)
  signum s = ifThenElse (s .<. 0, (ifThenElse (s .==. 0, (-1, 0)), 1))


-- sorting section
ordered :: (Eq a, Num a) => ([a], [a]) -> [(a, a)] -> [a]
ordered (sortedList, []) _ = sortedList
ordered (toList, fromList) deps = ordered (newList, w) deps
  where
    v = [v | v <- fromList, v `freeIn` (deps `exclude` toList)]
    newList = toList ++ v
    w = fromList `less` newList

freeIn :: (Eq a, Num a) => a -> [(a, a)] -> Bool
freeIn x []         = True
freeIn x ((a,b):ss) = x /= a && freeIn x ss

exclude :: (Eq a, Num a) => [(a, a)] -> [a] -> [(a, a)]
exclude [] xs = []
exclude ((a,b):ss) xs = if b `elem` xs
                         then exclude ss xs
                         else (a,b) : exclude ss xs

less :: (Eq a, Num a) => [a] -> [a] -> [a]
less xs ns = [v | v <- xs, v `notElem` ns]
