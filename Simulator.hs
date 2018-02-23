{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module Simulator (simulate) where

import           Declarations
import           StreamStruct
import           Utils        hiding (Valable)
import           Val

---- TMP SECTION -----

-- usage example: simulate negateS [I 1, I 2, I 3, I 4]
negateS :: Stream Int -> Stream Int
negateS = neg

--usage example: simulate compareS [(I 1,I 2),(I 3,I 4),(I 5,I 4)]
compareS :: (Stream Int, Stream Int) -> Stream Bool
compareS (x, y) = x .<. y

compareS' :: (Stream Int, Stream Int) -> (Stream Bool, Stream Int)
compareS' (x, y) = (x .<. y, x)

compareS'' :: (Stream Int, Stream Int) -> ((Stream Bool, Stream Bool), Stream Int)
compareS'' (x, y) = ((x .<. y, x .>. y), x)

compareS2 :: ((Stream Int, Stream Int), (Stream Int, Stream Int)) -> Stream Bool
compareS2 ((x1, y1), (x2, y2)) = (x1 .<. x2) .&&. (y1 .<. y2)

---- END OF TMP SECTION -----

simulate :: (StreamTuple a, StreamTuple b, ValTuple c) => (a -> b) -> [c] -> [[Val]]
simulate f inputs = applyToTuple simulateNode f'
  where
    f'      = f $ fromStreamStructure $ toStructure inputs

simulateNode :: Stream1 -> [Val]
simulateNode (ConstStream _ vals _) = vals
simulateNode (Declarations.GT _ s1 s2) = zipWithVal ((>) :: Int -> Int -> Bool) (simulateNode s1) (simulateNode s2)
simulateNode (Neg _ s) = map (I . negate . getInt) (simulateNode s)
simulateNode (Const _ v) = let x = v : x in x
simulateNode (Sqr _ s) = map (I . square . getInt) (simulateNode s)
simulateNode (Odd _ s) = map (B . Prelude.odd . getInt) (simulateNode s)
simulateNode (Not _ s) = mapWithVal Prelude.not (simulateNode s)
simulateNode (Add _ s1 s2) = zipWithVal ((+) :: Int -> Int -> Int) (simulateNode s1) (simulateNode s2)
simulateNode (Mul _ s1 s2) = zipWithVal ((*) :: Int -> Int -> Int) (simulateNode s1) (simulateNode s2)
simulateNode (Div _ s1 s2) = zipWithVal (Prelude.div :: Int -> Int -> Int) (simulateNode s1) (simulateNode s2)
simulateNode (Eq _ s1 s2) = zipWithVal ((==) :: Int -> Int -> Bool) (simulateNode s1) (simulateNode s2)
simulateNode (And _ s1 s2) = zipWithVal ((&&) :: Bool -> Bool -> Bool) (simulateNode s1) (simulateNode s2)
simulateNode (Or _ s1 s2)  = zipWithVal ((||) :: Bool -> Bool -> Bool) (simulateNode s1) (simulateNode s2)
simulateNode (IfThenElse _ s1 (s2, s3)) = ifThenElseU (simulateNode s1) (simulateNode s2) (simulateNode s3)
simulateNode (PreI _ _ memLoc n0 s) = I n0 : simulateNode s
simulateNode (PreB _ _ memLoc b0 s) = B b0 : simulateNode s
simulateNode (Dup _ s) = simulateNode s -- TODO : needs handling for loops
simulateNode (DeviceNode _ _ _ _ s) = simulateNode s
simulateNode (RootNode _ _ s) = simulateNode s
simulateNode x = error (show x)


ifThenElseU :: [Val] -> [Val] -> [Val] -> [Val]
ifThenElseU [] _ _ = []
ifThenElseU (b:bs) (n:ns) (x:xs)
  | getBool b = n : ifThenElseU bs ns xs
  | otherwise = x : ifThenElseU bs ns xs
