module TypeChecker where

import           Data.List
import           Declarations
import           Val


returnTypeOf :: Stream1 -> StreamType
returnTypeOf (Input _ _ _)= IType
returnTypeOf (Const _ (I n)) = IType
returnTypeOf (Const _ (B b)) = BType
returnTypeOf (Sqr _ _) = IType
returnTypeOf (Odd _ _) = BType
returnTypeOf (Add _ _ _) = IType
returnTypeOf (Mul _ _ _) = IType
returnTypeOf (Div _ _ _) = IType
returnTypeOf (Neg _ _) = IType
returnTypeOf (Declarations.GT _ _ _) = BType
returnTypeOf (Eq _ _ _) = BType
returnTypeOf (And _ _ _) = BType
returnTypeOf (Or _ _ _) = BType
returnTypeOf (Not _ _) = BType
returnTypeOf (IfThenElse _ _ (s2, s3)) = returnTypeOf s2 -- s2 and s3 should have same type
returnTypeOf (PreI _ _ _ _ _) = IType
returnTypeOf (PreB _ _ _ _ _) = BType
returnTypeOf (Dup _ s) = returnTypeOf s
returnTypeOf (DeviceNode _ _ _ _ s) = returnTypeOf s
returnTypeOf (RootNode _ _ s) = returnTypeOf s
returnTypeOf (Bundle _ ss) = listType
  where
    ssTypes = nub $ map returnTypeOf ss
    listType
       | length ssTypes > 1    = error "Bundle contains different stream types"
       | head ssTypes == IType = ListIType
       | head ssTypes == BType = ListBType
       | otherwise             = error ("Only List of Integer or Boolean supported. Found: List of " ++ show (head ssTypes))

typeCheck :: Stream1 -> Bool
typeCheck s = (typeCheck' s) && (deviceCheck s)

typeCheck' :: Stream1 -> Bool
typeCheck' (Input _ _ _) = True
typeCheck' (Const _ v) = True
typeCheck' (Sqr _ s) = typeCheck' s && (returnTypeOf s == IType)
typeCheck' (Odd _ s) = typeCheck' s && (returnTypeOf s == IType)
typeCheck' (Add _ s1 s2) = typeCheck' s1 && typeCheck' s2 && (returnTypeOf s1 == IType) && (returnTypeOf s2 == IType)
typeCheck' (Mul _ s1 s2) = typeCheck' s1 && typeCheck' s2 && (returnTypeOf s1 == IType) && (returnTypeOf s2 == IType)
typeCheck' (Div _ s1 s2) = typeCheck' s1 && typeCheck' s2 && (returnTypeOf s1 == IType) && (returnTypeOf s2 == IType)
typeCheck' (Neg _ s) = typeCheck' s && (returnTypeOf s == IType)
typeCheck' (Declarations.GT _ s1 s2) = typeCheck' s1 && typeCheck' s2 && (returnTypeOf s1 == IType) && (returnTypeOf s2 == IType)
typeCheck' (Eq _ s1 s2) = typeCheck' s1 && typeCheck' s2 && ((returnTypeOf s1 == IType) && (returnTypeOf s2 == IType) || (returnTypeOf s1 == BType) && (returnTypeOf s2 == BType))
typeCheck' (And _ s1 s2) = typeCheck' s1 && typeCheck' s2 && (returnTypeOf s1 == BType) && (returnTypeOf s2 == BType)
typeCheck' (Or _ s1 s2) = typeCheck' s1 && typeCheck' s2 && (returnTypeOf s1 == BType) && (returnTypeOf s2 == BType)
typeCheck' (Not _ s1) = typeCheck' s1 && (returnTypeOf s1 == BType)
typeCheck' (IfThenElse _ s1 (s2, s3)) = typeCheck' s1 && typeCheck' s2 && typeCheck' s3 && (returnTypeOf s1 == BType) && (returnTypeOf s2 == returnTypeOf s3)
typeCheck' (PreI _ _ memloc x s) = typeCheck' s && (returnTypeOf s == IType)
typeCheck' (PreB _ _ memloc x s) = typeCheck' s && (returnTypeOf s == BType)
typeCheck' (Dup _ s) = True
typeCheck' (DeviceNode _ _ _ _ s) = typeCheck' s
typeCheck' (RootNode _ _ s) = typeCheck' s
typeCheck' (Bundle label ss) = (returnTypeOf (Bundle label ss) == ListIType) || (returnTypeOf (Bundle label ss) == ListBType) && foldl (&&) True (map typeCheck' ss)

deviceCheck :: Stream1 -> Bool
deviceCheck = deviceCheck' 1

deviceCheck' :: DeviceLocation -> Stream1 -> Bool
deviceCheck' location (Input _ devLoc _) = (location == devLoc)
deviceCheck' location (Const _ v) = True
deviceCheck' location (Sqr _ s) = deviceCheck' location s
deviceCheck' location (Odd _ s) = deviceCheck' location s
deviceCheck' location (Add _ s1 s2) = deviceCheck' location s1 && deviceCheck' location s2
deviceCheck' location (Mul _ s1 s2) = deviceCheck' location s1 && deviceCheck' location s2
deviceCheck' location (Div _ s1 s2) = deviceCheck' location s1 && deviceCheck' location s2
deviceCheck' location (Neg _ s) = deviceCheck' location s
deviceCheck' location (Declarations.GT _ s1 s2) = deviceCheck' location s1 && deviceCheck' location s2
deviceCheck' location (Eq _ s1 s2) = deviceCheck' location s1 && deviceCheck' location s2
deviceCheck' location (And _ s1 s2) = deviceCheck' location s1 && deviceCheck' location s2
deviceCheck' location (Or _ s1 s2) = deviceCheck' location s1 && deviceCheck' location s2
deviceCheck' location (Not _ s1) = deviceCheck' location s1
deviceCheck' location (IfThenElse _ s1 (s2, s3)) = deviceCheck' location s1 && deviceCheck' location s2 && deviceCheck' location s3
deviceCheck' location (PreI _ _ memloc x s) = deviceCheck' location s
deviceCheck' location (PreB _ _ memloc x s) = deviceCheck' location s
deviceCheck' location (Dup _ s) = True
deviceCheck' location (DeviceNode (Sender _ loc1) (Receiver _ loc2) _ _ s) = (location == loc1) && (deviceCheck' loc2 s)
deviceCheck' _ (RootNode _ devLoc s) = deviceCheck' devLoc s
deviceCheck' location (Bundle _ ss) = foldl1 (&&) $ map (deviceCheck' location) ss
