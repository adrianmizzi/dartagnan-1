module Duplicates (duplicates) where

import Declarations
import Control.Monad.State

type LabelMap = [Label]

-- this is the main method visible externally
duplicates :: Stream1 -> Stream1
duplicates s = evalState (dups s) []

-- dups uses the State Monad.  If a label is encountered a second time, a new node "Dup" is created and points to it
dups :: Stream1 -> State LabelMap Stream1
dups (Input l devLoc inpLoc) = do
  labelMap <- get
  if (elem l labelMap)
    then 
      return (Dup (new_label()) (Input l devLoc inpLoc))
    else do
      put (l:labelMap)
      return (Input l devLoc inpLoc)

dups (Const l x) = do
  labelMap <- get
  if (elem l labelMap)
    then 
      return (Dup (new_label()) (Const l x))
    else do
      put (l:labelMap)
      return (Const l x)

dups (Dup l s) = return (Dup l s)

dups (DeviceNode (Sender l1 loc1) (Receiver l2 loc2) newPNum commType s) = do
  labelMap <- get
  if (elem l1 labelMap)
    then 
      return (Dup (new_label()) (DeviceNode (Sender l1 loc1) (Receiver l2 loc2) newPNum commType s))
    else do
      put (l1:labelMap)
      s' <- dups s
      return (DeviceNode (Sender l1 loc1) (Receiver l2 loc2) newPNum commType s')

dups (RootNode l devLoc s) = do
  labelMap <- get
  if (elem l labelMap)
    then 
      return (Dup (new_label()) (RootNode l devLoc s))
    else do
      put (l:labelMap)
      s' <- dups s
      return (RootNode l devLoc s')

dups (Sqr l s) = do
  labelMap <- get
  if (elem l labelMap)
    then 
      return (Dup (new_label()) (Sqr l s))
    else do
      put (l:labelMap)
      s' <- dups s
      return (Sqr l s')


dups (Odd l s) = do
  labelMap <- get
  if (elem l labelMap)
    then 
      return (Dup (new_label()) (Odd l s))
    else do
      put (l:labelMap)
      s' <- dups s
      return (Odd l s')

dups (Neg l s) = do
  labelMap <- get
  if (elem l labelMap)
    then 
      return (Dup (new_label()) (Neg l s))
    else do
      put (l:labelMap)
      s' <- dups s
      return (Neg l s')

dups (Not l s) = do
  labelMap <- get
  if (elem l labelMap)
    then 
      return (Dup (new_label()) (Not l s))
    else do
      put (l:labelMap)
      s' <- dups s
      return (Not l s')

dups (Add l s1 s2) = do
  labelMap <- get
  if (elem l labelMap)
    then 
      return (Dup (new_label()) (Add l s1 s2))
    else do
      put (l:labelMap)
      s1' <- dups s1
      s2' <- dups s2
      return (Add l s1' s2')

dups (Mul l s1 s2) = do
  labelMap <- get
  if (elem l labelMap)
    then 
      return (Dup (new_label()) (Mul l s1 s2))
    else do
      put (l:labelMap)
      s1' <- dups s1
      s2' <- dups s2
      return (Mul l s1' s2')

dups (Div l s1 s2) = do
  labelMap <- get
  if (elem l labelMap)
    then 
      return (Dup (new_label()) (Div l s1 s2))
    else do
      put (l:labelMap)
      s1' <- dups s1
      s2' <- dups s2
      return (Div l s1' s2')

dups (And l s1 s2) = do
  labelMap <- get
  if (elem l labelMap)
    then 
      return (Dup (new_label()) (And l s1 s2))
    else do
      put (l:labelMap)
      s1' <- dups s1
      s2' <- dups s2
      return (And l s1' s2')

dups (Declarations.GT l s1 s2) = do
  labelMap <- get
  if (elem l labelMap)
    then 
      return (Dup (new_label()) (Declarations.GT l s1 s2))
    else do
      put (l:labelMap)
      s1' <- dups s1
      s2' <- dups s2
      return (Declarations.GT l s1' s2')

dups (Eq l s1 s2) = do
  labelMap <- get
  if (elem l labelMap)
    then 
      return (Dup (new_label()) (Eq l s1 s2))
    else do
      put (l:labelMap)
      s1' <- dups s1
      s2' <- dups s2
      return (Eq l s1' s2')

dups (Or l s1 s2) = do
  labelMap <- get
  if (elem l labelMap)
    then 
      return (Dup (new_label()) (Or l s1 s2))
    else do
      put (l:labelMap)
      s1' <- dups s1
      s2' <- dups s2
      return (Or l s1' s2')

dups (IfThenElse l s1 (s2,s3)) = do
  labelMap <- get
  if (elem l labelMap)
    then 
      return (Dup (new_label()) (IfThenElse l s1 (s2,s3)))
    else do
      put (l:labelMap)
      s1' <- dups s1
      s2' <- dups s2
      s3' <- dups s3
      return (IfThenElse l s1' (s2',s3'))

dups (PreI l1 l2 memLoc x s) = do
  labelMap <- get
  if (elem l1 labelMap)
    then 
      return (Dup (new_label()) (PreI l1 l2 memLoc x s))
    else do
      put (l1:labelMap)
      s' <- dups s
      return (PreI l1 l2 memLoc x s')

dups (PreB l1 l2 memLoc x s) = do
  labelMap <- get
  if (elem l1 labelMap)
    then 
      return (Dup (new_label()) (PreB l1 l2 memLoc x s))
    else do
      put (l1:labelMap)
      s' <- dups s
      return (PreB l1 l2 memLoc x s')

dups (Bundle l ss) = do
  labelMap <- get
  if (elem l labelMap)
    then
      return (Dup (new_label()) (Bundle l ss))
    else do
      put (l:labelMap)
      ss' <- dupsList ss
--      ss' <- evalState (dupsList ss) labelMap
      return (Bundle l ss')

dupsList :: [Stream1] -> State LabelMap [Stream1]
dupsList [] = return []
dupsList (s:ss) = do
  s' <- dups s
  ss' <- dupsList ss
  return (s':ss')


