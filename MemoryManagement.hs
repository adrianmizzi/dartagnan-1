module MemoryManagement (memInfo) where

import Declarations as My


-- updates the memory locations by giving a new number to each memory location
memInfo :: Stream1 -> Stream1
memInfo s = s'
  where
    (_, s') = memInfo' (0, s)

memInfo' :: (Integer, Stream1) -> (Integer, Stream1)
memInfo' (n, Input label deviceLoc inputLoc) = (n, Input label deviceLoc inputLoc)
memInfo' (n, Const label v) = (n, Const label v)
memInfo' (n, Dup label s) = (n, Dup label s)
memInfo' (n, DeviceNode (Sender l1 loc1) (Receiver l2 loc2) newPNum commType s) = (n', DeviceNode (Sender l1 loc1) (Receiver l2 loc2) newPNum commType s')
  where (n', s') = memInfo' (n, s)
memInfo' (n, RootNode l devLoc s) = (n', RootNode l devLoc s')
  where (n', s') = memInfo' (n, s)
memInfo' (n, Sqr label  s) = (n', Sqr label s')
  where (n', s') = memInfo' (n, s)
memInfo' (n, Odd label  s) = (n', Odd label s')
  where (n', s') = memInfo' (n, s)
memInfo' (n, Neg label  s) = (n', Neg label s')
  where (n', s') = memInfo' (n, s)
memInfo' (n, Not label  s) = (n', Not label s')
  where (n', s') = memInfo' (n, s)
memInfo' (n, Add label  s1 s2) = (n'', Add label s1' s2')
  where 
    (n', s1') = memInfo' (n, s1)
    (n'', s2') = memInfo' (n', s2)
memInfo' (n, Mul label  s1 s2) = (n'', Mul label s1' s2')
  where 
    (n', s1') = memInfo' (n, s1)
    (n'', s2') = memInfo' (n', s2)
memInfo' (n, Div label  s1 s2) = (n'', Div label s1' s2')
  where 
    (n', s1') = memInfo' (n, s1)
    (n'', s2') = memInfo' (n', s2)
memInfo' (n, My.GT label  s1 s2) = (n'', My.GT label s1' s2')
  where 
    (n', s1') = memInfo' (n, s1)
    (n'', s2') = memInfo' (n', s2)
memInfo' (n, Eq label  s1 s2) = (n'', Eq label s1' s2')
  where 
    (n', s1') = memInfo' (n, s1)
    (n'', s2') = memInfo' (n', s2)
memInfo' (n, And label  s1 s2) = (n'', And label s1' s2')
  where 
    (n', s1') = memInfo' (n, s1)
    (n'', s2') = memInfo' (n', s2)
memInfo' (n, Or label  s1 s2) = (n'', Or label s1' s2')
  where 
    (n', s1') = memInfo' (n, s1)
    (n'', s2') = memInfo' (n', s2)
memInfo' (n, IfThenElse label  s1 (s2,s3)) = (n''', IfThenElse label s1' (s2', s3'))
  where 
    (n', s1') = memInfo' (n, s1)
    (n'', s2') = memInfo' (n', s2)
    (n''', s3') = memInfo' (n'', s3)
memInfo' (n, PreI l1 l2 memLoc init s) = (n'+1, PreI l1 l2 n' init s')
  where 
    (n', s') = memInfo' (n, s)
memInfo' (n, PreB l1 l2 memLoc init s) = (n'+1, PreB l1 l2 n' init s')
  where 
    (n', s') = memInfo' (n, s)
memInfo' (n, Bundle l ss) = (n', Bundle l ss')
  where
    (n', ss') = memInfoBundle (n, ss)

memInfoBundle :: (Integer, [Stream1]) -> (Integer, [Stream1])
memInfoBundle (n, []) = (n, [])
memInfoBundle (n, s:ss) = (n'', s':ss') 
  where
    (n', s') = memInfo' (n, s)
    (n'', ss') = memInfoBundle (n', ss)

