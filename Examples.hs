module Examples where

import Declarations as My
import Prelude

-- this section contains a list of valid stream processors

t1 = add (input1, constant 5)
t2 = add  (constant 1, constant 5)
t3 = add (input1, constant 5)
t4 = sqr (add (input1, input2))
t5 = My.not (My.odd (add (input1, constant 5)))
t6 = ifThenElse (My.odd (input1), (input1, input2))
t7 = 
  let a = add (input1, pre 0 a)
  in a
t8 = 
  let a = sqr (pre 0 a)
  in a
t9 = root 1 (add (sqr input1, pull (device 2) (add (sqr input2, pull (device 1) input1))))
t10 = root 1 (add (sqr input1, add (sqr (pull (device 2) input2), input1)))
t11 = root 1 (add (sqr input1, add (sqr (pull (device 2) input2), pull (device 3) input3)))
t12 = root 1 (add (sqr (pre 4 input1), add (sqr (pull (device 2) input2), pull (device 3) input3)))
t13 = 
  let a = add (input1, pre 0 (add (input2, a)))
  in a
t14 = root 1 (add (sqr (pre 4 (input1)), add (sqr (input2), input3)))
t15 = ifThenElse (input1 .==. input2, (input1,input2))

tmp = root 1 (add (input1, pull (device 2) input2))


h = let x=pre 0 (x :: Stream Int)
    in x

-- should fail type checking (typecheck test)
-- j = add (odd input1) (constant 1)

k = let x = sqr (input1)
    in add (x, x)

-- should fail as short-circuit
l = let x = add (input1, x)
    in add (x, x)

l1 = let x = add (input1, pre 2 x)
     in x

l2 = add (input1, pre 2 (constant 1))

m = let x = add (input1, constant 1)
    in add (x, x)

n = let x = add (sqr (pull (device 2) (inputI (device 2) (sensor 1))), sqr (pull (device 2) (inputI (device 2) (sensor 1))))
    in pull (device 1) (add (pre 2 x, pre 2 x))

o = let x = add (y, y)
        y = input1
    in add (x, x)

p = pre 5 (add (input1, input2))

input1 = inputI (device 1) (sensor 124)
input2 = inputI (device 2) (sensor 127)
input3 = inputI (device 3) (sensor 198)
input4 = inputI (device 4) (sensor 158)
input5 = inputI (device 5) (sensor 1)


