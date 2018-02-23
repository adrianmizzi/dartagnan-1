module DeviceManagement (deviceInfo) where

import Declarations as My

-- update the device locations for correct communication
deviceInfo :: Stream1 -> Stream1
deviceInfo (RootNode l devLoc s) = deviceInfo' 0 s'
  where
    s' = manageDevices devLoc (RootNode l devLoc s)
deviceInfo s = deviceInfo r
  where
    Stream r = (root 1 (Stream s))

deviceInfo' :: DeviceLocation -> Stream1 -> Stream1
deviceInfo' _ (Input l devLoc inpLoc) = Input l devLoc inpLoc
deviceInfo' _ (Const l v)             = Const l v
deviceInfo' devLoc (Dup l s)        = Dup l (deviceInfo' devLoc s)
deviceInfo' devLoc (DeviceNode (Sender l1 _) (Receiver l2 loc2) newPNum commType s) = DeviceNode (Sender l1 devLoc) (Receiver l2 loc2) newPNum commType (deviceInfo' loc2 s)
deviceInfo' _ (RootNode l devLoc s) = RootNode l devLoc (deviceInfo' devLoc s)
deviceInfo' devLoc (Sqr l s)        = Sqr l (deviceInfo' devLoc s)
deviceInfo' devLoc (Odd l s)        = Odd l (deviceInfo' devLoc s)
deviceInfo' devLoc (Neg l s)        = Neg l (deviceInfo' devLoc s)
deviceInfo' devLoc (Not l s)        = Not l (deviceInfo' devLoc s)
deviceInfo' devLoc (Add l s1 s2)    = Add l (deviceInfo' devLoc s1) (deviceInfo' devLoc s2)
deviceInfo' devLoc (Mul l s1 s2)    = Mul l (deviceInfo' devLoc s1) (deviceInfo' devLoc s2)
deviceInfo' devLoc (Div l s1 s2)    = Div l (deviceInfo' devLoc s1) (deviceInfo' devLoc s2)
deviceInfo' devLoc (And l s1 s2)    = And l (deviceInfo' devLoc s1) (deviceInfo' devLoc s2)
deviceInfo' devLoc (My.GT l s1 s2)  = My.GT l (deviceInfo' devLoc s1) (deviceInfo' devLoc s2)
deviceInfo' devLoc (Eq l s1 s2)     = Eq l (deviceInfo' devLoc s1) (deviceInfo' devLoc s2)
deviceInfo' devLoc (Or l s1 s2)     = Or l (deviceInfo' devLoc s1) (deviceInfo' devLoc s2)
deviceInfo' devLoc (IfThenElse l s1 (s2, s3)) = IfThenElse l (deviceInfo' devLoc s1) ((deviceInfo' devLoc s2), (deviceInfo' devLoc s3))
deviceInfo' devLoc (PreI l1 l2 memLoc x s) = PreI l1 l2 memLoc x (deviceInfo' devLoc s)
deviceInfo' devLoc (PreB l1 l2 memLoc x s) = PreB l1 l2 memLoc x (deviceInfo' devLoc s)
deviceInfo' devLoc (Bundle l ss) = Bundle l (map (deviceInfo' devLoc) ss)

-- Introduces device nodes if communication between different nodes is required and is not present
manageDevices :: DeviceLocation -> Stream1 -> Stream1
manageDevices dev (Input l devLoc inpLoc) 
                           | dev == devLoc = Input l devLoc inpLoc
                           | otherwise     = p
             where
                Stream p = pull (device devLoc) (Stream (Input l devLoc inpLoc))
manageDevices dev (Const l x) = Const l x
manageDevices dev (Dup l s) = Dup l (manageDevices dev s)
manageDevices dev (DeviceNode sender receiver newPNum commType s) = DeviceNode sender receiver newPNum commType s
manageDevices dev (RootNode l devLoc s) = RootNode l devLoc (manageDevices dev s)
manageDevices dev (Sqr l s) = Sqr l (manageDevices dev s)
manageDevices dev (Odd l s) = Odd l (manageDevices dev s)
manageDevices dev (Neg l s) = Neg l (manageDevices dev s)
manageDevices dev (Not l s) = Not l (manageDevices dev s)
manageDevices dev (Add l s1 s2) = Add l  (manageDevices dev s1)  (manageDevices dev s2)
manageDevices dev (Mul l s1 s2) = Mul l (manageDevices dev s1) (manageDevices dev s2)
manageDevices dev (Div l s1 s2) = Div l (manageDevices dev s1) (manageDevices dev s2)
manageDevices dev (And l s1 s2) = And l (manageDevices dev s1) (manageDevices dev s2)
manageDevices dev (My.GT l s1 s2) = My.GT l (manageDevices dev s1) (manageDevices dev s2)
manageDevices dev (Eq l s1 s2) = Eq l (manageDevices dev s1) (manageDevices dev s2)
manageDevices dev (Or l s1 s2) = Or l (manageDevices dev s1) (manageDevices dev s2)
manageDevices dev (IfThenElse l s1 (s2, s3)) = IfThenElse l (manageDevices dev s1) (manageDevices dev s2,manageDevices dev s3)
manageDevices dev (PreI l1 l2 memLoc x s) = PreI l1 l2 memLoc x (manageDevices dev s)
manageDevices dev (PreB l1 l2 memLoc x s) = PreB l1 l2 memLoc x (manageDevices dev s)
manageDevices dev (Bundle l ss) = Bundle l ss'
  where
    ss' = map (manageDevices dev) ss

-- Determines whether the current device location can execute the next piece of the Stream Processor
supportsDevice :: Device -> Stream1 -> Bool
supportsDevice (Device x) (Input l devLoc inpLoc) = (x == devLoc)
supportsDevice _ _ = True

-- In case of "Input" use the device location of the input, otherwise use 0 which will later be updated
getDevice :: Stream1 -> Device
getDevice (Input l devLoc inpLoc) = device devLoc
getDevice _ = device 0


