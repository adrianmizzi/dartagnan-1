module ExampleAutomation (automation, plan) where

import Declarations as SP
import Prelude hiding (or)
import Utils
import Data.List (nub)
import Val

-- Min and Max values for temperature and light control
type Min = Int
type Max = Int

-- min control - decides whether to switch on 
minControl :: Min -> Stream Int -> Stream Bool
minControl min s = s .<=. constant min

-- max control - determines whether to switch on 
maxControl :: Max -> Stream Int -> Stream Bool
maxControl max s = s .>=. constant max

-- average function used for reliable temperature and light
average :: [Stream Int] -> Stream Int
average ss = (foldl1 (\x y-> x .+. y) ss) ./. (constant (fromIntegral (length ss)))

-- any function returns true if any of the inputs is true
anyInput :: [Stream Bool] -> Stream Bool
anyInput ss = foldl1 (\x y -> x .||. y) ss

-- Application v2
type Plan = [(Room, Room)] -- list of adjoining rooms
data Room = Room {
              name    :: String,
              motionS :: [MotionSensor],
              lightS  :: [LightSensor],
              tempS   :: [TempSensor]}
  deriving (Eq, Show)

newtype MotionSensor = Motion (Device, Sensor) deriving (Eq, Show)
newtype LightSensor  = Light (Device, Sensor) deriving (Eq, Show)
newtype TempSensor   = Temperature (Device, Sensor) deriving (Eq, Show)
type LightControl = Bool
type TempControl  = Bool

automation :: Plan -> Stream Bool
automation plan = bundle $ automation' plan rooms
  where
    rooms = getRooms plan
    switches = automation' plan rooms

automation' :: Plan -> [Room] -> [Stream Bool]
automation' plan [] = []
automation' plan (r:rs) = roomAutomation plan r ++ automation' plan rs

roomAutomation :: Plan -> Room -> [Stream Bool]
roomAutomation plan room = [autoMinControl motion lightSensors 50, autoMaxControl motion tempSensors 25]
  where
    linkedRooms  = linkedTo plan room
    motion       = msToStream $ getMotionSensors linkedRooms
    lightSensors = lsToStream $ lightS room
    tempSensors  = tsToStream $ tempS room
    
autoMinControl :: [Stream Bool] -> [Stream Int] -> Min -> Stream Bool
autoMinControl motion sensors min = ifThenElse (anyInput motion, (minControl min (average sensors), false))

autoMaxControl :: [Stream Bool] -> [Stream Int] -> Max -> Stream Bool
autoMaxControl motion sensors max = ifThenElse (anyInput motion, (maxControl max (average sensors), false))

getRooms :: Plan -> [Room]
getRooms [] = []
getRooms ((x,y):rs) = nub (x : y : getRooms rs)

roomNames :: [Room] -> [String]
roomNames [] = []
roomNames (r:rs) = name r : roomNames rs

linkedTo :: Plan -> Room -> [Room]
linkedTo [] room = room : []
linkedTo ((x,y):rs) room 
                   | x == room = y : linkedTo rs room
                   | y == room = x : linkedTo rs room
                   | otherwise = linkedTo rs room

getMotionSensors :: [Room] -> [MotionSensor]
getMotionSensors [] = []
getMotionSensors (r:rs) = motionS r ++ getMotionSensors rs

msToStream :: [MotionSensor] -> [Stream Bool]
msToStream [] = []
msToStream (Motion (dev, inp): ms) = inputB dev inp : msToStream ms

lsToStream :: [LightSensor] -> [Stream Int]
lsToStream [] = []
lsToStream (Light (dev, inp): ms) = inputI dev inp : lsToStream ms

tsToStream :: [TempSensor] -> [Stream Int]
tsToStream [] = []
tsToStream (Temperature (dev, inp): ms) = inputI dev inp : tsToStream ms

-----------

motionSensor1 = Motion (device 1,sensor 0)
motionSensor2 = Motion (device 2,sensor 1)
motionSensor3 = Motion (device 3,sensor 2)
motionSensor4 = Motion (device 4,sensor 3)
motionSensor5 = Motion (device 5,sensor 4)
--motionSensor6 = Motion (device 6,sensor 5)
--motionSensor7 = Motion (device 7,sensor 6)
lightSensor1 = Light (device 1,sensor 7)
lightSensor2 = Light (device 2,sensor 8)
lightSensor3 = Light (device 3,sensor 9)
lightSensor4 = Light (device 4,sensor 10)
lightSensor5 = Light (device 5,sensor 11)
--lightSensor6 = Light (device 6,sensor 12)
--lightSensor7 = Light (device 7,sensor 13)
tempSensor1 = Temperature (device 1,sensor 14)
tempSensor2 = Temperature (device 2,sensor 15)
tempSensor3 = Temperature (device 3,sensor 16)
tempSensor4 = Temperature (device 4,sensor 17)
tempSensor5 = Temperature (device 5,sensor 18)
--tempSensor6 = Temperature (device 6,sensor 19)
--tempSensor7 = Temperature (device 7,sensor 20)

room1 = Room {name="room1", motionS=[motionSensor1, motionSensor2], lightS=[lightSensor1, lightSensor2], tempS=[tempSensor1, tempSensor2]}
room2 = Room {name="room2", motionS=[motionSensor3], lightS=[lightSensor3], tempS=[tempSensor3]}
room3 = Room {name="room3", motionS=[motionSensor4], lightS=[lightSensor4], tempS=[tempSensor4]}
room4 = Room {name="room4", motionS=[motionSensor5], lightS=[lightSensor5], tempS=[tempSensor5]}
--room3 = Room {name="room3", motionS=[motionSensor4, motionSensor5], lightS=[lightSensor4, lightSensor5], tempS=[tempSensor4, tempSensor5]}
--room4 = Room {name="room4", motionS=[motionSensor6, motionSensor7], lightS=[lightSensor6, lightSensor7], tempS=[tempSensor6, tempSensor7]}

plan = [(room1, room2), (room2, room3), (room3, room4)]

--simulatedValues = [L [B True,B False,B False,B False,B False,B False,B False,0,0,0,0,0,0,0,30,30,30,30,30,30,30],
 --                  L [B False,B False,B True,B False,B False,B False,B False,0,0,0,0,0,0,0,30,30,30,30,30,30,30]]
                   






