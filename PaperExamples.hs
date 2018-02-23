module PaperExamples where

import           Declarations
import           Utils
import           Val

average :: (Stream Int, Stream Int, Stream Int) -> Stream Int
average (i1,i2,i3) = (i1 .+. i2 .+. i3) ./. 3

average2 :: (Stream Int, Stream Int, Stream Int) -> (Stream Int, Stream Int)
average2 (i1,i2,i3) = (avg, avg)
  where
    avg = (i1 .+. i2 .+. i3) ./. 3

-- a simple firealarm.  Returns true if the temperature reading is higher than 50
firealarm50 :: Stream Int -> Stream Bool
firealarm50 sensor1 = sensor1 .>. 50

--firealarm threshold sensor1 - opens up a new family of firealarms which can accept different values
firealarm :: Int -> Stream Int -> Stream Bool
firealarm threshold sensor1 = sensor1 .>. constant threshold

stickyalarm :: Int -> Stream Int -> Stream Bool
stickyalarm threshold sensor1 = feedback (\x -> pre False x .||. (sensor1 .>. constant threshold))

-- example showing how to double up without resampling
doubleUp :: ((Stream Int, Stream Int) -> Stream Int) -> (Stream Int, Stream Int) -> Stream Int
doubleUp f (sensor1, sensor2) = let x = f (sensor1, sensor2)
                                  in x .+. x

doubleUp2 :: ((Stream Int, Stream Int) -> Stream Int) ->
                      (Stream Int, Stream Int) -> Stream Int
doubleUp2 f (sensor1, sensor2) = f (sensor1, sensor2) .+. f (sensor1, sensor2)


firealarm5 :: Stream Int -> Stream Int -> (Stream Bool, Stream Bool)
firealarm5 threshold sensor1 =
            let x = pre False x .||. (sensor1 .>. threshold)
                y = (pre 0 sensor1 .<=. threshold) .&&. (sensor1 .>. threshold)
            in (x, y)

------------------------------


-- the simplest form of Stream Processor
simple sensor1 = sensor1

-- firealarm1 will sound the alarm when the temperature read on sensor1 is higher than 50 degrees
firealarm1 :: (Stream Int, Stream Int) -> Stream Bool
firealarm1 (sensor1, threshold) = sensor1 .>. threshold

-- firealarm2 checks both sensors 1 and 2 every time. Sensor 1 fetches (Pull) information from Sensor 2 every time
--firealarm2 ((sensor1, sensor2), threshold) =
--         and (sensor1 .>. threshold) (sensor2 .>. threshold)


-- firealarm3 will check reading on sensor2 (if sensor1 is high) before sounding the alarm. Reduces false alarms.
--firealarm3 ((sensor1, sensor2), threshold) =
--             ifThenElse (sensor1 .>. threshold)
--                (sensor2 .>. threshold, (constB False))


-- firealarm2' checks both sensors 1 and 2 every time. Sensor 2 sends information to Sensor 1 pre-emptively
--firealarm2' ((sensor1, sensor2), threshold) = and (sensor1 .>. threshold) (push (device 2) sensor2 .>. threshold)

-- firealarm4 triggers an alert when temperature goes over the 50 degrees (invisible) line
firealarm4 (sensor1, threshold) =
                   (pre 0 sensor1 .<=. threshold) .&&. (sensor1 .>. threshold)

-- firealarm4 will keep sounding the alarm if at any point the temperature is higher than 50 degrees
-- firealarm4 sensor1 = let x = or (preB False x) (sensor1 .>. (constI 50))
--                     in x

-- firealarm5 allows the alarm to be manually reset
--resetButton sensor2 = eq sensor2 (constI 1)
--firealarm5 sensor1 resetButton = let y = sensor1 .>. (constI 50)
--                                     x = ifThenElse resetButton ((constB False),(or (preB False x) y))
--                                 in x

-- firealarm5 sounds an alarm AND sends notifications
--firealarm5 (sensor1, threshold) =
--            let x = or (preB False x, sensor1 .>. threshold)
--                y = and ((preI 0 sensor1) .<=. threshold, sensor1 .>. threshold)
--            in bundle [x, y]



-- Observable Sharing
shared = let x = Declarations.odd (inputI (device 1) (sensor 1))
         in x .&&. x

-- Higher Level Abstraction
-- Reliable Alarm
--reliableAlarm ((i1, i2), trigger, diff) = ifThenElse (i1 .>. trigger)
--                      (((max i1 i2) .-. (min i1 i2)) .<. diff, (constB False))


twoReadings sensor1 sensor2 = add (sensor1, sensor2)
doubleReadings sensor1 = let x = sensor1
                         in add (x, x)
