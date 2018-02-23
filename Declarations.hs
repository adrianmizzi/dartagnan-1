{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Declarations where

import           Data.IORef
import           System.IO.Unsafe
import           Val

import qualified Data.Map         as Map

newtype Stream a = Stream Stream1
  deriving (Eq, Show)

data Stream1
  = RootNode Label DeviceLocation Stream1
  | Input Label DeviceLocation InputLocation
  | Const Label Val
  | ConstStream Label [Val] StreamType
  | Sqr Label Stream1
  | Odd Label Stream1
  | Neg Label Stream1
  | Not Label Stream1
  | Add Label Stream1 Stream1
  | Mul Label Stream1 Stream1
  | Div Label Stream1 Stream1
  | GT Label Stream1 Stream1
  | Eq Label Stream1 Stream1
  | And Label Stream1 Stream1
  | Or Label Stream1 Stream1
  | IfThenElse Label Stream1 (Stream1, Stream1)
  | PreI Label Label MemoryLocation Int Stream1
  | PreB Label Label MemoryLocation Bool Stream1
  | Dup Label Stream1
  | DeviceNode Sender Receiver ProcessId CommunicationType Stream1
  | Bundle Label [Stream1]
  deriving (Eq)

data Sender = Sender Label DeviceLocation
  deriving (Eq, Show)
data Receiver = Receiver Label DeviceLocation
  deriving (Eq, Show)

data StreamType = IType | BType | ListIType | ListBType
  deriving (Eq, Show, Ord)

data Statement = Statement {dev    :: DeviceLocation,
                            pNum   :: ProcessId,
                            op     :: Operand,
                            label  :: Label,
                            inputs :: [Variable]}
  deriving (Eq)

data Operand  =
  Op_InputI InputLocation
  | Op_InputB InputLocation
  | Op_ConstI Int
  | Op_ConstB Bool
  | Op_Sqr
  | Op_Odd
  | Op_Add
  | Op_Mul
  | Op_Div
  | Op_Neg
  | Op_GT
  | Op_Eq
  | Op_And
  | Op_Or
  | Op_Not
  | Op_IfThenElse StreamType
  | Op_PreI MemoryLocation Int
  | Op_PreI' MemoryLocation
  | Op_PreB MemoryLocation Bool
  | Op_PreB' MemoryLocation
  | Op_DuplicateNode
  | Op_Comm_Send DeviceLocation CommunicationType ProcessId -- send to recipient DeviceLocation and the ProcessId
  | Op_Comm_Receive DeviceLocation CommunicationType ProcessId -- return result to sender at DeviceLocation and the ProcessId
  | Op_Root StreamType
  | Op_Bundle StreamType
  deriving (Show, Eq, Ord)

data CommunicationType = Push | Pull
  deriving (Ord, Eq, Show)

type Label = Integer
type Variable = Integer
type MemoryLocation = Integer
type InputLocation = Integer
type DeviceLocation = Integer
type ProcessId = Label
type CProgram = String
type NodeName = String
type SortOrder = [Integer]
type Method = [Statement]
type Process = (DeviceLocation, ProcessId, Method, SortOrder)
type AbstractC = [Process]
newtype Device = Device Integer deriving (Eq, Show)
newtype Sensor = Sensor Integer deriving (Eq, Show)

-- Show implementation removes loops with Dup node
instance Show Stream1 where
  show (Input label devLoc inpLoc) = "Input |" ++ show label ++ "| " ++ show devLoc ++ " " ++ show inpLoc ++ " "
  show (Const label val) = "Const |" ++ show label ++ "| (" ++ show val ++ ")"
  show (ConstStream label vals streamtype) = "ConstStream |" ++ show label ++ "| (" ++ show vals ++ ")"
  show (Sqr label s) = "Sqr |" ++ show label ++ "| (" ++ show s ++ ") "
  show (Odd label s) = "Odd |" ++ show label ++ "| (" ++ show s ++ ") "
  show (Neg label s) = "Neg |" ++ show label ++ "| (" ++ show s ++ ") "
  show (Not label s) = "Not |" ++ show label ++ "| (" ++ show s ++ ") "
  show (Add label s1 s2) = "Add |" ++ show label ++ "| (" ++ show s1 ++ ") (" ++ show s2 ++ ") "
  show (Mul label s1 s2) = "Mul |" ++ show label ++ "| (" ++ show s1 ++ ") (" ++ show s2 ++ ") "
  show (Div label s1 s2) = "Div |" ++ show label ++ "| (" ++ show s1 ++ ") (" ++ show s2 ++ ") "
  show (Declarations.GT label s1 s2) = "GT |" ++ show label ++ "| (" ++ show s1 ++ ") (" ++ show s2 ++ ") "
  show (Eq label s1 s2) = "Eq |" ++ show label ++ "| (" ++ show s1 ++ ") (" ++ show s2 ++ ") "
  show (And label s1 s2) = "And |" ++ show label ++ "| (" ++ show s1 ++ ") (" ++ show s2 ++ ") "
  show (Or label s1 s2) = "Or |" ++ show label ++ "| (" ++ show s1 ++ ") (" ++ show s2 ++ ") "
  show (IfThenElse label s1 (s2, s3)) = "IfThenElse |" ++ show label ++ "| (" ++ show s1 ++ ") ((" ++ show s2 ++ "), (" ++ show s3 ++ ")) "
  show (PreI l1 l2 memLoc def s) = "PreI " ++ show l1 ++ " " ++ show l2 ++ " " ++ show memLoc ++ " " ++ show def ++ " (" ++ show s ++ ") "
  show (PreB l1 l2 memLoc def s) = "PreB " ++ show l1 ++ " " ++ show l2 ++ " " ++ show memLoc ++ " " ++ show def ++ " (" ++ show s ++ ") "
  show (Dup label s) = "Dup |" ++ show label ++ "| -> " ++ show (getLabel s)
  show (DeviceNode (Sender l1 loc1) (Receiver l2 loc2) pNum commType s) = "DeviceNode |" ++ show l1 ++ "->" ++ show l2 ++ "|" ++ " " ++ show loc1 ++ "->" ++ show loc2 ++ " " ++ show pNum ++ " " ++ show commType ++ " (" ++ show s ++ ") "
  show (RootNode label devLoc s) = "RootNode |" ++ show label ++ "| " ++ show devLoc ++ " (" ++ show s ++ ") "
  show (Bundle label ss) = "Bundle |" ++ show label ++ "| " ++ show ss

class Render a where
  render :: a -> String

instance Render Stream1 where
  render (Input label devLoc inpLoc) = "Input" ++ show devLoc ++ "-" ++ show inpLoc
  render (Const label val) = "Const"++ " " ++ show val
  render (Sqr label s) = "Sqr" ++ " (" ++ render s ++ ") "
  render (Odd label s) = "Odd" ++ " (" ++ render s ++ ") "
  render (Neg label s) = "Neg" ++ " (" ++ render s ++ ") "
  render (Not label s) = "Not" ++ " (" ++ render s ++ ") "
  render (Add label s1 s2) = "Add" ++ " (" ++ render s1 ++ ") (" ++ render s2 ++ ")"
  render (Mul label s1 s2) = "Mul" ++ " (" ++ render s1 ++ ") (" ++ render s2 ++ ")"
  render (Div label s1 s2) = "Div" ++ " (" ++ render s1 ++ ") (" ++ render s2 ++ ")"
  render (Declarations.GT label s1 s2) = "GT" ++ " (" ++ render s1 ++ ") (" ++ render s2 ++ ")"
  render (Eq label s1 s2) = "Eq" ++ " (" ++ render s1 ++ ") (" ++ render s2 ++ ")"
  render (And label s1 s2) = "And" ++ " (" ++ render s1 ++ ") (" ++ render s2 ++ ")"
  render (Or label s1 s2) = "Or" ++ " (" ++ render s1 ++ ") (" ++ render s2 ++ ")"
  render (IfThenElse label s1 (s2, s3)) = "IfThenElse" ++ " (" ++ render s1 ++ ") ((" ++ render s2 ++ "), (" ++ render s3 ++ "))"
  render (PreI l1 l2 memLoc def s) = "PreI" ++ show memLoc ++ " " ++ show def ++ " (" ++ render s ++ ") "
  render (PreB l1 l2 memLoc def s) = "PreB" ++ show memLoc ++ " " ++ show def ++ " (" ++ render s ++ ") "
  render (Dup label s) = "Dup " ++ " -> " ++ show (getLabel s)
  render (DeviceNode (Sender l1 loc1) (Receiver l2 loc2) pNum commType s) = "DeviceNode " ++ " " ++ show loc1 ++ "->" ++ show loc2 ++ " " ++ show pNum ++ " " ++ show commType ++ " (" ++ render s ++ ") "
  render (RootNode label devLoc s) = "RootNode " ++ " " ++ show devLoc ++ " (" ++ render s ++ ") "

instance Show Statement where
  show s = "\nStatement {" ++
     "dev=" ++ show (dev s) ++ ", " ++
     "pNum=" ++ show (pNum s) ++ ", " ++
     "op=" ++ show (op s) ++ ", " ++
     "label=" ++ show (label s) ++ ", " ++
     "inputs=" ++ show (inputs s) ++
     "}"

instance Ord Statement where
  s1 <= s2 = elem (label s1) (inputs s2)

isPre :: Operand -> Bool
isPre (Op_PreI _ _) = True
isPre (Op_PreB _ _) = True
isPre _             = False

isPre' :: Operand -> Bool
isPre' (Op_PreI' _) = True
isPre' (Op_PreB' _) = True
isPre' _            = False


-- type classes
class StreamProcessor a where
  toAbstractC :: a -> AbstractC
  toCode :: String -> a -> [(DeviceLocation, String)]

class Syn a b where
  (.==.) :: a -> a -> b

(.||.) :: Stream Bool -> Stream Bool -> Stream Bool
x .||. y = Declarations.or (x, y)

(.&&.) :: Stream Bool -> Stream Bool -> Stream Bool
x .&&. y = Declarations.and (x, y)

(.+.) :: Stream Int -> Stream Int -> Stream Int
x .+. y = add (x, y)

(.-.) :: Stream Int -> Stream Int -> Stream Int
x .-. y = sub (x, y)

(.*.) :: Stream Int -> Stream Int -> Stream Int
x .*. y = mul (x, y)

(./.) :: Stream Int -> Stream Int -> Stream Int
x ./. y = Declarations.div (x, y)

(.>.) :: Stream Int -> Stream Int -> Stream Bool
x .>. y = gt (x, y)

(.<.) :: Stream Int -> Stream Int -> Stream Bool
x .<. y = gt (y, x)

(.>=.) :: Stream Int -> Stream Int -> Stream Bool
x .>=. y = gte (x, y)

(.<=.) :: Stream Int -> Stream Int -> Stream Bool
x .<=. y = gte (y, x)


infixl 7 .*.
infixl 7 ./.
infixl 6 .+.
infixl 6 .-.
infix  4 .>.
infix  4 .>=.
infix  4 .<.
infix  4 .<=.
infix  4 .==.
infixr 3 .&&.
infixr 3 .||.

instance Syn (Stream Int) (Stream Bool) where
  x .==. y = eq (x, y)

instance Syn (Stream Bool)  (Stream Bool) where
  x .==. y = eq (x, y)

-- returns labels
getLabel :: Stream1 -> Label
getLabel (Input l _ _) = l
getLabel (Const l _)      = l
getLabel (Dup l s)        = l
getLabel (DeviceNode (Sender l1 loc1) (Receiver l2 loc2) newPNum commType s) = l1
getLabel (RootNode l devLoc s) = l
getLabel (Sqr l s)        = l
getLabel (Odd l s)        = l
getLabel (Neg l s)        = l
getLabel (Not l s)        = l
getLabel (Add l s1 s2)    = l
getLabel (Mul l s1 s2)    = l
getLabel (Div l s1 s2)    = l
getLabel (And l s1 s2)    = l
getLabel (Declarations.GT l s1 s2)    = l
getLabel (Eq l s1 s2)    = l
getLabel (Or l s1 s2)     = l
getLabel (IfThenElse l s1 (s2, s3)) = l
getLabel (PreI l1 l2 memLoc x s) = l1
getLabel (PreB l1 l2 memLoc x s) = l1
getLabel (Bundle l ss)     = l


{-# NOINLINE counter #-}
counter = unsafePerformIO (newIORef 1)

-- generate a new label every time
new_label :: () -> Integer
new_label () = unsafePerformIO $ do
  p <- readIORef counter
  writeIORef counter (p+1)
  return p

tCounter = unsafePerformIO (newIORef 2)

-- generate a new label every time
new_process_id :: () -> Integer
new_process_id () = unsafePerformIO $ do
  p <- readIORef tCounter
  writeIORef tCounter (p+1)
  return p


-- Define constructors as functions
false :: Stream Bool
false = constant False

true :: Stream Bool
true = constant True

add :: (Stream Int, Stream Int) -> Stream Int
add (Stream s1, Stream s2) = Stream (Add (new_label()) s1 s2)

sub :: (Stream Int, Stream Int) -> Stream Int
sub (s1, s2) = add (s1, neg s2)

mul :: (Stream Int, Stream Int) -> Stream Int
mul (Stream s1, Stream s2) = Stream (Mul (new_label()) s1 s2)

div :: (Stream Int, Stream Int) -> Stream Int
div (Stream s1, Stream s2) = Stream (Div (new_label()) s1 s2)

sqr :: Stream Int -> Stream Int
sqr (Stream s) = Stream (Sqr (new_label()) s)

neg :: Stream Int -> Stream Int
neg (Stream s) = Stream (Neg (new_label()) s)

odd :: Stream Int -> Stream Bool
odd (Stream s) = Stream (Odd (new_label()) s)

gt :: (Stream Int, Stream Int) -> Stream Bool
gt (Stream s1, Stream s2) = Stream (Declarations.GT (new_label()) s1 s2)

gte :: (Stream Int, Stream Int) -> Stream Bool
gte (s1, s2) = Declarations.or (gt (s1, s2), eq (s1, s2))

eq :: (Stream a, Stream a) -> Stream Bool
eq (Stream s1, Stream s2) = Stream (Eq (new_label()) s1 s2)

or :: (Stream Bool, Stream Bool) -> Stream Bool
or (Stream s1, Stream s2) = Stream (Or (new_label()) s1 s2)

and :: (Stream Bool, Stream Bool) -> Stream Bool
and (Stream s1, Stream s2) = Stream (And (new_label()) s1 s2)

not :: Stream Bool -> Stream Bool
not (Stream s) = Stream (Not (new_label()) s)

ifThenElse :: (Stream Bool, (Stream a, Stream a)) -> Stream a
ifThenElse (Stream s1, (Stream s2, Stream s3)) = Stream (IfThenElse (new_label()) s1 (s2, s3))

inputI :: Device -> Sensor -> Stream Int
inputI (Device a) (Sensor b) = Stream (Input (new_label()) a b)

inputB :: Device -> Sensor -> Stream Bool
inputB (Device a) (Sensor b) = Stream (Input (new_label()) a b)

max :: (Stream Int, Stream Int) -> Stream Int
max (s1, s2) = ifThenElse (gte (s1, s2), (s1,s2))

min :: (Stream Int, Stream Int) -> Stream Int
min (s1, s2) = ifThenElse (gte (s1, s2), (s2,s1))

constant :: (Streamable a) => a -> Stream a
constant x = Stream (Const (new_label()) (toVal x))

pre :: (Streamable a) => a -> Stream a -> Stream a
pre x (Stream s) = Stream (getPre x s)

root :: DeviceLocation -> Stream a -> Stream a
root x (Stream s) = Stream (RootNode (new_label()) x s)

pull :: Device -> Stream a -> Stream a
pull (Device x) (Stream s) = Stream (DeviceNode (Sender (getLabel s) 0) (Receiver (new_label()) x) (new_process_id()) Pull s')
  where
    s' = newLabel s

push :: Device -> Stream a -> Stream a
push (Device x) (Stream s) = Stream (DeviceNode (Sender (getLabel s) 0) (Receiver (new_label()) x) (new_process_id()) Push s')
  where
    s' = newLabel s

bundle :: [Stream a] -> Stream a
bundle ss = Stream (Bundle (new_label()) ss')
  where
    ss' = [a | (Stream a) <- ss]

unwrapStream :: Stream a -> Stream1
unwrapStream (Stream s) = s

wrapStream :: Stream1 -> Stream a
wrapStream s = Stream s

sensor x = Sensor x
device x = Device x

feedback e = e (feedback e)


newLabel :: Stream1 -> Stream1
newLabel (Input label devLoc inpLoc)    = Input (new_label()) devLoc inpLoc
newLabel (Const label val)              = Const (new_label()) val
newLabel (Sqr label s)                  = Sqr (new_label()) s
newLabel (Odd label s)                  = Odd (new_label()) s
newLabel (Neg label s)                  = Neg (new_label()) s
newLabel (Not label s)                  = Not (new_label()) s
newLabel (Add label s1 s2)              = Add (new_label()) s1 s2
newLabel (Mul label s1 s2)              = Mul (new_label()) s1 s2
newLabel (Div label s1 s2)              = Div (new_label()) s1 s2
newLabel (Declarations.GT label s1 s2)  = Declarations.GT (new_label()) s1 s2
newLabel (Eq label s1 s2)               = Eq (new_label()) s1 s2
newLabel (And label s1 s2)              = And (new_label()) s1 s2
newLabel (Or label s1 s2)               = Or (new_label()) s1 s2
newLabel (IfThenElse label s1 (s2, s3)) = IfThenElse (new_label()) s1 (s2, s3)
newLabel (PreI l1 l2 memLoc n s)        = PreI (new_label()) l2 memLoc n s
newLabel (PreB l1 l2 memLoc b s)        = PreB (new_label()) l2 memLoc b s
newLabel (Dup label s)                  = Dup (new_label()) s
-- newLabel (DeviceNode (Sender l1 devLoc1) (Receiver l2 devLoc2) pNum commType s) = DeviceNode (Sender l1 devLoc1) (Receiver l2 devLoc2) pNum commType s
newLabel (RootNode label devLoc s)      = RootNode (new_label()) devLoc s
--newLabel (Bundle label ss) = Bundle (new_label()) ss



class Streamable a where
  getPre :: a -> Stream1 -> Stream1
  toVal  :: a -> Val

instance Streamable Int where
  getPre n s = PreI (new_label()) (new_label()) 0 n s
  toVal n = (I n)

instance Streamable Bool where
  getPre b s = PreB (new_label()) (new_label()) 0 b s
  toVal b = (B b)
