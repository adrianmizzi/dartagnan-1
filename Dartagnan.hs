module Dartagnan where

import           CodeGenContikiCM5000
import           CodeGenContikiIotLab
import           Data.List
import qualified Data.Map             as Map
import           Declarations         as My
import           DeviceManagement
import           Duplicates
import           ExampleAutomation
import           Examples
import           GraphDraw
import           MemoryManagement
import           PaperExamples
import           Simulator
import           TypeChecker
import           Utils
import           Val

-- Graph Visualisation.  Output is for GraphViz
-- can use http://graphs.grevian.org/graph/6002904091590656 to visualise the generated graph
-- alternatively, file is stored in graphs/graph.dot and can be converted into a PNG with script file

viz :: Stream a -> IO()
viz (Stream s) = writeFile "graphs/graph.dot" g
  where
    g = graph $ preprocessor s

{--
The preprocessor does 2 things:
1. Introduces new nodes when recursion is detected - Duplicate nodes.
2. Introduces device nodes when communication between 2 nodes is required and not explicitly defined.
--}

-- utility method
preprocessorS :: Stream a -> Stream1
preprocessorS (Stream s) = preprocessor s

preprocessor :: Stream1 -> Stream1
preprocessor s = deviceInfo $ duplicates s

-- 2 utility methods (main and mainToFile) to output code to screen or to file
main :: Stream a -> IO()
main (Stream s) = printCode "." False s

mainToFile :: String -> Stream a -> IO()
mainToFile device (Stream s) = printCode device True s

-- This is the main method which outputs the code to screen or to file.
printCode :: String -> Bool -> Stream1 -> IO()
printCode device toFile s = if toFile
                       then writeFiles dLoc code
                       else putStrLn $ foldl (++) "" code
  where
    (dLoc, code) = unzip $ toCode device s

-- instance of StreamProcessor type
instance StreamProcessor Stream1 where
  toAbstractC s = getProcesses (getSortOrder s') stmtList
    where
      s' = preprocessor s
      stmtList = eval s'
  toCode device s = toActualCode device $ toAbstractC s

-- Utility function to write files to disk
writeFiles :: [DeviceLocation] -> [String] -> IO()
writeFiles (d:[]) (c:[]) = writeFile ("Ccode/Device" ++ show d ++ ".c") c
writeFiles (d:dd) (c:cc) = do
                           writeFile ("Ccode/Device" ++ show d ++ ".c") c
                           writeFiles dd cc

-- Takes a list of processes and generates a list of DeviceLocations + Code
toActualCode :: String -> AbstractC -> [(DeviceLocation, String)]
toActualCode _ [] = []
toActualCode device (p:pp) = (devLoc, code) : toActualCode device plList
  where
    code | device == "CM5000" = CodeGenContikiCM5000.generateNode pnList
         | otherwise          = CodeGenContikiIotLab.generateNode pnList
    sameDevice (devLoc1, _, _, _) (devLoc2, _, _, _)  = devLoc1 == devLoc2
    notSameDevice p1 p2 = Prelude.not (sameDevice p1 p2)
    (devLoc, _, _, _) = p
    pnList = filter (sameDevice p) (p:pp)  -- all processes on this node
    plList = filter (notSameDevice p) pp   -- all other processes

-- Gets the order of the statements
getSortOrder :: Stream1 -> SortOrder
getSortOrder s = ordered ([], getAllLabels s) (getDependencies s)

-- Returns a list of Processes (which include devicelocation, statements, sort order and process id)
getProcesses :: SortOrder -> [Statement] -> AbstractC
getProcesses _ [] = []
getProcesses order ss = (devLoc, processNum, filtered, filteredOrder) : getProcesses order remaining
  where
    devLoc = dev (head ss)
    processNum   = pNum (head ss)
    (filtered, remaining) = filterStatements devLoc processNum ss
    filteredOrder = filterOrder order filtered

filterStatements :: DeviceLocation -> ProcessId -> [Statement] -> (Method, [Statement])
filterStatements devLoc processNum ss = ([s | s <- ss, (dev s == devLoc) && (pNum s == processNum)], [s | s <- ss, (dev s /= devLoc) || (pNum s /= processNum)])

-- filters the sort order to include elements for this device only
filterOrder :: SortOrder -> [Statement] -> SortOrder
filterOrder [] ss = []
filterOrder (o:oo) ss
             | exists o ss = o : filterOrder oo ss
             | otherwise   = filterOrder oo ss

exists :: Integer -> [Statement] -> Bool
exists _ [] = False
exists i (s:ss)
          | i == label s = True
          | otherwise      = exists i ss

-- evaluate the Stream Processor into a list of statements
eval :: Stream1 -> [Statement]
eval (RootNode label devLoc s) = (Statement {dev=devLoc, pNum=1, op=Op_Root streamType, label=label, inputs=[getLabel s']}) : eval' devLoc 1 s'
  where
    s' = memInfo s
    streamType = returnTypeOf s'
eval _ = error "Invalid Root Node - should start with a RootNode"

eval' :: DeviceLocation -> ProcessId -> Stream1 -> [Statement]
eval' _ pNum (Input l devLoc inpLoc) = Statement {dev=devLoc, pNum=pNum, op=Op_InputI inpLoc, label=l, inputs=[]}  : []
--eval' _ pNum (Input l devLoc inpLoc BType) = Statement {dev=devLoc, pNum=pNum, op=Op_InputB inpLoc, label=l, inputs=[]}  : []
eval' devLoc pNum (Const l (I x)) = Statement {dev=devLoc, pNum=pNum, op=Op_ConstI x, label=l, inputs=[]} : []
eval' devLoc pNum (Const l (B x)) = Statement {dev=devLoc, pNum=pNum, op=Op_ConstB x, label=l, inputs=[]} : []
eval' devLoc pNum (Dup l s)       = Statement {dev=devLoc, pNum=pNum, op=Op_DuplicateNode, label=l, inputs=[getLabel s]} : []
eval' devLoc pNum (Sqr l s)       = Statement {dev=devLoc, pNum=pNum, op=Op_Sqr, label=l,  inputs=[getLabel s]} : evalStream devLoc pNum s
eval' devLoc pNum (Odd l s)       = Statement {dev=devLoc, pNum=pNum, op=Op_Odd, label=l,  inputs=[getLabel s]} : evalStream devLoc pNum s
eval' devLoc pNum (Neg l s)       = Statement {dev=devLoc, pNum=pNum, op=Op_Neg, label=l,  inputs=[getLabel s]} : evalStream devLoc pNum s
eval' devLoc pNum (Not l s)       = Statement {dev=devLoc, pNum=pNum, op=Op_Not, label=l,  inputs=[getLabel s]} : evalStream devLoc pNum s
eval' devLoc pNum (Add l s1 s2)   = Statement {dev=devLoc, pNum=pNum, op=Op_Add, label=l, inputs=(getLabel s1) : [(getLabel s2)]} : eval2Streams devLoc pNum s1 s2
eval' devLoc pNum (Mul l s1 s2)   = Statement {dev=devLoc, pNum=pNum, op=Op_Mul, label=l, inputs=(getLabel s1) : [(getLabel s2)]} : eval2Streams devLoc pNum s1 s2
eval' devLoc pNum (Div l s1 s2)   = Statement {dev=devLoc, pNum=pNum, op=Op_Div, label=l, inputs=(getLabel s1) : [(getLabel s2)]} : eval2Streams devLoc pNum s1 s2
eval' devLoc pNum (My.GT l s1 s2) = Statement {dev=devLoc, pNum=pNum, op=Op_GT, label=l, inputs=(getLabel s1) : [(getLabel s2)]} : eval2Streams devLoc pNum s1 s2
eval' devLoc pNum (Eq l s1 s2)    = Statement {dev=devLoc, pNum=pNum, op=Op_Eq, label=l, inputs=(getLabel s1) : [(getLabel s2)]} : eval2Streams devLoc pNum s1 s2
eval' devLoc pNum (And l s1 s2)   = Statement {dev=devLoc, pNum=pNum, op=Op_And, label=l, inputs=(getLabel s1) : [(getLabel s2)]} : eval2Streams devLoc pNum s1 s2
eval' devLoc pNum (Or l s1 s2)    = Statement {dev=devLoc, pNum=pNum, op=Op_Or, label=l, inputs=(getLabel s1) : [(getLabel s2)]} : eval2Streams devLoc pNum s1 s2
eval' devLoc pNum (IfThenElse l s1 (s2,s3)) = Statement {dev=devLoc, pNum=pNum, op=Op_IfThenElse (returnTypeOf s2), label=l, inputs=(getLabel s1) : (getLabel s2) : [(getLabel s3)]} : eval3Streams devLoc pNum s1 s2 s3
eval' devLoc pNum (PreI l1 l2 memLoc x s) = (statement1 : statement2 : evalStream devLoc pNum s )
  where
    statement1 = Statement {dev=devLoc, pNum=pNum, op=Op_PreI memLoc x, label=l1,  inputs=[]}
    statement2 = Statement {dev=devLoc, pNum=pNum, op=Op_PreI' memLoc, label=l2, inputs=[getLabel s]}
eval' devLoc pNum (PreB l1 l2 memLoc x s) = (statement1 : statement2 : evalStream devLoc pNum s )
  where
    statement1 = Statement {dev=devLoc, pNum=pNum, op=Op_PreB memLoc x, label=l1,  inputs=[]}
    statement2 = Statement {dev=devLoc, pNum=pNum, op=Op_PreB' memLoc, label=l2, inputs=[getLabel s]}
eval' devLoc pNum (DeviceNode (Sender l1 loc1) (Receiver l2 loc2) newPNum commType s) = s1 : s2 : evalStream loc2 newPNum s
  where
    s1 = Statement {dev=devLoc, pNum=pNum, op=Op_Comm_Send loc2 commType newPNum, label=l1,  inputs=[]}
    s2 = Statement {dev=loc2, pNum=newPNum, op=Op_Comm_Receive loc1 commType newPNum, label=l2,  inputs=[getLabel s]}
eval' devLoc pNum (Bundle l ss) = Statement {dev=devLoc, pNum=pNum,op=Op_Bundle (returnTypeOf (Bundle l ss)), label=l, inputs=getBundleInputs ss} : evalStreams devLoc pNum ss

getBundleInputs :: [Stream1] -> [Label]
getBundleInputs []     = []
getBundleInputs (s:ss) = getLabel s : getBundleInputs ss

evalStream :: DeviceLocation -> ProcessId -> Stream1 -> [Statement]
evalStream devLoc pNum s = eval' devLoc pNum s

eval2Streams :: DeviceLocation -> ProcessId -> Stream1 -> Stream1 -> [Statement]
eval2Streams devLoc pNum s1 s2 = (evalStream devLoc pNum s2 ++ evalStream devLoc pNum s1)

eval3Streams :: DeviceLocation -> ProcessId -> Stream1 -> Stream1 -> Stream1 -> [Statement]
eval3Streams devLoc pNum s1 s2 s3 = eval2Streams devLoc pNum s1 s2 ++ evalStream devLoc pNum s3

evalStreams :: DeviceLocation -> ProcessId -> [Stream1] -> [Statement]
evalStreams devLoc pNum [] = []
evalStreams devLoc pNum (s:ss) = evalStream devLoc pNum s ++ evalStreams devLoc pNum (ss)

-- Graph Utilities -------

type DependencyMap = [(Label, Label)]

getDependencies :: Stream1 -> DependencyMap
getDependencies = nub . getDependencies'

getDependencies' :: Stream1 -> DependencyMap
getDependencies' (Input l devLoc inpLoc) = []
getDependencies' (Const l (I x))  = []
getDependencies' (Const l (B x))  = []
getDependencies' (Dup l s)        = (l, getLabel s) : []
getDependencies' (Sqr l s)        = (l, getLabel s) : getDependencies' s
getDependencies' (Odd l s)        = (l, getLabel s) : getDependencies' s
getDependencies' (Neg l s)        = (l, getLabel s) : getDependencies' s
getDependencies' (Not l s)        = (l, getLabel s) : getDependencies' s
getDependencies' (Add l s1 s2)    = (l, getLabel s1) : (l, getLabel s2) : getDependencies' s1 ++ getDependencies' s2
getDependencies' (Mul l s1 s2)    = (l, getLabel s1) : (l, getLabel s2) : getDependencies' s1 ++ getDependencies' s2
getDependencies' (Div l s1 s2)    = (l, getLabel s1) : (l, getLabel s2) : getDependencies' s1 ++ getDependencies' s2
getDependencies' (My.GT l s1 s2)  = (l, getLabel s1) : (l, getLabel s2) : getDependencies' s1 ++ getDependencies' s2
getDependencies' (Eq l s1 s2)     = (l, getLabel s1) : (l, getLabel s2) : getDependencies' s1 ++ getDependencies' s2
getDependencies' (And l s1 s2)    = (l, getLabel s1) : (l, getLabel s2) : getDependencies' s1 ++ getDependencies' s2
getDependencies' (Or l s1 s2)     = (l, getLabel s1) : (l, getLabel s2) : getDependencies' s1 ++ getDependencies' s2
getDependencies' (IfThenElse l s1 (s2,s3)) = (l, getLabel s1) : (l, getLabel s2) : (l, getLabel s3) : getDependencies' s1 ++ getDependencies' s2 ++ getDependencies' s3
getDependencies' (PreI l1 l2 memLoc x s) = (l2, getLabel s) : getDependencies' s
getDependencies' (PreB l1 l2 memLoc x s) = (l2, getLabel s) : getDependencies' s
getDependencies' (DeviceNode (Sender l1 loc1) (Receiver l2 loc2) newPNum commType s) = (l2, getLabel s) : getDependencies' s
getDependencies' (RootNode l devLoc s) = (l, getLabel s) : getDependencies' s
getDependencies' (Bundle l ss)      =  getBundleDependencies l ss ++ getDependenciesBundle ss

getBundleDependencies :: Label -> [Stream1] -> DependencyMap
getBundleDependencies _ []     = []
getBundleDependencies l (s:ss) = (l, getLabel s) : getBundleDependencies l ss

getDependenciesBundle :: [Stream1] -> DependencyMap
getDependenciesBundle []     = []
getDependenciesBundle (s:ss) = getDependencies' s ++ getDependenciesBundle  ss


-- Utility Methods
getAllLabels :: Stream1 -> [Label]
getAllLabels (Input l _ _) = l : []
getAllLabels (Const l _)      = l : []
getAllLabels (Dup l s)        = l : []
getAllLabels (DeviceNode (Sender l1 loc1) (Receiver l2 loc2) newPNum commType s) = nub (l1 : l2 : getAllLabels s)
getAllLabels (Sqr l s)        = nub (l : getAllLabels s)
getAllLabels (Odd l s)        = nub (l : getAllLabels s)
getAllLabels (Neg l s)        = nub (l : getAllLabels s)
getAllLabels (Not l s)        = nub (l : getAllLabels s)
getAllLabels (Add l s1 s2)    = nub (l : getAllLabels s1 ++ getAllLabels s2)
getAllLabels (Mul l s1 s2)    = nub (l : getAllLabels s1 ++ getAllLabels s2)
getAllLabels (Div l s1 s2)    = nub (l : getAllLabels s1 ++ getAllLabels s2)
getAllLabels (And l s1 s2)    = nub (l : getAllLabels s1 ++ getAllLabels s2)
getAllLabels (My.GT l s1 s2)    = nub (l : getAllLabels s1 ++ getAllLabels s2)
getAllLabels (Eq l s1 s2)    = nub (l : getAllLabels s1 ++ getAllLabels s2)
getAllLabels (Or l s1 s2)     = nub (l : getAllLabels s1 ++ getAllLabels s2)
getAllLabels (IfThenElse l s1 (s2, s3)) = nub (l : getAllLabels s1 ++ getAllLabels s2 ++ getAllLabels s3)
getAllLabels (PreI l1 l2 memLoc x s) = nub (l1 : l2 : getAllLabels s)
getAllLabels (PreB l1 l2 memLoc x s) = nub (l1 : l2 : getAllLabels s)
getAllLabels (RootNode l devLoc s) = nub (l : getAllLabels s)
getAllLabels (Bundle l ss) = nub (l : getAllLabelsBundle ss)

getAllLabelsBundle :: [Stream1] -> [Label]
getAllLabelsBundle []     = []
getAllLabelsBundle (s:ss) = getAllLabels s ++ getAllLabelsBundle ss
