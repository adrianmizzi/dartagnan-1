module CodeGenContikiCM5000 where

import Declarations
import Data.Char

{- 
    This is the Contiki implementation.  The code is generated specifically for Contiki OS.  
-}

-- Generate content of one node.  The processes are all running on the same node
generateNode :: [Process] -> String
generateNode pp = --"--------- DEVICE " ++ show devLoc ++ "--------\n" ++
                  imports pp ++ 
                  addr ++ 
                  callback pp ++ 
                  init_process pp master ++ 
                  generateNodeProcesses pp
  where
    (devLoc, pNum, ss, ssOrder) = head pp
    master = devLoc == 1

generateNodeProcesses :: [Process] -> String
generateNodeProcesses [] = ""
generateNodeProcesses (p:pp) = process_begin pNum ++ 
                           initInt (getIntVars ss) ++
                           initBool (getBoolVars ss) ++
                           initIntList (getIntListVars ss) ++
                           initBoolList (getBoolListVars ss) ++
                           code ++
                           process_end ++
                           generateNodeProcesses pp
  where
    (devLoc, pNum, ss, ssOrder) = p
    code = printStatements ssOrder ss

-- retrieve all int variables to initialise them as static with value 0
getIntVars :: [Statement] -> [String]
getIntVars [] = []
getIntVars (s:ss) 
   | stmtType == IType = ("x" ++ show (label s)) : getIntVars ss
   | otherwise = getIntVars ss
  where 
    stmtType = getType (op s)

-- retrieve all bool variables to initialise them as static
getBoolVars :: [Statement] -> [String]
getBoolVars [] = []
getBoolVars (s:ss) 
   | stmtType == BType = ("x" ++ show (label s)) : getBoolVars ss
   | otherwise = getBoolVars ss
  where 
    stmtType = getType (op s)

-- retrieve all int variables to initialise them as static
getIntListVars :: [Statement] -> [String]
getIntListVars [] = []
getIntListVars (s:ss) 
   | stmtType == ListIType = ("x" ++ show (label s)) : getIntListVars ss
   | otherwise = getIntListVars ss
  where 
    stmtType = getType (op s)

-- retrieve all int variables to initialise them as static
getBoolListVars :: [Statement] -> [String]
getBoolListVars [] = []
getBoolListVars (s:ss) 
   | stmtType == ListBType = ("x" ++ show (label s)) : getBoolListVars ss
   | otherwise = getBoolListVars ss
  where 
    stmtType = getType (op s)

getType :: Operand -> StreamType
getType (Op_InputI _) = IType
getType (Op_InputB _) = BType
getType (Op_ConstI _) = IType
getType Op_Sqr = IType
getType Op_Neg = IType
getType Op_Add = IType
getType Op_Mul = IType
getType Op_Div = IType
getType Op_And = IType
getType Op_Or = IType
getType (Op_IfThenElse streamType) = streamType
getType Op_DuplicateNode = IType
getType (Op_Comm_Send _ _ _) = IType
getType (Op_Comm_Receive _ _ _) = IType
getType (Op_Root streamType) = streamType
getType (Op_PreI _ _) = IType
getType (Op_ConstB _) = BType
getType Op_Odd = BType
getType Op_Not = BType
getType Op_GT = BType
getType Op_Eq = BType
getType (Op_PreB _ _) = BType
getType (Op_Bundle streamType) = streamType


-- For every statement, call the toC function
printStatements :: [Label] -> [Statement] -> String
printStatements [] ss = ""
printStatements (l:ll) ss = printStatement l ss ++ printStatements ll ss

printStatement :: Label -> [Statement] -> String
printStatement _ [] = ""
printStatement l (s:ss) = if (l == label s) 
                            then "  " ++ toC (op s) (label s) (inputs s) ++ "\n" 
                            else printStatement l ss


-- Converts a Statement into C Code
toC :: Operand -> Variable -> [Variable] -> String
toC (Op_InputI inpLoc) output input = "x" ++ show output ++ " = (int) readTemperature();" 
toC (Op_InputB inpLoc) output input = "x" ++ show output ++ " = (bool) readSensor();" 
toC (Op_ConstI x) output input = "x" ++ show output ++ " = " ++ show x ++ ";"
toC (Op_ConstB x) output input = "x" ++ show output ++ " = " ++ map toLower (show x) ++ ";"
toC Op_Sqr output input = "x" ++ show output ++ " = x" ++ show (head input) ++ " * x" ++ show (head input) ++ ";"
toC Op_Odd output input = "x" ++ show output ++ " = x" ++ show (head input) ++ " % 2;"
toC Op_Neg output input = "x" ++ show output ++ " = -x" ++ show (head input) ++ ";"
toC Op_Not output input = "x" ++ show output ++ " = !x" ++ show (head input) ++ ";"
toC Op_Add output input = "x" ++ show output ++ " = x" ++ show (head input) ++ " + x" ++ show (input!!1) ++ ";"
toC Op_Mul output input = "x" ++ show output ++ " = x" ++ show (head input) ++ " + x" ++ show (input!!1) ++ ";"
toC Op_Div output input = "x" ++ show output ++ " = x" ++ show (head input) ++ " + x" ++ show (input!!1) ++ ";"
toC Op_GT output input = "x" ++ show output ++ " = x" ++ show (head input) ++ " > x" ++ show (input!!1) ++ ";"
toC Op_Eq output input = "x" ++ show output ++ " = x" ++ show (head input) ++ " == x" ++ show (input!!1) ++ ";"
toC Op_And output input = "x" ++ show output ++ " = x" ++ show (head input) ++ " && x" ++ show (input!!1) ++ ";"
toC Op_Or output input = "x" ++ show output ++ " = x" ++ show (head input) ++ " || x" ++ show (input!!1) ++ ";"
toC (Op_IfThenElse _) output input = "if (x" ++ show (input !! 0) ++ ")\n    x"++ show output ++ "=x" ++ show (input!!1) ++ ";\n  else\n    x"++ show output ++ "=x" ++ show (input!!2) ++ ";"
toC (Op_PreI memLoc x) output input = "x" ++ show output ++ " = mem" ++ show memLoc ++ ";"
toC (Op_PreI' memLoc) output input = "mem" ++ show memLoc ++ " = x" ++ show (head input) ++ ";"
toC (Op_PreB memLoc x) output input = "x" ++ show output ++ " = mem" ++ show memLoc ++ ";"
toC (Op_PreB' memLoc) output input = "mem" ++ show memLoc ++ " = x" ++ show (head input) ++ ";"
toC (Op_DuplicateNode) output input = "x" ++ show output ++ " = x" ++ show (head input) ++ ";"
toC (Op_Comm_Send destLoc commType msgNum) output input = send_message commType output destLoc msgNum
toC (Op_Comm_Receive destLoc commType msgNum) output input = return_result (head input) destLoc msgNum
toC (Op_Root IType) output input = "\n  printf(\"Result is %d\\n\\n\", x"++ show (head input) ++ ");"
toC (Op_Root BType) output input = "\n  printf(\"Result is %s\\n\\n\", x"++ show (head input) ++ "? \"true\" : \"false\");" 
toC (Op_Root ListIType) output input = "\n  printIntArray(x"++ show (head input) ++ ");"
toC (Op_Root ListBType) output input = "\n  printBoolArray(x"++ show (head input) ++ ");" 
toC (Op_Bundle _) output input = plexToC 0 output input

plexToC :: Integer -> Variable -> [Variable] -> String
plexToC _ _ [] = ""
plexToC index varName (v:[]) = "x" ++ show varName++"["++show index++"] = x" ++ show v ++ ";"
plexToC index varName (v:vv) = "x" ++ show varName++"["++show index++"] = x" ++ show v ++ ";\n  " ++ plexToC (index+1) varName vv

-- Unused Section
printDebug :: [Statement] -> String
printDebug [] = ""
printDebug (s:ss) = 
  if (Prelude.not(isPre'(op s)))
    then
      "    printf(\"x"++ show (label s) ++ "=[%d]; \",x"++ show (label s) ++ ");\n" ++ printDebug ss
    else 
      printDebug ss

printOutput :: Stream1 -> String
printOutput s = "    printf(\"Output x" ++ show (label) ++ "=[%d]\", x" ++ show (label) ++ ");\n"
  where 
    label = getLabel s


-- Template Code
imports :: [Process] -> String
imports pp =
  "#include \"contiki.h\"\n" ++
  "#include \"net/rime/rime.h\"\n" ++
    "\n" ++
  "#include <stdio.h>\n" ++
  "#include <stdbool.h>\n" ++
  "#include \"../lib/mycomms.c\"\n" ++
  "#include \"../lib/mysensorutils.c\"\n" ++ 
  "#include \"../lib/myutils.c\"\n" ++ 
  "#include \"../lib/myencoderdecoder.c\"\n" ++
  "#include \"../lib/myscheduler.c\"\n" ++
    "\n" ++
  "/*---------------------------------------------------------------------------*/\n" ++
  "PROCESS(init_process, \"Initialise Process\");\n" ++
  processes pp ++
  "AUTOSTART_PROCESSES(&init_process, &scheduler_process);\n" ++
  "/*---------------------------------------------------------------------------*/\n\n" ++
  processevents pp ++
  "\n" ++
  doMems pp

addr = "\n" ++
  "static linkaddr_t node1 = { { 21, 59 } };\n" ++
  "static linkaddr_t node2 = { { 13, 41 } };\n" ++
  "static linkaddr_t node3 = { { 168, 31 } };\n" ++
  "static linkaddr_t node4 = { { 207, 37 } };\n" ++
  "static linkaddr_t node5 = { { 96, 41 } };\n"

processes :: [Process] -> String
processes [] = ""
processes ((_, pNum, _, _):pp) = "PROCESS(process_"++ show pNum ++", \"Process "++ show pNum ++"\");\n" ++ processes pp

processevents :: [Process] -> String
processevents [] = ""
processevents (p:pp) = processevents' messages
  where 
    (_, _, ss, _) = p
    messages = getComms operands
    operands = [op s | s <- ss]

processevents' :: [ProcessId] -> String
processevents' [] = ""
processevents' (m:mm) = "static process_event_t event_data_ready_"++ show m ++ ";\n" ++ processevents' mm

doMems :: [Process] -> String
doMems [] = ""
doMems (p:pp) = doMems' op_mems ++ doMems pp
  where
    (_, _, ss, _) = p
    op_mems = [op mems | mems <- ss, isPre (op mems)]

doMems' :: [Operand] -> String
doMems' [] = ""
doMems' ((Op_PreI memLoc x):oo) = "static int mem" ++ show memLoc ++ " = " ++ show x ++ ";\n" ++ doMems' oo
doMems' ((Op_PreB memLoc x):oo) = "static bool mem" ++ show memLoc ++ " = " ++ show x ++ ";\n" ++ doMems' oo

callback :: [Process] -> String
callback pp = "\n" ++
  "static void recv_uc(struct unicast_conn *c, const linkaddr_t *from)\n" ++
  "{\n" ++
  "  char *incoming;\n" ++
  "  incoming = packetbuf_dataptr();\n" ++
  "\n" ++
  "  printf(\"Received a message: %s \\n\", incoming);\n" ++
  "\n" ++
  "  switch (getMessageType(incoming)) {\n" ++
  doSwitch pp ++
  "    default :\n" ++
  "      break;\n" ++
  "  }\n" ++
  "}\n" ++
  "\n" ++
  "static const struct unicast_callbacks unicast_callbacks = {recv_uc};\n" ++
  "/*---------------------------------------------------------------------------*/\n"

doSwitch :: [Process] -> String
doSwitch [] = ""
doSwitch ((_, pNum, ss, _):pp) = doSwitch' pNum oo ++ doSwitch pp
  where
    oo = [op s | s <- ss]

doSwitch' :: ProcessId -> [Operand] -> String
doSwitch' _ [] = ""
doSwitch' pNum ((Op_Comm_Send _ _ msgNum):ss) = 
  "    case " ++ show msgNum ++ " :\n" ++ 
  "      process_post(&process_" ++ show pNum ++ ", event_data_ready_" ++ show msgNum ++ ", incoming);\n" ++
  "      break;\n" ++
         doSwitch' pNum ss
doSwitch' pNum ((Op_Comm_Receive _ _ msgNum):ss) = 
  "    case " ++ show msgNum ++ " :\n" ++ 
  "      process_start(&process_" ++ show pNum ++ ", NULL);\n" ++
  "      break;\n" ++
         doSwitch' pNum ss
doSwitch' pNum (_:ss) = doSwitch' pNum ss

init_process :: [Process] -> Bool -> String
init_process pp master = "\n" ++
  "PROCESS_THREAD(init_process, ev, data)\n" ++
  "{\n" ++
  "  PROCESS_BEGIN();\n" ++
  "\n" ++
  "  initialiseTemperatureSensor();\n" ++
  "\n" ++
  "  // allocate the events\n" ++
  init_processevents pp ++
  "\n" ++
  "  init_comms(&unicast_callbacks);\n" ++
  "\n" ++
  "  // schedule jobs in scheduler\n" ++
  scheduled_jobs pp ++
  "\n" ++
  master_code_only ++
  "\n" ++
  "  PROCESS_END();\n" ++
  "}\n"
  where
    master_code_only = if master 
                         then 
                           "  static struct etimer et;\n" ++
                           "\n" ++
                           "  while (1) {\n" ++
                           "    // start main process every 60 seconds\n" ++
                           "    etimer_set(&et, 60*CLOCK_SECOND);\n" ++
                           "    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&et));\n" ++
                           "    printf (\"\\n\");\n" ++
                           "\n" ++
                           "    process_start(&process_1, NULL); \n" ++
                           "  }\n"
                         else
                           ""

scheduled_jobs :: [Process] -> String
scheduled_jobs pp = scheduled_jobs' (getReceivers pp) 0

scheduled_jobs' :: [(DeviceLocation, ProcessId)] -> Integer -> String
scheduled_jobs' [] n = []
scheduled_jobs' ((dest, p):pp) n = "  jobs[" ++ show n ++ "].jobId=" ++ show (n+1) ++ ";\n" ++
                           "  jobs[" ++ show n ++ "].period=3;\n" ++
                           "  jobs[" ++ show n ++ "].process=&process_" ++ show p ++ ";\n" ++
                           "  jobs[" ++ show n ++ "].destination=node"++ show dest ++ ";\n" ++
                           "  printf(\"Initialisation of job %d complete\\n\", jobs["++ show n ++"].jobId);" ++
                           scheduled_jobs' pp (n+1)

getReceivers :: [Process] -> [(DeviceLocation, ProcessId)]
getReceivers [] = []
getReceivers (p:pp) = getReceivers' operands ++ getReceivers pp
  where 
    (_, _, ss, _) = p
    operands = [op s | s <- ss]

getReceivers' :: [Operand] -> [(DeviceLocation, ProcessId)]
getReceivers' [] = []
getReceivers' ((Op_Comm_Receive devLoc Push msgNum):ss) = (devLoc, msgNum) : getReceivers' ss
getReceivers' (_:ss) = getReceivers' ss



init_processevents :: [Process] -> String
init_processevents [] = ""
init_processevents (p:pp) = init_processevents' messages
  where 
    (_, _, ss, _) = p
    messages = getComms operands
    operands = [op s | s <- ss]

init_processevents' :: [ProcessId] -> String
init_processevents' [] = ""
init_processevents' (m:mm) = "  event_data_ready_"++ show m ++" = process_alloc_event();\n" ++ init_processevents' mm

getComms :: [Operand] -> [ProcessId]
getComms [] = []
getComms ((Op_Comm_Send devLoc commType msgNum):ss) = msgNum : getComms ss
getComms ((Op_Comm_Receive devLoc commType msgNum):ss) = msgNum : getComms ss
getComms (_:ss) = getComms ss


process_begin :: ProcessId -> String
process_begin pNum = "\n" ++
  "PROCESS_THREAD(process_" ++ show (pNum) ++ ", ev, data)\n" ++
  "{\n" ++
  "  PROCESS_BEGIN();\n" ++
  "  static struct etimer et;\n"

initInt :: [String] -> String
initInt [] = ""
initInt nn = "  static int " ++ foldl1 (\x y -> x ++ "," ++ y) nn ++ " = 0;\n"

initBool :: [String] -> String
initBool [] = ""
initBool nn = "  static bool " ++ foldl1 (\x y -> x ++ "," ++ y) nn ++ " = false;\n"

initIntList :: [String] -> String
initIntList [] = ""
initIntList (n:nn) = "  static int " ++ n ++ "[10];\n" ++ initIntList nn
-- initIntlist (n:nn) = "  static int["++ length n ++ "] " ++ foldl1 (\x y -> x ++ "," ++ y) nn ++ " = int [" ++ length nn ++ ";\n" 


initBoolList :: [String] -> String
initBoolList [] = ""
initBoolList (n:nn) = "  static bool " ++ n ++ "[10];\n" ++ initBoolList nn

send_message :: CommunicationType -> Integer -> Integer -> Integer -> String
send_message commType var destNode msgNum = "\n" ++
  mode commType destNode msgNum ++
  "  // wait for max 10 seconds\n" ++
  "  etimer_set(&et, 10*CLOCK_SECOND);\n" ++
  "  while (ev != event_data_ready_"++ show msgNum ++ " && !etimer_expired(&et)){\n" ++
  "    PROCESS_YIELD();\n" ++
  "  }\n" ++
  "\n" ++
  "  if (etimer_expired(&et)) {\n" ++
  "    printf (\"No reading received in this call.\\n\");\n" ++
  "    goto END;\n" ++
  "  } else if (ev == event_data_ready_"++ show msgNum ++") {\n" ++
  "    x"++ show (var) ++ " = getReading(data);\n" ++
  "  }\n"

mode :: CommunicationType -> Integer -> Integer -> String
mode commType destNode msgNum = if (commType == Pull)
                                then 
                                  "  char *message"++ show msgNum ++ " = encode(" ++ show msgNum ++", \"\");\n" ++
                                  "  send_message(node"++ show destNode ++ ", message"++ show msgNum ++");\n"
                                else
                                  ""

return_result :: Integer -> Integer -> Integer -> String
return_result var destNode msgNum = "char result[5];\n" ++
  "  itoa(x"++ show var ++", result, 10);\n" ++
  "  char *message = encode("++ show msgNum ++ ", result);\n" ++
  "  send_message(node"++ show destNode++", message);\n"

process_end = "\n" ++
  "  printf(\"\\n\");\n" ++
  "  END:break;\n" ++
  "  PROCESS_END();\n" ++
  "}\n"


