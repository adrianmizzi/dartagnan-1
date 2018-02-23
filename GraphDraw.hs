module GraphDraw where

import Declarations
import Examples
import Utils
import Prelude ((++), show, IO, ($), putStrLn, String)
import Duplicates

graph :: Stream1 -> String
graph s = "digraph {\n" ++ labels s' ++ edges s' ++ "\n}"
  where
    s' = duplicates s 

labels :: Stream1 -> String
labels (Input label devLoc _) = "\n\t" ++ show label ++ "[label=\"Input "++ show devLoc ++"\"];"
labels (Const label val) = "\n\t" ++ show label ++ "[label=\""++ show val ++"\"];"
labels (Sqr label s) = "\n\t" ++ show label ++ " [label=\"Sqr\"];" ++ labels s
labels (Odd label s) = "\n\t" ++ show label ++ " [label=\"Odd\"];" ++ labels s
labels (Neg label s) = "\n\t" ++ show label ++ " [label=\"Neg\"];" ++ labels s
labels (Not label s) = "\n\t" ++ show label ++ " [label=\"Not\"];" ++ labels s
labels (Add label s1 s2) = "\n\t" ++ show label ++ " [label=\"Add\"];" ++ labels s1 ++ labels s2
labels (Mul label s1 s2) = "\n\t" ++ show label ++ " [label=\"Mul\"];" ++ labels s1 ++ labels s2
labels (Div label s1 s2) = "\n\t" ++ show label ++ " [label=\"Div\"];" ++ labels s1 ++ labels s2
labels (Declarations.GT label s1 s2) = "\n\t" ++ show label ++ " [label=\"GT\"];" ++ labels s1 ++ labels s2
labels (Eq label s1 s2) = "\n\t" ++ show label ++ " [label=\"Eq\"];" ++ labels s1 ++ labels s2
labels (And label s1 s2) = "\n\t" ++ show label ++ " [label=\"And\"];" ++ labels s1 ++ labels s2
labels (Or label s1 s2) = "\n\t" ++ show label ++ " [label=\"Or\"];" ++ labels s1 ++ labels s2
labels (IfThenElse label s1 (s2, s3)) = "\n\t" ++ show label ++ " [label=\"IfThenElse\"];" ++ labels s1 ++ labels s2 ++ labels s3
labels (PreI l1 l2 memLoc _ s) = "\n\t" ++ show l1 ++ " [label=\"PreI\"];" ++ labels s
labels (PreB l1 l2 memLoc _ s) = "\n\t" ++ show l1 ++ " [label=\"PreB\"];" ++ labels s
labels (Dup label s) =  "\n\t" ++ show label ++ " [label=\"Dup\"];" 
labels (DeviceNode (Sender l1 devLoc1) (Receiver l2 devLoc2) pNum commType s) = "\n\t" ++ show l1 ++ " [label=\"device " ++ show devLoc2 ++ "\"];" ++ labels s
labels (RootNode label devLoc s) = "\n\t" ++ show label ++ " [label=\"Root " ++ show devLoc++ "\"];" ++ labels s
--labels (Bundle label ss) = "\n\t" ++ show label ++ " [label=\"Bundle\"];" ++ labelsBundle ss

labelsBundle :: [Stream1] -> String
labelsBundle [] = ""
labelsBundle (s:ss) = labels s ++ labelsBundle ss 

edges :: Stream1 -> String
edges (Input label devLoc _) = ""
edges (Const label val) = ""
edges (Sqr label s) = "\n\t" ++ show label ++ " -> "++ show (getLabel s) ++ ";" ++ edges s
edges (Odd label s) = "\n\t" ++ show label ++ " -> "++ show (getLabel s) ++ ";" ++ edges s
edges (Neg label s) = "\n\t" ++ show label ++ " -> "++ show (getLabel s) ++ ";" ++ edges s
edges (Not label s) = "\n\t" ++ show label ++ " -> "++ show (getLabel s) ++ ";" ++ edges s
edges (Add label s1 s2) = "\n\t" ++ show label ++ " -> {"++ show (getLabel s1) ++ "," ++ show (getLabel s2) ++ "};" ++ edges s1 ++ edges s2
edges (Mul label s1 s2) = "\n\t" ++ show label ++ " -> {"++ show (getLabel s1) ++ "," ++ show (getLabel s2) ++ "};" ++ edges s1 ++ edges s2
edges (Div label s1 s2) = "\n\t" ++ show label ++ " -> {"++ show (getLabel s1) ++ "," ++ show (getLabel s2) ++ "};" ++ edges s1 ++ edges s2
edges (Declarations.GT label s1 s2) = "\n\t" ++ show label ++ " -> {"++ show (getLabel s1) ++ "," ++ show (getLabel s2) ++ "};" ++ edges s1 ++ edges s2
edges (Eq label s1 s2) = "\n\t" ++ show label ++ " -> {"++ show (getLabel s1) ++ "," ++ show (getLabel s2) ++ "};" ++ edges s1 ++ edges s2
edges (And label s1 s2) = "\n\t" ++ show label ++ " -> {"++ show (getLabel s1) ++ "," ++ show (getLabel s2) ++ "};" ++ edges s1 ++ edges s2
edges (Or label s1 s2) = "\n\t" ++ show label ++ " -> {"++ show (getLabel s1) ++ "," ++ show (getLabel s2) ++ "};" ++ edges s1 ++ edges s2
edges (IfThenElse label s1 (s2, s3)) = "\n\t" ++ show label ++ " -> {"++ show (getLabel s1) ++ "," ++ show (getLabel s2) ++ "," ++ show (getLabel s3) ++ "};" ++ edges s1 ++ edges s2 ++ edges s3
edges (PreI l1 l2 memLoc _ s) = "\n\t" ++ show l1 ++ " -> "++ show (getLabel s) ++ ";" ++ edges s
edges (PreB l1 l2 memLoc _ s) = "\n\t" ++ show l1 ++ " -> "++ show (getLabel s) ++ ";" ++ edges s
edges (Dup label s) = "\n\t" ++ show label ++ " -> "++ show (getLabel s) ++ ";" 
edges (DeviceNode (Sender l1 devLoc1) (Receiver l2 devLoc2) pNum commType s) = "\n\t" ++ show l1 ++ " -> "++ show (getLabel s) ++ ";" ++ edges s
edges (RootNode label devLoc s) = "\n\t" ++ show label ++ " -> "++ show (getLabel s) ++ ";" ++ edges s
--edges (Bundle label ss) = "\n\t" ++ show label ++ " -> {\""++ getLabelsBundle ss ++ "\"};" ++ edgesBundle ss

edgesBundle :: [Stream1] -> String
edgesBundle [] = ""
edgesBundle (s:ss) = edges s ++ edgesBundle ss

getLabelsBundle :: [Stream1] -> String
getLabelsBundle (s:[]) = show (getLabel s)
getLabelsBundle (s:ss) = show (getLabel s) ++ "\",\"" ++ getLabelsBundle ss




