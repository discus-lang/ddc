
module Dot.Graph
	( dotMapVarVars
	, expandVarNodes )
where
import qualified Data.Map	as Map
import DDC.Main.Pretty
import DDC.Var
import Dot.Exp
import Util

-- | Make a graph from a Map of Var -> [Var]
dotMapVarVars 
	:: Map Var [Var] -> Graph
	
dotMapVarVars mm
 	= expandVarNodes
	$ DiGraph 
	$ catMap (\(v, vs) -> [SEdge (NVar v) (NVar x) | x <- vs]) 
	$ Map.toList mm
			


-- | Looks to see what vars are refered to in the edges of a graph
--	and adds node definitions at the start so that each node in the
--	output graph will be rendered with the correct var name.
expandVarNodes
	:: Graph -> Graph
	
expandVarNodes graph
 = let	
 	-- see what vars are referenced in the graph
 	varsRef	= nub $ collectVarsG graph
	
	-- see what vars already have node defs.
	varsDef	= nub $ [v | SNode (NVar v) _ <- graphGetStmts graph]
	
	-- add definitions for nodes which don't already have them.
	varsAdd	= varsRef \\ varsDef
	
 	ss	= map (\v -> SNode (NVar v) [ALabel (pprStrPlain v)]) varsAdd
   in	graphAddStmts ss graph
   

collectVarsG :: Graph -> [Var]
collectVarsG g
 = case g of
 	Graph ss	-> catMap collectVarsS ss
	DiGraph ss	-> catMap collectVarsS ss
	
collectVarsS ss
 = case ss of
 	SEdge n1 n2	-> collectVarsN n1 ++ collectVarsN n2
	SNode n1 aa	-> collectVarsN n1
	
collectVarsN nn
 = case nn of
 	NVar v		-> [v]
	NString _	-> []


graphAddStmts :: [Stmt] -> Graph -> Graph
graphAddStmts	ss graph
 = case graph of
 	DiGraph ss2	-> DiGraph (ss2 ++ ss)
	Graph   ss2	-> Graph   (ss2 ++ ss)
	

graphGetStmts :: Graph -> [Stmt]
graphGetStmts    graph
 = case graph of
 	DiGraph ss	-> ss
	Graph ss	-> ss
	

