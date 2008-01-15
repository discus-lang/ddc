
module Core.Sequence
	( slurpSuperDepsTree
	, sequenceCafsTree
	, dotSuperDeps )
where

import qualified Debug.Trace	as Debug

import Util
import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var
import Shared.Var		(NameSpace(..))
import Shared.Error

import Core.Exp
import Core.Util
import Core.Util.Slurp
import Core.Plate.FreeVars

import qualified Dot.Exp	as G
import qualified Dot.Graph	as G

import Util.Graph.Deps

-----
stage	= "Core.Sequence"

-----
sequenceCafsTree
	:: Tree -> Tree

sequenceCafsTree tree
 = let	
 	-- Slurp out the list of variables referenced
	--	by each top level binding.
	--
 	deps		= Map.fromList
			$ [(v, ds)	| (v, (ds, isCaf))	
						<- Map.toList $ slurpSuperDepsTree tree]

	-- Work out which top level bindings are cafs.
	--
	cafVars		= [v 	| p@(PBind v x) <- tree
				, isCafP p ]
			
	-- Check that the cafs are not recursively defined,
	--	our Sea level implementation does not handle this.
	--
	recCafs		= [v	| v		<- cafVars
				, Set.member v (graphReachable deps [v]) ]

   in	if not $ isNil recCafs
   		then panic stage $ "sequenceCafsTree: Cafs are recursive " % recCafs % "\n"
		else sequenceCafsTree2 deps cafVars tree
		

sequenceCafsTree2 deps cafVars tree
 = let
 	-- Work out an appropriate initialisation order for cafs
	--	we can't call a caf if it references other which
	--	are not yet initialisd.
	--
	initSeq		= [v	| v	<- graphSequence deps Set.empty cafVars
					, elem v cafVars]

	-- Partition top level bindings into cafs and non-cafs.
	(topCafs, topOthers)
			= partition isCafP tree
	
	-- Use the init sequence to order the cafs.
	--
	cafMap		= slurpSuperMapPs topCafs
	topCafs'	= foldl (\cafs v 
			 -> let	Just cafP	= Map.lookup v cafMap
			    in	cafs ++ [cafP])
			[] initSeq
			
   in	topCafs' ++ topOthers
   

-----
-- slurpSuperDepsTree
--	Slurps out the list of value variables which are free in
--	each top level binding in a tree.
--
--	Also checks whether a binding is a caf. (a non-function binding)
--
slurpSuperDepsTree
	:: Tree 
	-> Map Var ([Var], Bool)
	
slurpSuperDepsTree tree
 = let	deps	= catMaybes $ map slurpSuperDepsP tree
   in	Map.fromList deps
   	
	
slurpSuperDepsP pp
 = case pp of
 	PBind v x	-> Just (v, (slurpDepsX x, isCafP pp))
	_ 		-> Nothing

slurpDepsX xx
 	= [ v	| v <- Set.toList $ freeVars xx
		, not $ Var.isCtorName v
		, Var.nameSpace v == NameValue ]
				
	
-----
-- dotSuperDeps
--	Convert the output of slurpSuperDepsTree to a graph for printing.	
--
dotSuperDeps
	:: Map Var ([Var], Bool) -> G.Graph

dotSuperDeps deps
	= G.expandVarNodes
	$ G.DiGraph 
	$ catMap
		(\(v, (vs, isCaf)) 
			-> let 	cafMark	= if isCaf then "\nCAF" else ""
				color	= if isCaf then "blue" else "black"
			   in	[ G.SNode (G.NVar v) 
					[ G.ALabel (pprStr v ++ cafMark)
					, G.AColor color ]
				]

			++ [ G.SEdge (G.NVar v) (G.NVar x) | x <- vs])
	$ Map.toList deps
			
