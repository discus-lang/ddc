-- | Handles crushing of shape constraints.

module Type.Crush.Shape
	( crushShape )
where

import Util
import Shared.Var	(Var, NameSpace(..))
import qualified Shared.VarBind	as Var

import Type.Exp
import Type.Pretty
import Type.Util
import Type.State
import Type.Class

import Type.Crush.Unify

-----
debug	= True
trace s	= when debug $ traceM s
stage	= "Type.Crush.Shape"


-----
-- crushShape
--	Try and crush the Shape constraint in this class.
--	If any of the nodes in the constraint contains a type constructor then add a similar constructor
--	to the other nodes and remove the constraint from the graph.
--
--	TODO: Add more shape fetters recursively.
--
--	returns whether we managed to crush this fetter.
--
crushShape :: ClassId -> SquidM Bool
crushShape shapeCid
 = do 	
 	-- Grab the Shape fetter from the class and extract the list of cids to be merged.
	Just shapeC	<- lookupClass shapeCid

	let f@(FConstraint v shapeTs) = classFetter shapeC
	let mergeCids	= map (\(TClass k cid) -> cid) shapeTs

	trace	$ "*   Crush.crushShape " 	% shapeCid 	% "\n"
		% "    fetter      = "	 	% f		% "\n"
		% "    mergeCids   = "		% mergeCids 	% "\n"


 	-- Make sure that all the classes to be merged are unified.
	--	We're expecting a maximum of one constructor per class queue.
	--
 	mapM crushUnifyClass mergeCids
 
	-- Lookup all the nodes.
 	mergeCs		<- liftM (map (\(Just c) -> c)) 
 			$  mapM lookupClass mergeCids

	-- Try and extract a type template from one of the nodes.
	let mts		= map (\c -> case classType c of
				Just t@(TData{})	-> Just t
				Just _			-> Nothing)
			$ mergeCs
	
	let template	= takeFirstJust mts

	trace	$ "    mTs         = "	% mts		% "\n"
		% "    template    = "	% template	% "\n"
		% "\n"
		
	case template of
	 -- There's no template to work from, and nothing else we can do.
	 --	We need to reactivate ourselves so we get called in the next grind
	 Nothing	
	  -> do	activateClass shapeCid
	  	return False

	 -- We've got a template. 
	 --	We can merge the sub classes and remove the shape constaint.
	 Just tt	
	  -> do	crushShapeMerge mergeCids mergeCs mts tt
		delClass shapeCid
		return True


crushShapeMerge 
	:: [ClassId] 		-- classIds to merge
	-> [Class] 		-- classes corresponding to each classId above.
	-> [Maybe Type] 	-- the types of the nodes, or Nothing if they're bottom.
	-> Type			-- the template type.
	-> SquidM ()

crushShapeMerge cids cs mts template@(TData v templateTs)
 = do 	let kinds	= map kindOfType templateTs

	-- Grab the type args from each of the available constructors.
 	let mArgss	= map (\mt -> liftM (\(TData v ts) -> ts) mt) mts

	-- Merge together type/effect/closure args but leave region args independent.
	--	If there is no region arg in a type then make a fresh one.
	--
	argsMerged	<- mapM (shapeArgs templateTs) mArgss

	-- Update the classes with the freshly merged types
	let cs'		= zipWith 
				(\c args -> c { classType = Just (TData v args) }) 
				cs 
				argsMerged
	
	zipWithM updateClass cids cs'
			
	-- debugging
	trace	$ "    kinds        = " % kinds			% "\n"
		% "    mArgss       = "	% mArgss		% "\n"
		% "    argsMerged   = " % argsMerged		% "\n"
		% "\n"
		
	return () 
	

-- The template isn't a TData as we were expecting.
--	This'll end up being a type error, but just merge all the classes for now
--	so unify finds the error.
--	
crushShapeMerge cids cs mts template
 = do	mergeClasses cids
 	return ()
	
	
shapeArgs aa Nothing	= synthArgs	aa
shapeArgs aa (Just bb)	= mergeArgs	aa bb
 
synthArgs :: [Type] -> SquidM [Type]
synthArgs []	
	= return []
	
synthArgs (a:as) 
	| TClass KRegion cidA	<- a
	= do	var	<- newVarN    NameRegion
		cidB	<- allocClass KRegion
		addToClass cidB (TSSynth var) (TVar KRegion var)
		
		rest	<- synthArgs as
		return	(TClass KRegion cidB : rest)
		
	| otherwise
	= do	rest	<- synthArgs as
		return	(a : rest)
		
		
mergeArgs :: [Type] -> [Type] -> SquidM [Type]
mergeArgs []	 []
	= return []
	
mergeArgs (a:as) (b:bs)
	| TClass kA cidA	<- a
	, TClass kB cidB	<- b
	= do
		cid	<- mergeClasses [cidA, cidB]
		rest	<- mergeArgs as bs
		return	(TClass kA cid : rest)
		
	| otherwise
	= do	rest	<- mergeArgs as bs
		return	(b : rest)

