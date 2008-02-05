-- | Handles crushing of shape constraints.

module Type.Crush.Shape
	( crushShape )
where

import Util
import Shared.Var	(Var, NameSpace(..))
import qualified Shared.VarBind	as Var

import Type.Location
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
	Just shapeC@ClassFetter
		{ classFetter	= f@(FConstraint v shapeTs)
		, classSource	= src }	<- lookupClass shapeCid

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

	-- See if any of the nodes already contain data constructors.
	let mData	= map (\c -> case classType c of
				Just t@(TData{})	-> Just t
				Just _			-> Nothing)
			$ mergeCs
	
	-- If we have to propagate the constraint we'll use the first constructor as a template.
	let mTemplate	= takeFirstJust mData
	trace	$ "    mData       = "	% mData		% "\n"
		% "    mTemplate    = "	% mTemplate	% "\n"
		% "\n"

	let result
		-- all of the nodes already contain data constructors.
		-- TODO: add more fetters recursively
		| and $ map isJust mData
		= do	delClass shapeCid
			return True
			
		-- none of the nodes contain data constructors, so there's no template to work from
		| Nothing	<- mTemplate
		= return False
		
		-- we've got a template
		--	we can now merge the sub-classes and remove the shape constraint.
		| Just template	<- mTemplate
		= do	crushShapeMerge f src mergeCids mergeCs mData template
			delClass shapeCid
			return True
		
	result

-- TODO: only constrain nodes that haven't already got a data constructor in them
--	otherwise we'll over-constrain the regions.
crushShapeMerge 
	:: Fetter		-- the fetter being crushed
	-> TypeSource		-- the source of the original fetter
	-> [ClassId] 		-- classIds to merge
	-> [Class] 		-- classes corresponding to each classId above.
	-> [Maybe Type] 	-- the types of the nodes, or Nothing if they're bottom.
	-> Type			-- the template type.
	-> SquidM ()

crushShapeMerge f src cids cs mts template@(TData v templateTs)
 = do 	let kinds	= map kindOfType templateTs

	-- Grab the type args from each of the available constructors.
 	let mArgss	= map (\mt -> liftM (\(TData v ts) -> ts) mt) mts

	-- Merge together type/effect/closure args but leave region args independent.
	--	If there is no region arg in a type then make a fresh one.
	--
	argsMerged	<- mapM (shapeArgs f src templateTs) mArgss

	-- Update the classes with the freshly merged types
	zipWithM
		(\c args -> addToClass (classId c) (TSI $ SICrushed src f) (TData v args))
		cs 
		argsMerged
	
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
crushShapeMerge f src cids cs mts template
 = do	mergeClasses cids
 	return ()


	
shapeArgs f src aa Nothing	= synthArgs f src aa
shapeArgs f src aa (Just bb)	= mergeArgs aa bb
 

synthArgs :: Fetter -> TypeSource -> [Type] -> SquidM [Type]
synthArgs f src []	
	= return []
	
synthArgs f src (a:as) 
	| TClass KRegion cidA	<- a
	= do	var	<- newVarN    NameRegion
		cidB	<- allocClass KRegion
		addToClass cidB (TSI $ SICrushed src f) (TVar KRegion var)
		
		rest	<- synthArgs f src as
		return	(TClass KRegion cidB : rest)
		
	| otherwise
	= do	rest	<- synthArgs f src as
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

