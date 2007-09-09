
module Type.Crush.Shape
--	( crushShape )
	()

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

{-
-----
-- crushShape
--	Crush the shape fetter in this class.
--
--	TODO: Add more shape fetters recursively.
--
crushShape
	:: ClassId -> SquidM ()

crushShape shapeCid
 = do 	-- Grab the Shape fetter from the class and extract the list
	--	of cids to be merged.
	--
	Just shapeC	<- lookupClass shapeCid
	let fs@[TFetter (FClass v shapeTs)]
			= classQueue shapeC

	let mergeCids	= map (\(TClass cid) -> cid) shapeTs

	trace	$ "*   Crush.crushShape " 	% shapeCid 	% "\n"
		% "    fs        = "	 	% fs		% "\n"
		% "    mergeCids = "		% mergeCids 	% "\n"

 	-- Make sure that all the classes to be merged are unified.
	--	We're expecting a maximum of one constructor per class queue.
	--
 	mapM unifyClass mergeCids
 
	-- Lookup all the classes.
 	mergeCs		<- liftM (map (\(Just c) -> c)) 
 			$  mapM lookupClass mergeCids

	-- Try and extract a type template from one of the queues.
	--
	let mts		= map (\c -> case classQueue c of
				[]	-> Nothing
				[t]	-> Just t)
			$ mergeCs
	
	let template	= takeFirstJust mts

	trace	$ "    mTs        = "	% mts		% "\n"
		% "    template   = "	% template	% "\n"
		% "\n"
		
	case template of
	 -- There's no template to work from, and nothing else we can do.
	 Nothing	-> return ()

	 -- We've got a template. 
	 --	We can merge the sub classes and remove the shape constaint.
	 Just tt	
	  -> do	crushShapeMerge mergeCids mergeCs mts tt
	 	let shapeC'	= shapeC { classQueue = [] }
		updateClass shapeCid shapeC'

		unregisterClass (Var.FShape 0) shapeCid
		return ()



crushShapeMerge 
	:: [ClassId] 		-- classIds to merge
	-> [Class] 		-- classes corresponding to each classId above.
	-> [Maybe Type] 	-- Just a type from the queue, or Nothing if the queue was empty.
	-> Type			-- a representative type.
	-> SquidM ()

crushShapeMerge cids cs mts template@(TCon v templateTs)
 = do 	let kinds	= map typeToKind templateTs

	-- Grab the type args from each of the available constructors.
 	let mArgss	= map (\mt -> liftM (\(TCon v ts) -> ts) mt) mts

	-- Merge together type/effect/closure args but leave region args independent.
	--	If there is no region arg in a type then make a fresh one.
	--
	argsMerged	<- mapM (shapeArgs templateTs) mArgss

	-- Update the classes with the freshly merged types
	let cs'		= zipWith 
				(\c args -> c { classQueue = [TCon v args] }) 
				cs 
				argsMerged
	
	zipWithM updateClass cids cs'
			
	-- debugging
	trace	$ "    kinds        = " % kinds			% "\n"
		% "    mArgss       = "	% mArgss		% "\n"
		% "    argsMerged   = " % argsMerged		% "\n"
		% "\n"
		
	return () 
	

-- The template isn't a TCon as we were expecting.
--	This'll be a type error.
--	Just merge all the classes so that Unify finds the error.
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
	| TRegion (RClass cidA)	<- a
	= do	var	<- newVarN    NameRegion
		cidB	<- allocClass KRegion
		addToClass cidB (TRegion (RVar var), TSSynth var)
		
		rest	<- synthArgs as
		return	(TRegion (RClass cidB) : rest)
		
	| otherwise
	= do	rest	<- synthArgs as
		return	(a : rest)
		
		
mergeArgs :: [Type] -> [Type] -> SquidM [Type]
mergeArgs []	 []
	= return []
	
mergeArgs (a:as) (b:bs)
	| TClass cidA	<- a
	, TClass cidB	<- b
	= do
		cid	<- mergeClasses [cidA, cidB]
		rest	<- mergeArgs as bs
		return	(TClass cid : rest)
		
	| otherwise
	= do	rest	<- mergeArgs as bs
		return	(b : rest)
-}
