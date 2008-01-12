
module Core.Optimise.FreeLevel
(
	annotLevelTree
)
where

import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Shared.Error

import Core.Exp
import Core.Util
import Core.Util.Slurp

import Core.Plate.FreeVars

-----
stage	= "Core.Optimise.FreeLevel"

-----
annotLevelTree 
	:: Tree 
	-> Tree
	-> Tree

annotLevelTree cHeader pp	
 = let	topVs	= topLevelVs (cHeader ++ pp)

	vMap	= foldr (\v -> Map.insert v 0) Map.empty 
		$ Set.toList topVs

	st	= (0, vMap)
		
   in	map (annotLevelP st) pp
	

annotLevelP st@(level, vMap) pp
 = case pp of
 	PBind v x	
	 -> let (x', free)	= annotLevelX (level + 1, vMap) x
	    in	PBind v x'


	_		-> pp
	
annotLevelX st@(level, vMap) xx
 = case xx of
	XAnnot 	n x	
	 -> let	(x', free)	= annotLevelX st x
	    in	( XAnnot n x'
	    	, free)
		
	XLAM 	v k x	
	 -> let (x', free)	= annotLevelX st x
	    in	( XLAM v k x'
	    	, free)
	
	XAPP x t
	 -> let	(x', free)	= annotLevelX st x
	    in	( XAPP x' t
	    	, free)
		
 	XLam 	v t x eff clo
	 -> let	level'		= level + 1
	 	vMap'		= Map.insert v level' vMap
	 	st'		= (level', vMap')

		(x', free)	= annotLevelX st' x
	    
	    in	( XLam v t x' eff clo
	    	, (v, level') `Set.delete` free)

	XApp x1 x2 eff
	 -> let	(x1', free1)	= annotLevelX st x1
		x1E		= eraseSpineAppX x1'

	 	(x2', free2)	= annotLevelX st x2
		free		= free1 `Set.union` free2
	    in	( tagVL free 	$ XApp x1E x2' eff
	    	, free )

		
	XTet 	vts x
	 -> let	(x', free)	= annotLevelX st x
	    in	( XTet vts x'
	    	, free)
		
	XTau 	t x	
	 -> let	(x', free)	= annotLevelX st x
	    in	( XTau t x'
	    	, free)

	XType{}
	 ->	(xx, Set.empty)

	XDo     ss	
	 -> let	vMap'		= foldr (\v -> Map.insert v level) vMap
				$ catMaybes
				$ map slurpBoundVarS ss
	 
	 	st'		= (level, vMap')
	 
	 	(ss', frees)	= unzip
				$ map (annotLevelS st') ss

		boundHere	= catMap slurpBoundVarsS ss

		free		= Set.fromList
				$ [ (v, l) 	| (v, l)	<- Set.toList 
								$  Set.unions frees
						, not $ elem v boundHere]
						
	    in	( tagVL free	$ XDo ss'
	    	, free)
		
	XMatch aa
	 -> let	(aa', frees)	= unzip
	 			$ map (annotLevelA st) aa
		
		free		= Set.unions frees

	    in	( tagVL free	$ XMatch aa'
	    	, free)

	XConst{}
	 ->	( xx, Set.empty)
	 
	XVar v t
	 -> case Map.lookup v vMap of 
	 	Nothing		-> panic stage $ "annotLevelX: var " % v % " is not bound in level map.\n"
		Just ll		
		 -> 	( XVar v t
		 	, Set.singleton (v, ll))

	XLocal  v vs x	
	 -> let	(x', free)	= annotLevelX st x
	    in	( XLocal v vs x'
	    	, free)

	XPrim m xs
	 -> let	(xs', frees)	= unzip $ map (annotLevelX st) xs
		free		= Set.unions frees
	    in	( tagVL free	$ XPrim m xs'
	    	, free)

		
	_ -> panic stage
		$ "annotLevelX: no match for " % show xx % "\n"


annotLevelS st ss
 = case ss of
 	SBind mV x
	 -> let	(x', free)	= annotLevelX st x
	    in	( SBind mV x'
	    	, free)
		
annotLevelA st@(level, vMap) aa
 = case aa of
	AAlt gs x	
	 -> let	boundGs		= Set.unions $ map varsBoundByG gs
		vMap'		= foldr (\v -> Map.insert v level) vMap
				$ Set.toList boundGs

		st'		= (level, vMap')

	 	(x', free)	= annotLevelX st' x

		freeL		= Set.toList free
		freeLD		= [ f | f@(v, l) <- freeL, not $ Set.member v boundGs]

	    in	( AAlt gs x'
	    	, Set.fromList freeLD )


		 	

{-
annotLevelG st@(level, vMap) gg
 = case gg of
 	GCon x c fs
	 -> let	boundVs		= map t3_2 fs
	 	vMap'		= foldr (\v -> Map.insert v level) vMap boundVs
		st'		= (level, vMap')
		 
	 	(x', free)	= annotLevelX st' x
	 	free'		= Set.delete [(v, level) | v <- boundVs]
	    in	( GCon x' c fs
	    	, free')
-}

tagVL free x
 = let	free'	= filter (\(v, n) -> n /= 0) 
 		$ Set.toList free
   in	addXAnnot (NFreeLevel free') x


slurpBoundVarS ss
 = case ss of
 	SBind mV x	-> mV
	_		-> Nothing


topLevelVs :: Tree -> Set Var
topLevelVs tree
	= Set.fromList
	$ concat
	$ map topLevelVsP tree

topLevelVsP pp
 = case pp of	
 	PBind  		v x			-> [v]
	PExtern 	v t1 t2			-> [v]
	PCtor  	 	v t1 t2			-> [v]
	PClassDict	v ts context sigs	-> map fst sigs
	PData 		v vs ctors		-> v : [ vF	| CtorDef v fields	<- ctors
							, Just vF		<- map dLabel fields ]
	
	_					-> []



eraseSpineAppX :: Exp -> Exp
eraseSpineAppX	xx
 = case xx of
	XAnnot n x@(XApp{})
	 -> x
	 
	_ -> xx
