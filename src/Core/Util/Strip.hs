
module Core.Util.Strip 
	( stripSchemeT
	, buildScheme
	, slurpForallsT 
	, stripToShapeT 
	, stripContextT 
	, slurpForallContextT )
where

import qualified Data.Map	as Map

import Core.Exp
import Core.Util.Bits

import qualified Shared.Var	as Var

-----
stripSchemeT	:: Type 
		-> 	( [(Bind, Kind)]
			, [(Var,  Type)]
			, [Kind] 
			, Type)
			
stripSchemeT tt 
 = stripSchemeT' [] [] [] tt
 
stripSchemeT' forallVTs tetVTs classes tt
 = case tt of
 	TForall v t tRest 
	 -> stripSchemeT' 
	 		(forallVTs ++ [(v, t)]) 
	 		tetVTs 
			classes 
			tRest

	TWhere tRest vts
	 -> stripSchemeT' 
	 		forallVTs
			(tetVTs ++ vts) 
			classes 
			tRest

	TContext  c tRest		
	 -> stripSchemeT' 
	 		forallVTs 
			tetVTs 
			(classes ++ [c])
			tRest

	_ -> let sub	= Map.fromList forallVTs
	     in ( forallVTs
	     	, tetVTs
		, classes
		, tt)



-----
buildScheme ::	[(Bind, Kind)] -> [(Var, Type)] -> [Kind] -> Type -> Type
buildScheme	forallVTs bindVTs classes shape
 = let	tC	= foldl (\s c	   -> TContext c s)  shape	$ reverse classes

	tL	= case bindVTs of
			[]	-> tC
			_	-> TWhere tC bindVTs

 	tF	= foldl (\s (v, t) -> TForall v t s) tL 	$ reverse forallVTs

   in	tF


slurpForallsT 
	:: Type -> [(Bind, Kind)]

slurpForallsT tt
 = let 	(forallVTs, _, _, _)	= stripSchemeT tt
   in	forallVTs


-- | slurp of forall bound vars and contexts from the front of this type
slurpForallContextT :: Type -> ([Type], [Kind])
slurpForallContextT tt
 = case tt of
 	TForall b t1 t2	
	 -> let	v	= varOfBind b
	    	k	= kindOfSpace $ Var.nameSpace v
		
		(vs, ks) = slurpForallContextT t2
		
	    in	( TVar k v : vs
	    	, ks)

	TContext k1 t2	
	 -> let (vs, ks) = slurpForallContextT t2
	    in	( vs
	        , k1 : ks)

	TWhere t1 vts	
	 -> slurpForallContextT t1

	_		
	 -> ([], [])


-----
-- stripToShapeT
--	Strip off TForalls, TLets and TContext of a type to get
--	the underlying shape.
--
stripToShapeT :: Type -> Type
stripToShapeT tt
 = case tt of
 	TForall  v k t		-> stripToShapeT t
	TWhere   t vts		-> stripToShapeT t
	TContext c t		-> stripToShapeT t
	_			-> tt


-- | strip context off the front of this type
stripContextT :: Type -> Type
stripContextT tt
 = case tt of
 	TContext c t		-> stripContextT t
	TWhere t vts		-> TWhere (stripContextT t) vts
	_			-> tt




