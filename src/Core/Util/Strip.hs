
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
			, [Fetter]
			, [Kind] 
			, Type)
			
stripSchemeT tt 
 = stripSchemeT' [] [] [] tt
 
stripSchemeT' forallVTs fsAcc classes tt
 = case tt of
 	TForall v t tRest 
	 -> stripSchemeT' 
	 		(forallVTs ++ [(v, t)]) 
	 		fsAcc
			classes 
			tRest

	TFetters tRest fs
	 -> stripSchemeT' 
	 		forallVTs
			(fsAcc ++ fs) 
			classes 
			tRest

	TContext  c tRest		
	 -> stripSchemeT' 
	 		forallVTs 
			fsAcc 
			(classes ++ [c])
			tRest

	_ ->    ( forallVTs
	     	, fsAcc
		, classes
		, tt)



-----
buildScheme ::	[(Bind, Kind)] -> [Fetter] -> [Kind] -> Type -> Type
buildScheme	forallVTs bindVTs classes shape
 = let	tC	= foldl (\s c	   -> TContext c s)  shape	$ reverse classes

	tL	= case bindVTs of
			[]	-> tC
			_	-> TFetters tC bindVTs

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
	    	Just k	= kindOfSpace $ Var.nameSpace v
		
		(vs, ks) = slurpForallContextT t2
		
	    in	( TVar k v : vs
	    	, ks)

	TContext k1 t2	
	 -> let (vs, ks) = slurpForallContextT t2
	    in	( vs
	        , k1 : ks)

	TFetters t1 fs
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
	TFetters t fs		-> stripToShapeT t
	TContext c t		-> stripToShapeT t
	_			-> tt


-- | strip context off the front of this type
stripContextT :: Type -> Type
stripContextT tt
 = case tt of
 	TContext c t		-> stripContextT t
	TFetters t fs		-> TFetters (stripContextT t) fs
	_			-> tt




