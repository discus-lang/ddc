
module Core.Util.Strip 
	( stripSchemeT
	, buildScheme
	, slurpForallsT 
	, stripToShapeT )
where

import Data.Map			(Map)
import qualified Data.Map	as Map

import Core.Exp
import Core.Util.Substitute


-----
stripSchemeT	:: Type 
		-> 	( [(Var, Type)]
			, [(Var, Type)]
			, [Class] 
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
buildScheme ::	[(Var, Type)] -> [Class] -> Type -> Type
buildScheme	forallVTs classes shape
 = let	tC	= foldl (\s c	   -> TContext c s)  shape	$ reverse classes
--	tL	= foldl (\s (v, t) -> TLet    v t s) tC		$ reverse tetVTs
 	tF	= foldl (\s (v, t) -> TForall v t s) tC 	$ reverse forallVTs

   in	tF


slurpForallsT 
	:: Type -> [(Var, Type)]

slurpForallsT tt
 = let 	(forallVTs, _, _, _)	= stripSchemeT tt
   in	forallVTs



-----
-- stripToShapeT
--	Strip off TForalls, TLets and TContext of a type to get
--	the underlying shape.
--
stripToShapeT :: Type -> Type
stripToShapeT tt
 = case tt of
 	TForall  v k t		-> stripToShapeT t
--	TLet     v t1 t2	-> stripToShapeT t2
	TContext c t		-> stripToShapeT t
	_			-> tt
