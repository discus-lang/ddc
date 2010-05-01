
module Core.Util.Strip 
	( stripSchemeT
	, buildScheme
	, slurpForallsT 
	, stripContextT 
	, slurpForallContextT )
where
import Type.Exp

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
 	TForall b k t2	
	 -> let	tBind	= case b of
	 			BVar v 		-> TVar k v
				BMore v t	-> TVarMore k v t
				
		(vs, ks) = slurpForallContextT t2
		
	    in	( tBind : vs
	    	, ks)

	TContext k1 t2	
	 -> let (vs, ks) = slurpForallContextT t2
	    in	( vs
	        , k1 : ks)

	TFetters t1 fs
	 -> slurpForallContextT t1

	_		
	 -> ([], [])


-- | strip context off the front of this type
stripContextT :: Type -> Type
stripContextT tt
 = case tt of
 	TContext c t		-> stripContextT t
	TFetters t fs		-> TFetters (stripContextT t) fs
	_			-> tt




