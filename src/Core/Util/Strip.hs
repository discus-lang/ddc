
module Core.Util.Strip 
	( buildScheme
	, stripContextT 
	, slurpForallContextT )
where
import DDC.Type


buildScheme ::	[(Bind, Kind)] -> [Fetter] -> [Kind] -> Type -> Type
buildScheme	forallVTs bindVTs classes shape
 = let	tC	= foldl (\s k	   -> TForall BNil k s)  shape	
		$ reverse classes

	tL	= case bindVTs of
			[]	-> tC
			_	-> TFetters tC bindVTs

 	tF	= foldl (\s (v, t) -> TForall v t s) tL 	
		$ reverse forallVTs

   in	tF


-- | slurp of forall bound vars and contexts from the front of this type
slurpForallContextT :: Type -> ([(Bind, Kind)], [Kind], Type)
slurpForallContextT tt
 = case tt of
	TForall BNil k1 t2	
	 -> let ( bks, ks, tBody) = slurpForallContextT t2
	    in	( bks
	        , k1 : ks
		, tBody)

 	TForall b k t2	
	 -> let	(bks, ks, tBody) = slurpForallContextT t2
	    in	( (b, k) : bks
	    	, ks
		, tBody)
		
	TConstrain t1 _
	 -> slurpForallContextT t1

	TFetters t1 fs
	 -> slurpForallContextT t1

	_		
	 -> ([], [], tt)


-- | strip context off the front of this type
stripContextT :: Type -> Type
stripContextT tt
 = case tt of
 	TForall BNil k t	-> stripContextT t
	TFetters t fs		-> TFetters (stripContextT t) fs
	_			-> tt

