
module Core.Util.Strip 
	(buildScheme)
where
import DDC.Type

buildScheme ::	[(Bind, Kind)] -> [Fetter] -> [Kind] -> Type -> Type
buildScheme	forallVTs bindVTs classes shape
 = let	tC	= foldl (\s k -> TForall BNil k s)  shape	
		$ reverse classes

	tL	= case bindVTs of
			[]	-> tC
			_	-> TFetters tC bindVTs

 	tF	= foldl (\s (v, t) -> TForall v t s) tL 	
		$ reverse forallVTs

   in	tF


