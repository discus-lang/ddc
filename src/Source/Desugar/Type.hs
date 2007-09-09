
module Source.Desugar.Type
	()

--	(expandT)
where

import Util
import Shared.Error
import Type.Exp
import Type.Util

-----
stage	= "Source.Desugar.Type"

-----
{-
expandT :: Monad m
	=> (?getKind	:: Var -> m Kind)
	-> Type -> m Type
	
expandT tt
 = case tt of
 	TVar v		-> return tt

	TFun t1 t2 eff clo
	 -> do	t1'	<- expandT t1
	 	t2'	<- expandT t2
		return	$ TFun t1' t2' eff clo	

	TData v ts
	 -> do	k		<- ?getKind v
	 	let Just kParts	=  takeInit $ flattenKind k
		let ?expandMe	= tt
		let ?expandMeK	= kParts
		let tsE		=  expandTs ts kParts
		ts'		<- mapM expandT tsE
		return		$ TData v ts'
	
	TForall vks t
	 -> do	t'	<- expandT t
	 	return	$ TForall vks t'
		
	TFetters fs t
	 -> do	t'	<- expandT t
	 	return	$ TFetters fs t'
		
	TRegion{}	-> return tt
	TEffect{}	-> return tt
	TClosure{}	-> return tt

	TWild k		-> return tt
	
expandTs tt kk
	| []			<- tt
	, []			<- kk
	= []
	
	-- region is present
	| t@(TWild KRegion) : ts <- tt
	, KRegion	: ks	 <- kk
	= t : expandTs ts ks

	| t@(TRegion{}) : ts	<- tt
	, KRegion       : ks	<- kk
	= t : expandTs ts ks
	
	-- region is not present
	| KRegion	: ks	<- kk
	= TWild KRegion : expandTs tt ks
	
	| (t:ts)		<- tt
	, (k:ks)		<- kk
	= t		: expandTs ts ks
	
	| otherwise
	= panic stage 	$ "expandTs: no match for\n"
			% "  tt = " % show tt	% "\n"
			% "  kk = " % show kk 	% "\n"
			% "  ex = " % show ?expandMe % "\n"
			% "  k  = " % show ?expandMeK % "\n"


-}
