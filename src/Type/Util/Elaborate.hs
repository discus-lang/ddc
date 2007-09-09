
module Type.Util.Elaborate
	( elaborateT )

where

-----
import Util

-----
import qualified Shared.Var as Var
import Shared.Var 	(Var, NameSpace(..))
import Shared.VarPrim
import Shared.Error 

import Type.Exp
import Type.Util.Bits		
-- import Type.Effect.MaskFresh	(maskEsFreshT)

-----
stage	= "Type.Elaborate"


elaborateT 
	:: Monad m
	=> (?newVarN 	:: NameSpace 	-> m Var)
	-> (?getKind	:: Var		-> m Kind)
	-> Type -> m Type

elaborateT t = return t

{-
elaborateT t
 = do	(t', newEffs)	<- elaborateT2 t	

	let newRs	= map (\(TEffect _ [TVar r]) -> r) newEffs
	(tEffect,  newEs)	<- pushActiveEffsT t'
	(tClosure, newCs)	<- pushActiveCloT  [] tEffect

	let moreVks	=  nub
			$  map (\v 	 -> (v, KRegion))  newRs
			++ map (\v  	 -> (v, KEffect))  newEs
			++ map (\(v, cs) -> (v, KClosure)) newCs

	let moreFs	
		=  [ FEffect  (EVar v) (ESum newEffs)	
			|  v 	<- newEs ]

		++ [ FClosure (CVar v) clo		
			| (v, clo) <- newCs ]

		++ [ FClass   primMutable r
			| ECon name r <- newEffs
			, name == primWrite ]

	let tFF		= addForall  moreVks
			$ addFetters moreFs
			$ tClosure
			
	let tMasked	= maskEsFreshT tFF
	

	return		tMasked
			
			
				 

addForall vksMore t
 = case t of
 	TForall vks x	-> TForall (vks ++ vksMore) 	x
	_		-> TForall vksMore		t

addFetters fsMore t
 = case t of
 	TForall  vks x	-> TForall vks (addFetters fsMore x)
	TFetters fs  x	-> TFetters (fs ++ fsMore) x
	_		-> TFetters fsMore t


	

elaborateT2 tt
 = case tt of
	TVar v
	 ->	return (tt, [])
	 
	TFun t1 t2 eff clo
	 -> do	(t1', eff1)	<- elaborateT2 t1
	 	(t2', eff2)	<- elaborateT2 t2
		return	( TFun t1' t2' eff clo
			, eff1 ++ eff2)
		
 	TCon v ts	
	 -> do	k			<- ?getKind v
	 	let kParts		= flattenKind k
		(tsR, newRsHere)	<- elaborateRs ts kParts
		let newEffsHere		= map (\r -> ECon primRead [TRegion (RVar r)]) newRsHere

		(ts', newEffss)		<- liftM unzip
					$  mapM elaborateT2 tsR
		return 	( TCon v ts'
			, newEffsHere ++ concat newEffss)
	 
	TForall vks t
	 -> do	(t', effs)	<- elaborateT2 t
	 	return	( TForall vks t'
			, effs)
	
	TFetters fs t
	 -> do	(t', effs)	<- elaborateT2 t
	 	return	( TFetters fs t'
			, effs)
	
	TRegion{}	-> return (tt, [])
	TEffect{}	-> return (tt, [])
	TClosure{}	-> return (tt, [])
	 	 
	TMutable t
	 -> do	(t', effs)	<- elaborateT2 t
		let effs'
			= map (\e -> case e of
				ECon name r
				 | name == primRead	-> ECon primWrite r
				 | otherwise		-> e)
			$ effs
		
	 	return	( t'
			, effs')


elaborateRs tt kk

	| []			<- tt
	, []			<- kk
	= return ([], [])

	| []			<- tt
	, [KType]		<- kk
	= return ([], [])
 
	| t@(TRegion _) : ts	<- tt
	, KRegion   	: ks	<- kk
	= do	(rest, newRs)	<- elaborateRs ts ks
		return	( t : rest
			, newRs)

	| t@(TEffect _) : ts	<- tt
	, KEffect 	: ks	<- kk
	= do	(rest, newRs)	<- elaborateRs ts ks
		return	( t : rest
			, newRs)
		
	| t@(TClosure _) : ts	<- tt
	, KClosure	: ks	<- kk
	= do	(rest, newRs)	<- elaborateRs ts ks
		return	( t : rest
			, newRs)

	| _			<- tt
	, KRegion : ks		<- kk
	= do	r		<- ?newVarN NameRegion 
	 	(rest, newRs)	<- elaborateRs tt ks
		return	( TRegion (RVar r) : rest
			, r : newRs)
		
	| t : ts		<- tt
	, KType		: ks	<- kk
	= do	(rest, newRs)	<- elaborateRs ts ks
		return	( t : rest
			, newRs)
	
	| otherwise
	= panic stage 
	$ "elaborateRs: no match for " % show (tt, kk) % "\n"


-----------------------
-- pushActiveCloT
--	
--
pushActiveCloT 
	:: Monad m
	=> (?newVarN :: NameSpace -> m Var)
	-> [Closure]
	-> Type -> m (Type, [(Var, Closure)])

pushActiveCloT env tt
	| TForall vks x		<- tt
	= do	(x', cs)	<- pushActiveCloT env x
		return	( TForall vks x'
			, cs)
			
	| TFetters fs x		<- tt
	= do	(x', cs)	<- pushActiveCloT env x
		return	( TFetters fs x'
			, cs)
			
	| TVar{}		<- tt
	= 	return (tt, [])
	
	| TCon{}		<- tt
	=	return (tt, [])


	| TFun t1 t2 eff clo	<- tt
	, CNil			<- clo
	, []			<- env
	= do	
		x		<- ?newVarN NameValue
		let n		= CFreeT x t1
		(t2', cs)	<- pushActiveCloT (env ++ [n]) t2
		return	( TFun t1 t2' eff CNil
			, cs)	
	
	| TFun t1 t2 eff clo	<- tt
	, CNil			<- clo
	, not $ isNil env
	= do	v		<- ?newVarN NameClosure
		x		<- ?newVarN NameValue
		let n		= CFreeT x t1
		(t2', cs)	<- pushActiveCloT (env ++ [n]) t2

		return	( TFun t1 t2' eff (CVar v)
			, (v, CSum env) : cs)
	
	


-----------------------
-- pushActiveEffT
--	Make sure there are effect vars on active function ctors, 
--	creating them if need be.
--
pushActiveEffsT 
	:: Monad m
	=> (?newVarN :: NameSpace -> m Var)
	-> Type -> m (Type, [Var])
	
pushActiveEffsT tt
 	| TForall vks x		<- tt
	= do	(x', vs)	<- pushActiveEffsT x
	 	return	( TForall vks x'
			, vs)
		
	| TFetters fs x		<- tt
	= do	(x', vs)	<- pushActiveEffsT x
	 	return	( TFetters fs x'
			, vs)

	| TVar{}		<- tt
	= 	return	( tt, [])

	| TCon{}		<- tt
	=	return	( tt, [])
			
	| TFun t1 t2 eff clo	<- tt
	, TFun{}		<- t2
	= do	(t2', vs)	<- pushActiveEffsT t2
		return	( TFun t1 t2' eff clo
			, vs)
		
	| TFun t1 t2 eff clo	<- tt
	, EVar v		<- eff
	= 	return	( TFun t1 t2 eff clo
			, [v])
	
	| TFun t1 t2 eff clo	<- tt
	, ENil			<- eff
	= do	v	<- ?newVarN NameEffect
		return	( TFun t1 t2 (EVar v) clo
			, [v])
-}
