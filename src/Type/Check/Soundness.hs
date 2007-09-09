
module Type.Check.Soundness
(
--	checkUpdateSoundness,
--	dangerousCidsT
)

where

import Util

import qualified Shared.Var	as Var
import qualified Shared.VarBind	as Var
import Shared.VarPrim

import Type.Exp
import Type.Error
import Type.Util
import Type.Plate.Collect
import Type.State
import Type.Class

-----
stage	= "Type.Soundness"
debug	= True
trace s = when debug $ traceM s


-- checkUpdateSoundness
--	Check for update soudness problems in an already generalised scheme.
--
--	The type inferencer might find out that some scheme has mutable components
--	after it's already generalised and instantiated it several times. It's 
--	too late to apply monomorphism restrictions, but we can at least report
--	an error.
--
--	If you want the monomorphism restriction, and not the error, then supply
--	a type sig for the scheme including an appropriate mutability constraint.
--
{-
checkUpdateSoundness
	:: Var -> Type -> SquidM ()
	
checkUpdateSoundness varT t
 = do	
	let ?fsMutable
		= nub
		$ [f 	| f@(FClass v _) <- collectFetters t
			, Var.bind v == Var.FMutable]

	let dangerTs
		= nub
		$ [t	| t@(TVar v)	<- dangerT [] t]
 
 	trace	$ "*   CheckUpdate.checkUpdate " 	% t 	% "\n"
		% "    fsMutable = " % ?fsMutable		% "\n"
		% "    dangerTs  = " % dangerTs			% "\n"
		% "\n"
 	
	when (not $ isNil dangerTs)
	 $ do
	 	addErrors 
			[ErrorUpdateSoundness 
				{ eVar		= varT
				, eType		= t
				, eTypeDanger	= dangerTs }]
		
	return	()
 
-----
dangerousCidsT :: Type -> SquidM [ClassId]
dangerousCidsT tt
 = do
 	let ?fsMutable
		= nub
		$ [f 	| f@(FClass v _) <- collectFetters tt
			, Var.bind v == Var.FMutable]
 	
	let dangerTs
		= nub	$ dangerT [] tt

	let dangerCidsT
		= [cid	| TClass cid <- dangerTs]

	let dangerCidsE
		= [ cid | TEffect (EClass cid) <- dangerTs]
		
	let dangerCidsC
		= [ cid | TClosure (CClass cid) <- dangerTs]

	return (dangerCidsT ++ dangerCidsE ++ dangerCidsC)



-----
-- BUGS! Not the same as Leroy, check handling of labels on functions.
--
dangerT :: (?fsMutable	:: [Fetter])
	-> [Fetter] -> Type -> [Type]

dangerT fs tt
 = case tt of
 	TVar{}			-> []
	TClass{}		-> []

	TFun t1 t2 eff clo	
	 -> (catMap (dangerC fs)
	 	$ concat
		[cs	| FClosure c (CSum cs) <- fs
			, c == clo])

	TCon v ts
	 -> let	rsMutable = [r 	| TRegion r <- ts
	 			, elem (FClass primMutable [TRegion r]) ?fsMutable]

	    in	if isNil rsMutable
	    	  then catMap (dangerT  fs) ts
		  else catMap (collectT fs) ts
		  
	TForall vks t		-> collectT fs t
	TFetters fs' t		-> dangerT (fs' ++ fs) t
	TRegion{}		-> []
	TEffect{}		-> []
	TClosure{}		-> []
	TError{}		-> []

dangerF fs ff
 = case ff of
 	FClosure _ c		-> dangerC fs c
	_			-> []
	
dangerC fs cc
 = case cc of
 	CSum cs			-> catMap (dangerC fs) cs
	CFreeT _ t		-> dangerT fs t
	_			-> []
	
	    
----	
collectT fs tt
 = case tt of
 	TVar{}			-> [tt]
	TClass{}		-> [tt]

	TFun t1 t2 eff clo 
	 -> collectT fs t1 
	 ++ collectT fs t2 
	 ++ [TEffect eff]
	 ++ (catMap (collectC fs) 
	 	$ concat
	 	[cs 	| FClosure c (CSum cs) <- fs
	 		, c == clo])
			
	TCon v ts		-> catMap (collectT fs) ts
	TForall vks t		-> collectT fs t
	TFetters fs' t		-> collectT (fs ++ fs') t
	TRegion{}		-> []
	TEffect{}		-> [tt]
	TClosure{}		-> [tt]
	TError{}		-> []
	
collectF fs ff
 = case ff of
 	FClosure _ c		-> collectC fs c
	_			-> []
	

collectC 
	:: (?fsMutable :: [Fetter])
	-> [Fetter] -> Closure -> [Type]
collectC fs cc
 = case cc of
 	CSum cs			-> catMap (collectC fs) cs
	CFreeT _ t		-> dangerT fs t
	_			-> []
	 
	 
-}	 
	 
	 
	 
	 
	 
	 
	  	
