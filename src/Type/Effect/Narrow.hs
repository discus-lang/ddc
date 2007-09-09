
module Type.Effect.Narrow
	(
--	narrowEsT, 
--	cleanCidsT
	)

where

import qualified Debug.Trace	as Debug

import Util
import Shared.Error

import Type.Exp
import Type.Util
import Type.Pretty

import Type.Plate.Trans
import Type.Plate.Collect

-----
stage	= "Type.Effect.Narrow"

-----
-- narrowEsT
--	Narrows the classIds present in effect fetters so that they only 
--	mention classes in the environment, closure, or which are serving as ports.
--
--	eg, narrowing
--	  fun 	:: Thing @r0 @e0 @t2 -(@e1)> @t2
--        	:- @e0   = {@e0}
--        	,  @e1   = {@e0, @e8, @e6, @e4, !Read @r0, @e2, @e9, @e1}
--
--	gives
--	  fun	:: Thing @r0 @e0 @t2 -(@e1)> @t2
--		:- @e1	= {@e0, !Read @r0}
--
--	The extra effect classes mentioned in the original type are variables
--	internal to the function and are not needed in the scheme. 
--
--
{-

narrowEsT :: [ClassId] -> Type -> Type
narrowEsT	  env t
 = case t of
 	TFetters fs x
	 -> let	
		portCids	= portCidsT x

		-- get closure cids.
		--	schemes are already generalised, so we can just extract all cid which appear in closure.

		fsNarrowed	= catMaybes
				$ map cleanEmptyF 
				$ map (narrowF env portCids) fs
 		
		fsCids		= [cid | Just cid <- map takeFEffClo fsNarrowed]
		tPacked		= case fsNarrowed of
					[]	-> x
					_	-> TFetters fsNarrowed x
				
	    in	tPacked
	    
	_ -> t



-----
narrowF envCids portCids
	(FClosure (CClass cidE) (CSum effs))
 =	(FClosure (CClass cidE) (CSum effs'))
 where
	noMask	= envCids ++ portCids
	effs'	= catMaybes
		$ map (\e -> case e of 
			CClass cidE'	
			 | cidE == cidE'	-> Nothing
			 | elem cidE' noMask	-> Just e
			 | otherwise		-> Nothing
			 
			_ 			-> Just e)
		$ effs

	
narrowF envCids portCids
	(FEffect (EClass cidE) (ESum effs))
 =	(FEffect (EClass cidE) (ESum effs'))
 where
	noMask	= envCids ++ portCids
	effs'	= catMaybes
		$ map (\e -> case e of 
			EClass cidE'	
			 | cidE == cidE'	-> Nothing
			 | elem cidE' noMask	-> Just e
			 | otherwise		-> Nothing
			 
			_ 			-> Just e)
		$ effs

narrowF _ _ f
 =	f


-----
cleanCidsT :: 	[ClassId] -> Type -> Type
cleanCidsT	envCids tt
 = case tt of
 	TFetters fs x	
	 -> let fsCids		= collectClassIds fs
		portCids	= portCidsT x
		endCids		= endCidsT  x
	    in	TFetters fs 	$ cleanCidsT2 envCids fsCids portCids endCids x
	
	_ -> let
		portCids	= portCidsT tt
		endCids		= endCidsT  tt
	     in	cleanCidsT2 envCids [] portCids endCids tt
	
cleanCidsT2 	envCids fsCids portCids endCids tt
 = let
	keepEnds
		= [c 	| c <- endCids
			,   elem c envCids	-- keep cids who are in the env
			 || elem c fsCids	-- keep cids who have a (non-empty) effect/closure set
			 || elem c portCids]	-- keep cids who also appear as ports.
	
	keepPorts
		= [c	| c <- portCids
			,   elem c envCids
			 || elem c fsCids
			 || elem c endCids ]
			 
	keepCids
		= nub $ keepEnds ++ keepPorts
		
   in	transformT (cleanT keepCids) tt

      
cleanT keepCids tt
 = case tt of
 	TFun t1 t2 eff clo
	 -> let	eff'	= case eff of
				EClass cidE
				 | elem cidE keepCids	-> eff
				 | otherwise		-> ENil
				_			-> eff	 
	 	
		clo'	= case clo of
				CClass cidC
				 | elem cidC keepCids	-> clo
				 | otherwise		-> CNil
				_			-> clo
				
	     in	TFun t1 t2 eff' clo'
	     
	_	-> tt
	
	
-----
-- portCidsT
--	Port cids are the effect and closure cids via which useful information
--	is passed into the type scheme.
--
--	These are the cids which appear contra-variantly in the type, ie to the
--	left of a function arrow.
--
portCidsT ::	Type -> [ClassId]
portCidsT    t
 = case t of
 	TFun	t1 t2 eff clo	-> portCidsC t1 ++ portCidsT t2
	TCon	v ts		-> catMap portCidsT ts
	_			-> []
	 
portCidsC t
 = case t of
 	TFun	t1 t2 eff clo	
	 -> 	[cid | EClass cid <- [eff]]
	 ++	[cid | CClass cid <- [clo]]
	 ++ portCidsC t1 
	 ++ portCidsC t2

	TCon	v ts		-> catMap portCidsC ts
	TEffect  (EClass cid)	-> [cid]
	TClosure (CClass cid)	-> [cid]
	_			-> []


endCidsT ::	Type -> [ClassId]
endCidsT    	t
 = case t of
	TFun t1 t2 eff clo	
	 -> [cid | EClass cid <- [eff]]
	 ++ [cid | CClass cid <- [clo]]
	 ++ endCidsT t2
			
	TCon v ts		-> catMap endCidsT ts
	TEffect  (EClass cid)	-> [cid]
	TClosure (CClass cid)	-> [cid]
	_			-> []
	

-----
cleanEmptyF ::	Fetter -> Maybe Fetter
cleanEmptyF	f
 = case f of
 	FEffect _  ENil		-> Nothing
	FEffect _  (ESum [])	-> Nothing
	
	FClosure _ CNil		-> Nothing
	FClosure _ (CSum [])	-> Nothing
	
	_			-> Just f

-----
takeFEffClo ::	Fetter -> Maybe ClassId
takeFEffClo	f
 = case f of
 	FEffect  (EClass cid) _	-> Just cid
	FClosure (CClass cid) _	-> Just cid
	_			-> Nothing
 	
-}
