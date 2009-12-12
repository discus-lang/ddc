-- | Pack a type into standard form.
module Type.Util.PackFast
	(packType)
where

import Type.Exp
import Type.Util.Bits

import Shared.Error
import Shared.VarPrim
import Util

import Data.Map			(Map)
import Data.Set			(Set)
import qualified Data.Map	as Map
import qualified Data.Set	as Set

stage	= "Type.Util.PackFast"


-- | This gets called often by the constraint solver, so needs to be reasonably efficient.
--
--	The input type needs to be in "TConstrain" form
--	Packing does NOT crush fetters. Use crushT for that.
--
-- TODO: This is does a naieve substitution.
--	 It'd be better to use destructive update to implement the substitution, 
--	 then eat up all the IORefs in a second pass.
--
-- TODO: Either embed infinite loop errors in the type with an error node,
--	 throw an exception, or use a monad.
--
packType 
	:: Type 
	-> Type

packType tt
	= {-# SCC "packType" #-}
	  packTypeCrsSub Map.empty Set.empty tt

-----
packTypeCrsSub 
	:: Map Type Type		-- all the eq constraints to substitute
	-> Set Type			-- vars of constraints already subsituted in this context
	-> Type				-- type to pack into
	-> Type

packTypeCrsSub crsEq subbed tt
 = case tt of

	-- decend into foralls
	TForall v k t
	 -> TForall v k $ packTypeCrsSub crsEq subbed t
	
	TFetters t fs
	 -> panic stage "packType: doesn't handle TFetters"
	
	-- In a constrained type, all the equality constraints are inlined,
	--	but we keep all the "more than" and type class constraints.
	--
	TConstrain t crs@(Constraints crsEq2 crsMore2 crsOther2)
	 -> let	crsEq_all	= Map.union crsEq crsEq2
		t'		= packTypeCrsSub crsEq_all subbed t
		crsMore2'	= Map.map (packTypeCrsSub  crsEq_all subbed) crsMore2
		crsOther2'	=     map (packTypeCrsSubF crsEq_all subbed) crsOther2
		crs'		= Constraints Map.empty crsMore2' crsOther2'
	    in	addConstraints crs' t'
	
	TSum k ts
	 -> let ts'	= map (packTypeCrsSub crsEq subbed) ts
	    in	TSum k ts'
	
	TApp t1 t2
	 -> let	t1'	= packTypeCrsSub crsEq subbed t1
		t2'	= packTypeCrsSub crsEq subbed t2
	    in	TApp t1' t2'
	
	TCon{}	-> tt
	TBot{}	-> tt
	TTop{}	-> tt
	
	TEffect v ts
	 -> let ts'	= map (packTypeCrsSub crsEq subbed) ts
	    in	TEffect v ts'
	
	TFree v t
	 -> let t'	= packTypeCrsSub crsEq subbed t
	    in	TFree v t'
	
	TDanger t1 t2
	 -> let t2'	= packTypeCrsSub crsEq subbed t2
	    in	TDanger t1 t2'
	
	TVar k v
	 -> let result
		 | k == kEffect		= tt
		 | k == kClosure	= tt

		 | otherwise
		 = case Map.lookup tt crsEq of
			Just t		-> packTypeCrsSub crsEq (Set.insert tt subbed) t
			Nothing		-> tt
			
	    in 	result
	
	TClass k cid	
	 -> let result
		 | k == kEffect		= tt
		 | k == kClosure	= tt
		
		 | otherwise
		 = case Map.lookup tt crsEq of
			Just t		-> packTypeCrsSub crsEq (Set.insert tt subbed) t
			Nothing		-> tt
			
	   in	result
	
	_ -> panic stage
		$ "packType: no match for " % show tt

-----
packTypeCrsSubF
	:: Map Type Type		-- all the eq constraints to substitute
	-> Set Type			-- vars of constraints already subsituted in this context
	-> Fetter			-- fetter to pack into
	-> Fetter

packTypeCrsSubF crsEq subbed ff
 = case ff of
	FConstraint v ts
	 -> let	ts'	= map (packTypeCrsSub crsEq subbed) ts
	    in	FConstraint v ts'
	
	FProj tProj vInst t1 t2
	 -> let	t1'	= packTypeCrsSub crsEq subbed t1
		t2'	= packTypeCrsSub crsEq subbed t2
	    in	FProj tProj vInst t1' t2'
	
	_ -> panic stage
		$ "packTypeF: no match for " % show ff

	