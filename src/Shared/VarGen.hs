{-# OPTIONS -O2 #-}

-- | A state monad to generate fresh variable names.
module Shared.VarGen
	( VarGen, VarGenM
	, newVarN
	, newVarN_named
	, newVarId
	, newVarsN
	, evalVarGen
	, uniquifyV)
where
import Shared.Var
import qualified Shared.Var 	as Var
import Control.Monad.State.Strict

-----
type VarGenM 	= State VarGen
type VarGen	= Var.VarId


-- | Create a fresh variable id
newVarId :: State VarGen VarId
newVarId
 = do	VarId prefix num	<- get
	put 	$ VarId prefix (num + 1)
	return 	$ VarId prefix num	


-- | Create a fresh variable
newVarN :: NameSpace -> State VarGen Var
newVarN space
 = do	vid@ (VarId prefix num)	
		<- newVarId

	let name	= prefix ++ (show num)
	let var		= (Var.new name) 
			{ Var.varId = vid
			, Var.nameSpace = space }
	return var

-- | Create a fresh variable with the given name.
newVarN_named :: NameSpace -> String	-> VarGenM Var
newVarN_named space str
 = do	var	<- newVarN space
	return	var { Var.name = Var.name var ++ "_" ++ str }


-- | Create some new variables.
newVarsN :: Var.NameSpace -> Int -> State VarGen [Var]
newVarsN space count 
 	= replicateM count (newVarN space)


-- | Rewrite the bind on this variable to a fresh one,
--	keeping the rest of the info associated with the var.
uniquifyV :: Var -> State VarGen Var
uniquifyV v
 = do	vid	<- newVarId
	return	$ v { Var.varId = vid }


-- | Evaluate the monad, using this unique string to name vars after.
evalVarGen :: State VarGen a -> String -> a
evalVarGen comp prefix	
	= evalState comp (VarId prefix 0)
	

