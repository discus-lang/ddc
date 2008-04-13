
-- | A state monad to generate fresh variable names.
module Shared.VarGen
	( VarGen, VarGenM
	, newVarN
	, newVarId
	, newVarsN
	, evalVarGen
	, uniquifyV)

where

-----
import Control.Monad.State
import qualified Shared.Var as Var
import Shared.Var (Var, VarBind)

-----
type VarGenM 	= State VarGen
type VarGen	= Var.VarBind


-- | Create a fresh variable id
newVarId :: State VarGen VarBind
newVarId
 = do	Var.XBind prefix num	<- get
	put 	$ Var.XBind prefix (num + 1)
	return 	$ Var.XBind prefix num	


-- | Create a fresh variable
newVarN :: Var.NameSpace -> State VarGen Var
newVarN space
 = do	varId@ (Var.XBind prefix num)	
		<- newVarId

	let name	= prefix ++ (show num)
	let var		= (Var.new name) 
			{ Var.bind = varId 
			, Var.nameSpace = space }
	return var


-- | Create some new variables.
newVarsN :: Var.NameSpace -> Int -> State VarGen [Var]
newVarsN space count 
 	= replicateM count (newVarN space)


-- | Rewrite the bind on this variable to a fresh one,
--	keeping the rest of the info associated with the var.
uniquifyV ::	Var	-> State VarGen Var
uniquifyV	v
 = do	varId	<- newVarId
	return	$ v { Var.bind = varId }


-- | Evaluate the monad, using this unique string to name vars after.
evalVarGen ::	State VarGen a	-> String 	-> a
evalVarGen	comp prefix	
	= evalState comp (Var.XBind prefix 0)
	

