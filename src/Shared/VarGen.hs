
module Shared.VarGen
(
	VarGen,
	newVar,
	newVarId,
	newVars,
	evalVarGen,

	uniquifyV
)

where

-----
import Control.Monad
import Control.Monad.State

-----
import Util.Pretty

-----
import qualified Shared.Var as Var
import Shared.Var (Var, VarBind)

-----
type VarGen	= Var.VarBind


newVarId ::	State VarGen VarBind
newVarId
 = do
 	Var.XBind prefix num	<- get
	put 	$ Var.XBind prefix (num+1)
	return 	$ Var.XBind prefix num	


newVar :: 	State VarGen Var
newVar 
 = do
	varId@ (Var.XBind prefix num)	
		<- newVarId

	let name	= prefix ++ padLc '0' 4 (show num)
	let var		= 
		(Var.new name) 
			{ Var.bind = varId }

	return var

uniquifyV ::	Var	-> State VarGen Var
uniquifyV	v
 = do
 	varId	<- newVarId
	return	$ v { Var.bind = varId }
	


newVars :: 	Int -> State VarGen [Var]
newVars 	count 
 	= replicateM count newVar
	

evalVarGen ::	State VarGen a	->	String 	-> a
evalVarGen	comp			prefix	
	= evalState comp (Var.XBind prefix 0)
	

