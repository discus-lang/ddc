
module Core.Lift.Base
	( LiftS(..)
	, LiftM
	, initLiftS
	, bindType
	, getType
	, getKind
	, newVar
	, addChopped
	, getChopped)

where

-----
import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

-----
import Util

import qualified Shared.Var	as Var
import Shared.Var		(Var, VarBind, NameSpace(..))
import Shared.Error
import qualified Shared.Unique	as Unique

import Core.Exp
import Core.Util

stage	= "Core.Lift.Base"

-----
data LiftS
	= LiftS
	{ stateVarGen		:: VarBind

	, stateTypes		:: Map Var Type

	-- | Vars defined at top level,
	--	grows as new supers are lifted out.
	, stateTopVars		:: Set Var			


	-- | A list of bindings chopped out on this pass
	--	old name, new (top level) name, expression
	--
	, stateChopped		:: [(Var, Var, Top)] 		
								
	}	
								
	
	
type LiftM
	= State LiftS	

-----
initLiftS
	= LiftS
	{ stateVarGen		= Var.XBind ("v" ++ Unique.coreLift) 0
	, stateTypes		= Map.empty
	, stateTopVars		= Set.empty
	, stateChopped		= [] 
	}



-----
bindType ::	Var -> Type -> LiftM ()
bindType	v	t
 	= modify (\s -> s 
		{ stateTypes 	= Map.insert v t (stateTypes s) })
		

getType ::	Var -> LiftM Type
getType		v
 = case Var.nameSpace v of
	NameValue	
	 -> do	t	<- liftM (fromMaybe TNil)
			$  liftM (Map.lookup v)
			$  gets stateTypes
			
		return t
	
	_ -> panic stage $ "getType: no type for " % v % " space = " % Var.nameSpace v
	

getKind ::	Var -> LiftM Kind
getKind		v
 = case Var.nameSpace v of
	NameType	-> return KData
 	NameRegion	-> return KRegion
	NameEffect	-> return KEffect
	NameClosure	-> return KClosure

	-- doh
	NameClass	-> return KNil
	

-----	
newVar ::	NameSpace -> LiftM Var
newVar	space
 = do
 	gen		<- gets stateVarGen
	let gen'	= Var.incVarBind gen
	let var		= (Var.new $ pprStr gen) 
				{ Var.bind 		= gen 
				, Var.nameSpace		= space }
	
	modify (\s -> s { stateVarGen = gen' })
	
	return	var
	
	
-----
addChopped ::	Var -> Var -> Top -> LiftM ()
addChopped	old    new    x
 	= modify (\s -> s { stateChopped =  stateChopped s ++ [(old, new, x)]})

getChopped ::	LiftM [(Var, Var, Top)]
getChopped	
 = do
 	cs	<- gets stateChopped
	modify (\s -> s { stateChopped = [] })
	
	return cs


