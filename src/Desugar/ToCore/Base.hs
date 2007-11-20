-- | State Monad for the Desugared to Core IR transform.

module Desugar.ToCore.Base
	( Annot
	, CoreS(..)
	, CoreM
	, initCoreS
	, newVarN
	, getType
	, getKind
	, lookupInst
	, withPortSub )

where

-----
import Util
import Shared.Var			(Var, VarBind, NameSpace(..))
import Shared.Error
import qualified Shared.Var		as Var
import qualified Shared.VarUtil		as Var

import qualified Data.Map		as Map
import Data.Map				(Map)

import qualified Data.Set		as Set
import Data.Set				(Set)

import qualified Type.Exp		as T
import qualified Type.ToCore		as T

import qualified Core.Exp		as C
import qualified Core.Util		as C

import Desugar.Project			(ProjTable)

-----
stage	= "Desugar.ToCore.Base"

-----
type	Annot	= Maybe (T.Type, T.Effect)

-- | The state for the Desugared to Core IR transform.
data CoreS 
	= CoreS 
	{ -- | Value var to type var mapping
	  coreSigmaTable	:: Map Var Var

	  -- | type var to type mapping
	, coreMapTypes		:: Map Var T.Type

	  -- | how each variable was instantiated
	, coreMapInst		:: Map Var (T.InstanceInfo T.Type T.Type)

	  -- | a set of all the vars which are ports
	, corePortVars		:: Set Var

	  -- | the substitution from the contra-variant port rewrite.
	, corePortTable		:: Map Var (Map Var C.Type)

	  -- | what port substitution we're currently using.
	  --	This changes when we enter a let-binding.
	, corePortSub		:: Maybe (Map Var C.Type)

	  -- | table of type based projections.
	, coreProject		:: ProjTable

	  -- | variable generator for value vars.
	, coreGenValue		:: VarBind }
	
type CoreM 
	= State CoreS
	
initCoreS 
	= CoreS 
	{ coreSigmaTable	= Map.empty
	, coreMapTypes		= Map.empty
	, coreMapInst		= Map.empty
	, corePortVars		= Set.empty
	, corePortTable		= Map.empty
	, corePortSub		= Nothing
	, coreProject		= Map.empty
	, coreGenValue		= Var.XBind "xC" 0 }


-- | Create a fresh new variable in this namespace.
newVarN	:: NameSpace -> CoreM Var
newVarN	space
 = do
 	gen		<- gets coreGenValue
	let gen'	= Var.incVarBind gen
	modify (\s -> s { coreGenValue = gen' })
	
	return		(Var.new (pretty gen)) { Var.bind = gen, Var.nameSpace = space }

-- | Get the type of this variable.
getType :: Var -> CoreM C.Type
getType	v
	| Var.nameSpace v == NameValue
	= do	sigmaTable	<- gets coreSigmaTable
	 	let Just vT	=  Map.lookup v sigmaTable
		getType' vT
		
	| otherwise
	=	getType' v
	
getType' vT
 = do	mapTypes	<- gets coreMapTypes

	case Map.lookup vT mapTypes of
	 Nothing		
	  -> panic stage 
	  $ "getType: no scheme for " % vT % " " % Var.prettyPos vT % " - " % show vT % "\n"
	  % "  visible vars = " % Map.keys mapTypes % "\n"

	 Just t	
	  -> applyPortSub $ T.toCoreT t


-- | apply the current port substitution to this type
applyPortSub :: C.Type -> CoreM C.Type
applyPortSub tt
 = do	mPortSub	<- gets corePortSub
 	case mPortSub of
	 Nothing	-> return tt
	 Just portSub	-> return $ C.substituteT portSub tt




-- | Get the kind of this variable.
getKind :: Var 	-> CoreM C.Kind
getKind	v	= return $ C.kindOfSpace (Var.nameSpace v)


-- | Lookup how the type scheme for this variable was instantiate.
lookupInst ::	Var	-> CoreM (Maybe (T.InstanceInfo C.Type C.Type))
lookupInst	v
 = do 	mapInst	<- gets coreMapInst
	return	$ liftM toCoreInfo 
		$ Map.lookup v mapInst


-- | Convert the types in an InstanceInfo from source to core representation.
toCoreInfo 
	:: T.InstanceInfo T.Type T.Type
	-> T.InstanceInfo C.Type C.Type

toCoreInfo ii
 = case ii of
 	T.InstanceLambda v1 v2 mt	
	 -> T.InstanceLambda v1 v2 (liftM T.toCoreT mt)
	
	T.InstanceLet    v1 v2 ts t
	 -> T.InstanceLet v1 v2 (map T.toCoreT ts) (T.toCoreT t)
	
	T.InstanceLetRec v1 v2 mt
	 -> T.InstanceLetRec v1 v2 (liftM T.toCoreT mt)
	

-- | Do this action while using the port substitution that we got
--	when this var was generalised.
withPortSub
	:: Var -> CoreM a -> CoreM a

withPortSub var f
 = do	
	-- remember the current substitution
 	oldSub		<- gets corePortSub

	-- set the substitition associated with the var
 	portTable	<- gets corePortTable
--	let Just thisSub = 
--		Map.lookup var portTable

	modify $ \s -> s { corePortSub = Map.lookup var portTable }
	
	-- run the action
	x	<- f
	
	-- restore the old substitution
	modify $ \s -> s { corePortSub = oldSub }
	
	return x
	
	
	
	
