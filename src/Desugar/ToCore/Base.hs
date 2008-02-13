-- | State Monad for the Desugared to Core IR transform.

module Desugar.ToCore.Base
	( Annot
	, CoreS(..)
	, CoreM
	, initCoreS
	, newVarN
	, lookupType
	, lookupAnnotT, lookupAnnotE
	, getKind
	, lookupInst)

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
import Debug.Trace

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

	  -- | the vars that were quantified during type inference (with optional :> bound)
	, coreQuantVars		:: Map Var (T.Kind, Maybe T.Type)

	  -- | table of type based projections.
	, coreProjTable		:: ProjTable

	  -- | table to resolve projections 
	  --	instantiation type var -> value var for projection function
	, coreProjResolve	:: Map Var Var

	  -- | variable generator for value vars.
	, coreGenValue		:: VarBind }
	
type CoreM 
	= State CoreS
	
initCoreS 
	= CoreS 
	{ coreSigmaTable	= Map.empty
	, coreMapTypes		= Map.empty
	, coreMapInst		= Map.empty
	, coreQuantVars		= Map.empty
	, coreProjTable		= Map.empty
	, coreProjResolve	= Map.empty
	, coreGenValue		= Var.XBind "xC" 0 }


-- | Create a fresh new variable in this namespace.
newVarN	:: NameSpace -> CoreM Var
newVarN	space
 = do
 	gen		<- gets coreGenValue
	let gen'	= Var.incVarBind gen
	modify (\s -> s { coreGenValue = gen' })
	
	return		(Var.new (pprStr gen)) { Var.bind = gen, Var.nameSpace = space }

-- | Get the type corresponding to the type of this annotation
lookupAnnotT :: Annot -> CoreM (Maybe C.Type)
lookupAnnotT (Just (T.TVar T.KData vT, _))
	= lookupType vT

-- | Get the effect corresponding to the effect of this annotation
lookupAnnotE :: Annot -> CoreM (Maybe C.Effect)
lookupAnnotE (Just (_, T.TVar T.KEffect vE))
	= lookupType vE


-- | Get the type of this variable.
lookupType :: Var -> CoreM (Maybe C.Type)
lookupType v
 = do	sigmaTable	<- gets coreSigmaTable
 
 	let res
		| Var.nameSpace v /= NameValue
		= lookupType' v
		
		| Just vT 	<- Map.lookup v sigmaTable
		= lookupType' vT
		
		| otherwise
		= freakout stage
 			("getType: no type var for value var " % v % "\n")
			$ return Nothing
			
	res
	
lookupType' vT
 = do	mapTypes	<- gets coreMapTypes

	case Map.lookup vT mapTypes of
	 Nothing		
	  -> freakout stage 
		  ( "lookupType: no scheme for " % vT % " " % Var.prettyPos vT % " - " % show vT % "\n"
		  % "  visible vars = " % Map.keys mapTypes % "\n")
		  $ return Nothing

	 Just tType
	  -> do	let cType	= T.toCoreT tType
		
{-		trace	( pretty 
			$ "lookupType: " % vT % "\n"
			% "    tType = "  % tType	% "\n"
			% "    cType = " % cType	% "\n")
			$ return ()
-}
		return $ Just $ C.flattenT cType


-- | Get the kind of this variable.
getKind :: Var 	-> CoreM C.Kind
getKind	v	
 = let	Just k	= C.kindOfSpace $ Var.nameSpace v
   in	return k


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
	

