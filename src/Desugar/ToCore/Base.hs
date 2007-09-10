
module Desugar.ToCore.Base
	( Annot
	, CoreS(..)
	, CoreM
	, initCoreS
	, newVarN
	, getType
	, getKind
	, lookupInst)
	

where

-----
import Util
import Shared.Var			(Var, VarBind, NameSpace(..))
import Shared.Error
import qualified Shared.Var		as Var

import qualified Data.Set		as Set
import Data.Set				(Set)

import qualified Data.Map		as Map
import Data.Map				(Map)

import qualified Type.Exp		as T
import qualified Type.ToCore		as T

import qualified Core.Exp		as C
import qualified Core.Util		as C
import qualified Core.Plate.Trans	as C
import qualified Core.Pack		as C

import Desugar.Project			(ProjTable)

import qualified Debug.Trace		as Debug


-----
stage	= "Desugar.ToCore.Base"

-----
type	Annot	= Maybe (T.Type, T.Effect)

data CoreS 
	= CoreS 
	{ coreSigmaTable	:: Map Var Var		-- valueVar -> typeVar
	, coreMapTypes		:: Map Var T.Type
	, coreMapInst		:: Map Var (T.InstanceInfo T.Type T.Type)
	, coreProject		:: ProjTable
	, coreGenValue		:: VarBind }
	
type CoreM 
	= State CoreS
	

initCoreS 
	= CoreS 
	{ coreSigmaTable	= Map.empty
	, coreMapTypes		= Map.empty
	, coreMapInst		= Map.empty
	, coreProject		= Map.empty
	, coreGenValue		= Var.XBind "xC" 0 }

-----
newVarN	:: NameSpace -> CoreM Var
newVarN	space
 = do
 	gen		<- gets coreGenValue
	let gen'	= Var.incVarBind gen
	modify (\s -> s { coreGenValue = gen' })
	
	return		(Var.new (pretty gen)) { Var.bind = gen, Var.nameSpace = space }


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
	  $ "getScheme: no scheme for " % vT % "\n"
	  % "  visible vars = " % Map.keys mapTypes % "\n"

	 Just t		-> return $ T.toCoreT t

-----
getKind ::	Var 	-> CoreM C.Kind
getKind		v
 = case Var.nameSpace v of
	NameType	-> return C.KData
 	NameRegion	-> return C.KRegion
	NameEffect	-> return C.KEffect
	NameClosure	-> return C.KClosure


-----
lookupInst ::	Var	-> CoreM (Maybe (T.InstanceInfo C.Type C.Type))
lookupInst	v
 = do 	mapInst	<- gets coreMapInst
	return	$ liftM toCoreInfo 
		$ Map.lookup v mapInst

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
	
	





