
-- | Export information from the final state of the type constraint solver.
--
module Type.Export
	( squidExport )

where

import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Shared.Var	as Var
import Shared.Var		(Var, NameSpace(..))

import Shared.Error
import Type.Exp

import Type.Base
import Type.State
import Type.Scheme
import Type.Plug

stage	= "Type.Squid.Export"

-- | Export some stuff from the constraint solver state.
squidExport 
	:: Set Var					-- ^ The vars for the bindings we want types for.
	-> SquidM 
		( Map Var Type				-- Type schemes.
		, Map Var (InstanceInfo Type Type)	-- How each instantiation was done.
		, Map Var (Map Var Type) )		-- The substition from the "contra-variant vars are ports"
							--	rewriting process.
squidExport vsTypesPlease
 = do
	-- Export types for the requested vars.
	schemes		<- exportTypes vsTypesPlease

	-- Export the instantiation table.
	inst		<- exportInst

	-- The port table was already plugged by Scheme.generaliseType
	portTable	<- gets statePortTable

	return 	( schemes
		, inst 
		, portTable)


exportVarType :: Var -> SquidM (Maybe Type)
exportVarType v
 = do 	mEx	<- extractType v
 	case mEx of
	 Nothing	-> return Nothing
	 Just tEx
	  -> do	tPlug		<- plugClassIds [] tEx
		return $ Just tPlug
 	
exportType :: Type -> SquidM Type
exportType t
 = do	t'	<- plugClassIds [] t
 	return	t'
	
exportMaybeType :: Maybe Type -> SquidM (Maybe Type)
exportMaybeType mt
 = case mt of
 	Nothing	-> return Nothing
	Just t 
	 -> do	t'	<- exportType t
	 	return	$ Just t'

--
exportTypes :: Set Var -> SquidM (Map Var Type)
exportTypes vsTypesPlease
 = do	
 	-- these are the type that were asked for by the slurper.
 	let vsPlease	=  Set.toList vsTypesPlease
	Just ts		<- liftM sequence $ mapM exportVarType vsPlease
	return	$ Map.fromList 
		$ zip vsPlease ts
		
-----
-- | Build a map of all the instantiations
--
exportInst :: SquidM (Map Var (InstanceInfo Type Type))
exportInst 
 = do	inst	<- gets stateInst
	vts	<- mapM exportInstInfo
		$  Map.toList inst
			
	return	$ Map.fromList vts

exportInstInfo 	:: (Var, InstanceInfo Var Type)
		-> SquidM (Var, InstanceInfo Type Type)

exportInstInfo (v, ii)
 = case ii of	
 	InstanceLambda v1 v2 mt
	 -> do	mt'		<- exportMaybeType mt
	 	return		$ (v, InstanceLambda v1 v2 mt)

	InstanceLet    v1 v2 vs t
	 -> do	Just ts 	<- liftM sequence $ mapM exportVarType vs
	 	t'		<- exportType t
	 	return		$ (v, InstanceLet v1 v2 ts t')
		
	InstanceLetRec 	vUse vDef Nothing
	 -> do 	Just tDef	<- exportVarType vDef
	 	return		$ (v, InstanceLetRec vUse vDef (Just tDef))
	 
