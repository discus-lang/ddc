
module Type.Export
	( squidExport )

where

import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Shared.Error
import Type.Exp

import Type.Base
import Type.State
import Type.Scheme
import Type.Plug

stage	= "Type.Squid.Export"


squidExport 
	:: Set Var
	-> SquidM 
		( Map Var Type
		, Map Var [Type])

squidExport vsTypesPlease
 = do
	schemes		<- exportTypes vsTypesPlease
	inst		<- exportInst

	return 	( schemes
		, inst )


exportType :: Var -> SquidM Type
exportType v
 = do 	tEx		<- extractType v
	tPlug		<- plugClassIds [] tEx
	return tPlug
 	

--
exportTypes :: Set Var -> SquidM (Map Var Type)
exportTypes vsTypesPlease
 = do	
 	-- these are the type that were asked for by the slurper.
 	let vsPlease	= Set.toList vsTypesPlease
	ts		<- mapM exportType vsPlease
	return	$ Map.fromList 
		$ zip vsPlease ts
		
-----
exportInst :: SquidM (Map Var [Type])
exportInst 
 = do	inst	<- gets stateInst
 	let vvs	=  Map.toList inst
	
	vts	<- mapM exportInst' vvs
	
	return	$ Map.fromList vts

exportInst' (v, vs)
 = do	ts'	<- mapM exportType vs
 	return	(v, ts')



