
-- | Elaborate data definitions.
module Desugar.Data 
	(elaborateData)
where

import Type.Util.Elaborate
import Type.Exp

import Desugar.Pretty
import Desugar.Exp

import Shared.Pretty
import Shared.Exp
import Shared.Base
import Shared.VarPrim
import Shared.Var		(Var, NameSpace(..))
import qualified Shared.Var	as Var

import Util
import qualified Data.Set	as Set

import qualified Debug.Trace

-----
debug		= False
trace s xx	= if debug then Debug.Trace.trace (pprStrPlain s) xx else xx

-----
elaborateData 
	:: Monad m
	=> (NameSpace 	-> m Var)
	-> (Var		-> m Kind)
	-> Top SourcePos -> m (Top SourcePos)

elaborateData newVarN getKind 
	p@(PData sp vData vsData ctors)
 = do
	trace 	( "elaborateData\n"
		% "    in:\n" %> stripAnnot p	% "\n")
		$ return ()

	let ?newVar	= newVarN
	let ?getKind	= getKind

	-- work out what var to use as the primary region.
	let takePrimary
		| Just v	<- takeHead vsData
		, Var.nameSpace v == NameRegion
		= return v
		
		| otherwise
		= do	r	<- newVarN NameRegion
			return r
		
	rPrimary	<- takePrimary

	-- use the primary region for all region annots
	let newVarNR n 
		| n == NameRegion
		= return rPrimary
		
		| otherwise
		= newVarN n

	let thing
		-- unboxed types and TUnit don't need a primary region
		| vData == primTUnit || Set.member vData primTVarsUnboxed
		= return p
	
		-- boxed ones do
		| otherwise
		= do	(ctors', vksNew)	
					<- liftM unzip $ mapM (elaborateCtor newVarNR getKind) ctors
			let vsData'	= nub $ rPrimary : vsData ++ (map fst $ concat vksNew)

			return $ PData sp vData vsData' ctors'

	p'	<- thing

	trace	( "    out:\n"	%> stripAnnot p'	% "\n")
		$ return ()
		
	return p'


elaborateCtor 
	:: Monad m
	=> (NameSpace -> m Var)		-- a fn to generate a new region var
	-> (Var -> m Kind)		-- a fn to get the kind of a data type
	-> (CtorDef SourcePos)
	-> m 	( CtorDef SourcePos
		, [(Var, Kind)] )

elaborateCtor newVar getKind (CtorDef sp var fields)
 = do	(fields', vksNew)
 		<- liftM unzip 
		$ mapM (elaborateField newVar getKind) fields

 	return	( CtorDef sp var fields'
		, concat vksNew)

elaborateField newVar getKind field@(DataField { dType = t })
 = do	
 	(t_elab, vks)	
 		<- elaborateRsT newVar getKind t

 	return	( field { dType = t_elab }
		, vks )
	
