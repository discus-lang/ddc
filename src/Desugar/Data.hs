
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
	let mPrimary
		| Just v	<- takeHead vsData
		, Var.nameSpace v == NameRegion
		= Just v
		
		| otherwise
		= Nothing

	let thing
		-- unboxed types and TUnit don't need a primary region
		| vData == primTUnit || Set.member vData primTVarsUnboxed
		= return p
		
		-- if the data type already has a primary region then use that
		| Just v	<- mPrimary
		= do	(ctors', vksNew)	
					<- liftM unzip $ mapM (elaborateCtor (return v) getKind) ctors
			let vsData'	= nub $ v : vsData ++ (map fst $ concat vksNew)

			return $ PData sp vData vsData' ctors'
		
		-- otherwise make a new one
		| otherwise
		= do	v	<- newVarN NameRegion
			(ctors', vksNew)	
					<- liftM unzip $ mapM (elaborateCtor (return v) getKind) ctors
			let vsData'	= nub $ v : vsData ++ (map fst $ concat vksNew)

			return $ PData sp vData vsData' ctors'

	p'	<- thing

	trace	( "    out:\n"	%> stripAnnot p'	% "\n")
		$ return ()
		
	return p'


elaborateCtor 
	:: Monad m
	=> (m Var)			-- a fn to generate a new region var
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
	
