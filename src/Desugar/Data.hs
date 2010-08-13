
-- | Elaborate data definitions.
module Desugar.Data 
	( elaborateData
	, elaborateTypeSynonym )
where
import Desugar.Pretty
import Desugar.Exp
import Shared.Exp
import Shared.VarPrim
import DDC.Base.SourcePos
import DDC.Base.DataFormat
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Type
import DDC.Var
import Util
import qualified Debug.Trace

stage		= "Desugar.Data"
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
		, varNameSpace v == NameRegion
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
		-- TUnit doesn't have a region
		| vData == primTUnit 
		= return p
	
		-- Unboxed types, except for String# don't have regions
		| varIsUnboxedTyConData vData
		, vData /= primTString Unboxed
		= return p

		-- boxed ones do
		| otherwise
		= do	(ctors', vksNew)	
					<- liftM unzip $ mapM (elaborateCtor newVarNR) ctors
			let vsData'	= nub $ rPrimary : vsData ++ (map fst $ concat vksNew)

			return $ PData sp vData vsData' ctors'

	p'	<- thing

	trace	( "    out:\n"	%> stripAnnot p'	% "\n")
		$ return ()
		
	return p'

elaborateTypeSynonym 
	:: Monad m
	=> (NameSpace 	-> m Var)
	-> (Var		-> m Kind)
	-> Top SourcePos -> m (Top SourcePos)

elaborateTypeSynonym newVarN getKind 
	p@(PTypeSynonym sp vData typ)
 = do
	trace 	( "elaborateTypeSynonym\n"
		% "    in:\n" %> stripAnnot p	% "\n")
		$ return ()

	let ?newVar	= newVarN
	let ?getKind	= getKind

	panic stage "Sorry don't handle PTypeSynonym yet!\n"



elaborateCtor 
	:: Monad m
	=> (NameSpace -> m Var)		-- a fn to generate a new region var
	-> (CtorDef SourcePos)
	-> m 	( CtorDef SourcePos
		, [(Var, Kind)] )

elaborateCtor newVar (CtorDef sp var fields)
 = do	(fields', vksNew)
 		<- liftM unzip 
		$ mapM (elaborateField newVar) fields

 	return	( CtorDef sp var fields'
		, concat vksNew)

elaborateField newVar field@(DataField { dType = t })
 = do	
 	(t_elab, vks)	
		<- elaborateRsT newVar t
	
 	return	( field { dType = t_elab }
		, vks )
	
