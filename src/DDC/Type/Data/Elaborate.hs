
module DDC.Type.Data.Elaborate
	(elaborateData)
where
import DDC.Type.Data.Base
import DDC.Type
import DDC.Var
import DDC.Main.Error
import DDC.Main.Pretty
import qualified Shared.VarPrim	as Var
import qualified Debug.Trace



stage	= "DDC.Type.Data.Elaborate"
debug		= False
trace ss xx	= if debug then Debug.Trace.trace (pprStrPlain ss) xx else xx

-- | Elaborate a data type declaration.
--   This uses the kind of each type constructor to fill in missing region variables.
--   TODO: This only works on one declaration at a time, so it won't propertly
--         elaborate recursive data types. We also want to merge kind inference with
--         the elaboration process.
--
elaborateData 
	:: Monad m
	=> (NameSpace 	-> m Var)		-- ^ Function to allocate a fresh variable.
	-> (Var		-> m Kind)		-- ^ Function to get the kind of a type variable.
	-> DataDef
	-> m DataDef

elaborateData newVarN getKind 
	dataDef@(DataDef vData vsData ctors)
 = do
	trace 	( "elaborateData\n"
		% "    in:\n" %> dataDef	% "\n")
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
{-	let newVarNR n 
		| n == NameRegion
		= return rPrimary
		
		| otherwise
		= newVarN n
-}
	let thing
		-- TUnit doesn't have a region
		| vData == Var.primTUnit 
		= return dataDef
	
		-- Unboxed types, except for String# don't have regions
		| varIsUnboxedTyConData vData
		, vData /= Var.primTString Unboxed
		= return dataDef
		
		-- boxed ones do
		| otherwise
		= do{-	(ctors', vksNew)	
					<- liftM unzip $ mapM (elaborateCtor newVarNR) ctors
			let vsData'	= nub $ rPrimary : vsData ++ (map fst $ concat vksNew)
		-}	freakout stage ("elaboration of data decls is wrong")
			 $ return $ DataDef vData vsData ctors

	p'	<- thing		
	return p'

{-
elaborateCtor 
	:: Monad m
	=> (NameSpace -> m Var)		-- a fn to generate a new region var
	-> CtorDef
	-> m 	( CtorDef
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
-}	
