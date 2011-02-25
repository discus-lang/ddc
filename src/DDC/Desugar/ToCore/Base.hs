{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | State Monad for the Desugared to Core IR transform.
module DDC.Desugar.ToCore.Base
	( Annot
	, CoreM
	, CoreS(..)
	, initCoreS
	, newVarN
	, lookupType
	, lookupAnnotT)
where
import Util
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Solve.Interface.Solution
import DDC.Type
import DDC.Var
import DDC.Desugar.Project		(ProjTable)
import qualified Shared.VarUtil		as Var
import qualified Data.Map		as Map
import qualified Type.ToCore		as T
import qualified Debug.Trace

stage		= "DDC.Desugar.ToCore.Base"
debug		= False
trace ss x	= if debug then Debug.Trace.trace (pprStrPlain ss) x else x

-- | Each node in the desugared tree is annotated with its value type and effect variable.
type	Annot	= Maybe (Type, Effect)

-- | State monad used when converting to core.
type CoreM = State CoreS

-- | The state for the Desugared to Core IR transform.
data CoreS 
	= CoreS 
	{ -- | Value var to type var mapping
	  coreSigmaTable	:: Map Var Var

	  -- | type var to type mapping
	, coreMapTypes		:: Map Var Type

	  -- | how each variable was instantiated
	, coreMapInst		:: Map Var (InstanceInfo Type)

	  -- | table of type based projections.
	, coreProjTable		:: ProjTable

	  -- | table to resolve projections 
	  --	instantiation type var -> value var for projection function
	, coreProjResolve	:: Map Var Var

	  -- | variable generator for value vars.
	, coreGenValue		:: VarId }


initCoreS 
	= CoreS 
	{ coreSigmaTable	= Map.empty
	, coreMapTypes		= Map.empty
	, coreMapInst		= Map.empty
	, coreProjTable		= Map.empty
	, coreProjResolve	= Map.empty
	, coreGenValue		= VarId "xC" 0 }


-- | Create a fresh new variable in this namespace.
newVarN	:: NameSpace -> CoreM Var
newVarN	space
 = do 	gen		<- gets coreGenValue
	let gen'	= incVarId gen
	modify (\s -> s { coreGenValue = gen' })
	
	return	(varWithName (pprStrPlain gen)) 
		{ varId	 = gen
		, varNameSpace	 = space }


-- | Get the type corresponding to the type of this annotation
lookupAnnotT :: Annot -> CoreM (Maybe Type)
lookupAnnotT (Just (TVar kV (UVar vT), _))
	| kV	== kValue
	= lookupType vT

lookupAnnotT tt
	= panic stage
	$ "lookupAnnotT: no match for " % tt


-- | Get the type of this variable.
lookupType :: Var -> CoreM (Maybe Type)
lookupType v
 = do	sigmaTable	<- gets coreSigmaTable
 	let (res :: CoreM (Maybe Type))
		| varNameSpace v /= NameValue
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
	  -> do	
		let cType	= T.toCoreT tType
		let cType_flat	= flattenT cType
		return	$ trace (vcat	
				[ ppr "lookupType"
				, ppr "    source type:\n" 	%> tType
				, ppr "    core type:\n"	%> cType
				, ppr "    flat core type:\n" 	%> cType_flat
				, blank])
			$ Just cType_flat
