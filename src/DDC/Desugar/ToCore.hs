{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Desugar.ToCore
	( toCoreTree
	, toCoreP)
where
import Util
import DDC.Desugar.ToCore.Base
import DDC.Desugar.ToCore.Exp
import DDC.Main.Error
import DDC.Var
import Type.ToCore			(toCoreT, toCoreK)
import Type.Export			(Solution(..))
import Desugar.Pretty			()
import Desugar.Project			(ProjTable)
import qualified DDC.Type		as T
import qualified DDC.Type.Data		as T
import qualified DDC.Core.Exp 		as C
import qualified DDC.Desugar.Exp 	as D
import qualified Desugar.Slurp.Util	as D
import qualified Shared.Exp		as S
import qualified Data.Map		as Map
import qualified Data.Set		as Set

stage		= "DDC.Desugar.ToCore"

-- Tree --------------------------------------------------------------------------------------------
toCoreTree
 	:: Map Var Var		-- ^ Map value to type vars
	-> ProjTable		-- ^ projection table
	-> Solution		-- ^ solution from type constraint solver
	-> D.Tree Annot
	-> C.Tree

toCoreTree 
	mapValueToTypeVars
	projTable
	solution 
	sTree

 = let	initCoreS'
		= initCoreS
		{ coreSigmaTable	= mapValueToTypeVars
		, coreMapTypes		= solutionTypes solution
		, coreMapInst		= solutionInstanceInfo solution
		, coreProjTable		= projTable  
		, coreProjResolve	= solutionProjResolution solution}
	
	toCoreTreeM tree
		= liftM concat
		$ mapM toCoreP tree

   in	evalState (toCoreTreeM sTree) initCoreS'


-- Top ---------------------------------------------------------------------------------------------
-- | Convert a top level thing to core.
toCoreP	:: D.Top Annot	
	-> CoreM [C.Top]

toCoreP p
 = case p of
	D.PExtern _ v tv (Just to)
	 -> do	let tv'	= toCoreT tv
		let to'	= toCoreT to	
		return	[C.PExtern v tv' to']

	D.PExternData _ _ v k
	 -> 	return	[C.PExternData v k]

	D.PData _ vData vsParam ctors
	 -> do	ctors'	<- zipWithM 
				(toCoreCtorDef vData vsParam) 
				ctors
				[0 .. length ctors]
					
		let mmCtors	= Map.fromList 
				$ [(T.ctorDefName def, def) | def <- ctors']

		return	[C.PData vData mmCtors]

	D.PBind nn (Just v) x
	 -> do	
		-- We only use this set of variables for controlling what effect and
		-- closure annots to annotate the tree with, so it doesn't need to 
		-- also contain the top-level region variables.
		let vsBound	= Set.empty

		Just (C.SBind (Just v') x') 
			<- toCoreS vsBound (D.SBind nn (Just v) x)

		return	[C.PBind v' x']

	-- TODO: This isn't being used.
	D.PRegion _ v 
	 -> 	return	[C.PRegion v []]

	D.PKindSig _ v k

	 -- Abstract type constrctors have no data constructors.
	 | T.resultKind k == T.kValue
	 ->	return	[C.PData   v Map.empty]
	
	 -- An abstract effect constructor.
	 | T.resultKind k == T.kEffect
	 ->	return	[C.PEffect v (toCoreK k)]
	
	 -- We could probably add the following, but we don't have test 
	--  programs for them yet.
	 | otherwise
	 -> panic stage $ unlines
		[ "toCoreP: Type constructors that do not have a result kind"
		, "of either * or ! are not supported in the core langauge yet" ]

	D.PSuperSig _ v s
	 -> 	return	[C.PClass v s]

	D.PClassDecl _ vClass cts sigs
	 -> do	let sigs'	= [(v, toCoreT t) | (v, t) <- sigs]

		let vks'	= map (\(T.TVar k (T.UVar v')) -> (v', k))
				$ map toCoreT cts

		return	[C.PClassDict vClass vks' sigs']

	-- type class instance
	D.PClassInst _ vClass cts ss
	 -> do
		-- rewrite all the bindings in this instance
		-- The right of each binding must be a var, which should
		--	be enforced by Desugar.Project.
		let rewriteInstS s
			| D.SBind _ (Just v1) (D.XVarInst _ v2)	<- s
			= return (v1, v2)
			
			| D.SBind _ (Just v1) (D.XVar     _ v2) <- s
			= return (v1, v2)
			
			| otherwise
			= panic stage
				$  "toCoreP: RHS of projection dict must be a var\n"
				%  "    stmt was:\n" %> s % "\n"
	
		ss'	<- mapM rewriteInstS ss

		-- rewrite the context types
		let cts'	= map toCoreT cts

		return	[C.PClassInst vClass cts' ss']


	-- These don't mean anything in the core language.
	D.PProjDict{}	-> return []
	D.PImport{}	-> return []
	D.PTypeSig{}	-> return []

	_ -> panic stage
		$ "toCoreP: no match for " % show p % "\n"
	

-- CtorDef -----------------------------------------------------------------------------------------
-- | Convert a desugared data constructor definition to core form.
toCoreCtorDef	
	:: Var			-- ^ var of data type constructor the data constructor belongs to
	-> [Var]		-- ^ vars of params to data type constructor
	-> D.CtorDef Annot	-- ^ data constructor definition to convert
	-> Int			-- ^ data constructor tag
	-> CoreM T.CtorDef
		
toCoreCtorDef vData vsParam (D.CtorDef _ vCtor dataFields) tag
 = do 	tCtor	<- liftM toCoreT
		$  D.makeCtorType newVarN vData vsParam vCtor dataFields

	let fieldIndicies
		= takeFieldIndicies dataFields

	return	$ T.CtorDef vCtor tCtor (length dataFields) tag fieldIndicies


-- | For fields with a label, 
--	construct a map from the label var to the index of that field in the constructor.
takeFieldIndicies 
	:: [S.DataField (D.Exp Annot) T.Type] 	-- ^ fields of a data constructor
	-> Map Var Int

takeFieldIndicies dfs
 	= Map.fromList
 	$ [ (v, i) | (Just v, i) <- zip (map S.dLabel dfs) [0..] ]
	
