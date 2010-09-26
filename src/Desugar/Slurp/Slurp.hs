{-# OPTIONS -fno-warn-missing-fields #-}
{-# OPTIONS -fwarn-incomplete-patterns #-}
-- | Slurp out type constraints from the desugared IR.
module Desugar.Slurp.Slurp
	(slurpTree)
where
import Util
import Constraint.Exp
import Constraint.Bits
import Constraint.Simplify
import Desugar.Slurp.Base
import Desugar.Slurp.SlurpS
import DDC.Solve.Location
import DDC.Solve.Interface.Problem
import DDC.Var
import DDC.Type			()
import DDC.Type.Data
import qualified Data.Map	as Map
import qualified Data.Set	as Set

stage	= "Desugar.Slurp.Slurp"

-- | Slurp out a type inferencer problem from this tree.
--   The problem carries type constraints and other information from the program
--   that we'll need to solve them.
slurpTree 
	:: Bool 		-- ^ Whether to require the main fn to have type () -> ()
	-> Tree Annot1		-- ^ Desugared header
	-> Tree Annot1 		-- ^ Desugared tree.
	-> (  Tree Annot2	--   Desugared tree   with type and effect annots.
	    , Problem		--   Problem for type constraints solver.
	    , [Error])		--   Errors found when slurping constraints.

slurpTree blessMain hTree sTree
 = let
	state	= initCSlurpS

	-- slurp constraints from this module
	((_ , hConstraints, vsTopHeader), state1)
		= runState (slurpTreeM hTree) state

	((sTree', sConstraints, vsTopSource), state2)
		= runState (slurpTreeM sTree) state1
		
	-- problem for the type constraint solver
   	problem
		= Problem
		{ problemDefs			= Map.empty	-- TODO: add defs
		, problemDataDefs		= Map.fromList [(dataDefName def, def) | PData _ def <- hTree ++ sTree ]
		, problemSigs			= Map.empty	-- TODO: add sigs
		, problemProjDicts		= Map.empty	-- TODO: add proj dicts
		, problemClassInst		= Map.empty	-- TODO: add class instances
		, problemValueToTypeVars	= stateVarType state2
		, problemTopLevelTypeVars	= Set.union vsTopHeader vsTopSource
		, problemMainIsMain		= blessMain
		, problemConstraints		= simplify (stateTypesRequest state2) (hConstraints ++ sConstraints)
		, problemTypeVarsPlease		= stateTypesRequest state2 }

   in	(sTree', problem, stateErrors state2)


slurpTreeM 
	:: Tree Annot1
	-> CSlurpM 
		( Tree Annot2	-- the tree annotated with TREC variables linking it
				--	with the constraints.
		, [CTree]
		, Set Var)	-- vars bound at top level

slurpTreeM tree
 = do
	-- sort the top level things so that data definitions go through before their uses.
	let psSorted	
		= partitionFsSort
			[ (=@=) PRegion{}, 	(=@=) PData{}
			, (=@=) PSuperSig{},	(=@=) PClassDecl{}
			, (=@=) PImport{},	(=@=) PExtern{}
			, (=@=) PProjDict{},	(=@=) PClassInst{}
			, (=@=) PTypeSig{}
			, (=@=) PBind{} ]
			tree

	-- Slurp out type constraints from the tree.
	(tree', qss)	<- liftM unzip $ mapM slurpP psSorted
	let qs		= concat qss
	
	-- pack all the bindings together.
	let (qsBranch, qsRest)
		= partition isCBranch qs

	let vsLet	= concat
			$ map (\b -> case branchBind b of
				BLet vs	-> vs
				_	-> [])
			$ qsBranch

	let qsFinal_let	= [CBranch 
				{ branchBind 	= BLetGroup vsLet
				, branchSub	= qsBranch }]
				
	-- Sort the constraints into an order acceptable by the solver.
	let qsFinal_rest
		= partitionFsSort
			[ (=@=) CProject{}, (=@=) CClassInst{}
			, (=@=) CDef{}
			, (=@=) CSig{} ]
			qsRest
			
	let qsFinal = qsFinal_rest ++ qsFinal_let
	
	return	( tree'
	 	, qsFinal
		, Set.fromList vsLet)
		
		
-- Top --------------------------------------------------------------------------------------------
-- | Slurp out type constraints from a top level thing.
slurpP 	:: Top Annot1	
	-> CSlurpM (Top Annot2, [CTree])

slurpP	(PImport sp ms)
 =	return	( PImport Nothing ms
		, [])

slurpP	(PExtern sp v tv to) 
 = do
	vT		<- lbindVtoT v
	let qs	= 
		[CDef (TSV $ SVSigExtern sp v) vT tv]
	
	return	( PExtern Nothing v tv to
		, qs)

slurpP	(PRegion sp v)
 =	return 	( PRegion Nothing v
		, [])

slurpP	(PKindSig sp v k)
   =	return	( PKindSig Nothing v k
		, [])
 
slurpP	(PSuperSig sp v k)
 =	return	( PSuperSig Nothing v k
		, [])

slurpP top@(PClassDecl sp vClass tsParam sigs)
 = do 	
	-- create a signature from each of the bindings in the class definition
	-- eg: for something like
	--
	--	class Num a where
	--	 (+) :: a -> a -> a
	--
	-- the actual type of (+) is
	--
	-- 	(+) :: forall a. Num a => a -> a -> a
	--
	let makeDef (vSig, tSig)
	     	= do 	vT		<- lbindVtoT vSig

			-- add a forall for each of the parameters of the type class
	     		let bksParam	= map (\(TVar k (UVar v)) -> (BVar v, k)) tsParam

			-- add the enclosing class constraint
			let tSig'	= makeTForall_front bksParam
					$ pushConstraintsOther [FConstraint vClass tsParam] tSig

			return $ CDef 	(TSV $ SVSigClass sp vClass) vT tSig'

	qs <- mapM makeDef sigs

	return	( PClassDecl Nothing vClass tsParam sigs
		, qs)
		
slurpP top@(PClassInst sp v ts ss)
 = do	
	-- All the RHS of the statements are vars, so we don't get any useful constraints back
	(_, _, _, ss', _)
			<- liftM unzip5
			$  mapM slurpS ss


	return	( PClassInst Nothing v ts ss'
		, [ CClassInst (TSM $ SMClassInst sp v) v ts ] )

slurpP	(PTypeSig sp sigMode vs tSig) 
 = do	tVars		<- mapM lbindVtoT vs

	let qs	= 
		[CSig (TSV $ SVSig sp v) tVar tSig
			| v 	<- vs
			| tVar	<- tVars ]

 	return	( PTypeSig Nothing sigMode vs tSig
		, qs)

slurpP x@(PTypeSynonym sp v t)
 = 	panic stage $ "Oops, we don't handle PTypeSynonym yet!"

slurpP p@(PData sp dataDef)
 = do	modify 	$ addDataDefToState dataDef
	return 	( PData Nothing dataDef
		, [])

			
slurpP	(PProjDict sp t ss)
 = do 	let projVars	= [ (vField, vImpl)
				| SBind _ (Just vField) (XVar _ vImpl) <-  ss]

	-- All the RHS of the statements are vars, so we don't get any useful constraints back
	(_, _, _, ss', _)
			<- liftM unzip5
			$  mapM slurpS ss

	return	( PProjDict Nothing t ss'
		, [CDictProject (TSM $ SMProjDict sp) t (Map.fromList projVars)] )
	
slurpP (PBind sp v x)
 = do	(vT, eff, clo, stmt', qs)
			<- slurpS (SBind sp (Just v) x)

	-- If the stmt binds a var then we want the type for it from the solver.
	(case bindingVarOfStmt stmt' of
		Just v	
		 -> do	vT_bound	<- getVtoT v
		 	wantTypeV vT_bound
			return ()
		
		Nothing	-> return ())
			
	let (SBind _ (Just v') x')
			= stmt'
	
	return	( PBind Nothing v' x'
		, qs )

					
