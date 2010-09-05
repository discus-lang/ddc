{-# OPTIONS -fno-warn-missing-fields #-}
{-# OPTIONS -fwarn-incomplete-patterns #-}
-- | Slurp out type constraints from the desugared IR.
module Desugar.Slurp.Slurp
	(slurpTreeM)
where
import Util
import Constraint.Exp
import Constraint.Bits
import Desugar.Slurp.Base
import Desugar.Slurp.SlurpS
import DDC.Solve.Location
import DDC.Var
import DDC.Type			()
import DDC.Type.Data
import qualified Data.Map	as Map
import qualified Data.Set	as Set


stage	= "Desugar.Slurp.Slurp"

-- | Slurp out type constraints from this tree.
slurpTreeM :: Tree Annot1
	-> CSlurpM 
		( Tree Annot2	-- the tree annotated with TREC variables linking it
				--	with the constraints.
		, [CTree]	-- list of type constraints
		, Set Var)	-- type vars of top level bindings
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

slurpP	(PTypeSig sp vs tSig) 
 = do	tVars		<- mapM lbindVtoT vs

	let qs	= 
		[CSig (TSV $ SVSig sp v) tVar tSig
			| v 	<- vs
			| tVar	<- tVars ]

 	return	( PTypeSig Nothing vs tSig
		, qs)

slurpP x@(PTypeSynonym sp v t)
 = 	panic stage $ "Oops, we don't handle PTypeSynonym yet!"

slurpP p@(PData sp dataDef)
 = do	modify 	$ \s -> s 
		{ stateDataDefs	= Map.insert 
					(dataDefName dataDef)
					dataDef
					(stateDataDefs s)

		, stateCtorData	= Map.union 
					(stateCtorData s)
					(Map.fromList $ zip
						(Map.keys  $ dataDefCtors dataDef)
						(repeat    $ dataDefName dataDef)) }
			 
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

					
