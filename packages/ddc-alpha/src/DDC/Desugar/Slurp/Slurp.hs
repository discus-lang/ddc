{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# OPTIONS -fno-warn-missing-fields #-}
-- | Slurping type constraints from the Desugared IR.
module DDC.Desugar.Slurp.Slurp
	(slurpTree)
where
import DDC.Desugar.Slurp.Base
import DDC.Desugar.Slurp.SlurpS
import DDC.Solve.Interface.Problem
import DDC.Var
import DDC.Type			()
import DDC.Type.Data
import Source.Desugar		(Annot, spOfAnnot)
import Control.DeepSeq
import Util
import qualified Data.MapUtil	as Map
import qualified Data.Set	as Set
import qualified Data.Bag	as Bag
import Data.Bag			(Bag)

stage	= "DDC.Desugar.Slurp.Slurp"

-- | Slurp out a type inferencer problem from this tree.
--   The problem carries type constraints and other information from the program
--   that we'll need to solve them.
slurpTree
	:: Bool 		-- ^ Whether to require the main fn to have type () -> ()
	-> Tree Annot		-- ^ Desugared header
	-> Tree Annot 		-- ^ Desugared tree.
	-> (  Tree Annot2	--   Desugared tree   with type and effect annots.
	    , Problem		--   Problem for type constraints solver.
	    , [Error])		--   Errors found when slurping constraints.

slurpTree blessMain hTree sTree
 = let
	tree	= hTree ++ sTree
	state0	= initCSlurpS

	-- TODO: doing thing lbindVtoT thing is a pain in the ass.
	--       It would be nice to have a constructor in Bound that takes the type of a value
	--	 variable and changes it into the corresponding type variable... but then we can't
	--       add that variable to sets of other type varibles.
	(defMap, state1)
	 = runState
	    (do
		-- convert external type definitions and method types.
		let defsExtern	= [ProbDef v (spOfAnnot annot) typ
					| PExtern annot v typ _ 	<- tree]

		let defsMethod	= concat
				$ [map (\(v, t) -> ProbDef v (spOfAnnot annot) (makeMethodType vClass tsParam v t)) sigs
					| PClassDecl annot vClass tsParam sigs <- tree]

		let defs	= defsExtern ++ defsMethod

		-- Built the map of type definitions.
		defMap'		<- liftM Map.fromList
				$ mapM (\def@(ProbDef v _ _) -> do
					TVar _ (UVar vT) <- lbindVtoT v
					return	(vT, def))
				$ defs

		return	defMap')
	    state0


	-- slurp constraints from this module
	((_ , hConstraints, vsTopHeader), state2)
		= runState (slurpTreeM hTree) state1

	((sTree', sConstraints, vsTopSource), state3)
		= runState (slurpTreeM sTree) state2

	-- problem for the type constraint solver
   	problem
		= Problem
		{ problemDefs		   = defMap
		, problemDataDefs	   = Map.fromList [(dataDefName def, def) | PData _ def <- tree ]
		, problemSigs		   = stateSlurpSigs      state3
		, problemProjDicts	   = stateSlurpProjDict  state3
		, problemClassInst	   = stateSlurpClassInst state3
		, problemValueToTypeVars   = stateVarType        state3
		, problemTopLevelTypeVars  = Set.union vsTopHeader vsTopSource
		, problemMainIsMain	   = blessMain
		, problemConstraints	   = CBranch BNothing $ strict $ Bag.toList (hConstraints >< sConstraints)
		, problemTypeVarsPlease	   = stateTypesRequest   state3 }

   in	(sTree', problem, stateErrors state3)


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
-- TODO: Put this somewhere else.
-- TODO: Check the quantified variables aren't already there.
--
--
makeMethodType vClass tsParam _ tSig
 = 	-- add a forall for each of the parameters of the type class
   let	bksParam	= map (\t -> let TVar k (UVar v) = t
 				     in (BVar v, k)) tsParam

	-- add the enclosing class constraint
   in	makeTForall_front bksParam
		$ pushConstraintsOther [FConstraint vClass tsParam] tSig


slurpTreeM
	:: Tree Annot
	-> CSlurpM
		( Tree Annot2	-- the tree annotated with TREC variables linking it
				--	with the constraints.
		, Bag CTree
		, Set Var)	-- vars bound at top level

slurpTreeM tree
 = do
	-- sort the top level things so that data definitions go through before their uses.
	let psSorted
		= partitionBySort
			[ (=@=) PRegion{}, 	(=@=) PData{}
			, (=@=) PSuperSig{},	(=@=) PClassDecl{}
			, (=@=) PImport{},	(=@=) PExtern{}
			, (=@=) PProjDict{},	(=@=) PClassInst{}
			, (=@=) PTypeSig{}
			, (=@=) PBind{} ]
			tree

	-- Slurp out type constraints from the tree.
	(tree', qss)	<- liftM unzip $ mapM slurpP psSorted
	let qs		= Bag.concat qss

	-- pack all the bindings together.
	let (qsBranch, qsRest)
			= partition isCBranch $ Bag.toList qs

	let vsLet	= concat
			$ map (\b -> case branchBind b of
				BLet vs	-> vs
				_	-> [])
			$ qsBranch

	let qsFinal_let	= Bag.singleton
			$ CBranch
				{ branchBind 	= BLetGroup vsLet
				, branchSub	= qsBranch }

	-- Sort the constraints into an order acceptable by the solver.
	let qsFinal_rest
		= Bag.fromList
		$ partitionBySort
			[ isCProject ]
			qsRest

	return	( tree'
	 	, qsFinal_rest >< qsFinal_let
		, Set.fromList vsLet)


-- Top --------------------------------------------------------------------------------------------
-- | Slurp out type constraints from a top level thing.
slurpP 	:: Top Annot
	-> CSlurpM (Top Annot2, Bag CTree)

slurpP	(PImport _ ms)
 =	return	( PImport Nothing ms
		, Bag.empty)

slurpP	(PRegion _ v)
 =	return 	( PRegion Nothing v
		, Bag.empty)

slurpP	(PKindSig _ v k)
   =	return	( PKindSig Nothing v k
		, Bag.empty)

slurpP	(PSuperSig _ v k)
 =	return	( PSuperSig Nothing v k
		, Bag.empty)

slurpP (PClassInst annot v ts ss)
 = do	-- All the RHS of the statements are vars, so we don't get any useful constraints back
	(_, _, _, ss', _) <- liftM unzip5 $  mapM slurpS ss

	-- Add the instance to the state.
	modify $ \s -> s {
		stateSlurpClassInst
		 	= Map.unionWith (++)
 				(stateSlurpClassInst s)
			 	(Map.singleton v [ProbClassInst v (spOfAnnot annot) ts]) }

	return	( PClassInst Nothing v ts ss'
		, Bag.empty )

slurpP	(PTypeSig annot sigMode vs tSig)
 = do
	-- Add the sigs to the state.
	forM_ vs
	 $ \v -> do
		TVar _ (UVar vT) <- lbindVtoT v
		let sig	= ProbSig v (spOfAnnot annot) sigMode tSig
		modify $ \s -> s {
			stateSlurpSigs = Map.adjustWithDefault (++ [sig]) [] vT (stateSlurpSigs s) }

	return	( PTypeSig Nothing sigMode vs tSig
		, Bag.empty)

slurpP (PTypeSynonym{})
 = 	panic stage $ "Oops, we don't handle PTypeSynonym yet!"

slurpP (PData _ dataDef)
 = do	modify 	$ addDataDefToState dataDef
	return 	( PData Nothing dataDef
		, Bag.empty)


slurpP	(PProjDict annot t ss)
 = do 	let vsProjInst	= Map.fromList
			$ [ (vField, vImpl)
				| SBind _ (Just vField) (XVar _ vImpl) <-  ss]

	-- All the RHS of the statements are vars, so we don't get any useful constraints back
	(_, _, _, ss', _) <- liftM unzip5 $  mapM slurpS ss

	let Just (vCtor, _, _) = takeTData t

	modify $ \s -> s {
		stateSlurpProjDict
			= Map.unionWith (++)
				(stateSlurpProjDict s)
				(Map.singleton vCtor [ProbProjDict vCtor (spOfAnnot annot) vsProjInst]) }

	return	( PProjDict Nothing t ss'
		, Bag.empty )

slurpP (PBind annot v x)
 = do	(_, _, _, stmt', qs)
			<- slurpS (SBind annot (Just v) x)

	-- If the stmt binds a var then we want the type for it from the solver.
	(case bindingVarOfStmt stmt' of
		Just v'
		 -> do	vT_bound	<- getVtoT v'
		 	wantTypeV vT_bound
			return ()

		Nothing	-> return ())

	let (SBind _ (Just v') x')
			= stmt'

	return	( PBind Nothing v' x'
		, qs )

slurpP (PExtern _ v t tSea)
 = 	return	( PExtern Nothing v t tSea
		, Bag.empty)

slurpP (PClassDecl _ vClass tsParam tsMethods)
 = 	return	( PClassDecl Nothing vClass tsParam tsMethods
		, Bag.empty)

