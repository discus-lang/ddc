{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Convert CoreIR to Abstract-C
module DDC.Core.ToSea
	(toSeaGlobs)
where
import DDC.Core.ToSea.State
import DDC.Core.ToSea.Type
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Base.Prim
import DDC.Type.Predicates
import DDC.Var
import Data.Function
import Shared.VarUtil			(prettyPos)
import Data.Sequence			(Seq)
import Data.Traversable			(mapM)
import Util				hiding (mapM)
import Prelude				hiding (mapM)
import qualified Core.Util		as C
import qualified DDC.Core.ToSea.Sequence as C
import qualified DDC.Core.OpType	as C
import qualified DDC.Core.Glob		as C
import qualified DDC.Core.Exp 		as C
import qualified DDC.Type		as T
import qualified DDC.Type.Data		as T
import qualified DDC.Core.Check.Exp	as C
import qualified DDC.Sea.Exp  		as E
import qualified DDC.Sea.Pretty		as E
import qualified Data.Map		as Map
import qualified Data.Set		as Set
import qualified Data.Sequence		as Seq

stage	= "DDC.Core.ToSea"


toSeaGlobs
	:: String		-- ^ unique
	-> C.Glob 		-- ^ header glob
	-> C.Glob		-- ^ module glob
	-> Either
		[Var]					-- recursive caf variables.
		(Seq (E.Top ()), Seq (E.Top ()))	-- the converted tops

toSeaGlobs unique cgHeader cgModule
  = let
	-- Determine the order we need to initialise the CAFs in.
	-- Mutually recursive CAFs don't work.
	eCafOrder	= C.slurpCafInitSequence cgModule
    in	case eCafOrder of
	 Left vsRecursive
	  -> Left vsRecursive

	 Right vsCafOrdering
	  -> Right $ toSeaGlob_withCafOrdering unique cgHeader cgModule vsCafOrdering

toSeaGlob_withCafOrdering unique cgHeader cgModule vsCafOrdering
 = let
	-- Partition the bindings into CAFs and non-CAFs
	cgModule_binds = C.globBind cgModule
	(  cgModule_binds_cafs
	     , cgModule_binds_nonCafs)
			= Map.partition C.isCafP cgModule_binds

	-- Order the CAFs by the given CAF initilization order.
	cgModule_binds_orderedCafs
		= foldl' (\psCafs v
			    -> let  Just pCaf	= Map.lookup v cgModule_binds_cafs
			       in   psCafs Seq.|> pCaf)
		   	Seq.empty
			vsCafOrdering

	-- All the bindings with ordered CAFs out the front.
	cModule_binds_ordered
		=      cgModule_binds_orderedCafs
		Seq.>< (Seq.fromList $ Map.elems cgModule_binds_nonCafs)

	cModule_nobinds	= C.seqOfGlob (cgModule { C.globBind = Map.empty })

	cModule'	=      cModule_binds_ordered
			Seq.>< cModule_nobinds

	-- For the toSea transform we need to know which bindings are CAFs
	vsCafs		= Set.union
				(Set.fromList 	$ Map.keys cgModule_binds_cafs)
				(Set.fromList 	$ Map.keys
						$ Map.filter C.isCafP
						$ C.globExtern cgHeader)

	esHeader	= toSeaTree (unique ++ "S") vsCafs cgHeader cgModule
				$ C.seqOfGlob cgHeader

	esModule	= toSeaTree (unique ++ "H") vsCafs cgHeader cgModule
				cModule'

   in	(esHeader, esModule)


toSeaTree
	:: String		-- ^ unique
	-> Set Var		-- ^ vars that are known to be CAFs.
	-> C.Glob		-- ^ original header glob
	-> C.Glob		-- ^ original module glob
	-> Seq C.Top 		-- ^ sequence of tops to convert.
	-> Seq (E.Top ())

toSeaTree unique vsCafs cgHeader cgModule ps
 = evalState 	(liftM join $ mapM toSeaP ps)
		SeaS 	{ stateVarGen		= VarId ("x" ++ unique) 0
			, stateCafVars		= vsCafs
			, stateDirectRegions	= Set.empty
			, stateHeaderGlob	= cgHeader
			, stateModuleGlob	= cgModule }


-- Top --------------------------------------------------------------------------------------------
toSeaP :: C.Top -> SeaM (Seq (E.Top ()))
toSeaP	xx
 = case xx of

	-- region
	--	slurp witnesses on the way down
	C.PRegion _ vts
	 -> do	let ks	= map (T.kindOfType . snd) vts
	 	mapM_ slurpWitnessKind ks
	 	return Seq.empty

	C.PExtern v t _
	 ->	return $ Seq.fromList [E.PExtern v (toSeaExternT t)]

 	C.PBind v x
	 -> do	let to		= C.superOpTypeX x
		let to'		= toSeaT to

		let (argTypes, resultType)
				= splitOpType to

		-- split the RHS into its value args and expression
	 	(argNames, x')
				<- splitSuper [] x

		sss'		<- mapM toSeaS $ slurpStmtsX x'
		let ss'		= concat sss'
		let argNTs	= zip argNames argTypes

		retV		<- newVarN NameValue
		let ssRet	= assignLastSS (E.XVar (E.NAuto retV) resultType, resultType) ss'
				++ [E.SReturn  (E.XVar (E.NAuto retV) resultType)]


		if T.takeValueArityOfType to == Just 0
		 then return	$ Seq.fromList
				[ E.PCafProto	v to'
				, E.PCafSlot 	v to'
				, E.PSuper	v [] 	resultType  ssRet]

		 else return	$ Seq.fromList
				[E.PSuper 	v argNTs resultType ssRet]


	C.PData (T.DataDef
			{ T.dataDefName		= v
			, T.dataDefCtors	= ctors })
	 -> do
		-- Convert data type declaration
		let ctors'	= Map.map toSeaCtorDef ctors
	 	let dataDef	= E.PData v ctors'

		-- Make #defines for data constructor tags, and
		--	sort them so they come out in the same order in the Sea
		--	file as in the original source file.
		let makeTagDef ctor@T.CtorDef{}
				= E.PCtorTag
			 		(E.seaVar False (T.ctorDefName ctor)) (T.ctorDefTag ctor)

		let tagDefs	= map makeTagDef
				$ sortBy (compare `on` T.ctorDefTag)
				$ Map.elems ctors

		-- All constructors with unboxed fields need a struct definition.
		-- For code generation, we need a struct name derived from the
		-- constructor name and a (index, type) pair for each field.
		-- Once we have the field (index, type) pairs, we sort the fields
		-- so that the boxed fields come first.
		let ctorStructs	= map makeCtorStruct
				$ filter hasUnboxedField
				$ sortBy (compare `on` E.ctorDefTag)
				$ Map.elems ctors'

	 	return		$ Seq.fromList
				$ dataDef : ctorStructs ++ tagDefs

	_ ->	return Seq.empty


hasUnboxedField ctor@E.CtorDef{}
 = any isUnboxedET $ E.ctorDefFieldTypes ctor


makeCtorStruct ctor@E.CtorDef{}
 = let (unboxed, boxed)
		= partition (isUnboxedET . snd)
		$ zip [0..]
		$ E.ctorDefFieldTypes ctor
   in E.PCtorStruct (E.ctorDefName ctor) (boxed ++ unboxed)


isUnboxedET t
 = case t of
	E.TCon (E.TyConUnboxed _) -> True
	_ -> False


-- | split the RHS of a supercombinator into its args and expression
splitSuper :: [Var] -> C.Exp -> SeaM ([Var], C.Exp)
splitSuper accArgs xx

	| C.XLam v _ x _ _	<- xx
	= splitSuper (accArgs ++ [v]) x

	| C.XLAM _ k x		<- xx
	= do	slurpWitnessKind k
		splitSuper accArgs x

	| C.XTau _ x		<- xx
	= splitSuper accArgs x

	| C.XLocal _ vts x	<- xx
	= do	let ks	= map (T.kindOfType . snd) vts
		mapM_ slurpWitnessKind ks
		splitSuper accArgs x

	| otherwise
	= return (accArgs, xx)



-- CtorDef ----------------------------------------------------------------------------------------
toSeaCtorDef :: T.CtorDef -> E.CtorDef
toSeaCtorDef (T.CtorDef vCtor tCtor arity tag fields)
 = let	tCtor'	= toSeaT tCtor
	-- Drop the last one because thats the type of the constructor.
	ptypes	= map toSeaT $ init $ T.flattenTFuns $ T.stripToBodyT tCtor
   in	E.CtorDef vCtor tCtor' arity tag fields ptypes


-- Exp --------------------------------------------------------------------------------------------
toSeaX	:: C.Exp -> SeaM (E.Exp ())
toSeaX		xx
 = case xx of
	C.XVar v t
	 -> do	-- Get the operational type of the var.
		vsCafs	<- gets stateCafVars
		if Set.member v vsCafs
		 then return $ E.XVar (E.NCaf  v) (toSeaT t)
		 else return $ E.XVar (E.NAuto v) (toSeaT t)

	C.XTau _ x
	 -> toSeaX x

	-- slurp region witnesses on the way down
	C.XLocal  _ vts x
	 -> do	let ks	= map (T.kindOfType . snd) vts
	 	mapM_ slurpWitnessKind ks
	 	toSeaX x

	-- slurp region witnesses on the way down
	C.XLAM _ k x
	 -> do 	slurpWitnessKind k
	 	toSeaX x

	-- non string constants
	C.XLit litFmt@(LiteralFmt _ fmt)
	 | dataFormatIsBoxed fmt
	 -> panic stage $ "toSeaX[XLit]: can't convert boxed literal " % litFmt

	 | otherwise
	 -> return	$ E.XLit (E.LLit litFmt)

	-- string constants are always applied to regions
	C.XAPP (C.XLit litFmt@(LiteralFmt LString{} _)) (T.TVar k _)
	 | k == T.kRegion
	 -> return $ E.XLit (E.LLit litFmt)

	C.XApp{}
	 -> toSeaApps [x | Left x <- C.flattenApps xx]

	C.XAPP{}
	 -> toSeaApps [x | Left x <- C.flattenApps xx]

	-- call a zero-arity constructor
	C.XPrim{}
	 -> toSeaApps [xx]

	_ -> panic stage
		$ "toSeaX: cannot convert expression to Sea IR.\n"
		% "-----\n"
		% xx					% "\n"


toSeaApps :: [C.Exp] -> SeaM (E.Exp ())
toSeaApps parts
 = case parts of
	-- forcing
	C.XPrim (C.MForce) _ : args
	 -> do	args'	<- mapM toSeaX args
	 	return	$ E.XPrim (E.MFun E.PFunForce) args'

	-- boxing
	C.XPrim C.MBox{} t : args
	 | [tUnboxed, _tBoxed]	<- T.flattenTFuns $ T.stripToBodyT t
	 -> do	args'		<- mapM toSeaX args
	 	return	$ E.XPrim
				(E.MBox (toSeaT tUnboxed))
				args'

	-- the unboxing function is named after the result type
	C.XPrim C.MUnbox{} t : args
	 | [_tBoxed, tUnboxed]	<- T.flattenTFuns $ T.stripToBodyT t
	 -> do	args'		<- mapM toSeaX args
		return	$ E.XPrim
				(E.MUnbox $ toSeaT tUnboxed)
				args'

	-- arithmetic operators
	C.XPrim (C.MOp _ op) _ : args
	 -> do	args'	<- mapM toSeaX args
		return	$ E.XPrim (E.MOp op) args'

	-- casting
	C.XPrim (C.MCast (PrimCast pt1 pt2)) _ : args
	 -> do	args'	<- mapM toSeaX args
		return	$ E.XPrim (E.MCast (PrimCast pt1 pt2)) args'

	-- coersion between pointer types
	-- converting these is a hassle because they contain Type.Exps
	C.XPrim (C.MCoerce (PrimCoercePtr t1 t2)) _ : args
	 -> do	let t1'	=  toSeaT t1
		let t2'	=  toSeaT t2
		args'	<- mapM toSeaX args
		return	$ E.XPrim (E.MCoerce (PrimCoercePtr t1' t2')) args'

	C.XPrim (C.MCoerce (PrimCoerceAddrToPtr t1)) _ : args
	 -> do	let t1'	=  toSeaT t1
		args'	<- mapM toSeaX args
		return	$ E.XPrim (E.MCoerce (PrimCoerceAddrToPtr t1')) args'

	C.XPrim (C.MCoerce (PrimCoercePtrToAddr t1)) _ : args
	 -> do	let t1'	=  toSeaT t1
		args'	<- mapM toSeaX args
		return	$ E.XPrim (E.MCoerce (PrimCoercePtrToAddr t1')) args'

	-- pointer ops
	C.XPrim (C.MPtr prim) _ : args
	 -> do	args'	<- mapM toSeaX args
		return	$ E.XPrim (E.MPtr prim) args'

	-- function calls
	-- For these four we statically know that the thing we're calling is a supercombinator.
	-- Note that if the resulting variables refer to supercombinators, their operational
	--   types contain function constructors. To make these we need the correct arity.
	--   We call getOpTypeVar, which inspects the original definition of each super for
	--   locally defined ones, or returns the operational type from the interface files
	--   for imported ones.
	C.XPrim (C.MCall (C.PrimCallTail vSuper)) _ : args
	 -> do	args'		<- mapM toSeaX args
		Just tSuper	<- getOpTypeOfVar vSuper
		return	$ E.XPrim (E.MApp E.PAppTailCall)
				  (E.XVar (E.NSuper vSuper) (toSeaSuperT tSuper) : args')


	C.XPrim (C.MCall (C.PrimCallSuper vSuper)) _ : [C.XVar _ (T.TCon (T.TyConData tname _ _))]
	 | varName tname == "Void#"
	 -> do
		Just tSuper	<- getOpTypeOfVar vSuper
	    	return	$ E.XPrim (E.MApp E.PAppCall)
				  (E.XVar (E.NSuper vSuper) (toSeaSuperT tSuper) : [])


	C.XPrim (C.MCall (C.PrimCallSuper vSuper)) _ : args
	 -> do	args'		<- mapM toSeaX args
		Just tSuper	<- getOpTypeOfVar vSuper
	    	return	$ E.XPrim (E.MApp E.PAppCall)
				  (E.XVar (E.NSuper vSuper) (toSeaSuperT tSuper) : args')

	C.XPrim (C.MCall (C.PrimCallSuperApply vSuper superA)) _ : args
	 -> do	args'		<- mapM toSeaX args
		Just tSuper	<- getOpTypeOfVar vSuper
		return	$ E.XPrim (E.MApp $ E.PAppCallApp superA)
				  (E.XVar (E.NSuper vSuper) (toSeaSuperT tSuper) : args')

	C.XPrim (C.MCall (C.PrimCallCurry vSuper superA)) _ : args
	 -> do	Just tSuper	<- getOpTypeOfVar vSuper
		if  any isUnboxedExp args
                 then panic stage
				$ "Partial application of function to unboxed args at "
 				% prettyPos vSuper
                 else
		  do	args'	<- mapM toSeaX args
			return	$ E.XPrim (E.MApp $ E.PAppCurry superA)
					  (E.XVar (E.NSuper vSuper) (toSeaSuperT tSuper) : args')

	-- For general application, the two things we're applying are boxed objects.
	C.XPrim (C.MCall (C.PrimCallApply var)) t : args
	 -> do	xFun'		<- toSeaX (C.XVar var t)
		args'		<- mapM toSeaX args
	    	return	$ E.XPrim
				(E.MApp $ E.PAppApply)
				(xFun' : args')

	-- A plain variable that had some type applications applied to it.
	[C.XVar v t]
	 -> return $ E.XVar (E.NAuto v) (toSeaT t)

	_ -> panic stage $ "toSeaApps: no match for " % pprStr [PrettyCoreTypes] parts


isUnboxedExp :: C.Exp -> Bool
isUnboxedExp x
 = case x of
	-- This may not be complete.
	C.XLit (LiteralFmt _ fmt) -> dataFormatIsUnboxed fmt
	_ -> False


-- Stmt -------------------------------------------------------------------------------------------
-- | Convert a statement into Sea
--
--   In the core, the RHS of a stmt might be another do, but there won't be any value
--   lambdas in front of it due to lambda lifting.
--
--   eg:  s = /\ +w13 :: Mutable %r1
--            [** type]
--            do { ... }
--
--   The Sea code doesn't handle nested groups of statements, but we can flatten them
--   all out into a single list here.
--
--
toSeaS	:: C.Stmt -> SeaM [E.Stmt ()]
toSeaS xx
 = case xx of
	-- decend past type info
	C.SBind b (C.XTau _ x)
	 -> toSeaS $ C.SBind b x

	C.SBind b (C.XLAM _ _ x)
	 -> toSeaS $ C.SBind b x

	C.SBind b (C.XLocal _ _ x)
	 -> toSeaS $ C.SBind b x

	-- flatten out the initial statements and recursively bind the lhs
	--	to the last expression in the list.
	C.SBind b (C.XDo ss)
	 -> do  let Just ssInit			= takeInit ss
	 	let Just (C.SBind Nothing x) 	= takeLast ss

		ssInit'	<- liftM concat $ mapM toSeaS ssInit
		ssMore	<- toSeaS (C.SBind b x)

	    	return	$ ssInit' ++ ssMore

	-- matches
	C.SBind (Just var) x@(C.XMatch aa)
	 -> do	aa'		<- mapM (toSeaA Nothing) aa

		let xT		= C.checkedTypeOfOpenExp (stage ++ ".toSeaS") x
		let t		= toSeaT xT
		let aaL		= map (assignLastA (E.XVar (E.NAuto var) t, t)) aa'

		return		[E.SMatch aaL]


	C.SBind Nothing	(C.XMatch aa)
	 -> do	aa'		<- mapM (toSeaA Nothing) aa
	    	return		[E.SMatch aa']


	-- expressions
	C.SBind (Just var) x
	 -> do	x'		<- toSeaX $ C.slurpExpX x
		let t		= C.checkedTypeOfOpenExp (stage ++ ".toSeaS") x
	    	return		[E.SAssign (E.XVar (E.NAuto var) (toSeaT t)) (toSeaT t) x']

	C.SBind Nothing x
	 -> do	x'		<- toSeaX x
	    	return		[E.SStmt x']


-- Alt --------------------------------------------------------------------------------------------
toSeaA :: (Maybe C.Exp) -> C.Alt -> SeaM (E.Alt ())
toSeaA	   mObjV xx
 = case xx of
	C.AAlt [] x
	 -> do
	 	ss'		<- liftM concat
				$  mapM toSeaS
				$  slurpStmtsX x

	    	return	$ E.ADefault ss'

	C.AAlt gs x
	 -> do	(ssFront, mgs')	<- mapAccumLM (toSeaG mObjV) [] gs
		let gs'		= catMaybes mgs'

	    	ss'		<- liftM concat
				$  mapM toSeaS
				$  slurpStmtsX x

		return	$ E.AAlt gs' (ssFront ++ ss')


-- Guard ------------------------------------------------------------------------------------------
toSeaG	:: Maybe C.Exp 		-- match object
	-> [E.Stmt ()] 		-- stmts to add to the front of this guard.
	-> C.Guard
	-> SeaM ( [E.Stmt ()]	-- stmts to add to the front of the next guard.
		,  Maybe (E.Guard ()))

toSeaG	_ ssFront gg
 = case gg of

	C.GExp w x
	 -> do	-- work out the type of the RHS
	 	let t		= C.checkedTypeOfOpenExp (stage ++ ".toSeaG") x
		let t'		= toSeaT t

	  	-- convert the RHS expression into a sequence of stmts
	 	ssRHS		<- liftM concat
				$  mapM toSeaS
				$  slurpStmtsX x

		-- if the guard expression is in a direct region then we don't need to check
		--	for suspensions during the match
		rhsIsDirect	<- isDirectType t

		let result
			-- if the LHS is var we can make the last stmt of the RHS assign it.
			| C.WVar var'	<- w
			= do	let ssL		= assignLastSS (E.XVar (E.NAuto var') t', t') ssRHS
				return	( ssFront ++ ssL
					, Nothing)

			-- the Sea language can't match against boxed literals
			| C.WLit _ (LiteralFmt _ fmt)	<- w
			, dataFormatIsBoxed fmt
			= panic stage 	$ "toSeaG: can't match against boxed data: " % show fmt % "\n"
					% "   when converting guard: " % gg

			-- match against an unboxed literal value
			| C.WLit spos litFmt@(LiteralFmt _ fmt)	<- w
			, dataFormatIsUnboxed fmt
			= do	name	<- liftM E.NAuto $ newVarN NameValue

				let compX	= if isPatConst w
					then E.XVar name t'
					else E.XTag $ E.XVar name t'

				let ssL		= assignLastSS (E.XVar name t', t') ssRHS
				return	( []
					, Just $ E.GCase spos False (ssFront ++ ssL) compX (E.XLit (E.LLit litFmt)))

			-- match against constructor
			| C.WCon sp v lvts	<- w
			= do	name		<- liftM E.NAuto $ newVarN NameValue

				let compX	= if isPatConst w
					then E.XVar name t'
					else E.XTag $ E.XVar name t'

				let ssL		= assignLastSS (E.XVar name t', t') ssRHS
				return	( map (toSeaGL name) lvts
					, Just $ E.GCase sp
							(not rhsIsDirect)
							(ssFront ++ ssL)
							compX (
							(E.XLit (E.LDataTag v))))

			| otherwise
			= panic stage $ "toSeaG: no match"

		result


-- check if this type is in a direct region
isDirectType :: T.Type -> SeaM Bool
isDirectType tt
	| Just (_, _, T.TVar kR (T.UVar vR) : _)	<- T.takeTData tt
	, kR == T.kRegion
	= do	directRegions	<- gets stateDirectRegions
	 	return	$ Set.member vR directRegions

	| otherwise
	= 	return False


isPatConst gg
 = case gg of
 	C.WLit{}	-> True
	_		-> False


toSeaGL	nObj (label, var, t)
	| C.LIndex i	<- label
	, isUnboxedT	t
	= E.SAssign
		(E.XVar (E.NAuto var) (toSeaT t))
		(toSeaT t)
		(E.XArgUnboxedData (E.XVar nObj (toSeaT t)) i)

	| C.LIndex i	<- label
	= E.SAssign
		(E.XVar (E.NAuto var) (toSeaT t))
		(toSeaT t)
		(E.XArgBoxedData (E.XVar nObj (toSeaT t)) i)

	| otherwise
	= panic stage $ "toSeaGL: no match"


-- | Decend into XLocal and XDo and slurp out the contained lists of statements.
slurpStmtsX :: C.Exp -> [C.Stmt]
slurpStmtsX xx
 = case xx of
 	C.XLocal _ _ x	-> slurpStmtsX x
	C.XDo ss	-> ss
	_		-> []




-- | Assign the value of the stmt(s) in this list to the provided exp.
assignLastSS :: (E.Exp (), E.Type) -> [E.Stmt ()] -> [E.Stmt ()]
assignLastSS	xT    ss
 = let	Just firstSS	= takeInit ss
 	Just lastS	= takeLast ss

   in	firstSS ++ (assignLastS xT lastS)


assignLastS :: (E.Exp (), E.Type) -> E.Stmt () -> [E.Stmt ()]
assignLastS xT@(aX, t) ss
 = case ss of
 	E.SStmt 	x	-> [E.SAssign aX t x]
	E.SAssign 	x _ _ 	-> [ss] ++ [E.SAssign aX t x]
	E.SSwitch       x aa	-> [E.SSwitch x (map (assignLastA xT) aa)]
	E.SMatch 	aa	-> [E.SMatch (map (assignLastA xT) aa)]
	_			-> panic stage $ "assignLastS: no match"


assignLastA :: (E.Exp (), E.Type) -> E.Alt () -> E.Alt ()
assignLastA xT aa
 = case aa of
 	E.ASwitch x ss		-> E.ASwitch x	(assignLastSS xT ss)
	E.ADefault ss		-> E.ADefault	(assignLastSS xT ss)
	E.AAlt gs ss		-> E.AAlt gs	(assignLastSS xT ss)
	_			-> panic stage $ "assignLastA: no match"

