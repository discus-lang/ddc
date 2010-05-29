
-- | Convert CoreIR to Abstract-C
module Core.ToSea
	(toSeaTree) 
where
import Data.Function
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Var
import Shared.VarUtil			(prettyPos)
import Data.Sequence			(Seq)
import Data.Traversable			(mapM)
import Util				hiding (mapM)
import Prelude				hiding (mapM)
import qualified Core.Exp 		as C
import qualified Core.Util		as C
import qualified Core.Reconstruct	as C
import qualified Core.OpType		as C
import qualified DDC.Type		as T
import qualified Type.Util		as T
import qualified Sea.Exp  		as E
import qualified Sea.Pretty		as E
import qualified Shared.VarPrim		as Var
import qualified DDC.Var.PrimId		as Var
import qualified Data.Map		as Map
import qualified Data.Set		as Set
import qualified Data.Sequence		as Seq

stage	= "Core.ToSea"


-- State ------------------------------------------------------------------------------------------
data SeaS
	= SeaS
	{ -- variable name generator
	  stateVarGen		:: VarId

	  -- regions known to be direct
	, stateDirectRegions	:: Set Var }

type SeaM	= State SeaS
	
newVarN ::	NameSpace -> SeaM Var
newVarN		space
 = do 	varBind		<- gets stateVarGen
	let varBind'	= incVarId varBind
	modify (\s -> s { stateVarGen = varBind' })

	let var		= (varWithName $ pprStrPlain varBind)
			{ varId		= varBind
			, varNameSpace	= space }
	return var


-- | If this is a witness to constness of a region or type, or purity of an effect
--	then slurp it into the table.
slurpWitnessKind :: T.Kind -> SeaM ()
slurpWitnessKind kk
 = case kk of
	-- const regions
 	T.KApp k (T.TVar kR (T.UVar r))
 	 | k	== T.kDirect
         , kR 	== T.kRegion
	 -> modify $ \s -> s { stateDirectRegions 
		 		= Set.insert r (stateDirectRegions s) }

	_ -> return ()


-- Tree -------------------------------------------------------------------------------------------
toSeaTree 
	:: String		-- unique
	-> Seq C.Top
	-> Seq (E.Top ())
	
toSeaTree unique cTree
  = evalState
  	(liftM join $ mapM toSeaP cTree)
	SeaS 	{ stateVarGen		= VarId ("x" ++ unique) 0
		, stateDirectRegions	= Set.empty }
   
    
-- Top --------------------------------------------------------------------------------------------
toSeaP :: C.Top -> SeaM (Seq (E.Top ()))
toSeaP	xx
 = case xx of
	
	-- region
	--	slurp witnesses on the way down
	C.PRegion v vts
	 -> do	let Just ks	= sequence $ map (T.kindOfType . snd) vts
	 	mapM_ slurpWitnessKind ks
	 	return Seq.empty

	C.PExtern{}
	 ->	return Seq.empty
	 	 
 	C.PBind v x
	 -> do	let to		= C.superOpTypeX x

		let (argTypes, resultType)	
				= splitOpType to

		-- split the RHS into its value args and expression
	 	(argNames, exp)	
				<- splitSuper [] x
		
		sss'		<- mapM toSeaS $ slurpStmtsX exp
		let ss'		= concat sss'
		let argNTs	= zip argNames argTypes

		retV		<- newVarN NameValue
		let ssRet	= assignLastSS (E.XVar retV resultType, resultType) ss'
				++ [E.SReturn (E.XVar retV resultType)]
		
	   	return	$ case argNTs of
			    	 [] ->	Seq.fromList
					[ E.PCafProto	v resultType
					, E.PCafSlot 	v resultType
					, E.PSuper	v [] 	resultType  ssRet]

				 _ ->	Seq.fromList
				 	[E.PSuper 	v argNTs resultType ssRet]
	    
	C.PData v ctors
	 -> do	
		-- Convert data type declaration
		let ctors'	= Map.map toSeaCtorDef ctors
	 	let dataDef	= E.PData v ctors'

		-- Make #defines for data constructor tags, and
		--	sort them so they come out in the same order in the Sea
		--	file as in the original source file.
		let makeTagDef ctor@C.CtorDef{} 
				= E.PHashDef 
			 		("_tag"         ++ E.seaVar False (C.ctorDefName ctor))
					("(_tagBase + " ++ show (C.ctorDefTag ctor) ++ ")")

		let tagDefs	= map makeTagDef 
				$ sortBy (compare `on` C.ctorDefTag)
				$ Map.elems ctors
								 
	 	return		$ Seq.fromList 
				$ dataDef : tagDefs
	   		
	_ ->	return Seq.empty


-- | split the RHS of a supercombinator into its args and expression
splitSuper :: [Var] -> C.Exp -> SeaM ([Var], C.Exp)
splitSuper accArgs xx

	| C.XLam v t x eff clo	<- xx
	= splitSuper (accArgs ++ [v]) x
	
	| C.XLAM v k x		<- xx
	= do	slurpWitnessKind k
		splitSuper accArgs x

	| C.XTau t x		<- xx
	= splitSuper accArgs x
		
	| C.XLocal v vts x	<- xx
	= do	let Just ks	= sequence $ map (T.kindOfType . snd) vts
		mapM_ slurpWitnessKind ks
		splitSuper accArgs x
	
	| otherwise
	= return (accArgs, xx)	 



-- CtorDef ----------------------------------------------------------------------------------------
toSeaCtorDef
	:: C.CtorDef
	-> E.CtorDef 
	
toSeaCtorDef (C.CtorDef vCtor tCtor arity tag fields)
 = let	tCtor'	= toSeaT tCtor
   in	E.CtorDef vCtor tCtor' arity tag fields


-- Exp --------------------------------------------------------------------------------------------
toSeaX	:: C.Exp -> SeaM (E.Exp ())
toSeaX		xx
 = case xx of
	C.XVar v t
	 -> return $ E.XVar v (toSeaT t)

	C.XTau    t x		
	 -> toSeaX x

	-- slurp region witnesses on the way down
	C.XLocal  v vts x	
	 -> do	let Just ks	= sequence $ map (T.kindOfType . snd) vts
	 	mapM_ slurpWitnessKind ks
	 	toSeaX x

	-- slurp region witnesses on the way down
	C.XLAM v k x
	 -> do 	slurpWitnessKind k
	 	toSeaX x

	-- function calls
	C.XPrim C.MTailCall xs
	 -> do	let (C.XVar v _) : args	= stripValues xs
		args'	<- mapM toSeaX args
		return	$ E.XTailCall v args'

	C.XPrim C.MCall xs
	 -> do	let (C.XVar v _) : args	= stripValues xs
		args'	<- mapM toSeaX args
	    	return	$ E.XCall v args'

	C.XPrim (C.MCallApp superA) xs
	 -> do	let (C.XVar v _) : args	= stripValues xs
		args'	<- mapM toSeaX args
		return	$ E.XCallApp v superA args'

	C.XPrim C.MApply xs
	 -> do	let (C.XVar v t) : args	= stripValues xs
		args'	<- mapM toSeaX args
	    	return	$ E.XApply (E.XVar v (toSeaT t)) args'
	   
	C.XPrim (C.MCurry superA) xs
	 -> do	let (C.XVar v _) : args	= stripValues xs
		if any isUnboxed args
                 then panic stage $ "Partial application of function to unboxed args at " % prettyPos v
                 else
		  do	args'	<- mapM toSeaX args
			return	$ E.XCurry v superA args'

	C.XPrim (C.MOp op) xs
	 -> do	let args		= stripValues xs
		args'	<- mapM toSeaX args
		return	$ E.XPrim (toSeaOp op) args'

	-- suspend
	C.XPrim (C.MSuspend fn)	args 
	 -> do	let args'	= map (\ (v, t) -> E.XVar v $ toSeaT t)
		 		$ filter (\ (v, t) -> varNameSpace v == NameValue) 
				$ map (\(C.XVar v t) -> (v, t))
		 		$ args

		return	$ E.XSuspend fn args'

	-- boxing
	C.XPrim C.MBox [_, x]
	 -> do	let t	= C.reconX_type (stage ++ "toSeaX") x
		x'	<- toSeaX x

		return	$ E.XBox (toSeaT t) x'

	-- the unboxing function is named after the result type
	C.XPrim C.MUnbox [C.XType r, x]
	 -> do	let tResult	= C.reconUnboxType r 
	 			$ C.reconX_type (stage ++ "toSeaX") x

		x'	<- toSeaX x

		return	$ E.XUnbox (toSeaT tResult) x'

	-- forcing
	C.XPrim (C.MForce) [x]
	 -> do	x'	<- toSeaX x
	 	return	$ E.XForce x'


	-- non string constants
	C.XLit litFmt@(LiteralFmt lit fmt)
	 | dataFormatIsBoxed fmt
	 -> panic stage $ "toSeaX[XLit]: can't convert boxed literal " % litFmt
	 
	 | otherwise
	 -> return	$ E.XLit litFmt

	-- string constants are always applied to regions 
	C.XAPP (C.XLit litFmt@(LiteralFmt l@LString{} fmt)) (T.TVar k r)
	 | k == T.kRegion
	 -> return $ E.XLit litFmt

	-- An application to type/region/effects only
	--	we can just discard the TRE applications and keep the value.
	C.XAPP{}
	 -> let
	 	parts		= C.flattenApps xx
		(C.XVar vF vT : _)	= parts
		
	    in 	return	$ E.XVar vF $ toSeaT vT
	 	
	_ -> panic stage
		$ "toSeaX: cannot convert expression to Sea IR.\n" 
		% "-----\n"
		% xx					% "\n"


isUnboxed :: C.Exp -> Bool
isUnboxed x
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
	C.SBind b (C.XTau t x)
	 -> toSeaS $ C.SBind b x
	 
	C.SBind b (C.XLAM v k x)
	 -> toSeaS $ C.SBind b x

	C.SBind b (C.XLocal v vts x)
	 -> toSeaS $ C.SBind b x


	-- do
	-- flatten out the initial statements and recursively bind the lhs 
	--	to the last expression in the list.
	C.SBind b (C.XDo ss)
	 -> do  let Just ssInit			= takeInit ss
	 	let Just (C.SBind Nothing x) 	= takeLast ss
		
		ssInit'	<- liftM concat $ mapM toSeaS ssInit
		ssMore	<- toSeaS (C.SBind b x)
		
	    	return	$ ssInit' ++ ssMore

	-- matches
	C.SBind (Just v) x@(C.XMatch aa)
	 -> do	aa'		<- mapM (toSeaA Nothing) aa

		let xT		= C.reconX_type (stage ++ ".toSeaS") x
		let t		= toSeaT xT
		let aaL		= map (assignLastA (E.XVar v t, t)) aa'
		
		return		[E.SMatch aaL]


	C.SBind Nothing	x@(C.XMatch aa)
	 -> do	aa'		<- mapM (toSeaA Nothing) aa
	    	return		[E.SMatch aa']

	    
	-- expressions
	C.SBind (Just v) x
	 -> do	x'		<- toSeaX $ C.slurpExpX x
		let t		= C.reconX_type (stage ++ ".toSeaS") x
	    	return		[E.SAssign (E.XVar v (toSeaT t)) (toSeaT t) x']

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


toSeaG	mObjV ssFront gg
 = case gg of

	C.GExp w x
	 -> do	-- work out the type of the RHS
	 	let t		= C.reconX_type (stage ++ ".toSeaG") x
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
			= do	let ssL		= assignLastSS (E.XVar var' t', t') ssRHS
				return	( ssFront ++ ssL
					, Nothing)

			-- the Sea language can't match against boxed literals
			| C.WLit _ litFmt@(LiteralFmt lit fmt)	<- w
			, dataFormatIsBoxed fmt
			= panic stage 	$ "toSeaG: can't match against boxed data: " % show fmt % "\n"
					% "   when converting guard: " % gg

			-- match against an unboxed literal value
			| C.WLit spos litFmt@(LiteralFmt lit fmt)	<- w
			, dataFormatIsUnboxed fmt
			= do	var	<- newVarN NameValue

				let compX	= if isPatConst w
					then E.XVar var t'
					else E.XTag $ E.XVar var t'

				let ssL		= assignLastSS (E.XVar var t', t') ssRHS
				return	( []
					, Just $ E.GCase spos False (ssFront ++ ssL) compX (E.XLit litFmt))
			  
			-- match against constructor
			| C.WCon sp v lvts	<- w
			= do	var		<- newVarN NameValue

				let compX	= if isPatConst w
					then E.XVar var t'
					else E.XTag $ E.XVar var t'

				let ssL		= assignLastSS (E.XVar var t', t') ssRHS
				return	( map (toSeaGL var) lvts
					, Just $ E.GCase sp 
							(not rhsIsDirect) 
							(ssFront ++ ssL) 
							compX (
							E.XCon v))

		result


-- check if this type is in a direct region
isDirectType :: T.Type -> SeaM Bool
isDirectType tt
	| Just (v, k, T.TVar kR (T.UVar vR) : _)	<- T.takeTData tt
	, kR == T.kRegion
	= do	directRegions	<- gets stateDirectRegions
	 	return	$ Set.member vR directRegions

	| otherwise
	= 	return False

	
isPatConst gg
 = case gg of
 	C.WLit{}	-> True
	_		-> False
	

toSeaGL	 objV (label, var, t)
	| C.LIndex i	<- label
	= E.SAssign 
		(E.XVar var (toSeaT t)) 
		E.TObj 
		(E.XArg (E.XVar objV (toSeaT t)) E.TObjData i)


-- | Decend into XLocal and XDo and slurp out the contained lists of statements.
slurpStmtsX :: C.Exp -> [C.Stmt]
slurpStmtsX xx
 = case xx of
 	C.XLocal v vs x	-> slurpStmtsX x
	C.XDo ss	-> ss
	_		-> []


-- Type -------------------------------------------------------------------------------------------
-- | Convert an operational type from the core to the equivalent Sea type.
toSeaT :: T.Type -> E.Type
toSeaT tt
 = case tt of
	T.TForall _ _ t		-> toSeaT t
	T.TFetters t _		-> toSeaT t
	T.TConstrain t _	-> toSeaT t

	T.TApp{}
	 -> let result
		 | Just tx		<- T.takeTData tt
		 = toSeaT_data tx
	
		 | Just (t1, t2, _, _)	<- T.takeTFun tt
		 = E.TObj
	
	    in result

	T.TCon{}
		| Just tx		<- T.takeTData tt
		-> toSeaT_data tx
	
	T.TVar{}		-> E.TObj

	_ 	-> panic stage
		$ "toSeaT: No match for " ++ show tt ++ "\n"
	

toSeaT_data tx
	-- the unboxed void type is represented directly.
 	| (v, _, _)			<- tx
 	, VarIdPrim Var.TVoidU	<- varId v
	= E.TVoid

 	 -- we know about unboxed pointers
	| (v, _, [t])			<- tx
	, VarIdPrim Var.TPtrU	<- varId v
	= E.TPtr (toSeaT t)

	-- the built-in unboxed types are represented directly.
	| (v, _, ts)			<- tx
	, Var.varIsUnboxedTyConData v
	= E.TCon v (map toSeaT $ filter hasValueKind ts)

	-- some user defined unboxed type.
	-- TODO: We just check for a '#' in the name to detect these, which is pretty nasty. 
	--	 Is there a better way to detect this?
	| (v, _, ts)			<- tx
	, elem '#' (varName v)
	= E.TCon v (map toSeaT $ filter hasValueKind ts)

	| otherwise
	= E.TObj


-- | Check if this type has value kind
hasValueKind :: T.Type -> Bool
hasValueKind xx
	| Just k	<- T.kindOfType xx
	, not $ elem k [T.kRegion, T.kClosure, T.kEffect]
	= True
	
	| otherwise
	= False


-- | Split a type into its params and return parts.
splitOpType :: T.Type -> ([E.Type], E.Type)
splitOpType to
  = let	opParts		= T.flattenFun to
	opParts'@(_:_)	= map toSeaT opParts
		
	argTypes	= init opParts'
	resultType	= last opParts'
   in 	(argTypes, resultType)


-- | Throw away the type terms in this list of expressions.
stripValues :: [C.Exp] -> [C.Exp]
stripValues args
	= catMaybes 
	$ map stripValues' args

stripValues' a
 = case a of
	C.XVar v t
	 |  varNameSpace v /= NameValue
	 -> Nothing

	C.XType _
	 -> Nothing
	 
	_ -> Just a
	 
	
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

    
assignLastA :: (E.Exp (), E.Type) -> E.Alt () -> E.Alt ()
assignLastA xT aa
 = case aa of
 	E.ASwitch x ss		-> E.ASwitch x	(assignLastSS xT ss)
	E.ADefault ss		-> E.ADefault	(assignLastSS xT ss)
	E.AAlt gs ss		-> E.AAlt gs	(assignLastSS xT ss)



-- | Convert a Core operator to a Sea primitive
toSeaOp :: C.Op -> E.Prim
toSeaOp op
 = case op of
	-- arithmetic
	C.OpNeg -> E.FNeg
 	C.OpAdd	-> E.FAdd
	C.OpSub	-> E.FSub
	C.OpMul	-> E.FMul
	C.OpDiv	-> E.FDiv
	C.OpMod	-> E.FMod

	-- comparison
	C.OpEq	-> E.FEq
	C.OpNeq	-> E.FNEq
	C.OpGt	-> E.FGt
	C.OpGe	-> E.FGe
	C.OpLt	-> E.FLt
	C.OpLe	-> E.FLe
	
	-- boolean
	C.OpAnd	-> E.FAnd
	C.OpOr	-> E.FOr
