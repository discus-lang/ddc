
-- | Convert CoreIR to Abstract-C
module Core.ToSea
	( toSeaTree )

where

import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.Var	as Var
import qualified Shared.VarPrim	as Var
import Shared.Var		(Var, NameSpace(..))
import Shared.Error
import qualified Shared.VarBind	as Var
import qualified Shared.Unique	as Unique

import qualified Core.Exp 		as C
import qualified Core.Util		as C
import qualified Core.Pretty		as C
import qualified Core.Util		as C
import qualified Core.Util.Slurp	as C
import qualified Core.Reconstruct	as C
import qualified Core.ReconKind		as C

import qualified Sea.Exp  	as E
import qualified Sea.Util	as E
import qualified Sea.Pretty	as E

import Shared.Literal

-----
stage	= "Core.ToSea"

type SeaM	= State SeaS
type Table	= Map String Var

uniqueSea	= "x" ++ Unique.coreToSea

data SeaS
	= SeaS
	{ stateVarGen	:: Var.VarBind
	, stateCtorDefs	:: Map Var C.CtorDef }
	
newVarN ::	NameSpace -> SeaM Var
newVarN		space
 = do 	varBind		<- gets stateVarGen
	let varBind'	= Var.incVarBind varBind
	modify (\s -> s { stateVarGen = varBind' })

	let var		= (Var.new $ pprStr varBind)
			{ Var.bind	= varBind
			, Var.nameSpace	= space }
	
	return var


-----
toSeaTree 
	:: Map Var C.CtorDef	-- Map of Constructor Definitions.
				--	Used for converting field label projections
				--	to projections field index projections.
	-> C.Tree
	-> E.Tree ()
	
toSeaTree mapCtorDefs cTree
  = evalState
  	(liftM concat $ mapM toSeaP cTree)
	SeaS 	{ stateVarGen	= Var.XBind uniqueSea 0
		, stateCtorDefs	= mapCtorDefs }
    
    
toSeaP :: C.Top -> SeaM [E.Top ()]
toSeaP	xx
 = case xx of
--	C.PExtern v tv to
--	 -> do	let (argTs, resultT)= splitOpType to
--		return [E.PProto v argTs resultT]

	C.PExtern{}
	 ->	return []
	 	 
 	C.PBind v x
	 -> do	let to		= C.superOpTypeX x

		let (argTypes, resultType)	
				= splitOpType to

	 	let (argNames, exp)	
				= splitSuper [] x
		
		sss'		<- mapM toSeaS $ slurpStmtsX exp
		let ss'		= concat sss'
		let argNTs	= zip argNames argTypes

		retV		<- newVarN NameValue
		let ssRet	= assignLastSS (E.XVar retV, resultType) ss'
				++ [E.SReturn (E.XVar retV)]
		
	   	return	$ case argNTs of
			    	 [] ->	[ E.PCafProto	v
					, E.PCafSlot 	v
					, E.PSuper	v [] 	resultType  ssRet]

				 _ ->	[ E.PSuper 	v argNTs resultType ssRet]
	
		
	    
	C.PData v ts cs
	 -> do	cs'		<- mapM toSeaCtor cs
	 	let dataDef	= E.PData v cs'
	 	let tagDefs	= map (\(v, i) -> E.PHashDef 
			 		("_tag" ++ E.seaVar False v)
					("(_tagBase + " ++ show i ++ ")"))
				$ (zip (map (\(C.CtorDef v _) -> v) cs) [0..])
	 
		let structDefs	= map toSeaStruct cs
		return		$ [dataDef] ++ tagDefs ++ structDefs
	 
	   		
	_ ->	return []


-----    
toSeaStruct 
	:: C.CtorDef
	-> E.Top ()

toSeaStruct (C.CtorDef name fs)
	= E.PStruct name
	$ map (\(i, df) -> 
		( fromMaybe (Var.new $ "a" ++ show i) $ C.dLabel df
		, E.TObj )) -- toSeaT $ C.dType df ))
	$ zip [0..] fs

-----
toSeaCtor 
	:: C.CtorDef
	-> SeaM (Var, [E.DataField E.Var E.Type])
	
toSeaCtor (C.CtorDef name fs)
 = do
 	fs'	<- mapM toSeaDataField fs
	return	$ (name, fs')
	
-----
toSeaDataField
	:: C.DataField C.Var C.Type
	-> SeaM (E.DataField E.Var E.Type)
	
toSeaDataField field
 = do	mInit	<- case C.dInit field of
 			Nothing		-> return $ Nothing
			Just x		-> return $ Just x		
				
	return	E.DataField
		{ E.dPrimary	= C.dPrimary 	field
		, E.dLabel	= C.dLabel	field
		, E.dType	= E.TObj
		, E.dInit	= mInit }

-----
toSeaX	:: C.Exp -> (E.Exp ())

toSeaX		xx
 = case xx of
	C.XVar v t
	 -> E.XVar v

	-- discard left over annots
	C.XAnnot  n x		-> toSeaX x
	C.XTau    t x		-> toSeaX x
	C.XTet    vts x		-> toSeaX x
	C.XLocal  v vs x	-> toSeaX x

	-- discard type applications
	C.XLAM v k x
	 -> toSeaX x

	-- function calls
	C.XPrim C.MTailCall xs
	 -> let (C.XVar v t) : args	= stripValues xs
	    in  E.XTailCall v 		$ map toSeaX args

	C.XPrim C.MCall xs
	 -> let (C.XVar v t) : args	= stripValues xs
	    in  E.XCall v 		$ map toSeaX args

	C.XPrim (C.MCallApp superA) xs
	 -> let (C.XVar v t) : args	= stripValues xs
	    in  E.XCallApp v superA 	$ map toSeaX args

	C.XPrim C.MApply xs
	 -> let (C.XVar v t) : args	= stripValues xs
	    in  E.XApply (E.XVar v) 	$ map toSeaX args
	   
	C.XPrim (C.MCurry superA) xs
	 -> let (C.XVar v t) : args	= stripValues xs
	    in	E.XCurry v superA 	$ map toSeaX args

	C.XPrim (C.MFun) xs
	 -> let (C.XVar v t) : args	= stripValues xs
	    in  E.XPrim (toSeaPrimV v)	$ map toSeaX args

	-- suspend
	C.XPrim (C.MSuspend fn)	args 
	 -> let	args'	= map E.XVar 
	 		$ filter (\v -> Var.nameSpace v == NameValue) 
			$ map (\(C.XVar v t) -> v)
	 		$ args

	    in  E.XSuspend fn args'

	-- boxing
	C.XPrim C.MBox [x]
	 -> let	t	= C.reconX_type (stage ++ "toSeaX") x
	    in	E.XBox (toSeaT t) (toSeaX x)

	C.XPrim C.MUnbox [x]
	 -> let	t	= C.reconX_type (stage ++ "toSeaX") x
	    in	E.XUnbox (toSeaT t) (toSeaX x)

	-- forcing
	C.XPrim (C.MForce) [x]
	 -> E.XForce (toSeaX x)	 

	C.XAtom v ts
	 -> E.XAtom v

	-- core constants are always applied when in expressions
	C.XAPP (C.XLit l) (C.TVar C.KRegion r)
	 -> toSeaConst l

	-- An application to type/region/effects only
	--	we can just discard the TRE applications and keep the value.
	C.XAPP{}
	 -> let
	 	parts		= C.flattenApps xx
		(C.XVar vF t : _)	= parts
		
	    in 	E.XVar vF
	 	
	_ -> panic stage
		$ "toSeaX: cannot convert expression to Sea IR.\n" 
		% "-----\n"
		% xx					% "\n"
	   


-- Stmt --------------------------------------------------------------------------------------------
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

--	 	let Just xT	= C.maybeSlurpTypeX x

		let xT		= C.reconX_type (stage ++ ".toSeaS") x
		let t		= toSeaT xT
		let aaL		= map (assignLastA (E.XVar v, t)) aa'
		
		return		[E.SMatch aaL]


	C.SBind Nothing	x@(C.XMatch aa)
	 -> do	aa'		<- mapM (toSeaA Nothing) aa
	    	return		[E.SMatch aa']

	    
	-- expressions
	C.SBind (Just v) x
	 -> do	let x'		= toSeaX $ C.slurpExpX x
		let t		= C.reconX_type (stage ++ ".toSeaS") x
	    	return		[E.SAssign (E.XVar v) (toSeaT t) x']

	C.SBind Nothing x
	 -> do	let x'	= toSeaX x
	    	return		[E.SStmt x']


-- Alt ---------------------------------------------------------------------------------------------
toSeaA :: (Maybe C.Exp) -> C.Alt -> SeaM (E.Alt ())
toSeaA	   mObjV xx
 = case xx of
	C.AAlt [] x
	 -> do	
	 	ss'		<- liftM concat
				$  mapM toSeaS
				$  slurpStmtsX x
	 
{-	 	ss'	<- case x of
				C.XDo ss	-> liftM concat $ mapM toSeaS ss
				_ 		-> return	$ [E.SStmt (toSeaX x)] -}

	    	return	$ E.ADefault ss'

	C.AAlt gs x
	 -> do	(ssFront, gs')	<- mapAccumLM (toSeaG mObjV) [] gs

	    	ss'		<- liftM concat
				$  mapM toSeaS
				$  slurpStmtsX x
				
	   	return	$ E.AAlt gs' (ssFront ++ ss')


toSeaG	:: Maybe C.Exp 		-- match object
	-> [E.Stmt ()] 		-- stmts to add to the front of this guard.
	-> C.Guard 
	-> SeaM ( [E.Stmt ()]	-- stmts to add to the front of the next guard.
		,  E.Guard ())


toSeaG	mObjV ssFront gg
 = case gg of

	C.GExp w x
	 -> do	ss'		<- liftM concat
				$  mapM toSeaS
				$  slurpStmtsX x
		
		var		<- newVarN NameValue

		let t		= C.reconX_type (stage ++ ".toSeaG") x
		let t'		= toSeaT t
		let ssL		= assignLastSS (E.XVar var, t') ss'

		let (w', ssDecon)	
	 			= toSeaW (Just (C.XVar var t)) w

		let compX	= if isPatConst w
					then E.XVar var
					else E.XTag $ E.XVar var

	    	return	( ssDecon 
			, E.GCase (ssFront ++ ssL) compX w')


toSeaW	_ 
	(C.WLit l)
 = 	( toSeaConst l
 	, [])

toSeaW  (Just (C.XVar objV t))
	(C.WCon   v lvts)	

 = 	( E.XCon v
 	, map (toSeaGL objV) lvts)


isAltConst  aa
 = case aa of
-- 	C.AAlt [C.GCase (C.WConst c)] x	-> True
	_				-> False

isPatConst gg
 = case gg of
 	C.WLit{}	-> True
	_		-> False


toSeaGL	 objV (label, var, t)
	| C.LIndex i	<- label
	= E.SAssign (E.XVar var) E.TObj (E.XArg (E.XVar objV) E.TData i)


-----
slurpStmtsX :: C.Exp -> [C.Stmt]
slurpStmtsX xx
 = case xx of
 	C.XLocal v vs x	-> slurpStmtsX x
	C.XDo ss	-> ss
	_		-> []


-----
-- toSeaT
--	Conversion from Core to Sea types.
--
toSeaT :: C.Type	-> E.Type
toSeaT	xx

	-- void type
	| C.TData v _	<- xx
	, Var.TVoidU	<- Var.bind v
	= E.TVoid
	
	-- some unboxed object
	| C.TData v ts	<- xx
	, last (Var.name v) == '#'
	= E.TCon v (map toSeaT $ filter (not . isREC) ts)
	
	-- trap regions/effect/closure info trying to get into the Sea tree.
	| isREC xx
	= panic stage $ "toSeaT: cannot convert " % xx % " to Sea type\n"

	-- some first class, boxed object	
	| otherwise
	= E.TObj

	where isREC xx	= elem (C.kindOfType xx) [C.KRegion, C.KEffect, C.KClosure]
	

splitOpType to
  = let
  	opParts		= C.flattenFun to
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
	 |  Var.nameSpace v /= NameValue
	 -> Nothing

	C.XType _
	 -> Nothing
	 
	_ -> Just a
	 
	
-----
splitSuper args xx

	| C.XLam v t x eff clo	<- xx
	= splitSuper (args ++ [v]) x
	
	| C.XLAM v k x		<- xx
	= splitSuper args x

	| C.XTau t x		<- xx
	= splitSuper args x
	
	| C.XTet vts x		<- xx
	= splitSuper args x
	
	| C.XLocal v vs x	<- xx
	= splitSuper args x
	
	| otherwise
	= (args, xx)	 


-----
toSeaConst l
 = case l of
	C.LInt8 i	-> E.XLiteral (LInt	$ fromIntegral i)
	C.LInt16 i	-> E.XLiteral (LInt	$ fromIntegral i)
	C.LInt32 i	-> E.XLiteral (LInt	$ fromIntegral i)
	C.LInt64 i	-> E.XLiteral (LInt	$ fromIntegral i)

	C.LWord8 i	-> E.XLiteral (LInt	$ fromIntegral i)
	C.LWord16 i	-> E.XLiteral (LInt	$ fromIntegral i)
	C.LWord32 i	-> E.XLiteral (LInt	$ fromIntegral i)
	C.LWord64 i	-> E.XLiteral (LInt	$ fromIntegral i)

	C.LFloat32 f	-> E.XLiteral (LFloat	$ (fromRational . toRational) f)
	C.LFloat64 f	-> E.XLiteral (LFloat	$ (fromRational . toRational) f)

	C.LChar c	-> E.XLiteral (LChar	$ c)
	C.LString s	-> E.XLiteral (LString	$ s)
	

 	



-----
-- assignLastSS
--	Assign the value of the stmt(s) in this list
--	to the provided exp.
--
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
	E.SMatch	aa	-> [E.SMatch (map (assignLastA xT) aa)]

    
assignLastA :: (E.Exp (), E.Type) -> E.Alt () -> E.Alt ()
assignLastA xT aa
 = case aa of
 	E.ASwitch x ss		-> E.ASwitch x	(assignLastSS xT ss)
	E.ADefault ss		-> E.ADefault	(assignLastSS xT ss)
	E.AAlt gs ss		-> E.AAlt gs	(assignLastSS xT ss)



toSeaPrimV :: C.Var -> E.Prim
toSeaPrimV var
 = case Var.name var of
	"primProjField"		-> E.FProjField
	"primProjFieldR"	-> E.FProjFieldR

	-- int
 	"primInt32_add"		-> E.FAdd
	"primInt32_sub"		-> E.FSub
	"primInt32_mul"		-> E.FMul
	"primInt32_div"		-> E.FDiv
	"primInt32_mod"		-> E.FMod
	"primInt32_eq"		-> E.FEq
	"primInt32_neq" 	-> E.FNEq
	"primInt32_gt"		-> E.FGt
	"primInt32_ge"		-> E.FGe
	"primInt32_lt"		-> E.FLt
	"primInt32_le"		-> E.FLe

	-- unboxed int
	"primInt32U_add"	-> E.FAdd
	"primInt32U_sub"	-> E.FSub
	"primInt32U_mul"	-> E.FMul
	"primInt32U_div"	-> E.FDiv
	"primInt32U_mod"	-> E.FMod
	"primInt32U_eq"		-> E.FEq
	"primInt32U_neq"	-> E.FNEq
	"primInt32U_gt"		-> E.FGt
	"primInt32U_lt"		-> E.FLt
	"primInt32U_ge"		-> E.FGe
	"primInt32U_le"		-> E.FLe
	
	"mod"			-> E.FMod
	"&&"			-> E.FAnd
	"||"			-> E.FOr

	-- float
	"primFloat32_add"	-> E.FAdd
	"primFloat32_sub"	-> E.FSub
	"primFloat32_mul"	-> E.FMul
	"primFloat32_div"	-> E.FDiv
	"primFloat32_eq"	-> E.FEq
	"primFloat32_neq"	-> E.FNEq
	"primFloat32_gt"	-> E.FGt
	"primFloat32_ge"	-> E.FGe
	"primFloat32_lt"	-> E.FLt
	"primFloat32_le"	-> E.FLe
	
	-- unboxed float
	"primFloat32U_add"	-> E.FAdd
	"primFloat32U_sub"	-> E.FSub
	"primFloat32U_mul"	-> E.FMul
	"primFloat32U_div"	-> E.FDiv
	"primFloat32U_mod"	-> E.FMod
	"primFloat32U_eq"	-> E.FEq
	"primFloat32U_neq"	-> E.FNEq
	"primFloat32U_gt"	-> E.FGt
	"primFloat32U_ge"	-> E.FGe
	"primFloat32U_lt"	-> E.FLt
	"primFloat32U_le"	-> E.FLe

	-- array	
	"arrayUI_get"		-> E.FArrayPeek (E.TCon Var.primTInt32U [])
	"arrayUI_set"		-> E.FArrayPoke (E.TCon Var.primTInt32U [])
	
	_			-> panic stage
				$ "toSeaPrim: no match for " % var
