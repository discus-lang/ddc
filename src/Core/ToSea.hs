
module Core.ToSea
(
	toSeaTree
)

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

	let var		= (Var.new $ pretty varBind)
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
		let ss'		= catMaybes sss'
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
	-> SeaM (Var, [E.DataField [E.Stmt ()] E.Type])
	
toSeaCtor (C.CtorDef name fs)
 = do
 	fs'	<- mapM toSeaDataField fs
	return	$ (name, fs')
	
-----
toSeaDataField
	:: C.DataField C.Exp C.Type
	-> SeaM (E.DataField [E.Stmt() ] E.Type)
	
toSeaDataField field
 = do	mInit	<- case C.dInit field of
 			Nothing		-> return $ Nothing
			Just x		
			 -> do	ss'	<- mapM toSeaS $ slurpStmtsX x
			 	return	$ Just $ catMaybes ss'
				
	return	E.DataField
		{ E.dPrimary	= C.dPrimary 	field
		, E.dLabel	= C.dLabel	field
		, E.dType	= E.TObj
		, E.dInit	= mInit }

-----
toSeaX	:: C.Exp -> (E.Exp ())

toSeaX		xx
 = case xx of
	C.XVar v
	 -> E.XVar v

	-- discard left over annots
	C.XAnnot  n x		-> toSeaX x
	C.XTau    t x		-> toSeaX x
	C.XTet    vts x		-> toSeaX x
	C.XLocal  v vs x	-> toSeaX x

	-- discard type info
	C.XLAM v k x
	 -> toSeaX x

	-- core constructs
	C.XConst c t
	 -> toSeaConst c
	
	-- application
	C.XPrim (C.MTailCall v)	args eff
	 -> E.XTailCall v 	$ stripValues args

	C.XPrim (C.MCall v)	args eff
	 -> E.XCall v 		$ stripValues args

	C.XPrim (C.MCallApp v superA) args eff
	 -> E.XCallApp v superA $ stripValues args

	C.XPrim (C.MApply v)	args eff
	 -> E.XApply (E.XVar v) $ stripValues args
	   
	C.XPrim (C.MCurry v superA) args eff
	 -> E.XCurry v superA $ stripValues args

	C.XPrim (C.MSuspend fn)	 args eff
	 -> let	args'	= map E.XVar 
	 		$ filter (\v -> Var.nameSpace v == NameValue) 
			$ map (\(C.XVar v) -> v)
	 		$ args

	    in  E.XSuspend fn args'

	C.XPrim (C.MForce) [x] eff
	 -> E.XForce (toSeaX x)	 

	-- boxing
	C.XPrim (C.MBox tB tU) [x] eff
	 -> E.XBox	(toSeaT tU) (toSeaX x)
	 
	C.XPrim (C.MUnbox tU tB) [x] eff
	 -> E.XUnbox	(toSeaT tU) (toSeaX x)

	-- other primitive operations
	C.XPrim m xx eff
	 -> E.XPrim (toSeaPrim m) (map toSeaX xx)


	C.XAtom v ts
	 -> E.XAtom v

	-- An application to type/region/effects only
	--	we can just discard the TRE applications and keep the value.
	C.XAPP{}
	 -> let
	 	parts		= C.flattenApps xx
		(C.XVar vF : _)	= parts
		
	    in 	E.XVar vF
	 	
	_ -> panic stage
		$ "toSeaX: cannot convert expression to Sea IR.\n" 
		% "    exp = " % show xx	    	% "\n"
		% "\n"
		% "-----\n"
		% xx					% "\n"
	   
toSeaPrim :: C.Prim -> E.Prim
toSeaPrim (C.MFun v tR)
 = case Var.name v of
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
				$ "toSeaPrim: no match for " % v


-----
toSeaS	:: C.Stmt -> SeaM (Maybe (E.Stmt ()))
toSeaS xx
 = case xx of
	C.SComment{}
	 -> 	return Nothing

	-- match
	C.SBind (Just v) x@(C.XMatch aa eff)
	 -> do	aa'		<- mapM (toSeaA Nothing) aa
	 	let Just xT	= C.maybeSlurpTypeX x
		let t		= toSeaT xT
		let aaL		= map (assignLastA (E.XVar v, t)) aa'
		
		return		$ Just $ E.SMatch aaL


	C.SBind Nothing	x@(C.XMatch aa eff)
	 -> do	aa'	<- mapM (toSeaA Nothing) aa
	    	return	$ Just $ E.SMatch aa'

	    
	-- bind
	C.SBind mV (C.XTau t x@C.XMatch{})
	 -> toSeaS (C.SBind mV x)

	C.SBind (Just v) x
	 -> do	let x'		= toSeaX $ C.slurpExpX x
		let Just t	= C.maybeSlurpTypeX x
	    	return		$ Just $ E.SAssign (E.XVar v) (toSeaT t) x'

	C.SBind Nothing x
	 -> do	let x'	= toSeaX x
	    	return	$ Just $ E.SStmt x'


-----
toSeaA :: (Maybe C.Exp) -> C.Alt -> SeaM (E.Alt ())
toSeaA	   mObjV xx
 = case xx of
	C.AAlt [] x
	 -> do	ss'	<- case x of
				C.XDo ss	-> liftM catMaybes $ mapM toSeaS ss
				_ 		-> return	$ [E.SStmt (toSeaX x)] 

	    	return	$ E.ADefault ss'

	C.AAlt gs x
	 -> do	(ssFront, gs')	<- mapAccumLM (toSeaG mObjV) [] gs

	    	ss'		<- liftM catMaybes
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
	 -> do	ss'		<- liftM catMaybes
				$  mapM toSeaS
				$  slurpStmtsX x
		
		var		<- newVarN NameValue

		let Just t	= C.maybeSlurpTypeX x
		let t'		= toSeaT t
		let ssL		= assignLastSS (E.XVar var, t') ss'

		let (w', ssDecon)	
	 			= toSeaW (Just (C.XVar var)) w

		let compX	= if isPatConst w
					then E.XVar var
					else E.XTag $ E.XVar var

	    	return	( ssDecon 
			, E.GCase (ssFront ++ ssL) compX w')


toSeaW	_ 
	(C.WConst c)	
 = 	( toSeaConst c
 	, [])

toSeaW  (Just (C.XVar objV))
	(C.WCon   v lvts)	

 = 	( E.XCon v
 	, map (toSeaGL objV) lvts)


isAltConst  aa
 = case aa of
-- 	C.AAlt [C.GCase (C.WConst c)] x	-> True
	_				-> False

isPatConst gg
 = case gg of
 	C.WConst c	-> True
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


-----
stripValues args
	= catMaybes 
	$ map stripValues' args

stripValues' a
 = case a of
	C.XVar v
	 |  Var.nameSpace v == NameValue
	 -> Just $ E.XVar v

--	C.XConst c t
--	 -> Just $ toSeaConst c
	 
	C.XType _
	 -> Nothing
	 
	_ 	-> panic stage
		$  "stripValues: no match for " % show a % "\n"
	 
	
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
toSeaConst c 
 = case c of
 	CConstU l@(LInt    _)	-> E.XLiteral l
 	CConstU l@(LFloat  _)	-> E.XLiteral l
 	CConstU l@(LChar   _)	-> E.XLiteral l
 	CConstU l@(LString _)	-> E.XLiteral l

 	CConst l@(LInt    _)	-> E.XBox (E.TCon Var.primTInt32U [])   (E.XLiteral l)
 	CConst l@(LFloat  _)	-> E.XBox (E.TCon Var.primTFloat32U []) (E.XLiteral l)
-- 	CConst l@(LChar   _)	-> E.XBox (E.TCon Var.primTCharU [])    (E.XLiteral l)
 	CConst l@(LString _)	-> E.XBox (E.TCon Var.primTStringU [])  (E.XLiteral l)

 	



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




