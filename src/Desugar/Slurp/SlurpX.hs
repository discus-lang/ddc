-- | Constraint slurping for Expressions.

module Desugar.Slurp.SlurpX
	( slurpX )
where

-----
import {-# SOURCE #-} Desugar.Slurp.SlurpS
import {-# SOURCE #-} Desugar.Slurp.SlurpA
import Desugar.Slurp.Base

import Type.Util
import Type.Location
import qualified Shared.Var	as Var
import Util

-----
stage	= "Desugar.Slurp.SlurpX"

-----------------------
-- slurpX
--	Slurps out type an effect constraints from an expression.
--	Annotate the expression with new type and effect variables as we go.
--
slurpX	:: Exp Annot1
	-> CSlurpM 
		( Type		-- type of expression.
		, Effect	-- effect of expression.
		, Closure	-- closure of expression.
		, Exp Annot2	-- annotated exp.
		, [CTree])	-- constraints.


-----------------------
-- Lambda	
--	
slurpX	exp@(XLambda sp vBound xBody)
 = do
	tX		<- newTVarDS "lam"
	cX		<- newTVarCS "lam"

	-- Create type vars for all the lambda bound vars.
	Just tBound@(TVar _ vBoundT)	
			<- bindVtoT vBound
	
	-- Slurp the body.
	(tBody, eBody, cBody, xBody', qsBody)	
			<- slurpX xBody

	let qs	= 
		[ CEq (TSV $ SVLambda sp) tX	$ TFun tBound tBody eBody cX
		, CEq (TSC $ SCLambda sp) cX	$ makeTMask KClosure cBody (TTag vBound)]
	

	-- If the sub expression is also a lambda
	--	then we can pack its constraints into this branch as well.
	--	This reduces the number of nested branches and saves indenting in the constraint file.
	--
	let qs' = case xBody of
		XLambda{}
		  -> let [branch2]	= qsBody
		     in	 newCBranch
		     		{ branchBind 	= mergeCBinds (BLambda [vBoundT]) (branchBind branch2)
				, branchSub  	= qs ++ branchSub branch2 }
				
		_
		 ->	newCBranch
		 		{ branchBind 	= BLambda [vBoundT]
				, branchSub  	= qs ++ qsBody }
	

	-- we'll be wanting to annotate these vars with TECs when we convert to core.
	wantTypeVs
		$  vBoundT
		:  [v | TVar KEffect v  <- [eBody]]
		++ [v | TVar KClosure v <- [cX]]
	
	return	( tX
		, pure
		, cX
		, XLambdaTEC (Just (tX, pure)) vBound xBody' tBound eBody cX
		, [qs'])


-----------------------
-- App
--
slurpX	exp@(XApp sp fun arg)
 = do
	tX		<- newTVarDS "app"
	eX		<- newTVarES "app"
	cX		<- newTVarCS "app"

	eApp@(TVar KEffect vEApp)
			<- newTVarES "app"

	-- function
 	(tFun, eFun, cFun, fun', qsFun)	
			<- slurpX fun

	-- arg
	(tArg, eArg, cArg, arg', qsArg)	
			<- slurpX arg
	
	let qs	= 
		[ CEq (TSV $ SVApp sp) tFun	$ TFun tArg tX eApp empty 
		, CEq (TSE $ SEApp sp) eX	$ makeTSum KEffect  [eFun, eArg, eApp]
		, CEq (TSC $ SCApp sp) cX	$ makeTSum KClosure [cFun, cArg] ]
	
	return	( tX
		, eX
		, cX
		, XApp (Just (tX, eX)) fun' arg'
		, qsFun ++ qsArg ++ qs)


-----------------------
-- Match
--
slurpX	exp@(XMatch sp (Just obj) alts)
 = do
	-- unification vars
	tRHS		<- newTVarDS "matRHS"
	eX		<- newTVarES "mat"
	cX		<- newTVarCS "mat"

	eMatch@(TVar KEffect vEMatch)		
			<- newTVarES "matI"

	-- object
	(tObj, eObj, cObj, obj', qsObj)	
			<- slurpX obj

	let TVar KData vObj = tObj
	wantTypeV vObj

	-- alternatives
	(tsAltsLHS, tsAltsRHS, esAlts, csAlts, alts', qsAlts)	
			<- liftM unzip6 $ mapM slurpA alts

	let qsMatch	= 
		[ CEqs (TSU $ SUAltLeft sp)	(tObj : tsAltsLHS)
		, CEqs (TSU $ SUAltRight sp)	(tRHS : tsAltsRHS)
		, CEq  (TSE $ SEMatchObj sp)	eMatch	$ TEffect primReadH [tObj]
		, CEq  (TSE $ SEMatch sp) 	eX	$ makeTSum KEffect  ([eObj, eMatch] ++ esAlts) 
		, CEq  (TSC $ SCMatch sp)	cX	$ makeTSum KClosure ([cObj] ++ csAlts) ]

	return	( tRHS
		, eX
		, cX
		, XMatch (Just (tRHS, eX)) (Just obj') alts'
		, qsMatch ++ qsObj ++ qsAlts)


slurpX	exp@(XMatch sp Nothing alts)
 = do
	-- unification vars
	tLHS		<- newTVarDS "matLHS"
	tRHS		<- newTVarDS "matRHS"
	eMatch		<- newTVarES "mat"
	cMatch		<- newTVarCS "mat"

	-- alternatives
	(altsTP, altsTX, altsEs, altsClos, alts', altsQs)	
			<- liftM unzip6 $ mapM slurpA alts
	
	let matchQs	=  
		[ CEqs (TSU $ SUAltLeft sp)	(tLHS 	: altsTP)
		, CEqs (TSU $ SUAltRight sp)	(tRHS	: altsTX)
		, CEq  (TSE $ SEMatch sp)	eMatch	$ makeTSum KEffect altsEs
		, CEq  (TSC $ SCMatch sp)	cMatch	$ makeTSum KClosure altsClos ]
		
				  
	return	( tRHS
		, eMatch
		, cMatch
		, XMatch (Just (tRHS, eMatch)) Nothing alts'
		, matchQs ++ altsQs)


-----------------------
-- Const
--
slurpX	exp@(XConst sp c)
 = do	
 	tX@(TVar KData vT)	<- newTVarDS "lit"
	tConst			<- getConstType c
	let qs = 
		[ CEq (TSV $ SVLiteral sp c) tX	$ tConst]

	wantTypeV vT
	
	return	( tX
		, pure
		, empty
	  	, XConst (Just (tX, pure)) c
		, qs)


-----------------------
-- Var
--
slurpX 	exp@(XVar sp var)
 = do	tV@(TVar k vT)	<- lbindVtoT    var
	
	wantTypeV vT
	slurpV exp tV


-----------------------
-- Do
--
slurpX	exp@(XDo sp stmts)
 = do
	tX		<- newTVarDS 	"do"
	eX		<- newTVarES	"do"
	cX		<- newTVarCS	"do"
	
	--  Add all the bound vars to the bindMode map.
	let boundVs	= [v | Just v <- map takeStmtBoundV stmts]
	boundVsT	<- mapM lbindVtoT boundVs
	
	-- Decend into each statement in turn.
	(tsStmts, esStmts, csStmts, stmts', qssStmts)
			<- liftM unzip5 $ mapM slurpS stmts

	boundTVs	<- mapM getVtoT boundVs
	wantTypeVs boundTVs

	let Just tLast	= takeLast tsStmts
	let qsStmts	= concat qssStmts


	-- Signal that we're leaving the scope of all the let bindings in this block
	let letBindsC c	
		| CBranch{}	<- c
		, BLet vs	<- branchBind c
		= vs
		
		| otherwise
		= []

	let vsBind	= catMap letBindsC qsStmts
	let bindLeave	= case vsBind of
				[]	-> BNil
				_	-> BLetGroup vsBind
			
	-- The type for this expression is the type of the last statement.
	let q	= CBranch 
		{ branchBind	= bindLeave
		, branchSub	
		   = 	[ CEq (TSV $ SVDoLast sp) tX 	$ tLast
			, CEq (TSE $ SEDo sp)	eX 	$ makeTSum  KEffect  esStmts
			, CEq (TSC $ SCDo sp)	cX	$ makeTMask KClosure 
								(makeTSum KClosure csStmts) 
								(makeTSum KClosure $ map TTag boundVs) ] 
		   ++ qsStmts }

	return	( tX
		, eX
		, cX
		, XDo (Just (tX, eX)) stmts'
		, [q])


-----------------------
-- IfThenElse
--
slurpX	exp@(XIfThenElse sp xObj xThen xElse)
 = do
	tAlts		<- newTVarDS 	"ifAlts"
	eX		<- newTVarES	"if"
	cX		<- newTVarCS 	"if"
	eTest		<- newTVarES 	"ifObj"

	-- Slurp the case object.
 	(tObj, eObj, cObj, xObj', qsObj)	
			<- slurpX xObj

	let TVar KData vObj = tObj
	wantTypeV vObj

	-- The case object must be a Bool
	boolType	<- getGroundType "Bool"
	tR		<- newTVarR
	let tBool	= TData boolType [tR]

	-- Slurp the THEN expression.
	(tThen, eThen, cThen, xThen', qsThen)	
			<- slurpX xThen

	-- Slurp the ELSE expression.
	(tElse, eElse, cElse, xElse', qsElse)	
			<- slurpX xElse
	
	let qs	= 
		[ CEq  (TSV $ SVIfObj sp)	tObj	$ tBool
		, CEqs (TSU $ SUIfAlt sp)	(tAlts	: [tThen, tElse])
		
		, CEq  (TSE $ SEIfObj sp)	eTest 	$ TEffect primReadH [tObj]
		, CEq  (TSE $ SEIf sp)		eX	$ makeTSum KEffect  [eObj, eThen, eElse, eTest]
		, CEq  (TSC $ SCIf sp)		cX	$ makeTSum KClosure [cObj, cThen, cElse] ]
		
	return	( tAlts
		, eX
		, cX
		, XIfThenElse (Just (tAlts, eX)) xObj' xThen' xElse'
		, qs ++ qsObj ++ qsThen ++ qsElse )


---------------------------
-- Proj
--	x . l
--	l & x
--
slurpX 	exp@(XProj sp xBody proj)
 = do
	let projT	= case proj of
				JField  nn l	-> TJField  l
				JFieldR nn l	-> TJFieldR l

	-- the result of the projection
	tX	<- newTVarDS	"proj"
	eX	<- newTVarES	"proj"
	cX	<- newTVarCS	"proj"
	fX	<- newTVarFS	"proj"

	-- instance var for projection function.
	--	When we work out what the projection function is it'll be instantiated and bound to this var.
	tInst@(TVar KData vInst) 
		<- newTVarD

	-- effect and closure of projection function
	--	This will turn into the effect/closure of the function that is being applied
	--	to perform the projection - once we work out what that function is.
	--
	eProj	<- newTVarES	"proj"
	cProj	<- newTVarCS	"proj"

 	(tBody, eBody, cBody, xBody', qsBody)
			<- slurpX xBody
	

	let proj'	= transformN (\n -> Nothing) proj
			
	let qs	 = 
		[ CProject (TSV $ SVProj sp projT)	
			projT vInst tBody (TFun tBody tX eProj cProj)

		, CEq	(TSE $ SEProj sp)	
			eX $ makeTSum KEffect  [eBody, eProj]

		, CEq	(TSC $ SCProj sp)	
			cX $ makeTSum KClosure [cBody, cProj] ]

	return	( tX
		, eX
		, cX
		, XProjTagged (Just (tX, eX)) vInst xBody' proj'
		, qsBody ++ qs )


slurpX	exp@(XProjT sp tDict proj)
 = do	
 	let projT	= case proj of
				JField  nn l	-> TJField  l
				JFieldR nn l	-> TJFieldR l

	let vField	= case proj of
				JField  nn l	-> l
				JFieldR nn l	-> l

	-- the result of the projection
	tX	<- newTVarDS	"proj"
	eX	<- newTVarES	"proj"
	cX	<- newTVarCS	"proj"
	fX	<- newTVarFS	"proj"

	-- a var to tie the dictionary type to
	tDictVar <- newTVarDS	"proj"

	-- instance var for projection function.
	--	When we work out what the projection function is it'll be instantiated and bound to this var.
	tInst@(TVar KData vInst) 
		<- newTVarD

	let proj'	= transformN (\n -> Nothing) proj

	let qs	 = 
		[ CEq	   (TSV $ SVProj sp projT) 
			tDictVar $ tDict

		, CProject (TSV $ SVProj sp projT) 
			projT vInst tDictVar tX ]

	return	( tX
		, eX
		, TFree vField tX
		, XProjTaggedT (Just (tX, eX)) vInst proj'
		, qs )

slurpX x
 = panic stage $ "slurpX: cannot slurp " ++ show x


---------------------------
-- slurpV
--
slurpV exp@(XVar sp var) tV@(TVar _ vT)
 = do
	vTu		<- makeUseVar var vT
	let tX		= TVar KData vTu
	let eX		= pure
	let cX		= TFree var tV

	let qs	= 
		[ CInst (TSV $ SVInst sp vT) vTu vT ]

	return	( tX
		, eX
		, cX
	        , XVarInst (Just (tX, eX)) var
		, qs)

makeUseVar vInst vT
 = do 	TVar _ vTu_	<- newTVarD
	let vTu		= vTu_  
			{ Var.name = (Var.name vTu_)  ++ "_" ++ (Var.name vT) 
			, Var.info = [Var.IValueVar vInst]}
 	
	return vTu



