-- | Constraint slurping for Expressions.

module Desugar.Slurp.SlurpX
	( slurpX )
where

-----
import {-# SOURCE #-} Desugar.Slurp.SlurpS
import {-# SOURCE #-} Desugar.Slurp.SlurpA
import Desugar.Slurp.Base

import Type.Location
import Type.Builtin
import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var
import Shared.Base

import DDC.Base.NameSpace

import Util	(liftM, unzip6, unzip5, takeLast, catMap)

import qualified Data.Set	as Set
import qualified Data.Set	(Set)


-----
stage	= "Desugar.Slurp.SlurpX"


-- | Slurp out type an effect constraints from an expression.
--   Annotate the expression with new type and effect variables as we go.
slurpX	:: Exp Annot1
	-> CSlurpM 
		( Type		-- type of expression.
		, Effect	-- effect of expression.
		, Closure	-- closure of expression.
		, Exp Annot2	-- annotated exp.
		, [CTree])	-- constraints.


-- Lam ------------------------------------------------------------------------
slurpX	exp@(XLambda sp vBound xBody)
 = do
	tX		<- newTVarDS "" -- "lam"
	cX		<- newTVarCS "" -- "lam"
	cX2		<- newTVarCS "" -- "lam"

	-- Create type vars for all the lambda bound vars.
	Just tBound@(TVar _ vBoundT)	
			<- bindVtoT vBound
	
	-- Slurp the body.
	(tBody, eBody, cBody, xBody', qsBody)	
			<- slurpX xBody

	-- Get the closure terms for value vars free in this expression
	let freeVs	= Set.filter (\v ->  (Var.nameSpace v == NameValue)
					 &&  (not $ Var.isCtorName v))
			$ freeVars exp 

	tsCloFree	<- mapM (\v -> do
					vT	<- lbindVtoT v
					return	$ TFree v vT)
			$ Set.toList freeVs

	-- Get closure terms from unresolved projection functions
	let tsProjTags	= collectClosureProjTags xBody'
	let tsClo	= tsCloFree ++ tsProjTags

	-- the constraints
	let qs	= 
		[ CEq (TSV $ SVLambda sp) tX	$ makeTFun tBound tBody eBody cX
		, CEq (TSC $ SCLambda sp) cX	$ makeTSum kClosure tsClo ]
 	
	-- If the sub expression is also a lambda
	--	then we can pack its constraints into this branch as well.
	--	This reduces the number of nested branches and saves indenting in the constraint file.
	--
	let qs' = case xBody of
		XLambda{}
		  -> let [branch2]	= qsBody
		     in	 CBranch
		     		{ branchBind 	= mergeCBinds (BLambda [vBoundT]) (branchBind branch2)
				, branchSub  	= qs ++ branchSub branch2 }
				
		_
		 ->	CBranch
		 		{ branchBind 	= BLambda [vBoundT]
				, branchSub  	= qs ++ qsBody }
	

	-- we'll be wanting to annotate these vars with TECs when we convert to core.
	wantTypeVs
		$  vBoundT
		:  [v | TVar kE v <- [eBody],	kE == kEffect]
		++ [v | TVar kC v <- [cX],	kC == kClosure]
	
	return	( tX
		, tPure
		, tEmpty
		, XLambdaTEC (Just (tX, tPure)) vBound xBody' tBound eBody cX
		, [qs'])


-- App ------------------------------------------------------------------------
slurpX	exp@(XApp sp fun arg)
 = do
	tX		<- newTVarDS "" --  "app"
	eX		<- newTVarES "" -- "app"
	cX		<- newTVarCS "" -- "app"

	eApp@(TVar _ vEApp)
			<- newTVarES "" -- "app"

	-- function
 	(tFun, eFun, cFun, fun', qsFun)	
			<- slurpX fun

	-- arg
	(tArg, eArg, cArg, arg', qsArg)	
			<- slurpX arg
	
	let qs	= 
		[ CEq (TSV $ SVApp sp) tFun	$ makeTFun tArg tX eApp tEmpty 
		, CEq (TSE $ SEApp sp) eX	$ makeTSum kEffect  [eFun, eArg, eApp] ]
	
	return	( tX
		, eX
		, tEmpty
		, XApp (Just (tX, eX)) fun' arg'
		, qsFun ++ qsArg ++ qs)


-- Match ----------------------------------------------------------------------
slurpX	exp@(XMatch sp (Just obj) alts)
 = do
	-- unification vars
	tRHS		<- newTVarDS "matRHS"
	eX		<- newTVarES "mat"
	cX		<- newTVarCS "mat"

	eMatch@(TVar _ vEMatch)		
			<- newTVarES "matI"

	-- object
	(tObj, eObj, cObj, obj', qsObj)	
			<- slurpX obj

	let TVar _ vObj = tObj

	-- alternatives
	(tsAltsLHS, tsAltsRHS, esAlts, csAlts, alts', qsAlts)	
			<- liftM unzip6 $ mapM slurpA alts

	let qsMatch	= 
		[ CEqs (TSU $ SUAltLeft sp)	(tObj : tsAltsLHS)
		, CEqs (TSU $ SUAltRight sp)	(tRHS : tsAltsRHS)
		, CEq  (TSE $ SEMatchObj sp)	eMatch	$ TEffect primReadH [tObj]
		, CEq  (TSE $ SEMatch sp) 	eX	$ makeTSum kEffect  ([eObj, eMatch] ++ esAlts) ]

	wantTypeV vObj

	return	( tRHS
		, eX
		, tEmpty
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
		, CEq  (TSE $ SEMatch sp)	eMatch	$ makeTSum kEffect altsEs ]
				  
	return	( tRHS
		, eMatch
		, tEmpty
		, XMatch (Just (tRHS, eMatch)) Nothing alts'
		, matchQs ++ altsQs)


-- Lit ------------------------------------------------------------------------
slurpX	exp@(XLit sp litFmt)
 = do	
 	tX@(TVar _ vT)	<- newTVarDS "lit"

	-- work out the type of this literal
	let TyConData 
		{ tyConName 	= tcVar
		, tyConDataKind = tcKind }
		= tyConOfLiteralFmt litFmt

	-- if the literal type needs a region var then make a fresh one
	let tLitM	
		| tcKind	== kValue
		= return $ makeTData tcVar tcKind []

		| tcKind	== KFun kRegion kValue
		= do	vR	<- newVarN NameRegion
		 	return	$ makeTData tcVar tcKind [TVar kRegion vR]
	tLit	<- tLitM
	
	let qs = [ CEq (TSV $ SVLiteral sp litFmt) tX tLit]

	wantTypeV vT
		
	return	( tX
		, tPure
		, tEmpty
	  	, XLit (Just (tX, tPure)) litFmt
		, qs)



-- Var ------------------------------------------------------------------------
slurpX 	exp@(XVar sp var)
 = do	tV@(TVar k vT)	<- lbindVtoT    var
	
	wantTypeV vT
	slurpV exp tV


-- Do -------------------------------------------------------------------------
slurpX	exp@(XDo sp stmts)
 = do
	tX		<- newTVarDS 	"do"
	eX		<- newTVarES	"do"
	cX		<- newTVarCS	"do"
	
	--  Add all the bound vars to the bindMode map.
	let boundVs	= [v | Just v <- map bindingVarOfStmt stmts]
	boundVsT	<- mapM lbindVtoT boundVs
	
	-- Decend into each statement in turn.
	(tsStmts, esStmts, csStmts, stmts', qssStmts)
			<- liftM unzip5 $ mapM slurpS stmts

	boundTVs	<- mapM getVtoT boundVs

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
			, CEq (TSE $ SEDo sp)	eX 	$ makeTSum  kEffect  esStmts ]
		   ++ qsStmts }

	wantTypeVs boundTVs

	return	( tX
		, eX
		, tEmpty
		, XDo (Just (tX, eX)) stmts'
		, [q])


-- If -------------------------------------------------------------------------
slurpX	exp@(XIfThenElse sp xObj xThen xElse)
 = do
	tAlts		<- newTVarDS 	"ifAlts"
	eX		<- newTVarES	"if"
	cX		<- newTVarCS 	"if"
	eTest		<- newTVarES 	"ifObj"

	-- Slurp the case object.
 	(tObj, eObj, cObj, xObj', qsObj)	
			<- slurpX xObj

	let TVar _ vObj = tObj

	-- The case object must be a Bool
	tR		<- newTVarR
	let tBool	= makeTData (primTBool Boxed) (KFun kRegion kValue) [tR]

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
		, CEq  (TSE $ SEIf sp)		eX	$ makeTSum kEffect  [eObj, eThen, eElse, eTest] ]
		
	wantTypeV vObj
		
	return	( tAlts
		, eX
		, tEmpty
		, XIfThenElse (Just (tAlts, eX)) xObj' xThen' xElse'
		, qs ++ qsObj ++ qsThen ++ qsElse )


-- Proj ------------------------------------------------------------------------
slurpX 	exp@(XProj sp xBody proj)
 = do
	let (projT, label)	
		= case proj of
			JField  nn l	-> (TJField  l, l)
			JFieldR nn l	-> (TJFieldR l, l)

	-- the result of the projection
	tX	<- newTVarDS	"proj"
	eX	<- newTVarES	"proj"
	cX	<- newTVarCS	"proj"

	-- instance var for projection function.
	--	When we work out what the projection function is it'll be instantiated and bound to this var.
	tInst@(TVar _ vInst) <- newTVarD

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
			projT vInst tBody (makeTFun tBody tX eProj cProj)

		, CEq	(TSE $ SEProj sp)	
			eX $ makeTSum kEffect  [eBody, eProj] ]

	return	( tX
		, eX
		, tEmpty
		, XProjTagged (Just (tX, eX)) vInst (TFree label cProj) xBody' proj'
		, qsBody ++ qs )


slurpX	exp@(XProjT sp tDict proj)
 = do	
 	let (projT, label)	
		= case proj of
			JField  nn l	-> (TJField  l, l)
			JFieldR nn l	-> (TJFieldR l, l)

	-- the result of the projection
	tX	<- newTVarDS	"proj"
	eX	<- newTVarES	"proj"
	cX	<- newTVarCS	"proj"

	-- a var to tie the dictionary type to
	tDictVar <- newTVarDS	"proj"

	-- instance var for projection function.
	--	When we work out what the projection function is it'll be instantiated and bound to this var.
	tInst@(TVar _ vInst) 
		<- newTVarD

	-- closure of projection function
	-- 	Make a closure term out of the projection function, once we know what it is.
	--	This means that the caller knows that the projection function is free in it.
	--
	cProj	<- newTVarCS	"proj"

	let proj'	= transformN (\n -> Nothing) proj

	let qs	 = 
		[ CProject (TSV $ SVProj sp projT) projT vInst tDictVar tX 
		, CEq	   (TSV $ SVProj sp projT) tDictVar tDict
		, CEq      (TSV $ SVProj sp projT) cProj (TFree label tX)
		]

	return	( tX
		, eX
		, tEmpty
		, XProjTaggedT (Just (tX, eX)) vInst (TFree label cProj) proj'
		, qs )

slurpX x
 = panic stage $ "slurpX: cannot slurp " ++ show x


-- | Make an Inst constraint for a variable.
slurpV exp@(XVar sp var) tV@(TVar k vT)
 = do
	vTu		<- makeUseVar var vT
	let tX		= TVar k vTu
	let eX		= tPure
	let qs		= [ CInst (TSV $ SVInst sp vT) vTu vT ]

	return	( tX
		, eX
		, tEmpty
	        , XVarInst (Just (tX, eX)) var
		, qs)

makeUseVar vInst vT
 = do 	TVar _ vTu_	<- newTVarD
	let vTu		= vTu_  
			{ Var.name = (Var.name vTu_)  ++ "_" ++ (Var.name vT) 
			, Var.info = [Var.IValueVar vInst]}
 	
	return vTu



