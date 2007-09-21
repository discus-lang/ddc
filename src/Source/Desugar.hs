
-- Source.Desugar
--	This is the pre type inference desugarer.
--	Desugaring before the inferencer makes the error messages not as good, 
--	but the constraint slurper a lot smaller.
--   
module Source.Desugar
(
	rewriteTree,
	rewrite
)

where

import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.Var	as Var
import qualified Shared.Unique	as Unique
import qualified Shared.VarUtil	as Var

import Shared.VarPrim
import Shared.Base

import Shared.Base		(SourcePos)
import Shared.Var		(NameSpace(..))
import Shared.Error
import Shared.Literal

import Type.Exp
import Type.Plate.Trans
import Type.Util			

import qualified Source.Exp		as S
import qualified Source.Util		as S
import qualified Source.Pretty		as S
import qualified Source.Plate.Trans	as S

import qualified Desugar.Exp		as D
import qualified Desugar.Util		as D
import qualified Desugar.Pretty		as D

import Shared.Exp

import Source.Desugar.Base
import Source.Desugar.Patterns
import Source.Desugar.Dict
import Source.Desugar.Type

import {-# SOURCE #-} Source.Desugar.ListComp

-----
stage	= "Source.Desugar"

-----
rewriteTree 
	:: Map Var Kind
	-> S.Tree 
	-> D.Tree Annot

rewriteTree kindMap tree
 	= evalState (rewriteTreeM tree)
	$ initRewriteS
	{ stateKind	= kindMap }

rewriteTreeM :: S.Tree -> RewriteM (D.Tree Annot)
rewriteTreeM tree
 = do	treeR		<- liftM catMaybes 
 			$ mapM rewrite tree

	treeP		<- rewritePatTreeM treeR

	return		$ treeP
 	


-----
instance Rewrite S.Top (Maybe (D.Top Annot)) where
 rewrite xx
  = case xx of

	S.PImportExtern v tv to
	 ->	returnJ $ D.PExtern none v tv to

	S.PImportModule ms
	 ->	returnJ	$ D.PImport none ms

	S.PForeign (S.OImport (S.OExtern mName v tv to)) 
	 -> do	tv'	<- rewrite tv
		let v'	= case mName of
				Nothing		-> v
				Just seaName	-> v { Var.info = Var.info v ++ [Var.ISeaName seaName ]}
		
		let to'	= maybeJust to (let Just to2 = makeOpTypeT tv' in to2)
	 	returnJ $ D.PExtern none v' tv' to'


	S.PData v vs ctors
	 -> do	ctors'	<- mapM rewriteCtorDef ctors
	 	returnJ	$ D.PData none v vs ctors'
	
	S.PEffect v k
	 ->	returnJ	$ D.PEffect none v k
	 
	S.PRegion v
	 ->	returnJ	$ D.PRegion none v
	 
	-- classes
	S.PClass v k
	 ->	returnJ	$ D.PClass  none v k

 	S.PClassDict vC vs inh sigs
	 -> do	let sigs'	= catMap (\(vs, t) ->
					zip vs (repeat t))
				$ sigs

		returnJ		$ D.PClassDict none vC (map makeTVar vs) [] sigs'

	S.PClassInst vC ts context stmts
	 -> do	
		stmts'		<- mapM rewrite stmts
		defs		<- mapM (\(D.SBind _ (Just v) x) 
					-> do	return	$ (v, x))
				$ stmts'

		let context'	= map (\(v, vs) ->
					D.ClassContext v ts)
				$ context

		let ?getKind	= getKind
--		tsExp		<- mapM expandT ts
		ts'		<- mapM rewrite ts

	 	returnJ		$ D.PClassInst none vC ts' context' defs

	-- projections
	S.PProjDict t ss
	 -> do	ss'		<- mapM rewrite ss
	 	
		let ?getKind	= getKind
--		tExp		<- expandT t
		t'		<- rewrite t
		
		returnJ		$ D.PProjDict none t' ss'
		

	S.PType sp v t
	 -> do	t'	<- rewrite t
	 	returnJ	$ D.PSig sp v t'

  	S.PStmt s
	 -> do	(D.SBind sp mV x)	<- rewrite s
	 	returnJ			$ D.PBind sp mV x
		
	_  ->	return	Nothing
	
-----
rewriteCtorDef (v, fs)
 = do	fs'	<- mapM rewriteField fs
 	return	$ D.CtorDef none v fs'
	
rewriteField field
 = do	mX'	<- liftMaybe rewrite $ dInit field
 	return	$ field { dInit = mX' }


-----
instance Rewrite S.Exp (D.Exp Annot) where
 rewrite xx
  = case xx of

	-- core language.
	S.XNil		-> return D.XNil

	S.XUnit	sp
	 -> 	return	$ D.XVar  	sp primUnit

	S.XVoid	sp	
	 -> 	return	$ D.XVoid 	sp

	S.XConst sp c
	 -> 	return	$ D.XConst 	sp c

	S.XVar sp v	
	 -> 	return	$ D.XVar 	sp v

	S.XProj sp x1 (S.JIndex x2)
	 -> do	x1'	<- rewrite x1
	 	x2'	<- rewrite x2
		return	$ D.XApp sp (D.XApp sp (D.XVar sp primIndex) x1') x2'
	
	S.XProj sp x1 (S.JIndexR x2)
	 -> do	x1'	<- rewrite x1
	 	x2'	<- rewrite x2
		return	$ D.XApp sp (D.XApp sp (D.XVar sp primIndexR) x1') x2'

	S.XProj sp x pj
	 -> do	x'	<- rewrite x
	 	pj'	<- rewrite pj
	 	return	$ D.XProj 	sp x' pj'
		
	S.XLambda sp v x
	 -> do	x'	<- rewrite x
		return	$ D.XLambda sp v x'
		
	S.XApp sp x1 x2
	 -> do	x1'	<- rewrite x1
	 	x2'	<- rewrite x2
		return	$ D.XApp 	sp x1' x2'
		
	S.XCase sp x aa
	 -> do	x'	<- rewrite x
	 	aa'	<- rewrite aa
		return	$ D.XMatch 	sp (Just x') aa'
		
	S.XLet sp ss x
	 -> do	ss'	<- rewrite ss
	 	x'	<- rewrite x
		return	$ D.XDo 	sp (ss' ++ [D.SBind sp Nothing x'])
		
	S.XDo sp ss
	 -> do	ss'	<- rewrite ss
	 	return	$ D.XDo 	sp ss'
		
	S.XIfThenElse sp x1 x2 x3
	 -> do	x1'	<- rewrite x1
	 	x2'	<- rewrite x2
		x3'	<- rewrite x3
		return	$ D.XIfThenElse sp x1' x2' x3'
		
	-- lambda sugar.
	S.XLambdaPats sp ps x
	 -> do	x'	<- rewrite x
	 	matchPats sp ps x'

	S.XLambdaCase sp alts
	 -> do
		alts'	<- rewrite alts

	 	var_	<- newVarN NameValue
		let var	= var_
			{ Var.info = [Var.ISourcePos sp] }
		
		return	$ D.XLambda sp var (D.XMatch sp (Just $ D.XVar sp var) alts')

	S.XLambdaProj sp j xs
	 -> do	var_	<- newVarN NameValue
	 	let var	= var_
			{ Var.info = [Var.ISourcePos sp] }

		j'	<- rewrite j
		xs'	<- rewrite xs
			
		return	$ D.XLambda sp var (D.unflattenApps sp (D.XProj sp (D.XVar sp var) j' : xs'))

	-- match sugar.
	S.XMatch sp aa
	 -> do	aa'	<- rewrite aa
		
	 	return	$ D.XMatch sp Nothing aa'
	 	

	-- exception sugar.
	S.XThrow sp x
	 -> do
		x'	<- rewrite x
	 	return	$ (D.XApp sp (D.XVar sp primThrow) x')

	S.XTry sp x aa Nothing
	 -> do
		x'	<- rewrite x
		aa'	<- rewrite aa
	 	rewriteTry sp [] x' aa'

	S.XTry sp x aa (Just w)
	 -> do
		x'	<- rewrite x
		w'	<- rewrite w
		aa'	<- rewrite aa

		withV	<- newVarNI NameValue [Var.ISourcePos sp]
		withVA	<- newVarNI NameValue [Var.ISourcePos sp]
		d	<- newVarN  NameValue
		
		let ssMore	= [D.SBind sp (Just withV) (D.XLambda sp d w')]
		let appW	= D.XApp sp (D.XVar sp withV) (D.XVar sp primUnit)
		
		let aaWith	= map (addWithAlt appW) aa'
		rewriteTry sp ssMore x' aaWith


	-- imperative sugar
	S.XWhen	sp testX bodyX
	 -> do	testX'	<- rewrite testX
	 	bodyX'	<- rewrite bodyX
		return	$ D.XMatch sp (Just testX')
				[ D.AAlt sp	[D.GCase sp (D.WConLabel sp primTrue [])] 
						bodyX'

				, D.AAlt sp	[]
						(D.XVar sp primUnit) ]
				
	S.XUnless sp testX bodyX
	 -> do	testX'	<- rewrite testX
	 	bodyX'	<- rewrite bodyX
	 	return	$ D.XMatch sp (Just testX')
		 		[ D.AAlt sp	[D.GCase sp (D.WConLabel sp primFalse [])]
						bodyX'

				, D.AAlt sp 	[]
						(D.XVar sp primUnit) ]
	 
	S.XWhile sp testX bodyX
	 -> do	d1	<- newVarN NameValue
		vLoop	<- newVarN NameValue
		testX'	<- rewrite testX
		bodyX'	<- rewrite bodyX
	
		let loopX	= D.XLambda sp d1 
				$ D.XDo sp 
				[ D.SBind sp Nothing 
					(D.XMatch sp (Just testX')
						[ D.AAlt sp	[D.GCase sp (D.WConLabel sp primFalse [])] 
								(D.XApp sp (D.XVar sp primThrow) (D.XVar sp primExceptionBreak))

						, D.AAlt sp	[]
								(D.XVar sp primUnit) ])
										
				, D.SBind sp Nothing bodyX' ]

		return		$ D.XDo sp 
				[ D.SBind sp (Just vLoop) 	$ loopX
				, D.SBind sp Nothing		$ D.XApp sp (D.XVar sp primGateLoop) (D.XVar sp vLoop)]

	S.XBreak sp 
	 ->	return	$ D.XApp sp (D.XVar sp primThrow) (D.XVar sp primExceptionBreak)


	-- list sugar
	S.XList sp xs
	 -> do	let makeList = \xx -> case xx of
	 		[]	-> D.XVar sp primNil
			(x:xs)	-> D.XApp sp (D.XApp sp (D.XVar sp primCons) x) (makeList xs)
			
		xs'	<- mapM rewrite xs
		return	$ makeList xs'
	
	S.XListRange sp True x1 (Just x2)
	 -> do	x1'	<- rewrite x1
	 	x2'	<- rewrite x2
		return	$ D.XApp sp (D.XApp sp (D.XVar sp primRangeL) x1') x2'

	S.XListRange sp False x1 (Just x2)
	 -> do	x1'	<- rewrite x1
	 	x2'	<- rewrite x2
		return	$ D.XApp sp (D.XApp sp (D.XVar sp primRange) x1') x2'

	S.XListRange sp True x1 Nothing
	 -> do	x1'	<- rewrite x1
	  	return	$ D.XApp sp (D.XVar sp primRangeInfL) x1'

	S.XListComp{}
	 -> 	rewriteListComp xx


	-- data expression/pattern sugar.
	S.XTuple sp xx
	 -> do	xx'	<- rewrite xx
	 	return	$ D.unflattenApps sp ((D.XVar sp $ primTuple (length xx)) : xx')

	_	-> panic stage
		$ "rewrite[Exp]: can't rewrite " % show xx	% "\n"

		
-----
instance Rewrite S.Proj (D.Proj Annot) where
 rewrite pp
  = case pp of
  	S.JField  v	-> return $ D.JField  none v
	S.JFieldR v	-> return $ D.JFieldR none v

	S.JIndex  x	
	 -> do	v	<- newVarN NameValue
	 	return	$ D.JField none v

	S.JIndexR  x	
	 -> do	v	<- newVarN NameValue
	 	return	$ D.JFieldR none v


-----
instance Rewrite S.Stmt (D.Stmt Annot) where
 rewrite ss
  = case ss of
	S.SBindPats sp v ps x
	 -> do	x'	<- rewrite x
	 	x2	<- matchPats sp ps x'
		return	$ D.SBind sp (Just v) x2

	S.SBind sp mV x
	 -> do	x'	<- rewrite x
	 	return	$ D.SBind sp mV x'
				
	S.SSig sp v t
	 -> do 	t'	<- rewrite t
	 	return	$ D.SSig sp v t'


-----
instance Rewrite S.Alt (D.Alt Annot) where
 rewrite aa
  = case aa of
	S.APat w x
	 -> do	w'		<- rewrite w
	  	x'		<- rewrite x
	 	return	$ D.AAlt none 	[D.GCase none w'] x'

	S.AAlt gs x
	 -> do	gs'	<- rewrite gs
	 	x'	<- rewrite x
		return	$ D.AAlt none gs' x'

{-	 	gsAts	<- mapM (sprinkleAtsG none) gsD
	 
	 	let gsLift	= catMap liftAtsG gsAts
-}	 

	S.ADefault x
	 -> do	x'	<- rewrite x
	 	return	$ D.AAlt none 	[] x'


-----
instance Rewrite S.Guard (D.Guard Annot) where
 rewrite gg 
  = case gg of
	S.GCase w
	 -> do	w'	<- rewrite w
	 	return	$ D.GCase none w'
		
	S.GExp w x
	 -> do	w'	<- rewrite w
	 	x'	<- rewrite x
		return	$ D.GExp none w' x'
		
	S.GBool x
	 -> do	x'	<- rewrite x
	 	return	$ D.GExp none (D.WConLabel none primTrue []) x'
		
	S.GBoolU x
	 -> do	x'	<- rewrite x
	 	return	$ D.GExp none (D.WConst none (CConstU (LInt 1))) x'
		

----
-- This is basic rewriting of the AST from S.Pat to D.Pat
--	all the more involved rewrites should go in Desugar.Patterns

instance Rewrite S.Pat (D.Pat Annot) where
 rewrite ww
  = case ww of
	S.WVar v
	 -> do	return	$ D.WVar none  v

	S.WConst c
	 -> do	return	$ D.WConst none c

	S.WCon v ps
	 -> do	ps'	<- mapM rewrite ps
	 	return	$ D.WConLabelP none 
				(rewritePatVar v) 
				(zip [D.LIndex none i | i <- [0..]] ps')
		
	S.WAt v p
	 -> do	p'	<- rewrite p
	 	return	$ D.WAt none v p'
		
	S.WWildcard
	 -> do	return	$ D.WWildcard none
	 
	S.WConLabel v lvs
	 -> do	lvs'	<- mapZippedM 
	 			rewrite rewrite 
	 			lvs

	 	return	$ D.WConLabelP none (rewritePatVar v) lvs'

	---
	S.WUnit 
	 -> do	return	$ D.WConLabel none primUnit []

	S.WTuple ps
	 -> do	ps'	<- mapM rewrite ps
	 	return	$ D.WConLabelP none 
				(primTuple (length ps)) 
				(zip [D.LIndex none i | i <- [0..]] ps')
		
	S.WCons p1 p2
	 -> do	p1'	<- rewrite p1
	 	p2'	<- rewrite p2
		return	$ D.WConLabelP none 
				primCons 
				[ (D.LIndex none 0, p1')
				, (D.LIndex none 1, p2') ]

	S.WList []
	 -> 	return	$ D.WConLabelP none 
	 			primNil []

	---
	S.WExp x
	 -> do	let p	= expToPat x
	 	p'	<- rewrite p
		return	p'

	_	-> panic stage	
		$ "rewrite[S.Pat]: can't rewrite " % show ww % "\n"
		
		
expToPat :: S.Exp -> S.Pat
expToPat xx
 = case xx of
	S.XVar sp v
	 | Var.isCtorName v
	 -> S.WCon v []

	 | otherwise
	 -> S.WVar v

 	S.XTuple sp xs	-> S.WTuple (map expToPat xs)

	S.XApp sp x1 x2
	 -> let	(S.XVar sp v : xs)	= S.flattenApps xx
	    in	S.WCon v (map expToPat xs)
		
	S.XList sp xx	-> S.WList (map expToPat xx)	

	S.XConst sp c	-> S.WConst c
	
	_	-> panic stage
		$ "expToPat: no match for " % show xx % "\n"
		
-----
instance Rewrite S.Label (D.Label Annot) where
 rewrite ll
  = case ll of
  	S.LIndex i	-> return $ D.LIndex none i
	
	S.LVar v	-> return $ D.LVar   none v


-----
rewriteTry
	:: SourcePos -> [D.Stmt Annot] -> D.Exp Annot -> [D.Alt Annot] 
	-> RewriteM (D.Exp Annot)

rewriteTry	   sp ssMore x aa
 = do
	tryExpV		<- newVarNI NameValue [Var.ISourcePos sp]
	tryExpVA	<- newVarNI NameValue [Var.ISourcePos sp]
		
	tryCatchV	<- newVarNI NameValue [Var.ISourcePos sp]
	tryCatchVA	<- newVarNI NameValue [Var.ISourcePos sp]

	d		<- newVarN  NameValue
	let aDefault	= D.AAlt sp [] (D.XApp sp (D.XVar sp primThrow) (D.XVar sp tryCatchVA))

	let exp	= D.XDo sp $
		[ D.SBind sp (Just tryExpV) 	(D.XLambda sp d x) ]
		++ ssMore ++
		[ D.SBind sp (Just tryCatchV) 	(D.XLambda sp tryCatchVA 
							(D.XMatch sp (Just $ D.XVar sp tryCatchVA) (aa ++ [aDefault])))
		, D.SBind sp Nothing 
			$ D.XApp sp 	(D.XApp sp	(D.XVar sp primTry)
							(D.XVar sp tryExpV))
					(D.XVar sp tryCatchV) 
		]

	return exp


addWithAlt :: 	(D.Exp Annot) -> (D.Alt Annot) -> (D.Alt Annot)
addWithAlt	w (D.AAlt sp aa x)
 = case x of
 	D.XDo sp ss	-> D.AAlt sp aa (D.XDo sp (ss ++ [D.SBind sp Nothing w]))
	_		-> D.AAlt sp aa (D.XDo sp [D.SBind sp Nothing x, D.SBind sp Nothing w])
	

-----
instance Rewrite S.Type S.Type where
 rewrite tt
  = case tt of
  	TVar k v			
	 -> return tt

	TFun t1 t2 eff clo
	 -> do	t1'	<- rewrite t1
	 	t2'	<- rewrite t2
		return	$ TFun t1' t2' eff clo
		
	TData v ts
	 -> do	ts'	<- rewrite ts
	 	return	$ TData v ts'
		
	TForall vks t
	 -> do	t'	<- rewrite t
	 	return	$ TForall vks t'
		
	TFetters fs t
	 -> do	t'	<- rewrite t
	 	return	$ TFetters fs t'
		
	TWild k		-> return tt
	
	TElaborate t	
	 -> do	let ?newVarN	= newVarN
	 	let ?getKind	= getKind
	 	t'	<- elaborateT t
	 	return	$ t'
		


	
