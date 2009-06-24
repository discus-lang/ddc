
-- Source.Desugar
--	This is the pre type inference desugarer.
--	Desugaring before the inferencer makes the error messages not as good, 
--	but the constraint slurper a lot smaller.
--   
module Source.Desugar
	( rewriteTree
	, rewrite)

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
import qualified Source.Error		as S

import qualified Desugar.Util		as D
import qualified Desugar.Pretty		as D
import qualified Desugar.Exp		as D
import qualified Desugar.Bits		as D

import Shared.Exp

import Source.Desugar.Base
import Source.Desugar.Patterns
import Source.Desugar.MergeBindings

import {-# SOURCE #-} Source.Desugar.ListComp

import Debug.Trace

-----
stage	= "Source.Desugar"

-----
rewriteTree 
	:: String		-- unique id
	-> Map Var Kind		-- kind table
	-> S.Tree SourcePos	-- header tree
	-> S.Tree SourcePos	-- source tree

	-> ( D.Tree Annot	-- desugared header tree
	   , D.Tree Annot	-- desugared source tree
	   , [S.Error] ) 	-- errors encountered during desugaring

rewriteTree unique kindMap hTree sTree
 = let	state	= RewriteS
		{ stateVarGen	= Var.XBind unique 0 
		, stateErrors	= [] }
	
	((hTree', sTree'), state')
		= runState (rewriteTreeM hTree sTree) state

   in	(hTree', sTree', stateErrors state')

rewriteTreeM :: S.Tree SourcePos -> S.Tree SourcePos -> RewriteM (D.Tree Annot, D.Tree Annot)
rewriteTreeM hTree sTree
 = do	hTreeR	<- liftM catMaybes $ mapM rewrite hTree
	hTreeP	<- rewritePatternsTreeM hTreeR

	sTreeR	<- liftM catMaybes $ mapM rewrite sTree
	sTreeP	<- rewritePatternsTreeM sTreeR

	return	(hTreeP, sTreeP)
 	

-- Top ---------------------------------------------------------------------------------------------
instance Rewrite (S.Top SourcePos) (Maybe (D.Top Annot)) where
 rewrite pp
  = case pp of
	S.PImportModule sp ms
	 ->	returnJ	$ D.PImport sp ms

	-- imported values 
	S.PForeign sp (S.OImport mName v tv to)
	 -> do	tv'	<- rewrite tv
		let v'	= case mName of
				Nothing		-> v
				Just seaName	-> v { Var.info = Var.info v ++ [Var.ISeaName seaName ]}
		
		let to'	= maybeJust to (let Just to2 = makeOpTypeT tv' in to2)
	 	returnJ $ D.PExtern sp v' tv' to'

	-- imported unboxed types
	S.PForeign sp (S.OImportUnboxedData name var k)
	 -> do	let var'	= var { Var.info = Var.info var ++ [Var.ISeaName name]}
		returnJ	$ D.PExternData sp name var' k

	-- types
	S.PTypeKind sp v k
	 -> returnJ $ D.PTypeKind sp v k

	-- data definitions
	S.PData sp v vs ctors
	 -> do	-- desugar field initialisation code
	 	ctors'	<- mapM (rewriteCtorDef sp) ctors
	 	returnJ	$ D.PData sp v vs ctors'
	
	S.PEffect sp v k
	 ->	returnJ	$ D.PEffect sp v k
	 
	S.PRegion sp v
	 ->	returnJ	$ D.PRegion sp v
	 
	-- classes
	S.PClass sp v k
	 ->	returnJ	$ D.PClass sp v k

	-- class dictionaries
 	S.PClassDict sp vC vks context sigs
	 -> do
	 	-- convert type param vars into actual types
		let tsParam	= map (\(v, k) -> TVar k v) vks

	 	-- For each member function in this class, quantify the class
		--	params and add the constraints.
	 	let makeSigs (vsT, t) = zip vsT (repeat t)

	 	let sigs'	= catMap makeSigs sigs

		returnJ		$ D.PClassDict sp vC tsParam [] sigs'

	-- class instances
	S.PClassInst sp vC ts context ss
	 -> do	
		-- merge pattern bindings
		ss'		<- mapM rewrite ss
		let (ss_merged, errs)
				= mergeBindings ss'
		mapM_ addError errs

		let context'	= map (\(v, vs) ->
					D.ClassContext v ts)
				$ context

		ts_rewrite	<- mapM rewrite ts

	 	returnJ		$ D.PClassInst sp vC ts context' ss_merged

	-- projections
	S.PProjDict sp t ss
	 -> do	ss'		<- mapM rewrite ss
	 	
		t'		<- rewrite t
		
		returnJ		$ D.PProjDict sp t' ss'
		

	S.PStmt (S.SSig sp v t)
	 -> do	t'	<- rewrite t

	 	returnJ	$ D.PSig sp v t

  	S.PStmt s
	 -> do	(D.SBind sp mV x)	<- rewrite s
	 	returnJ			$ D.PBind sp mV x
		
	_  ->	return	Nothing
	
-----
rewriteCtorDef sp (v, fs)
 = do	fs'	<- mapM rewriteField fs
 	return	$ D.CtorDef sp v fs'
	
rewriteField field
 = do	mX'	<- liftMaybe rewrite $ dInit field
 	return	$ field { dInit = mX' }


-- Exp ---------------------------------------------------------------------------------------------
instance Rewrite (S.Exp SourcePos) (D.Exp Annot) where
 rewrite xx
  = case xx of

	-- core language.
	S.XNil		-> return D.XNil

	S.XLit sp litFmt
	 -> return	$ D.XLit sp $ defaultLiteralFmt litFmt
	
	S.XVar sp v	
	 -> 	return	$ D.XVar 	sp v

	-- projections
	S.XProj sp x1 (S.JIndex sp2 x2)
	 -> do	x1'	<- rewrite x1
	 	x2'	<- rewrite x2
		return	$ D.XApp sp (D.XApp sp (D.XVar sp primIndex) x1') x2'
	
	S.XProj sp x1 (S.JIndexR sp2 x2)
	 -> do	x1'	<- rewrite x1
	 	x2'	<- rewrite x2
		return	$ D.XApp sp (D.XApp sp (D.XVar sp primIndexR) x1') x2'

	S.XProj sp x pj
	 -> do	x'	<- rewrite x
	 	pj'	<- rewrite pj
	 	return	$ D.XProj 	sp x' pj'

	S.XProjT sp t pj
	 -> do	t'	<- rewrite t
	 	pj'	<- rewrite pj

		return	$ D.XProjT sp t pj'
		
	S.XApp sp x1 x2
	 -> do	x1'	<- rewrite x1
	 	x2'	<- rewrite x2
		return	$ D.XApp sp x1' x2'
		
	S.XCase sp x aa
	 -> do	x'	<- rewrite x
	 	aa'	<- rewrite aa
		return	$ D.XMatch sp (Just x') aa'

	S.XDo sp ss
	 -> do	ss'		<- mapM rewrite ss
	
		-- desugar monadic bindings
		ss_demon	<- rewriteDoSS ss'

		-- merge pattern bindings
		let (ss_merged, errs) = mergeBindings ss_demon
		mapM_ addError errs

		return	$ D.XDo sp ss_merged
		
	-- Let and where expressions are treated similarly.
	--	They're just sugar for do expressions, and don't support mutual recursion.
	S.XLet sp ss x
	 -> do	ss'		<- rewrite ss
		x'		<- rewrite x
		let ssX		= ss' ++ [D.SBind sp Nothing x']
	 
		ssX_demon	<- rewriteDoSS ssX
	
 		-- merge pattern bindings
		let (ssX_merged, errs) = mergeBindings ssX_demon
		mapM_ addError errs
	 
		return	$ D.XDo sp ssX_merged

	S.XWhere sp x ss
	 -> do	ss'		<- mapM rewrite ss
		x'		<- rewrite x
	 	let ssX		= ss' ++ [D.SBind sp Nothing x']
	
		ssX_demon	<- rewriteDoSS ssX
	
 		-- merge pattern bindings
		let (ssX_merged, errs) = mergeBindings ssX_demon
		mapM_ addError errs

		return	$ D.XDo	sp ssX_merged
		
	S.XIfThenElse sp x1 x2 x3
	 -> do	x1'	<- rewrite x1
	 	x2'	<- rewrite x2
		x3'	<- rewrite x3
		return	$ D.XIfThenElse sp x1' x2' x3'
		
	-- lambda sugar.
	S.XLambdaPats sp ps x
	 -> do	x'	<- rewrite x
		ps'	<- mapM rewrite ps
	 	x2	<- makeMatchFunction sp ps' x'
		return x2

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
		$ "rewrite[Exp]: can't rewrite " % xx % "\n\n"
		%  show xx	% "\n"


-- Default literals to the machine type
--	This should really look out type bindings like
--	type Int = Int32 
--	instead of being hard-wired like this
defaultLiteralFmt :: LiteralFmt -> LiteralFmt
defaultLiteralFmt litFmt@(LiteralFmt lit fmt)
 = case (lit, fmt) of
	(LWord _,  Boxed)	-> LiteralFmt lit (BoxedBits   32)
	(LWord _,  Unboxed)	-> LiteralFmt lit (UnboxedBits 32)
	(LInt _,   Boxed)	-> LiteralFmt lit (BoxedBits   32)
	(LInt _,   Unboxed)	-> LiteralFmt lit (UnboxedBits 32)
	(LFloat _, Boxed)	-> LiteralFmt lit (BoxedBits   32)
	(LFloat _, Unboxed)	-> LiteralFmt lit (UnboxedBits 32)
	(LChar _,  Boxed)	-> LiteralFmt lit (BoxedBits   32)
	(LChar _,  Unboxed)	-> LiteralFmt lit (UnboxedBits 32)
	_			-> litFmt

-- Proj ---------------------------------------------------------------------------------------------
instance Rewrite (S.Proj SourcePos) (D.Proj Annot) where
 rewrite pp
  = case pp of
  	S.JField  sp v	-> return $ D.JField  sp v
	S.JFieldR sp v	-> return $ D.JFieldR sp v

	S.JIndex  sp x	
	 -> do	v	<- newVarN NameValue
	 	return	$ D.JField sp v

	S.JIndexR sp x	
	 -> do	v	<- newVarN NameValue
	 	return	$ D.JFieldR sp v


-- Stmt ---------------------------------------------------------------------------------------------
instance Rewrite (S.Stmt SourcePos) (D.Stmt Annot) where
 rewrite ss
  = case ss of
	S.SSig sp v t
	 -> do 	t'	<- rewrite t
	 	return	$ D.SSig sp v t

	S.SStmt sp x
	 -> do	x'	<- rewrite x
	 	return	$ D.SBind sp Nothing x'


	-- If the right is just a single expression, then we don't want
	-- 	to wrap it in a dummy match.
	S.SBindFun sp v [] [S.ADefault sp' x]
	 -> do	x'	<- rewrite x
		return	$ D.SBind sp (Just v) x'
				
	S.SBindFun sp v ps as
	 -> do	ps'	<- mapM rewrite ps
		as'	<- mapM rewrite as

		-- Make guards to deconstruct each of the patterns
		(vs, mGs)	<- liftM unzip $ mapM makeGuard ps'
		let newGs	= catMaybes mGs

		-- Add those guards to each alternative
		let asPat	= map (\a -> case a of
						D.AAlt sp gs x -> D.AAlt sp (newGs ++ gs) x)
				$ as'

		-- Add lambdas to the front to bind each of the arguments.
		let x2		= D.addLambdas sp vs 
				$ D.XMatch sp Nothing asPat

		return	$ D.SBind sp (Just v) x2

	S.SBindPat sp pat x
	 -> do	pat'	<- rewrite pat
	 	x'	<- rewrite x
		return	$ D.SBindPat sp pat' x'
	 	

	S.SBindMonadic sp pat x
	 -> do	pat'	<- rewrite pat
	 	x'	<- rewrite x
	 	return	$ D.SBindMonadic sp pat' x'



-- Alt ---------------------------------------------------------------------------------------------
instance Rewrite (S.Alt SourcePos) (D.Alt Annot) where
 rewrite aa
  = case aa of
	S.APat sp w x
	 -> do	w'		<- rewrite w
	  	x'		<- rewrite x
	 	return	$ D.AAlt sp [D.GCase sp w'] x'

	S.AAlt sp gs x
	 -> do	gs'	<- rewrite gs
	 	x'	<- rewrite x
		return	$ D.AAlt sp gs' x'

	S.ADefault sp x
	 -> do	x'	<- rewrite x
	 	return	$ D.AAlt sp [] x'


-- Guard ------------------------------------------------------------------------------------------
instance Rewrite (S.Guard SourcePos) (D.Guard Annot) where
 rewrite gg 
  = case gg of
	S.GExp sp w x
	 -> do	w'	<- rewrite w
	 	x'	<- rewrite x
		return	$ D.GExp sp w' x'
		
	S.GBool sp x
	 -> do	x'	<- rewrite x
	 	return	$ D.GExp sp (D.WConLabel sp primTrue []) x'
		
	

-- Pat --------------------------------------------------------------------------------------------
-- This is basic rewriting of the AST from S.Pat to D.Pat
--	includings renaming things like [] to Nil.
--	Introducing intermediate variables and the conversion to match
--	expressions takes place in Desugar.Patterns.
--
instance Rewrite (S.Pat SourcePos) (D.Pat Annot) where
 rewrite ww
  = case ww of
	S.WVar sp v
	 -> do	return	$ D.WVar sp v

	S.WLit sp litFmt
	 -> do	return	$ D.WLit sp $ defaultLiteralFmt litFmt
	
	S.WCon sp v ps
	 -> do	ps'	<- mapM rewrite ps
	 	return	$ D.WConLabelP sp
				(rewritePatVar v) 
				(zip [D.LIndex sp i | i <- [0..]] ps')
		
	S.WAt sp v p
	 -> do	p'	<- rewrite p
	 	return	$ D.WAt sp v p'
		
	S.WWildcard sp
	 -> do	v	<- newVarN NameValue
	 	return	$ D.WVar sp v
	 
	S.WConLabel sp v lvs
	 -> do	lvs'	<- mapZippedM 
	 			rewrite rewrite 
	 			lvs

	 	return	$ D.WConLabelP sp (rewritePatVar v) lvs'

	S.WUnit sp
	 -> do	return	$ D.WConLabel sp primUnit []

	S.WTuple sp ps
	 -> do	ps'	<- mapM rewrite ps
	 	return	$ D.WConLabelP sp
				(primTuple (length ps)) 
				(zip [D.LIndex sp i | i <- [0..]] ps')
		
	S.WCons sp p1 p2
	 -> do	p1'	<- rewrite p1
	 	p2'	<- rewrite p2
		return	$ D.WConLabelP sp
				primCons 
				[ (D.LIndex sp 0, p1')
				, (D.LIndex sp 1, p2') ]

	-- [] -> Nil
	S.WList sp []
	 -> 	return	$ D.WConLabelP sp primNil []

	-- [a, b, c] -> Cons a (Cons b (Cons c Nil))
	S.WList sp ps
	 -> 	rewritePatList sp ps

	_	-> panic stage	
		$ "rewrite[S.Pat]: can't rewrite " % show ww % "\n"
		
-- | Rewrite a source list pattern to individual constructor patterns.
rewritePatList 
	:: SourcePos 
	-> [S.Pat SourcePos] 
	-> RewriteM (D.Pat Annot)
	
rewritePatList sp []
 = do	return $ D.WConLabelP sp primNil []

rewritePatList sp (p:ps)
 = do	p'	<- rewrite p
	ps'	<- rewritePatList sp ps
	
	return	$ D.WConLabelP sp 
			primCons 
			[ (D.LIndex sp 0, p')
			, (D.LIndex sp 1, ps') ]
			
			
-- Label ------------------------------------------------------------------------------------------
instance Rewrite (S.Label SourcePos) (D.Label Annot) where
 rewrite ll
  = case ll of
  	S.LIndex sp i	-> return $ D.LIndex sp i
	S.LVar sp v	-> return $ D.LVar   sp v


-- Try syntax--------------------------------------------------------------------------------------
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
		[ D.SBind sp 	(Just tryExpV) 	
				(D.XLambda sp d x) ]
		++ ssMore ++
		[ D.SBind sp 	(Just tryCatchV) 	
				(D.XLambda sp tryCatchVA 
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
	

-- Monadic do syntax ------------------------------------------------------------------------------
-- | Desugar monadic do notation
rewriteDoSS :: [D.Stmt Annot] -> RewriteM [D.Stmt Annot]
rewriteDoSS []		= return []
rewriteDoSS (s : ss)
 = case s of
	D.SSig{}	
	 -> do	ss'	<- rewriteDoSS ss
	 	return	$ s : ss'

	D.SBind{}	
	 -> do	ss'	<- rewriteDoSS ss
	 	return	$ s : ss'

	D.SBindPat sp pat x
	 -> do	ss'		<- rewriteDoSS ss
	 	let xRest	= D.XDo sp ss'
		let g		= D.GExp sp pat x
		
		return	[ D.SBind sp Nothing 
				(D.XMatch sp Nothing [D.AAlt sp [g] xRest]) ]
				

 	D.SBindMonadic sp pat x 
	 -> do 	ss'	<- rewriteDoSS ss
	  	let xDo	= D.XDo sp ss'

		([var], xMatch)	<- makeMatchExp sp [pat] xDo

	        let xRest	= D.XLambda sp var xMatch

	    	return	[D.SBind sp Nothing 
				(D.XApp sp (D.XApp sp (D.XVar sp primBind) x) xRest)]
			
	
	


-- Type -------------------------------------------------------------------------------------------
instance Rewrite S.Type S.Type where
 rewrite tt
  = case tt of
  	TVar k v			
	 -> return tt

	TFun t1 t2 eff clo
	 -> do	t1'	<- rewrite t1
	 	t2'	<- rewrite t2
		return	$ TFun t1' t2' eff clo
		
	TData k v ts
	 -> do	ts'	<- rewrite ts
	 	return	$ TData k v ts'
		
	TForall b k t
	 -> do	t'	<- rewrite t
	 	return	$ TForall b k t'
		
	TFetters t fs
	 -> do	t'	<- rewrite t
	 	return	$ TFetters t' fs
	
	TBot k	-> return $ TBot k
	TTop k	-> return $ TTop k
		
	TWild k		-> return tt

	TElaborate ee t	-> return tt

