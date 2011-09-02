
-- | This is the pre type inference desugarer.
--   Desugaring before the inferencer makes the error messages not as good,
--   but the constraint slurper much easier to write.
--
--   We also default the formats of unsized literals like '5' to a format
--   suitable for the platform (like Int32).
--
module Source.Desugar
	( Annot
	, rewriteTree
	, rewrite
	, rewriteLetStmts )
where
import Util
import Type.Util
import Source.Desugar.Base
import Source.Desugar.Patterns
import Source.Desugar.MergeBindings
import Shared.VarPrim
import DDC.Base.SourcePos
import DDC.Base.Literal
import DDC.Main.Error
import DDC.Type.Data.CtorType
import DDC.Type
import DDC.Var
import Source.Pretty			()
import qualified Source.Exp		as S
import qualified DDC.Source.Error	as S
import qualified DDC.Desugar.Bits	as D
import qualified DDC.Desugar.Exp	as D
import qualified DDC.Type.Data.Base	as T
import qualified Data.Map		as Map

import {-# SOURCE #-} Source.Desugar.ListComp

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
		{ stateVarGen	= VarId unique 0
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
	 ->	returnJ	$ D.PImport (sp, Nothing) ms

	-- imported values
	S.PForeign sp (S.OImport mName v tv to)
	 -> do	tv'	<- rewrite tv
		let v'	= case mName of
				Nothing		-> v
				Just seaName	-> v { varInfo = varInfo v ++ [ISeaName seaName ]}

		let to'	= maybeJust to (let Just to2 = makeOpTypeT tv' in to2)
	 	returnJ $ D.PExtern (sp, Nothing) v' tv' to'

	-- imported unboxed types
	S.PForeign sp (S.OImportUnboxedData name var k)
	 -> do	let var'	= var { varInfo = varInfo var ++ [ISeaName name]}
		let Just ksParams
			= case k of
				KCon{}	-> Just []
				KFun{}	-> Just $ flattenKFun k

		vksParams	<- mapM (\k -> do let Just space = spaceOfKind k
						  v              <- newVarN space
						  return (v, k))
					ksParams

		returnJ	$ D.PData (sp, Nothing)
			$ T.DataDef
				{ T.dataDefName			= var'
				, T.dataDefSeaName		= Just name
				, T.dataDefParams		= vksParams
				, T.dataDefCtors		= Map.empty
				, T.dataDefMaterialVars		= Nothing
				, T.dataDefImmaterialVars	= Nothing }

	-- types
	S.PKindSig sp v k
	 | isValueKind (resultKind k)
	 -> do
		let Just ksParams
			= case k of
				KCon{}	-> Just []
				KFun{}	-> Just $ flattenKFun k

		vksParams	<- mapM (\k -> do let Just space = spaceOfKind k
						  v              <- newVarN space
						  return (v, k))
					ksParams

	 	returnJ $ D.PData (sp, Nothing)
			$ T.DataDef
				{ T.dataDefName			= v
				, T.dataDefSeaName		= Nothing
				, T.dataDefParams		= vksParams
				, T.dataDefCtors		= Map.empty
				, T.dataDefMaterialVars		= Nothing
				, T.dataDefImmaterialVars	= Nothing }

	 | otherwise
	 -> returnJ $ D.PKindSig (sp, Nothing) v k

	S.PTypeSynonym sp v t
	 -> returnJ	$ D.PTypeSynonym (sp, Nothing) v t

	-- data definitions
	S.PData sp v vs ctors
	 -> do	dataDef	<- makeDataDef v vs ctors
	 	returnJ	$ D.PData (sp, Nothing) dataDef

	S.PRegion sp v
	 ->	returnJ	$ D.PRegion (sp, Nothing) v

	-- classes
	S.PClass sp v s
	 ->	returnJ	$ D.PSuperSig (sp, Nothing) v s

	-- class dictionaries
 	S.PClassDict sp vC vks _ sigs
	 -> do
	 	-- convert type param vars into actual types
		let tsParam	= map (\(v, k) -> TVar k $ UVar v) vks

	 	-- For each member function in this class, quantify the class
		--	params and add the constraints.
	 	let makeSigs (vsT, t) = zip vsT (repeat t)

	 	let sigs'	= catMap makeSigs sigs

		returnJ		$ D.PClassDecl (sp, Nothing) vC tsParam sigs'

	-- class instances
	S.PClassInst sp vC ts _ ss
	 -> do
		-- merge pattern bindings
		ss'		<- mapM rewrite ss
		let (ss_merged, errs)
				= mergeBindings ss'
		mapM_ addError errs

		ts_rewrite	<- mapM rewrite ts
	 	returnJ		$ D.PClassInst (sp, Nothing) vC ts ss_merged

	-- projections
	S.PProjDict sp t ss
	 -> do	ss'		<- mapM rewrite ss
		let (ss_merged, errs)
				= mergeBindings ss'
		mapM_ addError errs

		t'		<- rewrite t

		returnJ		$ D.PProjDict (sp, Nothing) t' ss_merged


	S.PStmt (S.SSig sp sigMode vs t)
	 -> do	t'	<- rewrite t
	 	returnJ	$ D.PTypeSig (sp, Nothing) sigMode vs t

  	S.PStmt s
	 -> do	(D.SBind an (Just v) x)	<- rewrite s
	 	returnJ			$ D.PBind an v x

	_  ->	return	Nothing


-- | Make a data type definition.
makeDataDef
	:: Var				-- ^ Name of data type constructor.
	-> [Var]			-- ^ Params to data type constructor.
	-> [S.CtorDef SourcePos]	-- ^ Constructors in type
	-> RewriteM T.DataDef

makeDataDef vData vsParam ctors
 = do	ctors'	<- zipWithM (makeCtorDef vData vsParam)
			[0..]
			ctors

	return	$ T.DataDef
		{ T.dataDefName		= vData
		, T.dataDefSeaName	= Nothing
		, T.dataDefParams	= [(v, let Just k = defaultKindOfVar v in k) | v <- vsParam]
		, T.dataDefCtors	= Map.fromList
					[ (T.ctorDefName ctor, ctor)
						| ctor	<- ctors']
		, T.dataDefMaterialVars	  = Nothing
		, T.dataDefImmaterialVars = Nothing }

-- | Make a data constructor definition.
makeCtorDef
	:: Var				-- ^ Name of data type constructor.
	-> [Var]			-- ^ Params to data type constructor.
	-> Int				-- ^ Tag of constructor (order in data type definition).
	-> S.CtorDef SourcePos
	-> RewriteM T.CtorDef

makeCtorDef vData vsParams tag (S.CtorDef vCtor fields)
 = do	let fieldIxs	= zip 	(mapMaybe S.dataFieldLabel fields)
				([0..] :: [Int])

	tCtor		<- makeCtorType
				newVarN
				vData vsParams
				vCtor (map (fixupKindsT . S.dataFieldType) fields)

	return	$ T.CtorDef
		{ T.ctorDefName		= vCtor
		, T.ctorDefType		= tCtor
		, T.ctorDefArity	= length fields
		, T.ctorDefTag		= tag
		, T.ctorDefFields	= Map.fromList fieldIxs }



-- Exp ---------------------------------------------------------------------------------------------
instance Rewrite (S.Exp SourcePos) (D.Exp Annot) where
 rewrite xx
  = case xx of

	-- core language.
	S.XNil		-> return D.XNil

	S.XLit sp litFmt
	 -> return	$ D.XLit (sp, Nothing) $ defaultLiteralFmt litFmt

	S.XVar sp v
	 -> 	return	$ D.XVar (sp, Nothing) v

	-- projections
	S.XProj sp x1 (S.JIndex _ x2)
	 -> do	x1'	<- rewrite x1
	 	x2'	<- rewrite x2
	 	let ann = (sp, Nothing)
		return	$ D.XApp ann (D.XApp ann (D.XVar ann primIndex) x1') x2'

	S.XProj sp x1 (S.JIndexR _ x2)
	 -> do	x1'	<- rewrite x1
	 	x2'	<- rewrite x2
	 	let ann = (sp, Nothing)
		return	$ D.XApp ann (D.XApp ann (D.XVar ann primIndexR) x1') x2'

	S.XProj sp x pj
	 -> do	x'	<- rewrite x
	 	pj'	<- rewrite pj
	 	return	$ D.XProj (sp, Nothing) x' pj'

	S.XProjT sp t pj
	 -> do	t'	<- rewrite t
	 	pj'	<- rewrite pj

		return	$ D.XProjT (sp, Nothing) t pj'

	S.XApp sp x1 x2
	 -> do	x1'	<- rewrite x1
	 	x2'	<- rewrite x2
		return	$ D.XApp (sp, Nothing) x1' x2'

	S.XCase sp x aa
	 -> do	x'	<- rewrite x
	 	aa'	<- rewrite aa
		return	$ D.XMatch (sp, Nothing) (Just x') aa'

	S.XDo sp ss
	 -> do	ss'		<- mapM rewrite ss

		-- desugar monadic bindings
		ss_demon	<- rewriteDoSS ss'

		-- merge pattern bindings
		let (ss_merged, errs) = mergeBindings ss_demon
		mapM_ addError errs

		return	$ D.XDo (sp, Nothing) ss_merged

	-- Let and where expressions are treated similarly.
	--	They're just sugar for do expressions, and don't support mutual recursion.
	S.XLet sp ss x
	 -> do	x'		<- rewrite x
		rewriteLetStmts sp ss x'

	S.XWhere sp x ss
	 -> do	x'		<- rewrite x
		rewriteLetStmts sp ss x'

	S.XIfThenElse sp x1 x2 x3
	 -> do	x1'	<- rewrite x1
	 	x2'	<- rewrite x2
		x3'	<- rewrite x3
		return	$ D.XIfThenElse (sp, Nothing) x1' x2' x3'

	-- lambda sugar.
	S.XLambdaPats sp ps x
	 -> do	x'	<- rewrite x
		ps'	<- mapM rewrite ps
	 	makeMatchFunction sp ps' x' Nothing


	S.XLambdaCase sp alts
	 -> do	alts'	<- rewrite alts
	 	var_	<- newVarN NameValue
		let var	= var_ { varInfo = [ISourcePos sp] }
		let ann = (sp, Nothing)
		return	$ D.XLambda ann var (D.XMatch ann (Just $ D.XVar ann var) alts')

	S.XLambdaProj sp j xs
	 -> do	var_	<- newVarN NameValue
	 	let var	= var_ { varInfo = [ISourcePos sp] }
		let ann = (sp, Nothing)
		j'	<- rewrite j
		xs'	<- rewrite xs

		return	$ D.XLambda ann var (D.unflattenApps ann (D.XProj ann (D.XVar ann var) j') xs')

	-- match sugar.
	S.XMatch sp aa
	 -> do	aa'	<- rewrite aa
	 	return	$ D.XMatch (sp, Nothing) Nothing aa'


	-- exception sugar.
	S.XThrow sp x
	 -> do	x'	<- rewrite x
		let ann = (sp, Nothing)
	 	return	$ (D.XApp ann (D.XVar ann primThrow) x')

	S.XTry sp x aa Nothing
	 -> do	x'	<- rewrite x
		aa'	<- rewrite aa
	 	rewriteTry sp [] x' aa'

	S.XTry sp x aa (Just w)
	 -> do
		x'	<- rewrite x
		w'	<- rewrite w
		aa'	<- rewrite aa

		withV	<- newVarNI NameValue [ISourcePos sp]
		withVA	<- newVarNI NameValue [ISourcePos sp]
		d	<- newVarN  NameValue

		let annot	= (sp, Nothing)
		let ssMore	= [D.SBind annot (Just withV) (D.XLambda annot d w')]
		let appW	= D.XApp annot (D.XVar annot withV) (D.XVar annot primUnit)

		let aaWith	= map (addWithAlt appW) aa'
		rewriteTry sp ssMore x' aaWith

	-- imperative sugar
	S.XWhen	sp testX bodyX
	 -> do	testX'	<- rewrite testX
	 	bodyX'	<- rewrite bodyX
		let ann = (sp, Nothing)
		return	$ D.XMatch ann (Just testX')
				[ D.AAlt ann	[D.GCase ann (D.WConLabel ann primTrue [])]
						bodyX'

				, D.AAlt ann	[]
						(D.XVar ann primUnit) ]

	S.XUnless sp testX bodyX
	 -> do	testX'	<- rewrite testX
	 	bodyX'	<- rewrite bodyX
	 	let ann = (sp, Nothing)
	 	return	$ D.XMatch ann (Just testX')
		 		[ D.AAlt ann	[D.GCase ann (D.WConLabel ann primFalse [])]
						bodyX'

				, D.AAlt ann 	[]
						(D.XVar ann primUnit) ]

	S.XWhile sp testX bodyX
	 -> do	d1	<- newVarN NameValue
		vLoop	<- newVarN NameValue
		testX'	<- rewrite testX
		bodyX'	<- rewrite bodyX
		let ann = (sp, Nothing)
		let loopX	= D.XLambda ann d1
				$ D.XDo ann
				[ D.SBind ann Nothing
					(D.XMatch ann (Just testX')
						[ D.AAlt ann	[D.GCase ann (D.WConLabel ann primFalse [])]
								(D.XApp ann (D.XVar ann primThrow) (D.XVar ann primExceptionBreak))

						, D.AAlt ann	[]
								(D.XVar ann primUnit) ])

				, D.SBind ann Nothing bodyX' ]

		return		$ D.XDo ann
				[ D.SBind ann (Just vLoop) 	$ loopX
				, D.SBind ann Nothing		$ D.XApp ann (D.XVar ann primGateLoop) (D.XVar ann vLoop)]

	S.XBreak sp
	 -> do	let annot = (sp, Nothing)
		return	$ D.XApp annot (D.XVar annot primThrow) (D.XVar annot primExceptionBreak)


	-- list sugar
	S.XList sp xs
	 -> do	let ann	= (sp, Nothing)
		let makeList = \xx -> case xx of
	 		[]	-> D.XVar ann primNil
			(x:xs)	-> D.XApp ann (D.XApp ann (D.XVar ann primCons) x) (makeList xs)

		xs'	<- mapM rewrite xs
		return	$ makeList xs'

	S.XListRange sp True x1 Nothing (Just x2)
	 -> do	x1'	<- rewrite x1
	 	x2'	<- rewrite x2
	 	let ann = (sp, Nothing)
		return	$ D.XApp ann (D.XApp ann (D.XVar ann primRangeL) x1') x2'

	S.XListRange sp False x1 Nothing (Just x2)
	 -> do	x1'	<- rewrite x1
	 	x2'	<- rewrite x2
		let ann = (sp, Nothing)
		return	$ D.XApp ann (D.XApp ann (D.XVar ann primRange) x1') x2'

	S.XListRange sp True x1 Nothing Nothing
	 -> do	x1'	<- rewrite x1
		let ann = (sp, Nothing)
	  	return	$ D.XApp ann (D.XVar ann primRangeInfL) x1'

	S.XListRange sp False x1 (Just x2) (Just x3)
	 -> do	x1'	<- rewrite x1
		x2'	<- rewrite x2
		x3'	<- rewrite x3
		let ann = (sp, Nothing)
		return	$ D.XApp ann (D.XApp ann (D.XApp ann (D.XVar ann primRangeStep) x1') x2') x3'

	S.XListRange sp True x1 (Just x2) Nothing
	 -> do	x1'	<- rewrite x1
		x2'	<- rewrite x2
		let ann	= (sp, Nothing)
		return	$ D.XApp ann (D.XApp ann (D.XVar ann primRangeInfStepL) x1') x2'

	S.XListComp{}
	 -> 	rewriteListComp xx


	-- data expression/pattern sugar.
	S.XTuple sp xx
	 -> do	xx'	<- rewrite xx
		let ann	= (sp, Nothing)
	 	return	$ D.unflattenApps ann (D.XVar ann $ primTuple (length xx)) xx'

	_	-> panic stage
		$ "rewrite[Exp]: can't rewrite " % xx % "\n\n"
		%  show xx	% "\n"

-- | Used for rewriting let/where-like statements into a use of the do notation.
-- Takes a body of the let that has already been rewritten.
rewriteLetStmts :: SourcePos -> [S.Stmt SourcePos] -> D.Exp Annot -> RewriteM (D.Exp Annot)
rewriteLetStmts sp ss x_body'
  = do	ss'		<- mapM rewrite ss
	let annot	= (sp, Nothing)
	let ssX		= ss' ++ [D.SBind annot Nothing x_body']

	ssX_demon	<- rewriteDoSS ssX

	-- merge pattern bindings
	let (ssX_merged, errs) = mergeBindings ssX_demon
	mapM_ addError errs

	return	$ D.XDo	annot ssX_merged


-- Proj ---------------------------------------------------------------------------------------------
instance Rewrite (S.Proj SourcePos) (D.Proj Annot) where
 rewrite pp
  = case pp of
  	S.JField  sp v	-> return $ D.JField  (sp, Nothing) v
	S.JFieldR sp v	-> return $ D.JFieldR (sp, Nothing) v

	S.JIndex  sp x
	 -> do	v	<- newVarN NameValue
	 	return	$ D.JField (sp, Nothing) v

	S.JIndexR sp x
	 -> do	v	<- newVarN NameValue
	 	return	$ D.JFieldR (sp, Nothing) v


-- Stmt ---------------------------------------------------------------------------------------------
instance Rewrite (S.Stmt SourcePos) (D.Stmt Annot) where
 rewrite ss
  = case ss of
	S.SSig sp sigMode vs t
	 -> do 	t'	<- rewrite t
	 	return	$ D.SSig (sp, Nothing) sigMode vs t

	S.SStmt sp x
	 -> do	x'	<- rewrite x
	 	return	$ D.SBind (sp, Nothing) Nothing x'


	-- If the right is just a single expression, then we don't want
	-- 	to wrap it in a dummy match.
	S.SBindFun sp v [] [S.ADefault sp' x]
	 -> do	x'	<- rewrite x
		return	$ D.SBind (sp, Nothing) (Just v) x'

	S.SBindFun sp v ps as
	 -> do	ps'	<- mapM rewrite ps
		as'	<- mapM rewrite as

		-- Make guards to deconstruct each of the patterns
		(vs, mGs)	<- liftM unzip $ mapM makeGuard ps'
		let newGs	= catMaybes mGs
		let annot	= (sp, Nothing)

		-- Add those guards to each alternative
		let asPat	= map (\a -> case a of
						D.AAlt annot gs x -> D.AAlt annot (newGs ++ gs) x)
				$ as'

		-- Add lambdas to the front to bind each of the arguments.
		let x2		= D.addLambdas annot vs
				$ D.XMatch annot Nothing asPat

		return	$ D.SBind annot (Just v) x2

	S.SBindPat sp pat x
	 -> do	pat'	<- rewrite pat
	 	x'	<- rewrite x
		return	$ D.SBindPat (sp, Nothing) pat' x'


	S.SBindMonadic sp pat x
	 -> do	pat'	<- rewrite pat
	 	x'	<- rewrite x
	 	return	$ D.SBindMonadic (sp, Nothing) pat' x'



-- Alt ---------------------------------------------------------------------------------------------
instance Rewrite (S.Alt SourcePos) (D.Alt Annot) where
 rewrite aa
  = case aa of
	S.APat sp w x
	 -> do	w'		<- rewrite w
	  	x'		<- rewrite x
	  	let annot	= (sp, Nothing)
	 	return	$ D.AAlt annot [D.GCase annot w'] x'

	S.AAlt sp gs x
	 -> do	gs'	<- rewrite gs
	 	x'	<- rewrite x
		return	$ D.AAlt (sp, Nothing) gs' x'

	S.ADefault sp x
	 -> do	x'	<- rewrite x
	 	return	$ D.AAlt (sp, Nothing) [] x'


-- Guard ------------------------------------------------------------------------------------------
instance Rewrite (S.Guard SourcePos) (D.Guard Annot) where
 rewrite gg
  = case gg of
	S.GExp sp w x
	 -> do	w'	<- rewrite w
	 	x'	<- rewrite x
		return	$ D.GExp (sp, Nothing) w' x'

	S.GBool sp x
	 -> do	x'	<- rewrite x
		let ann = (sp, Nothing)
	 	return	$ D.GExp ann (D.WConLabel ann primTrue []) x'



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
	 -> do	return	$ D.WVar (sp, Nothing) v

	S.WLit sp litFmt
	 -> do	return	$ D.WLit (sp, Nothing) $ defaultLiteralFmt litFmt

	S.WCon sp v ps
	 -> do	ps'	<- mapM rewrite ps
		let ann = (sp, Nothing)
	 	return	$ D.WConLabelP ann
				(rewritePatVar v)
				(zip [D.LIndex ann i | i <- [0..]] ps')

	S.WAt sp v p
	 -> do	p'	<- rewrite p
	 	return	$ D.WAt (sp, Nothing) v p'

	S.WWildcard sp
	 -> do	v	<- newVarN NameValue
	 	return	$ D.WVar (sp, Nothing) v

	S.WConLabel sp v lvs
	 -> do	lvs'	<- mapZippedM
	 			rewrite rewrite
	 			lvs

	 	return	$ D.WConLabelP (sp, Nothing) (rewritePatVar v) lvs'

	S.WUnit sp
	 -> do	return	$ D.WConLabel (sp, Nothing) primUnit []

	S.WTuple sp ps
	 -> do	ps'	<- mapM rewrite ps
		let ann = (sp, Nothing)
	 	return	$ D.WConLabelP ann
				(primTuple (length ps))
				(zip [D.LIndex ann i | i <- [0..]] ps')

	S.WCons sp p1 p2
	 -> do	p1'	<- rewrite p1
	 	p2'	<- rewrite p2
	 	let ann = (sp, Nothing)
		return	$ D.WConLabelP ann
				primCons
				[ (D.LIndex ann 0, p1')
				, (D.LIndex ann 1, p2') ]

	-- [] -> Nil
	S.WList sp []
	 -> 	return	$ D.WConLabelP (sp, Nothing) primNil []

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
 = do	return $ D.WConLabelP (sp, Nothing) primNil []

rewritePatList sp (p:ps)
 = do	p'	<- rewrite p
	ps'	<- rewritePatList sp ps
	let ann = (sp, Nothing)
	return	$ D.WConLabelP ann
			primCons
			[ (D.LIndex ann 0, p')
			, (D.LIndex ann 1, ps') ]


-- Label ------------------------------------------------------------------------------------------
instance Rewrite (S.Label SourcePos) (D.Label Annot) where
 rewrite ll
  = case ll of
  	S.LIndex sp i	-> return $ D.LIndex (sp, Nothing) i
	S.LVar sp v	-> return $ D.LVar   (sp, Nothing) v


-- Try syntax--------------------------------------------------------------------------------------
rewriteTry
	:: SourcePos -> [D.Stmt Annot] -> D.Exp Annot -> [D.Alt Annot]
	-> RewriteM (D.Exp Annot)

rewriteTry	   sp ssMore x aa
 = do
	tryExpV		<- newVarNI NameValue [ISourcePos sp]
	tryExpVA	<- newVarNI NameValue [ISourcePos sp]

	tryCatchV	<- newVarNI NameValue [ISourcePos sp]
	tryCatchVA	<- newVarNI NameValue [ISourcePos sp]

	d		<- newVarN  NameValue
	let annot	= (sp, Nothing)
	let aDefault	= D.AAlt annot [] (D.XApp annot (D.XVar annot primThrow) (D.XVar annot tryCatchVA))

	let exp	= D.XDo annot $
		[ D.SBind annot (Just tryExpV)
				(D.XLambda annot d x) ]
		++ ssMore ++
		[ D.SBind annot 	(Just tryCatchV)
				(D.XLambda annot tryCatchVA
					(D.XMatch annot (Just $ D.XVar annot tryCatchVA) (aa ++ [aDefault])))

		, D.SBind annot Nothing
			$ D.XApp annot 	(D.XApp annot (D.XVar annot primTry)
							(D.XVar annot tryExpV))
					(D.XVar annot tryCatchV)
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


 	D.SBindMonadic annot pat x
	 -> do 	ss'	<- rewriteDoSS ss
	  	let xDo	= D.XDo annot ss'

		([var], xMatch)	<- makeMatchExp (fst annot) [pat] xDo Nothing

	        let xRest	= D.XLambda annot var xMatch

	    	return	[D.SBind annot Nothing
				(D.XApp annot (D.XApp annot (D.XVar annot primBind) x) xRest)]


-- Type -------------------------------------------------------------------------------------------
instance Rewrite Type Type where
 rewrite tt
  = case tt of
	TForall b k t
	 -> do	t'	<- rewrite t
	 	return	$ TForall b k t'

	TConstrain t crs
	 -> do	t'	<- rewrite t
		return	$ TConstrain t' crs

	TApp t1 t2
	 -> do	t1'	<- rewrite t1
	 	t2'	<- rewrite t2
		return	$ TApp t1' t2'

	TCon{}		-> return tt
	TVar{}		-> return tt

	TSum _ []	-> return tt

	_ -> panic stage $ "rewrite[Type]: no match for " % tt

