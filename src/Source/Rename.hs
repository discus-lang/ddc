
-- | Apply scoping rules and add VarBinds to every variable in the program.
module Source.Rename
	( Rename
	, renameData
	, renameTrees )
where

-----
import qualified Debug.Trace	as Debug
import Util

-----
import Shared.Error		(panic)
import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var
import Shared.Var		(NameSpace (..), Module(..))
import Shared.VarUtil		(isCtorName)
import Shared.Pretty
import Shared.Base

import qualified Debug.Trace	as Debug

import Source.Exp
import Source.Util
import Source.Horror
import Source.RenameM
import Source.Slurp
import Source.Error

import Type.Util
import Type.Plate.Collect

-----
stage	= "Source.Rename"

-- Tree --------------------------------------------------------------------------------------------
renameTrees
	:: [(Module, Tree SourcePos)]
	-> RenameM [(Module, Tree SourcePos)]

renameTrees 
	(mTree1: mTreeHs)
 = do
 	let mTree1'	= mTree1

	mTreeHs'	<- renameTrees' mTreeHs	
	[mTree1']	<- renameTrees' [mTree1']	
				
	return 	$ (mTree1' : mTreeHs')

renameTrees' mTrees
 = do
	-- Slurp out all the top-level names.
	let topNamesHs	= concat 
			$ map (\(m, tree) -> [(v, m) | v <- catMap slurpTopNames tree])
			$ mTrees
		
	-- Add them to the rename state.
	mapM_	(\(v, m) 
		  -> lbindZ 
			v { Var.nameModule	= m })
		topNamesHs
		
	-- Now rename trees
	mTrees'	<- mapM (\(m, tree) 
			-> do	tree' <- renameTree m tree
				return (m, tree'))
				mTrees
				
	return mTrees'
 	

renameTree :: Module -> Tree SourcePos -> RenameM (Tree SourcePos)
renameTree m tree
 = do	
 	modify (\s -> s { stateCurrentModule = m })
	tree'	<- rename tree
	return tree'
	
-- Top ---------------------------------------------------------------------------------------------
instance Rename (Top SourcePos) where
 rename	top
  = case top of
	PPragma sp es
	 -> do 	return	$ PPragma sp es 	

	PModule sp m
	 -> do 	m'	<- rename m
		modify (\s -> s { stateCurrentModule = m' })
		return	$ PModule sp m'

 	PImportExtern sp v tv to
	 -> do	v'	<- lookupV v
		tv'	<- local $ rename tv
		to'	<- local $ rename to
		return	$ PImportExtern sp v' tv' to'

	PImportModule sp ms
	 -> do	ms'	<- rename ms
		return	$ PImportModule sp ms'

	PForeign sp f
	 -> do	f'	<- rename f
	 	return	$ PForeign sp f'

	PType sp v t		
	 -> do 	v'	<- lookupV v
	 	t'	<- local $ rename t
		return	$ PType sp v' t'
		
	PInfix sp m i vs
	 -> do 	vs'	<- mapM lbindV vs
		return	$ PInfix sp m i vs'
	
	PData sp v vs cs		
	 -> do	(v', vs', cs')	<- renameData v vs cs
		return	$ PData sp v' vs' cs' 
	
	PRegion sp v
	 -> do	v'	<- lookupN NameRegion v
	 	return	$ PRegion sp v'
	
	PEffect sp v k
	 -> do 	v'	<- lookupN NameEffect v
		return	$ PEffect sp v' k

	PStmt	s
	 -> -- local
	    do	{-(case takeStmtBoundV s of
	  	 	Just v	-> do 
				lbindN_shadow NameValue v
				return ()
				
			Nothing	-> 
				return ())
	 	-}
	 	s'	<- rename s
		return	$ PStmt s'

	
	-- classes
	PClass sp v k
	 -> do 	v'	<- lookupN NameClass v
	 	return	$ PClass sp v' k

	PClassDict sp v vs inh sigs
	 -> do 	v'	<- lbindN NameClass v

		(vs', inh', sigs')
		 <- local
		 $ do	vs'	<- mapM (lbindN NameType) vs
			inh'	<- mapM renameClassInh inh
			sigs'	<- mapM renameClassSig sigs
			return	(vs', inh', sigs')
	
		return	$ PClassDict sp v' vs' inh' sigs'

	PClassInst sp v ts inh stmts
	 -> do
	 	v'	<- lbindN NameClass v

		(ts', inh')
		 <- local
		 $ do	ts'	<- rename ts
			inh'	<- mapM renameInstInh inh
			return	(ts', inh')

		stmts'	<- rename stmts

		return	$ PClassInst sp v' ts' inh' stmts'			

	-- projections
	PProjDict sp t ss
	 -> do	
	 	-- The way the projection dict is parsed, the projection funtions end up in the wrong namespace, 
		--	NameValue. Convert them to NameField vars here.
		--	
	 	m	<- gets stateCurrentModule
		let fixupV v	= v 	{ Var.nameSpace 	= NameField
					, Var.nameModule 	= m }
		
		let ssF		= map (\s -> case s of 
					SSig  sp v t		-> SSig   sp 	(fixupV v) t
					SBindPats sp v xs x	-> SBindPats sp	(fixupV v) xs x)
			$ ss
	 
	   	t' 	<- local
		 	 $ do	t'	<- rename t
				return	t'
	
		ss'	<- rename ssF
		return	$ PProjDict sp t' ss'

	
-- Module ------------------------------------------------------------------------------------------
instance Rename Module where
 rename m	= return m
 
 
-- Foreign -----------------------------------------------------------------------------------------
instance Rename (Foreign SourcePos) where
 rename ff
  = case ff of
  	OImport f
	 -> do	f'	<- rename f	
	 	return	$ OImport f'
		
	OExport f
	 -> do	f'	<- rename f
	 	return	$ OExport f'
		
	OCCall mS v t
	 -> local
	 $ do	v'	<- lookupV v
	 	t'	<- rename t
		return	$ OCCall mS v' t'
		
	OExtern mS v tv to 
	 -> local
	 $  do 	v'	<- lookupV v
	 	tv'	<- rename tv
		to'	<- rename to
		return	$ OExtern mS v' tv' to' 


-- Classes -----------------------------------------------------------------------------------------
-- all this stuff should really have it's own constructor
--
renameClassInh :: (Var, [Var])  -> RenameM (Var, [Var])
renameClassInh	  (v, vs)
 = do	v'	<- lbindN NameClass v
	vs'	<- mapM (lbindN NameType) vs
	return	(v, vs)


renameClassSig :: ([Var], Type) -> RenameM ([Var], Type)
renameClassSig    (vs, t)
 = local
 $ do 	vs'	<- mapM lbindV vs
	t'	<- rename t
	return	(vs', t')


renameInstInh :: (Var, [Type])	-> RenameM (Var, [Type])
renameInstInh    (v, ts)
 = do
 	v'	<- lookupN NameClass v
	ts'	<- rename ts
	return	(v', ts')
		
renameData 	
	:: Var -> [Var] -> [(Var, [DataField (Exp SourcePos) Type])]	
	-> RenameM (Var, [Var], [Ctor SourcePos])

renameData v vs cs
 = local
 $ do 	v'	<- lookupN NameType v
	vs'	<- mapM bindZ vs
	cs'	<- mapM renameCtor cs
	return	(v', vs', cs')

renameCtor 
	:: (Var, [DataField (Exp SourcePos) Type])	
	-> RenameM (Var, [DataField (Exp SourcePos) Type])

renameCtor	(v, fs)
 = do	v'	<- lookupV v
	fs'	<- mapM rename fs
	return	(v', fs')


-- DataField ---------------------------------------------------------------------------------------
instance Rename (DataField (Exp SourcePos) Type) where
 rename df
  = do	
 	m	<- gets stateCurrentModule
	let fixupV v	= v 	{ Var.nameSpace 	= NameField
				, Var.nameModule 	= ModuleNil }

  
  	mLabel'	<- case dLabel df of
			Nothing		-> return Nothing
			Just label
			 -> do 	label'	<- lbindN NameField label
				return	$ Just $ fixupV label'

	t'	<- rename $ dType df
	mExp'	<- rename $ dInit df
	 	
	return 	df
		{ dLabel	= mLabel'
		, dType		= t'
		, dInit		= mExp' }
		
	
-- Expressions -----------------------------------------------------------------
instance Rename (Exp SourcePos) where 
 rename exp
  = case exp of

	-- core
	XConst sp c		
	 -> return exp

	XVar sp v		
	 -> do 	v'	<- lookupV v
		return	$ XVar sp v'

	XProj sp x proj
	 -> do 	x'	<- rename x
		proj'	<- rename proj
		return	$ XProj sp x' proj'

	XProjT sp t proj
	 -> do	t'	<- rename t
	 	proj'	<- rename proj
		return	$ XProjT sp t' proj'

	XLambda sp v e	
	 -> local
	 $  do	v'	<- bindV v
		e'	<- rename e
		return	$ XLambda sp v' e'

	XApp sp e1 e2	
	 -> do 	e1'	<- rename e1
		e2'	<- rename e2
		return	$ XApp sp e1' e2'

	XCase sp e1 cs	
	 -> do 	e1'	<- rename e1
		cs'	<- rename cs
		return	$ XCase sp e1' cs'

	XDo sp ss 
	 -> local
	 $ do 	ss'	<- renameSs ss
		return	$  XDo sp ss'

 	XLet sp ss e	
	 -> local
	 $ do 	ss'	<- renameSs ss
		e'	<- rename e
		return	$ XLet sp ss' e'
		
	XWhere sp x ss
	 -> local
	 $ do	ss'	<- renameSs ss
		x'	<- rename x
		return	$ XWhere sp x' ss'
		

	XIfThenElse sp e1 e2 e3 
	 -> do 	e1'	<- rename e1
		e2'	<- rename e2
		e3'	<- rename e3
		return	$ XIfThenElse sp e1' e2' e3'
		
	-- oop
	XObjField sp v
	 -> do	objV	<- peekObjectVar
		v'	<- lbindN NameField v
		return	$ XProj sp (XVar sp objV) (JField sp v')
		
	-- sugar
	XLambdaPats sp ps x
	 -> local
	 $ do 	(ps', _)	<- liftM unzip $ mapM bindPatternExp ps
		x'		<- rename x
		return	$ XLambdaPats sp ps' x'

	XLambdaCase sp cs
	 -> do 	cs'	<- rename cs
		return	$ XLambdaCase sp cs'
		
	XLambdaProj sp j xs
	 -> do	j'	<- rename j
	 	xs'	<- rename xs
	 	return	$ XLambdaProj sp j' xs'
		
	-- defix sugar
	XOp sp v 
	 | Var.name v	== "@"
	 -> 	return	$ XOp sp v
	 
	 | otherwise
	  -> do	v'	<- lookupV v
	 	return	$ XOp sp v'

	XDefix sp es 
	 -> do 	es'	<- rename es
		return	$ XDefix sp es'

	XDefixApps sp es
	 -> do	es'	<- rename es
	 	return	$ XDefixApps sp es'
		
	XAppSusp sp x1 x2
	 -> do	x1'	<- rename x1
	 	x2'	<- rename x2
		return	$ XAppSusp sp x1' x2'
		
	-- match sugar
	XMatch sp aa
	 -> do	aa'	<- rename aa
	 	return	$ XMatch sp aa'
		
	-- exception sugar
	XTry sp x aa w
	 -> do 	x'	<- rename x
	 	aa'	<- rename aa
		w'	<- rename w
		return	$ XTry sp x' aa' w'

	XThrow sp x
	 -> do	x'	<- rename x
	 	return	$ XThrow sp x'

	-- imperative sugar
	XWhile sp x1 x2
	 -> do	x1'	<- rename x1
	 	x2'	<- rename x2
		return	$ XWhile sp x1' x2'
		
	XWhen sp x1 x2
	 -> do	x1'	<- rename x1
	 	x2'	<- rename x2
		return	$ XWhen sp x1' x2'
		
	XUnless sp x1 x2
	 -> do	x1'	<- rename x1
	 	x2'	<- rename x2
		return	$ XUnless sp x1' x2'

	XBreak sp
	 ->	return	exp

	-- list sugar
	XListRange sp b x1 x2
	 -> do	x1'	<- rename x1
	 	x2'	<- rename x2
		return	$ XListRange sp b x1' x2'

	XListComp sp x qs
	 -> local
	 $ do	qs'	<- renameLCQuals qs

		addN NameValue
			$ concat $ map boundByLCQual qs'
		x'	<- rename x
		return	$ XListComp sp x' qs'

	-- patterns
	XCon sp v xx
	 -> do	v'	<- lookupV v
	 	xx'	<- rename xx
		return	$ XCon sp v' xx'
		
	XTuple sp xx
	 -> do	xx'	<- rename xx
	 	return	$ XTuple sp xx'
		
	XCons sp x1 x2
	 -> do	x1'	<- rename x1
	 	x2'	<- rename x2
		return	$ XCons sp x1' x2'
		
	XList sp xx
	 -> do	xx'	<- rename xx
	 	return	$ XList sp xx'
		
	_ -> panic stage
		$ "rename: cannot rename " % show exp
		
			
-- Projections -------------------------------------------------------------------------------------
instance Rename (Proj SourcePos) where
 rename jj 
  = case jj of
	JField sp v
	 -> do	v'	<- lbindN NameField v
		return	$ JField sp v'
		
	JFieldR sp v
	 -> do	v'	<- lbindN NameField v
	 	return	$ JFieldR sp v'

	JIndex sp x
	 -> do	x'	<- rename x
	 	return	$ JIndex sp x'
		
	JIndexR sp x
	 -> do	x'	<- rename x
	 	return	$ JIndexR sp x'

	
-- Alternatives ------------------------------------------------------------------------------------
instance Rename (Alt SourcePos) where
 rename a
  = case a of
	APat sp p x2
	 -> do	(p', x2')
	 	 	<- local
		  	$ do	p'	<- bindPat p
			 	x2'	<- rename x2
				return (p', x2')

		return	$ APat sp p' x2'

	AAlt sp gs x
	 -> do	(gs', x')
	 		<- local
			$ do	gs'	<- mapM bindGuard gs
				x'	<- rename x
				return	(gs', x')
				
		return	$ AAlt sp gs' x'

	ADefault sp x
	 -> do	x'	<- rename x
	 	return	$  ADefault sp x'


-- Labels ------------------------------------------------------------------------------------------
instance Rename (Label SourcePos) where
 rename ll
  = case ll of
  	LIndex sp i	-> return ll

	LVar sp v 
	 -> do	v'	<- lbindN NameField v
	 	return	$  LVar sp v'
	 


-- Patterns ----------------------------------------------------------------------------------------

-- | Bind the variables in a pattern.
--	The data type for patterns in function bindings is shared with that for expressions.
--	However, only  some constructors should appear in a pattern context. ie XList but not XMatch
--
bindPatternExp 
	:: (Exp SourcePos) 
	-> RenameM 
		( Exp SourcePos		-- renamed pattern expression
		, [Var])		-- bound object vars
		
bindPatternExp xx
 = case xx of
 	XVar sp v		
	 | Var.isCtorName v
	 -> do	v'	<- lookupV v
	 	return	( XVar sp v'
			, [])
		
	 | otherwise
	 -> do	v'	<- bindV v
	 	return	( XVar sp v'
			, [])
		
	XList sp xx
	 -> do	(xx', vss)	<- liftM unzip $ mapM bindPatternExp xx
	 	return	( XList sp xx'	
			, concat vss)

	XCons sp x1 x2
	 -> do	(x1', vs1)	<- bindPatternExp x1
	 	(x2', vs2)	<- bindPatternExp x2
		return	( XCons sp x1' x2'
			, vs1 ++ vs2)
		
	XTuple sp xs
	 -> do	(xs', vss)	<- liftM unzip $ mapM bindPatternExp xs
	 	return	( XTuple sp xs'
			, concat vss)

{-	XApp sp _ _
	 -> do	let (x:xs)	=  flattenApps xx
	 	x'		<- rename x
		xs'		<- mapM bindPatternExp xs
		return		$ unflattenApps sp (x':xs')
-}
	XDefix sp xs
	 -> do	(xs', vss)	<- liftM unzip $ mapM bindPatternExp xs
	 	return	( XDefix sp xs'
			, concat vss)
	 
	XOp sp v
	 -> do	v'	<- lookupV v
	 	return	( XOp sp v'
			, [])

	XObjVar sp v
	 ->  do
	 	v'	<- bindV v
		return	( XVar sp v'
			, [v'])


	XConst{}
	 ->	return	( xx
	 		, [])

	_ 	-> panic stage
		$ "bindPatternExp: no match for " % show xx	% "\n"


-- | Bind the variables in a guard
bindGuard :: Guard SourcePos -> RenameM (Guard SourcePos)
bindGuard gg
 = case gg of
 	GCase sp pat
	 -> do	pat'	<- bindPat pat
	 	return	$ GCase sp pat'
		
	GExp sp  pat x
	 -> do	x'	<- rename x	
	 	pat'	<- bindPat pat
		return	$ GExp sp pat' x'
		
	GBool sp x
	 -> do	x'	<- rename x
	 	return	$ GBool sp x'
		

-- | Bind the variables in a pattern
bindPat :: Pat SourcePos -> RenameM (Pat SourcePos)
bindPat ww
 = case ww of
 	WVar sp v
	 -> do	v'	<- bindV v
	 	return	$ WVar sp v'
 
 	WExp x	
	 -> do	(x', _)	<- bindPatternExp x
	 	return	$ WExp x'

	WConLabel sp v lvs 
	 -> do	v'		<- lookupV v

	 	let (ls, vs)	= unzip lvs
		ls'		<- rename ls
		vs'		<- mapM bindPat vs
		let lvs'	= zip ls' vs'
		
		return		$ WConLabel sp v' lvs'
		
	_	-> panic stage
		$ "bindPat: no match for " % show ww % "\n"
	 

-----------------------
-- renameLCQuals
--	Rename some list comprehension qualifiers.
--	In a sequence of qualifiers, the vars bound in a qualifier
--	are in-scope for subsequent qualifiers.
--
renameLCQuals :: [LCQual SourcePos] -> RenameM [LCQual SourcePos]
renameLCQuals qq
 = case qq of
  	[]			
	 -> 	return []

	(LCGen b (XVar sp v) x2 : qs)
	 -> do	x2'	<- rename x2
		local
		 $ do	v'	<- bindV v
			qs'	<- renameLCQuals qs		
			return	$ (LCGen b (XVar sp v') x2' : qs')
		
	(LCExp x : qs)
	 -> do	x'	<- rename x
	 	qs'	<- renameLCQuals qs
		return	$ LCExp x' : qs'
		

boundByLCQual :: LCQual SourcePos -> [Var]
boundByLCQual    q
 = case q of
 	LCGen _ (XVar sp v) _	-> [v]
	LCExp{}			-> []


-- Stmt --------------------------------------------------------------------------------------------

instance Rename (Stmt SourcePos) where
 rename s
  = case s of
	SBindPats sp v ps x
	 -> do	v'	<- lbindZ v

	 	local
		 $ do	(ps', objVss)	<- liftM unzip
					$  mapM bindPatternExp ps

			let objVs	= concat objVss

			(case objVs of
			  []	-> return ()
			  [v]	-> pushObjectVar v)

			x'		<- rename x

			(case objVs of
			  []	-> return ()
			  [v]	-> do { popObjectVar; return () })

			return	$ SBindPats sp v' ps' x'
	 	
	SStmt sp x
	 -> do	x'	<- local $ rename x
		return	$ SStmt sp x'		
				

	SSig sp v t
	 -> do	v'	<- lbindZ v
		t'	<- local $ rename t
		return	$ SSig sp v' t'
		

-- | Rename the variables in a list of statements
--	When using pattern bindings, the bound variable can appear in multiple statements
--	ie 	not True	= False
--		not False	= True
--
--	BUGS: make it an error for non-consecutive bindings to bind the same variable.
---
renameSs ::	[Stmt SourcePos] -> RenameM [Stmt SourcePos]
renameSs	ss
 = do	-- work out all the vars that are bound by this list of stmts
 	let vsBound	= catMaybes $ map takeStmtBoundV ss

	-- create fresh binding occurances to shadow anything with the same name bound above.
	mapM_ (lbindN_shadow NameValue) 
		$ nub vsBound

	-- Rename each statement.
	ss'	<- mapM rename ss

	return 	$ Debug.trace "hello"
		$ ss'

		

-----------------------
-- renamePat
-- |	Rename a pattern. 
--
{-
renamePat 
	:: Exp SourcePos	-- expression to rename
	-> RenameM 
		( Exp SourcePos	-- renamed expression
		, [Var])	-- bound object vars

renamePat	e
 = case e of
 	XApp sp e1 e2
	 -> do
	 	(e1', objVs1)	<- renamePat e1
		(e2', objVs2)	<- renamePat e2

		return	( XApp sp e1' e2'
			, objVs1 ++ objVs2)

	XTuple sp xs
	 -> do	(xs', vss)	<- liftM unzip
	 			$  mapM renamePat xs
				
		return	(XTuple sp xs', concat vss)
		
	XConst sp c
	 -> 	return	( XConst sp c, [])
	 
	XVar sp v
	 -> if Var.isCtorName v

		-- a constructor pattern
	 	then do
			v'	<- lookupV v
			return	(XVar sp v', [])

		-- a bound variable
		else do
			v'	<- bindV v
			return	(XVar sp v', [])

	XObjVar sp v
	 ->  do
	 	v'	<- bindV v
		return	( XVar sp v'
			, [v'])

	XDefix sp xx
	 -> do	(xx', vs)	<- renamePats [] [] xx
	 	return	( XDefix sp xx'
			, vs)
	
	XOp sp v
	 -> 	return	( XOp sp v
	 		, [])

	_ -> panic stage
		$ "renamePat: no match for " % show e

renamePats bound done []	
 = 	return (done, bound)
 
renamePats bound done (x:xs)
 = do	(x', boundHere)	<- renamePat x
 	renamePats (bound ++ boundHere) (done ++ [x']) xs
-}

-----
instance Rename Type where
 rename tt
  = case tt of
	TForall vs t
	 -> do 	(vs', t')	<- local 
		 $  do	vs'	<- mapMt2_1 bindZ vs
			t'	<- rename t
			return	(vs', t')

		return	$ TForall vs' t'

	TFetters fs t
	 -> do
	 	let bindingVars	=  catMaybes $ map takeBindingVarF fs

	  	-- if any of the LHS vars are already bound (perhaps by a forall) at this level
	  	--	then this is an error. Type vars aren't permitted to shadow each other.
		isShadowed	<- mapM isBound_local bindingVars
		let shadowVars	= [ v 	| (v, True) <- zip bindingVars isShadowed]
		let shadow	=  not $ null shadowVars

		when shadow
		 $ modify (\s -> s { 
	  		stateErrors 	= (stateErrors s) ++ map ErrorShadowVar shadowVars })

		local
		  $ do	
		  	-- bind the vars on the LHS of let binds
		  	mapM_ bindZ bindingVars

			fs'	<- rename fs
			t'	<- rename t
			return	$ TFetters fs' t'
			 	
	TVar k v 	
	 -> do 	v'	<- lbindN (spaceOfKind k) v
		return	$ TVar k v'

	TSum k ts
	 -> do	ts'	<- rename ts
	 	return	$ TSum k ts'

	TTop k 
	 -> 	return tt
	 
	TBot k
	 ->	return tt

	-- data
 	TFun t1 t2 eff clo
	 -> do 	t1'	<- rename t1
		t2'	<- rename t2
		eff'	<- rename eff
		clo'	<- rename clo
		return	$ TFun t1' t2' eff' clo'

	TData v ts		
	 -> do 	v'	<- lookupN NameType v
		ts'	<- mapM rename ts
		return	$ TData v' ts'
	
	-- effect
	TEffect v rs
	 -> do 	v'	<- lookupN NameEffect v
		rs'	<- rename rs
		return	$ TEffect v' rs'
		
	-- closure
	TFree v t
	 -> do	v'	<- lbindV v
	 	local
		 $  do	t'	<- rename t
		 	return	$ TFree v' t'
	
	TDanger t1 t2
	 -> do	t1'	<- rename t1
	 	t2'	<- rename t2
		return	$ TDanger t1' t2'
	
	TMask k t1 t2
	 -> do	t1'	<- rename t1
	 	t2'	<- rename t2
		return	$ TMask k t1' t2'
	
	TTag v
	 -> do	v'	<- lbindV v
	 	return	$ TTag v'
	
	-- wildcards
	TWild k
	 -> 	return	$ TWild k

	-- 
	TElaborate t
	 -> do	t'	<- rename t
	 	return	$ TElaborate t'
		
	TMutable t
	 -> do	t'	<- rename t
	 	return	$ TMutable t'

-----
instance Rename Fetter where
 rename f
  = case f of
	FConstraint v ts
	 -> do 	v'	<- lookupN NameClass v
		ts'	<- rename ts
		return	$ FConstraint v' ts'

  	FLet t1 t2
	 -> do	t1'	<- rename t1
		t2'	<- rename t2
		return	$ FLet t1' t2'

