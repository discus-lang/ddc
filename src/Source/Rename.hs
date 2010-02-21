
-- | The renamer renames variables so all binding and bound occurances of a variable
--	in the same scope have the same variable ids. This lets us perform substitution
--	later in the compiler without having to worry about scope and variable capture.
--
--   At top level scope it's ok to have variables with the same names, provided
--   we supply module ids when referencing them.
--
module Source.Rename
	( Rename
	, renameTrees )
where

import Source.Rename.State
import Source.Rename.Object
import Source.Rename.Binding

import Source.Util
import Source.Horror
import Source.Slurp
import Source.Error
import Source.Exp

import Type.Util
import Type.Plate.Collect
import Type.Plate.FreeVars

import Shared.Error		(panic)
import Shared.Var		(NameSpace (..), Module(..))
import Shared.VarUtil		(isCtorName)
import Shared.Pretty
import Shared.Base
import Data.Set			(Set)
import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var
import qualified Data.Set	as Set
import qualified Debug.Trace
import Util

-----
stage		= "Source.Rename"
-- debug		= True
-- trace s xx	= if debug then Debug.Trace.trace (pprStrPlain s) xx else xx


-- Tree --------------------------------------------------------------------------------------------

-- | Rename the variables in some source trees.
--	The current module should be first, then the interfaces of all the imported ones.
renameTrees
	:: [(Module, Tree SourcePos)]		-- ^ modules to rename		
	-> RenameM [(Module, Tree SourcePos)]	-- ^ renamed modules

renameTrees mTrees@(mTree1 : mTreeImports)
 = do
	-- bind all the top-level names.
	--	Do the imports first so if we have name clashes at top level the vars
	--	in the error messages will come out in the right order.
	mapM_ bindTopNames mTreeImports
	bindTopNames mTree1
	
	-- now rename all the trees
	mapM renameTree mTrees


-- | Add the top-level names from this module to the renamer state
bindTopNames :: (Module, Tree SourcePos) -> RenameM ()
bindTopNames (moduleName, tree)
 = do	-- Set the current module id
	modify $ \s -> s { stateModule = Just moduleName }
	
 	-- Slurp out all the top-level names.	
	let vsTop	= catMap slurpTopNames tree
	
	-- Add them to the rename state.
	mapM_ lbindZ_topLevel vsTop

	return ()


-- | Rename a source tree in this module
renameTree :: (Module, Tree SourcePos) -> RenameM (Module, Tree SourcePos)
renameTree (moduleName, tree)
 = do	
	-- Set the current module id
	modify $ \s -> s { stateModule = Just moduleName }

	-- Rename all the vars
	tree'	<- rename tree

	return	(moduleName, tree')


-- Top ---------------------------------------------------------------------------------------------
instance Rename (Top SourcePos) where
 rename	top
  = case top of
	PPragma sp es
	 -> do 	return	$ PPragma sp es 	

	PModule sp m
	 -> do 	m'	<- rename m
		return	$ PModule sp m'

	PImportModule sp ms
	 -> do	ms'	<- rename ms
		return	$ PImportModule sp ms'

	PExport sp exs
	 -> do	exs'	<- rename exs
		return	$ PExport sp exs'

	PForeign sp f
	 -> do	f'	<- rename f
	 	return	$ PForeign sp f'

	PInfix sp m i vs
	 -> do 	vs'	<- mapM lbindV_bound vs
		return	$ PInfix sp m i vs'

	-- types
	PTypeKind sp v k
	 -> do	v'	<- linkV v
	 	return	$ PTypeKind sp v' k

	PTypeSynonym sp v t
	 -> withLocalScope 
          $ do 	v'	<- linkN NameType v
	 	t'	<- rename t
		return	$ PTypeSynonym sp v' t'

	PData sp vData vsData ctors
	 -> withLocalScope
	  $ do	vData'	<- linkN NameType vData
		vsData'	<- mapM bindZ vsData
		ctors'	<- mapM (renameCtor vData' vsData') ctors
		return	$ PData sp vData' vsData' ctors' 
	
	PRegion sp v
	 -> do	v'	<- linkN NameRegion v
	 	return	$ PRegion sp v'
	
	PEffect sp v k
	 -> do 	v'	<- linkN NameEffect v
		return	$ PEffect sp v' k

	PStmt	s
	 -> do	s'	<- rename s
		return	$ PStmt s'
	
	-- classes
	PClass sp v k
	 -> do 	v'	<- linkN NameClass v
	 	return	$ PClass sp v' k

	PClassDict sp v vks inh sigs
	 -> do 	v'	<- linkN NameClass v

		(vs', inh', sigs')
		 <- withLocalScope
		 $ do	let (vs, ks)	= unzip vks
			vs'		<- mapM (bindN NameType) vs
			let vks'	= zip vs' ks
			
			inh'	<- mapM renameClassInh inh
			sigs'	<- mapM renameClassSig sigs
			return	(vks', inh', sigs')
	
		return	$ PClassDict sp v' vs' inh' sigs'

	PClassInst sp v ts inh stmts
	 -> do
	 	v'	<- linkN NameClass v

		(ts', inh')
		 <- withLocalScope
		 $ do	ts'	<- rename ts
			inh'	<- mapM renameInstInh inh
			return	(ts', inh')

		-- The names on the left of a binding in a class instance refer to the
		-- 	variables in the class definition, which might be in a different module.
		stmts'	<- mapM (renameStmt linkZ) stmts

		return	$ PClassInst sp v' ts' inh' stmts'			

	-- projections
	PProjDict sp t ss
	 -> do	
	 	-- The way the projection dict is parsed, the projection funtions end up in the wrong namespace, 
		--	NameValue. Convert them to NameField vars here.
		--	
		let fixupV v	= v 	{ Var.nameSpace 	= NameField }
		
		let ssF		
			= map (\s -> case s of 
					SSig  sp vs t		-> SSig   sp 	(map fixupV vs) t
					SBindFun sp v pats alts	-> SBindFun sp	(fixupV v) pats alts)
			$ ss
	 
	   	t' 	<- withLocalScope
		 	 $ do	t'	<- rename t
				return	t'
	
		ss'	<- rename ssF
		return	$ PProjDict sp t' ss'


-- Export ------------------------------------------------------------------------------------------
instance Rename (Export SourcePos) where
 rename ex	
  = case ex of
	EValue sp v 
	 -> do	v'	<- linkV v
		return	$ EValue sp v'

	EType sp v 
	 -> do	v'	<- linkN NameType v
		return	$ EType sp v'
	
	ERegion sp v 
	 -> do	v'	<- linkN NameRegion v
		return	$ ERegion sp v'

	EEffect sp v 
	 -> do	v'	<- linkN NameEffect v
		return	$ EEffect sp v'

	EClass sp v 
	 -> do	v'	<- linkN NameClass v
		return	$ EClass sp v'

-- Module ------------------------------------------------------------------------------------------
instance Rename Module where
 rename m	= return m
 
 
-- Foreign -----------------------------------------------------------------------------------------
instance Rename (Foreign SourcePos) where
 rename ff
  = case ff of
 	OImport mS v tv to 
	 -> withLocalScope
	 $  do 	v'	<- lbindV_binding v
	 	tv'	<- rename tv
		to'	<- rename to
		return	$ OImport mS v' tv' to' 
	
	OImportUnboxedData s v k
	 -> withLocalScope
	  $ do	v'	<- lbindN_binding NameType v
		return	$ OImportUnboxedData s v' k

-- Classes -----------------------------------------------------------------------------------------
-- all this stuff should really have it's own constructor
--
renameClassInh :: (Var, [Var])  -> RenameM (Var, [Var])
renameClassInh	  (v, vs)
 = do	v'	<- linkN NameClass v
	vs'	<- mapM (lbindN_bound NameType) vs
	return	(v, vs)


renameClassSig :: ([Var], Type) -> RenameM ([Var], Type)
renameClassSig    (vs, t)
 = withLocalScope
 $ do 	vs'	<- mapM lbindV_binding vs
	t'	<- rename t
	return	(vs', t')


renameInstInh :: (Var, [Type])	-> RenameM (Var, [Type])
renameInstInh    (v, ts)
 = do
 	v'	<- linkN NameClass v
	ts'	<- rename ts
	return	(v', ts')
		

-- Constructor -----------------------------------------------------------------
renameCtor 
	:: Var			-- type constructor name
	-> [Var]		-- type constructor parameters
	-> (Var, [DataField (Exp SourcePos) Type])	
	-> RenameM (Var, [DataField (Exp SourcePos) Type])

renameCtor vData vsData (v, fields)
 = do	v'	<- linkV v
	fields'	<- mapM (renameDataField vData vsData) fields
	return	(v', fields')


renameDataField vData vsData df
  = do	
  	-- field vars aren't supposed to have module qualifiers...
  	let fixupV v	
  		= v { Var.nameSpace 	= NameField
		    , Var.nameModule 	= ModuleNil }

   	mLabel'	<- case dLabel df of
			Nothing		-> return Nothing
			Just label
			 -> do 	label'	<- lbindN_binding NameField label
				return	$ Just $ fixupV label'

	tField'	<- rename $ dType df

	-- check that all vars in the field type are params to the type constructor.
	let vsFree	= Set.filter (not . Var.isCtorName) $ freeVars tField'
	let vsBad	= Set.difference vsFree (Set.fromList vsData)

	when (not $ Set.null vsBad)
	 	$ mapM_ (\v -> addError $ ErrorUndefinedVar v) 
		$ Set.toList vsBad

	mExp'	<- rename $ dInit df

	return 	df
		{ dLabel	= mLabel'
		, dType		= tField'
		, dInit		= mExp' }
		
	
-- Expressions -----------------------------------------------------------------
instance Rename (Exp SourcePos) where 
 rename exp
  = case exp of

	-- core
	XLit sp lit		
	 -> return exp

	XVar sp v		
	 -> do 	v'	<- linkV v
		return	$ XVar sp v'

	XProj sp x proj
	 -> do 	x'	<- rename x
		proj'	<- rename proj
		return	$ XProj sp x' proj'

	XProjT sp t proj
	 -> do	t'	<- rename t
	 	proj'	<- rename proj
		return	$ XProjT sp t' proj'

	XApp sp e1 e2	
	 -> do 	e1'	<- rename e1
		e2'	<- rename e2
		return	$ XApp sp e1' e2'

	XCase sp e1 cs	
	 -> do 	e1'	<- rename e1
		cs'	<- rename cs
		return	$ XCase sp e1' cs'

	XDo sp ss 
	 -> withLocalScope
	 $ do 	ss'	<- renameSs ss
		return	$  XDo sp ss'

 	XLet sp ss e	
	 -> withLocalScope
	  $ do 	ss'	<- renameSs ss
		e'	<- rename e
		return	$ XLet sp ss' e'
		
	XWhere sp x ss
	 -> withLocalScope
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
		v'	<- lbindN_binding NameField v
		return	$ XProj sp (XVar sp objV) (JField sp v')
		
	-- sugar
	XLambdaPats sp ps x
	 -> withLocalScope
	 $ do 	(ps', _)	
			<- liftM unzip 
	 		$ mapM (bindPat False) ps

		x'	<- rename x
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
	  -> do	v'	<- linkV v
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
	XListRange sp b x1 x2 x3
	 -> do	x1'	<- rename x1
	 	x2'	<- rename x2
	 	x3'	<- rename x3
		return	$ XListRange sp b x1' x2' x3'

	XListComp sp x qs
	 -> withLocalScope
	 $ do	(qs', x') <- renameListComp qs x
		return	$ XListComp sp x' qs'

	XTuple sp xx
	 -> do	xx'	<- rename xx
	 	return	$ XTuple sp xx'
		
	XList sp xx
	 -> do	xx'	<- rename xx
	 	return	$ XList sp xx'
		
	_ -> panic stage
		$ "rename: cannot rename " % show exp


-- | Rename some list comprehension qualifiers.
--   The vars bound in a qualifier are in-scope for subsequent qualifiers, 
--   and vars bound in all qualifiers are in-scope for the final expression.
--
renameListComp 
	:: [LCQual SourcePos]
	-> Exp SourcePos 
	-> RenameM ([LCQual SourcePos], Exp SourcePos)
	
renameListComp qq xx
 = case qq of
  	[]			
	 -> do	xx'	<- rename xx
		return	([], xx')

	LCGen b w x2 : qs
	 -> do	x2'	<- rename x2
		withLocalScope
		 $ do	(w', [])	<- bindPat False w
			(qs', xx')	<- renameListComp qs xx		
			return	(LCGen b w' x2' : qs', xx')

	LCExp x : qs
	 -> do	x'		<- rename x
	 	(qs', xx')	<- renameListComp qs xx
		return	(LCExp x' : qs', xx')
		
			
-- Projections -------------------------------------------------------------------------------------
instance Rename (Proj SourcePos) where
 rename jj 
  = case jj of
	JField sp v
	 -> do	v'	<- lbindN_binding NameField v
		return	$ JField sp v'
		
	JFieldR sp v
	 -> do	v'	<- lbindN_binding NameField v
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
	 	 	<- withLocalScope
		  	$ do	(p', [])	<- bindPat False p
			 	x2'		<- rename x2
				return (p', x2')

		return	$ APat sp p' x2'

	AAlt sp gs x
	 -> do	(gs', x')
	 		<- withLocalScope
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
	 -> do	v'	<- lbindN_binding NameField v
	 	return	$  LVar sp v'
	 


-- Patterns ----------------------------------------------------------------------------------------

-- | Bind the variables in a guard
bindGuard :: Guard SourcePos -> RenameM (Guard SourcePos)
bindGuard gg
 = case gg of
	GExp sp  pat x
	 -> do	x'		<- rename x	
	 	(pat', [])	<- bindPat False pat
		return	$ GExp sp pat' x'
		
	GBool sp x
	 -> do	x'	<- rename x
	 	return	$ GBool sp x'
		

-- | Bind the variables in a pattern
bindPat :: Bool			-- lazy bind
	-> Pat SourcePos 
	-> RenameM 
		( Pat SourcePos
		, [Var])

bindPat lazy ww
 = case ww of
 	WVar sp v
	 -> do	v'	<- if lazy 	then lbindV_binding v
	 				else bindV v

	 	return	( WVar sp v'
			, [])

	WObjVar sp v	
	 -> do	v'	<- bindV v
	 	return	( WVar sp v'
			, [v'])
 
 	WLit sp _
	 -> do	return	(ww, [])
	 
	WCon sp v ps
	 -> do	v'		<- linkV v
	 	(ps', bvs)	<- liftM unzip $ mapM (bindPat lazy) ps
		return	( WCon sp v' ps'
			, concat bvs)
	 
	WConLabel sp v lvs 
	 -> do	v'		<- linkV v

	 	let (ls, vs)	= unzip lvs
		ls'		<- rename ls
		(vs', bvss)	<- liftM unzip 
				$ mapM (bindPat lazy) vs
		let lvs'	= zip ls' vs'
		
		return	( WConLabel sp v' lvs'
			, concat bvss)
	
	WAt sp v p
	 -> do	(p', vs)	<- (bindPat lazy) p
	 	v'		<- bindV v
		return	( WAt sp v' p'
			, vs)
	
	WWildcard sp	-> return (ww, [])
	WUnit sp	-> return (ww, [])

	WTuple sp xs
	 -> do	(xs', vss)	<- liftM unzip $ mapM (bindPat lazy) xs
	 	return	( WTuple sp xs'
			, concat vss)
	
	WCons sp x1 x2
	 -> do	(x1', vs1)	<- bindPat lazy x1
	 	(x2', vs2)	<- bindPat lazy x2
		return	( WCons sp x1' x2'
			, vs1 ++ vs2)
		
	WList sp xs
	 -> do	(xs', vss)	<- liftM unzip $ mapM (bindPat lazy) xs
	 	return	( WList sp xs'
			, concat vss)
		
	 
		
{-
boundByLCQual :: LCQual SourcePos -> [Var]
boundByLCQual    q
 = case q of
 	LCGen _ (WVar sp v) _	-> [v]
	LCExp{}			-> []
-}

-- Stmt --------------------------------------------------------------------------------------------
instance Rename (Stmt SourcePos) where
 rename s = renameStmt lbindZ_binding s

renameStmt bindLHS s
  = case s of

	-- Note that we give sigs for both value and projection vars.
	SSig sp vs t
	 -> do	vs'	<- mapM bindLHS vs
		t'	<- withLocalScope $ rename t
		return	$ SSig sp vs' t'

	SStmt sp x
	 -> do	x'	<- withLocalScope $ rename x
		return	$ SStmt sp x'		

	-- Note that we give bindings for both value and projection vars.
	SBindFun sp v ps as
	 -> do	v'	<- bindLHS v
	 	withLocalScope
		 $ do	(ps', objVss)	<- liftM unzip
					$  mapM (bindPat False) ps

			let objVs	= concat objVss

			(case objVs of
			  []	-> return ()
			  [v]	-> pushObjectVar v)

			as'		<- rename as
	
			(case objVs of
			  []	-> return ()
			  [v]	-> do { popObjectVar; return () })

			return	$ SBindFun sp v' ps' as'

	SBindPat sp pat x
	 -> do	(pat', _) <- bindPat True pat
	 	x'	<- rename x
		return	$ SBindPat sp pat' x'
	 	
	SBindMonadic sp pat x
	 -> do	(pat', _) <- bindPat True pat
	 	x'	<- rename x
		return	$ SBindMonadic sp pat' x'


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
 	let vsBound	= catMap takeStmtBoundVs ss

	-- create fresh binding occurances to shadow anything with the same name bound above.
	mapM_ (lbindV_binding) 
		$ nub vsBound

	-- Rename each statement.
	ss'	<- mapM rename ss

	return ss'

		
-- Type --------------------------------------------------------------------------------------------
instance Rename Type where
 rename tt
  = case tt of
	TForall b k t
	 -> do	let (BVar v)	= b
		let reused	= tforallHasVarName (Var.name v) t
		when (not $ null reused)
		 $ modify (\s -> s {
	  		stateErrors 	= (stateErrors s) ++ map ErrorShadowForall reused })

		(b', t')	<- withLocalScope 
		 $  do	v'	<- bindZ v
			t'	<- rename t
			return	(BVar v', t')

		let tt'		= TForall b' k t'
		return tt'

	TFetters t fs
	 -> do
	 	let bindingVars	=  catMaybes $ map takeBindingVarF fs

	  	-- if any of the LHS vars are already bound (perhaps by a forall) at this level
	  	--	then this is an error. Type vars aren't permitted to shadow each other.
{-		isShadowed	<- mapM isBound_withLocalScope bindingVars
		let shadowVars	= [ v 	| (v, True) <- zip bindingVars isShadowed]
		let shadow	=  not $ null shadowVars

		when shadow
		 $ modify (\s -> s { 
	  		stateErrors 	= (stateErrors s) ++ map ErrorShadowVar shadowVars })
-}
		withLocalScope
		  $ do	
		  	-- bind the vars on the LHS of let binds
		  	mapM_ bindZ bindingVars

			fs'	<- rename fs
			t'	<- rename t
			return	$ TFetters t' fs'
			 	
	TVar k v 	
	 -> do 	let Just space = spaceOfKind k
		v'	<- lbindN_bound space v
		return	$ TVar k v'

	TSum k ts
	 -> do	ts'	<- rename ts
	 	return	$ TSum k ts'

	TApp t1 t2
	 -> do	t1'	<- rename t1
	 	t2'	<- rename t2
		return	$ TApp t1' t2'
		
	TCon tc
	 -> do	tc'	<- rename tc
	 	return	$ TCon tc'

	TTop k 	 -> return tt
	TBot k	 -> return tt

	-- effect
	TEffect v rs
	 -> do 	v'	<- linkN NameEffect v
		rs'	<- rename rs
		return	$ TEffect v' rs'
		
	-- closure
	TFree v t
	 -> do	v'	<- lbindV_bound v
	 	withLocalScope
		 $  do	t'	<- rename t
		 	return	$ TFree v' t'
	
	TDanger t1 t2
	 -> do	t1'	<- rename t1
	 	t2'	<- rename t2
		return	$ TDanger t1' t2'
	
	-- 
	TElaborate ee t
	 -> do	t'	<- rename t
	 	return	$ TElaborate ee t'


tforallHasVarName name tt
 = case tt of
	TForall b k t
         -> do	let (BVar v)	= b
		if Var.name v == name
		 then v : tforallHasVarName name t
                 else tforallHasVarName name t
	_ -> []
 
		
-- TyCon -------------------------------------------------------------------------------------------
instance Rename TyCon where
 rename tc
  = case tc of
  	TyConFun{}
	 -> do 	return	tc
		
	TyConData { tyConName }
	 -> do	v	<- linkN NameType tyConName
	 	return	$ tc { tyConName = v }


-- Fetter ------------------------------------------------------------------------------------------
instance Rename Fetter where
 rename f
  = case f of
	FConstraint v ts
	 -> do 	v'	<- linkN NameClass v
		ts'	<- rename ts
		return	$ FConstraint v' ts'

  	FWhere t1 t2
	 -> do	t1'	<- rename t1
		t2'	<- rename t2
		return	$ FWhere t1' t2'

