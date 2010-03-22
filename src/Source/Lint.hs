{-# OPTIONS -fwarn-incomplete-patterns #-}

-- | Check for problems in the structure of the source program that we want to pick
--	up before running the desugarer. Every check we can add to the lint checker
--	is guaranteed to save 10x the pain further down the road. Inspecting every node
--	of the AST also gets us a deepseq.
--
-- Everytime you fix a bug, ask whether it should have been picked up but the lint checker.
--
-- All things being equal, it's better to detect errors as early as possible. 
--	* For internal problems that are likely to be due to bugs we can just panic, but
--	  the earlier in the compiler the panic is the less code we have to dig through
--	  to find the problem.
--
--	* For user-level problems we can construct an error message which relates
--	  more closely to the program the user actualy wrote.
--
-- Things checked for: 
--	(* marks user level problem)
--
-- in Vars:
--	Missing unique binders on vars.
--	Missing module annots on vars.
--	Missing namespaces on vars.
--	Vars in wrong namespaces.
--	Vars with null names.
--
-- in Expressions:
--	No bindings in let expressions.
--	No alts  in case expressions.
--	No stmts in do expressions.
--	No sigs  in class definitions.
--	No stmts in class instances.
--	No stmts in projection dictionaries.
--	XDefix nodes should have been resolved by the defixer.
--	
-- in Type Sigs:
--	Empty constraint lists.
--	Various nodes that shouldn't show up in the source program.
--	* Type sigs without decls.
--
module Source.Lint
	(lintTree)
where
import Source.Exp
import Source.Error
import Type.Exp
import Type.Util
import Util
import Shared.Literal
import DDC.Base.SourcePos
import DDC.Main.Error
import DDC.Var.NameSpace
import DDC.Var.ModuleId
import Shared.Var		(Var)
import qualified Shared.Var	as Var	
import qualified Data.Set	as Set

-----
stage	= "Source.Lint"

lintTree :: Tree SourcePos -> LintM (Tree SourcePos)
lintTree t	
 = do	let ss	= [s	| PStmt s <- t]
	_	<- lintStmts_sigsHaveBindings ss
	lint t


-- teh Monadz -------------------------------------------------------------------------------------
type LintM = State [Error]

addError :: Error -> LintM ()
addError err
 = modify $ \s -> s ++ [err]

---------------------------------------------------------------------------------------------------
class Lint a 
 where	lint :: a -> LintM a

instance Lint a => Lint [a] 
 where	lint xx	= mapM lint xx
 
instance Lint a => Lint (Maybe a) 
 where	lint Nothing	= return Nothing
	lint (Just x)
	 = do	x'	<- lint x
		return	$ Just x'
 
instance (Lint a, Lint b) => Lint (a, b) 
 where	lint (a, b)	
	 = do	a'	<- lint a
		b'	<- lint b
		return	(a', b')
		
		
-- | What to do when we find an internal error.
death x s
 	= panic stage 
	$ "lint: " 	% s 		% "\n"


-- Top --------------------------------------------------------------------------------------------
instance Lint (Top SourcePos) where
 lint xx
  = case xx of
	PPragma sp es			
	 -> return $ PPragma sp es

 	PModule sp ms			
 	 -> do	ms'	<- lint ms
		return $ PModule sp ms'

	PImportModule sp ms		
	 -> do	ms'	<- lint ms
		return $ PImportModule sp ms

	PExport sp es
	 -> do	es'	<- lint es
		return $ PExport sp es'

	PForeign sp f
	 -> do	f'	<- lint f
		return $ PForeign sp f'

	PInfix sp m i vs		
	 -> do	vs'	<- lint vs
		return $ PInfix	sp m i vs'
		
	PTypeKind sp v k		
	 -> do	v'	<- lint v
		k'	<- lint k
		return	$ PTypeKind sp v' k'
		
	PTypeSynonym sp v t		
	 -> do	v'	<- lint v
		t'	<- lint t
		return	$ PTypeSynonym	sp v' t'
	
	PData sp v vs ctors
	 -> do	v'	<- lint v
		vs'	<- lint vs
		ctors'	<- lint ctors
		return	$ PData	sp v' vs' ctors'
		
	PRegion sp v
	 -> do	v'	<- lint v
		return	$ PRegion sp v'
	
	PEffect sp v k	
	 -> do	v'	<- lint v
		return	$ PEffect sp v' k

	PClass sp v s			
	 -> do	v'	<- lint v
		s'	<- lint s
		return	$ PClass sp v' s'
		
	PClassDict sp v vs context sigs	
	 | null vs	-> death xx "PClassDict - no params'"
	 | null sigs	-> death xx "PClassDict - no sigs"
	 | otherwise
	 -> do	v'		<- lint v
		vs'		<- lint vs
		context'	<- lint context
		sigs'		<- lint sigs
		return		$ PClassDict sp v' vs' context' sigs'

	PClassInst sp v ts context stmts	
	 | null ts	-> death xx "PClassInst - no params"
	 | null stmts	-> death xx "PClassInst - no stmts"
	 | otherwise
	 -> do	v'		<- lint v
	 	ts'		<- lint ts
		context'	<- lint context
		stmts'		<- lint stmts
		return		$ PClassInst sp v' ts' context' stmts'

	PProjDict sp t stmts
	 | null stmts	-> death xx "PProjDict - no stmts"
	 | otherwise
	 -> do	t'		<- lint t
		stmts'		<- lint stmts
		return	$ PProjDict sp t' stmts'

	PStmt s				
	 -> do	s'	<- lint s
		return	$ PStmt	s'
	
instance Lint (Export SourcePos) where
 lint e	= return e
	
instance Lint (Foreign SourcePos) where
 lint f = return f

instance Lint (DataField (Exp SourcePos) Type) where
 lint f = return f

-- Stmt --------------------------------------------------------------------------------------------
instance Lint (Stmt SourcePos) where
 lint s
  = case s of
	SSig  sp v t			
	 -> do	v'	<- lint v
		t'	<- lint t
		return	$ SSig sp v' t'

	SStmt sp x	
	 -> do	x'	<- lint x
		return	$ SStmt sp x'

	SBindFun sp v ws as
	 -> do	v'	<- lint v
		ws'	<- lint ws
		as'	<- lint as
		return	$ SBindFun sp v' ws' as'
	
	SBindPat sp w x
	 -> do	w'	<- lint w
		x'	<- lint x
		return	$ SBindPat sp w' x'
	
	SBindMonadic sp w x
	 -> do	w'	<- lint w
		x'	<- lint x
		return	$ SBindMonadic sp w' x'


-- | Check that every signature in this list of statements has an associated binding
lintStmts_sigsHaveBindings :: [Stmt SourcePos] -> LintM [Stmt SourcePos]
lintStmts_sigsHaveBindings ss
 = do	let sigVars	
		= Set.fromList 
		$ concat
		$ [vs	| SSig _ vs _		<- ss ]
			
	let bindVars	
		= Set.fromList
		$ [v	| SBindFun _ v _ _	<- ss ]
	
	let sigVars_noBind
		= sigVars `Set.difference` bindVars
		
	mapM_ addError 
		[ErrorSigLacksBinding v 
			| v <- Set.toList sigVars_noBind]
		
	return ss
	

-- Exp --------------------------------------------------------------------------------------------
instance Lint (Exp SourcePos) where
 lint x
  = case x of
	XNil				
	 -> return x

	XLit sp litfmt
	 -> do	litfmt'	<- lint litfmt
		return	$ XLit sp litfmt'

	XVar sp v				
	 | not $ inSpaceV [v]	-> death x "XVar - var in wrong namespace."
	 | otherwise			
	 -> do	v'	<- lint v
	 	return	$ XVar sp v'
	
	XObjField sp v
	 -> do	v'	<- lint v
		return	$ XObjField sp v'
	
	XProj sp e p
	 -> do	e'	<- lint e
		p'	<- lint p
		return	$ XProj sp e' p'
	
	XProjT sp t p
	 -> do	t'	<- lint t
		p'	<- lint p
		return	$ XProjT sp t' p'
		
	XLambdaPats sp ws e
	 -> do	ws'	<- lint ws
		e'	<- lint e
		return	$ XLambdaPats sp ws' e'
	
	XLambdaProj sp j es
	 -> do	j'	<- lint j
		es'	<- lint es
		return	$ XLambdaProj sp j' es'

	XLambdaCase sp aa		
	 | isNil aa	-> death x "XCaseL - no alternatives."
	 | otherwise	
	 -> do	aa'	<- lint aa
	 	return	$ XLambdaCase sp aa'

	XCase sp o aa			
	 | isNil aa	-> death x "XCase - no alternatives."
	 | otherwise	
	 -> do	o'	<- lint o
		aa'	<- lint aa
		return	$ XCase sp o' aa'

	XMatch sp as
	 -> do	as'	<- lint as
		return	$ XMatch sp as'
		
	XDo sp ss				
	 | isNil ss	-> death x "XDo - no statements."
	 | otherwise	
	 -> do	ss2	<- lintStmts_sigsHaveBindings ss
		ss'	<- lint ss2
		return	$ XDo sp ss'

	XLet sp ss e			
	 | isNil ss	-> death x "XLet - no bindings."
	 | otherwise			
	 -> do	ss2	<- lintStmts_sigsHaveBindings ss
		ss'	<- lint ss2
		e'	<- lint e
		return	$ XLet sp ss' e'

	XIfThenElse sp e1 e2 e3		
	 -> do	e1'	<- lint e1
		e2'	<- lint e2
		e3'	<- lint e3
		return	$ XIfThenElse sp e1' e2' e3'

	XTry sp e as me
	 -> do	e'	<- lint e
		as'	<- lint as
		me'	<- lint me
		return	$ XTry sp e' as' me'
	
	XThrow sp e
	 -> do	e'	<- lint e
		return	$ XThrow sp e'
		
	XWhere sp e ss
	 -> do	e'	<- lint e
		ss2	<- lintStmts_sigsHaveBindings ss
		ss'	<- lint ss2
		return	$ XWhere sp e' ss'
		
	XTuple sp es
	 -> do	es'	<- lint es
		return	$ XTuple sp es'
	
	XList sp es
	 -> do	es'	<- lint es
		return	$ XList sp es'
		
	XListRange sp b e me1 me2
	 -> do	e'	<- lint e
		me1'	<- lint me1
		me2'	<- lint me2
		return	$ XListRange sp b e' me1' me2'
	
	XListComp sp e qs
	 -> do	e'	<- lint e
		qs'	<- lint qs
		return	$ XListComp sp e' qs'
		
	XWhile sp e1 e2
	 -> do	e1'	<- lint e1
		e2'	<- lint e2
		return	$ XWhile sp e1' e2'
			
	XWhen sp e1 e2
	 -> do	e1'	<- lint e1
		e2'	<- lint e2
		return	$ XWhen sp e1' e2'
	
	XUnless sp e1 e2
	 -> do	e1'	<- lint e1
		e2'	<- lint e2
		return	$ XUnless sp e1' e2'
	
	XBreak sp
	 -> 	return	$ XBreak sp
	
 	XDefix{}	-> death x "XDefix - should have been eliminated by Source.Defix"
	XDefixApps{}	-> death x "XDefixApps - should have been eliminated by Source.DefixApps"
	XOp{}		-> death x "XOp -  should have been eliminated by Source.Defix"
		
	XApp sp e1 e2		
	 -> do	e1'	<- lint e1
		e2'	<- lint e2
		return	$ XApp sp e1' e2'

	XAppSusp{}	-> death x "XAppSupp - should have been eliminated by Source.Defix"
	XParens{}	-> death x "XParens - should only be used in the parser"


-- LCQual -----------------------------------------------------------------------------------------
instance Lint (LCQual SourcePos) where
 lint qual
  = case qual of
	LCGen b w x
	 -> do	w'	<- lint w
		x'	<- lint x
		return	$ LCGen b w' x'
		
	LCLet ss
	 -> do	ss'	<- lint ss
		return	$ LCLet ss
		
	LCExp e
	 -> do	e'	<- lint e
		return	$ LCExp e'
		
		
-- Proj -------------------------------------------------------------------------------------------
instance Lint (Proj SourcePos) where
 lint proj
  = case proj of
	JField sp v
	 -> do	v'	<- lint v
		return	$ JField sp v'
		
	JFieldR sp v
	 -> do	v'	<- lint v
		return	$ JFieldR sp v'
		
	JIndex sp e
	 -> do	e'	<- lint e
		return	$ JIndex sp e'
		
	JIndexR sp e
	 -> do	e'	<- lint e
		return	$ JIndexR sp e'
		

-- LiteralFmt -------------------------------------------------------------------------------------
instance Lint LiteralFmt where
 lint litfmt 
  = return litfmt


-- Alt --------------------------------------------------------------------------------------------
instance Lint (Alt SourcePos) where
 lint a
  = case a of				
	APat sp w e
	 -> do	w'	<- lint w
		e'	<- lint e
		return	$ APat sp w' e'
		
	AAlt sp ws x
	 -> do	ws'	<- lint ws
		x'	<- lint x
		return	$ AAlt sp ws' x'
	
	ADefault sp x	
	 -> do	x'	<- lint x
		return	$ ADefault sp x'
	
	
-- Pat ---------------------------------------------------------------------------------------------	
instance Lint (Pat SourcePos) where
 lint ww
  = case ww of
	WVar sp v
	 -> do	v'	<- lint v
		return	$ WVar sp v'
		
	WObjVar sp v
	 -> do	v'	<- lint v
		return	$ WObjVar sp v'
		
	WLit sp l
	 -> do	l'	<- lint l
		return	$ WLit sp l'
		
	WCon sp v ws
	 -> do	v'	<- lint v
		ws'	<- lint ws
		return	$ WCon sp v' ws'
		
	WConLabel sp v lws
	 -> do	v'	<- lint v
		lws'	<- lint lws
		return	$ WConLabel sp v' lws'
		
	WAt sp v w
	 -> do	v'	<- lint v
		w'	<- lint w
		return	$ WAt sp v' w'
		
	WWildcard sp
	 -> 	return	$ WWildcard sp
	
	WUnit sp
	 -> 	return	$ WUnit sp
	
	WTuple sp ws
	 -> do	ws'	<- lint ws
		return	$ WTuple sp ws'
		
	WCons sp w1 w2
	 -> do	w1'	<- lint w1
		w2'	<- lint w2
		return	$ WCons sp w1' w2'
		
	WList a ws
	 -> do	ws'	<- lint ws
		return	$ WList a ws'
		
		
-- Label -------------------------------------------------------------------------------------------
instance Lint (Label SourcePos) where
 lint l
  = case l of
	LIndex sp i
	 -> 	return	$ LIndex sp i
	
	LVar sp v
	 -> do	v'	<- lint v
		return	$ LVar sp v'
		
	
-- Guard -------------------------------------------------------------------------------------------
instance Lint (Guard SourcePos) where
 lint gg
  = case gg of
	GExp sp w x
	 -> do	w'	<- lint w
		x'	<- lint x
		return	$ GExp sp w' x'
		
	GBool sp x
	 -> do	x'	<- lint x
		return	$ GBool sp x'
	
	
-- Type --------------------------------------------------------------------------------------------
instance Lint Type where
 lint tt
  = case tt of
	TNil 		-> death tt "TNil - shouldn't exist anymore"
	
	TForall b k t
	 -> do	k'	<- lint k
		t'	<- lint t
		return	$ TForall b k' t'

	TContext k t
	 -> do	k'	<- lint k
		t'	<- lint t
		return	$ TContext k' t'
		
	TFetters t fs
	 | isNil fs	-> death tt "TFetters - list is empty"
	 | otherwise	
	 -> do	t'	<- lint t
		fs'	<- lint fs
		return	$ TFetters t' fs'

	TConstrain ts cs -> death tt "TConstrain - shouldn't exist in source program"
	
	TApp t1 t2
	 -> do	t1'	<- lint t1
		t2'	<- lint t2
		return	$ TApp t1' t2'

	TSum k ts
	 -> do	k'	<- lint k 
		ts'	<- lint ts
		return	$ TSum k' ts'

	TCon{}
	 -> return tt

  	TVar k v
	 | let Just space = spaceOfKind k
	   in  not $ inSpaceN space [v]	
	 -> death tt "TVar - var in wrong namespace."

	 | otherwise		
	 -> do	k'	<- lint k
		v'	<- lint v
		return	$ TVar k' v'

	TIndex{}	-> death tt "TIndex - shouldn't exist in source program"

	TTop k		
	 -> do	k'	<- lint k
		return	$ TTop k'

	TBot k		
	 -> do	k'	<- lint k
		return	$ TBot k'
	 	
	-- effect
	TEffect v ts
 	 | not $ inSpaceE [v]	-> death tt "TEffect - var in wrong namespace"
	 | otherwise
	 -> do	v'	<- lint v
		ts'	<- lint ts
		return	$ TEffect v' ts'
			
	-- closure
	TFree v t
	 -> do	v'	<- lint v
		t'	<- lint t
		return	$ TFree v' t'
		
	TDanger t1 t2
	 -> do	t1'	<- lint t1
		t2'	<- lint t2
		return	$ TDanger t1' t2'
	
	TElaborate e t
	 -> do	t'	<- lint t
		return	$ TElaborate e t'
	
	TClass{}	-> death tt "TClass - shouldn't exist in source program"
	TError{}	-> death tt "TError - shouldn't exist in source program"
	TVarMore{}	-> death tt "TVarMore - shouldn't exist in source program"
	TWitJoin{}	-> death tt "TWitJoin - shouldn't exist in source program"
	
	
-- Fetter -----------------------------------------------------------------------------------------
instance Lint Fetter where
 lint ff
  = case ff of
	FConstraint v ts
	 -> do	v'	<- lint v
		ts'	<- lint ts
		return	$ FConstraint v' ts'
		
	FWhere t1 t2
	 -> do	t1'	<- lint t1
		t2'	<- lint t2
		return	$ FWhere t1' t2'

	FMore t1 t2
	 -> do	t1'	<- lint t1
		t2'	<- lint t2
		return	$ FMore t1' t2'
		
	FProj{}		-> death ff "FProj - shouldn't exist in source program"

-- Kind -------------------------------------------------------------------------------------------
instance Lint Kind where
 lint k	
  = return k

-- Super ------------------------------------------------------------------------------------------
instance Lint Super where
 lint s
  = return s

-- Var --------------------------------------------------------------------------------------------
instance Lint Var where
 lint v
 	| Var.varId v		== Var.VarIdNil
	= death v "Var - var's bind code is undefined."

	| Var.name v == ""
	= death v "Var - name is the empty string."

	| Var.nameSpace v == NameNothing
	= death v "Var - var is in name space 'NameNothing'.\n"
	
	| otherwise
	= return v
			
-----
inSpaceN space vs	= and $ map (\v -> Var.nameSpace v == space)  vs
inSpaceV vs		= and $ map (\v -> Var.nameSpace v == NameValue)  vs
inSpaceE vs		= and $ map (\v -> Var.nameSpace v == NameEffect) vs


-- Module -----------------------------------------------------------------------------------------
instance Lint ModuleId where
 lint m	= return m

