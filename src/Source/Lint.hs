{-# OPTIONS -fwarn-incomplete-patterns #-}

-- | Check for problems in the structure of the source program that we want to pick
--	up before running the desugarer. Every check we can add to the lint checker
--	is guaranteed to save 10x the pain further down the road. Inspecting every node
--	of the AST also gets us a deepseq.
--
-- Everytime you fix a bug, ask whether it could have been picked up but the lint checker.
--
-- All things being equal, it's better to detect errors as early as possible. 
--	* For internal problems that are likely to be due to bugs we can just panic, but
--	  the earlier in the compiler the panic is the less code we have to dig through
--	  to find the problem.
--
--	* For user-level problems we can construct an error message which relates
--	  more closely to the program the user actualy wrote.
--
-- Things checked for: (* marks user level problem)
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
--	No alts in case expressions.
--	No stmts in do expressions.
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
import Type.Pretty
import Type.Util

import Shared.Error	(panic)
import qualified Shared.Var	as Var	
import Shared.Var		(NameSpace(..))
import Shared.Base

import Util.Generics
import Util

-----
stage	= "Source.Lint"

lintTree :: Tree SourcePos -> State [Error] (Tree SourcePos)
lintTree t	= lint t

class Lint a 
 where	lint :: a -> State [Error] a

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
 	 -> return $ PModule sp ms		

	PImportModule sp ms		
	 -> return $ PImportModule sp ms

	PExport sp a
	 -> return $ PExport sp a

	PForeign sp f
	 -> return $ PForeign sp f

	PInfix sp m i vs		
	 -> do	vs'	<- lint vs
		return $ PInfix	sp m i vs'
		
	PTypeKind sp v k		
	 -> do	v'	<- lint v
		return	$ PTypeKind sp v' k
		
	PTypeSynonym sp v t		
	 -> do	v'	<- lint v
		t'	<- lint t
		return	$ PTypeSynonym	sp v' t'
	
	PData sp v vs cs		
	 -> do	v'	<- lint v
		vs'	<- lint vs
		return	$ PData	sp v' vs' cs
		
	PRegion sp v
	 -> do	v'	<- lint v
		return	$ PRegion sp v'
	
	PEffect sp v k	
	 -> do	v'	<- lint v
		return	$ PEffect sp v' k

	-- classes
	PClass sp v k			
	 -> do	v'	<- lint v
		return	$ PClass sp v' k
		
	PClassDict sp v vs inh sigs	
	 -> do	v'	<- lint v
		vs'	<- lint vs
		inh'	<- lint inh
		sigs'	<- lint sigs
		return	$ PClassDict sp v' vs' inh' sigs'

	PClassInst sp v ts inh stmts	
	 -> do	v'	<- lint v
	 	ts'	<- lint ts
		inh'	<- lint inh
		stmts'	<- lint stmts
		return	$ PClassInst sp v' ts' inh' stmts'

	PProjDict sp t ss
	 -> do	return	$ PProjDict sp t ss

	PStmt s				
	 -> do	s'	<- lint s
		return	$ PStmt	s'
	

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
	 -> return	$ SBindFun sp v ws as
	
	SBindPat sp w x
	 -> return	$ SBindPat sp w x
	
	SBindMonadic sp w x
	 -> return	$ SBindMonadic sp w x	


-- Exp --------------------------------------------------------------------------------------------
instance Lint (Exp SourcePos) where
 lint x
  = case x of
	XNil				
	 -> return x

	XLit{}
	 -> return x

	XVar sp v				
	 | not $ inSpaceV [v]	-> death x "XVar - var in wrong namespace."
	 | otherwise			
	 -> do	v'	<- lint v
	 	return	$ XVar sp v'
	
	XObjField{}
	 -> return x
	
	XProj sp e p
	 -> do	e'	<- lint e
		return	$ XProj sp e' p
	
	XProjT{}
	 -> return x
	
	XLambdaPats{}
	 -> return x
	
	XLambdaProj{}
	 -> return x

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

	XMatch{}
	 -> return x

	XDo sp ss				
	 | isNil ss	-> death x "XDo - no statements."
	 | otherwise	
	 -> do	ss'	<- lint ss
		return	$ XDo sp ss'

	XLet sp ss e			
	 | isNil ss	-> death x "XLet - no bindings."
	 | otherwise			
	 -> do	ss'	<- lint ss
		e'	<- lint e
		return	$ XLet sp ss' e'

	XIfThenElse sp e1 e2 e3		
	 -> do	e1'	<- lint e1
		e2'	<- lint e2
		e3'	<- lint e3
		return	$ XIfThenElse sp e1' e2' e3'

	XTry{}
	 -> return x
	
	XThrow{}
	 -> return x
	
	XWhere{}
	 -> return x
	
	XTuple{}
	 -> return x
	
	XList{}
	 -> return x
	
	XListRange{}
	 -> return x
	
	XListComp{}
	 -> return x
	
	XWhile{}
	 -> return x
	
	XWhen{}
	 -> return x
	
	XUnless{}
	 -> return x
	
	XBreak{}
	 -> return x
	
 	XDefix{}
  	 -> death x "XDefix - should have been eliminated by Source.Defix."

	XDefixApps{}
	 -> death x "XDefixApps - should have been eliminated by Source.DefixApps"
	
	XOp{}
	 -> return x
		
	XApp sp e1 e2		
	 -> do	e1'	<- lint e1
		e2'	<- lint e2
		return	$ XApp sp e1' e2'

	XAppSusp{}
	 -> return x

	XParens{}
	 -> return x


-- Alt --------------------------------------------------------------------------------------------
instance Lint (Alt SourcePos) where
 lint a
  = case a of				
	APat{}
	 -> return a

	AAlt{}
	 -> return a
	
	ADefault sp x	
	 -> do	x'	<- lint x
		return	$ ADefault sp x'
		
	
	
-- Type --------------------------------------------------------------------------------------------
instance Lint Type where
 lint tt
  = case tt of
	TNil 
	 ->	return tt
	
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
		return	$ TFetters t' fs

	TConstrain ts cs
	 -> do	ts'	<- lint ts
		return	$ TConstrain ts' cs
	
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
	

-- Kind -------------------------------------------------------------------------------------------
instance Lint Kind where
 lint k	
  = return k

-- Var --------------------------------------------------------------------------------------------
instance Lint Var where
 lint v
 	| Var.bind v		== Var.XNil
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
-- inSpaceT vs		= and $ map (\v -> Var.nameSpace v == NameType)	  vs
inSpaceE vs		= and $ map (\v -> Var.nameSpace v == NameEffect) vs


