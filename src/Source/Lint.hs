
module Source.Lint
(
	lintFinal
)

where

-----
import	Util.Generics
import Util

-----
import	Shared.Error	(panic)
import	qualified Shared.Var	as Var	
import Shared.Var		(NameSpace(..))
import Shared.Base

import	Source.Exp
import	Type.Exp
import	Type.Pretty

-----
stage	= "Source.Lint"

-----------------------
-- Check source tree for lint before handing
--	it off to the type checker.
--	
lintFinal ::	Show a => Tree a -> Tree a
lintFinal	t	= lint t

class Lint a  
 where
 	lint :: a -> a

instance Lint a => Lint [a] 
 where
 	lint xx	= map lint xx
 
instance Lint a => Lint (Maybe a) 
 where
 	lint xx	= liftM lint xx
 
instance (Lint a, Lint b) => Lint (a, b) 
 where
	lint (a, b)	= (lint a, lint b)
 
-----
death x s
 	= panic stage 
	$ "lint: " 	% s 		% "\n"
	% "  snip = " 	% show x	% "\n"


-----------------------
-- Top
--
instance Show a => Lint (Top a) where
 lint xx
  = case xx of
	PPragma sp es			-> PPragma sp es
 	PModule sp ms			-> PModule sp ms		
	PImportExtern sp v tv to		
	 | not $ inSpaceV [v]		-> death xx "PImportExtern - vars in wrong namespace."
	 | otherwise			-> PImportExtern sp (lint v) (lint tv) (lint to)

	PImportModule sp ms		-> PImportModule sp ms

	PType sp v t			-> PType	sp (lint v) (lint t)
	PInfix sp m i vs		-> PInfix	sp m i      (lint vs)
	
	PData sp v vs cs		-> PData	sp (lint v) (lint vs) cs

	PRegion sp v			-> PRegion	sp (lint v)
	PEffect sp v k			-> PEffect 	sp (lint v) k

	-- classes
	PClass sp v k			-> PClass 	sp (lint v) k
	PClassDict sp v vs inh sigs	-> PClassDict	sp (lint v) (lint vs) (lint inh) (lint sigs)
	PClassInst sp v ts inh stmts	-> PClassInst	sp (lint v) (lint ts) (lint inh) (lint stmts)

	PStmt s				-> PStmt	(lint s)
	

-----------------------
-- Stmt
--
instance Show a => Lint (Stmt a) where
 lint s
  = case s of
	SStmt sp x			-> SStmt sp (lint x)
	SSig  sp v t			-> SSig  sp (lint v) (lint t)

-----------------------
-- Exp
--
instance Show a => Lint (Exp a) where
 lint x
  = case x of
	XNil				-> x
	XUnit sp			-> x
	XVoid sp			-> x

	XLet sp ss e			
	 | isNil ss			-> death x "XLet - no bindings."
	 | otherwise			-> XLet sp (lint ss) (lint e)

	XLambda	sp vs e			
	 | otherwise			-> XLambda sp  (lint vs) (lint e)

	XApp sp e1 e2			-> XApp sp (lint e1) (lint e2)

	XCase sp o aa			
	 | isNil aa			-> death x "XCase - no alternatives."
	 | otherwise			-> XCase sp (lint o) (lint aa)

	XLambdaCase sp aa		
	 | isNil aa			-> death x "XCaseL - no alternatives."
	 | otherwise			-> XLambdaCase sp (lint aa)
	
	XLambdaProj sp j xs		-> XLambdaProj sp j xs
	
	XConst sp c			-> XConst sp c

	XVar sp v				
	 | not $ inSpaceV [v]		-> death x "XVar - var in wrong namespace."
	 | otherwise			-> XVar sp (lint v)
	
	XDo sp ss				
	 | isNil ss			-> death x "XDo - no statements."
	 | otherwise			-> XDo sp (lint ss)
	 
	XIfThenElse sp e1 e2 e3		-> XIfThenElse sp (lint e1) (lint e2) (lint e3)

 	XDefix{}			-> death x "XDefix - should have been eliminated by Source.Defix."
	
	XProj sp e p			-> XProj sp (lint e) p
	
	XAppE sp e1 e2 eff		-> XAppE  sp (lint e1) (lint e2) (lint eff)
	
	XCaseE sp e1 alts eff		-> XCaseE sp (lint e1) (lint alts) (lint eff)
	
	XAt sp v x				
	 | not $ inSpaceV [v]		-> death x "XAt - var in wrong namespace."
	 | otherwise			-> XAt sp   (lint v) (lint x)

-----------------------
-- Alt
--
instance Show a => Lint (Alt a) where
 lint a
  = case a of				
	ADefault sp x	-> ADefault sp (lint x)
	
-----------------------
-- Type
-- 
instance Lint Type where
 lint tt
  = case tt of

	TForall vks t
	 | isNil vks			-> death tt "TForall - quant var list is empty."
	 | otherwise			-> TForall vks (lint t)


	TFetters fs t			
	 | isNil fs			-> death tt "TFetters - list is empty"
	 | otherwise			-> TFetters fs (lint t)

  	TVar k v
	 | not $ inSpaceT [v]		-> death tt "TVar - var in wrong namespace."
	 | otherwise			-> TVar k (lint v)

	TSum k ts			-> TSum k (lint ts)
	TMask k t1 t2			-> TMask k (lint t1) (lint t2)

	TTop k				-> TTop k
	TBot k				-> TBot k

	-- data
	TFun t1 t2 eff clo		-> TFun (lint t1) (lint t2) (lint eff) (lint clo)
	
	TData v ts
	 | not $ inSpaceT [v]		-> death tt "TCon - var in wrong namespace."
	 | otherwise			-> TData (lint v) (lint ts)
	 	
	-- effect
	TEffect v ts			-> TEffect v (lint ts)
			
	-- closure
	TFree v t			-> TFree v (lint t)
		
	_ 				-> death tt "unexpected constructor."

	 

-----------------------
-- Var
--
instance Lint Var where
 lint v
 	| Var.bind v		== Var.XNil
	= death v "Var - var's bind code is undefined."

	| Var.name v == ""
	= death v "Var - name is the empty string."

	| Var.nameSpace v == NameNothing
	= death v "Var - var is in name space 'NameNothing'.\n"
	
	| otherwise
	= v
	
		
-----
inSpaceV vs	= and $ map (\v -> Var.nameSpace v == NameValue)  vs
-- inSpaceR vs	= and $ map (\v -> Var.nameSpace v == NameRegion) vs
inSpaceT vs	= and $ map (\v -> Var.nameSpace v == NameType)	  vs
-- inSpaceE vs	= and $ map (\v -> Var.nameSpace v == NameEffect) vs
-- inSpaceM vs	= and $ map (\v -> Var.nameSpace v == NameModule) vs


