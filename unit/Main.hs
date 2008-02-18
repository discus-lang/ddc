
module Main 
	(module Util)
where

import Type.Pretty
import Type.Exp
import Type.Util

import Shared.VarPrim
import qualified Shared.Var	as Var
import qualified Shared.VarBind	as Var

import Util

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Data.Map	as Map
import Data.Map			(Map)

-- var helpers
varX d	= (Var.new $ "x" ++ show d)
		{ Var.nameSpace = Var.NameValue
		, Var.bind	= Var.XBind "x" d }

varD d	= (Var.new $ "r" ++ show d)
		{ Var.nameSpace = Var.NameType
		, Var.bind	= Var.XBind "t" d }

varR d	= (Var.new $ "r" ++ show d)
		{ Var.nameSpace = Var.NameRegion
		, Var.bind	= Var.XBind "r" d }

varE d	= (Var.new $ "e" ++ show d)
		{ Var.nameSpace = Var.NameEffect
		, Var.bind	= Var.XBind "e" d }

varC d	= (Var.new $ "c" ++ show d)
		{ Var.nameSpace = Var.NameClosure
		, Var.bind	= Var.XBind "c" d }

	
-- type helpers
tVarD d = TVar KData    (varD d)
tVarR d = TVar KRegion  (varR d)
tVarE d = TVar KEffect  (varE d)
tVarC d = TVar KClosure (varC d)

tBotC	= TBot KClosure
tBotE	= TBot KEffect

tFetters tt fs		= TFetters fs tt
tFun 	t1 eff clo t2	= TFun t1 t2 eff clo
tIntR 	d		= TData primTInt [tVarR d]

-- effect helpers
eReadR d		= TEffect primRead [tVarR d]

-- closure helpers
cMask t1 tag		= TMask KClosure t1 (TTag tag)
cFree d t1		= TFree (varX d) t1
cSum ts			= TSum KClosure ts

-- example types
clo1		= (cSum [cFree 0 (tVarR 1), cFree 1 (tVarR 2)])
clo2		= (cMask (tVarC 1) (varX 0))
type1		= tFetters
			(tFun 	(tIntR 0) (tVarC 0) tBotE 
				(tFun (tIntR 1) (tVarC 1) (tVarE 1) (tIntR 2)))
				
			[ FLet (tVarE 1) (eReadR 1)
			, FLet (tVarC 1) (cSum [cFree 0 (tVarR 1), cFree 1 (tVarR 2)])
			, FLet (tVarC 0) (cMask (tVarC 1) (varX 0)) ]


type3		= tFetters
			(tVarD 0)
			[ FLet (tVarD 0) (tFun (tIntR 0) (tVarC 1) tBotE (tIntR 0))
			, FLet (tVarC 1) (cFree 0 (tIntR 3)) ]
			
		
-- trimC
trimC t = trimClosureC Set.empty Set.empty t
trimT t = trimClosureT Set.empty Set.empty t

outType t 	
	= putStr	
	$ pprStr
	$ "\n" %> prettyTS t % "\n\n"
