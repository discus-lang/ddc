{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Simple predicates on type expressions.
module DDC.Type.Predicates 
	( -- * Single constructors.
	  isTBot
	, isTApp
	, isTClass
	, isFConstraint
	, isFWhere
	, isFMore

	 -- * Compound things.
	, isSomeTVar
	, isUnboxedT)
where
import DDC.Type.Exp
import DDC.Var
import Type.Util.Bits

-- Directly on the Type ---------------------------------------------------------------------------
isTBot :: Type -> Bool
isTBot tt
 = case tt of
	TSum _ []	-> True
	_		-> False


isTApp :: Type -> Bool
isTApp tt
 = case tt of
 	TApp{}		-> True
	_		-> False

isFConstraint :: Fetter -> Bool
isFConstraint ff
 = case ff of
 	FConstraint{}	-> True
	_		 -> False

isFWhere :: Fetter -> Bool
isFWhere ff
 = case ff of
 	FWhere{}	-> True
	_ 		-> False

isFMore :: Fetter -> Bool
isFMore ff
 = case ff of
 	FMore{}		-> True
	_ 		-> False


-- Compound things --------------------------------------------------------------------------------
isSomeTVar :: Type -> Bool
isSomeTVar tt
 = case tt of
 	TVar _ UVar{}	-> True
	TVar _ UMore{}	-> True
	_		-> False

isTClass :: Type -> Bool
isTClass tt
 = case tt of
 	TVar _ UClass{}	-> True
	_		-> False


-- | Check if a type represents some unboxed value
isUnboxedT :: Type -> Bool
isUnboxedT t
 = case takeTData t of
 	Just (v, _, _)
	 | last (varName v) == '#'	-> True	 
	_				-> False

	