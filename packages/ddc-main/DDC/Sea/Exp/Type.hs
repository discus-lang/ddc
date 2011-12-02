{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Sea.Exp.Type
	( Type		(..)
	, TyCon		(..)
	, typeIsBoxed
	, unboxedSize
	, tObj,   tPtrObj)
where

import DDC.Base.DataFormat
import DDC.Main.Error
import DDC.Var
import DDC.Var.PrimId

stage = "DDC.Sea.Exp"

-- | Sea types.
--	By the time we've reached the Sea language we only care about operational information.
--	We need to distinguish between boxed and unboxed values, but not much else.
data Type
	-- | The void type.
	= TVoid

	-- | The function type.
	| TFun [Type] Type

	-- | An unboxed pointer to something else.
	| TPtr Type

	-- | An unboxed data object.
	| TCon TyCon
	deriving (Show, Eq)


data TyCon
	-- | Some anonymous boxed object.
	--   This might be algabraic data, a non-algebraic boxed object like an array
	--   of unboxed ints, a thunk or suspension.
	--   This is a supertype of `TyConData` and `TyConThunk`.
	= TyConObj

	-- | An unboxed, primitive type.
 	| TyConUnboxed Var

	-- | Some abstract type that we don't know anything about.
	--   The are types like FILE which are defined by the system libraries, and will
	--   usually be be referenced via a pointer.
	| TyConAbstract Var
	deriving (Show, Eq)


-- | Check if a type represends a boxed object.
--   Boxed objects have a uniform representation with a constructor tag
--   and format information in the first word. Boxed objects are managed
--   by the Garbage Collector, and we must store pointers to them on the
--   GC shadow stack.
typeIsBoxed :: Type -> Bool
typeIsBoxed tt
 = case tt of
	TPtr (TCon TyConObj)	-> True
	_			-> False


unboxedSize :: Type -> Int
unboxedSize (TCon (TyConUnboxed v))
 = case varId v of
	VarIdPrim (TChar (UnboxedBits 32))	-> 4

	VarIdPrim (TFloat (UnboxedBits 32))	-> 4
	VarIdPrim (TFloat (UnboxedBits 64))	-> 8

	VarIdPrim (TInt (UnboxedBits 8))	-> 1
	VarIdPrim (TInt (UnboxedBits 16))	-> 2
	VarIdPrim (TInt (UnboxedBits 32))	-> 4
	VarIdPrim (TInt (UnboxedBits 64))	-> 8

	VarIdPrim (TWord (UnboxedBits 8))	-> 1
	VarIdPrim (TWord (UnboxedBits 16))	-> 2
	VarIdPrim (TWord (UnboxedBits 32))	-> 4
	VarIdPrim (TWord (UnboxedBits 64))	-> 8

	vid -> panic stage $ "unboxedSize : " ++ show vid

unboxedSize t = panic stage $ "unboxedSize : " ++ show t

-- Short hand -------------------------------------------------------------------------------------
tObj		= TCon TyConObj
tPtrObj		= TPtr tObj



