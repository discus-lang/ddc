{-# OPTIONS -fno-warn-unused-binds -fno-warn-type-defaults -cpp #-}

-- | Wrappers for compiler stages dealing with LLVM code.
module Llvm.Assign
	(llvmOfAssign)
where

import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Main.Error
import DDC.Sea.Exp

import Llvm
import LlvmM
import Llvm.Runtime
import Llvm.Util
import Llvm.XPrim
import Llvm.Var


stage = "Llvm.Assign"

debug = True

llvmOfAssign :: Exp a -> Type -> Exp a -> LlvmM ()
llvmOfAssign (XVar (NSlot v i) (TPtr (TCon TyConObj))) t@(TPtr (TCon TyConObj)) src
 = do	reg	<- loadExp t src
	writeSlot reg i

llvmOfAssign (XVar n1@NAuto{} t1) t@(TPtr (TCon TyConObj)) (XVar n2@NSlot{} t2)
 | t1 == t && t2 == t
 =	readSlotVar (nameSlotNum n2) $ toLlvmVar (varOfName n1) t


llvmOfAssign (XVar v1@NCaf{} t1) t@(TPtr (TPtr (TCon TyConObj))) (XVar v2@NRts{} t2)
 | t1 == t && t2 == t
 = do	src		<- newUniqueReg $ toLlvmType t
	addBlock	[ Assignment src (loadAddress (toLlvmCafVar (varOfName v2) t2))
			, Store src (pVarLift (toLlvmCafVar (varOfName v1) t1)) ]



llvmOfAssign (XVar v1@NCafPtr{} t1) t@(TPtr (TCon TyConObj)) (XLit (LLit (LiteralFmt (LInt 0) Unboxed)))
 | t1 == t
 = do	dst		<- newUniqueReg $ pLift $ toLlvmType t1
	addBlock	[ Assignment dst (loadAddress (pVarLift (toLlvmCafVar (varOfName v1) t1)))
			, Store (LMLitVar (LMNullLit (toLlvmType t1))) dst ]


llvmOfAssign (XVar v1@NCafPtr{} t1) t@(TPtr (TCon TyConObj)) x@(XPrim op args)
 | t1 == t
 = do	result		<- llvmOfXPrim op args
	addBlock	[ Store result (pVarLift (toLlvmCafVar (varOfName v1) t)) ]



llvmOfAssign xv@(XVar v1@NRts{} t1) _ b@(XPrim op args)
 = do	addComment  (stage ++ " (" ++ (show __LINE__) ++ ") llvmOfAssig\n" ++ (show v1) ++ "\n" ++ (show b) ++ "\n")
	result	<- llvmOfXPrim op args
	addBlock	[ Store result (pVarLift (llvmVarOfXVar xv)) ]






llvmOfAssign a b c
 = panic stage $ "llvmOfAssign (" ++ (show __LINE__) ++ ") Unhandled : \n"
	++ {- take 150 -} (show a) ++ "\n"
	++ {- take 150 -} (show b) ++ "\n"
	++ {- take 150 -} (show c) ++ "\n"



--------------------------------------------------------------------------------

loadExp :: Type -> Exp a -> LlvmM LlvmVar
loadExp (TPtr (TCon TyConObj)) (XVar n t@(TPtr (TCon TyConObj)))
 = 	return $ toLlvmVar (varOfName n) t

loadExp (TPtr (TCon TyConObj)) (XPrim op args)
 =	llvmOfXPrim op args

loadExp t src
 = panic stage $  " (" ++ (show __LINE__) ++ ") loadExp\n"
	++ show t ++ "\n"
	++ show src ++ "\n"




