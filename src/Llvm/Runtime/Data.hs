{-# OPTIONS -fno-warn-type-defaults #-}


module Llvm.Runtime.Data where

import Util

import Llvm
import LlvmM
import Llvm.Util


panicOutOfSlots :: LlvmFunctionDecl
panicOutOfSlots = LlvmFunctionDecl "_panicOutOfSlots" External CC_Ccc LMVoid FixedArgs [] ptrAlign

allocCollect :: LlvmFunctionDecl
allocCollect = LlvmFunctionDecl "_allocCollect" External CC_Ccc LMVoid FixedArgs [(i32, [])] ptrAlign


ddcSlotPtr :: LlvmVar
ddcSlotPtr = pVarLift (LMGlobalVar "_ddcSlotPtr" ppObj External Nothing ptrAlign False)

ddcSlotMax :: LlvmVar
ddcSlotMax = pVarLift (LMGlobalVar "_ddcSlotMax" ppObj External Nothing ptrAlign False)

ddcSlotBase :: LlvmVar
ddcSlotBase = pVarLift (LMGlobalVar "_ddcSlotBase" ppObj External Nothing ptrAlign False)


ddcHeapPtr :: LlvmVar
ddcHeapPtr = pVarLift (LMGlobalVar "_ddcHeapPtr" pChar External Nothing ptrAlign False)

ddcHeapMax :: LlvmVar
ddcHeapMax = pVarLift (LMGlobalVar "_ddcHeapMax" pChar External Nothing ptrAlign False)


localSlotBase :: LlvmVar
localSlotBase = LMNLocalVar "local.slotPtr" ppObj


force :: LlvmFunctionDecl
force = LlvmFunctionDecl "_force" External CC_Ccc pObj FixedArgs [(pObj, [])] ptrAlign


forceObj :: LlvmVar -> LlvmM LlvmVar
forceObj orig
 = do	let fun	= LMGlobalVar "_force" (LMFunction force) External Nothing Nothing True
	forced	<- lift $ newUniqueNamedReg "forced" pObj
	addBlock [ Assignment forced (Call StdCall fun [orig] []) ]
	return forced


followObj :: LlvmVar -> LlvmM LlvmVar
followObj orig
 = do	addComment $ "followObj " ++ show orig
	r0 	<- lift $ newUniqueReg pObj
	r1	<- lift $ newUniqueReg ppObj
	r2	<- lift $ newUniqueReg pObj
	addBlock
		[ Assignment r0 (GetElemPtr False orig [llvmWordLitVar 2])
		, Assignment r1 (Cast LM_Bitcast r0 ppObj)
		, Assignment r2 (Load r1) ]
	return	r2


getObjTag :: LlvmVar -> LlvmM LlvmVar
getObjTag obj
 = do	r0	<- lift $ newUniqueReg $ pLift i32
	r1	<- lift $ newUniqueReg $ i32
	val	<- lift $ newUniqueNamedReg "tag.val" i32
	addBlock
		[ Assignment r0 (GetElemPtr False obj [llvmWordLitVar 0, i32LitVar 0])
		, Assignment r1 (Load r0)
		, Assignment val (LlvmOp LM_MO_AShr r1 (i32LitVar 8))
		]
	return	val




baseFalse :: LlvmFunctionDecl
baseFalse = LlvmFunctionDecl "Base_False" External CC_Ccc pObj FixedArgs [] ptrAlign

baseTrue :: LlvmFunctionDecl
baseTrue = LlvmFunctionDecl "Base_True" External CC_Ccc pObj FixedArgs [] ptrAlign


