{-# OPTIONS -fwarn-unused-imports #-}

-- | Helpers for converting Sea to LLVM code.
module Llvm.Runtime
	( runtimeEnter
	, runtimeLeave

	, panicOutOfSlots
	, panicSlotUnderflow
	, ddcSlotPtr
	, ddcSlotMax
	, ddcSlotBase )
where

import DDC.Main.Error

import Llvm
import Llvm.GhcReplace.Unique
import Llvm.Util


stage = "Llvm.Runtime"

-- | Generate LLVM code that reserves the required number of GC slots
-- at the start of a function.
runtimeEnter :: Int -> [LlvmStatement]
runtimeEnter count
 =	[ Comment ["_ENTER (" ++ show count ++ ")"]
	, Assignment enter0 (Load ddcSlotPtr)
	, Assignment enter1 (GetElemPtr True enter0 [llvmWordLitVar count])
	, Store enter1 ddcSlotPtr

	, Assignment enter2 (Load ddcSlotMax)
	, Assignment enter3 (Compare LM_CMP_Ult enter1 enter2)
	, BranchIf enter3 (LMLocalVar egood LMLabel) (LMLocalVar epanic LMLabel)
	, MkLabel epanic
	, Expr (Call StdCall (LMGlobalVar "_panicOutOfSlots" (LMFunction panicOutOfSlots) External Nothing Nothing True) [] [NoReturn])
	, Branch (LMLocalVar egood LMLabel)
	, MkLabel egood
	, Comment ["----- Slot initiialization -----"]
	]

	++ slotInit egood count
	++ [ Comment ["---------------------------------------------------------------"] ]
    where
	enter0 = LMNLocalVar "enter.0" ppObj
	enter1 = LMNLocalVar "enter.1" ppObj
	enter2 = LMNLocalVar "enter.2" ppObj
	enter3 = LMNLocalVar "enter.3" i1
	epanic = fakeUnique "enter.panic"
	egood = fakeUnique "enter.good"


-- | Generate LLVM code that releases the required number of GC slots
-- at the start of a function.
runtimeLeave :: Int -> [LlvmStatement]
runtimeLeave count
 =	[ Comment ["---------------------------------------------------------------"]
	, Comment ["_LEAVE (" ++ show count ++ ")"]
	, Assignment leave0 (Load ddcSlotPtr)
	, Assignment leave1 (GetElemPtr True leave0 [llvmWordLitVar (-count)])
	, Store leave1 ddcSlotPtr

	, Assignment leave2 (Load ddcSlotBase)
	, Assignment leave3 (Compare LM_CMP_Ult leave1 leave2)
	, BranchIf leave3 (LMLocalVar lpanic LMLabel) (LMLocalVar lgood LMLabel)
	, MkLabel lpanic
	, Expr (Call StdCall (LMGlobalVar "_panicSlotUnderflow" (LMFunction panicSlotUnderflow) External Nothing Nothing True) [] [NoReturn])
	, Branch (LMLocalVar lgood LMLabel)
	, MkLabel lgood

	, Comment ["---------------------------------------------------------------"]
	]
    where
	leave0	= LMNLocalVar "leave.0" ppObj
	leave1	= LMNLocalVar "leave.1" ppObj
	leave2	= LMNLocalVar "leave.2" ppObj
	leave3	= LMNLocalVar "leave.3" i1
	lpanic	= fakeUnique "leave.panic"
	lgood	= fakeUnique "leave.ok"


slotInit :: Unique -> Int -> [LlvmStatement]
slotInit _ count
 | count < 0
 = panic stage $ "Asked for " ++ show count ++ " GC slots."

slotInit _ count
 | count < 8
 =	Assignment init0 (Load ddcSlotPtr) : concatMap build [1 .. count]
    where
	init0 = LMNLocalVar "init.0" ppObj
	build n
	 =	let target = LMNLocalVar ("init.target." ++ show n) ppObj
		in	[ Assignment target (GetElemPtr False init0 [llvmWordLitVar (-n)])
			, Store nullObj target ]

slotInit initstart n
 | otherwise
 =	[ Assignment init0 (Load ddcSlotPtr)
	, Branch (LMLocalVar initloop LMLabel)

	, MkLabel initloop
	, Assignment index (Phi llvmWord [((llvmWordLitVar (0 :: Int)), (LMLocalVar initstart LMLabel)), (indexNext, (LMLocalVar initloop LMLabel))])
	, Assignment tmp (LlvmOp LM_MO_Add index (llvmWordLitVar (-n)))

	, Assignment target (GetElemPtr False init0 [tmp])
	, Store nullObj target

	, Assignment indexNext (LlvmOp LM_MO_Add index (llvmWordLitVar (1 :: Int)))
	, Assignment initdone (Compare LM_CMP_Eq indexNext (llvmWordLitVar n))
	, BranchIf initdone (LMLocalVar initend LMLabel) (LMLocalVar initloop LMLabel)
	, MkLabel initend
	]
    where
	initloop	= fakeUnique "init.loop"
	initend		= fakeUnique "init.end"
	init0		= LMNLocalVar "init.0" ppObj
	index		= LMNLocalVar "init.index" llvmWord
	indexNext	= LMNLocalVar "init.index.next" llvmWord
	initdone	= LMNLocalVar "init.done" i1
	tmp		= LMNLocalVar "init.tmp" llvmWord
	target		= LMNLocalVar "init.target" ppObj

--------------------------------------------------------------------------------
-- Data types and variables.

panicOutOfSlots :: LlvmFunctionDecl
panicOutOfSlots = LlvmFunctionDecl "_panicOutOfSlots" External CC_Ccc LMVoid FixedArgs [] ptrAlign

panicSlotUnderflow :: LlvmFunctionDecl
panicSlotUnderflow = LlvmFunctionDecl "_panicSlotUnderflow" External CC_Ccc LMVoid FixedArgs [] ptrAlign


ddcSlotPtr :: LlvmVar
ddcSlotPtr = pVarLift (LMGlobalVar "_ddcSlotPtr" ppObj External Nothing ptrAlign False)

ddcSlotMax :: LlvmVar
ddcSlotMax = pVarLift (LMGlobalVar "_ddcSlotMax" ppObj External Nothing ptrAlign False)

ddcSlotBase :: LlvmVar
ddcSlotBase = pVarLift (LMGlobalVar "_ddcSlotBase" ppObj External Nothing ptrAlign False)


