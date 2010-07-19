{-# OPTIONS -fwarn-unused-imports #-}

-- | Helpers for converting Sea to LLVM code.
module Llvm.Runtime
	( runtimeEnter
	, runtimeLeave

	, panicOutOfSlots
	, allocCollect

	, ddcSlotPtr
	, ddcSlotMax
	, ddcSlotBase

	, ddcHeapPtr
	, ddcHeapMax

	, localSlotBase )
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
	, Assignment localSlotBase (Load ddcSlotPtr)
	, Assignment enter1 (GetElemPtr True localSlotBase [llvmWordLitVar count])
	, Store enter1 ddcSlotPtr

	, Assignment enter2 (Load ddcSlotMax)
	, Assignment enter3 (Compare LM_CMP_Ult enter1 enter2)
	, BranchIf enter3 (LMLocalVar egood LMLabel) (LMLocalVar epanic LMLabel)
	, MkLabel epanic
	, Expr (Call StdCall (LMGlobalVar "_panicOutOfSlots" (LMFunction panicOutOfSlots) External Nothing Nothing True) [] [NoReturn])
	, Branch (LMLocalVar egood LMLabel)
	, MkLabel egood
	, Comment ["----- Slot initialization -----"]
	]

	++ slotInit egood count
	++ [ Comment ["---------------------------------------------------------------"] ]
    where
	enter1 = LMNLocalVar "enter.1" ppObj
	enter2 = LMNLocalVar "enter.2" ppObj
	enter3 = LMNLocalVar "enter.3" i1
	epanic = fakeUnique "enter.panic"
	egood = fakeUnique "enter.good"


-- | Generate LLVM code that releases the required number of GC slots
-- at the start of a function.
runtimeLeave :: [LlvmStatement]
runtimeLeave
 =	[ Comment ["---------------------------------------------------------------"]
	, Comment ["_LEAVE"]
	, Store localSlotBase ddcSlotPtr
	, Comment ["---------------------------------------------------------------"]
	]


slotInit :: Unique -> Int -> [LlvmStatement]
slotInit _ count
 | count < 0
 = panic stage $ "Asked for " ++ show count ++ " GC slots."

slotInit _ count
 | count < 8
 = let	build n
	 =	let target = LMNLocalVar ("init.target." ++ show n) ppObj
		in	[ Assignment target (GetElemPtr False localSlotBase [llvmWordLitVar n])
			, Store nullObj target ]
   in	concatMap build [0 .. (count - 1)]


slotInit initstart n
 | otherwise
 =	[ Branch (LMLocalVar initloop LMLabel)

	, MkLabel initloop
	, Assignment index (Phi llvmWord [((llvmWordLitVar (0 :: Int)), (LMLocalVar initstart LMLabel)), (indexNext, (LMLocalVar initloop LMLabel))])

	, Assignment target (GetElemPtr False localSlotBase [index])
	, Store nullObj target

	, Assignment indexNext (LlvmOp LM_MO_Add index (llvmWordLitVar (1 :: Int)))
	, Assignment initdone (Compare LM_CMP_Eq indexNext (llvmWordLitVar n))
	, BranchIf initdone (LMLocalVar initend LMLabel)  (LMLocalVar initloop LMLabel)
	, MkLabel initend
	]
    where
	initloop	= fakeUnique "init.loop"
	initend		= fakeUnique "init.end"
	index		= LMNLocalVar "init.index" llvmWord
	indexNext	= LMNLocalVar "init.index.next" llvmWord
	initdone	= LMNLocalVar "init.done" i1
	target		= LMNLocalVar "init.target" ppObj

--------------------------------------------------------------------------------
-- Data types and variables.

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

