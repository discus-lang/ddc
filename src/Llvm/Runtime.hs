{-# OPTIONS -fwarn-unused-imports #-}

-- | Helpers for converting Sea to LLVM code.
module Llvm.Runtime
	( runtimeEnter
	, runtimeLeave

	, module Llvm.GhcReplace.Unique
	, module Llvm.Runtime.Alloc
	, module Llvm.Runtime.Apply
	, module Llvm.Runtime.Boxing
	, module Llvm.Runtime.Data
	, module Llvm.Runtime.Error
	, module Llvm.Runtime.Slot
	, module Llvm.Runtime.Tags )
where

import DDC.Main.Error

import Llvm
import LlvmM
import Llvm.GhcReplace.Unique
import Llvm.Runtime.Alloc
import Llvm.Runtime.Apply
import Llvm.Runtime.Boxing
import Llvm.Runtime.Data
import Llvm.Runtime.Error
import Llvm.Runtime.Slot
import Llvm.Runtime.Tags

import Llvm.Util

stage :: String
stage = "Llvm.Runtime"

-- | Generate LLVM code that reserves the required number of GC slots
-- at the start of a function.
runtimeEnter :: Int -> LlvmM ()
runtimeEnter 0
 = addBlock $	[ Comment ["_ENTER (0)"]
		, Comment ["---------------------------------------------------------------"]
		]

runtimeEnter count
 = do	let enter1	= LMNLocalVar "enter.1" ppObj
	let enter2	= LMNLocalVar "enter.2" ppObj
	let enter3	= LMNLocalVar "enter.3" i1
	let epanic	= fakeUnique "enter.panic"
	let egood	= fakeUnique "enter.good"
	addGlobalFuncDecl panicOutOfSlots
	addBlock $
		[ Comment ["_ENTER (" ++ show count ++ ")"]
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
	slotInit egood count
	addBlock [ Comment ["---------------------------------------------------------------"] ]


-- | Generate LLVM code that releases the required number of GC slots
-- at the start of a function.
runtimeLeave :: Int -> LlvmM ()
runtimeLeave 0
 = addBlock
	[ Comment ["---------------------------------------------------------------"]
	, Comment ["_LEAVE (0)"]
	, Comment ["---------------------------------------------------------------"]
	]

runtimeLeave count
 = addBlock
	[ Comment ["---------------------------------------------------------------"]
	, Comment ["_LEAVE (" ++ show count ++ ")"]
	, Store localSlotBase ddcSlotPtr
	, Comment ["---------------------------------------------------------------"]
	]


slotInit :: Unique -> Int -> LlvmM ()
slotInit _ count
 | count < 0
 = panic stage $ "Asked for " ++ show count ++ " GC slots."

slotInit _ count
 | count < 8
 = let	build n
	 =	let target = LMNLocalVar ("init.target." ++ show n) ppObj
		in	[ Assignment target (GetElemPtr False localSlotBase [llvmWordLitVar n])
			, Store nullObj target ]
   in	addBlock $ concatMap build [0 .. (count - 1)]


slotInit initstart n
 | otherwise
 = do	let initloop	= fakeUnique "init.loop"
	let initend		= fakeUnique "init.end"
	let index		= LMNLocalVar "init.index" llvmWord
	let indexNext	= LMNLocalVar "init.index.next" llvmWord
	let initdone	= LMNLocalVar "init.done" i1
	let target		= LMNLocalVar "init.target" ppObj
	addBlock $
		[ Branch (LMLocalVar initloop LMLabel)

		, MkLabel initloop
		, Assignment index (Phi llvmWord [((llvmWordLitVar (0 :: Int)), (LMLocalVar initstart LMLabel)), (indexNext, (LMLocalVar initloop LMLabel))])

		, Assignment target (GetElemPtr False localSlotBase [index])
		, Store nullObj target

		, Assignment indexNext (LlvmOp LM_MO_Add index (llvmWordLitVar (1 :: Int)))
		, Assignment initdone (Compare LM_CMP_Eq indexNext (llvmWordLitVar n))
		, BranchIf initdone (LMLocalVar initend LMLabel)  (LMLocalVar initloop LMLabel)
		, MkLabel initend
		]


