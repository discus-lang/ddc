module Llvm.Runtime.Error where

import Llvm
import Llvm.Runtime.Object


deathCase :: LlvmFunctionDecl
deathCase = LlvmFunctionDecl "_deathCase" External CC_Ccc LMVoid FixedArgs
			[(LMPointer i8, []), (i32, []), (i32, [])] ptrAlign



{-
void	_deathEval (void)
		__attribute__((noreturn));

void	_panicOutOfHeap
		(UInt allocCount, UInt64 heapSize)
		__attribute__((noreturn));

void	_panicOutOfSlots (void)
		__attribute__((noreturn));

void	_panicApply (void)
		__attribute__((noreturn));


void	_panicSlotUnderflow (void)
		__attribute__((noreturn));


void	_panicCorruption (void)
		__attribute__((noreturn));

-}
