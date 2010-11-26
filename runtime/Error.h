
// Various error messages that halt the program.
//	They all have the ((noreturn)) attribute so GCC can generate
//	better code for the enclosing function.

#ifndef _DDC_Error
#define _DDC_Error

#include "Runtime.h"

void	_deathCase
		(const char* moduleName, int line, int column)
		__attribute__((noreturn));

void	_deathEval (void)
		__attribute__((noreturn));

void	_panicOutOfHeap
		(size_t allocCount, size_t heapSize)
		__attribute__((noreturn));

void	_panicOutOfSlots (void)
		__attribute__((noreturn));

void	_panicApply (void)
		__attribute__((noreturn));


void	_panicSlotUnderflow (void)
		__attribute__((noreturn));


void	_panicCorruption (void)
		__attribute__((noreturn));


#endif
