
#ifndef _DDC_Alloc
#define _DDC_Alloc

#include "Types.h"

// -- Alloc
void	_allocInit		(UInt 	heapSize);
void	_allocCollect		();


static inline Obj* 	_allocData		(Tag	tag, UInt words);
static inline Obj*	_allocDataAnchored	(Tag	tag, UInt words);

static inline Obj*	_allocDataR		(Tag	tag,	UInt dataSize);
static inline Obj*	_allocDataR_anchored	(Tag	tag,	UInt dataSize);

static inline Obj*	_allocDataRS		(Tag 	tag,	UInt dataSize);
static inline Obj*	_allocDataM		(Tag	tag, 	UInt ptrCount,	UInt dataSize);
static inline Obj*	_allocThunk		(FunPtr	func,	UInt airity,	UInt args);
static inline Obj*	_allocSusp		(Obj*	thunk,	UInt args);
static inline Thunk*	_copyThunk		(Thunk*	thunk);

#endif
