
#ifndef _Disciple_Context
#define _Disciple_Context

#include "Runtime.h"
#include "Prim.h"
#include "State.h"
#include <setjmp.h>

#define CONTEXT_RET_PUSH	0
#define CONTEXT_RET_THROW	1
#define CONTEXT_RET_BREAK	2


struct Context
{
	// The C stack context 
	jmp_buf env;
	
	// The GC slot ptr
	Obj**	slotPtr;
};

jmp_buf* 	_contextSetup	();
void		_contextRestore	(int retValue);

// static inline int  _contextPush ()
//	{ return setjmp (*_contextSetup()); }

#define _contextPush() \
	setjmp (*_contextSetup());

static inline void _contextPop ()
	{ _ddcContextIndex--; }

static inline void _contextThrow ()
	{ _contextRestore (CONTEXT_RET_THROW); }

static inline void _contextBreak ()
	{ _contextRestore (CONTEXT_RET_BREAK); }

static inline void _contextAgain (int ret)
	{ _contextRestore (ret); }


#include "Context.ci"

#endif
