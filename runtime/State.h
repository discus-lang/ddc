#ifndef _DDC_State_h
#define _DDC_State_h

#include "Types.h"
#include "Object.h"
#include "Debug/Profile.h"

#include <stdio.h>


// Profiling data
extern _Profile* 	_ddcProfile;


// Command line args
extern Int32		_ddcArgCount;
extern String*		_ddcArgValue;


// Heap pointers
extern Word8* 		_ddcHeapBase;		
extern Word8*		_ddcHeapPtr;
extern Word8* 		_ddcHeapMax;

extern Word8*		_ddcHeapBackBase;
extern Word8*		_ddcHeapBackPtr;
extern Word8*		_ddcHeapBackMax;


// GC slot stack
extern Obj**		_ddcSlotBase;
extern Obj**		_ddcSlotPtr;
extern Obj**		_ddcSlotMax;
extern Obj**		_ddcSlotHighWater;


// Context stack	
extern struct Context*	_ddcContextStack;
extern int		_ddcContextIndex;
extern int		_ddcContextMax;
extern Obj*		_ddcContextObject;


// Debugging / Tracing state
extern FILE*		_ddcTraceFile;
extern bool		_ddcDumpOnPanic;

#endif
