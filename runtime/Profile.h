
// Types and functions used by the RTS profiler.
//
#ifndef _DDC_Profile
#define _DDC_Profile

#include <stdio.h>
#include <sys/times.h>

#include "Types.h"
#include "Apply.h"
#include "Config.h"


// Helper macros ----------------------------------------------------------------------------------
//	These only produce run their argument code if the corresponding profiling
//	system is enabled.
//
#if _DDC_PROFILE_GC
#  define _PROFILE_GC(x)	(_ddcProfile->gc.x);
#else
#  define _PROFILE_GC(x)
#endif

#if _DDC_PROFILE_SLOT
#  define _PROFILE_SLOT(x)	(_ddcProfile->slot.x);
#else
#  define _PROFILE_SLOT(x)
#endif

#if _DDC_PROFILE_ALLOC
#  define _PROFILE_ALLOC(x)	(_ddcProfile->alloc.x);
#else
#  define _PROFILE_ALLOC(x)
#endif

#if _DDC_PROFILE_CONTEXT
#  define _PROFILE_CONTEXT(x)	(_ddcProfile->context.x);
#else
#  define _PROFILE_CONTEXT(x)
#endif

#if _DDC_PROFILE_BOXING
#  define _PROFILE_BOXING(x)	(_ddcProfile->boxing.x);
#else
#  define _PROFILE_BOXING(x)
#endif

#if _DDC_PROFILE_APPLY
#  define _PROFILE_APPLY(x)	(_ddcProfile->apply.x);
#else
#  define _PROFILE_APPLY(x)
#endif


// ProfileEnable ----------------------------------------------------------------------------------
//	Flags to enable the profiling of various parts of the runtime system
//
typedef enum {
	_ProfileEnableGC		= 0x01,
	_ProfileEnableSlot		= 0x02,
	_ProfileEnableAlloc		= 0x04,
	_ProfileEnableContext		= 0x08,
	_ProfileEnableBoxing		= 0x10,
	_ProfileEnableApply		= 0x20
} _ProfileEnable;
	

// GC Profile -------------------------------------------------------------------------------------
//	Records profiling information about the garbage collector.
//
typedef struct {
	Word64	count;				// number of GC's performed.
	
	Word64	copyCount;			// number of objects copied.
	Word64	copyBytes;			// number of bytes copied.

	Word64	allocBytes;			// number of bytes allocated 
						//	determined by checking size of heap at
						//	each collection.

	Word64	lastCompactionSize;		// Size of the heap after the last time the GC
						//	compacted it, in bytes. This is used to help
						//	track how much data is allocated.
	
	clock_t	timeMutatorUser;		// Clock ticks used by mutator.
	clock_t	timeMutatorSystem;
	
	clock_t	timeCollectorUser;		// Clock ticks user by collector.
	clock_t	timeCollectorSystem;
	
	clock_t	timeMarkUser;			// Buffer to hold the time of the start of the
	clock_t	timeMarkSystem;			//	current mutate / collect cycle
} _ProfileGC;


// Slot Stack Profile -----------------------------------------------------------------------------
typedef struct {
	Obj**	base;				// base of slot stack.
	Obj**	highWater;			// maximum element used in slot stack so far.
} _ProfileSlot;	


// Context Stack Profile --------------------------------------------------------------------------
typedef struct {
	Word64	setupCount;
	Word64	restoreCount;
	Word64	highWater;			// index of highest context stack record used.

	Word64	trysEntered;			// number of try blocks entered.
	Word64  trysCaught;			// number of times an exception was caught.
	Word64  trysContinue;			// number of times the try'd expression completed with no exception.
	Word64	trysThrough;			// number of times a context restore pushed through a try block

	Word64  throwCount;			// number of exceptions thrown.
} _ProfileContext;


// Alloc Profile ----------------------------------------------------------------------------------
// Number of allocations / bytes allocated for some sort of thing
typedef struct {
	Word64	count;				// total number of things allocated
	Word64	bytes;				// total number of bytes allocated
} _ProfileAllocRec;

typedef struct {
	_ProfileAllocRec total;			// total allocation performed of all object sorts

	// each of the following correspond with a type of object
	// defined in Types.h
	_ProfileAllocRec data;
	_ProfileAllocRec dataRaw;
	_ProfileAllocRec dataRawSmall;
	_ProfileAllocRec dataMixed;

	_ProfileAllocRec thunk;
	_ProfileAllocRec thunkCopy;
	_ProfileAllocRec susp;
} _ProfileAlloc;


// Boxing Profile ---------------------------------------------------------------------------------
// Number of boxings / bytes allocated / unboxings for some sort of data.
typedef struct {
	Word64	count;				// number of things boxed.
	Word64	bytes;				// number of bytes allocated by boxing.
	Word64	gets;				// number of things unboxed.
} _ProfileBoxingRec;

// There should be a record in this structure for each type of primitive boxed object.
typedef struct {
	_ProfileBoxingRec bString;
	_ProfileBoxingRec bChar8;
	_ProfileBoxingRec bWord8;
	_ProfileBoxingRec bInt32;
	_ProfileBoxingRec bFloat32;
} _ProfileBoxing;


// Apply Profile ----------------------------------------------------------------------------------
typedef struct {
	// Number of times each eval function from Eval.ci has beem called.
	Word64	eval	[_evalMaxAirity+1][_evalMaxImm+1];

	// Number of times each apply function from Apply.c has been called.
	Word64	apply	[_evalMaxImm+1];

	// Number of times force from Force.c has been called
	Word64	forceCount;

	// Counts of each forced function, for some function arity.
	Word64	force[5];
} _ProfileApply;	


// Top-level profiling structure ------------------------------------------------------------------
//	The top-level structure that contains all the profiling info
typedef struct {

	// What profiling options the RTS has been built with.
	_ProfileEnable		built;		

	// What profiling options the user has turned on.
	_ProfileEnable		enable;		

	// The following members will only contain data if the corresponding profiling
	//	system is enabled.
	_ProfileGC		gc;
	_ProfileSlot		slot;
	_ProfileAlloc		alloc;	
	_ProfileContext		context;
	_ProfileBoxing		boxing;
	_ProfileApply		apply;
} _Profile;


// Create a fresh new profile.
_Profile* _ddcProfileNew ();

// Print the profile to this file.
void	_ddcProfilePrint 	(FILE* file, _Profile* p);

// Pretty print a Word64 representing the size of some thing.
void	_ddcPrettySize	 	(char* str,  Word64 x);

// Process time delimited by these functions is attributed to the mutator.
void	_ddcProfileMutatorStart	();
void	_ddcProfileMutatorEnd 	();

// Process time delimited by these functions is attributed to the garbage collector.
void	_ddcProfileCollectorStart ();
void	_ddcProfileCollectorEnd	  ();
	
#endif

