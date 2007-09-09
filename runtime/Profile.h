
#ifndef _Trauma_Profile
#define _Trauma_Profile

#include <stdio.h>
#include <sys/times.h>

#include "Types.h"
#include "Apply.h"
#include "Config.h"

// helper macros
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




// -- ProfileEnable
typedef enum {
	_ProfileEnableGC		= 0x01,
	_ProfileEnableSlot		= 0x02,
	_ProfileEnableAlloc		= 0x04,
	_ProfileEnableContext		= 0x08,
	_ProfileEnableBoxing		= 0x10,
	_ProfileEnableApply		= 0x20
} _ProfileEnable;
	

// -- GC Profile.
typedef struct {
	Word64	count;				// number of GC's performed.

	Word64	copyCount;			// number of objects copied.
	Word64	copyBytes;			// number of bytes copied.

	Word64	allocBytes;			// number of bytes allocated 
						//	determined by checking size of heap at each collection.

	Word64	lastCompactionSize;		// Size of the heap after the last time the GC compacted it, in bytes.
						//	This is used to healp track how much data is allocated.
	
	clock_t	timeMutatorUser;		// Clock ticks used by mutator.
	clock_t	timeMutatorSystem;
	
	clock_t	timeCollectorUser;		// Clock ticks user by collector.
	clock_t	timeCollectorSystem;
	
	clock_t	timeMarkUser;			// Buffer to hold the time of the start of the
	clock_t	timeMarkSystem;			//	current mutate / collect cycle
} _ProfileGC;


// -- Slot stack profile.
typedef struct {
	Obj**	base;			// base of slot stack.
	Obj**	highWater;			// maximum element used in slot stack.
} _ProfileSlot;	


// -- Context stack Profile.
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


// -- Alloc Profile.
typedef struct {
	Word64	count;
	Word64	bytes;
} _ProfileAllocRec;

typedef struct {
	_ProfileAllocRec	total;

	_ProfileAllocRec	data;
	_ProfileAllocRec	dataRaw;
	_ProfileAllocRec	dataRawSmall;
	_ProfileAllocRec	dataMixed;

	_ProfileAllocRec	thunk;
	_ProfileAllocRec	thunkCopy;
	_ProfileAllocRec	susp;
} _ProfileAlloc;


// -- Boxing Profile.
typedef struct {
	Word64	count;				// number of elements boxed.
	Word64	bytes;				// number of bytes allocated by boxing.
	Word64	gets;				// number of elements unboxed.
} _ProfileBoxingRec;

typedef struct {
	_ProfileBoxingRec	bString;
	_ProfileBoxingRec	bChar8;
	_ProfileBoxingRec	bWord8;
	_ProfileBoxingRec	bInt32;
	_ProfileBoxingRec	bFloat32;
} _ProfileBoxing;


// -- Apply Profile
typedef struct {
	Word64	eval	[_evalMaxAirity+1][_evalMaxImm+1];
	Word64	apply	[_evalMaxImm+1];
	Word64	forceCount;			// number of times _force() was called
	Word64	force[5];
} _ProfileApply;	



// -----
typedef struct {
	_ProfileEnable		built;		// What profiling options the RTS has been built with.
	_ProfileEnable		enable;		// What profiling options the user has turned on.

	_ProfileGC		gc;
	_ProfileSlot		slot;
	_ProfileAlloc		alloc;	
	_ProfileContext		context;
	_ProfileBoxing		boxing;
	_ProfileApply		apply;
} _Profile;


_Profile*
	_ddcProfileNew ();

void	_ddcProfilePrint 	(FILE* file, _Profile* p);
void	_ddcPrettySize	 	(char* str,  Word64 x);

void	_ddcProfileMutatorStart	();			// Process time delimited by these functions will be 
void	_ddcProfileMutatorEnd 	();			//	attributed to the mutator.

void	_ddcProfileCollectorStart ();			// Process time delimited by these functions will be
void	_ddcProfileCollectorEnd	  ();			//	attributed to the collector.
	

#endif

