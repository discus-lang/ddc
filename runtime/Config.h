
#ifndef _DDC_Config
#define _DDC_Config

#define _DDC_VERSION "DDC Runtime System"


// Do profiling.
//	define these symbols to 1/0 to enable/disable profiling of that system.
//
#define _DDC_PROFILE_GC		1
#define _DDC_PROFILE_SLOT	0
#define _DDC_PROFILE_CONTEXT	0
#define _DDC_PROFILE_ALLOC	0
#define _DDC_PROFILE_BOXING	0
#define _DDC_PROFILE_APPLY	0


// Emit lots of tracing info
// #define	_DDC_TRACE


// -- This enables lots of internal consistency checks (slow).
#define _DDC_DEBUG_GC		0
#define _DDC_TRACE_GC		0

#define _DDC_DEBUG		0

//#define _DEBUG(x)	x;
#define _DEBUG(x)

void	_ddcConfigSetup();


#endif

