
// Static configuration options for the runtime system.
//	These options are compiled in and are not settable on the command line.
#ifndef _DDC_Config
#define _DDC_Config

#define _DDC_VERSION "DDC Runtime System"

// -- Defaults ----------------------------------------------------------------
//	Default size of heaps, and stacks. Can be overridden by +RTS flags.
//	We're setting the default heap size to 100 megs because the runtime
//	can't grow the heap when it runs out yet.
//
#define _DDC_DEFAULT_HEAPSIZE		100000000
#define _DDC_DEFAULT_SLOTSTACKSIZE	100000
#define _DDC_DEFAULT_CONTEXTSTACKSIZE	50


// -- Profiling ---------------------------------------------------------------
//	Set these to 1/0 to enable/disable profiling of that system.
//	Once this is done the options should show up when running the 
//	compiled binaries with +RTS -help
//
#define _DDC_PROFILE_GC		1
#define _DDC_PROFILE_SLOT	0
#define _DDC_PROFILE_CONTEXT	0
#define _DDC_PROFILE_ALLOC	0
#define _DDC_PROFILE_BOXING	0
#define _DDC_PROFILE_APPLY	0

void	_ddcConfigSetup();


// -- Debugging ---------------------------------------------------------------
// These can be useful when you get desperate.
// TODO: Unify these various options into a single DEBUG option that turns
//	 on all possible consistency checks. If the RTS is crashing then
//	 we'll want to turn on all the options anyway.

// Do some consistency checks (0/1)
#define _DDC_DEBUG		0

// ---- 
#if _DDC_DEBUG 
# define _DEBUG(x)	x
# define _DDC_DEBUG_GC	1
#else
# define _DEBUG(x)
# define _DDC_DEBUG_GC  0
#endif


// -- Tracing -----------------------------------------------------------------
// Spew lots of tracing info to the console
// #define	_DDC_TRACE
#define _DDC_TRACE_GC		0

#endif

