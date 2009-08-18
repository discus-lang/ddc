
// Static configuration options for the runtime system.
//	These options are compiled in and are not settable on the command line.
#ifndef _DDC_Config
#define _DDC_Config

#define _DDC_VERSION "DDC Runtime System"


// Profiling options.
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

// Load the above hash defines into the RTS's config data structure.
void	_ddcConfigSetup();


// Debugging options
//	These can be useful when you get desperate.
//	TODO: 	Unify these various options into a single DEBUG option that turns
//		on all possible consistency checks. If the RTS is crashing then
//		we'll want to turn on all the options anyway.

// Spew lots of tracing info to the console
// #define	_DDC_TRACE

// Do some consistency checks
#define _DDC_DEBUG		0

// Enable consistency checks and tracing for the garbage collector (0/1)
#define _DDC_DEBUG_GC		0
#define _DDC_TRACE_GC		0

// Run some extra code
//#define _DEBUG(x)	x;
#define _DEBUG(x)


#endif

