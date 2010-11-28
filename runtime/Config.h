
// Static configuration options for the runtime system.
//	These options are compiled in and are not settable on the command line.
#ifndef _DDC_Config
#define _DDC_Config

#define _DDC_VERSION "DDC Runtime System v0.1.2"

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


// -- Debugging ---------------------------------------------------------------
// 	Turn on lots of expensive consitency checks.
#define _DDC_DEBUG	0

#if _DDC_DEBUG 
# define _DEBUG(x)	x
#else
# define _DEBUG(x)
#endif

// ----------------------------------------------------------------------------
// Load the config defines into the runtime state.
void	_ddcConfigSetup();


#endif
