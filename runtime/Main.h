
#ifndef _DDC_Main
#define	_DDC_Main

#include "Runtime.h"

// Initialise the DDC runtime system.
void	_ddcRuntimeInit 	
		( int argc
		, char** argv
		, Word64	startHeapSize
		, Word64	startSlotStackSize
		, Word64	startContextStackSize);

// Shutdown the runtime system, and emit profiling information if need be.
void	_ddcRuntimeCleanup ();

// Parse RTS options
void	_ddcParseArgs
		( int 		argc
		, char**	argv
		, bool*		outVerbose
		, Word64*	outHeapSize
		, Word64*	outSlotStackSize
		, Word64*	outContextStackSize);

// Check that the profiling options requested have actually been built into this system.		
void	_ddcCheckProfileBuilt
		( char*			option
		, _ProfileEnable	flag);

// Dump the RTS help page to the console.
void	_ddcRTSHelp ();

#endif

