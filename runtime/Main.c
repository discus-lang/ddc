
#include "Runtime.h"
#include "Runtime.ci"

#include <stdlib.h>
#include <string.h>
#include <inttypes.h>


// Initialise the DDC runtime system.
void	_ddcRuntimeInit 
		( int argc
		, char** argv)
{
	// Stash our args
	_ddcArgCount	= argc - 1;
	_ddcArgValue	= argv;

	// Allocate profile data.
	_ddcProfile	= _ddcProfileNew ();
	_ddcConfigSetup ();

	// Parse RTS args.
	bool	verbose			= false;
	Word64	initContextStackSize	= 0;
	Word64	initSlotStackSize	= 0;
	Word64	initHeapSize		= 0;
	
	_ddcParseArgs 
		( argc
		, argv
		, &verbose
		, &initContextStackSize
		, &initSlotStackSize
		, &initHeapSize);

	// Default the size of stacks and heap if they haven't been specified.
	if (initContextStackSize == 0)	initContextStackSize	= 50;
	if (initSlotStackSize	 == 0)	initSlotStackSize	= 100000;
	if (initHeapSize	 == 0)	initHeapSize		= 10000000;

	// ----- 
	if (verbose) {
		printf ("* %s starting up...\n", _DDC_VERSION);
		printf ("  options:\n");
		printf ("    InitContextStackSize %" PRId64 "\n", initContextStackSize);
		printf ("    InitSlotStackSize    %" PRId64 "\n", initSlotStackSize);
		printf ("    InitHeapSize         %" PRId64 "\n", initHeapSize);
		printf ("\n");
		printf ("  * Creating stacks and heap.\n");
	}
	

	// Create stacks and heap.
	_contextInit 	(initContextStackSize);
	_collectInit	(initSlotStackSize);
	_allocInit	(initHeapSize);


	// Alloc atoms that are owned by the RTS.
	_primUnit	= _allocData_anchored (0, 0);
	_primTrue	= _allocData_anchored (1, 0);
	_primFalse	= _allocData_anchored (0, 0);
	

	// Dump trace to stdout.
	_ddcTraceFile	= stdout;


	// Tell the GC profiler we're starting the mutator now.
#if _DDC_PROFILE_GC
	_ddcProfileMutatorStart ();
#endif

	// -----
	if (verbose) 
		printf ("  * Entering mutator.\n");
	
}


// Shutdown the runtime system, and emit profiling information if need be.
void	_ddcRuntimeCleanup ()
{
	_ddcProfileMutatorEnd();

	// Add size of current heap to total alloc count.
	Word64	heapUsageFinal	= _ddcHeapPtr - _ddcHeapBase;
	_PROFILE_GC (allocBytes += heapUsageFinal - _ddcProfile ->gc.lastCompactionSize);

	// Write out profile information, if any profiling options are enabled.
	if (_ddcProfile ->enable != 0) 
	{	FILE*	file = fopen ("ddc-rts.prof", "w");
		_ddcProfilePrint (file, _ddcProfile);
		fclose (file);
	}
}


// Parse RTS options
void	_ddcParseArgs
		( int 		argc
		, char**	argv
		, bool*		outVerbose
		, Word64*	outContextStackSize
		, Word64*	outSlotStackSize
		, Word64*	outHeapSize)
{
	bool	enable	= false;
	for (int i = 1; i < argc; i++) {

		// Turn on/off RTS option parsing.
		if 	(strcmp (argv[i], "+RTS") == 0)	
		{	enable = true;
			continue;
		}
		else if (strcmp (argv[i], "-RTS") == 0) 
		{	enable = false;
			continue;
		}
		
		// This options isn't for us.
		if (!enable)
			continue;
			
		// ----- Accept RTS options

		if 	(   strcmp (argv[i], "-help")	== 0
			 || strcmp (argv[i], "--help")	== 0
			 || strcmp (argv[i], "-h")	== 0) 
		{	_ddcRTSHelp();
			exit(0);
		}
			
		// -- Be verbose.
		else if (  strcmp (argv[i], "-v")	== 0
			|| strcmp (argv[i], "-verbose")	== 0)
		{	*outVerbose	= true;
			continue;
		}
		
		// - Set init heap size
		else if (  strcmp (argv[i], "-H")	== 0
			|| strcmp (argv[i], "-heap-size-init") == 0)
		{	*outHeapSize	= atoll (argv[++i]);
			continue;
		}

		// - Set init slot stack size
		else if (  strcmp (argv[i], "-S")	== 0
			|| strcmp (argv[i], "-slot-size-init") == 0)
		{	*outSlotStackSize	= atoll(argv[++i]);
			continue;
		}

		// - Set init context stack size
		else if (  strcmp (argv[i], "-C")	== 0
			|| strcmp (argv[i], "-context-size-init") == 0)
		{	*outContextStackSize	= atoll(argv[++i]);
			continue;
		}


		// - Turn on all availiable profiling options.
		else if	(  strcmp (argv[i], "-P")	== 0
			|| strcmp (argv[i], "-profile") == 0) 
		{	_ddcProfile ->enable = _ddcProfile ->built;
			continue;
		}

		// - Turn on GC profiling.
		else if	(strcmp (argv[i], "-profile-gc") == 0) 
		{	_ddcCheckProfileBuilt 
				( "-profile-gc"
				, _ProfileEnableGC);

			_ddcProfile ->enable |= _ProfileEnableGC;
			continue;
		}

		// - Turn on Slot stack profiling.
		else if (strcmp (argv[i], "-profile-slot") == 0)
		{	_ddcCheckProfileBuilt 
				( "-profile-slot"
				, _ProfileEnableSlot);
		
			_ddcProfile ->enable |= _ProfileEnableSlot;
			continue;
		}

		// - Turn on Context profiling.
		else if	(strcmp (argv[i], "-profile-context") == 0) 
		{	_ddcCheckProfileBuilt 
				( "-profile-context"
				, _ProfileEnableContext);

			_ddcProfile ->enable |= _ProfileEnableContext;
			continue;
		}

		// - Turn on Alloc profiling.
		else if	(strcmp (argv[i], "-profile-alloc") == 0) 
		{	_ddcCheckProfileBuilt 
				( "-profile-alloc"
				, _ProfileEnableAlloc);

			_ddcProfile ->enable |= _ProfileEnableAlloc;

			continue;
		}

		// - Turn on Boxing profiling.
		else if	(strcmp (argv[i], "-profile-boxing") == 0) 
		{	_ddcCheckProfileBuilt 
				( "-profile-boxing"
				, _ProfileEnableBoxing);
				
			_ddcProfile ->enable |= _ProfileEnableBoxing;
			continue;
		}

		// - Turn on Apply profiling.
		else if	(strcmp (argv[i], "-profile-apply") == 0) 
		{	_ddcCheckProfileBuilt
				( "-profile-apply"
				, _ProfileEnableApply);

			_ddcProfile ->enable |= _ProfileEnableApply;
			continue;
		}
		
		// - ERROR! 
		else {
			fprintf (stderr, "*** DDC ERROR!\n");
			fprintf (stderr, "    Unknown RTS option on command line, '%s'.\n", argv[i]);
			fprintf (stderr, "      try \"<prog> +RTS -help\" for a list of RTS options.\n");
			abort();
		}

	}
}


// Check that the profiling options requested have actually been built into this system.
void	_ddcCheckProfileBuilt
		( char*			option
		, _ProfileEnable	flag)
{
	if ((_ddcProfile ->built & flag) == 0) {
		fprintf ( stderr
			, "*** DDC ERROR! '%s' requested, but RTS is not built to support it.\n"
			, option);
		
		abort();
	};
}


// Dump the RTS help page to the console.
void	_ddcRTSHelp ()
{
	printf ("%s.\n", _DDC_VERSION);
	printf ("  usage: <prog> +RTS [options..]\n");
	printf ("\n");
	printf ("  General Options.\n");
	printf ("    -h, -help, --help         Display this help.\n");
	printf ("    -v, -verbose              Be verbose.\n");
	printf ("\n");

	printf ("  Stack and Heap setup.\n");
	printf ("    -H, -heap-size-init       Initial heap size (in bytes).\n");
	printf ("    -S, -slot-size-init       Initial GC slot stack size (in slots).\n");
	printf ("    -C, -context-size-init    Initial context stack size (in slots).\n");
	printf ("\n");

	printf ("  Profiling.\n");
	printf ("    -P, -profile              Enable all profiling options supported by this build.\n");
	printf ("\n");
	printf ("    -profile-gc               Enable profiling of garbage collector.\n");
	printf ("    -profile-slot             Enable profiling of GC slot stack.\n");
	printf ("    -profile-context          Enable profiling of context stack and exception system.\n");
	printf ("    -profile-alloc            Enable profiling of space allocator.\n");
	printf ("    -profile-boxing           Enable profiling of boxing and unboxing.\n");
	printf ("    -profile-apply            Enable profiling of apply, eval and force.\n");
	printf ("\n");

	struct { char* name; _ProfileEnable flag; } 
	 flagNames[] = 
	 	{ { "-profile-gc",		_ProfileEnableGC }
		, { "-profile-slot",	_ProfileEnableSlot }
		, { "-profile-alloc",	_ProfileEnableAlloc }
		, { "-profile-context",	_ProfileEnableContext }
		, { "-profile-boxing",	_ProfileEnableBoxing }
		, { "-profile-apply",	_ProfileEnableApply }
		} ;
		
	printf ("    Profiling options supported by this build:\n");
	printf ("      ");
	for (int i = 0; i <= 5; i++) {
		if (_ddcProfile ->built & flagNames[i].flag)
			printf ("%s ", flagNames[i].name);
	}
	printf ("\n");				

	printf ("\n");
}


