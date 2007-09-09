
#ifndef _DDC_Main
#define	_DDC_Main

void	_ddcRuntimeInit 	
		( int argc
		, char** argv);

void	_ddcRuntimeCleanup	();


void	_ddcParseArgs
		( int 		argc
		, char**	argv
		, bool*		outVerbose
		, Word64*	outContextStackSize
		, Word64*	outSlotStackSize
		, Word64*	outHeapSize);
		
void	_ddcCheckProfileBuilt
		( char*			option
		, _ProfileEnable	flag);

void	_ddcRTSHelp ();

#endif

