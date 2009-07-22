
#include "Runtime.h"
#include "Profile.h"
#include "Apply.h"
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <inttypes.h>

_Profile*
	_ddcProfileNew ()
{
	_Profile *p	= malloc (sizeof (_Profile));
	memset (p, 0, sizeof (_Profile));
	return p;
}

float 	perc (Word64 x, Word64 a)
{
	return 100.0 * ((float)x / (float)a);
}

float	clockToFloat (clock_t c)
{
	return	(float)c / (float)sysconf (_SC_CLK_TCK);

}

float	ratioF (float x, float y)
{
	return	x / (x + y);
}


// _ddcPrettySize
// 	Converts a Word64 to a pretty string, with groups of 3 digits
//	separated by a comma.
//
void	_ddcPrettySize	(char* str, Word64 x)
{
	int	bufLen	= 64;
	char	buf [bufLen];
	char	out [bufLen];

	snprintf (buf, bufLen, "%" PRId64, x);

	int	len	= strlen (buf);
	
	int	o	= 0;
	for (int i = 0; i <= len; i++) {
		out[bufLen - 1 - o++] = buf[len - i];

		if (i 	  != 0				// Don't put a comma in pos zero, eg	1,234,567,
		 && i     != len			// Don't put a comma at head,     eg   ,1,234,567
		 && i % 3 == 0)				// Put a comma every third digit
		 	out[bufLen - 1 - o++] = ',';

	}

	strcpy (str, &(out[bufLen - o]));
}


// ----- GC
void	_ddcProfilePrintGC
		( FILE* 	file
		, _ProfileGC*	gc)
{
	char	buf[256];

	fprintf (file, "----------------------------------------------------------------------------\n");
	fprintf (file, "-- Garbage Collection\n");
	fprintf (file, "----------------------------------------------------------------------------\n");
	fprintf (file, "\n");

	_ddcPrettySize (buf, gc ->count);
	fprintf (file, "  collection    count = %23s\n", 		buf);
	fprintf (file, "\n");

	_ddcPrettySize (buf, gc ->allocBytes);
	fprintf (file, "  alloc         total = %23s     (bytes)\n", 	buf);
	fprintf (file, "\n");
	
	_ddcPrettySize (buf, gc ->copyBytes);
	fprintf (file, "  copy          total = %23s     (bytes)\n",	buf);

	_ddcPrettySize (buf, gc ->copyCount);
	fprintf (file, "                count = %23s     (objects)\n",	buf);
	fprintf (file, "      avg object size =     %23.3f (bytes)\n", 
		(float) gc ->copyBytes / (float) gc -> copyCount);

	fprintf (file, "\n");

	clock_t totalUser	= gc ->timeMutatorUser 		+ gc ->timeCollectorUser;
	clock_t totalSystem	= gc ->timeMutatorSystem	+ gc ->timeCollectorSystem;

	clock_t totalMutator	= gc ->timeMutatorUser		+ gc ->timeMutatorSystem;
	clock_t totalCollector	= gc ->timeCollectorUser	+ gc ->timeCollectorSystem;
	
	fprintf (file, "  process time   user =     %23.3f (s)\n",	clockToFloat (totalUser));
	fprintf (file, "               system =     %23.3f (s)\n",	clockToFloat (totalSystem));
	fprintf (file, "\n");

	fprintf (file, "  mutator       total =     %23.3f (s)\n",	clockToFloat (totalMutator));
	fprintf (file, "                 user =     %23.3f (s)\n",	clockToFloat (gc ->timeMutatorUser));
	fprintf (file, "               system =     %23.3f (s)\n",	clockToFloat (gc ->timeMutatorSystem));
	fprintf (file, "\n");

	fprintf (file, "  collector     total =     %23.3f (s)\n",	clockToFloat (totalCollector));
	fprintf (file, "                 user =     %23.3f (s)\n",	clockToFloat (gc ->timeCollectorUser));
	fprintf (file, "               system =     %23.3f (s)\n",	clockToFloat (gc ->timeCollectorSystem));
	fprintf (file, "\n");

	fprintf (file, "     time efficiency  =     %23.3f (%%)\n",	
		ratioF (totalMutator, totalCollector) * 100);

	fprintf (file, "\n\n");
}


// ----- Slot Stack
void	_ddcProfilePrintSlot
		( FILE* 	file
		, _ProfileSlot*	slot)
{
	char buf[256];

	fprintf (file, "----------------------------------------------------------------------------\n");
	fprintf (file, "-- Slot Stack\n");
	fprintf (file, "----------------------------------------------------------------------------\n");
	fprintf (file, "\n");

	// slots
	Word64	count	
		= slot ->base == 0 ? 0 : 
			(Word64) ((void*)slot ->highWater - (void*)slot ->base) / sizeof (Obj*);

	_ddcPrettySize (buf, count);
	fprintf (file, "  slot high     count = %23s\n", buf);

	Word64 highWater
		= slot ->base == 0 ? 0 : 
			(Word64) ((void*)slot ->highWater - (void*)slot ->base);

	_ddcPrettySize (buf, highWater);
	fprintf (file, "                bytes = %23s\n", buf);

	fprintf (file, "\n\n");
}


// ----- Context
void	_ddcProfilePrintContext
		( FILE*			file
		, _ProfileContext*	context)
{
	fprintf (file, "----------------------------------------------------------------------------\n");
	fprintf (file, "-- Context Stack\n");
	fprintf (file, "----------------------------------------------------------------------------\n");
	fprintf (file, "\n");
	fprintf (file, "  setup         count = %23" PRId64 "\n", context ->setupCount);
	fprintf (file, "  restore       count = %23" PRId64 "\n", context ->restoreCount);
	fprintf (file, "  high          water = %23" PRId64 "\n", context ->highWater);
	fprintf (file, "\n");
	fprintf (file, "  trys       entered  = %23" PRId64 "\n", context ->trysEntered);
	fprintf (file, "             continue = %23" PRId64 "\n", context ->trysContinue);
	fprintf (file, "             caught   = %23" PRId64 "\n", context ->trysCaught);
	fprintf (file, "             through  = %23" PRId64 "\n", context ->trysThrough);
	fprintf (file, "\n");
	fprintf (file, "  throw         count = %23" PRId64 "n", context ->throwCount);
	fprintf (file, "\n\n");
}


// ----- Alloc
void	_ddcProfilePrintAllocRatio 
		( FILE* file
		, char* name
		, _ProfileAllocRec rec
		, _ProfileAllocRec total)
{
	char buf[256];

	_ddcPrettySize (buf, rec.bytes);
	fprintf (file, "  %-12s  bytes = %23s  (%6.3f%%)\n"
		, name
		, buf
		, perc (rec.bytes, total.bytes));

	_ddcPrettySize (buf, rec.count);
	fprintf (file, "                count = %23s  (%6.3f%%)\n"
		, buf
		, perc (rec.count, total.count));


	fprintf (file, "\n");
}


void	_ddcProfilePrintAlloc
		( FILE* 		file
		, _ProfileAlloc*	alloc)
{
	char buf[256];

	fprintf (file, "----------------------------------------------------------------------------\n");
	fprintf (file, "-- Alloc\n");
	fprintf (file, "----------------------------------------------------------------------------\n");
	fprintf (file, "\n");

	_ddcPrettySize (buf, alloc ->total.bytes);
	fprintf (file, "  total         bytes = %23s\n", buf);

	_ddcPrettySize (buf, alloc ->total.count);
	fprintf (file, "                count = %23s\n", buf);
	fprintf (file, "\n");
	
	_ddcProfilePrintAllocRatio (file, "data ptrs",		alloc ->data,		alloc ->total);
	_ddcProfilePrintAllocRatio (file, "data raw",		alloc ->dataRaw,	alloc ->total);
	_ddcProfilePrintAllocRatio (file, "data raw sml",	alloc ->dataRawSmall, 	alloc ->total);
	_ddcProfilePrintAllocRatio (file, "data mixed", 	alloc ->dataMixed, 	alloc ->total);
	_ddcProfilePrintAllocRatio (file, "thunk", 		alloc ->thunk, 		alloc ->total);
	_ddcProfilePrintAllocRatio (file, "thunk copy",		alloc ->thunkCopy, 	alloc ->total);
	_ddcProfilePrintAllocRatio (file, "suspension", 	alloc ->susp,	 	alloc ->total);
	fprintf (file, "\n");
}


// ----- Apply
void	_ddcProfilePrintApply 
		( FILE* 		file
		, _ProfileApply*	apply)
{
	char buf[256];

	fprintf (file, "----------------------------------------------------------------------------\n");
	fprintf (file, "-- Apply\n");
	fprintf (file, "----------------------------------------------------------------------------\n");
	fprintf (file, "\n");	

	_ddcPrettySize (buf, apply ->apply[1]);
	fprintf (file, "  apply          imm1 = %23s\n", buf);

	_ddcPrettySize (buf, apply ->apply[2]);
	fprintf (file, "                 imm2 = %23s\n", buf);

	_ddcPrettySize (buf, apply ->apply[3]);
	fprintf (file, "                 imm3 = %23s\n", buf);

	_ddcPrettySize (buf, apply ->apply[4]);
	fprintf (file, "                 imm4 = %23s\n", buf);
	fprintf (file, "\n");
	
	_ddcPrettySize (buf, apply ->forceCount);
	fprintf (file, "  force         count = %23s\n", buf);

	_ddcPrettySize (buf, apply ->force[1]);
	fprintf (file, "       step susp imm1 = %23s\n", buf);

	_ddcPrettySize (buf, apply ->force[2]);
	fprintf (file, "       step susp imm2 = %23s\n", buf);
	fprintf (file, "\n");

	// ----- eval table
	int a, i;
	fprintf (file, "         ");
	for (i = 1; i <= _evalMaxImm; i++)
		fprintf (file, "----------- imm%-2d", i);
		
	fprintf (file, "\n");
	for (a = 1; a <= _evalMaxAirity; a++) {
		fprintf (file, "  eval%-2d: ", a);
			
		for (i = 1; i <= _evalMaxImm;    i++) {
			_ddcPrettySize (buf, apply ->eval[a][i]);
			fprintf (file, "%16s ", buf);

		}

		fprintf (file, "\n");
	}
	fprintf (file, "\n\n");
}


// ----- Boxing
void	_ddcProfilePrintBoxingRec 
		( FILE* file
		, char* name
		, _ProfileBoxingRec rec)
{
	char buf[256];

	_ddcPrettySize (buf, rec.bytes);
	fprintf (file, "  %-10s    bytes = %23s\n", name, buf);

	_ddcPrettySize (buf, rec.count);
	fprintf (file, "                count = %23s\n", buf);

	_ddcPrettySize (buf, rec.gets);
	fprintf (file, "           get  count = %23s\n", buf);
	fprintf (file, "\n");
}


void	_ddcProfilePrintBoxing
		( FILE*			file
		, _ProfileBoxing*	boxing)
{
	fprintf (file, "----------------------------------------------------------------------------\n");
	fprintf (file, "-- Boxing\n");
	fprintf (file, "----------------------------------------------------------------------------\n");
	fprintf (file, "\n");	

	// ----- Boxing
	fprintf 			(file, "* Boxing\n");
	_ddcProfilePrintBoxingRec	(file, "Char8",		boxing ->bChar8);
	_ddcProfilePrintBoxingRec	(file, "String", 	boxing ->bString);
	_ddcProfilePrintBoxingRec	(file, "Word8", 	boxing ->bWord8);
	_ddcProfilePrintBoxingRec	(file, "Int32",	 	boxing ->bInt32);
	_ddcProfilePrintBoxingRec	(file, "Float32", 	boxing ->bFloat32);
	fprintf (file, "\n\n");
}


// ------ Profile
void	_ddcProfilePrint
		( FILE* file
		, _Profile* p)
{
	if (p ->enable & _ProfileEnableGC)	_ddcProfilePrintGC	(file, &(p ->gc));
	if (p ->enable & _ProfileEnableSlot)	_ddcProfilePrintSlot	(file, &(p ->slot));
	if (p ->enable & _ProfileEnableContext)	_ddcProfilePrintContext	(file, &(p ->context));
	if (p ->enable & _ProfileEnableAlloc)	_ddcProfilePrintAlloc	(file, &(p ->alloc));
	if (p ->enable & _ProfileEnableBoxing)	_ddcProfilePrintBoxing	(file, &(p -> boxing));
	if (p ->enable & _ProfileEnableApply)	_ddcProfilePrintApply	(file, &(p ->apply));
}



// ----- Timing
void	_ddcProfileMutatorStart ()
{
	struct tms buf;
	times (&buf);
	
	_ddcProfile ->gc.timeMarkUser	= buf .tms_utime;
	_ddcProfile ->gc.timeMarkSystem	= buf .tms_stime;
}

void	_ddcProfileMutatorEnd ()
{
	struct tms buf;
	times (&buf);
	
	_ddcProfile ->gc.timeMutatorUser	+= buf .tms_utime - _ddcProfile ->gc.timeMarkUser;
	_ddcProfile ->gc.timeMutatorSystem	+= buf .tms_stime - _ddcProfile ->gc.timeMarkSystem;
}


void	_ddcProfileCollectorStart ()
{
	struct tms buf;
	times (&buf);
	
	_ddcProfile ->gc.timeMarkUser	= buf .tms_utime;
	_ddcProfile ->gc.timeMarkSystem	= buf .tms_stime;
}

void	_ddcProfileCollectorEnd ()
{
	struct tms buf;
	times (&buf);

	clock_t	pauseUser	= buf .tms_utime - _ddcProfile ->gc.timeMarkUser;
	clock_t	pauseSystem	= buf .tms_stime - _ddcProfile ->gc.timeMarkSystem;
	
	_ddcProfile ->gc.timeCollectorUser	+= pauseUser;
	_ddcProfile ->gc.timeCollectorSystem	+= pauseSystem;
}





