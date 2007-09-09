

#include <stdlib.h>
#include <stdio.h>


int	main (int argc, char** argv)
{
	printf ("sizeof unsigned int = %d\n", sizeof (unsigned int));
	printf ("sizeof long         = %d\n", sizeof (long));
	printf ("sizeof long long    = %d\n", sizeof (long long));
	printf ("sizeof char*        = %d\n", sizeof (char*));
	printf ("sizeof void*        = %d\n", sizeof (void*));
	printf ("sizeof float        = %d\n", sizeof (float));
	printf ("sizeof double       = %d\n", sizeof (double));
	printf ("sizeof char         = %d\n", sizeof (char));
	printf ("sizeof wchar_t      = %d\n", sizeof (wchar_t));
}
