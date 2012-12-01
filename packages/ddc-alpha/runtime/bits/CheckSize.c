

#include <stdlib.h>
#include <stdio.h>


int	main (int argc, char** argv)
{
	printf ("sizeof unsigned int = %zu\n", sizeof (unsigned int));
	printf ("sizeof long         = %zu\n", sizeof (long));
	printf ("sizeof long long    = %zu\n", sizeof (long long));
	printf ("sizeof char*        = %zu\n", sizeof (char*));
	printf ("sizeof void*        = %zu\n", sizeof (void*));
	printf ("sizeof float        = %zu\n", sizeof (float));
	printf ("sizeof double       = %zu\n", sizeof (double));
	printf ("sizeof char         = %zu\n", sizeof (char));
	printf ("sizeof wchar_t      = %zu\n", sizeof (wchar_t));
	return 0;
}
