// Macros used to squash warnings from the C compiler.

#ifndef _DDC_Util
#define _DDC_Util


// Sometimes we don't want to refer to all the parameters in a function
//	Use this macro to mark said parameters, and squash warnings
//	from the compilers about unused parameters.
//
#ifdef UNUSED
#elif defined (__GNUC__)
#	define UNUSED(x) UNUSED_ ## x __attribute__ ((unused))
#elif defined (__LCLINT__)
#	define UNUSED(x) /*@unused@*/ x
#else
#	define UNUSED(x) x
#endif

#ifdef __GNUC__
#	define WARN_UNUSED	__attribute__ ((warn_unused_result))
#else
#	define WARN_UNUSED
#endif

#endif
