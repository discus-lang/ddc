
#include "Runtime.h"

// Load the static config hash defines into the RTS's config data structure
void	_ddcConfigSetup ()
{
	_ddcProfile ->built	= 0;

#if 	_DDC_PROFILE_GC
	_ddcProfile ->built |= _ProfileEnableGC;
#endif

#if 	_DDC_PROFILE_SLOT
	_ddcProfile ->built |= _ProfileEnableSlot;
#endif

#if	_DDC_PROFILE_ALLOC
	_ddcProfile ->built |= _ProfileEnableAlloc;
#endif

#if	_DDC_PROFILE_CONTEXT
	_ddcProfile ->built |= _ProfileEnableContext;
#endif

#if	_DDC_PROFILE_BOXING
	_ddcProfile ->built |= _ProfileEnableBoxing;
#endif

#if	_DDC_PROFILE_APPLY
	_ddcProfile ->built |= _ProfileEnableApply;
#endif


}
