// These are all the macros that the DDC generated code uses

#ifndef _DDC_Macro
#define _DDC_Macro

// -- RTS Errors --------------------------------------------------------------
#define _PANIC(format, ...) \
	{ \
		fprintf (stderr, "DDC RTS PANIC: %s:%d > ", __FILE__, __LINE__); \
		fprintf (stderr, format, ##__VA_ARGS__); \
		fprintf (stderr, "\n\n"); \
		_dumpPanic(); \
		abort(); \
	}

#define _ERROR(format, ...) \
	{ \
		fprintf (stderr, "DDC RTS ERROR: %s:%d > ", __FILE__, __LINE__); \
		fprintf (stderr, format, ##__VA_ARGS__); \
	}


// -- Inspecting Objects -------------------------------------------------------
// Extract an constructor argument froma data object.
#define _DARG(data,i)	(((Data*)data) ->a[i])

// Extract a function argument from a thunk.
#define _TARG(thunk,i)	(((Thunk*)thunk) ->a[i])

// Extract a function argument from a suspension.
#define _SARG(susp,i)	(((SuspIndir*)susp) ->a[i])

// Force this object then extract a named field.
#define _FIELD(exp,type,label)	\
	(((struct type*)_force(exp))->label)

// Force this object then take a reference to a named field.
#define _FIELDR(exp,type,label)	\
	(_boxRef ( _force(exp) \
		 , &(((struct type*)_force(exp)) ->label) ) )


// -- GC Slot stack -----------------------------------------------------------
// Get a pointer from the GC stack, indexed relative to the current frame.
#define _S(index)	_localSlotBase [index]

// Take the name of a CAF pointer.
//   This points to the slot in the slot stack that holds the pointer
//   to the real object.
#define	_CAFPTR(name)	_ddcCAF_##name

// Take the pointer of the CAF object with this name.
//   As the pointer to the actual object is held on the slot stack,
//   we have to dereference this one to get at it.
#define _CAF(name)	*_ddcCAF_##name


// Push some slots on the GC stack.
//	We also have to set them to zero incase the GC is activated
//	before they are initialised.
#if _DDC_PROFILE_SLOT
#  define _ENTER(countS) \
		Obj** _localSlotBase = _ddcSlotPtr;\
		_ddcSlotPtr	+= countS; \
		if (_ddcSlotPtr >= _ddcSlotMax)\
			_panicOutOfSlots();\
		for (uint32_t _i = 0; _i < countS; _i++)\
			_localSlotBase [_i]	= 0; \
		if (_ddcSlotPtr > _ddcProfile ->slot.highWater) \
			_ddcProfile ->slot.highWater = _ddcSlotPtr;
#else
#  define _ENTER(countS) \
		Obj** _localSlotBase = _ddcSlotPtr;\
		_ddcSlotPtr	+= countS; \
		if (_ddcSlotPtr >= _ddcSlotMax)\
			_panicOutOfSlots();\
		for (uint32_t _i = 0; _i < countS; _i++)\
			_localSlotBase [_i]	= 0;
#endif


// Pop some slots from the GC stack.
#define _LEAVE(countS) \
		_ddcSlotPtr	= _localSlotBase;


// -- Laziness ----------------------------------------------------------------
// Force this object.
//	The result is guarantee not to be a suspension.
#define _FORCE(v)	(_force(v))

// Follow an indirection.
#define _FOLLOW(v)	(((SuspIndir*)v) ->obj)


// -- Pattern Matching --------------------------------------------------------
// These case alternatives are added to all statements that switch
//	on the tag of a data object. If the data object is a suspension
//	then it is forced or followed, then control continues from the
//	provided label.
#define _CASESUSP(var, label) \
	case _tagSusp: \
	  var = _FORCE(var); \
	  goto label;

#define _CASEINDIR(var, label) \
	case _tagIndir: \
	  var = _FOLLOW(var); \
	  goto label;

// Handle a non-exhaustive case match.
#define _CASEDEATH(file,line,col) \
	default: _deathCase (file, line, col);

// Emit a non-exaustive case match error.
#define _CASEFAIL(file,line,col) \
	_deathCase (file, line, col);


// Primitives for pointers casting and coersion -------------------------------
#define	_PLUSPTR(ptr,offset)	(ptr + offset)

#define _PEEK(ptr)			*(ptr)
#define _PEEKON(ignored,ptr)		*(ptr)

#define _POKE(ptr,x)			(*ptr = x)
#define _POKEON(ignored,ptr,x)		(*ptr = x)

#define _CAST(t1,t2,x)			(t2)(x)

#define _COERCE_PTR(t1,t2,x)		(t2*)(x)
#define _COERCE_PTR_TO_ADDR(t1,x)	(Addr)(x)
#define _COERCE_ADDR_TO_PTR(t1,x)	(t1*)(x)

#endif
