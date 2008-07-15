
#include "Prim.h"
#include "../Runtime.h"
#include "../Apply.h"
#include "../Object.h"

#include "../Collect.h"
#include "../Collect.ci"

#include "../Alloc.h"
#include "../Alloc.ci"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

// ----- Evil globals
//
Obj*	_primUnit	= 0;
Obj*	_primTrue	= 0;
Obj*	_primFalse	= 0;


// -----
Obj*	primUpdateCtor	(Obj* ctor_, Int32 i, Obj* obj)
{
	Data* ctor	= (Data*)ctor_;
	ctor->a[i]	= obj;
	
	return _primUnit;
}



// -----
Obj*	primPrintString	(Obj* obj)
{
	_DEBUG (assert (_TAG(obj) == _tagBase));
	String cStr	= _unbox (String, obj);

	printf ("%s", cStr);
	fflush (stdout);
	return _primUnit;
}


// -----
Obj*	primError 	(Obj* obj)
{
	_DEBUG (assert (_TAG(obj) == _tagBase));
	String str	= _unbox (String, obj);
	
	fprintf (stderr, "*** Exception: %s\n", str);
	abort();
}


Obj*	primExit	(Obj* code)
{
//	fprintf (stderr, "primExit: exiting\n");
	exit (_unbox(Int32, code));
}




// -----
Obj*	primStringChar (Obj* dChar)
{
	Char32 c = _unbox(Char32, dChar);

	Char8 s[2];
	s[0]	= (Char8)c;
	s[1]	= '\0';
	
	return	_boxString (s);
}


Obj*	primStringInt (Obj* dInt)
{
	Int32 i	= _unbox(Int32, dInt);
	
	Char8 s[20];			// how much do we actually need?
	snprintf (s, 20, "%d", i);
	
	return	_boxString (s);
}	


Obj*	primStringFloat32 (Obj* x)
{
	Float32 f	= _unbox(Float32, x);
	Char8 s[32];
	snprintf (s, 32, "% f", f);
	return	_boxString (s);
}




// ------ Ref
Obj*	primRefUpdate	(Obj* ref_, Obj* x_)
{
	_DEBUG (assert (_TAG(ref_) == _tagBase));
	
	_ENTER(2);
	_S(0)	= ref_;
	_S(1)	= x_;
	
	// unboxing.
	DataM* 	ref		= (DataM*) _force(_S(0));
	Obj***	payload		= (Obj***)ref ->payload;

	// update the field.
	*payload[1]		= _S(1);
	
	_LEAVE(2);
	return	_primUnit;
}



// ------ Network
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <unistd.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>

Int32	primConnect	(String hostName, Int32 port)
{
	// resolve host name
	struct hostent* hostInfo	
			= gethostbyname ((const char *)hostName);
	assert (hostInfo != 0);

	// create socket
	int sock	= socket (AF_INET, SOCK_STREAM, 0);
	assert (sock > 0);
	
	// connect to server
	struct sockaddr_in hostAddr;

	hostAddr.sin_family	= hostInfo->h_addrtype;
	memcpy 	((char *) &hostAddr.sin_addr.s_addr
		, hostInfo ->h_addr_list[0]
		, hostInfo ->h_length);
	
	hostAddr.sin_port	= htons (port);

	int flag	= 1;
	setsockopt (sock, SOL_SOCKET, TCP_NODELAY, &flag, sizeof(int));
	
	assert (connect ( sock
			, (struct sockaddr *) &hostAddr
			, sizeof (hostAddr) )
			>= 0);
			
	return sock;
}

Int32	primArgCount	(Obj* unit)
{
	return _ddcArgCount;
}

String	primArgValue 	(Int32 ix)
{
	assert (ix >= 0 && ix <= _ddcArgCount);
	return	_ddcArgValue[ix];
}
