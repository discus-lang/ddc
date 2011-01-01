
#include "../Runtime.h"
#include "../Runtime.ci"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <inttypes.h>

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


// ------ Ref
Obj*	primRefUpdate	(Obj* ref_, Obj* x_)
{
	_DEBUG (assert (_getObjTag(ref_) == _tagBase));

	_ENTER(2);
	_S(0)	= ref_;
	_S(1)	= x_;

	// unboxing.
	DataM* refDataM;
	Data* refData;
	int objType = _objType(_S(0));
	switch (objType) {
	 case _ObjTypeDataM:
		refDataM = (DataM*) _force(_S(0));
		Obj***  payload	= (Obj***)refDataM ->payload;
		*payload[1]	= _S(1);
		break;
	 case _ObjTypeData:
		refData = (Data*) _force(_S(0));
		refData->a[0] = _S(1);
		break;
	 default:
		_PANIC("Updating Ref with unknown internal object type");
		break;
	}

	_LEAVE(2);
	return	_primUnit;
}


// ------ Int64 
void	primInt64_toString	(Int64 value, char * str, int slen)
{
	// Need to do it this way to correctly print an Int64, because
	// we need "%lld" on 32 bit systems and "%ld" on 64 bit systems.
	snprintf (str, slen, "%" PRId64, value);
}


// ------ Word64
void	primWord64_toString	(Word64 value, char * str, int slen)
{
	// Need to do it this way to correctly print an Int64, because
	// we need "%llu" on 32 bit systems and "%lu" on 64 bit systems.
	snprintf (str, slen, "%" PRIu64, value);
}


// ------ Float32
// TODO: We could implmement both of these directly in Disciple using casts.
Obj*	primFloat32_toFloat (Obj* x1)
{
	Int32 x		= _unbox(Int32, x1);
	return	_boxFloat32 (x);
}

Obj*	primFloat32_truncate (Obj* x_)
{
	float x		= _unbox(Float32, x_);
	return	_boxInt32 (x);
}

// ------ Float64
// TODO: We could implmement both of these directly in Disciple using casts.
Obj*	primFloat64_toFloat (Obj* x1)
{
	Float64 x	= _unbox(Int64, x1);
	return	_boxFloat64 (x);
}

Obj*	primFloat64_truncate (Obj* x_)
{
	double x	= _unbox(Float64, x_);
	return	_boxInt64 (x);
}


// ------ Network
#include <sys/types.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <unistd.h>
#include <string.h>
#include <sys/socket.h>

Int32	primConnect	(String* hostName, Int32 port)
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

Int32	primArgCount	(Obj* UNUSED (unit))
{
	return _ddcArgCount;
}

String*	primArgValue 	(Int32 ix)
{
	assert (ix >= 0 && ix <= _ddcArgCount);
	return	_ddcArgValue[ix];
}
