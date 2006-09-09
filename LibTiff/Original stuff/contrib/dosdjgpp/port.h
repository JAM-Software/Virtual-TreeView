#ifndef _PORT_
#define _PORT_ 1
#ifdef __cplusplus
extern "C" {
#endif
#include <sys/types.h>
typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;
typedef unsigned long u_long;

#define HOST_FILLORDER FILLORDER_LSB2MSB
#define HOST_BIGENDIAN	0
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
typedef double dblparam_t;
#ifdef __STRICT_ANSI__
#define	INLINE	__inline__
#else
#define	INLINE	inline
#endif
#define GLOBALDATA(TYPE,NAME)	extern TYPE NAME
#ifdef __cplusplus
}
#endif
#endif
