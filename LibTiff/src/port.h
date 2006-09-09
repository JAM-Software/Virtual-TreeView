/*
 * Warning, this file was automatically created by the TIFF configure script
 * VERSION:	 v3.5.7
 * RELEASE:   
 * DATE:	 Wed Nov 28 23:15:22 2001
 * TARGET:	
 * CCOMPILER:	 /cygdrive/i/MINGW/BIN/gcc-2.95.3-6 (mingw special)
 */
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
#define HOST_FILLORDER FILLORDER_MSB2LSB
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
extern void* malloc(size_t);
extern void* realloc(void*, size_t);
extern void free(void*);
#ifdef __cplusplus
}
#endif
#endif
