#! smake
#	$Header: /cvsroot/osrs/libtiff/port/Makefile.in,v 1.3 2001/04/06 03:02:33 warmerda Exp $
#
# Warning, this file was automatically created by the TIFF configure script
#
# Tag Image File Format Library
#
# Copyright (c) 1995-1997 Sam Leffler
# Copyright (c) 1995-1997 Silicon Graphics, Inc.
# 
# Permission to use, copy, modify, distribute, and sell this software and 
# its documentation for any purpose is hereby granted without fee, provided
# that (i) the above copyright notices and this permission notice appear in
# all copies of the software and related documentation, and (ii) the names of
# Sam Leffler and Silicon Graphics may not be used in any advertising or
# publicity relating to the software without the specific, prior written
# permission of Sam Leffler and Silicon Graphics.
# 
# THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
# EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
# 
# IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
# ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
# OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
# WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
# LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
# OF THIS SOFTWARE.
#
SRCDIR	= ../port
VPATH	= ../port

#
# VERSION:	v3.5.7
# DATE:		Wed Nov 28 23:15:22  2001
# TARGET:	
# CCOMPILER:	/cygdrive/i/MINGW/BIN/gcc
#

SHELL	= /usr/bin/sh
SCRIPT_SH = /usr/bin/sh
NULL	=
CC	= /cygdrive/i/MINGW/BIN/gcc
AR	= /usr/bin/ar
AROPTS	= rc
RANLIB	= /cygdrive/i/MINGW/BIN/ranlib

IPATH	= -I. -I${SRCDIR}
COPTS	= -g
OPTIMIZER=-O
CFLAGS	=  ${COPTS} ${OPTIMIZER} ${IPATH}

CFILES	=  getopt.c
OBJECTS	= ${CFILES:.c=.o}
TARGETS	= libport.a

default all: ${TARGETS}

libport.a: ${OBJECTS}
	@rm -f $@;
	${AR} ${AROPTS} $@ ${OBJECTS}
	${RANLIB} $@
${PORT}/libport.a: libport.a

install: default

clean:
	rm -f ${TARGETS} ${OBJS} core a.out
