#	$Header: /cvsroot/osrs/libtiff/tools/Makefile.in,v 1.9 2001/07/17 13:13:37 warmerda Exp $
#
# Warning, this file was automatically created by the TIFF configure script
#
# TIFF Library Tools
#
# Copyright (c) 1988-1997 Sam Leffler
# Copyright (c) 1991-1997 Silicon Graphics, Inc.
# 
# Permission to use, copy, modify, distribute, and sell this software and 
# its documentation for any purpose is hereby granted without fee, provided
# that (i) the above copyright notices and this permission notice appear in
# all copies of the software and related documentation, and (ii) the names of
# Sam Leffler and Silicon Graphics may not be used in any advertising or
# publicity relating to the software without the specific, prior written
# permission of Stanford and Silicon Graphics.
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
DEPTH	= ..

SRCDIR	= ../tools
LIBDIR	= ../libtiff

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
INSTALL	= ${SCRIPT_SH} ../port/install.sh
#
COPTS	= -fnative-struct -fno-rtti -fno-exceptions -mcpu=pentium -march=i386
OPTIMIZER= -O2
IPATH	= -I. -I${SRCDIR} -I${LIBDIR}
CFLAGS	=  ${COPTS} ${OPTIMIZER} ${IPATH}
#
TIFFLIB	= ${DEPTH}/libtiff/libtiff.dll.a
LIBJPEG	= -ljpeg
LIBGZ	= -lz
LIBTIFF	= ${DEPTH}/libtiff/libtiff.dll.a
LIBPORT	= ../port/libport.a
MACHLIBS= 
LIBS	= ${LIBTIFF} ${LIBJPEG} ${LIBGZ} ${LIBPORT} ${MACHLIBS}
#
OBJS=	\
	fax2tiff.o \
	fax2ps.o \
	gif2tiff.o \
	pal2rgb.o \
	ppm2tiff.o \
	rgb2ycbcr.o \
	ras2tiff.o \
	thumbnail.o \
	tiff2bw.o \
	tiff2rgba.o \
	tiff2ps.o \
	tiffcmp.o \
	tiffcp.o \
	tiffdither.o \
	tiffdump.o \
	tiffinfo.o \
	tiffmedian.o \
	tiffsplit.o \
	tiffset.o \
	${NULL}
TARGETS	=\
	fax2tiff \
	fax2ps \
	gif2tiff \
	pal2rgb \
	ppm2tiff \
	rgb2ycbcr \
	thumbnail \
	ras2tiff \
	tiff2bw \
	tiff2rgba \
	tiff2ps \
	tiffcmp \
	tiffcp \
	tiffdither \
	tiffdump \
	tiffinfo \
	tiffmedian \
	tiffsplit \
	tiffset \
	${NULL}

# Rule to make compiled resource (Windows)
%-rc.o: %.rc
	windres -i $< -o $@

all:	${TARGETS}
	@if [ "no" = yes ]; then \
	    ${MAKE} -${MAKEFLAGS} sgi2tiff; \
	else \
	    true; \
	fi
	@if [ "no" = yes ]; then \
	    ${MAKE} -${MAKEFLAGS} tiffgt tiffsv; \
	else \
	    true; \
	fi
install: all
	${INSTALL} -idb nostrip -idb tiff.sw.tools -m 755 		\
		-dir ${DESTDIR}c:/progra~1/libtiff/bin
	${INSTALL} -idb nostrip -idb tiff.sw.tools -m 755 		\
		-F ${DESTDIR}c:/progra~1/libtiff/bin -O ${TARGETS}
	@if [ "no" = yes ]; then 				\
	    ${INSTALL} -idb tiff.sw.tools -m 755 			\
		-F ${DESTDIR}c:/progra~1/libtiff/bin -O sgi2tiff; 			\
	else 								\
	    true;							\
	fi
	@if [ "no" = yes ]; then 					\
	    ${INSTALL} -idb tiff.sw.tools -m 755 -F ${DESTDIR}c:/progra~1/libtiff/bin \
		-O tiffgt tiffsv;					\
	else 								\
	    true; 							\
	fi
clean:
	rm -f ${TARGETS} ${OBJS} sgigt.o tiffgt sgisv.o tiffsv 		\
	    sgi2tiff.o sgi2tiff core a.out ycbcr 

#
# System-independent tools
#

tiffinfo: tiffinfo.o tiffinfo-rc.o ${TIFFLIB}
	${CC} -o tiffinfo tiffinfo-rc.o ${CFLAGS} tiffinfo.o ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
tiffinfo.o: ${SRCDIR}/tiffinfo.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tiffinfo.c

tiffcmp:tiffcmp.o tiffcmp-rc.o ${TIFFLIB}
	${CC} -o tiffcmp tiffcmp-rc.o ${CFLAGS} tiffcmp.o ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
tiffcmp.o: ${SRCDIR}/tiffcmp.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tiffcmp.c

tiffcp:	tiffcp.o tiffcp-rc.o ${TIFFLIB}
	${CC} -o tiffcp tiffcp-rc.o ${CFLAGS} tiffcp.o ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
tiffcp.o: ${SRCDIR}/tiffcp.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tiffcp.c

tiffdump: tiffdump.o tiffdump-rc.o
	${CC} -o tiffdump tiffdump-rc.o ${CFLAGS} tiffdump.o ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
tiffdump.o: ${SRCDIR}/tiffdump.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tiffdump.c

tiffmedian: tiffmedian.o tiffmedian-rc.o ${TIFFLIB}
	${CC} -o tiffmedian tiffmedian-rc.o ${CFLAGS} tiffmedian.o ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
tiffmedian.o: ${SRCDIR}/tiffmedian.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tiffmedian.c

tiffsplit: tiffsplit.o tiffsplit-rc.o ${TIFFLIB}
	${CC} -o tiffsplit tiffsplit-rc.o ${CFLAGS} tiffsplit.o ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
tiffsplit.o: ${SRCDIR}/tiffsplit.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tiffsplit.c

tiff2ps: tiff2ps.o tiff2ps-rc.o ${TIFFLIB}
	${CC} -o tiff2ps tiff2ps-rc.o ${CFLAGS} tiff2ps.o ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
tiff2ps.o: ${SRCDIR}/tiff2ps.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tiff2ps.c

tiffset: tiffset.o tiffset-rc.o ${TIFFLIB}
	${CC} -o tiffset tiffset-rc.o ${CFLAGS} tiffset.o ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
tiffset.o: ${SRCDIR}/tiffset.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tiffset.c

#
# Junky stuff... programs that are more examples of how
# to use the library than full-blown useful tools.
#

# convert RGB image to B&W
tiff2bw: tiff2bw.o tiff2bw-rc.o ${TIFFLIB}
	${CC} -o tiff2bw tiff2bw-rc.o ${CFLAGS} tiff2bw.o ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
tiff2bw.o: ${SRCDIR}/tiff2bw.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tiff2bw.c

# convert anything to RGBA.
tiff2rgba: tiff2rgba.o tiff2rgba-rc.o ${TIFFLIB}
	${CC} -o tiff2rgba tiff2rgba-rc.o ${CFLAGS} tiff2rgba.o ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
tiff2rgba.o: ${SRCDIR}/tiff2rgba.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tiff2rgba.c

# convert B&W image to bilevel w/ FS dithering
tiffdither: tiffdither.o tiffdither-rc.o ${TIFFLIB}
	${CC} -o tiffdither tiffdither-rc.o ${CFLAGS} tiffdither.o ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
tiffdither.o: ${SRCDIR}/tiffdither.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tiffdither.c

# simple Sun rasterfile converter
ras2tiff: ras2tiff.o ras2tiff-rc.o ${TIFFLIB}
	${CC} -o ras2tiff ras2tiff-rc.o ${CFLAGS} ras2tiff.o ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
ras2tiff.o: ${SRCDIR}/ras2tiff.c
	${CC} -c ${CFLAGS} ${SRCDIR}/ras2tiff.c

# simple GIF converter
gif2tiff: gif2tiff.o gif2tiff-rc.o ${TIFFLIB}
	${CC} -o gif2tiff gif2tiff-rc.o ${CFLAGS} gif2tiff.o ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
gif2tiff.o: ${SRCDIR}/gif2tiff.c
	${CC} -c ${CFLAGS} ${SRCDIR}/gif2tiff.c

# very limited PBM converter
ppm2tiff: ppm2tiff.o ppm2tiff-rc.o ${TIFFLIB}
	${CC} -o ppm2tiff ppm2tiff-rc.o ${CFLAGS} ppm2tiff.o ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
ppm2tiff.o: ${SRCDIR}/ppm2tiff.c
	${CC} -c ${CFLAGS} ${SRCDIR}/ppm2tiff.c

# Group 3/4 FAX file converter
fax2tiff: fax2tiff.o fax2tiff-rc.o ${TIFFLIB}
	${CC} -o fax2tiff fax2tiff-rc.o ${CFLAGS} fax2tiff.o ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
fax2tiff.o: ${SRCDIR}/fax2tiff.c
	${CC} -c -I${LIBDIR} -I${DEPTH}/libtiff ${CFLAGS} ${SRCDIR}/fax2tiff.c

# Group 3/4 FAX to encoded PS converter
fax2ps: fax2ps.o fax2ps-rc.o ${TIFFLIB}
	${CC} -o fax2ps fax2ps-rc.o ${CFLAGS} fax2ps.o ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
fax2ps.o: ${SRCDIR}/fax2ps.c
	${CC} -c ${CFLAGS} ${SRCDIR}/fax2ps.c

# convert Palette image to RGB
pal2rgb: pal2rgb.o pal2rgb-rc.o ${TIFFLIB}
	${CC} -o pal2rgb pal2rgb-rc.o ${CFLAGS} pal2rgb.o ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
pal2rgb.o: ${SRCDIR}/pal2rgb.c
	${CC} -c ${CFLAGS} ${SRCDIR}/pal2rgb.c

# convert RGB image to YCbCr
rgb2ycbcr: rgb2ycbcr.o rgb2ycbcr-rc.o ${TIFFLIB}
	${CC} -o rgb2ycbcr rgb2ycbcr-rc.o ${CFLAGS} rgb2ycbcr.o ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
rgb2ycbcr.o: ${SRCDIR}/rgb2ycbcr.c
	${CC} -c ${CFLAGS} ${SRCDIR}/rgb2ycbcr.c

# generate thumbnail images from fax (example of SubIFD usage)
thumbnail: thumbnail.o thumbnail-rc.o ${TIFFLIB}
	${CC} -o thumbnail thumbnail-rc.o ${CFLAGS} thumbnail.o ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
thumbnail.o: ${SRCDIR}/thumbnail.c
	${CC} -c ${CFLAGS} ${SRCDIR}/thumbnail.c

#
# System-specific tools.
#

#
# sgi2tiff converts SGI RGB images to TIFF; it requires
# the SGI image library -limage.
#
sgi2tiff: sgi2tiff.o sgi2tiff-rc.o ${TIFFLIB}
	${CC} -o sgi2tiff sgi2tiff-rc.o ${CFLAGS} sgi2tiff.o -limage ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
sgi2tiff.o: ${SRCDIR}/sgi2tiff.c
	${CC} -c ${CFLAGS} ${SRCDIR}/sgi2tiff.c

# SGI versions of tiffgt & tiffsv that require -lgl
tiffgt:	sgigt.o sgigt-rc.o ${TIFFLIB}
	${CC} -o tiffgt tiffgt-rc.o ${CFLAGS} sgigt.o -lgutil -lgl ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
sgigt.o: ${SRCDIR}/sgigt.c
	${CC} -c ${CFLAGS} ${SRCDIR}/sgigt.c

tiffsv:	sgisv.o sgisv-rc.o ${TIFFLIB}
	${CC} -o tiffsv tiffsv-rc.o ${CFLAGS} sgisv.o -lgutil -lgl ${LIBS} -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
sgisv.o: ${SRCDIR}/sgisv.c
	${CC} -c ${CFLAGS} ${SRCDIR}/sgisv.c
