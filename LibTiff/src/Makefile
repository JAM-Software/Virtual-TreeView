#	$Header: /cvsroot/osrs/libtiff/libtiff/Makefile.in,v 1.16 2001/09/09 15:56:04 warmerda Exp $
#
# Tag Image File Format Library
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

SRCDIR	= ../libtiff

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
INSTALL	= ${SCRIPT_SH} ../port/install.sh

#
# If JPEG support is to be included and the Independent JPEG
# Software distribution is not installed then DIR_JPEG must
# refer to the directory where the include files reside.
#
# Similarly, if the libgz distribution is not installed, then
# DIR_LIBGZ must refer to the directory where the include files
# are located.  Note that recent versions 
#
IPATH	= -I. -I${SRCDIR} 
#
# To enable JPEG support include -DJPEG_SUPPORT here.
# To enable Deflate support add a -DZIP_SUPPORT here.
# Note that where the configure script is used these defines
# are automatically setup when JPEG/ZIP is set to "yes".
#
# Otherwise, consult tiffconf.h for information on controlling
# the configuration of optional library support.
#
CONF_LIBRARY=-DJPEG_SUPPORT -DZIP_SUPPORT
COPTS	= -fnative-struct -fno-rtti -fno-exceptions -mcpu=pentium -march=i386
OPTIMIZER= -O2
CFLAGS	=   ${COPTS} ${OPTIMIZER} ${IPATH} ${CONF_LIBRARY}
#
SRCS	= \
	tif_aux.c \
	tif_close.c \
	tif_codec.c \
	tif_compress.c \
	tif_dir.c \
	tif_dirinfo.c \
	tif_dirread.c \
	tif_dirwrite.c \
	tif_dumpmode.c \
	tif_error.c \
	tif_fax3.c \
	tif_fax3sm.c \
	tif_getimage.c \
	tif_jpeg.c \
	tif_flush.c \
	tif_luv.c \
	tif_lzw.c \
	tif_next.c \
	tif_ojpeg.c \
	tif_open.c \
	tif_packbits.c \
	tif_pixarlog.c \
	tif_predict.c \
	tif_print.c \
	tif_read.c \
	tif_swab.c \
	tif_strip.c \
	tif_thunder.c \
	tif_tile.c \
	tif_unix.c \
	tif_version.c \
	tif_warning.c \
	tif_write.c \
	tif_zip.c \
	${NULL}
OBJS	= \
	tif_aux.o \
	tif_close.o \
	tif_codec.o \
	tif_compress.o \
	tif_dir.o \
	tif_dirinfo.o \
	tif_dirread.o \
	tif_dirwrite.o \
	tif_dumpmode.o \
	tif_error.o \
	tif_fax3.o \
	tif_fax3sm.o \
	tif_getimage.o \
	tif_jpeg.o \
	tif_flush.o \
	tif_luv.o \
	tif_lzw.o \
	tif_next.o \
	tif_ojpeg.o \
	tif_open.o \
	tif_packbits.o \
	tif_pixarlog.o \
	tif_predict.o \
	tif_print.o \
	tif_read.o \
	tif_swab.o \
	tif_strip.o \
	tif_thunder.o \
	tif_tile.o \
	tif_unix.o \
	tif_version.o \
	tif_warning.o \
	tif_write.o \
	tif_zip.o \
	${NULL}
TARGETS	= libtiff.a

# Rule to make compiled resource (Windows)
%-rc.o: %.rc
	windres -i $< -o $@

all:	${TARGETS}
	if [ nodso != nodso ]; then \
	    ${MAKE} -${MAKEFLAGS} nodso; \
	else \
	    true; \
	fi

libtiff.a: ${OBJS}
	${AR} ${AROPTS} libtiff.a $?
	${RANLIB} libtiff.a

#
# NB: The configure script verifies that the configured
# tools are capable of producing a DSO before enabling
# their creation/use.  The following rules are effectively
# duplicated in the configure script to do this verification.
# This means that if you want to add support for building a
# DSO on another system you'll need to modify this file *and*
# configure if you want the right thing to happen automatically
# (should probably be fixed up).
#

# default IRIX DSO building rule
IRIXdso: ${OBJS}
	@if [ "`basename ${CC}`" = "gcc" ]; then \
	   ${LD} -n32  -o libtiff.a -shared -rdata_shared \
	   ${OBJS}; \
	else \
	    ${CC}  -o libtiff.a -shared -rdata_shared \
	        -check_registry ${SRCDIR}/../port/irix/so_locations \
	        -quickstart_info \
	        ${OBJS} -ljpeg -lz; \
	fi
	touch $@
# special rule for IRIX 5.2
IRIX52dso: ${OBJS}
	${LD} -elf -o libtiff.a -shared -no_unresolved -all ${OBJS} \
	    -ljpeg -lz -lc -lm
	touch $@
# Solaris 2.x
SOLARISdso: ${OBJS}
	${LD} -Lc:/progra~1/libtiff/lib -G -h libtiff.a -o libtiff.a ${OBJS}
	touch $@
# HP-UX 10.x+
HPUXdso: ${OBJS}
	${LD} -b -o libtiff.a +h libtiff.a ${OBJS}
	touch $@
# AIX 2.3.5 and 4.1.1
AIXdso: ${OBJS}
	rm -f libtiff.syms shr.o
	echo "#!" > libtiff.syms
	/bin/dump -g libtiff.a | sed -n -e \
	      's/^[ 	]*[0-9][0-9]*[	 ]*\([^	 .][^ 	]*\)$$/\1/p' \
	      >> libtiff.syms
	${LD} -o shr.o libtiff.a -H512 -T512 -bM\:SRE \
	    -bE\:libtiff.syms -ljpeg -lz -lc -lm -Lc:/progra~1/libtiff/lib
	rm -f libtiff.syms libtiff.a
	${AR} ${AROPTS} libtiff.a shr.o
	rm -f shr.o
	touch $@
# GNU linker shared libraries
GNULDdso: ${OBJS}
	${CC} -shared -Wl,-soname,libtiff.a \
	    -o libtiff.a  ${OBJS} \
		-ljpeg -lz 
	rm -f libtiff.a
	if [ a != a ] ; then \
	  cp -fp  libtiff.a libtiff.a; \
	fi
	touch $@
DARWINdso: ${OBJS}
	${CC} -dynamiclib -undefined suppress \
	  -install_name libtiff.a \
	  -o libtiff.a  ${OBJS} \
	  -ljpeg -lz 
	rm -f libtiff.a
	if [ a != a ] ; then \
	  cp -fp  libtiff.a libtiff.a; \
	fi
	touch $@
# NetBSD 1.1 or FreeBSD (old style)
NETBSDdso FREEBSDdso oldOPENBSDdso: ${OBJS}
	@rm -f libtiff_pic.a
	@${AR} cq libtiff_pic.a `lorder ${OBJS} | tsort -q`
	${RANLIB} libtiff_pic.a
	${LD} -x -Bshareable -Bforcearchive -o libtiff.a libtiff_pic.a
	rm -f libtiff_pic.a
	touch $@
# linux ELF shared lib rule
LINUXdso:  ${OBJS}
	${CC} -shared -Wl,-soname,libtiff.a \
	    -o libtiff.a  ${OBJS} \
		-ljpeg -lz @MACHLIBDEPS@
	rm -f libtiff.a
	cp -fp  libtiff.a libtiff.a
	touch $@
# OSF/1 3.2 shared lib rule
OSFdso: ${OBJS}
	${LD} -o libtiff.a -shared -error_unresolved ${OBJS} -ljpeg -lz -lc -lm

OPENBSDdso: ${OBJS}
	${CC} -shared  -o libtiff.a ${OBJS} -ljpeg -lz -lm
	touch $@


${OBJS}: ${SRCDIR}/tiffio.h ${SRCDIR}/tiff.h ${SRCDIR}/tif_dir.h
${OBJS}: ${SRCDIR}/tiffcomp.h ${SRCDIR}/tiffiop.h ${SRCDIR}/tiffconf.h
${OBJS}: ${SRCDIR}/tiffvers.h

VERSION	= .././VERSION

${SRCDIR}/tiffvers.h: ${VERSION} ${SRCDIR}/mkversion.c
	${CC} -o mkversion mkversion-rc.o ${CFLAGS} ${SRCDIR}/mkversion.c -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
	rm -f ${SRCDIR}/tiffvers.h
	./mkversion -v ${VERSION} ${SRCDIR}/tiffvers.h

#
# The finite state machine tables used by the G3/G4 decoders
# are generated by the mkg3states program.  On systems without
# make these rules have to be manually carried out.
#
tif_fax3sm.c: ${SRCDIR}/mkg3states.c ${SRCDIR}/tif_fax3.h
	${CC} -o mkg3states mkg3states-rc.o ${CFLAGS} ${SRCDIR}/mkg3states.c -Wl,--enable-auto-import -lstdbin -liberty -lMsup -lMstubs
	rm -f tif_fax3sm.c; ./mkg3states -c const tif_fax3sm.c

tif_aux.o: ${SRCDIR}/tif_aux.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_aux.c
tif_close.o: ${SRCDIR}/tif_close.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_close.c
tif_codec.o: ${SRCDIR}/tif_codec.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_codec.c
tif_compress.o: ${SRCDIR}/tif_compress.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_compress.c
tif_dir.o: ${SRCDIR}/tif_dir.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_dir.c
tif_dirinfo.o: ${SRCDIR}/tif_dirinfo.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_dirinfo.c
tif_dirread.o: ${SRCDIR}/tif_dirread.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_dirread.c
tif_dirwrite.o: ${SRCDIR}/tif_dirwrite.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_dirwrite.c
tif_dumpmode.o: ${SRCDIR}/tif_dumpmode.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_dumpmode.c
tif_error.o: ${SRCDIR}/tif_error.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_error.c
tif_fax3.o: ${SRCDIR}/tif_fax3.c ${SRCDIR}/t4.h ${SRCDIR}/tif_fax3.h
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_fax3.c
tif_getimage.o: ${SRCDIR}/tif_getimage.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_getimage.c
tif_jpeg.o: ${SRCDIR}/tif_jpeg.c  
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_jpeg.c
tif_flush.o: ${SRCDIR}/tif_flush.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_flush.c
tif_luv.o: ${SRCDIR}/tif_luv.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_luv.c
tif_lzw.o: ${SRCDIR}/tif_lzw.c ${SRCDIR}/tif_predict.h
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_lzw.c
tif_next.o: ${SRCDIR}/tif_next.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_next.c
tif_open.o: ${SRCDIR}/tif_open.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_open.c
tif_packbits.o: ${SRCDIR}/tif_packbits.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_packbits.c
tif_pixarlog.o: ${SRCDIR}/tif_pixarlog.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_pixarlog.c
tif_predict.o: ${SRCDIR}/tif_predict.c ${SRCDIR}/tif_predict.h
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_predict.c
tif_print.o: ${SRCDIR}/tif_print.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_print.c
tif_read.o: ${SRCDIR}/tif_read.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_read.c
tif_swab.o: ${SRCDIR}/tif_swab.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_swab.c
tif_strip.o: ${SRCDIR}/tif_strip.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_strip.c
tif_thunder.o: ${SRCDIR}/tif_thunder.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_thunder.c
tif_tile.o: ${SRCDIR}/tif_tile.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_tile.c
tif_unix.o: ${SRCDIR}/tif_unix.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_unix.c
tif_version.o: ${SRCDIR}/tif_version.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_version.c
tif_warning.o: ${SRCDIR}/tif_warning.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_warning.c
tif_write.o: ${SRCDIR}/tif_write.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_write.c
tif_zip.o: ${SRCDIR}/tif_zip.c ${SRCDIR}/tif_predict.h 
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_zip.c

tif_apple.o: ${SRCDIR}/tif_apple.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_apple.c
tif_atari.o: ${SRCDIR}/tif_atari.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_atari.c
tif_msdos.o: ${SRCDIR}/tif_msdos.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_msdos.c
tif_vms.o: ${SRCDIR}/tif_vms.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_vms.c
tif_win3.o: ${SRCDIR}/tif_win3.c
	${CC} -c ${CFLAGS} ${SRCDIR}/tif_win3.c

INCS	= ${SRCDIR}/tiff.h ${SRCDIR}/tiffio.h ${SRCDIR}/tiffconf.h \
	${SRCDIR}/tiffvers.h

INCS_PRIVATE = ${SRCDIR}/tiffiop.h ${SRCDIR}/tif_dir.h ${SRCDIR}/port.h

installHdrs: ${INCS}
	${INSTALL} -idb tiff.sw.dev -m 755 -dir ${DESTDIR}c:/progra~1/libtiff/include
	for i in ${INCS}; do						\
	    f=`basename $$i`;						\
	    ${INSTALL} -idb tiff.sw.dev -m 444 -F ${DESTDIR}c:/progra~1/libtiff/include	\
		-src $$i -O $$f;					\
	done

installPrivateHdrs: ${INCS_PRIVATE}
	${INSTALL} -idb tiff.sw.dev -m 755 -dir ${DESTDIR}c:/progra~1/libtiff/include
	for i in ${INCS_PRIVATE}; do					\
	    f=`basename $$i`;						\
	    ${INSTALL} -idb tiff.sw.dev -m 444 -F ${DESTDIR}c:/progra~1/libtiff/include	\
		-src $$i -O $$f;					\
	done

installDSO: nodso
	if [ a != a ]; then			\
	    ${INSTALL} -idb tiff.sw.tools -m 555 -F ${DESTDIR}c:/progra~1/libtiff/lib	\
		-O libtiff.a;				\
	    ${INSTALL} -idb tiff.sw.tools -F ${DESTDIR}c:/progra~1/libtiff/lib	\
		-lns libtiff.a -O libtiff.a;	\
	    ${INSTALL} -idb tiff.sw.tools -F ${DESTDIR}c:/progra~1/libtiff/lib	\
		-lns libtiff.a -O libtiff.so;			\
	else								\
	    ${INSTALL} -idb tiff.sw.tools -m 555 -F ${DESTDIR}c:/progra~1/libtiff/lib	\
		-O libtiff.a;					\
	fi

install: all installHdrs
	${INSTALL} -idb tiff.sw.dev -m 755 -dir ${DESTDIR}c:/progra~1/libtiff/lib
	${INSTALL} -idb tiff.sw.dev -m 444 -F ${DESTDIR}c:/progra~1/libtiff/lib -O libtiff.a
	if [ nodso != nodso ]; then					\
	    ${MAKE} -${MAKEFLAGS} installDSO;				\
	else								\
	    true;							\
	fi

install-private: install installPrivateHdrs

clean:
	rm -f ${TARGETS} ${OBJS} core a.out \
	    mkg3states tif_fax3sm.c \
	    mkversion ${SRCDIR}/tiffvers.h \
	    libtiff.a libtiff.a libtiff.a *dso
