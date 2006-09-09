#! smake
#	$Header: /cvsroot/osrs/libtiff/man/Makefile.in,v 1.10 2001/09/26 17:32:24 warmerda Exp $
#
# Warning, this file was automatically created by the TIFF configure script
#
# Tag Image File Format Library Manual Pages
#
# Copyright (c) 1991-1997 Sam Leffler
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
SRCDIR	= ../man

#
# VERSION:	v3.5.7
# DATE:		Wed Nov 28 23:15:22  2001
# TARGET:	
# CCOMPILER:	/cygdrive/i/MINGW/BIN/gcc
#
SHELL	= /usr/bin/sh
SCRIPT_SH = /usr/bin/sh
NULL	=
ECHO	= echo
SED	= sed
MV	= mv
RM	= rm -f
INSTALL	= ${SCRIPT_SH} ../port/install.sh

#
# Manual-related controls:
#
# MAN		head of manual tree
# MANAPPS	subdirectory for application programs
# MANLIB	subdirectory for library functions
#
MAN		= c:/progra~1/libtiff/man
MANAPPS		= man1
MANLIB		= man3
#
# MANSEDLOCAL	additional sed commands to use during source transformation
#		(might want to transform section names here)
# MANCVT	command to convert source to installable target (can use
#		$? and $@, but not $<)
MANSEDLOCAL	= 
MANCVT		= ${MANSED} $? >$@
#
# The following macros are used during the install step to generate
# the filenames of the target manual pages.  They appear inside a
# shell for loop in a command of the form:
#
#	f=${MAN*NAME}
#
# where the variable ``i'' contains the filename of the formatted
# manual page (formatted according to the MANCVT macro).
#
# MANAPPNAME	generate installed app man page filename
# MANLIBNAME	generate installed library man page filename
#
#
MANAPPNAME	= $$i
MANLIBNAME	= $$i

#
# Set this up if your manual system requires a
# pathname prefix in the .so commands when setting
# up alternate files for multi-function manual pages.
#
#MANDIR	= man3/

MANTOOLS=\
	apps/fax2tiff.1 \
	apps/fax2ps.1 \
	apps/gif2tiff.1 \
	apps/pal2rgb.1 \
	apps/ppm2tiff.1 \
	apps/ras2tiff.1 \
	apps/rgb2ycbcr.1 \
	apps/sgi2tiff.1 \
	apps/thumbnail.1 \
	apps/tiff2bw.1 \
	apps/tiff2rgba.1 \
	apps/tiff2ps.1 \
	apps/tiffcmp.1 \
	apps/tiffcp.1 \
	apps/tiffdither.1 \
	apps/tiffdump.1 \
	apps/tiffgt.1 \
	apps/tiffinfo.1 \
	apps/tiffmedian.1 \
	apps/tiffsplit.1 \
	apps/tiffsv.1 \
	${NULL}
MANDEV=\
	lib/libtiff.3t \
	\
	lib/TIFFClose.3t \
	lib/TIFFError.3t \
	lib/TIFFFlush.3t \
	lib/TIFFGetField.3t \
	lib/TIFFOpen.3t \
	lib/TIFFPrintDirectory.3t \
	lib/TIFFReadDirectory.3t \
	lib/TIFFReadEncodedStrip.3t \
	lib/TIFFReadEncodedTile.3t \
	lib/TIFFReadRGBAImage.3t \
	lib/TIFFReadRGBAStrip.3t \
	lib/TIFFReadRGBATile.3t \
	lib/TIFFReadRawStrip.3t \
	lib/TIFFReadRawTile.3t \
	lib/TIFFReadScanline.3t \
	lib/TIFFReadTile.3t \
	lib/TIFFRGBAImage.3t \
	lib/TIFFSetDirectory.3t \
	lib/TIFFSetField.3t \
	lib/TIFFWarning.3t \
	lib/TIFFWriteDirectory.3t \
	lib/TIFFWriteEncodedStrip.3t \
	lib/TIFFWriteEncodedTile.3t \
	lib/TIFFWriteTile.3t \
	lib/TIFFWriteRawStrip.3t \
	lib/TIFFWriteRawTile.3t \
	lib/TIFFWriteScanline.3t \
	\
	lib/TIFFbuffer.3t \
	lib/TIFFcodec.3t \
	lib/TIFFmemory.3t \
	lib/TIFFquery.3t \
	lib/TIFFsize.3t \
	lib/TIFFstrip.3t \
	lib/TIFFswab.3t \
	lib/TIFFtile.3t \
	${NULL}
MANDEVLINKS=\
	lib/TIFFmalloc.3t \
	lib/TIFFrealloc.3t \
	lib/TIFFfree.3t \
	lib/TIFFmemset.3t \
	lib/TIFFmemcpy.3t \
	lib/TIFFmemcmp.3t \
	lib/TIFFComputeTile.3t \
	lib/TIFFCheckTile.3t \
	lib/TIFFNumberOfTiles.3t \
	lib/TIFFComputeStrip.3t \
	lib/TIFFNumberOfStrips.3t \
	lib/TIFFCurrentDirectory.3t \
	lib/TIFFCurrentRow.3t \
	lib/TIFFCurrentStrip.3t \
	lib/TIFFCurrentTile.3t \
	lib/TIFFFdOpen.3t \
	lib/TIFFClientOpen.3t \
	lib/TIFFFileName.3t \
	lib/TIFFFileno.3t \
	lib/TIFFFindCODEC.3t \
	lib/TIFFFlushData.3t \
	lib/TIFFGetMode.3t \
	lib/TIFFIsTiled.3t \
	lib/TIFFIsByteSwapped.3t \
	lib/TIFFIsUpSampled.3t \
	lib/TIFFIsMSB2LSB.3t \
	lib/TIFFLastDirectory.3t \
	lib/TIFFRegisterCODEC.3t \
	lib/TIFFReverseBits.3t \
	lib/TIFFRewriteDirectory.3t \
	lib/TIFFRGBAImageOK.3t \
	lib/TIFFRGBAImageBegin.3t \
	lib/TIFFRGBAImageGet.3t \
	lib/TIFFRGBAImageEnd.3t \
	lib/TIFFSetErrorHandler.3t \
	lib/TIFFSetSubDirectory.3t \
	lib/TIFFSetWarningHandler.3t \
	lib/TIFFSwabArrayOfLong.3t \
	lib/TIFFSwabArrayOfShort.3t \
	lib/TIFFSwabLong.3t \
	lib/TIFFSwabShort.3t \
	lib/TIFFScanlineSize.3t \
	lib/TIFFDefaultStripSize.3t \
	lib/TIFFVStripSize.3t \
	lib/TIFFStripSize.3t \
	lib/TIFFDefaultTileSize.3t \
	lib/TIFFVTileSize.3t \
	lib/TIFFTileSize.3t \
	lib/TIFFTileRowSize.3t \
	lib/TIFFUnRegisterCODEC.3t \
	lib/TIFFVGetField.3t \
	lib/TIFFVSetField.3t \
	lib/TIFFReadBufferSetup.3t \
	lib/TIFFWriteBufferSetup.3t \
	${NULL}
TARGETS=apps/Makefile lib/Makefile

#
# System-specific manual page formatting commands should
# apply this sed command to the manual page source before
# handing it to the formatter.  This sets any pathnames
# in the man pages to reflect local conventions.
#
MANSED=${SED}\
	-e 's;\$${DIR_BIN};c:/progra~1/libtiff/bin;g' \
	-e 's;\$${DIR_LIB};c:/progra~1/libtiff/lib;g' \
	-e 's;\$${DIR_INC};c:/progra~1/libtiff/include;g' \
	${MANSEDLOCAL}

all:	${TARGETS}

apps/fax2tiff.1::	${SRCDIR}/fax2tiff.1;	${MANCVT}
apps/fax2ps.1::		${SRCDIR}/fax2ps.1;	${MANCVT}
apps/gif2tiff.1::	${SRCDIR}/gif2tiff.1;	${MANCVT}
apps/pal2rgb.1::	${SRCDIR}/pal2rgb.1;	${MANCVT}
apps/ppm2tiff.1::	${SRCDIR}/ppm2tiff.1;	${MANCVT}
apps/ras2tiff.1::	${SRCDIR}/ras2tiff.1;	${MANCVT}
apps/rgb2ycbcr.1::	${SRCDIR}/rgb2ycbcr.1;	${MANCVT}
apps/sgi2tiff.1::	${SRCDIR}/sgi2tiff.1;	${MANCVT}
apps/thumbnail.1::	${SRCDIR}/thumbnail.1;	${MANCVT}
apps/tiff2bw.1::	${SRCDIR}/tiff2bw.1;	${MANCVT}
apps/tiff2rgba.1::	${SRCDIR}/tiff2rgba.1;	${MANCVT}
apps/tiff2ps.1::	${SRCDIR}/tiff2ps.1;	${MANCVT}
apps/tiffcmp.1::	${SRCDIR}/tiffcmp.1;	${MANCVT}
apps/tiffcp.1::		${SRCDIR}/tiffcp.1;	${MANCVT}
apps/tiffdither.1::	${SRCDIR}/tiffdither.1;	${MANCVT}
apps/tiffdump.1::	${SRCDIR}/tiffdump.1;	${MANCVT}
apps/tiffgt.1::		${SRCDIR}/tiffgt.1;	${MANCVT}
apps/tiffinfo.1::	${SRCDIR}/tiffinfo.1;	${MANCVT}
apps/tiffmedian.1::	${SRCDIR}/tiffmedian.1;	${MANCVT}
apps/tiffsplit.1::	${SRCDIR}/tiffsplit.1;	${MANCVT}
apps/tiffsv.1::		${SRCDIR}/tiffsv.1;	${MANCVT}

lib/libtiff.3t::	${SRCDIR}/libtiff.3t;			${MANCVT}
lib/TIFFClose.3t::	${SRCDIR}/TIFFClose.3t;			${MANCVT}
lib/TIFFError.3t::	${SRCDIR}/TIFFError.3t;			${MANCVT}
lib/TIFFFlush.3t::	${SRCDIR}/TIFFFlush.3t;			${MANCVT}
lib/TIFFGetField.3t::	${SRCDIR}/TIFFGetField.3t;		${MANCVT}
lib/TIFFOpen.3t::	${SRCDIR}/TIFFOpen.3t;			${MANCVT}
lib/TIFFPrintDirectory.3t:: ${SRCDIR}/TIFFPrintDirectory.3t;	${MANCVT}
lib/TIFFReadDirectory.3t:: ${SRCDIR}/TIFFReadDirectory.3t;	${MANCVT}
lib/TIFFReadEncodedStrip.3t:: ${SRCDIR}/TIFFReadEncodedStrip.3t;${MANCVT}
lib/TIFFReadEncodedTile.3t:: ${SRCDIR}/TIFFReadEncodedTile.3t;	${MANCVT}
lib/TIFFReadRGBAImage.3t:: ${SRCDIR}/TIFFReadRGBAImage.3t;	${MANCVT}
lib/TIFFReadRGBATile.3t:: ${SRCDIR}/TIFFReadRGBATile.3t;	${MANCVT}
lib/TIFFReadRGBAStrip.3t:: ${SRCDIR}/TIFFReadRGBAStrip.3t;	${MANCVT}
lib/TIFFReadRawStrip.3t:: ${SRCDIR}/TIFFReadRawStrip.3t;	${MANCVT}
lib/TIFFReadRawTile.3t:: ${SRCDIR}/TIFFReadRawTile.3t;		${MANCVT}
lib/TIFFReadScanline.3t:: ${SRCDIR}/TIFFReadScanline.3t;	${MANCVT}
lib/TIFFReadTile.3t::	${SRCDIR}/TIFFReadTile.3t;		${MANCVT}
lib/TIFFRGBAImage.3t::	${SRCDIR}/TIFFRGBAImage.3t;		${MANCVT}
lib/TIFFSetDirectory.3t:: ${SRCDIR}/TIFFSetDirectory.3t;	${MANCVT}
lib/TIFFSetField.3t::	${SRCDIR}/TIFFSetField.3t;		${MANCVT}
lib/TIFFWarning.3t::	${SRCDIR}/TIFFWarning.3t;		${MANCVT}
lib/TIFFWriteDirectory.3t:: ${SRCDIR}/TIFFWriteDirectory.3t;	${MANCVT}
lib/TIFFWriteEncodedStrip.3t:: ${SRCDIR}/TIFFWriteEncodedStrip.3t; ${MANCVT}
lib/TIFFWriteEncodedTile.3t:: ${SRCDIR}/TIFFWriteEncodedTile.3t; ${MANCVT}
lib/TIFFWriteTile.3t:: ${SRCDIR}/TIFFWriteTile.3t; ${MANCVT}
lib/TIFFWriteRawStrip.3t:: ${SRCDIR}/TIFFWriteRawStrip.3t;	${MANCVT}
lib/TIFFWriteRawTile.3t:: ${SRCDIR}/TIFFWriteRawTile.3t;	${MANCVT}
lib/TIFFWriteScanline.3t:: ${SRCDIR}/TIFFWriteScanline.3t;	${MANCVT}
lib/TIFFbuffer.3t::	${SRCDIR}/TIFFbuffer.3t;		${MANCVT}
lib/TIFFcodec.3t::	${SRCDIR}/TIFFcodec.3t;			${MANCVT}
lib/TIFFmemory.3t::	${SRCDIR}/TIFFmemory.3t;		${MANCVT}
lib/TIFFquery.3t::	${SRCDIR}/TIFFquery.3t;			${MANCVT}
lib/TIFFsize.3t::	${SRCDIR}/TIFFsize.3t;			${MANCVT}
lib/TIFFstrip.3t::	${SRCDIR}/TIFFstrip.3t;			${MANCVT}
lib/TIFFswab.3t::	${SRCDIR}/TIFFswab.3t;			${MANCVT}
lib/TIFFtile.3t::	${SRCDIR}/TIFFtile.3t;			${MANCVT}

lib/TIFFComputeTile.3t:;	${ECHO} ".so ${MANDIR}TIFFtile.3t" > $@
lib/TIFFCheckTile.3t:;		${ECHO} ".so ${MANDIR}TIFFtile.3t" > $@
lib/TIFFNumberOfTiles.3t:;	${ECHO} ".so ${MANDIR}TIFFtile.3t" > $@
lib/TIFFComputeStrip.3t:;	${ECHO} ".so ${MANDIR}TIFFstrip.3t" > $@
lib/TIFFNumberOfStrips.3t:;	${ECHO} ".so ${MANDIR}TIFFstrip.3t" > $@
lib/TIFFCurrentDirectory.3t:;	${ECHO} ".so ${MANDIR}TIFFquery.3t" > $@
lib/TIFFCurrentRow.3t:;		${ECHO} ".so ${MANDIR}TIFFquery.3t" > $@
lib/TIFFCurrentStrip.3t:;	${ECHO} ".so ${MANDIR}TIFFquery.3t" > $@
lib/TIFFCurrentTile.3t:;	${ECHO} ".so ${MANDIR}TIFFquery.3t" > $@
lib/TIFFFdOpen.3t:;		${ECHO} ".so ${MANDIR}TIFFOpen.3t" > $@
lib/TIFFClientOpen.3t:;		${ECHO} ".so ${MANDIR}TIFFOpen.3t" > $@
lib/TIFFFileName.3t:;		${ECHO} ".so ${MANDIR}TIFFquery.3t" > $@
lib/TIFFFileno.3t:;		${ECHO} ".so ${MANDIR}TIFFquery.3t" > $@
lib/TIFFFlushData.3t:;		${ECHO} ".so ${MANDIR}TIFFFlush.3t" > $@
lib/TIFFGetMode.3t:;		${ECHO} ".so ${MANDIR}TIFFquery.3t" > $@
lib/TIFFIsTiled.3t:;		${ECHO} ".so ${MANDIR}TIFFquery.3t" > $@
lib/TIFFIsByteSwapped.3t:;	${ECHO} ".so ${MANDIR}TIFFquery.3t" > $@
lib/TIFFIsUpSampled.3t:;	${ECHO} ".so ${MANDIR}TIFFquery.3t" > $@
lib/TIFFIsMSB2LSB.3t:;		${ECHO} ".so ${MANDIR}TIFFquery.3t" > $@
lib/TIFFLastDirectory.3t:;	${ECHO} ".so ${MANDIR}TIFFquery.3t" > $@
lib/TIFFReverseBits.3t:;	${ECHO} ".so ${MANDIR}TIFFswab.3t" > $@
lib/TIFFRewriteDirectory.3t:;	${ECHO} ".so ${MANDIR}TIFFWriteDirectory.3t" > $@
lib/TIFFRGBAImageOK.3t:;	${ECHO} ".so ${MANDIR}TIFFRGBAImage.3t" > $@
lib/TIFFRGBAImageBegin.3t:;	${ECHO} ".so ${MANDIR}TIFFRGBAImage.3t" > $@
lib/TIFFRGBAImageGet.3t:;	${ECHO} ".so ${MANDIR}TIFFRGBAImage.3t" > $@
lib/TIFFRGBAImageEnd.3t:;	${ECHO} ".so ${MANDIR}TIFFRGBAImage.3t" > $@
lib/TIFFSetErrorHandler.3t:;	${ECHO} ".so ${MANDIR}TIFFError.3t" > $@
lib/TIFFSetSubDirectory.3t:;	${ECHO} ".so ${MANDIR}TIFFSetDirectory.3t" > $@
lib/TIFFSetWarningHandler.3t:;	${ECHO} ".so ${MANDIR}TIFFWarning.3t" > $@
lib/TIFFSwabArrayOfLong.3t:;	${ECHO} ".so ${MANDIR}TIFFswab.3t" > $@
lib/TIFFSwabArrayOfShort.3t:;	${ECHO} ".so ${MANDIR}TIFFswab.3t" > $@
lib/TIFFSwabLong.3t:;		${ECHO} ".so ${MANDIR}TIFFswab.3t" > $@
lib/TIFFSwabShort.3t:;		${ECHO} ".so ${MANDIR}TIFFswab.3t" > $@
lib/TIFFScanlineSize.3t:;	${ECHO} ".so ${MANDIR}TIFFsize.3t" > $@
lib/TIFFRasterScanlineSize.3t:;	${ECHO} ".so ${MANDIR}TIFFsize.3t" > $@
lib/TIFFDefaultStripSize.3t:;	${ECHO} ".so ${MANDIR}TIFFstrip.3t" > $@
lib/TIFFStripSize.3t:;		${ECHO} ".so ${MANDIR}TIFFstrip.3t" > $@
lib/TIFFVStripSize.3t:;		${ECHO} ".so ${MANDIR}TIFFstrip.3t" > $@
lib/TIFFTileSize.3t:;		${ECHO} ".so ${MANDIR}TIFFtile.3t" > $@
lib/TIFFVTileSize.3t:;		${ECHO} ".so ${MANDIR}TIFFtile.3t" > $@
lib/TIFFDefaultTileSize.3t:;	${ECHO} ".so ${MANDIR}TIFFtile.3t" > $@
lib/TIFFTileRowSize.3t:;	${ECHO} ".so ${MANDIR}TIFFtile.3t" > $@
lib/TIFFVGetField.3t:;		${ECHO} ".so ${MANDIR}TIFFGetField.3t" > $@
lib/TIFFVSetField.3t:;		${ECHO} ".so ${MANDIR}TIFFSetField.3t" > $@
lib/TIFFFindCODEC.3t:;		${ECHO} ".so ${MANDIR}TIFFcodec.3t" > $@
lib/TIFFRegisterCODEC.3t:;	${ECHO} ".so ${MANDIR}TIFFcodec.3t" > $@
lib/TIFFUnRegisterCODEC.3t:;	${ECHO} ".so ${MANDIR}TIFFcodec.3t" > $@
lib/TIFFmalloc.3t:;		${ECHO} ".so ${MANDIR}TIFFmemory.3t" > $@
lib/TIFFrealloc.3t:;		${ECHO} ".so ${MANDIR}TIFFmemory.3t" > $@
lib/TIFFfree.3t:;		${ECHO} ".so ${MANDIR}TIFFmemory.3t" > $@
lib/TIFFmemset.3t:;		${ECHO} ".so ${MANDIR}TIFFmemory.3t" > $@
lib/TIFFmemcpy.3t:;		${ECHO} ".so ${MANDIR}TIFFmemory.3t" > $@
lib/TIFFmemcmp.3t:;		${ECHO} ".so ${MANDIR}TIFFmemory.3t" > $@
lib/TIFFReadBufferSetup.3t:;	${ECHO} ".so ${MANDIR}TIFFbuffer.3t" > $@
lib/TIFFWriteBufferSetup.3t:;	${ECHO} ".so ${MANDIR}TIFFbuffer.3t" > $@

apps/Makefile:
	test -d apps || mkdir apps
	${MAKE} -${MAKEFLAGS} ${MANTOOLS}
	${RM} apps/Makefile
	(cd apps;							\
	 ${ECHO} 'install:';						\
	 for i in *.1; do						\
	    f=${MANAPPNAME};						\
	    ${ECHO} '	cd ..; ${INSTALL} -m 444 			\
		-F ${DESTDIR}${MAN}/${MANAPPS}				\
		-idb tiff.man.tools -src' apps/"$$i" '-O' "$$f";	\
	 done								\
	)>apps/Makefile
lib/Makefile:
	test -d lib || mkdir lib
	${MAKE} -${MAKEFLAGS} ${MANDEV} ${MANDEVLINKS}
	${RM} lib/Makefile
	(cd lib;							\
	 ${ECHO} 'install:';						\
	 for i in *.3t; do						\
	    f=${MANLIBNAME};						\
	    ${ECHO} '	cd ..; ${INSTALL} -m 444 			\
		-F ${DESTDIR}${MAN}/${MANLIB}				\
		-idb tiff.man.dev -src' lib/"$$i" '-O' "$$f";		\
	 done								\
	)>lib/Makefile

install: all
	${INSTALL} -m 755 -dir -idb tiff.man.tools ${DESTDIR}${MAN}
	${INSTALL} -m 755 -dir -idb tiff.man.tools ${DESTDIR}${MAN}/${MANAPPS}
	cd apps; ${MAKE} -${MAKEFLAGS} install
	${INSTALL} -m 755 -dir -idb tiff.man.dev ${DESTDIR}${MAN}/${MANLIB}
	cd lib; ${MAKE} -${MAKEFLAGS} install

clean:
	rm -rf apps lib

#
# Miscellaneous junk left over...
#

links:	${MANDEVLINKS}

short:
	${MV} TIFFClose.3t close.3t
	${MV} TIFFError.3t error.3t
	${MV} TIFFFlush.3t flush.3t
	${MV} TIFFGetField.3t getfield.3t
	${MV} TIFFOpen.3t open.3t
	${MV} TIFFPrintDirectory.3t print.3t
	${MV} TIFFReadEncodedStrip.3t rdestrip.3t
	${MV} TIFFReadencodedTile.3t rdetile.3t
	${MV} TIFFReadRawStrip.3t rdrstrip.3t
	${MV} TIFFReadRawTile.3t rdrtile.3t
	${MV} TIFFReadDirectory.3t readdir.3t
	${MV} TIFFReadRGBAImage.3t rdimage.3t
	${MV} TIFFReadRGBAStrip.3t rdsimage.3t
	${MV} TIFFReadRGBATile.3t rdtimage.3t
	${MV} TIFFReadScanline.3t readline.3t
	${MV} TIFFReadTile.3t readtile.3t
	${MV} TIFFRGBAImage.3t rgbaimage.3t
	${MV} TIFFSetDirectory.3t setdir.3t
	${MV} TIFFSetField.3t setfield.3t
	${MV} TIFFWarning.3t warning.3t
	${MV} TIFFWriteEncodedStrip.3t wrestrip.3t
	${MV} TIFFWriteEncodedTile.3t wretile.3t
	${MV} TIFFWriteTile.3t wrttile.3t
	${MV} TIFFWriteDirectory.3t writedir.3t
	${MV} TIFFWriteRawStrip.3t wrrstrip.3t
	${MV} TIFFWriteRawTile.3t wrrtile.3t
	${MV} TIFFWriteScanline.3t writeline.3t
	${MV} TIFFtile.3t tile.3t
	${MV} TIFFstrip.3t strip.3t
	${MV} TIFFquery.3t query.3t
	${MV} TIFFswab.3t swab.3t
	${MV} TIFFsize.3t size.3t
	${MV} TIFFcodec.3t codec.3t
	${MV} TIFFmemory.3t memory.3t
	${MV} TIFFbuffer.3t buffer.3t
