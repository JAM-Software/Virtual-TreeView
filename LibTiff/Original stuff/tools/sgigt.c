/* $Header: /cvsroot/osrs/libtiff/tools/sgigt.c,v 1.1.1.1 1999/07/27 21:50:28 mike Exp $ */

/*
 * Copyright (c) 1988-1997 Sam Leffler
 * Copyright (c) 1991-1997 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Sam Leffler and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Sam Leffler and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 * 
 * IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <gl.h>
#include <device.h>

#include "tiffio.h"

#ifndef TRUE
#define	TRUE	1
#define	FALSE	0
#endif

/* XXX fudge adjustment for window borders */
#define	YFUDGE	20
#define	XFUDGE	20

static	tileContigRoutine putContig;
static	tileSeparateRoutine putSeparate;
static	uint32 width, height;		/* window width & height */
static	uint32* raster = NULL;		/* displayable image */

extern	Colorindex greyi(int);
static	void setupColormapSupport(TIFFRGBAImage*);
static	void putContigAndDraw(TIFFRGBAImage*, uint32*,
    uint32, uint32, uint32, uint32, int32, int32, unsigned char*);
static	void putSeparateAndDraw(TIFFRGBAImage*, uint32*,
    uint32, uint32, uint32, uint32, int32, int32,
    unsigned char*, unsigned char*, unsigned char*, unsigned char*);

static	int prevImage(char* argv[], int ix, int b, int e, int wrap);
static	int nextImage(char* argv[], int ix, int b, int e, int wrap);
static	void usage(void);
static	uint16 photoArg(const char*);
static	void beep(void);

extern	char* optarg;
extern	int optind;

int
main(int argc, char* argv[])
{
    static Cursor hourglass = {
	0x1ff0, 0x1ff0, 0x0820, 0x0820,
	0x0820, 0x0c60, 0x06c0, 0x0100,
	0x0100, 0x06c0, 0x0c60, 0x0820,
	0x0820, 0x0820, 0x1ff0, 0x1ff0
    };
    int isRGB0 = -1, isRGB;
    int verbose = 0;
    int stoponerr = 0;			/* stop on read error */
    char* filename;
    TIFF* tif = NULL;
    int fg = 0;
    int c;
    int dirnum = -1;
    int order0 = 0, order;
    uint32 diroff = 0;
    uint16 photo0 = (uint16) -1, photo;
    long x, y, xmax, ymax;
    int ix, nix;
    TIFFErrorHandler oerror = TIFFSetErrorHandler(NULL);
    TIFFErrorHandler owarning = TIFFSetWarningHandler(NULL);
    uint32 w, h;
    long wid = -1;

    while ((c = getopt(argc, argv, "d:o:p:cerflmsvw")) != -1)
	switch (c) {
	case 'c':
	    isRGB0 = 0;
	    break;
	case 'd':
	    dirnum = atoi(optarg);
	    break;
	case 'e':
	    oerror = TIFFSetErrorHandler(oerror);
	    break;
	case 'f':
	    fg = 1;
	    break;
	case 'l':
	    order0 = FILLORDER_LSB2MSB;
	    break;
	case 'm':
	    order0 = FILLORDER_MSB2LSB;
	    break;
	case 'o':
	    diroff = strtoul(optarg, NULL, 0);
	    break;
	case 'p':
	    photo0 = photoArg(optarg);
	    break;
	case 'r':
	    isRGB0 = 1;
	    break;
	case 's':
	    stoponerr = 1;
	    break;
	case 'w':
	    owarning = TIFFSetWarningHandler(owarning);
	    break;
	case 'v':
	    verbose = 1;
	    break;
	case '?':
	    usage();
	    /*NOTREACHED*/
	}
    if (argc - optind < 1)
	usage();
    xmax = getgdesc(GD_XPMAX) - XFUDGE;
    ymax = getgdesc(GD_YPMAX) - YFUDGE;
    ix = optind;
    do {
	tif = TIFFOpen(argv[ix], "r");
    } while (tif == NULL && (ix = nextImage(argv, ix, optind, argc, FALSE)));
    if (tif == NULL)
	exit(0);
    if (ix == optind) {
	/*
	 * Set initial directory if user-specified
	 * file was opened successfully.
	 */
	if (dirnum != -1 && !TIFFSetDirectory(tif, dirnum))
	    TIFFError(argv[ix], "Error, seeking to directory %d", dirnum);
	if (diroff != 0 && !TIFFSetSubDirectory(tif, diroff))
	    TIFFError(argv[ix], "Error, setting subdirectory at %#x", diroff);
    }
    isRGB = isRGB0;
    order = order0;
    photo = photo0;
    goto newfile0;
    for (;;) {
	TIFFRGBAImage img;
	char title[1024];			/* window title line */
	const char* cp;
	int isrgb;

	if (order)
	    TIFFSetField(tif, TIFFTAG_FILLORDER, order);
	if (photo != (uint16) -1)
	    TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, photo);
	if (!TIFFRGBAImageBegin(&img, tif, stoponerr, title)) {
	    TIFFError(filename, title);
	    goto bad2;
	}
	/*
	 * Use a full-color window if the image is
	 * full color or a palette image and the
	 * hardware support is present.
	 */
	isrgb = isRGB;
	if (isrgb == -1)
	    isrgb = (img.bitspersample >= 8 &&
		(img.photometric == PHOTOMETRIC_RGB ||
		 img.photometric == PHOTOMETRIC_YCBCR ||
		 img.photometric == PHOTOMETRIC_SEPARATED ||
		 img.photometric == PHOTOMETRIC_PALETTE ||
		 img.photometric == PHOTOMETRIC_LOGLUV));
	/*
	 * Check to see if the hardware can display 24-bit RGB.
	 */
	if (isrgb && getgdesc(GD_BITS_NORM_SNG_RED) < img.bitspersample &&
	  !getgdesc(GD_DITHER)) {
	    if (verbose)
		printf("Warning, display is incapable of full RGB,%s\n",
			 " using dithered colormap");
	    isrgb = 0;
	}
	/*
	 * Colormap-based display is done by overriding the put
	 * routine to install a private method that understands
	 * how to convert RGBA values to suitable colormap indices.
	 */
	if (!isrgb)
	    setupColormapSupport(&img);
	/*
	 * Override default ``put routine'' with private
	 * routine that also draws the raster on the display.
	 */
	if (img.put.any == 0) {
	    TIFFError(filename,
		"No \"put\" routine; must not handle image format");
	    goto bad3;
	}
	if (img.isContig) {
	    putContig = img.put.contig;
	    img.put.contig = putContigAndDraw;
	} else {
	    putSeparate = img.put.separate;
	    img.put.separate = putSeparateAndDraw;
	}
	/*
	 * Setup the image raster as required.
	 */
	if ((w = img.width) > xmax)
	    w = xmax;
	if ((h = img.height) > ymax)
	    h = ymax;
	if (w != width || h != height) {
	    if (raster != NULL)
		_TIFFfree(raster), raster = NULL;
	    raster = (uint32*) _TIFFmalloc(w * h * sizeof (uint32));
	    if (raster == NULL) {
		width = height = 0;
		TIFFError(filename, "No space for raster buffer");
		goto bad3;
	    }
	    width = w;
	    height = h;
	}
	/*
	 * Create a new window or reconfigure an existing
	 * one to suit the image to be displayed.
	 */
	if (wid < 0) {
	    x = (xmax+XFUDGE-width)/2;
	    y = (ymax+YFUDGE-height)/2;
	    prefposition(x, x+width-1, y, y+height-1);
	    cp = strrchr(filename, '/');
	    sprintf(title, "%s [%u] %s",
		cp == NULL ? filename : cp+1,
		(unsigned int) TIFFCurrentDirectory(tif),
		isrgb ? " rgb" : " cmap");
	    if (fg)
		foreground();
	    wid = winopen(title);
	    if (wid < 0) {
		TIFFError(filename, "Can not create window");
		TIFFRGBAImageEnd(&img);
		break;
	    }
	    curstype(C16X1);
	    defcursor(1, hourglass);
	    qdevice(LEFTMOUSE);
	    qdevice(MIDDLEMOUSE);
	    qdevice(RIGHTMOUSE);
	    qdevice(KEYBD);
	    qdevice(PAGEUPKEY);
	    qdevice(PAGEDOWNKEY);
	    qdevice(HOMEKEY);
	    qdevice(ENDKEY);
	} else {
	    x = (xmax+XFUDGE-width)/2;
	    y = (ymax+YFUDGE-height)/2;
	    winposition(x, x+width-1, y, y+height-1);
	    viewport(0, width-1, 0, height-1);
	    cp = strrchr(filename, '/');
	    sprintf(title, "%s [%u] %s",
		cp == NULL ? filename : cp+1,
		(unsigned int) TIFFCurrentDirectory(tif),
		isrgb ? " rgb" : " cmap");
	    wintitle(title);
	}
	singlebuffer();
	if (isrgb) {
	    RGBmode();
	    gconfig();
	} else {
	    cmode();
	    gconfig();
	}
	/*
	 * Fetch the image.
	 */
	setcursor(1, 0, 0);
	greyi(225);
	clear();
	(void) TIFFRGBAImageGet(&img, raster, width, height);
	setcursor(0, 0, 0);
	/*
	 * Process input.
	 */
	for (;;) {
	    short val;
	    switch (qread(&val)) {
	    case KEYBD:
		switch (val) {
		case 'b':			/* photometric MinIsBlack */
		    photo = PHOTOMETRIC_MINISBLACK;
		    goto newpage;
		case 'l':			/* lsb-to-msb FillOrder */
		    order = FILLORDER_LSB2MSB;
		    goto newpage;
		case 'm':			/* msb-to-lsb FillOrder */
		    order = FILLORDER_MSB2LSB;
		    goto newpage;
		case 'c':			/* colormap visual */
		    isRGB = 0;
		    goto newpage;
		case 'r':			/* RGB visual */
		    isRGB = 1;
		    goto newpage;
		case 'w':			/* photometric MinIsWhite */
		    photo = PHOTOMETRIC_MINISWHITE;
		    goto newpage;
		case 'W':			/* toggle warnings */
		    owarning = TIFFSetWarningHandler(owarning);
		    goto newpage;
		case 'E':			/* toggle errors */
		    oerror = TIFFSetErrorHandler(oerror);
		    goto newpage;
		case 'z':			/* reset to defaults */
		case 'Z':
		    order = order0;
		    photo = photo0;
		    isRGB = isRGB0;
		    if (owarning == NULL)
			owarning = TIFFSetWarningHandler(NULL);
		    if (oerror == NULL)
			oerror = TIFFSetErrorHandler(NULL);
		    goto newpage;
		case 'q':			/* exit */
		case '\033':
		    TIFFRGBAImageEnd(&img);
		    goto done;
		}
		break;
	    case PAGEUPKEY:			/* previous logical image */
		if (val) {
		    if (TIFFCurrentDirectory(tif) > 0) {
			if (TIFFSetDirectory(tif, TIFFCurrentDirectory(tif)-1))
			    goto newpage;
			beep();		/* XXX */
		    } else {
			ix = prevImage(argv, ix, optind, argc, TRUE);
			/* XXX set directory to last image in new file */
			goto newfile;
		    }
		}
		break;
	    case PAGEDOWNKEY:			/* next logical image */
		if (val) {
		    if (!TIFFLastDirectory(tif)) {
			if (TIFFReadDirectory(tif))
			    goto newpage;
			beep();		/* XXX */
		    } else {
			ix = nextImage(argv, ix, optind, argc, TRUE);
			goto newfile;
		    }
		}
		break;
	    case HOMEKEY:			/* 1st image in current file */
		if (val) {
		    if (TIFFSetDirectory(tif, 0))
			goto newpage;
		    beep();
		}
		break;
	    case ENDKEY:			/* last image in current file */
		if (val) {
		    /* XXX */
		    beep();
		}
		break;
	    case RIGHTMOUSE:			/* previous file */
		if (val) {
		    if (nix = prevImage(argv, ix, optind, argc, FALSE)) {
			ix = nix;
			goto newfile;
		    }
		    beep();
		}
		break;
	    case LEFTMOUSE:			/* next file */
		if (val) {
		    if (nix = nextImage(argv, ix, optind, argc, FALSE)) {
			ix = nix;
			goto newfile;
		    }
		    beep();
		}
		break;
	    case MIDDLEMOUSE:			/* first file */
		if (val) {
		    if (nix = nextImage(argv, optind-1, optind, argc, FALSE)) {
			ix = nix;
			goto newfile;
		    }
		    beep();
		}
		break;
	    case REDRAW:
		lrectwrite(0, 0, width-1, height-1, raster);
		break;
	    }
	}
    newfile:
	TIFFRGBAImageEnd(&img);
	if (tif != NULL && argv[ix] != filename)
	    TIFFClose(tif), tif = NULL;
	/* fall thru... */
    newfile0:
	if (argv[ix] == NULL)
	    break;
	filename = argv[ix];
	if (tif == NULL) {
	    tif = TIFFOpen(filename, "r");
	    if (tif == NULL)
		goto bad1;
	    isRGB = isRGB0;
	    order = order0;
	    photo = photo0;
	}
	continue;
    newpage:
	TIFFRGBAImageEnd(&img);
	continue;
    bad3:
	TIFFRGBAImageEnd(&img);
    bad2:
	TIFFClose(tif), tif = NULL;
    bad1:
	argv[ix] = NULL;			/* don't revisit file */
	ix = nextImage(argv, ix, optind, argc, TRUE);
	goto newfile0;
    }
done:
    if (wid >= 0)
	winclose(wid);
    if (raster != NULL)
	_TIFFfree(raster);
    if (tif != NULL)
	TIFFClose(tif);
    return (0);
}

static int
prevImage(char* argv[], int ix, int b, int e, int wrap)
{
    int i;

    for (i = ix-1; i >= b && argv[i] == NULL; i--)
	;
    if (i < b) {
	if (wrap) {
	    for (i = e-1; i > ix && argv[i] == NULL; i--)
		;
	} else
	    i = 0;
    }
    return (i);
}

static int
nextImage(char* argv[], int ix, int b, int e, int wrap)
{
    int i;

    for (i = ix+1; i < e && argv[i] == NULL; i++)
	;
    if (i >= e) {
	if (wrap) {
	    for (i = b; i < ix && argv[i] == NULL; i++)
		;
	} else
	    i = 0;
    }
    return (i);
}

static void
beep(void)
{
    greyi(0);
    clear();
    sginap(5);
    lrectwrite(0, 0, width-1, height-1, raster);
}

char* stuff[] = {
"usage: tiffgt [options] file.tif",
"where options are:",
" -c		use colormap visual",
" -d dirnum	set initial directory (default is 0)",
" -e		enable display of TIFF error messages",
" -f  		run program in the foreground",
" -l  		force lsb-to-msb FillOrder",
" -m  		force msb-to-lsb FillOrder",
" -o offset	set initial directory offset",
" -p photo	override photometric interpretation",
" -r		use fullcolor visual",
" -s  		stop decoding on first error (default is ignore errors)",
" -v		enable verbose mode",
" -w		enable display of TIFF warning messages",
NULL
};

static void
usage(void)
{
    char buf[BUFSIZ];
    int i;

    setbuf(stderr, buf);
    for (i = 0; stuff[i] != NULL; i++)
	fprintf(stderr, "%s\n", stuff[i]);
    exit(-1);
}

static uint16
photoArg(const char* arg)
{
    if (strcmp(arg, "miniswhite") == 0)
	return (PHOTOMETRIC_MINISWHITE);
    else if (strcmp(arg, "minisblack") == 0)
	return (PHOTOMETRIC_MINISBLACK);
    else if (strcmp(arg, "rgb") == 0)
	return (PHOTOMETRIC_RGB);
    else if (strcmp(arg, "palette") == 0)
	return (PHOTOMETRIC_PALETTE);
    else if (strcmp(arg, "mask") == 0)
	return (PHOTOMETRIC_MASK);
    else if (strcmp(arg, "separated") == 0)
	return (PHOTOMETRIC_SEPARATED);
    else if (strcmp(arg, "ycbcr") == 0)
	return (PHOTOMETRIC_YCBCR);
    else if (strcmp(arg, "cielab") == 0)
	return (PHOTOMETRIC_CIELAB);
    else if (strcmp(arg, "logl") == 0)
	return (PHOTOMETRIC_LOGL);
    else if (strcmp(arg, "logluv") == 0)
	return (PHOTOMETRIC_LOGLUV);
    else
	return ((uint16) -1);
}

static void
putContigAndDraw(TIFFRGBAImage* img, uint32* raster,
    uint32 x, uint32 y, uint32 w, uint32 h,
    int32 fromskew, int32 toskew,
    unsigned char* cp)
{
    (*putContig)(img, raster, x, y, w, h, fromskew, toskew, cp);
    if (x+w == width) {
	w = width;
	if (img->orientation == ORIENTATION_TOPLEFT)
	    lrectwrite(0, y-(h-1), w-1, y, raster-x-(h-1)*w);
	else
	    lrectwrite(0, y, w-1, y+h-1, raster);
    }
}

static void
putSeparateAndDraw(TIFFRGBAImage* img, uint32* raster,
    uint32 x, uint32 y, uint32 w, uint32 h,
    int32 fromskew, int32 toskew,
    unsigned char* r, unsigned char* g, unsigned char* b, unsigned char* a)
{
    (*putSeparate)(img, raster, x, y, w, h, fromskew, toskew, r, g, b, a);
    if (x+w == width) {
	w = width;
	if (img->orientation == ORIENTATION_TOPLEFT)
	    lrectwrite(0, y-(h-1), w-1, y, raster-x-(h-1)*w);
	else
	    lrectwrite(0, y, w-1, y+h-1, raster);
    }
}

/*
 * {red,green,blue}_inverse are tables in libgutil.a that
 * do an inverse map from (r,g,b) to the closest colormap
 * index in the "standard" GL colormap.  grey_inverse is
 * the equivalent map for mapping greyscale values to
 * colormap indices.  We access these maps directly instead
 * of through the rgbi and greyi functions to avoid the
 * additional overhead of the color calls that they make.
 */
extern	u_char red_inverse[256];
extern	u_char green_inverse[256];
extern	u_char blue_inverse[256];
extern	u_char grey_inverse[256];
#define	greyi(g)	grey_inverse[g]

static u_char
rgbi(u_char r, u_char g, u_char b)
{
    return (r == g && g == b ? grey_inverse[r] :
	red_inverse[r] + green_inverse[g] + blue_inverse[b]);
}

/*
 * The following routines move decoded data returned
 * from the TIFF library into rasters that are suitable
 * for passing to lrecwrite.  They do the necessary
 * conversions for when a colormap drawing mode is used.
 */
#define	REPEAT8(op)	REPEAT4(op); REPEAT4(op)
#define	REPEAT4(op)	REPEAT2(op); REPEAT2(op)
#define	REPEAT2(op)	op; op
#define	CASE8(x,op)			\
    switch (x) {			\
    case 7: op; case 6: op; case 5: op;	\
    case 4: op; case 3: op; case 2: op;	\
    case 1: op;				\
    }
#define	CASE4(x,op)	switch (x) { case 3: op; case 2: op; case 1: op; }
#define	NOP

#define	UNROLL8(w, op1, op2) {		\
    uint32 _x;				\
    for (_x = w; _x >= 8; _x -= 8) {	\
	op1;				\
	REPEAT8(op2);			\
    }					\
    if (_x > 0) {			\
	op1;				\
	CASE8(_x,op2);			\
    }					\
}
#define	UNROLL4(w, op1, op2) {		\
    uint32 _x;				\
    for (_x = w; _x >= 4; _x -= 4) {	\
	op1;				\
	REPEAT4(op2);			\
    }					\
    if (_x > 0) {			\
	op1;				\
	CASE4(_x,op2);			\
    }					\
}
#define	UNROLL2(w, op1, op2) {		\
    uint32 _x;				\
    for (_x = w; _x >= 2; _x -= 2) {	\
	op1;				\
	REPEAT2(op2);			\
    }					\
    if (_x) {				\
	op1;				\
	op2;				\
    }					\
}

#define	SKEW(r,g,b,skew)	{ r += skew; g += skew; b += skew; }

#define	DECLAREContigPutFunc(name) \
static void name(\
    TIFFRGBAImage* img, \
    uint32* cp, \
    uint32 x, uint32 y, \
    uint32 w, uint32 h, \
    int32 fromskew, int32 toskew, \
    u_char* pp \
)

#define	DECLARESepPutFunc(name) \
static void name(\
    TIFFRGBAImage* img,\
    uint32* cp,\
    uint32 x, uint32 y, \
    uint32 w, uint32 h,\
    int32 fromskew, int32 toskew,\
    u_char* r, u_char* g, u_char* b, u_char* a\
)

static	tileContigRoutine libput;

/*
 * 8-bit packed samples => colormap
 */
DECLAREContigPutFunc(putcontig8bittile)
{
    int samplesperpixel = img->samplesperpixel;
    TIFFRGBValue* Map = img->Map;

    (void) y;
    fromskew *= samplesperpixel;
    if (Map) {
	while (h-- > 0) {
	    for (x = w; x-- > 0;) {
		*cp++ = rgbi(Map[pp[0]], Map[pp[1]], Map[pp[2]]);
		pp += samplesperpixel;
	    }
	    cp += toskew;
	    pp += fromskew;
	}
    } else {
	while (h-- > 0) {
	    for (x = w; x-- > 0;) {
		*cp++ = rgbi(pp[0], pp[1], pp[2]);
		pp += samplesperpixel;
	    }
	    cp += toskew;
	    pp += fromskew;
	}
    }
}

/*
 * Convert 8-bit packed samples => colormap
 */
DECLAREContigPutFunc(cvtcontig8bittile)
{
    (*libput)(img, cp, x, y, w, h, fromskew, toskew, pp);
    while (h-- > 0) {
	UNROLL8(w, NOP,
	    cp[0] = rgbi(TIFFGetR(cp[0]),TIFFGetG(cp[0]),TIFFGetB(cp[0])); cp++
	);
	cp += toskew;
    }
}

/*
 * 16-bit packed samples => colormap
 */
DECLAREContigPutFunc(putcontig16bittile)
{
    int samplesperpixel = img->samplesperpixel;
    TIFFRGBValue* Map = img->Map;

    (void) y;
    fromskew *= samplesperpixel;
    if (Map) {
	while (h-- > 0) {
	    for (x = w; x-- > 0;) {
		*cp++ = rgbi(Map[pp[0]], Map[pp[1]], Map[pp[2]]);
		pp += samplesperpixel;
	    }
	    cp += toskew;
	    pp += fromskew;
	}
    } else {
	while (h-- > 0) {
	    for (x = w; x-- > 0;) {
		*cp++ = rgbi(pp[0], pp[1], pp[2]);
		pp += samplesperpixel;
	    }
	    cp += toskew;
	    pp += fromskew;
	}
    }
}

/*
 * 8-bit unpacked samples => colormap
 */
DECLARESepPutFunc(putseparate8bittile)
{
    TIFFRGBValue* Map = img->Map;

    (void) y; (void) a;
    if (Map) {
	while (h-- > 0) {
	    for (x = w; x-- > 0;)
		*cp++ = rgbi(Map[*r++], Map[*g++], Map[*b++]);
	    SKEW(r, g, b, fromskew);
	    cp += toskew;
	}
    } else {
	while (h-- > 0) {
	    for (x = w; x-- > 0;)
		*cp++ = rgbi(*r++, *g++, *b++);
	    SKEW(r, g, b, fromskew);
	    cp += toskew;
	}
    }
}

/*
 * 16-bit unpacked samples => colormap
 */
DECLARESepPutFunc(putseparate16bittile)
{
    TIFFRGBValue* Map = img->Map;

    (void) y; (void) a;
    if (Map) {
	while (h-- > 0) {
	    for (x = 0; x < w; x++)
		*cp++ = rgbi(Map[*r++], Map[*g++], Map[*b++]);
	    SKEW(r, g, b, fromskew);
	    cp += toskew;
	}
    } else {
	while (h-- > 0) {
	    for (x = 0; x < w; x++)
		*cp++ = rgbi(*r++, *g++, *b++);
	    SKEW(r, g, b, fromskew);
	    cp += toskew;
	}
    }
}

/*
 * 8-bit packed CMYK samples => cmap
 *
 * NB: The conversion of CMYK->RGB is *very* crude.
 */
DECLAREContigPutFunc(putcontig8bitCMYKtile)
{
    int samplesperpixel = img->samplesperpixel;
    TIFFRGBValue* Map = img->Map;
    uint16 r, g, b, k;

    (void) y;
    fromskew *= samplesperpixel;
    if (Map) {
	while (h-- > 0) {
	    for (x = w; x-- > 0;) {
		k = 255 - pp[3];
		r = (k*(255-pp[0]))/255;
		g = (k*(255-pp[1]))/255;
		b = (k*(255-pp[2]))/255;
		*cp++ = rgbi(Map[r], Map[g], Map[b]);
		pp += samplesperpixel;
	    }
	    pp += fromskew;
	    cp += toskew;
	}
    } else {
	while (h-- > 0) {
	    UNROLL8(w, NOP,
		k = 255 - pp[3];
		r = (k*(255-pp[0]))/255;
		g = (k*(255-pp[1]))/255;
		b = (k*(255-pp[2]))/255;
		*cp++ = rgbi(r, g, b);
		pp += samplesperpixel);
	    cp += toskew;
	    pp += fromskew;
	}
    }
}

#define	YCbCrtoRGB(dst, yc) {						\
    int Y = (yc);							\
    dst = rgbi(								\
	clamptab[Y+Crrtab[Cr]],						\
	clamptab[Y + (int)((Cbgtab[Cb]+Crgtab[Cr])>>16)],		\
	clamptab[Y+Cbbtab[Cb]]);					\
}
#define	YCbCrSetup							\
    TIFFYCbCrToRGB* ycbcr = img->ycbcr;					\
    int* Crrtab = ycbcr->Cr_r_tab;					\
    int* Cbbtab = ycbcr->Cb_b_tab;					\
    int32* Crgtab = ycbcr->Cr_g_tab;					\
    int32* Cbgtab = ycbcr->Cb_g_tab;					\
    TIFFRGBValue* clamptab = ycbcr->clamptab

/*
 * 8-bit packed YCbCr samples w/ 2,2 subsampling => RGB
 */
DECLAREContigPutFunc(putcontig8bitYCbCr22tile)
{
    YCbCrSetup;
    uint32* cp1 = cp+w+toskew;
    int32 incr = 2*toskew+w;

    (void) y;
    /* XXX adjust fromskew */
    for (; h >= 2; h -= 2) {
	x = w>>1;
	do {
	    int Cb = pp[4];
	    int Cr = pp[5];

	    YCbCrtoRGB(cp [0], pp[0]);
	    YCbCrtoRGB(cp [1], pp[1]);
	    YCbCrtoRGB(cp1[0], pp[2]);
	    YCbCrtoRGB(cp1[1], pp[3]);

	    cp += 2, cp1 += 2;
	    pp += 6;
	} while (--x);
	cp += incr, cp1 += incr;
	pp += fromskew;
    }
}
#undef	YCbCrSetup
#undef	YCbCrtoRGB

/*
 * Setup to handle conversion for display in a colormap
 * window.  Many cases are handled by massaging the mapping
 * tables used by the normal library code to convert 32-bit
 * packed RGBA samples into colormap indices.  Other cases
 * are handled with special-case routines that replace the
 * normal ``put routine'' installed by the library.
 */
static void
setupColormapSupport(TIFFRGBAImage* img)
{
    int bitspersample = img->bitspersample;
    int i;

    if (img->BWmap) {
	i = 255;
	do {
	    uint32* p = img->BWmap[i];
	    switch (bitspersample) {
#define	GREY(x)	p[x] = greyi(TIFFGetR(p[x]))
	    case 1: GREY(7); GREY(6); GREY(5); GREY(4);
	    case 2: GREY(3); GREY(2);
	    case 4: GREY(1);
	    case 8: GREY(0);
	    }
#undef	GREY
	} while (i--);
    } else if (img->PALmap) {
	i = 255;
	do {
	    uint32 rgb;
	    uint32* p = img->PALmap[i];
#define	CMAP(x) \
    (rgb = p[x], p[x] = rgbi(TIFFGetR(rgb),TIFFGetG(rgb),TIFFGetB(rgb)))
	    switch (bitspersample) {
	    case 1: CMAP(7); CMAP(6); CMAP(5); CMAP(4);
	    case 2: CMAP(3); CMAP(2);
	    case 4: CMAP(1);
	    case 8: CMAP(0);
	    }
#undef CMAP
	} while (i--);
    } else if (img->isContig) {
	switch (img->photometric) {
	case PHOTOMETRIC_RGB:
	case PHOTOMETRIC_LOGLUV:
		switch (bitspersample) {
		case 8:  img->put.contig = putcontig8bittile; break;
		case 16: img->put.contig = putcontig16bittile; break;
		}
		break;
	case PHOTOMETRIC_SEPARATED:
		switch (bitspersample) {
		case 8:  img->put.contig = putcontig8bitCMYKtile; break;
		}
		break;
	case PHOTOMETRIC_YCBCR:
		if (img->bitspersample == 8) {
		    uint16 hs, vs;
		    TIFFGetFieldDefaulted(img->tif, TIFFTAG_YCBCRSUBSAMPLING,
			&hs, &vs);
		    switch ((hs<<4)|vs) {
		    case 0x22:			/* most common case */
			img->put.contig = putcontig8bitYCbCr22tile;
			break;
		    default:			/* all others cost more */
			libput = img->put.contig;
			img->put.contig = cvtcontig8bittile;
			break;
		    }
		}
		break;
	}
    } else {
	switch (img->photometric) {
	case PHOTOMETRIC_RGB:
	    switch (img->bitspersample) {
	    case 8:  img->put.separate = putseparate8bittile; break;
	    case 16: img->put.separate = putseparate16bittile; break;
	    }
	    break;
	}
    }
}
