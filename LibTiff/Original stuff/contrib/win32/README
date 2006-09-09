This mail from Scott describes changes to the library that I have
not made because I couldn't figure out exactly where they went.
Anything not in this file has either been placed in the appropriate
directory (e.g. libtiff/tif_win32.c) or applied to the current
source code (e.g. libtiff/tiffiop.h).

Note that the Window NT/Window 95 support is untested; Scott's work
was done with an earlier version of the library.  Hopefully this'll
get cleaned up soon.

	Sam

Date: Fri, 14 Apr 95 17:01:42 EDT
From: wagner@itek.com (scott wagner)
Message-Id: <9504142101.AA00764@cyan.>
To: sam@cthulhu.engr.sgi.com
Subject: Re: Libtiff for Win32 (Windows NT / Windows 95)

Hi, Sam!

Enclosed are my libtiff for win32 pieces.  They are in the form of 3 files
(tif_w32.c, dllshell.c, and libtiff.def), and 2 diffs (for tiffiop.h and 
tiffio.h).

Hope this is not too difficult to separate!

Regards,
Scott Wagner (wagner@itek.com)

tif_w32.c  ---------------------------------------------------------------

tiffiop.h  ---------------------------------------------------------------

tiffio.h  ---------------------------------------------------------------
38a39
> #ifdef _TIFFIOP_
39a41,43
> #else
> typedef void TIFF;	/* Avoid ANSI undefined structure warning */
> #endif
66a71,75
> #ifdef WIN32			/* WIN32 identifies Win32 compiles */
> #pragma warn -sig		/* Turn off Borland warn of long to short int convert */
> #pragma warn -par		/* Turn off Borland warn "Parameter x is never used" */
> DECLARE_HANDLE(thandle_t);		/* Win32 file handle */
> #else						/* if not WIN32_ */
67a77
> #endif					/* defined WIN32 */

(Message tiff:1396)
 -- using template mhl.format --
Date:    Mon, 17 Apr 1995 07:51:03 EDT
To:      sam@cthulhu.engr.sgi.com

From:    wagner@itek.com (scott wagner)
Subject: Libtiff for Win32

Return-Path: sam@flake.asd.sgi.com 
Delivery-Date: Mon, 17 Apr 1995 05:36:50 PDT
Return-Path: sam@flake.asd.sgi.com

Hello, Sam!

>   ... libtiff for win32 pieces.  They are in the form of 3 files
>   (tif_w32.c, dllshell.c, and libtiff.def), and 2 diffs (for tiffiop.h and 
>   tiffio.h).
>   
> I don't understand how these pieces fit together.  Can you please explain
> what dllshell.c and libtiff.def are for? 

Sorry I was short on documentation here ... I was rushing to get home to
dinner on Friday and the material for you was the last loose end I had to
deal with.  Excuses aside ...

The goal of the adaptation of libtiff to Win32 was to replace only one
environment-specific code module and to make minimal changes to header
files.  tif_win32.c required one addition to tiffio.h (the
DECLARE_HANDLE line, which is probably a better way to typedef thandle-t
under Windows 3.1 as well); it also required the addition of pv_map_handle
to the tiff structure and the conditional definition of the
TIFFUnmapFileContents macro in tiffiop.h (this because Win32 uses a handle
and a pointer in mapping memory, and I needed to save both).

Additionally, I made a general style change to tiffio.h.  If tiffio.h is
included by a client, which does not include tiffiop.h, then ANSI compilers
warn about the typedef of TIFF to a non-existent structure.  To avoid this,
I changed the typedef of TIFF in this case to void.

I also made a style change in the tiff structure.  All references to tif_fd
in the library treat it as int, yet the tif_fd member itself is short.  I
changed this member to int and added the reserved member to maintain
32-bit structure member alignment in a 32 bit environment.  [ At cost of
4 more bytes in an already bloated structure! :-) ]  As long as fd's are
less than 64k, the old member works; this is not the case in Win32, and is
not generally a safe assumption.

The module dllshell.c and the file libtiff.def are not specific to the 
functionality of libtiff; they are used to make a Win32 dynamic link
library, and would be best packaged with a Win32 makefile, which I have
not yet perfected.  I will, however, commit to a Borland and Microsoft C
makefile for libtiff; perhaps these two could be combined with the
makefile for the _next_ (!) release of libtiff when it is ready.

Once again, sorry for the confused message on Friday.  I hope that at
least tif_w32 and the tiffio.h and tiffiop.h diffs will be useful for
this release.

Regards,
Scott Wagner   (wagner@itek.com)
Itek Graphix
Rochester, NY
