//
//  Little cms
//  Copyright (C) 1998-2002 Marti Maria
//
// THIS SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
// EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
// WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
//
// IN NO EVENT SHALL MARTI MARIA BE LIABLE FOR ANY SPECIAL, INCIDENTAL,
// INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
// OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
// WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF
// LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
// OF THIS SOFTWARE.
//
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

// Version 1.10b

#ifndef __cms_H

// ********** Configuration toggles ****************************************

//   Optimization mode.
//
// Note that USE_ASSEMBLER Is fastest by far, but it is limited to Pentium.
// USE_FLOAT are the generic floating-point routines. USE_C should work on
// virtually any machine.

// #define USE_FLOAT        1
// #define USE_C            1
#define USE_ASSEMBLER    1

// 3D interpolation method - Tetrahedral is slightly faster, but not always
// is proper.  Also, tetrahedral algorithm seems it was
// covered by a Sakamoto's patent, now expired. Code is present here only
// for informational purposes.

#define USE_TRILINEAR       1
// #define USE_TETRAHEDRAL     1


// Define this if you are using this package as a DLL.
// The building of DLL is now available on all windows compilers

// #define LCMS_DLL     1
// #define LCMS_DLL_BUILD   1

// Uncomment if you are trying the engine in a non-windows environment
// like linux, SGI, VAX, FreeBSD, BeOS, etc.
// #define NON_WINDOWS  1

// Uncomment this one if you are using big endian machines (only meaningful
// when NON_WINDOWS is used)
// #define USE_BIG_ENDIAN   1

// Uncomment this one if your compiler/machine does support the
// "long long" type This will speedup fixed point math. (USE_C only)
#define USE_INT64        1


// Some machines does not have a reliable 'swab' function. Usually
// leave commented unless the testbed diagnoses the contrary.
// #define USE_CUSTOM_SWAB   1


// ********** End of configuration toggles ******************************

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

// Metroworks CodeWarrior

#if __MWERKS__ 
#   define unlink remove
#   if WIN32 
#       define USE_CUSTOM_SWAB 1
#   else
#       define NON_WINDOWS   1
#   endif
#endif


#ifdef NON_WINDOWS

// Non windows environments. Also avoid indentation on includes.

#undef LCMS_DLL

#ifdef  USE_ASSEMBLER
#  undef  USE_ASSEMBLER
#  define USE_C               1
#endif

#if defined(__sgi__) || defined(__sgi) || defined(__powerpc__)
#   define USE_BIG_ENDIAN      1
#endif

#if TARGET_CPU_PPC
#   define USE_BIG_ENDIAN   1
#endif

#ifdef macintosh
#   define USE_BIG_ENDIAN      1
#endif

#ifdef WORDS_BIGENDIAN
#   define USE_BIG_ENDIAN      1
#endif

#if defined(__OpenBSD__) || defined(__NetBSD__) || defined(__FreeBSD__)
#  include <sys/types.h>
#  define USE_INT64           1
#  undef  LONGLONG
#  define LONGLONG            u_int64_t
#endif

#ifdef USE_INT64
#   ifndef LONGLONG
#   define LONGLONG long long
#   endif
#endif

#include <memory.h>
#include <string.h>

#if defined(__GNUC__) || defined(__FreeBSD__)
#   include <sys/unistd.h>
#endif

typedef unsigned char BYTE, *LPBYTE; 
typedef unsigned short WORD, *LPWORD;
typedef unsigned int DWORD, *LPDWORD;
typedef int BOOL;
typedef char *LPSTR;
typedef void *LPVOID;
typedef void* LCMSHANDLE;


#define ZeroMemory(p,l)     memset((p),0,(l))
#define CopyMemory(d,s,l)   memcpy((d),(s),(l))
#define FAR

#ifndef stricmp
#   define stricmp strcasecmp
#endif


#ifndef FALSE
#       define FALSE 0
#endif
#ifndef TRUE
#       define TRUE  1
#endif

#define LOWORD(l)    ((WORD)(l))
#define HIWORD(l)    ((WORD)((DWORD)(l) >> 16))

#define MAX_PATH     (256)
#define cdecl


#else

// Win32 stuff

#ifndef WIN32_LEAN_AND_MEAN
#  define WIN32_LEAN_AND_MEAN
#endif

#include <windows.h>

typedef HANDLE LCMSHANDLE;


#ifdef  USE_INT64
#  ifndef LONGLONG
#    define LONGLONG __int64
#  endif
#endif

#endif


#include "icc34.h"          // ICC header file


// Plus some additions

#define lcmsSignature            ((icSignature)           0x6c636d73L)
#define icSigLuvKData            ((icColorSpaceSignature) 0x4C75764BL)  // 'LuvK'
#define icSigChromaticityTag     ((icTagSignature)        0x6368726dL)  // As per Addendum 2 to Spec. ICC.1:1998-09
#define icSigChromaticityType    ((icTagTypeSignature)    0x6368726dL)
#define icSigHexachromeData      ((icColorSpaceSignature) 0x4d434836L)  // MCH6
#define icSigParametricCurveType ((icTagTypeSignature)    0x70617261L)  // parametric (ICC 4.0)

#define icSigMultiLocalizedUnicodeType  ((icTagTypeSignature) 0x6D6C7563L)

#ifdef __cplusplus
extern "C" {
#endif

// Calling convention

#ifdef NON_WINDOWS
#  define LCMSEXPORT
#  define LCMSAPI
#else
# ifdef LCMS_DLL
#   ifdef __BORLANDC__
#      define LCMSEXPORT __stdcall _export
#      define LCMSAPI
#   else
       // VC++
#       define LCMSEXPORT  _stdcall
#       ifdef LCMS_DLL_BUILD
#           define LCMSAPI     __declspec(dllexport)
#       else
#           define LCMSAPI     __declspec(dllimport)
#       endif
#   endif
# else
#   ifdef __BORLANDC__ // ml: Let the user pick the calling convention via project settings.
#     define LCMSEXPORT
#   else
#       define LCMSEXPORT cdecl
#   endif
#   define LCMSAPI
# endif
#endif

#ifdef  USE_ASSEMBLER
#ifdef __BORLANDC__

#      define ASM     asm
#      define RET(v)  return(v)
#else
      // VC++
#      define ASM     __asm
#      define RET(v)  return
#endif
#endif

#ifdef _MSC_VER
#ifndef  stricmp
#      define stricmp _stricmp
#endif
#ifndef unlink
#      define unlink  _unlink
#endif
#ifndef swab
#      define swab    _swab
#endif
#ifndef itoa
#       define itoa   _itoa
#endif
#ifndef filelength
#       define filelength _filelength
#endif
#ifndef fileno
#       define fileno   _fileno
#endif
#ifndef strupr
#       define strupr   _strupr
#endif
#ifndef hypot
#       define hypot    _hypot
#endif
#endif


#ifndef M_PI
#       define M_PI    3.14159265358979323846
#endif

#ifndef LOGE
#       define LOGE   0.434294481   
#endif

// ********** Little cms API ***************************************************

typedef LCMSHANDLE cmsHPROFILE;        // Opaque typedefs to hide internals
typedef LCMSHANDLE cmsHTRANSFORM;

// Format of pixel is defined by one DWORD, using bit fields as follows
//
//            TTTTT U Y F P X S EEE CCCC BBB
//
//            T: Pixeltype
//            F: Flavor  0=MinIsBlack(Chocolate) 1=MinIsWhite(Vanilla)
//            P: Planar? 0=Chunky, 1=Planar
//            X: swap 16 bps endianess?
//            S: Do swap? ie, BGR, KYMC
//            E: Extra samples
//            C: Channels (Samples per pixel)
//            B: Bytes per sample
//            Y: Swap first - changes ABGR to BGRA and KCMY to CMYK


#define COLORSPACE_SH(s)       ((s) << 16)
#define SWAPFIRST_SH(s)        ((s) << 14)
#define FLAVOR_SH(s)           ((s) << 13)
#define PLANAR_SH(p)           ((p) << 12)
#define ENDIAN16_SH(e)         ((e) << 11)
#define DOSWAP_SH(e)           ((e) << 10)
#define EXTRA_SH(e)            ((e) << 7)
#define CHANNELS_SH(c)         ((c) << 3)
#define BYTES_SH(b)            (b)

// Pixel types

#define PT_ANY       0    // Don't check colorspace
                          // 1 & 2 are reserved
#define PT_GRAY      3
#define PT_RGB       4
#define PT_CMY       5
#define PT_CMYK      6
#define PT_YCbCr     7
#define PT_YUV       8      // Lu'v'
#define PT_XYZ       9
#define PT_Lab       10
#define PT_YUVK      11     // Lu'v'K
#define PT_HSV       12
#define PT_HLS       13
#define PT_Yxy       14
#define PT_HiFi      15

#define NOCOLORSPACECHECK(x)    ((x) & 0xFFFF)

// Some (not all!) representations

#ifndef TYPE_RGB_8      // TYPE_RGB_8 is a very common identifier, so don't include ours
                        // if user has already defined.

#define TYPE_GRAY_8            (COLORSPACE_SH(PT_GRAY)|CHANNELS_SH(1)|BYTES_SH(1))
#define TYPE_GRAY_8_REV        (COLORSPACE_SH(PT_GRAY)|CHANNELS_SH(1)|BYTES_SH(1)|FLAVOR_SH(1))
#define TYPE_GRAY_16           (COLORSPACE_SH(PT_GRAY)|CHANNELS_SH(1)|BYTES_SH(2))
#define TYPE_GRAY_16_REV       (COLORSPACE_SH(PT_GRAY)|CHANNELS_SH(1)|BYTES_SH(2)|FLAVOR_SH(1))
#define TYPE_GRAY_16_SE        (COLORSPACE_SH(PT_GRAY)|CHANNELS_SH(1)|BYTES_SH(2)|ENDIAN16_SH(1))
#define TYPE_GRAYA_8           (COLORSPACE_SH(PT_GRAY)|EXTRA_SH(1)|CHANNELS_SH(1)|BYTES_SH(1))
#define TYPE_GRAYA_16          (COLORSPACE_SH(PT_GRAY)|EXTRA_SH(1)|CHANNELS_SH(1)|BYTES_SH(2))
#define TYPE_GRAYA_16_SE       (COLORSPACE_SH(PT_GRAY)|EXTRA_SH(1)|CHANNELS_SH(1)|BYTES_SH(2)|ENDIAN16_SH(1))
#define TYPE_GRAYA_8_PLANAR    (COLORSPACE_SH(PT_GRAY)|EXTRA_SH(1)|CHANNELS_SH(1)|BYTES_SH(1)|PLANAR_SH(1))
#define TYPE_GRAYA_16_PLANAR   (COLORSPACE_SH(PT_GRAY)|EXTRA_SH(1)|CHANNELS_SH(1)|BYTES_SH(2)|PLANAR_SH(1))

#define TYPE_RGB_8             (COLORSPACE_SH(PT_RGB)|CHANNELS_SH(3)|BYTES_SH(1))
#define TYPE_RGB_8_PLANAR      (COLORSPACE_SH(PT_RGB)|CHANNELS_SH(3)|BYTES_SH(1)|PLANAR_SH(1))
#define TYPE_BGR_8             (COLORSPACE_SH(PT_RGB)|CHANNELS_SH(3)|BYTES_SH(1)|DOSWAP_SH(1))
#define TYPE_BGR_8_PLANAR      (COLORSPACE_SH(PT_RGB)|CHANNELS_SH(3)|BYTES_SH(1)|DOSWAP_SH(1)|PLANAR_SH(1))
#define TYPE_RGB_16            (COLORSPACE_SH(PT_RGB)|CHANNELS_SH(3)|BYTES_SH(2))
#define TYPE_RGB_16_PLANAR     (COLORSPACE_SH(PT_RGB)|CHANNELS_SH(3)|BYTES_SH(2)|PLANAR_SH(1))
#define TYPE_RGB_16_SE         (COLORSPACE_SH(PT_RGB)|CHANNELS_SH(3)|BYTES_SH(2)|ENDIAN16_SH(1))
#define TYPE_BGR_16            (COLORSPACE_SH(PT_RGB)|CHANNELS_SH(3)|BYTES_SH(2)|DOSWAP_SH(1))
#define TYPE_BGR_16_PLANAR     (COLORSPACE_SH(PT_RGB)|CHANNELS_SH(3)|BYTES_SH(2)|DOSWAP_SH(1)|PLANAR_SH(1))
#define TYPE_BGR_16_SE         (COLORSPACE_SH(PT_RGB)|CHANNELS_SH(3)|BYTES_SH(2)|DOSWAP_SH(1)|ENDIAN16_SH(1))

#define TYPE_RGBA_8            (COLORSPACE_SH(PT_RGB)|EXTRA_SH(1)|CHANNELS_SH(3)|BYTES_SH(1))
#define TYPE_RGBA_8_PLANAR     (COLORSPACE_SH(PT_RGB)|EXTRA_SH(1)|CHANNELS_SH(3)|BYTES_SH(1)|PLANAR_SH(1))
#define TYPE_RGBA_16           (COLORSPACE_SH(PT_RGB)|EXTRA_SH(1)|CHANNELS_SH(3)|BYTES_SH(2))
#define TYPE_RGBA_16_PLANAR    (COLORSPACE_SH(PT_RGB)|EXTRA_SH(1)|CHANNELS_SH(3)|BYTES_SH(2)|PLANAR_SH(1))
#define TYPE_RGBA_16_SE        (COLORSPACE_SH(PT_RGB)|EXTRA_SH(1)|CHANNELS_SH(3)|BYTES_SH(2)|ENDIAN16_SH(1))

#define TYPE_ARGB_8            (COLORSPACE_SH(PT_RGB)|EXTRA_SH(1)|CHANNELS_SH(3)|BYTES_SH(1)|SWAPFIRST_SH(1))
#define TYPE_ARGB_16           (COLORSPACE_SH(PT_RGB)|EXTRA_SH(1)|CHANNELS_SH(3)|BYTES_SH(2)|SWAPFIRST_SH(1))

#define TYPE_ABGR_8            (COLORSPACE_SH(PT_RGB)|EXTRA_SH(1)|CHANNELS_SH(3)|BYTES_SH(1)|DOSWAP_SH(1))
#define TYPE_ABGR_16           (COLORSPACE_SH(PT_RGB)|EXTRA_SH(1)|CHANNELS_SH(3)|BYTES_SH(2)|DOSWAP_SH(1))
#define TYPE_ABGR_16_PLANAR    (COLORSPACE_SH(PT_RGB)|EXTRA_SH(1)|CHANNELS_SH(3)|BYTES_SH(2)|DOSWAP_SH(1)|PLANAR_SH(1))
#define TYPE_ABGR_16_SE        (COLORSPACE_SH(PT_RGB)|EXTRA_SH(1)|CHANNELS_SH(3)|BYTES_SH(2)|DOSWAP_SH(1)|ENDIAN16_SH(1))

#define TYPE_BGRA_8            (COLORSPACE_SH(PT_RGB)|EXTRA_SH(1)|CHANNELS_SH(3)|BYTES_SH(1)|DOSWAP_SH(1)|SWAPFIRST_SH(1))
#define TYPE_BGRA_16           (COLORSPACE_SH(PT_RGB)|EXTRA_SH(1)|CHANNELS_SH(3)|BYTES_SH(2)|DOSWAP_SH(1)|SWAPFIRST_SH(1))
#define TYPE_BGRA_16_SE        (COLORSPACE_SH(PT_RGB)|EXTRA_SH(1)|CHANNELS_SH(3)|BYTES_SH(2)|ENDIAN16_SH(1)|SWAPFIRST_SH(1))

#define TYPE_CMY_8             (COLORSPACE_SH(PT_CMY)|CHANNELS_SH(3)|BYTES_SH(1))
#define TYPE_CMY_8_PLANAR      (COLORSPACE_SH(PT_CMY)|CHANNELS_SH(3)|BYTES_SH(1)|PLANAR_SH(1))
#define TYPE_CMY_16            (COLORSPACE_SH(PT_CMY)|CHANNELS_SH(3)|BYTES_SH(2))
#define TYPE_CMY_16_PLANAR     (COLORSPACE_SH(PT_CMY)|CHANNELS_SH(3)|BYTES_SH(2)|PLANAR_SH(1))
#define TYPE_CMY_16_SE         (COLORSPACE_SH(PT_CMY)|CHANNELS_SH(3)|BYTES_SH(2)|ENDIAN16_SH(1))

#define TYPE_CMYK_8            (COLORSPACE_SH(PT_CMYK)|CHANNELS_SH(4)|BYTES_SH(1))
#define TYPE_CMYK_8_REV        (COLORSPACE_SH(PT_CMYK)|CHANNELS_SH(4)|BYTES_SH(1)|FLAVOR_SH(1))
#define TYPE_YUVK_8            TYPE_CMYK_8_REV
#define TYPE_CMYK_8_PLANAR     (COLORSPACE_SH(PT_CMYK)|CHANNELS_SH(4)|BYTES_SH(1)|PLANAR_SH(1))
#define TYPE_CMYK_16           (COLORSPACE_SH(PT_CMYK)|CHANNELS_SH(4)|BYTES_SH(2))
#define TYPE_CMYK_16_REV       (COLORSPACE_SH(PT_CMYK)|CHANNELS_SH(4)|BYTES_SH(2)|FLAVOR_SH(1))
#define TYPE_YUVK_16           TYPE_CMYK_16_REV
#define TYPE_CMYK_16_PLANAR    (COLORSPACE_SH(PT_CMYK)|CHANNELS_SH(4)|BYTES_SH(2)|PLANAR_SH(1))
#define TYPE_CMYK_16_SE        (COLORSPACE_SH(PT_CMYK)|CHANNELS_SH(4)|BYTES_SH(2)|ENDIAN16_SH(1))

#define TYPE_KYMC_8            (COLORSPACE_SH(PT_CMYK)|CHANNELS_SH(4)|BYTES_SH(1)|DOSWAP_SH(1))
#define TYPE_KYMC_16           (COLORSPACE_SH(PT_CMYK)|CHANNELS_SH(4)|BYTES_SH(2)|DOSWAP_SH(1))
#define TYPE_KYMC_16_SE        (COLORSPACE_SH(PT_CMYK)|CHANNELS_SH(4)|BYTES_SH(2)|DOSWAP_SH(1)|ENDIAN16_SH(1))

#define TYPE_KCMY_8            (COLORSPACE_SH(PT_CMYK)|CHANNELS_SH(4)|BYTES_SH(1)|SWAPFIRST_SH(1))
#define TYPE_KCMY_8_REV        (COLORSPACE_SH(PT_CMYK)|CHANNELS_SH(4)|BYTES_SH(1)|FLAVOR_SH(1)|SWAPFIRST_SH(1))
#define TYPE_KCMY_16           (COLORSPACE_SH(PT_CMYK)|CHANNELS_SH(4)|BYTES_SH(2)|SWAPFIRST_SH(1))
#define TYPE_KCMY_16_REV       (COLORSPACE_SH(PT_CMYK)|CHANNELS_SH(4)|BYTES_SH(2)|FLAVOR_SH(1)|SWAPFIRST_SH(1))
#define TYPE_KCMY_16_SE        (COLORSPACE_SH(PT_CMYK)|CHANNELS_SH(4)|BYTES_SH(2)|ENDIAN16_SH(1)|SWAPFIRST_SH(1))


// HiFi separations, Thanks to Steven Greaves for providing the code,
// the colorspace is not checked

#define TYPE_CMYKcm_8          (CHANNELS_SH(6)|BYTES_SH(1))
#define TYPE_CMYKcm_8_PLANAR   (CHANNELS_SH(6)|BYTES_SH(1)|PLANAR_SH(1))
#define TYPE_CMYKcm_16         (CHANNELS_SH(6)|BYTES_SH(2))
#define TYPE_CMYKcm_16_PLANAR  (CHANNELS_SH(6)|BYTES_SH(2)|PLANAR_SH(1))
#define TYPE_CMYKcm_16_SE      (CHANNELS_SH(6)|BYTES_SH(2)|ENDIAN16_SH(1))

// Separations with more than 6 channels aren't very standarized,
// Except most start with CMYK and add other colors, so I just used
// then total number of channels after CMYK i.e CMYK8_8

#define TYPE_CMYK7_8           (CHANNELS_SH(7)|BYTES_SH(1))
#define TYPE_CMYK7_16          (CHANNELS_SH(7)|BYTES_SH(2))
#define TYPE_CMYK7_16_SE       (CHANNELS_SH(7)|BYTES_SH(2)|ENDIAN16_SH(1))
#define TYPE_KYMC7_8           (CHANNELS_SH(7)|BYTES_SH(1)|DOSWAP_SH(1))
#define TYPE_KYMC7_16          (CHANNELS_SH(7)|BYTES_SH(2)|DOSWAP_SH(1))
#define TYPE_KYMC7_16_SE       (CHANNELS_SH(7)|BYTES_SH(2)|DOSWAP_SH(1)|ENDIAN16_SH(1))
#define TYPE_CMYK8_8           (CHANNELS_SH(8)|BYTES_SH(1))
#define TYPE_CMYK8_16          (CHANNELS_SH(8)|BYTES_SH(2))
#define TYPE_CMYK8_16_SE       (CHANNELS_SH(8)|BYTES_SH(2)|ENDIAN16_SH(1))
#define TYPE_KYMC8_8           (CHANNELS_SH(8)|BYTES_SH(1)|DOSWAP_SH(1))
#define TYPE_KYMC8_16          (CHANNELS_SH(8)|BYTES_SH(2)|DOSWAP_SH(1))
#define TYPE_KYMC8_16_SE       (CHANNELS_SH(8)|BYTES_SH(2)|DOSWAP_SH(1)|ENDIAN16_SH(1))
#define TYPE_CMYK9_8           (CHANNELS_SH(9)|BYTES_SH(1))
#define TYPE_CMYK9_16          (CHANNELS_SH(9)|BYTES_SH(2))
#define TYPE_CMYK9_16_SE       (CHANNELS_SH(9)|BYTES_SH(2)|ENDIAN16_SH(1))
#define TYPE_KYMC9_8           (CHANNELS_SH(9)|BYTES_SH(1)|DOSWAP_SH(1))
#define TYPE_KYMC9_16          (CHANNELS_SH(9)|BYTES_SH(2)|DOSWAP_SH(1))
#define TYPE_KYMC9_16_SE       (CHANNELS_SH(9)|BYTES_SH(2)|DOSWAP_SH(1)|ENDIAN16_SH(1))
#define TYPE_CMYK10_8          (CHANNELS_SH(10)|BYTES_SH(1))
#define TYPE_CMYK10_16         (CHANNELS_SH(10)|BYTES_SH(2))
#define TYPE_CMYK10_16_SE      (CHANNELS_SH(10)|BYTES_SH(2)|ENDIAN16_SH(1))
#define TYPE_KYMC10_8          (CHANNELS_SH(10)|BYTES_SH(1)|DOSWAP_SH(1))
#define TYPE_KYMC10_16         (CHANNELS_SH(10)|BYTES_SH(2)|DOSWAP_SH(1))
#define TYPE_KYMC10_16_SE      (CHANNELS_SH(10)|BYTES_SH(2)|DOSWAP_SH(1)|ENDIAN16_SH(1))
#define TYPE_CMYK11_8          (CHANNELS_SH(11)|BYTES_SH(1))
#define TYPE_CMYK11_16         (CHANNELS_SH(11)|BYTES_SH(2))
#define TYPE_CMYK11_16_SE      (CHANNELS_SH(11)|BYTES_SH(2)|ENDIAN16_SH(1))
#define TYPE_KYMC11_8          (CHANNELS_SH(11)|BYTES_SH(1)|DOSWAP_SH(1))
#define TYPE_KYMC11_16         (CHANNELS_SH(11)|BYTES_SH(2)|DOSWAP_SH(1))
#define TYPE_KYMC11_16_SE      (CHANNELS_SH(11)|BYTES_SH(2)|DOSWAP_SH(1)|ENDIAN16_SH(1))
#define TYPE_CMYK12_8          (CHANNELS_SH(12)|BYTES_SH(1))
#define TYPE_CMYK12_16         (CHANNELS_SH(12)|BYTES_SH(2))
#define TYPE_CMYK12_16_SE      (CHANNELS_SH(12)|BYTES_SH(2)|ENDIAN16_SH(1))
#define TYPE_KYMC12_8          (CHANNELS_SH(12)|BYTES_SH(1)|DOSWAP_SH(1))
#define TYPE_KYMC12_16         (CHANNELS_SH(12)|BYTES_SH(2)|DOSWAP_SH(1))
#define TYPE_KYMC12_16_SE      (CHANNELS_SH(12)|BYTES_SH(2)|DOSWAP_SH(1)|ENDIAN16_SH(1))

// Colorimetric

#define TYPE_XYZ_16            (COLORSPACE_SH(PT_XYZ)|CHANNELS_SH(3)|BYTES_SH(2))
#define TYPE_Lab_8             (COLORSPACE_SH(PT_Lab)|CHANNELS_SH(3)|BYTES_SH(1))
#define TYPE_ALab_8            (COLORSPACE_SH(PT_Lab)|CHANNELS_SH(3)|BYTES_SH(1)|EXTRA_SH(1)|DOSWAP_SH(1))
#define TYPE_Lab_16            (COLORSPACE_SH(PT_Lab)|CHANNELS_SH(3)|BYTES_SH(2))
#define TYPE_Yxy_16            (COLORSPACE_SH(PT_Yxy)|CHANNELS_SH(3)|BYTES_SH(2))

// YCbCr

#define TYPE_YCbCr_8           (COLORSPACE_SH(PT_YCbCr)|CHANNELS_SH(3)|BYTES_SH(1))
#define TYPE_YCbCr_8_PLANAR    (COLORSPACE_SH(PT_YCbCr)|CHANNELS_SH(3)|BYTES_SH(1)|PLANAR_SH(1))
#define TYPE_YCbCr_16          (COLORSPACE_SH(PT_YCbCr)|CHANNELS_SH(3)|BYTES_SH(2))
#define TYPE_YCbCr_16_PLANAR   (COLORSPACE_SH(PT_YCbCr)|CHANNELS_SH(3)|BYTES_SH(2)|PLANAR_SH(1))
#define TYPE_YCbCr_16_SE       (COLORSPACE_SH(PT_YCbCr)|CHANNELS_SH(3)|BYTES_SH(2)|ENDIAN16_SH(1))

// YUV

#define TYPE_YUV_8           (COLORSPACE_SH(PT_YUV)|CHANNELS_SH(3)|BYTES_SH(1))
#define TYPE_YUV_8_PLANAR    (COLORSPACE_SH(PT_YUV)|CHANNELS_SH(3)|BYTES_SH(1)|PLANAR_SH(1))
#define TYPE_YUV_16          (COLORSPACE_SH(PT_YUV)|CHANNELS_SH(3)|BYTES_SH(2))
#define TYPE_YUV_16_PLANAR   (COLORSPACE_SH(PT_YUV)|CHANNELS_SH(3)|BYTES_SH(2)|PLANAR_SH(1))
#define TYPE_YUV_16_SE       (COLORSPACE_SH(PT_YUV)|CHANNELS_SH(3)|BYTES_SH(2)|ENDIAN16_SH(1))

// HLS

#define TYPE_HLS_8           (COLORSPACE_SH(PT_HLS)|CHANNELS_SH(3)|BYTES_SH(1))
#define TYPE_HLS_8_PLANAR    (COLORSPACE_SH(PT_HLS)|CHANNELS_SH(3)|BYTES_SH(1)|PLANAR_SH(1))
#define TYPE_HLS_16          (COLORSPACE_SH(PT_HLS)|CHANNELS_SH(3)|BYTES_SH(2))
#define TYPE_HLS_16_PLANAR   (COLORSPACE_SH(PT_HLS)|CHANNELS_SH(3)|BYTES_SH(2)|PLANAR_SH(1))
#define TYPE_HLS_16_SE       (COLORSPACE_SH(PT_HLS)|CHANNELS_SH(3)|BYTES_SH(2)|ENDIAN16_SH(1))


// HSV

#define TYPE_HSV_8           (COLORSPACE_SH(PT_HSV)|CHANNELS_SH(3)|BYTES_SH(1))
#define TYPE_HSV_8_PLANAR    (COLORSPACE_SH(PT_HSV)|CHANNELS_SH(3)|BYTES_SH(1)|PLANAR_SH(1))
#define TYPE_HSV_16          (COLORSPACE_SH(PT_HSV)|CHANNELS_SH(3)|BYTES_SH(2))
#define TYPE_HSV_16_PLANAR   (COLORSPACE_SH(PT_HSV)|CHANNELS_SH(3)|BYTES_SH(2)|PLANAR_SH(1))
#define TYPE_HSV_16_SE       (COLORSPACE_SH(PT_HSV)|CHANNELS_SH(3)|BYTES_SH(2)|ENDIAN16_SH(1))

// Named color index. Only 16 bits allowed (don't check colorspace) 

#define TYPE_NAMED_COLOR_INDEX   (CHANNELS_SH(1)|BYTES_SH(2))

// Double values. Painful slow, but sometimes helpful. NOTE THAT BYTES IS SET TO ZERO!

#define TYPE_XYZ_DBL        (COLORSPACE_SH(PT_XYZ)|CHANNELS_SH(3)|BYTES_SH(0))
#define TYPE_Lab_DBL        (COLORSPACE_SH(PT_Lab)|CHANNELS_SH(3)|BYTES_SH(0))
#define TYPE_GRAY_DBL       (COLORSPACE_SH(PT_GRAY)|CHANNELS_SH(1)|BYTES_SH(0))
#define TYPE_RGB_DBL        (COLORSPACE_SH(PT_RGB)|CHANNELS_SH(3)|BYTES_SH(0))
#define TYPE_CMYK_DBL       (COLORSPACE_SH(PT_CMYK)|CHANNELS_SH(4)|BYTES_SH(0))

#endif

// Gamma tables.

typedef struct {
              
    int  nEntries;
    WORD GammaTable[1];

    } GAMMATABLE;

typedef GAMMATABLE FAR* LPGAMMATABLE;


// Sampled curves (1D)

typedef struct {

    int     nItems;
    double* Values;

    } SAMPLEDCURVE;

typedef SAMPLEDCURVE FAR* LPSAMPLEDCURVE;



// Vectors

typedef struct {                // Float Vector

    double n[3];

    } VEC3;

typedef VEC3 FAR* LPVEC3;



typedef struct {                // Matrix
        
    VEC3 v[3];
    
    } MAT3; 

typedef MAT3 FAR* LPMAT3;


// Colorspace values

typedef struct {
    
        double X;
        double Y;
        double Z;

    } cmsCIEXYZ; 
        
typedef cmsCIEXYZ FAR* LPcmsCIEXYZ;

typedef struct {
               
        double x;
        double y;
        double Y;

    } cmsCIExyY;

typedef cmsCIExyY FAR* LPcmsCIExyY;

typedef struct {
               
        double L;
        double a;               
        double b;
               
    } cmsCIELab;

typedef cmsCIELab FAR* LPcmsCIELab;

typedef struct {
               
        double L;
        double C;
        double h;

    } cmsCIELCh;

typedef cmsCIELCh FAR* LPcmsCIELCh;

typedef struct {
               
        double J;
        double C;
        double h;

    } cmsJCh;

typedef cmsJCh FAR* LPcmsJCh;

// Primaries

typedef struct {

        cmsCIEXYZ  Red;
        cmsCIEXYZ  Green;
        cmsCIEXYZ  Blue;

    } cmsCIEXYZTRIPLE;

typedef cmsCIEXYZTRIPLE FAR* LPcmsCIEXYZTRIPLE;


typedef struct {
              
        cmsCIExyY  Red;
        cmsCIExyY  Green;
        cmsCIExyY  Blue;

    } cmsCIExyYTRIPLE;

typedef cmsCIExyYTRIPLE FAR* LPcmsCIExyYTRIPLE;



// Following ICC spec

#define D50X  (0.9642)  
#define D50Y  (1.0)
#define D50Z  (0.8249)

// Does return pointers to constant structs

LCMSAPI LPcmsCIEXYZ LCMSEXPORT cmsD50_XYZ();
LCMSAPI LPcmsCIExyY LCMSEXPORT cmsD50_xyY(); 


// Input/Output

LCMSAPI cmsHPROFILE   LCMSEXPORT cmsOpenProfileFromFile(const char *ICCProfile, const char *sAccess);
LCMSAPI cmsHPROFILE   LCMSEXPORT cmsOpenProfileFromMem(LPVOID MemPtr, DWORD dwSize);
LCMSAPI BOOL          LCMSEXPORT cmsCloseProfile(cmsHPROFILE hProfile);

// Predefined run-time profiles

LCMSAPI cmsHPROFILE   LCMSEXPORT cmsCreateRGBProfile(LPcmsCIExyY WhitePoint,
                                        LPcmsCIExyYTRIPLE Primaries,
                                        LPGAMMATABLE TransferFunction[3]);

LCMSAPI cmsHPROFILE   LCMSEXPORT cmsCreateGrayProfile(LPcmsCIExyY WhitePoint,
                                              LPGAMMATABLE TransferFunction);

LCMSAPI cmsHPROFILE LCMSEXPORT cmsCreateLinearizationDeviceLink(icColorSpaceSignature ColorSpace,
                                                        LPGAMMATABLE TransferFunctions[]);

LCMSAPI cmsHPROFILE LCMSEXPORT cmsCreateInkLimitingDeviceLink(icColorSpaceSignature ColorSpace,
                                                      double Limit);


LCMSAPI cmsHPROFILE   LCMSEXPORT cmsCreateLabProfile(LPcmsCIExyY WhitePoint);
LCMSAPI cmsHPROFILE   LCMSEXPORT cmsCreateXYZProfile(void);
LCMSAPI cmsHPROFILE   LCMSEXPORT cmsCreate_sRGBProfile(void);


// Colorimetric space conversions

LCMSAPI void          LCMSEXPORT cmsXYZ2xyY(LPcmsCIExyY Dest, const LPcmsCIEXYZ Source);
LCMSAPI void          LCMSEXPORT cmsxyY2XYZ(LPcmsCIEXYZ Dest, const LPcmsCIExyY Source);
LCMSAPI void          LCMSEXPORT cmsXYZ2Lab(LPcmsCIEXYZ WhitePoint, LPcmsCIELab Lab, const LPcmsCIEXYZ xyz);
LCMSAPI void          LCMSEXPORT cmsLab2XYZ(LPcmsCIEXYZ WhitePoint, LPcmsCIEXYZ xyz, const LPcmsCIELab Lab);
LCMSAPI void          LCMSEXPORT cmsLab2LCh(LPcmsCIELCh LCh, const LPcmsCIELab Lab);
LCMSAPI void          LCMSEXPORT cmsLCh2Lab(LPcmsCIELab Lab, const LPcmsCIELCh LCh);


// CIELab handling

LCMSAPI double        LCMSEXPORT cmsDeltaE(LPcmsCIELab Lab1, LPcmsCIELab Lab2);
LCMSAPI double        LCMSEXPORT cmsCIE94DeltaE(LPcmsCIELab Lab1, LPcmsCIELab Lab2);
LCMSAPI double        LCMSEXPORT cmsBFDdeltaE(LPcmsCIELab Lab1, LPcmsCIELab Lab2);
LCMSAPI double        LCMSEXPORT cmsCMCdeltaE(LPcmsCIELab Lab1, LPcmsCIELab Lab2);
LCMSAPI double        LCMSEXPORT cmsCIE2000DeltaE(LPcmsCIELab Lab1, LPcmsCIELab Lab2, double Kl, double Kc, double Kh);

LCMSAPI void          LCMSEXPORT cmsClampLab(LPcmsCIELab Lab, double amax, double amin, double bmax, double bmin);

LCMSAPI BOOL          LCMSEXPORT cmsWhitePointFromTemp(int TempK, LPcmsCIExyY WhitePoint);

LCMSAPI BOOL          LCMSEXPORT cmsAdaptToIlluminant(LPcmsCIEXYZ Result,
                                                        LPcmsCIEXYZ SourceWhitePt,
                                                        LPcmsCIEXYZ Illuminant,
                                                        LPcmsCIEXYZ Value);

LCMSAPI BOOL          LCMSEXPORT cmsBuildRGB2XYZtransferMatrix(LPMAT3 r,
                                                        LPcmsCIExyY WhitePoint,
                                                        LPcmsCIExyYTRIPLE Primaries);

// CIECAM97s

#define AVG_SURROUND_4     0
#define AVG_SURROUND       1
#define DIM_SURROUND       2
#define DARK_SURROUND      3
#define CUTSHEET_SURROUND  4

#define D_CALCULATE             (-1)
#define D_CALCULATE_DISCOUNT    (-2)

typedef struct {

              cmsCIEXYZ whitePoint;
              double    Yb;
              double    La;
              int       surround;
              double    D_value;

    } cmsViewingConditions;

typedef cmsViewingConditions FAR* LPcmsViewingConditions;


LCMSAPI LCMSHANDLE    LCMSEXPORT cmsCIECAM97sInit(LPcmsViewingConditions pVC2);
LCMSAPI void          LCMSEXPORT cmsCIECAM97sDone(LCMSHANDLE hModel);
LCMSAPI void          LCMSEXPORT cmsCIECAM97sForward(LCMSHANDLE hModel, LPcmsCIEXYZ pIn, LPcmsJCh pOut);
LCMSAPI void          LCMSEXPORT cmsCIECAM97sReverse(LCMSHANDLE hModel, LPcmsJCh pIn,    LPcmsCIEXYZ pOut);

// Gamma

LCMSAPI LPGAMMATABLE  LCMSEXPORT cmsBuildGamma(int nEntries, double Gamma);
LCMSAPI LPGAMMATABLE  LCMSEXPORT cmsBuildParametricGamma(int nEntries, int Type, double Params[]);
LCMSAPI LPGAMMATABLE  LCMSEXPORT cmsAllocGamma(int nEntries);
LCMSAPI void          LCMSEXPORT cmsFreeGamma(LPGAMMATABLE Gamma);
LCMSAPI void          LCMSEXPORT cmsFreeGammaTriple(LPGAMMATABLE Gamma[3]);
LCMSAPI LPGAMMATABLE  LCMSEXPORT cmsDupGamma(LPGAMMATABLE Src);
LCMSAPI LPGAMMATABLE  LCMSEXPORT cmsReverseGamma(int nResultSamples, LPGAMMATABLE InGamma);
LCMSAPI LPGAMMATABLE  LCMSEXPORT cmsJoinGamma(LPGAMMATABLE InGamma,  LPGAMMATABLE OutGamma);
LCMSAPI LPGAMMATABLE  LCMSEXPORT cmsJoinGammaEx(LPGAMMATABLE InGamma,  LPGAMMATABLE OutGamma, int nPoints);
LCMSAPI BOOL          LCMSEXPORT cmsSmoothGamma(LPGAMMATABLE Tab, double lambda);
LCMSAPI double        LCMSEXPORT cmsEstimateGamma(LPGAMMATABLE t);
LCMSAPI double        LCMSEXPORT cmsEstimateGammaEx(LPWORD Table, int nEntries, double Thereshold); 
LCMSAPI LPGAMMATABLE  LCMSEXPORT cmsReadICCGamma(cmsHPROFILE hProfile, icTagSignature sig);
LCMSAPI LPGAMMATABLE  LCMSEXPORT cmsReadICCGammaReversed(cmsHPROFILE hProfile, icTagSignature sig);



// Access to Profile data.

LCMSAPI void          LCMSEXPORT cmsSetLanguage(int LanguageCode, int CountryCode);
LCMSAPI BOOL          LCMSEXPORT cmsTakeMediaWhitePoint(LPcmsCIEXYZ Dest, cmsHPROFILE hProfile);
LCMSAPI BOOL          LCMSEXPORT cmsTakeMediaBlackPoint(LPcmsCIEXYZ Dest, cmsHPROFILE hProfile);
LCMSAPI BOOL          LCMSEXPORT cmsTakeIluminant(LPcmsCIEXYZ Dest, cmsHPROFILE hProfile);
LCMSAPI BOOL          LCMSEXPORT cmsTakeColorants(LPcmsCIEXYZTRIPLE Dest, cmsHPROFILE hProfile);
LCMSAPI const char*   LCMSEXPORT cmsTakeProductName(cmsHPROFILE hProfile);
LCMSAPI const char*   LCMSEXPORT cmsTakeProductDesc(cmsHPROFILE hProfile);
LCMSAPI const char*   LCMSEXPORT cmsTakeProductInfo(cmsHPROFILE hProfile);
LCMSAPI BOOL          LCMSEXPORT cmsIsTag(cmsHPROFILE hProfile, icTagSignature sig);
LCMSAPI int           LCMSEXPORT cmsTakeRenderingIntent(cmsHPROFILE hProfile);

LCMSAPI BOOL          LCMSEXPORT cmsTakeCharTargetData(cmsHPROFILE hProfile, char** Data, size_t* len);
                                                   
// Translate form our notation to ICC 
LCMSAPI icColorSpaceSignature LCMSEXPORT _cmsICCcolorSpace(int OurNotation);
LCMSAPI                   int LCMSEXPORT _cmsChannelsOf(icColorSpaceSignature ColorSpace);

#define LCMS_USED_AS_INPUT      0
#define LCMS_USED_AS_OUTPUT     1
#define LCMS_USED_AS_PROOF      2

LCMSAPI BOOL         LCMSEXPORT cmsIsIntentSupported(cmsHPROFILE hProfile, int Intent, int UsedDirection);

LCMSAPI icColorSpaceSignature   LCMSEXPORT cmsGetPCS(cmsHPROFILE hProfile);
LCMSAPI icColorSpaceSignature   LCMSEXPORT cmsGetColorSpace(cmsHPROFILE hProfile);
LCMSAPI icProfileClassSignature LCMSEXPORT cmsGetDeviceClass(cmsHPROFILE hProfile);


LCMSAPI void          LCMSEXPORT cmsSetDeviceClass(cmsHPROFILE hProfile, icProfileClassSignature sig);
LCMSAPI void          LCMSEXPORT cmsSetColorSpace(cmsHPROFILE hProfile, icColorSpaceSignature sig);
LCMSAPI void          LCMSEXPORT cmsSetPCS(cmsHPROFILE hProfile, icColorSpaceSignature pcs);

// Intents

#define INTENT_PERCEPTUAL                 0
#define INTENT_RELATIVE_COLORIMETRIC      1
#define INTENT_SATURATION                 2
#define INTENT_ABSOLUTE_COLORIMETRIC      3

// Flags

#define cmsFLAGS_MATRIXINPUT              0x0001
#define cmsFLAGS_MATRIXOUTPUT             0x0002
#define cmsFLAGS_MATRIXONLY               (cmsFLAGS_MATRIXINPUT|cmsFLAGS_MATRIXOUTPUT)

#define cmsFLAGS_NOPRELINEARIZATION		  0x0010	// Don't create prelinearization tables
									                // on precalculated transforms (internal use)					

#define cmsFLAGS_NOTPRECALC               0x0100    
#define cmsFLAGS_NULLTRANSFORM            0x0200    // Don't transform anyway
#define cmsFLAGS_HIGHRESPRECALC           0x0400    // Use more memory to give better accurancy
#define cmsFLAGS_LOWRESPRECALC            0x0800    // Use less memory to minimize resouces


#define cmsFLAGS_WHITEBLACKCOMPENSATION   0x2000    // Matches black and white on precalculated transforms

// Proofing flags

#define cmsFLAGS_GAMUTCHECK               0x1000    // Out of Gamut alarm
#define cmsFLAGS_SOFTPROOFING             0x4000    // Do softproofing




// Transforms

LCMSAPI cmsHTRANSFORM LCMSEXPORT cmsCreateTransform(cmsHPROFILE Input,
                                               DWORD InputFormat,
                                               cmsHPROFILE Output,
                                               DWORD OutputFormat,
                                               int Intent,
                                               DWORD dwFlags);

LCMSAPI cmsHTRANSFORM LCMSEXPORT cmsCreateProofingTransform(cmsHPROFILE Input,
                                               DWORD InputFormat,
                                               cmsHPROFILE Output,
                                               DWORD OutputFormat,
                                               cmsHPROFILE Proofing,
                                               int Intent,
                                               int ProofingIntent,
                                               DWORD dwFlags);

LCMSAPI cmsHTRANSFORM LCMSEXPORT cmsCreateMultiprofileTransform(cmsHPROFILE hProfiles[],
                                                                int nProfiles,
                                                                DWORD InputFormat,
                                                                DWORD OutputFormat,
                                                                int Intent,
                                                                DWORD dwFlags);

LCMSAPI void         LCMSEXPORT cmsDeleteTransform(cmsHTRANSFORM hTransform);

LCMSAPI void         LCMSEXPORT cmsDoTransform(cmsHTRANSFORM Transform,
                                                 LPVOID InputBuffer,
                                                 LPVOID OutputBuffer,
                                                 unsigned int Size);

LCMSAPI void         LCMSEXPORT cmsChangeBuffersFormat(cmsHTRANSFORM hTransform, DWORD InputFormat, DWORD dwOutputFormat);

LCMSAPI void         LCMSEXPORT cmsSetAlarmCodes(int r, int g, int b);
LCMSAPI void         LCMSEXPORT cmsGetAlarmCodes(int *r, int *g, int *b);


// Named color support

LCMSAPI int  LCMSEXPORT cmsNamedColorCount(cmsHTRANSFORM xform);
LCMSAPI BOOL LCMSEXPORT cmsNamedColorInfo(cmsHTRANSFORM xform, int nColor, char* Name, char* Prefix, char* Suffix);
LCMSAPI int  LCMSEXPORT cmsNamedColorIndex(cmsHTRANSFORM xform, const char* Name);

// Profile creation 

LCMSAPI BOOL LCMSEXPORT cmsAddTag(cmsHPROFILE hProfile, icTagSignature sig, void* data);

// Converts a transform to a devicelink profile
LCMSAPI cmsHPROFILE LCMSEXPORT cmsTransform2DeviceLink(cmsHTRANSFORM hTransform, DWORD dwFlags);


// Save profile
LCMSAPI BOOL LCMSEXPORT _cmsSaveProfile(cmsHPROFILE hProfile, const char* FileName);
LCMSAPI BOOL LCMSEXPORT _cmsSaveProfileToMem(cmsHPROFILE hProfile, void *MemPtr, 
																size_t* BytesNeeded);



// PostScript ColorRenderingDictionary and ColorSpaceArray

LCMSAPI DWORD LCMSEXPORT cmsGetPostScriptCSA(cmsHPROFILE hProfile, int Intent, LPVOID Buffer, DWORD dwBufferLen);
LCMSAPI DWORD LCMSEXPORT cmsGetPostScriptCRD(cmsHPROFILE hProfile, int Intent, LPVOID Buffer, DWORD dwBufferLen);


// Error handling

#define LCMS_ERROR_ABORT    0
#define LCMS_ERROR_SHOW     1
#define LCMS_ERROR_IGNORE   2

LCMSAPI int LCMSEXPORT cmsErrorAction(int nAction);

#define LCMS_ERRC_WARNING        0x1000
#define LCMS_ERRC_RECOVERABLE    0x2000
#define LCMS_ERRC_ABORTED        0x3000

void cdecl cmsSignalError(int ErrorCode, const char *ErrorText, ...);


// LUT manipulation


typedef struct _lcms_LUT_struc LUT, FAR* LPLUT; // opaque pointer

LCMSAPI LPLUT LCMSEXPORT cmsAllocLUT(void);
LCMSAPI LPLUT LCMSEXPORT cmsAllocLinearTable(LPLUT NewLUT, LPGAMMATABLE Tables[], int nTable);
LCMSAPI LPLUT LCMSEXPORT cmsAlloc3DGrid(LPLUT Lut, int clutPoints, int inputChan, int outputChan);
LCMSAPI LPLUT LCMSEXPORT cmsSetMatrixLUT(LPLUT Lut, LPMAT3 M);
LCMSAPI void  LCMSEXPORT cmsFreeLUT(LPLUT Lut);
LCMSAPI void  LCMSEXPORT cmsEvalLUT(LPLUT Lut, WORD In[], WORD Out[]);
LCMSAPI LPLUT LCMSEXPORT cmsReadICCLut(cmsHPROFILE hProfile, icTagSignature sig);
LCMSAPI LPLUT LCMSEXPORT cmsDupLUT(LPLUT Orig);

// LUT Sampling

typedef int (* _cmsSAMPLER)(register WORD In[],
                            register WORD Out[],
                            register LPVOID Cargo);

#define SAMPLER_HASTL1      LUT_HASTL1
#define SAMPLER_HASTL2      LUT_HASTL2
#define SAMPLER_INSPECT     0x01000000

LCMSAPI int LCMSEXPORT cmsSample3DGrid(LPLUT Lut, _cmsSAMPLER Sampler, LPVOID Cargo, DWORD dwFlags);

// Formatters

typedef unsigned char* (* cmsFORMATTER)(register void* CMMcargo,
										register WORD ToUnroll[],
										register LPBYTE Buffer);

LCMSAPI void LCMSEXPORT cmsSetUserFormatters(cmsHTRANSFORM hTransform, cmsFORMATTER Input, cmsFORMATTER Output);
LCMSAPI void LCMSEXPORT cmsGetUserFormatters(cmsHTRANSFORM hTransform, cmsFORMATTER* Input, cmsFORMATTER* Output);


// ***************************************************************************
// End of Little cms API From here functions are private
// You can use them only if using static libraries, and at your own risk of
// be stripped or changed at futures releases.

#ifndef LCMS_APIONLY


// Compatibility with anterior versions-- not needed anymore
//  -- Morge

LCMSAPI void          LCMSEXPORT cmsLabEncoded2Float(LPcmsCIELab Lab, const WORD wLab[3]);
LCMSAPI void          LCMSEXPORT cmsFloat2LabEncoded(WORD wLab[3], const LPcmsCIELab Lab);
LCMSAPI void          LCMSEXPORT cmsXYZEncoded2Float(LPcmsCIEXYZ fxyz, const WORD XYZ[3]);
LCMSAPI void          LCMSEXPORT cmsFloat2XYZEncoded(WORD XYZ[3], const LPcmsCIEXYZ fXYZ);


// Profiling Extensions --- Would be removed from API in future revisions

LCMSAPI BOOL LCMSEXPORT _cmsAddTextTag(cmsHPROFILE hProfile,  icTagSignature sig, const char* Text);
LCMSAPI BOOL LCMSEXPORT _cmsAddXYZTag(cmsHPROFILE hProfile,   icTagSignature sig, const LPcmsCIEXYZ XYZ);
LCMSAPI BOOL LCMSEXPORT _cmsAddLUTTag(cmsHPROFILE hProfile,   icTagSignature sig, void* lut);
LCMSAPI BOOL LCMSEXPORT _cmsAddGammaTag(cmsHPROFILE hProfile, icTagSignature sig, LPGAMMATABLE TransferFunction);
LCMSAPI BOOL LCMSEXPORT _cmsAddChromaticityTag(cmsHPROFILE hProfile, icTagSignature sig, LPcmsCIExyYTRIPLE Chrm);

//  -- end of morge


// Alignment handling (needed in ReadLUT16 and ReadLUT8)

typedef struct {
        icS15Fixed16Number a;
        icUInt16Number     b;

       } _cmsTestAlign16;

#define SIZEOF_UINT16_ALIGNED (sizeof(_cmsTestAlign16) - sizeof(icS15Fixed16Number))

typedef struct {
        icS15Fixed16Number a;
        icUInt8Number      b;

       } _cmsTestAlign8;

#define SIZEOF_UINT8_ALIGNED (sizeof(_cmsTestAlign8) - sizeof(icS15Fixed16Number))


// Fixed point

typedef icInt32Number Fixed32;       // Fixed 15.16 whith sign

#define INT_TO_FIXED(x)         ((x)<<16)
#define DOUBLE_TO_FIXED(x)      ((Fixed32) ((x)*65536.0+.5))
#define FIXED_TO_INT(x)         ((x)>>16)
#define FIXED_REST_TO_INT(x)    ((x)&0xFFFFU)
#define FIXED_TO_DOUBLE(x)      (((double)x)/65536.0)
#define ROUND_FIXED_TO_INT(x)   (((x)+0x8000)>>16)


Fixed32 cdecl FixedMul(Fixed32 a, Fixed32 b);
Fixed32 cdecl FixedDiv(Fixed32 a, Fixed32 b);
Fixed32 cdecl ToFixedDomain(int a);              // (a * 65536.0 / 65535.0)
int     cdecl FromFixedDomain(Fixed32 a);        // (a * 65535.0 + .5)
Fixed32 cdecl FixedLERP(Fixed32 a, Fixed32 l, Fixed32 h);
WORD    cdecl FixedScale(WORD a, Fixed32 s);

// Vector & Matrix operations. I'm using the notation frequently found in
// literature. Mostly 'Graphic Gems' samples. Not to be same routines.

// Vector members

#define VX      0
#define VY      1
#define VZ      2

typedef struct {                // Fixed 15.16 bits vector
        Fixed32 n[3];
        } WVEC3, FAR* LPWVEC3;

typedef struct {                // Matrix (Fixed 15.16)
        WVEC3 v[3];
        } WMAT3, FAR* LPWMAT3;



void cdecl VEC3init(LPVEC3 r, double x, double y, double z);   // double version
void cdecl VEC3initF(LPWVEC3 r, double x, double y, double z); // Fix32 version
void cdecl VEC3toFix(LPWVEC3 r, LPVEC3 v);
void cdecl VEC3fromFix(LPVEC3 r, LPWVEC3 v);
void cdecl VEC3scaleFix(LPWORD r, LPWVEC3 Scale);
void cdecl VEC3swap(LPVEC3 a, LPVEC3 b);
void cdecl VEC3divK(LPVEC3 r, LPVEC3 v, double d);
void cdecl VEC3perK(LPVEC3 r, LPVEC3 v, double d);
void cdecl VEC3minus(LPVEC3 r, LPVEC3 a, LPVEC3 b);
void cdecl VEC3perComp(LPVEC3 r, LPVEC3 a, LPVEC3 b);
BOOL cdecl VEC3equal(LPWVEC3 a, LPWVEC3 b, double Tolerance);

void cdecl MAT3identity(LPMAT3 a);
void cdecl MAT3per(LPMAT3 r, LPMAT3 a, LPMAT3 b);
void cdecl MAT3perK(LPMAT3 r, LPMAT3 v, double d);
int  cdecl MAT3inverse(LPMAT3 a, LPMAT3 b);
void cdecl MAT3eval(LPVEC3 r, LPMAT3 a, LPVEC3 v);
void cdecl MAT3toFix(LPWMAT3 r, LPMAT3 v);
void cdecl MAT3fromFix(LPMAT3 r, LPWMAT3 v);
void cdecl MAT3evalW(LPWVEC3 r, LPWMAT3 a, LPWVEC3 v);
BOOL cdecl MAT3isIdentity(LPWMAT3 a, double Tolerance);


// Is a table linear?

int  cdecl cmsIsLinear(WORD Table[], int nEntries);

// I hold this structures describing domain
// details mainly for optimization purposes.


typedef struct {              // Used on 16 bits interpolations

               int nSamples;       // Valid on all kinds of tables
               int nInputs;        // != 1 only in 3D interpolation
               int nOutputs;       // != 1 only in 3D interpolation

               WORD Domain;

               int opta1, opta2;
               int opta3, opta4;     // Optimization for 3D LUT
               int opta5, opta6;
               int opta7, opta8;


               } L16PARAMS, *LPL16PARAMS;


void    cdecl cmsCalcL16Params(int nSamples, LPL16PARAMS p);
void    cdecl cmsCalcCLUT16Params(int nSamples, int InputChan, int OutputChan, LPL16PARAMS p);
WORD    cdecl cmsLinearInterpLUT16(WORD Value, WORD LutTable[], LPL16PARAMS p);
Fixed32 cdecl cmsLinearInterpFixed(WORD Value1, WORD LutTable[], LPL16PARAMS p);
WORD    cdecl cmsReverseLinearInterpLUT16(WORD Value, WORD LutTable[], LPL16PARAMS p);

void cdecl cmsTrilinearInterp16(WORD Input[],
                                WORD Output[],
                                WORD LutTable[],
                                LPL16PARAMS p);

void cdecl cmsTetrahedralInterp16(WORD Input[],
                                  WORD Output[],
                                  WORD LutTable[], LPL16PARAMS p);

// LUT handling

#define LUT_HASMATRIX       0x0001        // Do-op Flags
#define LUT_HASTL1          0x0002
#define LUT_HASTL2          0x0008
#define LUT_HAS3DGRID       0x0010

// New in rev 4.0 of ICC spec

#define LUT_HASMATRIX3     0x0020   // Matrix for LutAToB
#define LUT_HASMATRIX3OFS  0x0040   // Offset for LutAToB

#define LUT_HASTL3         0x0080   // 'M' curves for LutAToB
#define LUT_HASTL4         0x0100   // 'M' curves for LutBToA

#define LUT_HASMATRIX4     0x0200   // Matrix for LutBToA
#define LUT_HASMATRIX4OFS  0x0400   // Offset for LutBToA



#define MAXCHANNELS  16            // Maximum number of channels

struct _lcms_LUT_struc {

               DWORD wFlags;
               WMAT3 Matrix;                    // 15fixed16 matrix

               unsigned int InputChan;
               unsigned int OutputChan;
               unsigned int InputEntries;
               unsigned int OutputEntries;
               unsigned int cLutPoints;

               
               LPWORD L1[MAXCHANNELS];          // First linearization
               LPWORD L2[MAXCHANNELS];          // Last linearization

               LPWORD T;                        // 3D CLUT
               unsigned int Tsize;              // CLUT size in bytes

              // Parameters & Optimizations

               L16PARAMS In16params;
               L16PARAMS Out16params;
               L16PARAMS CLut16params;

               int Intent;                       // Accomplished intent

               // New for Rev 4.0 of spec (reserved)

               WMAT3 Mat3;
               WVEC3 Ofs3;
               LPWORD L3[MAXCHANNELS];
               LPWORD L4[MAXCHANNELS];
               WMAT3 Mat4;
               WVEC3 Ofs4;

               L16PARAMS L3params;
               L16PARAMS L4params;

               unsigned int L3Entries;
               unsigned int L4Entries;

               }; // LUT, FAR* LPLUT;


BOOL         cdecl _cmsSmoothEndpoints(LPWORD Table, int nEntries);

// Sampled curves

LPSAMPLEDCURVE cdecl cmsAllocSampledCurve(int nItems);
void           cdecl cmsFreeSampledCurve(LPSAMPLEDCURVE p);
LPSAMPLEDCURVE cdecl cmsDupSampledCurve(LPSAMPLEDCURVE p);

LPSAMPLEDCURVE cdecl cmsConvertGammaToSampledCurve(LPGAMMATABLE Gamma, int nPoints);
LPGAMMATABLE   cdecl cmsConvertSampledCurveToGamma(LPSAMPLEDCURVE Sampled, double Max);

void           cdecl cmsEndpointsOfSampledCurve(LPSAMPLEDCURVE p, double* Min, double* Max);
void           cdecl cmsClampSampledCurve(LPSAMPLEDCURVE p, double Min, double Max);
BOOL           cdecl cmsSmoothSampledCurve(LPSAMPLEDCURVE Tab, double SmoothingLambda);
void           cdecl cmsRescaleSampledCurve(LPSAMPLEDCURVE p, double Min, double Max, int nPoints);

LPSAMPLEDCURVE cdecl cmsJoinSampledCurves(LPSAMPLEDCURVE X, LPSAMPLEDCURVE Y, int nResultingPoints);

// Shaper/Matrix handling

#define MATSHAPER_HASMATRIX        0x0001        // Do-ops flags
#define MATSHAPER_HASSHAPER        0x0002
#define MATSHAPER_INPUT            0x0004        // Behaviour
#define MATSHAPER_OUTPUT           0x0008
#define MATSHAPER_HASINPSHAPER     0x0010
#define MATSHAPER_ALLSMELTED       (MATSHAPER_INPUT|MATSHAPER_OUTPUT)


typedef struct {
               DWORD dwFlags;

               WMAT3 Matrix;

               L16PARAMS p16;       // Primary curve
               LPWORD L[3];
               
               L16PARAMS p2_16;     // Secondary curve (used as input in smelted ones)
               LPWORD L2[3];

               } MATSHAPER, FAR* LPMATSHAPER;

LPMATSHAPER cdecl cmsAllocMatShaper(LPMAT3 matrix, LPGAMMATABLE Shaper[], DWORD Behaviour);
LPMATSHAPER cdecl cmsAllocMatShaper2(LPMAT3 matrix, LPGAMMATABLE In[], LPGAMMATABLE Out[], DWORD Behaviour);

void        cdecl cmsFreeMatShaper(LPMATSHAPER MatShaper);
void        cdecl cmsEvalMatShaper(LPMATSHAPER MatShaper, WORD In[], WORD Out[]);

BOOL         cdecl cmsReadICCMatrixRGB2XYZ(LPMAT3 r, cmsHPROFILE hProfile);

LPMATSHAPER  cdecl cmsBuildInputMatrixShaper(cmsHPROFILE InputProfile, LPDWORD dwFlags);
LPMATSHAPER  cdecl cmsBuildOutputMatrixShaper(cmsHPROFILE OutputProfile, LPDWORD dwFlags);



// White Point & Primary chromas handling
BOOL cdecl cmsAdaptationMatrix(LPMAT3 r, LPcmsCIEXYZ FromIll, LPcmsCIEXYZ ToIll);
BOOL cdecl cmsAdaptMatrixToD50(LPMAT3 r, LPcmsCIExyY SourceWhitePt);
BOOL cdecl cmsAdaptMatrixFromD50(LPMAT3 r, LPcmsCIExyY DestWhitePt);

// Inter-PCS conversion routines. They assume D50 as white point.
void cdecl cmsXYZ2LabEncoded(WORD XYZ[3], WORD Lab[3]);
void cdecl cmsLab2XYZEncoded(WORD Lab[3], WORD XYZ[3]);

// Retrieve text representation of WP
void cdecl _cmsIdentifyWhitePoint(char *Buffer, LPcmsCIEXYZ WhitePt);

// Quantize to WORD in a (MaxSamples - 1) domain
WORD cdecl _cmsQuantizeVal(double i, int MaxSamples);

// Named color support
typedef struct {                
                char Name[MAX_PATH];
                WORD PCS[3];
                WORD DeviceColorant[MAXCHANNELS];
                

        } cmsNAMEDCOLOR, FAR* LPcmsNAMEDCOLOR;

typedef struct {
                int nColors;                
                int Allocated;
                int ColorantCount;              
                LPcmsNAMEDCOLOR List;
                char Prefix[33];
                char Suffix[33];

        } cmsNAMEDCOLORLIST, FAR* LPcmsNAMEDCOLORLIST;

LPcmsNAMEDCOLORLIST  cdecl cmsAllocNamedColorList();
int                  cdecl cmsReadICCnamedColorList(cmsHTRANSFORM xform, cmsHPROFILE hProfile, icTagSignature sig);
void                 cdecl cmsFreeNamedColorList(LPcmsNAMEDCOLORLIST List);
BOOL                 cdecl cmsAppendNamedColor(cmsHTRANSFORM xform, const char* Name, WORD PCS[3], WORD Colorant[MAXCHANNELS]);


// I/O

#define MAX_TABLE_TAG       50

// This is the internal struct holding profile details.

typedef struct {

              void*           stream;   // Associated stream. If NULL,
                                        // tags are supposed to be in
                                        // memory rather than in a file.

               // Only most important items found in ICC profile

               icProfileClassSignature DeviceClass;
               icColorSpaceSignature   ColorSpace;
               icColorSpaceSignature   PCS;
               icRenderingIntent       RenderingIntent;
               icUInt32Number          flags;
               cmsCIEXYZ               Illuminant;

               // Dictionary

               icInt32Number   TagCount;
               icTagSignature  TagNames[MAX_TABLE_TAG];
               size_t          TagSizes[MAX_TABLE_TAG];
               size_t          TagOffsets[MAX_TABLE_TAG];
               LPVOID          TagPtrs[MAX_TABLE_TAG];


               char   PhysicalFile[MAX_PATH];
               
               BOOL   IsWrite;

               // I/O handlers

               size_t (* Read)(void *buffer, size_t size, size_t count, void *stream);
               
               BOOL   (* Seek)(void* stream, size_t offset);
               BOOL   (* Close)(void* stream);
               
               // Writting

               BOOL   (* Write)(void* stream, size_t size, LPVOID Ptr);
             
              } LCMSICCPROFILE, FAR* LPLCMSICCPROFILE;



// These macros unpack format specifiers into integers

#define T_COLORSPACE(s)       (((s)>>16)&31)
#define T_SWAPFIRST(s)        (((s)>>14)&1)
#define T_FLAVOR(s)           (((s)>>13)&1)
#define T_PLANAR(p)           (((p)>>12)&1)
#define T_ENDIAN16(e)         (((e)>>11)&1)
#define T_DOSWAP(e)           (((e)>>10)&1)
#define T_EXTRA(e)            (((e)>>7)&7)
#define T_CHANNELS(c)         (((c)>>3)&15)
#define T_BYTES(b)            ((b)&7)



// Internal XFORM struct
struct _cmstransform_struct;

// Full xform
typedef void (* _cmsCOLORCALLBACKFN)(struct _cmstransform_struct *Transform,
                               LPVOID InputBuffer,
                               LPVOID OutputBuffer, unsigned int Size);

// intermediate pass, from WORD[] to WORD[]

typedef void   (* _cmsADJFN)(WORD In[], WORD Out[], LPWMAT3 m, LPWVEC3 b);

typedef void   (* _cmsTRANSFN)(struct _cmstransform_struct *Transform,
                               WORD In[], WORD Out[]);

typedef void   (* _cmsCNVRT)(WORD In[], WORD Out[]);

typedef LPBYTE (* _cmsFIXFN)(register struct _cmstransform_struct *info,
                             register WORD ToUnroll[],
                             register LPBYTE Buffer);



// Transformation
typedef struct _cmstransform_struct {

                            // Keep formats for further reference
                            DWORD InputFormat, OutputFormat;
                           
                            DWORD StrideIn, StrideOut;      // Planar support

                            int Intent, ProofIntent;
                            int DoGamutCheck;

                            cmsHPROFILE InputProfile;
                            cmsHPROFILE OutputProfile;
                            cmsHPROFILE PreviewProfile;

                            icColorSpaceSignature EntryColorSpace;
                            icColorSpaceSignature ExitColorSpace;
                        
                            WMAT3 m1, m2;       // Matrix holding inter PCS operation
                            WVEC3 of1, of2;     // Offset terms

                            _cmsCOLORCALLBACKFN xform;

                            // Steps in xFORM

                            _cmsFIXFN   FromInput;
                            _cmsTRANSFN FromDevice;
                            _cmsADJFN   Stage1;
                            _cmsADJFN   Stage2;
                            _cmsTRANSFN ToDevice;
                            _cmsFIXFN   ToOutput;

                            // LUTs

                            LPLUT Device2PCS;
                            LPLUT PCS2Device;
                            LPLUT Gamut;         // Gamut check
                            LPLUT Preview;       // Preview (Proof)

                            LPLUT DeviceLink;    // Precalculated grid -
                                                 // device link profile

                            // Matrix/Shapers

                            LPMATSHAPER InMatShaper;
                            LPMATSHAPER OutMatShaper;
                            LPMATSHAPER SmeltMatShaper;


                            // Phase of Lab/XYZ, Abs/Rel

                            int Phase1, Phase2, Phase3;

                            // Named color table
                            LPcmsNAMEDCOLORLIST NamedColorList;
                    

                            } _cmsTRANSFORM,FAR *_LPcmsTRANSFORM;



// Packing & Unpacking

_cmsFIXFN cdecl _cmsIdentifyInputFormat(_LPcmsTRANSFORM xform,  DWORD dwInput);
_cmsFIXFN cdecl _cmsIdentifyOutputFormat(_LPcmsTRANSFORM xform, DWORD dwOutput);


// Conversion

#define XYZRel       0
#define LabRel       1
#define XYZAbs       2
#define LabAbs       3


int cdecl cmsChooseCnvrt(int Absolute,
                 int Phase1, LPcmsCIEXYZ BlackPointIn,
                             LPcmsCIEXYZ WhitePointIn,
                             LPcmsCIEXYZ IlluminantIn,

                 int Phase2, LPcmsCIEXYZ BlackPointOut,
                             LPcmsCIEXYZ WhitePointOut,
                             LPcmsCIEXYZ IlluminantOut,

                 _cmsADJFN *fn1,
                 LPWMAT3 wm, LPWVEC3 wof);



// Clamping & Gamut handling

BOOL cdecl   _cmsEndPointsBySpace(icColorSpaceSignature Space,
                            WORD **White, WORD **Black, int *nOutputs);

WORD * cdecl _cmsWhiteBySpace(icColorSpaceSignature Space);

WORD cdecl Clamp_XYZ(int in);
WORD cdecl Clamp_RGB(int in);

WORD cdecl Clamp_L(Fixed32 in);
WORD cdecl Clamp_ab(Fixed32 in);


// choose reasonable resolution
int _cmsReasonableGridpointsByColorspace(icColorSpaceSignature Colorspace, DWORD dwFlags);

// Precalculate device link
LPLUT cdecl _cmsPrecalculateDeviceLink(cmsHTRANSFORM h, DWORD dwFlags);

// Compute gamut boundary
LPLUT cdecl _cmsComputeGamutLUT(cmsHPROFILE hProfile, int Intent);

// Compute softproof
LPLUT _cmsComputeSoftProofLUT(cmsHPROFILE hProfile, int nIntent);

// Find a suitable prelinearization tables, matching the given transform
void cdecl _cmsComputePrelinearizationTablesFromXFORM(cmsHTRANSFORM h[], int nTransforms, LPLUT Grid);

// These are two VITAL macros, from converting between 8 and 16 bit
// representation. 

#define RGB_8_TO_16(rgb) (WORD) ((((WORD) (rgb)) << 8)|(rgb)) 
#define RGB_16_TO_8(rgb) (BYTE) ((((rgb) * 65281 + 8388608) >> 24) & 0xFF)


#endif  // LCMS_APIONLY


#define __cms_H

#ifdef __cplusplus
}
#endif

#endif

