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

// Version 1.10

// This program does apply profiles to (some) TIFF files


#include "lcms.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#ifndef NON_WINDOWS
#include <io.h>
#endif

#include "tiffio.h"


// xgetopt() interface -----------------------------------------------------

extern int   xoptind;
extern char *xoptarg;
extern int   xopterr;
extern char   SW;
int    cdecl xgetopt(int argc, char *argv[], char *optionS);

// ------------------------------------------------------------------------

#ifdef NON_WINDOWS
#  ifndef stricmp
#     define stricmp strcasecmp
#  endif
#endif

// Flags

static BOOL Verbose                = FALSE;
static BOOL BlackWhiteCompensation = FALSE;
static BOOL IgnoreEmbedded         = FALSE;
static BOOL EmbedProfile           = FALSE;
static BOOL Width16                = FALSE;
static BOOL GamutCheck             = FALSE;
static BOOL lIsDeviceLink          = FALSE;
static int Intent                  = INTENT_PERCEPTUAL;
static int ProofingIntent          = INTENT_PERCEPTUAL;
static int PrecalcMode             = 1;

static char *cInpProf  = NULL;
static char *cOutProf  = NULL;
static char *cProofing = NULL;


// Console error & warning


static
void ConsoleWarningHandler(const char* module, const char* fmt, va_list ap)
{
        char e[512] = { '\0' };
        if (module != NULL)
              strcat(strcpy(e, module), ": ");

        vsprintf(e+strlen(e), fmt, ap);
        strcat(e, ".");
        if (Verbose) {

              fprintf(stderr, "\nWarning");
              fprintf(stderr, " %s\n", e);
              fflush(stderr);
              }
}

static
void ConsoleErrorHandler(const char* module, const char* fmt, va_list ap)
{
       char e[512] = { '\0' };

       if (module != NULL)
              strcat(strcpy(e, module), ": ");

       vsprintf(e+strlen(e), fmt, ap);
       strcat(e, ".");
       fprintf(stderr, "\nError");
       fprintf(stderr, " %s\n", e);
       fflush(stderr);
}



// Force an error and exit w/ return code 1

static
void FatalError(const char *frm, ...)
{
       va_list args;

       va_start(args, frm);
       ConsoleErrorHandler("TIFFICC", frm, args);
       va_end(args);
       exit(1);
}


// Out of mem

static
void OutOfMem(size_t size)
{
    FatalError("Out of memory on allocating %d bytes.", size);
}


// Build up the pixeltype descriptor

static
DWORD GetInputPixelType(TIFF *Bank)
{
     uint16 Photometric, bps, spp, extra, PlanarConfig, *info;
     uint16 Compression, reverse = 0;
     int ColorChannels, IsPlanar = 0, pt = 0;

     TIFFGetField(Bank,           TIFFTAG_PHOTOMETRIC,   &Photometric);
     TIFFGetFieldDefaulted(Bank,  TIFFTAG_BITSPERSAMPLE, &bps);

     if (bps == 1)
       FatalError("Sorry, bilevel TIFFs has nothig to do with ICC profiles");

     if (bps != 8 && bps != 16)
              FatalError("Sorry, 8 or 16 bits per sample only");

     TIFFGetFieldDefaulted(Bank, TIFFTAG_SAMPLESPERPIXEL, &spp);
     TIFFGetFieldDefaulted(Bank, TIFFTAG_PLANARCONFIG, &PlanarConfig);

     switch (PlanarConfig)
     {
     case PLANARCONFIG_CONTIG: IsPlanar = 0; break;
     case PLANARCONFIG_SEPARATE: IsPlanar = 1; break;
     default:

     FatalError("Unsupported planar configuration (=%d) ", (int) PlanarConfig);
     }

     // If Samples per pixel == 1, PlanarConfiguration is irrelevant and need
     // not to be included.

     if (spp == 1) IsPlanar = 0;


     // Any alpha?

     TIFFGetFieldDefaulted(Bank, TIFFTAG_EXTRASAMPLES, &extra, &info);
     ColorChannels = spp - extra;

     switch (Photometric) {

     case PHOTOMETRIC_MINISWHITE:
                                   
            reverse = 1;

     case PHOTOMETRIC_MINISBLACK:
                                   
            pt = PT_GRAY;                                
            break;

     case PHOTOMETRIC_RGB:
                                   
            pt = PT_RGB;
            break;


     case PHOTOMETRIC_PALETTE:
                                             
            FatalError("Sorry, palette images not supported (at least on this version)"); 

     case PHOTOMETRIC_SEPARATED:
           if (ColorChannels == 4)
                  pt = PT_CMYK;
           else
           if (ColorChannels == 3)
                  pt = PT_CMY;
           else
           if (ColorChannels == 6)
                  pt = PT_HiFi;
           else
                  FatalError("What a weird separation of %d channels?!?!", ColorChannels);
           break;

     case PHOTOMETRIC_YCBCR:
           TIFFGetField(Bank, TIFFTAG_COMPRESSION, &Compression);
           {
                  uint16 subx, suby;

                  pt = PT_YCbCr;
                  TIFFGetFieldDefaulted(Bank, TIFFTAG_YCBCRSUBSAMPLING, &subx, &suby);
                  if (subx != 1 || suby != 1)
                         FatalError("Sorry, subsampled images not supported");

           }
           break;

     case 9:
     case PHOTOMETRIC_CIELAB:
           pt = PT_Lab;
           break;

     case PHOTOMETRIC_LOGL:     /* CIE Log2(L) */
           FatalError("Hummm... I have been unable of find any of  these, please contact me at marti@littlecms.com, thanx.");

     case PHOTOMETRIC_LOGLUV:      /* CIE Log2(L) (u',v') */

           TIFFSetField(Bank, TIFFTAG_SGILOGDATAFMT, SGILOGDATAFMT_16BIT);
           pt = PT_YUV;             // *ICCSpace = icSigLuvData;
           bps = 16;               // 16 bits forced by LibTiff
           break;

     default:
           FatalError("Unsupported TIFF color space (Photometric %d)", Photometric);
     }

     // Convert bits per sample to bytes per sample

     bps >>= 3; 

     return (COLORSPACE_SH(pt)|PLANAR_SH(IsPlanar)|EXTRA_SH(extra)|CHANNELS_SH(ColorChannels)|BYTES_SH(bps)|FLAVOR_SH(reverse));
}



// Rearrange pixel type to build output descriptor

static
DWORD ComputeOutputFormatDescriptor(DWORD dwInput, int OutColorSpace, int bps)
{
   int IsPlanar  = T_PLANAR(dwInput);
   int Channels = 0;

   switch (OutColorSpace) {

   case PT_GRAY:
               Channels = 1;
               break;
   case PT_RGB:
   case PT_CMY:
   case PT_Lab:
   case PT_YUV:
   case PT_YCbCr:
               Channels = 3;
               break;

   case PT_CMYK:
               Channels = 4;
               break;

   case  PT_HiFi:
               Channels = 6;
               break;
   default:
               FatalError("Unsupported output color space");
   }

    return (COLORSPACE_SH(OutColorSpace)|PLANAR_SH(IsPlanar)|CHANNELS_SH(Channels)|BYTES_SH(bps));
}


// Equivalence between ICC color spaces and lcms color spaces



static
int ICC2LCMS(icColorSpaceSignature ProfileSpace)
{
    
       switch (ProfileSpace) {

       case icSigGrayData: return  PT_GRAY;
       case icSigRgbData:  return  PT_RGB;
       case icSigCmyData:  return  PT_CMY;
       case icSigCmykData: return  PT_CMYK;
       case icSigYCbCrData:return  PT_YCbCr;
       case icSigLuvData:  return  PT_YUV;
       case icSigXYZData:  return  PT_XYZ;
       case icSigLabData:  return  PT_Lab;
       case icSigLuvKData: return  PT_YUVK;
       case icSigHsvData:  return  PT_HSV;
       case icSigHlsData:  return  PT_HLS;
       case icSigYxyData:  return  PT_Yxy;

       case icSigHexachromeData: return PT_HiFi;

       default:  return icMaxEnumData;
       }
}


// Tile based transforms

static
int TileBasedXform(cmsHTRANSFORM hXForm, TIFF* in, TIFF* out)
{
    tsize_t BufSizeIn  = TIFFTileSize(in);
    tsize_t BufSizeOut = TIFFTileSize(out);
    unsigned char *BufferIn, *BufferOut;
    ttile_t i, TileCount = TIFFNumberOfTiles(in);
    uint16 tw, tl;
    int PixelCount;


    TIFFGetFieldDefaulted(in, TIFFTAG_TILEWIDTH,  &tw);
    TIFFGetFieldDefaulted(in, TIFFTAG_TILELENGTH, &tl);

     PixelCount = (int) tw * tl;

     BufferIn = (unsigned char *) _TIFFmalloc(BufSizeIn);
    if (!BufferIn) OutOfMem(BufSizeIn);

    BufferOut = (unsigned char *) _TIFFmalloc(BufSizeOut);
    if (!BufferOut) OutOfMem(BufSizeOut);


    for (i = 0; i < TileCount; i++) {

        if (TIFFReadEncodedTile(in, i, BufferIn, BufSizeIn) < 0)   goto cleanup;

      cmsDoTransform(hXForm, BufferIn, BufferOut, PixelCount);

        if (TIFFWriteEncodedTile(out, i, BufferOut, BufSizeOut) < 0) goto cleanup;

    }

    _TIFFfree(BufferIn);
    _TIFFfree(BufferOut);
    return 1;


cleanup:

    _TIFFfree(BufferIn);
   _TIFFfree(BufferOut);
   return 0;
}


// Strip based transforms

static
int StripBasedXform(cmsHTRANSFORM hXForm, TIFF* in, TIFF* out)
{
    tsize_t BufSizeIn  = TIFFStripSize(in);
    tsize_t BufSizeOut = TIFFStripSize(out);
    unsigned char *BufferIn, *BufferOut;
    ttile_t i, StripCount = TIFFNumberOfStrips(in);
    uint32 sw;
    uint32 sl;
    uint32 iml;
    int PixelCount;

    TIFFGetFieldDefaulted(in, TIFFTAG_IMAGEWIDTH,  &sw);
    TIFFGetFieldDefaulted(in, TIFFTAG_ROWSPERSTRIP, &sl);
    TIFFGetFieldDefaulted(in, TIFFTAG_IMAGELENGTH, &iml);

    // PixelCount = (int) sw * sl;

    BufferIn = (unsigned char *) _TIFFmalloc(BufSizeIn);
    if (!BufferIn) OutOfMem(BufSizeIn);

    BufferOut = (unsigned char *) _TIFFmalloc(BufSizeOut);
    if (!BufferOut) OutOfMem(BufSizeOut);


    for (i = 0; i < StripCount; i++) {

        if (TIFFReadEncodedStrip(in, i, BufferIn, BufSizeIn) < 0)   goto cleanup;

        PixelCount = (int) sw * (iml < sl ? iml : sl);
        iml -= sl;

        cmsDoTransform(hXForm, BufferIn, BufferOut, PixelCount);

        if (TIFFWriteEncodedStrip(out, i, BufferOut, BufSizeOut) < 0) goto cleanup;

    }

    _TIFFfree(BufferIn);
    _TIFFfree(BufferOut);
    return 1;


cleanup:

    _TIFFfree(BufferIn);
   _TIFFfree(BufferOut);
   return 0;
}





// Creates minimum required tags

static
void WriteOutputTags(TIFF *out, int Colorspace, int BytesPerSample)
{
    int BitsPerSample = (8 * BytesPerSample);

    switch (Colorspace) {

  case PT_GRAY:
           TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
           TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, 1);
           TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, BitsPerSample);
           break;

   case PT_RGB:
           TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
           TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, 3);
           TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, BitsPerSample);
           break;

   case PT_CMY:
           TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_SEPARATED);
           TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, 3);
           TIFFSetField(out, TIFFTAG_INKSET, 2);
           TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, BitsPerSample);
           break;

   case PT_CMYK:
           TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_SEPARATED);
           TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, 4);
           TIFFSetField(out, TIFFTAG_INKSET, INKSET_CMYK);
           TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, BitsPerSample);
           break;

    case PT_Lab:
            if (BitsPerSample == 16) 
            TIFFSetField(out, TIFFTAG_PHOTOMETRIC, 9);
            else
            TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_CIELAB);
            TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, 3);
            TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, BitsPerSample);    // Needed by TIFF Spec
            break;

     case PT_HiFi:

            TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_SEPARATED);
            TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, 6);
            TIFFSetField(out, TIFFTAG_INKSET, 2);
            TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, BitsPerSample);
            break;
   default:
           FatalError("Unsupported output colorspace");

   }

}


// Copies a bunch of tages

static
void CopyOtherTags(TIFF* in, TIFF* out)
{
#define CopyField(tag, v) \
    if (TIFFGetField(in, tag, &v)) TIFFSetField(out, tag, v)


        short shortv;
        uint32 ow, ol;
        float floatv;
        char *stringv;
        uint32 longv;

        CopyField(TIFFTAG_SUBFILETYPE, longv);

        TIFFGetField(in, TIFFTAG_IMAGEWIDTH, &ow);
        TIFFGetField(in, TIFFTAG_IMAGELENGTH, &ol);

        TIFFSetField(out, TIFFTAG_IMAGEWIDTH, ow);
        TIFFSetField(out, TIFFTAG_IMAGELENGTH, ol);

        CopyField(TIFFTAG_PLANARCONFIG, shortv);

        CopyField(TIFFTAG_COMPRESSION, shortv);
        CopyField(TIFFTAG_PREDICTOR, shortv);

        CopyField(TIFFTAG_THRESHHOLDING, shortv);
        CopyField(TIFFTAG_FILLORDER, shortv);
        CopyField(TIFFTAG_ORIENTATION, shortv);
        CopyField(TIFFTAG_MINSAMPLEVALUE, shortv);
        CopyField(TIFFTAG_MAXSAMPLEVALUE, shortv);
        CopyField(TIFFTAG_XRESOLUTION, floatv);
        CopyField(TIFFTAG_YRESOLUTION, floatv);
        CopyField(TIFFTAG_RESOLUTIONUNIT, shortv);
        CopyField(TIFFTAG_ROWSPERSTRIP, longv);
        CopyField(TIFFTAG_XPOSITION, floatv);
        CopyField(TIFFTAG_YPOSITION, floatv);
        CopyField(TIFFTAG_IMAGEDEPTH, longv);
        CopyField(TIFFTAG_TILEDEPTH, longv);

        CopyField(TIFFTAG_ARTIST, stringv);
        CopyField(TIFFTAG_IMAGEDESCRIPTION, stringv);
        CopyField(TIFFTAG_MAKE, stringv);
        CopyField(TIFFTAG_MODEL, stringv);

        CopyField(TIFFTAG_DATETIME, stringv);
        CopyField(TIFFTAG_HOSTCOMPUTER, stringv);
        CopyField(TIFFTAG_PAGENAME, stringv);
        CopyField(TIFFTAG_DOCUMENTNAME, stringv);

}

// A replacement for (the nonstandard) filelenght

static
int xfilelength(int fd)
{
#ifdef _MSC_VER
        return _filelength(fd);
#else
        struct stat sb;
        if (fstat(fd, &sb) < 0)
                return(-1);
        return(sb.st_size);
#endif


}
static
void DoEmbedProfile(TIFF* Out, const char* ProfileFile)
{
    FILE* f;
    size_t size, EmbedLen;
    LPBYTE EmbedBuffer;

        f = fopen(ProfileFile, "rb");
        if (f == NULL) return;

        size = xfilelength(fileno(f));
        EmbedBuffer = (LPBYTE) malloc(size + 1);
        EmbedLen = fread(EmbedBuffer, 1, size, f);
        fclose(f);
        EmbedBuffer[EmbedLen] = 0;

        TIFFSetField(Out, TIFFTAG_ICCPROFILE, EmbedLen, EmbedBuffer);
        free(EmbedBuffer);
}


static
cmsHPROFILE OpenProfile(const char* File)
{
       if (!File) 
            return cmsCreate_sRGBProfile();    
       
       if (stricmp(File, "*Lab") == 0)
                return cmsCreateLabProfile(NULL);
       
       if (stricmp(File, "*XYZ") == 0)
                return cmsCreateXYZProfile();
         
       if (stricmp(File, "*Gray22") == 0) {
           LPGAMMATABLE Gamma = cmsBuildGamma(256, 2.2);
           cmsHPROFILE hProfile = cmsCreateGrayProfile(cmsD50_xyY(), Gamma);
           cmsFreeGamma(Gamma);
           return hProfile;

       }

        return cmsOpenProfileFromFile(File, "r");
}





// Transform one image

static
int TransformImage(TIFF* in, TIFF* out, char *cDefInpProf, char *cOutProf)
{
    cmsHPROFILE hIn, hOut, hProof;
    cmsHTRANSFORM xform;
    DWORD wInput, wOutput;
    int OutputColorSpace;
    int bps = (Width16 ? 2 : 1);
    DWORD dwFlags = 0; 
    DWORD EmbedLen;
    LPBYTE EmbedBuffer;
    
    if (EmbedProfile && cOutProf) 
        DoEmbedProfile(out, cOutProf);
    
    
    if (BlackWhiteCompensation) {
        
        dwFlags |= cmsFLAGS_WHITEBLACKCOMPENSATION;
        if (PrecalcMode == 0) 
            FatalError("Cannot use White/Black compensation without precalculation. Use /c1, /c2 or /c3.");
    }
    
    switch (PrecalcMode) {
        
    case 0: dwFlags |= cmsFLAGS_NOTPRECALC; break;
    case 2: dwFlags |= cmsFLAGS_HIGHRESPRECALC; break;
    case 3: dwFlags |= cmsFLAGS_LOWRESPRECALC; break;
    case 1: break;
        
    default: FatalError("Unknown precalculation mode '%d'", PrecalcMode);
    }
    
    
    if (GamutCheck)
        dwFlags |= cmsFLAGS_GAMUTCHECK;
    
    
    hProof = NULL;
    hOut   = NULL;
    
    if (lIsDeviceLink) {
        
        hIn = cmsOpenProfileFromFile(cDefInpProf, "r");                  
    }
    else {
        
        if (!IgnoreEmbedded && 
            TIFFGetField(in, TIFFTAG_ICCPROFILE, &EmbedLen, &EmbedBuffer)) {
            
            hIn = cmsOpenProfileFromMem(EmbedBuffer, EmbedLen);
            if (Verbose) {
                fprintf(stdout, " (embedded profile found)");
                fflush(stdout);
            }
            
        }
        else
        {                    
            hIn = OpenProfile(cDefInpProf);
        }
        
        
        hOut = OpenProfile(cOutProf);
        
        if (cProofing != NULL) {
            
            hProof = cmsOpenProfileFromFile(cProofing, "r");
            dwFlags |= cmsFLAGS_SOFTPROOFING;
        }
    }
    
    // Take input color space
    
    wInput = GetInputPixelType(in);
    
    // Assure both, input profile and input TIFF are on same colorspace
    
    if (cmsGetColorSpace(hIn) != _cmsICCcolorSpace(T_COLORSPACE(wInput)))
        FatalError("Input profile is not operating in proper color space");
    
    
    if (!lIsDeviceLink) 
        OutputColorSpace = ICC2LCMS(cmsGetColorSpace(hOut));
    else 
        OutputColorSpace = ICC2LCMS(cmsGetPCS(hIn));
    
    wOutput      = ComputeOutputFormatDescriptor(wInput, OutputColorSpace, bps);
    
    WriteOutputTags(out, OutputColorSpace, bps);
    CopyOtherTags(in, out);
    
    xform = cmsCreateProofingTransform(hIn, wInput, hOut, wOutput, hProof, Intent, 
                                        ProofingIntent, dwFlags);
    
    // Handle tile by tile or strip by strip
    
    if (TIFFIsTiled(in)) {
        
        TileBasedXform(xform, in, out);
    }
    else {
        
        StripBasedXform(xform, in, out);
    }
    
    
    cmsDeleteTransform(xform);
    cmsCloseProfile(hIn);
    cmsCloseProfile(hOut);

    if (hProof) 
        cmsCloseProfile(hProof);
    
    
    TIFFWriteDirectory(out);
    
    return 1;
}


// Simply print help

static
void Help(int level)
{
    fprintf(stderr, "little cms ICC profile applier for TIFF - v2.33\n\n");
    fflush(stderr);
    
     switch(level) {

     default:
     case 0:

     fprintf(stderr, "usage: tifficc [flags] input.tif output.tif\n");

     fprintf(stderr, "\nflags:\n\n");
     fprintf(stderr, "%cv - Verbose\n", SW);
     fprintf(stderr, "%ci<profile> - Input profile (defaults to sRGB)\n", SW);
     fprintf(stderr, "%co<profile> - Output profile (defaults to sRGB)\n", SW);   
     fprintf(stderr, "%cl<profile> - Transform by device-link profile\n", SW);   
     fprintf(stderr, "%ct<0,1,2,3> - Intent (0=Perceptual, 1=Colorimetric, 2=Saturation, 3=Absolute)\n", SW);    
     fprintf(stderr, "\n");

     fprintf(stderr, "%cw - Wide output (generates 16 bps tiff)\n", SW);
     fprintf(stderr, "%cb - Black/White compensation\n", SW);
     fprintf(stderr, "%cn - Ignore embedded profile on input\n", SW);
     fprintf(stderr, "%ce - Embed destination profile\n", SW);
     fprintf(stderr, "%cc<0,1,2,3> - Precalculates transform (0=Off, 1=Normal, 2=Hi-res, 3=LoRes) [defaults to 1]\n", SW);     
     fprintf(stderr, "\n");

     fprintf(stderr, "%cp<profile> - Soft proof profile\n", SW);
     fprintf(stderr, "%cm<0,1,2,3> - Soft proof intent\n", SW);
     fprintf(stderr, "%cg - Marks out-of-gamut colors on softproof\n", SW);
     
     fprintf(stderr, "\n");
     fprintf(stderr, "%ch<0,1,2> - More help\n", SW);
     fprintf(stderr, "\n");
     fprintf(stderr, "You can also use '*Lab' and '*XYZ' as predefined, built-in\n");
     fprintf(stderr, "profiles for CIE L*a*b* and XYZ color spaces.\n");
     
     break;

     case 1:

     
     fprintf(stderr, "Examples:\n\n"
                     "To color correct from scanner to sRGB:\n"
                     "\ttifficc %ciscanner.icm in.tif out.tif\n"
                     "To convert from monitor1 to monitor2:\n"
                     "\ttifficc %cimon1.icm %comon2.icm in.tif out.tif\n"
                     "To make a CMYK separation:\n"
                     "\ttifficc %coprinter.icm inrgb.tif outcmyk.tif\n"
                     "To recover sRGB from a CMYK separation:\n"
                     "\ttifficc %ciprinter.icm incmyk.tif outrgb.tif\n"
                     "To convert from CIELab TIFF to sRGB\n"
                     "\ttifficc %ciTiffLab8Spac.icm in.tif out.tif\n\n", 
                     SW, SW, SW, SW, SW, SW);
     break;

     case 2:

    
     fprintf(stderr, "This program is intended to be a demo of the little cms\n"
                     "engine. Both lcms and this program are freeware. You can\n"
                     "obtain both in source code at http://www.littlecms.com\n"
                     "For suggestions, comments, bug reports etc. send mail to\n"
                     "marti@littlecms.com\n\n");
    
     break;
     }

     fflush(stderr);
     exit(0);
}


// The toggles stuff

static
void HandleSwitches(int argc, char *argv[])
{
       int s;
      
       while ((s=xgetopt(argc,argv,"eEbBwWnNvVGgh:H:i:I:o:O:P:p:t:T:c:C:l:L:M:m:")) != EOF) {

       switch (s)
       {

       case 'e':
       case 'E':
            EmbedProfile = TRUE;
            break;

       case 'b':
       case 'B':
            BlackWhiteCompensation = TRUE;
            break;

       case 'v':
       case 'V':
            Verbose = TRUE;
            break;

       case 'i':
       case 'I':
            if (lIsDeviceLink)
                   FatalError("Device-link already specified"); 

            cInpProf = xoptarg;
            break;

       case 'o':
       case 'O':
           if (lIsDeviceLink)
                   FatalError("Device-link already specified"); 

           cOutProf = xoptarg;
           break;

       case 'l':
       case 'L': 
                cInpProf = xoptarg;
                lIsDeviceLink = TRUE;
                break;

       case 'p':
       case 'P':
           cProofing = xoptarg;
           break;

       case 't':
       case 'T':
            Intent = atoi(xoptarg);
            if (Intent > 3) Intent = 3;
            if (Intent < 0) Intent = 0;
            break;


       case 'm':
       case 'M':
            ProofingIntent = atoi(xoptarg);
            if (ProofingIntent > 3) ProofingIntent = 3;
            if (ProofingIntent < 0) ProofingIntent = 0;
            break;

       case 'N':
       case 'n':
            IgnoreEmbedded = TRUE;
            break;

       case 'W':
       case 'w':
            Width16 = TRUE;
            break;

        case 'g':
        case 'G':
            GamutCheck = TRUE;
            break;

        case 'c':
        case 'C':
            PrecalcMode = atoi(xoptarg);
            if (PrecalcMode < 0 || PrecalcMode > 3)
                    FatalError("Unknown precalc mode '%d'", PrecalcMode);
            break;

        case 'H':
        case 'h':  {

            int a =  atoi(xoptarg);
            Help(a); 
            }
            break;

  default:

       FatalError("Unknown option - run without args to see valid ones");
    }
       
    }
}


// The main sink

int main(int argc, char* argv[])
{
      TIFF *in, *out;
      char *Intents[] = {"perceptual",
                         "relative colorimetric",
                         "saturation",
                         "absolute colorimetric" };

      HandleSwitches(argc, argv);

      if ((argc - xoptind) != 2) {

              Help(0);              
              }

     
      if (Verbose) {

        if (lIsDeviceLink)
            fprintf(stdout, "%s(device link) -> %s [%s]", 
                                                argv[xoptind], 
                                                argv[xoptind+1], 
                                                Intents[Intent]);

        else
           fprintf(stdout, "%s(%s) -> %s(%s) [%s]", argv[xoptind],
                                                (cInpProf == NULL ? "sRGB": cInpProf), 
                                                argv[xoptind+1],
                                                (cOutProf == NULL ? "sRGB" : cOutProf), 
                                                Intents[Intent]);
        fflush(stdout);
      }

      TIFFSetErrorHandler(ConsoleErrorHandler);
      TIFFSetWarningHandler(ConsoleWarningHandler);

      in = TIFFOpen(argv[xoptind], "r");
      if (in == NULL) FatalError("Unable to open '%s'", argv[xoptind]);

      out = TIFFOpen(argv[xoptind+1], "w");

      if (out == NULL) {

             TIFFClose(in);
             FatalError("Unable to write '%s'", argv[xoptind+1]);
             }

      do {

              TransformImage(in, out, cInpProf, cOutProf);


      } while (TIFFReadDirectory(in));


      if (Verbose) { fprintf(stdout, "\n"); fflush(stdout); }

      TIFFClose(in);
      TIFFClose(out);

      return 0;
}

