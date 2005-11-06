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
// 
// 1.10a


//     Input/Output


#include "lcms.h"
#include <time.h>

cmsHPROFILE   LCMSEXPORT cmsOpenProfileFromFile(const char *ICCProfile, const char *sAccess);
cmsHPROFILE   LCMSEXPORT cmsOpenProfileFromMem(LPVOID MemPtr, DWORD dwSize);
cmsHPROFILE   LCMSEXPORT cmsCreateRGBProfile(LPcmsCIExyY WhitePoint,
                                        LPcmsCIExyYTRIPLE Primaries,
                                        LPGAMMATABLE TransferFunction[3]);

cmsHPROFILE   LCMSEXPORT cmsCreateGrayProfile(LPcmsCIExyY WhitePoint,
                                              LPGAMMATABLE TransferFunction);

BOOL          LCMSEXPORT cmsCloseProfile(cmsHPROFILE hProfile);
LPLUT         LCMSEXPORT cmsReadICCLut(cmsHPROFILE hProfile, icTagSignature sig);
LPGAMMATABLE  LCMSEXPORT cmsReadICCGamma(cmsHPROFILE hProfile, icTagSignature sig);
LPGAMMATABLE  LCMSEXPORT cmsReadICCGammaReversed(cmsHPROFILE hProfile, icTagSignature sig);
int           cdecl      cmsReadICCnamedColorList(cmsHTRANSFORM xform, cmsHPROFILE hProfile, icTagSignature sig);

BOOL          LCMSEXPORT cmsTakeMediaWhitePoint(LPcmsCIEXYZ Dest, cmsHPROFILE hProfile);
BOOL          LCMSEXPORT cmsTakeMediaBlackPoint(LPcmsCIEXYZ Dest, cmsHPROFILE hProfile);
BOOL          LCMSEXPORT cmsTakeIluminant(LPcmsCIEXYZ Dest, cmsHPROFILE hProfile);
BOOL          LCMSEXPORT cmsTakeColorants(LPcmsCIEXYZTRIPLE Dest, cmsHPROFILE hProfile);
BOOL          LCMSEXPORT cmsIsTag(cmsHPROFILE hProfile, icTagSignature sig);


const char*   LCMSEXPORT cmsTakeProductName(cmsHPROFILE hProfile);

icColorSpaceSignature   LCMSEXPORT cmsGetPCS(cmsHPROFILE hProfile);
icColorSpaceSignature   LCMSEXPORT cmsGetColorSpace(cmsHPROFILE hProfile);
icProfileClassSignature LCMSEXPORT cmsGetDeviceClass(cmsHPROFILE hProfile);


BOOL LCMSEXPORT _cmsAddLUTTag(cmsHPROFILE hProfile, icTagSignature sig, LPVOID lut);
BOOL LCMSEXPORT _cmsAddGammaTag(cmsHPROFILE hProfile, icTagSignature sig, LPGAMMATABLE TransferFunction);
BOOL LCMSEXPORT _cmsAddChromaticityTag(cmsHPROFILE hProfile, icTagSignature sig, LPcmsCIExyYTRIPLE Chrm);

BOOL LCMSEXPORT cmsAddTag(cmsHPROFILE hProfile, icTagSignature sig, LPVOID Tag);
BOOL LCMSEXPORT _cmsSaveProfile(cmsHPROFILE hProfile, const char* FileName);
BOOL LCMSEXPORT _cmsSaveProfileToMem(cmsHPROFILE hProfile, LPVOID MemPtr, size_t* BytesSaved);

// ------------------- implementation -----------------------------------


#define ALIGNLONG(x) (((x)+3) & ~(3))         // Aligns to DWORD boundary

static size_t UsedSpace;            // Using this makes writting of profiles non-reentrant!
static int    GlobalLanguageCode;   // Language & country descriptors, for ICC 4.0 support
static int    GlobalCountryCode;

                           
#ifdef __BEOS__
#       define USE_CUSTOM_SWAB  1
#endif


#ifdef USE_CUSTOM_SWAB

// Replacement to swab function, thanks to YNOP
// for providing the BeOS port
//
// from: @(#)swab.c       5.10 (Berkeley)  3/6/91

static
void xswab(const void *from, void *to, size_t len)
{
         register unsigned long temp;
         register int n;
         register char *fp, *tp;

         n = (len >> 1) + 1;
         fp = (char *)from;
         tp = (char *)to;
#define STEP    temp = *fp++,*tp++ = *fp++,*tp++ = temp
         /* round to multiple of 8 */
         while ((--n) & 07)
                 STEP;
         n >>= 3;
         while (--n >= 0) {

                 STEP; STEP; STEP; STEP;
                 STEP; STEP; STEP; STEP;
         }
#undef STEP
}
#else
#define xswab swab
#endif


//
//      Little-Endian to Big-Endian
//

#ifdef USE_BIG_ENDIAN
#define AdjustEndianess16(a)
#define AdjustEndianess32(a)
#define AdjustEndianessArray16(a, b)
#else

static
void AdjustEndianess16(LPBYTE pByte)
{
       BYTE tmp;

       tmp = pByte[0];
       pByte[0] = pByte[1];
       pByte[1] = tmp;
}

static
void AdjustEndianess32(LPBYTE pByte)
{
        BYTE temp1;
        BYTE temp2;

        temp1 = *pByte++;
        temp2 = *pByte++;
        *(pByte-1) = *pByte;
        *pByte++ = temp2;
        *(pByte-3) = *pByte;
        *pByte = temp1;
}


// swap bytes in a array of words

static
void AdjustEndianessArray16(LPWORD p, size_t num_words)
{
       xswab((char*) p, (char*)p, (int) num_words * sizeof(WORD));
}

#endif


// Fixed point conversion

static
double Convert8Fixed8(WORD fixed8)
{
       BYTE msb, lsb;

       lsb = (BYTE) (fixed8 & 0xff);
       msb = (BYTE) (((WORD) fixed8 >> 8) & 0xff);

       return (double) ((double) msb + ((double) lsb / 256.0));
}

static
double Convert15Fixed16(icS15Fixed16Number fix32)
{
    double floater, sign, mid, hack;
    int Whole, FracPart;


    AdjustEndianess32((LPBYTE) &fix32);

    sign  = (fix32 < 0 ? -1 : 1);
    fix32 = abs(fix32);

    Whole = LOWORD(fix32 >> 16);
    FracPart  = LOWORD(fix32 & 0x0000ffffL);

    hack    = 65536.0;
    mid     = (double) FracPart / hack;
    floater = (double) Whole + mid;

    return sign*floater;
}


// Memory-based I/O ---------------------------------------------------

typedef struct {
                LPBYTE Block;       // Points to allocated memory
                size_t Size;        // Size of allocated memory
                int Pointer;        // Points to current location

                } FILEMEM;

static
LPVOID MemoryOpen(LPBYTE Block, size_t Size, char Mode)
{
    FILEMEM* fm = (FILEMEM*) malloc(sizeof(FILEMEM));
    ZeroMemory(fm, sizeof(FILEMEM));

	if (Mode == 'r') {

		fm ->Block   = (LPBYTE) malloc(Size);
		if (fm ->Block == NULL) {
			free(fm);
			return NULL;
		}
	

    CopyMemory(fm->Block, Block, Size);
	}
	else fm ->Block = NULL;

    fm ->Size    = Size;
    fm ->Pointer = 0;

    return (LPVOID) fm;
}


static
size_t MemoryRead(LPVOID buffer, size_t size, size_t count, LPVOID f)
{   
     FILEMEM* ResData = f;
     LPBYTE Ptr;
     size_t len = size * count;
     

    if (ResData -> Pointer + len > ResData -> Size)
            len = (ResData -> Size - ResData -> Pointer);

    Ptr  = ResData -> Block;
    Ptr += ResData -> Pointer;
    CopyMemory(buffer, Ptr, len);        
    ResData -> Pointer += len;

    return count;
}

// SEEK_CUR is assumed

static
BOOL MemorySeek(LPVOID f, size_t offset)
{
    FILEMEM* ResData = f;

    ResData ->Pointer = offset; 
    return 0; 
}

// Writes data to memory, also keeps used space for further reference

static
BOOL MemoryWrite(void *OutStream, size_t size, void *Ptr)
{
		FILEMEM* ResData = OutStream;

       if (size == 0) return TRUE;
       
	   if (ResData != NULL) 
		   CopyMemory(ResData ->Block + UsedSpace, Ptr, size);

       UsedSpace += size;    

       return TRUE;
}


static
BOOL MemoryClose(LPVOID f)
{
    FILEMEM* ResData = f;

    if (ResData ->Block) free(ResData ->Block);
    free(ResData);
    return 0;
}


// File-based I/O -------------------------------------------------------

static
LPVOID FileOpen(const char* filename)
{
    return (void*) fopen(filename, "rb");
}

static
size_t FileRead(void *buffer, size_t size, size_t count, LPVOID stream)
{
    size_t nReaded = fread(buffer, size, count, (FILE*) stream);
    if (nReaded != count) {
            cmsSignalError(LCMS_ERRC_WARNING, "Read error. Got %d entries, table should be of %d entries", nReaded, count);         
    }

    return nReaded;
}


static
BOOL FileSeek(LPVOID stream, size_t offset)
{
    if (fseek((FILE*) stream, offset, SEEK_SET)) {

       cmsSignalError(LCMS_ERRC_ABORTED, "Seek error; probably corrupted file");
       return TRUE;
    }

    return 0;
}

// Writes data to stream, also keeps used space for further reference


static
BOOL FileWrite(void* OutStream, size_t size, LPVOID Ptr)
{
       if (size == 0) return TRUE;
       
       UsedSpace += size;

       if (OutStream == NULL) {

              return TRUE;
       }

       return (fwrite(Ptr, size, 1, (FILE*) OutStream) == 1);
}


static
BOOL FileClose(LPVOID stream)
{
    return fclose((FILE*) stream);
}

// ----------------------------------------------------------------------------------------------------



// Allocate ICC struct. I/O routines are passed through

static
LPLCMSICCPROFILE ICCAllocStruct( 
                                size_t (* Read)(void *buffer, size_t size, size_t count, LPVOID stream),                        
                                BOOL   (* Seek)(LPVOID stream, size_t offset),
                                BOOL   (* Close)(LPVOID stream))

{
    LPLCMSICCPROFILE Icc = (LPLCMSICCPROFILE) malloc(sizeof(LCMSICCPROFILE));
    if (Icc == NULL) return NULL;

    ZeroMemory(Icc, sizeof(LCMSICCPROFILE));

    Icc ->Read  = Read; 
    Icc ->Seek  = Seek;
    Icc ->Close = Close;

    Icc ->Write = NULL;
    

    return Icc;
}

// Does create & init a struct holding profile directory and some aids

static
LPLCMSICCPROFILE CreateICCProfileHandler(LPVOID ICCfile,
                             size_t (* Read)(void *buffer, size_t size, size_t count, void *stream),                            
                             BOOL   (* Seek)(LPVOID stream, size_t offset),
                             BOOL   (* Close)(LPVOID stream))
{
       LPLCMSICCPROFILE Icc;
       icTag Tag;
       icHeader Header;
       icInt32Number TagCount, i;


       Icc = ICCAllocStruct(Read, Seek, Close);
       if (Icc == NULL) return NULL;

       Icc -> stream = ICCfile;
       Icc -> Read(&Header, sizeof(icHeader), 1, ICCfile);

       // Convert endian

       AdjustEndianess32((LPBYTE) &Header.size);
       AdjustEndianess32((LPBYTE) &Header.cmmId);
       AdjustEndianess32((LPBYTE) &Header.version);
       AdjustEndianess32((LPBYTE) &Header.deviceClass);
       AdjustEndianess32((LPBYTE) &Header.colorSpace);
       AdjustEndianess32((LPBYTE) &Header.pcs);
       AdjustEndianess32((LPBYTE) &Header.magic);
       AdjustEndianess32((LPBYTE) &Header.flags);
       AdjustEndianess32((LPBYTE) &Header.renderingIntent);

       // Validate it

       if (Header.magic != icMagicNumber) goto ErrorCleanup;
             
      
       if (Icc ->Read(&TagCount, sizeof(icInt32Number), 1, ICCfile) != 1)
                     goto ErrorCleanup;

       AdjustEndianess32((LPBYTE) &TagCount);

       Icc -> DeviceClass     = Header.deviceClass;
       Icc -> ColorSpace      = Header.colorSpace;
       Icc -> PCS             = Header.pcs;
       Icc -> RenderingIntent = (icRenderingIntent) Header.renderingIntent;
       Icc -> Illuminant.X    = Convert15Fixed16(Header.illuminant.X);
       Icc -> Illuminant.Y    = Convert15Fixed16(Header.illuminant.Y);
       Icc -> Illuminant.Z    = Convert15Fixed16(Header.illuminant.Z);


       // Read tag directory

       Icc -> TagCount   = TagCount;
       for (i=0; i < TagCount; i++)
       {
              Icc ->Read(&Tag, sizeof(icTag), 1, ICCfile);

              AdjustEndianess32((LPBYTE) &Tag.offset);
              AdjustEndianess32((LPBYTE) &Tag.size);
              AdjustEndianess32((LPBYTE) &Tag.sig);            // Signature

              Icc -> TagNames[i]   = Tag.sig;
              Icc -> TagOffsets[i] = Tag.offset;
              Icc -> TagSizes[i]   = Tag.size;
       }

       return Icc;


ErrorCleanup:

              Icc ->Close(ICCfile);
              free(Icc);
              return NULL;
}


// Does search for a specific tag in tag dictionary
// Returns position or -1 if tag not found

static
icInt32Number  SearchTag(LPLCMSICCPROFILE Profile, icTagSignature sig)
{
       icInt32Number i;

       if (sig == 0) return -1;     // 0 identifies a special tag holding raw memory.

       for (i=0; i < Profile -> TagCount; i++)
       {
              if (sig == Profile -> TagNames[i])
                            return i;
       }

       return -1;
}


// Search for a particular tag, replace if found or add new one else

static
LPVOID InitTag(LPLCMSICCPROFILE Icc, icTagSignature sig, size_t size, const LPVOID Init)
{
       LPVOID Ptr;
       icInt32Number i;

       i = SearchTag(Icc, sig);
       if (i >=0) {

              if (Icc -> TagPtrs[i]) free(Icc -> TagPtrs[i]);
       }
       else  {
             i = Icc -> TagCount;
             Icc -> TagCount++;
             }


       Ptr = malloc(size);
       CopyMemory(Ptr, Init, size);

       Icc ->TagNames[i] = sig;
       Icc ->TagSizes[i] = size;
       Icc ->TagPtrs[i]  = Ptr;

       return Ptr;
}



static
int SizeOfGammaTab(LPGAMMATABLE In)
{
       return sizeof(GAMMATABLE) + (In -> nEntries - 1)*sizeof(WORD);
}


// This function creates a profile based on White point, primaries and
// transfer functions.

cmsHPROFILE LCMSEXPORT cmsCreateRGBProfile(LPcmsCIExyY WhitePoint,
                                LPcmsCIExyYTRIPLE Primaries,
                                LPGAMMATABLE TransferFunction[3])
{
       LCMSICCPROFILE *Icc;
       cmsCIEXYZ tmp;
       MAT3 MColorants;
       cmsCIEXYZTRIPLE Colorants;
       cmsCIExyY MaxWhite;
      

       Icc = ICCAllocStruct(NULL, NULL, NULL);
       if (!Icc)                          // can't allocate
            return NULL;
              

       Icc -> stream          = NULL;
       Icc -> DeviceClass     = icSigDisplayClass;      
       Icc -> ColorSpace      = icSigRgbData;
       Icc -> PCS             = icSigXYZData;
       Icc -> RenderingIntent = icPerceptual;    
       Icc -> flags           = 0;
       Icc -> Illuminant.X    = D50X;
       Icc -> Illuminant.Y    = D50Y;
       Icc -> Illuminant.Z    = D50Z;


       // Implement profile using following tags:
       //
       //  1 icSigProfileDescriptionTag
       //  2 icSigMediaWhitePointTag
       //  3 icSigRedColorantTag
       //  4 icSigGreenColorantTag
       //  5 icSigBlueColorantTag
       //  6 icSigRedTRCTag
       //  7 icSigGreenTRCTag
       //  8 icSigBlueTRCTag

       // This conforms a standard RGB DisplayProfile as says ICC, and then I add

       // 9 icSigChromaticityTag

       // As addendum II

       Icc -> TagCount   = 0;

       // Fill-in the tags

       InitTag(Icc, icSigProfileDescriptionTag, 11, "(internal)");

       if (WhitePoint) {

       cmsxyY2XYZ(&tmp, WhitePoint);
       InitTag(Icc, icSigMediaWhitePointTag, sizeof(cmsCIEXYZ), &tmp);
       }

       if (WhitePoint && Primaries) {

        MaxWhite.x =  WhitePoint -> x;
        MaxWhite.y =  WhitePoint -> y;
        MaxWhite.Y =  1.0;

       if (!cmsBuildRGB2XYZtransferMatrix(&MColorants, &MaxWhite, Primaries))
       {
              free(Icc);
              return NULL;
       }  
     
       cmsAdaptMatrixToD50(&MColorants, &MaxWhite);

       Colorants.Red.X = MColorants.v[0].n[0];
       Colorants.Red.Y = MColorants.v[1].n[0];
       Colorants.Red.Z = MColorants.v[2].n[0];

       Colorants.Green.X = MColorants.v[0].n[1];
       Colorants.Green.Y = MColorants.v[1].n[1];
       Colorants.Green.Z = MColorants.v[2].n[1];

       Colorants.Blue.X = MColorants.v[0].n[2];
       Colorants.Blue.Y = MColorants.v[1].n[2];
       Colorants.Blue.Z = MColorants.v[2].n[2];

       InitTag(Icc, icSigRedColorantTag,     sizeof(cmsCIEXYZ), &Colorants.Red);
       InitTag(Icc, icSigBlueColorantTag,    sizeof(cmsCIEXYZ), &Colorants.Blue);
       InitTag(Icc, icSigGreenColorantTag,   sizeof(cmsCIEXYZ), &Colorants.Green);
       }


       if (TransferFunction) {

       // In case of gamma, we must dup' the table pointer

       InitTag(Icc, icSigRedTRCTag, SizeOfGammaTab(TransferFunction[0]), TransferFunction[0]);
       InitTag(Icc, icSigGreenTRCTag, SizeOfGammaTab(TransferFunction[1]), TransferFunction[1]);
       InitTag(Icc, icSigBlueTRCTag, SizeOfGammaTab(TransferFunction[2]), TransferFunction[2]);
       }

       if (Primaries) {
       InitTag(Icc, icSigChromaticityTag, sizeof(cmsCIExyYTRIPLE), Primaries);
       }

       return (cmsHPROFILE) Icc;
}

// This function creates a profile based on White point and transfer function.

cmsHPROFILE   LCMSEXPORT cmsCreateGrayProfile(LPcmsCIExyY WhitePoint,
                                              LPGAMMATABLE TransferFunction)
{
       LCMSICCPROFILE *Icc;
       cmsCIEXYZ tmp;              
      

       Icc = ICCAllocStruct(NULL, NULL, NULL);
       if (!Icc)                          // can't allocate
            return NULL;
              

       Icc -> stream          = NULL;
       Icc -> DeviceClass     = icSigDisplayClass;      
       Icc -> ColorSpace      = icSigGrayData;
       Icc -> PCS             = icSigXYZData;
       Icc -> RenderingIntent = icPerceptual;    
       Icc -> flags           = 0;
       Icc -> Illuminant.X    = D50X;
       Icc -> Illuminant.Y    = D50Y;
       Icc -> Illuminant.Z    = D50Z;


       // Implement profile using following tags:
       //
       //  1 icSigProfileDescriptionTag
       //  2 icSigMediaWhitePointTag
       //  6 icSigGrayTRCTag
       
       // This conforms a standard Gray DisplayProfile 
       
       Icc -> TagCount   = 0;

       // Fill-in the tags

       InitTag(Icc, icSigProfileDescriptionTag, 11, "(internal)");

       if (WhitePoint) {

       cmsxyY2XYZ(&tmp, WhitePoint);
       InitTag(Icc, icSigMediaWhitePointTag, sizeof(cmsCIEXYZ), &tmp);
       }


       if (TransferFunction) {

       // In case of gamma, we must dup' the table pointer

       InitTag(Icc, icSigGrayTRCTag, SizeOfGammaTab(TransferFunction), TransferFunction);
       }

       return (cmsHPROFILE) Icc;

}


// Does convert a transform into a device link profile

cmsHPROFILE LCMSEXPORT cmsTransform2DeviceLink(cmsHTRANSFORM hTransform, DWORD dwFlags)
{
       LCMSICCPROFILE *Icc;
        _LPcmsTRANSFORM v = (_LPcmsTRANSFORM) hTransform;
        LPLUT Lut;
        BOOL MustFreeLUT;
      
        if (v ->DeviceLink) {

                Lut = v -> DeviceLink;
                MustFreeLUT = FALSE;
        }
        else {

            Lut = _cmsPrecalculateDeviceLink(hTransform, dwFlags);
            if (!Lut) return NULL;
            MustFreeLUT = TRUE;
        }

       Icc = ICCAllocStruct(NULL, NULL, NULL);
       if (!Icc) {

            if (MustFreeLUT) cmsFreeLUT(Lut);
            return NULL;
       }

           
       Icc -> stream          = NULL;
       Icc -> DeviceClass     = icSigLinkClass;      
       Icc -> ColorSpace      = v -> EntryColorSpace;
       Icc -> PCS             = v -> ExitColorSpace;
       Icc -> RenderingIntent = v -> Intent;    
       Icc -> flags           = 0;
       Icc -> Illuminant.X    = D50X;
       Icc -> Illuminant.Y    = D50Y;
       Icc -> Illuminant.Z    = D50Z;


       // Implement devicelink profile using following tags:
       //
       //  1 icSigProfileDescriptionTag
       //  2 icSigMediaWhitePointTag
       //  3 icSigAToB0Tag
       //  and should store 4 icSigProfileSequenceDescTag, but currently it doesn't
       
            
       Icc -> TagCount   = 0;
       
       InitTag(Icc, icSigProfileDescriptionTag, 11, "(internal device link)");       
       InitTag(Icc, icSigMediaWhitePointTag, sizeof(cmsCIEXYZ), cmsD50_XYZ());

       cmsAddTag((cmsHPROFILE) Icc, icSigAToB0Tag, Lut);
       
       if (MustFreeLUT) cmsFreeLUT(Lut);

       return (cmsHPROFILE) Icc;

}


// This is a devicelink operating in the target colorspace with as many transfer
// functions as components

cmsHPROFILE LCMSEXPORT cmsCreateLinearizationDeviceLink(icColorSpaceSignature ColorSpace,
                                                        LPGAMMATABLE TransferFunctions[])
{
       LCMSICCPROFILE *Icc;
       LPLUT Lut;
           
       Icc = ICCAllocStruct(NULL, NULL, NULL);
       if (!Icc)                          
            return NULL;
              

       Icc -> stream          = NULL;
       Icc -> DeviceClass     = icSigLinkClass;      
       Icc -> ColorSpace      = ColorSpace;
       Icc -> PCS             = ColorSpace;
       Icc -> RenderingIntent = icPerceptual;    
       Icc -> flags           = 0;
       Icc -> Illuminant.X    = D50X;
       Icc -> Illuminant.Y    = D50Y;
       Icc -> Illuminant.Z    = D50Z;


       // Creates a LUT with prelinearization step only
       Lut = cmsAllocLUT();

       // Set up channels
       Lut ->InputChan = Lut ->OutputChan = _cmsChannelsOf(ColorSpace);

       // Copy tables to LUT
       cmsAllocLinearTable(Lut, TransferFunctions, 1);

       // Create tags
       Icc -> TagCount   = 0;      
       InitTag(Icc, icSigProfileDescriptionTag, 11, "(internal linearization device link)");       
       InitTag(Icc, icSigMediaWhitePointTag, sizeof(cmsCIEXYZ), cmsD50_XYZ());

       cmsAddTag((cmsHPROFILE) Icc, icSigAToB0Tag, Lut);
       
       // LUT is already on virtual profile
       cmsFreeLUT(Lut);

       // Ok, done
       return (cmsHPROFILE) Icc;
}

// Ink-limiting algorithm
//
//  Sum = C + M + Y + K 
//  If Sum > InkLimit 
//        Ratio= 1 - (Sum - InkLimit) / (C + M + Y)
//        if Ratio <0 
//              Ratio=0
//        endif     
//     Else 
//         Ratio=1
//     endif
//
//     C = Ratio * C
//     M = Ratio * M
//     Y = Ratio * Y
//     K: Does not change
   
static
int InkLimitingSampler(register WORD In[], register WORD Out[], register LPVOID Cargo)
{
        double InkLimit = *(double *) Cargo;
        double SumCMY, SumCMYK, Ratio;
    
        InkLimit = (InkLimit * 655.35);

        SumCMY   = In[0]  + In[1] + In[2];
        SumCMYK  = SumCMY + In[3];      

        if (SumCMYK > InkLimit) {

                Ratio = 1 - ((SumCMYK - InkLimit) / SumCMY);
                if (Ratio < 0)
                        Ratio = 0;
        }
        else Ratio = 1;
                
        Out[0] = (WORD) floor(In[0] * Ratio + 0.5);     // C
        Out[1] = (WORD) floor(In[1] * Ratio + 0.5);     // M
        Out[2] = (WORD) floor(In[2] * Ratio + 0.5);     // Y

        Out[3] = In[3];                                 // K (untouched)

        return TRUE;
}

// This is a devicelink operating in CMYK for ink-limiting


cmsHPROFILE LCMSEXPORT cmsCreateInkLimitingDeviceLink(icColorSpaceSignature ColorSpace,
                                                        double Limit)
{
       LCMSICCPROFILE *Icc;
       LPLUT Lut;
           
       if (ColorSpace != icSigCmykData) {
            cmsSignalError(LCMS_ERRC_ABORTED, "InkLimiting: Only CMYK currently supported");
            return NULL;
       }

       if (Limit < 0.0 || Limit > 400) {

           cmsSignalError(LCMS_ERRC_WARNING, "InkLimiting: Limit should be between 0..400");        
           if (Limit < 0) Limit = 0;
           if (Limit > 400) Limit = 400;
       
       }

       Icc = ICCAllocStruct(NULL, NULL, NULL);
       if (!Icc)                          
            return NULL;
              
       Icc -> stream          = NULL;
       Icc -> DeviceClass     = icSigLinkClass;      
       Icc -> ColorSpace      = icSigCmykData;
       Icc -> PCS             = icSigCmykData;
       Icc -> RenderingIntent = icPerceptual;    
       Icc -> flags           = 0;
       Icc -> Illuminant.X    = D50X;
       Icc -> Illuminant.Y    = D50Y;
       Icc -> Illuminant.Z    = D50Z;


       // Creates a LUT with prelinearization step only
       Lut = cmsAllocLUT();


       cmsAlloc3DGrid(Lut, 17, 4, 4);
       if (!cmsSample3DGrid(Lut, InkLimitingSampler, (LPVOID) &Limit, 0)) {

                // Shouldn't reach here
                cmsFreeLUT(Lut);
                free(Icc);
                return NULL;
       }    
       
       // Create tags
       Icc -> TagCount   = 0;      
       InitTag(Icc, icSigProfileDescriptionTag, 11, "(internal ink limiting device link)");       
       InitTag(Icc, icSigMediaWhitePointTag, sizeof(cmsCIEXYZ), cmsD50_XYZ());

       cmsAddTag((cmsHPROFILE) Icc, icSigAToB0Tag, Lut);
       
       // LUT is already on virtual profile
       cmsFreeLUT(Lut);

       // Ok, done
       return (cmsHPROFILE) Icc;
}





//
//

// Create profile from disk file

cmsHPROFILE LCMSEXPORT cmsOpenProfileFromFile(const char *lpFileName, const char *sAccess)
{
       LPLCMSICCPROFILE NewIcc;
       LPVOID ICCfile;
       cmsHPROFILE hEmpty;
       
       if (*sAccess == 'W' || *sAccess == 'w') {

           hEmpty = cmsCreateLabProfile(NULL);
           NewIcc = (LPLCMSICCPROFILE) (LPSTR) hEmpty;
           NewIcc -> IsWrite = TRUE;
           strncpy(NewIcc ->PhysicalFile, lpFileName, MAX_PATH-1);

           return hEmpty;
       }


       ICCfile = FileOpen(lpFileName);
       if (!ICCfile) {
              cmsSignalError(LCMS_ERRC_ABORTED, "File '%s' not found", lpFileName);
              return NULL;  
       }

       NewIcc = CreateICCProfileHandler(ICCfile, FileRead, FileSeek, FileClose);
       if (!NewIcc) {

              cmsSignalError(LCMS_ERRC_ABORTED, "Bad file format: '%s'", lpFileName);
              return NULL;  // Urecoverable
       }

       strncpy(NewIcc -> PhysicalFile, lpFileName, MAX_PATH-1);
       NewIcc -> IsWrite     = FALSE;

       return (cmsHPROFILE) (LPSTR) NewIcc;
}




// Open from mem

cmsHPROFILE LCMSEXPORT cmsOpenProfileFromMem(LPVOID MemPtr, DWORD dwSize)
{
       LPLCMSICCPROFILE NewIcc;
       LPVOID ICCfile;
                    
       ICCfile = MemoryOpen(MemPtr, (size_t) dwSize, 'r');
       if (!ICCfile) {
              cmsSignalError(LCMS_ERRC_ABORTED, "Couldn't allocate %ld bytes for profile", dwSize);
              return NULL;  
       }

       NewIcc = CreateICCProfileHandler(ICCfile, MemoryRead, MemorySeek, MemoryClose);
       if (!NewIcc) {

              cmsSignalError(LCMS_ERRC_ABORTED, "Bad file format on memory profile");
              return NULL;  // Urecoverable
       }

       NewIcc -> PhysicalFile[0] = 0;
       // NewIcc -> IsTemporary = FALSE;
       NewIcc -> IsWrite     = FALSE;

       return (cmsHPROFILE) (LPSTR) NewIcc;

}



BOOL LCMSEXPORT cmsCloseProfile(cmsHPROFILE hProfile)
{
       LPLCMSICCPROFILE icco = (LPLCMSICCPROFILE) (LPSTR) hProfile;
       FILE *file;
       BOOL rc = TRUE;

       if (!icco) return FALSE;


       // Was open in write mode?   
       if (icco ->IsWrite) {

           icco ->IsWrite = FALSE;      // Assure no further writting
           rc = _cmsSaveProfile(hProfile, icco ->PhysicalFile);        
       }


       file = icco -> stream;

       if (!file)
       {
              icInt32Number i;

              for (i=0; i < icco -> TagCount; i++)
              {
                  if (icco -> TagPtrs[i])
                            free(icco -> TagPtrs[i]);
              }

       }
       else   {
              icco -> Close(file);            
              }

       free(icco);

       return rc;
}



static
unsigned int uipow(unsigned int a, unsigned int b) {
        unsigned int rv = 1;
        for (; b > 0; b--)
                rv *= a;
        return rv;
}


// Convert between notations.

#define TO16_TAB(x)      (WORD) (((x) << 8) | (x))


// LUT8 can come only in Lab space. There is a fatal flaw in
// converting from Lut8 to Lut16. Due to particular encoding 
// of Lab, different actions should be taken from input and 
// output Lab8 LUTS. For input, is as easy as applying a << 8,
// since numbers comes in fixed point. However, for output LUT
// things goes a bit more complex.... LUT 16 is supposed to
// have a domain of 0..ff00, so we should remap the LUT in order
// to get things working. Affected signatures are B2Axx tags, 
// preview and gamut.

// I do solve it by multiplying input matrix by:
//  
//  | 0xffff/0xff00   0    0  |
//  |       0         1    0  |
//  |       0         0    1  |
//
// The input values got then remapped to adequate domain

static
void FixLUT8(LPLUT Lut, icTagSignature sig, int nTabSize)
{   
    MAT3 Fixup, Original, Result;
    LPWORD PtrW;
    int i;

    switch (sig) {
 

       case icSigBToA0Tag:
       case icSigBToA1Tag:
       case icSigBToA2Tag:
       case icSigGamutTag:
       case icSigPreview0Tag:
       case icSigPreview1Tag:
       case icSigPreview2Tag: 
                
                VEC3init(&Fixup.v[0], (double) 0xFFFF/0xFF00, 0, 0);
                VEC3init(&Fixup.v[1], 0, 1, 0);
                VEC3init(&Fixup.v[2], 0, 0, 1);

                MAT3fromFix(&Original, &Lut->Matrix);               
                MAT3per(&Result, &Original, &Fixup);
                MAT3toFix(&Lut->Matrix, &Result);

                Lut -> wFlags |= LUT_HASMATRIX;
                break;

       // For input, clear low part since this has to be
       // Lab in fixed point

       default:        

                PtrW = Lut -> T;
                for (i = 0; i < nTabSize; i++) {

                             *PtrW++ &= 0xFF00;
                }
    }
    
}

// The infamous LUT 8
static
void ReadLUT8(LPLCMSICCPROFILE Icc, LPLUT NewLUT, icTagSignature sig)
{
    icLut8 LUT8;
    LPBYTE Temp;
    size_t nTabSize;
    unsigned int i, j;
    unsigned int AllLinear;
    LPWORD PtrW;

       Icc ->Read(&LUT8, sizeof(icLut8) - SIZEOF_UINT8_ALIGNED, 1, Icc -> stream);
       
       NewLUT -> wFlags        = LUT_HASTL1|LUT_HASTL2|LUT_HAS3DGRID;
       NewLUT -> cLutPoints    = LUT8.clutPoints;
       NewLUT -> InputChan     = LUT8.inputChan;
       NewLUT -> OutputChan    = LUT8.outputChan;
       NewLUT -> InputEntries  = 256;
       NewLUT -> OutputEntries = 256;


       AdjustEndianess32((LPBYTE) &LUT8.e00);
       AdjustEndianess32((LPBYTE) &LUT8.e01);
       AdjustEndianess32((LPBYTE) &LUT8.e02);
       AdjustEndianess32((LPBYTE) &LUT8.e10);
       AdjustEndianess32((LPBYTE) &LUT8.e11);
       AdjustEndianess32((LPBYTE) &LUT8.e12);
       AdjustEndianess32((LPBYTE) &LUT8.e20);
       AdjustEndianess32((LPBYTE) &LUT8.e21);
       AdjustEndianess32((LPBYTE) &LUT8.e22);


       // Matrix handling

       NewLUT -> Matrix.v[0].n[0] = (Fixed32) LUT8.e00;
       NewLUT -> Matrix.v[0].n[1] = (Fixed32) LUT8.e01;
       NewLUT -> Matrix.v[0].n[2] = (Fixed32) LUT8.e02;
       NewLUT -> Matrix.v[1].n[0] = (Fixed32) LUT8.e10;
       NewLUT -> Matrix.v[1].n[1] = (Fixed32) LUT8.e11;
       NewLUT -> Matrix.v[1].n[2] = (Fixed32) LUT8.e12;
       NewLUT -> Matrix.v[2].n[0] = (Fixed32) LUT8.e20;
       NewLUT -> Matrix.v[2].n[1] = (Fixed32) LUT8.e21;
       NewLUT -> Matrix.v[2].n[2] = (Fixed32) LUT8.e22;


       // Only operates if not identity...

       if (!MAT3isIdentity(&NewLUT -> Matrix, 0.0001)) {

              NewLUT -> wFlags |= LUT_HASMATRIX;
       }


       // Copy input tables

       Temp = (LPBYTE) malloc(256);
       AllLinear = 0;
       for (i=0; i < NewLUT -> InputChan; i++) {

              PtrW = (LPWORD) malloc(sizeof(WORD) * 256);
              NewLUT -> L1[i] = PtrW;
              Icc ->Read(Temp, 1, 256, Icc -> stream);
              for (j=0; j < 256; j++)
                     PtrW[j] = TO16_TAB(Temp[j]);
                     AllLinear += cmsIsLinear(NewLUT -> L1[i], NewLUT -> InputEntries);
              }

       // Linear input, so ignore full step

       if (AllLinear == NewLUT -> InputChan) {

              NewLUT -> wFlags &= ~LUT_HASTL1;
       }

       free(Temp);

       // Copy 3D CLUT

       nTabSize = (NewLUT -> OutputChan * uipow(NewLUT->cLutPoints,
                                                NewLUT->InputChan));

       PtrW = (LPWORD) malloc(sizeof(WORD) * nTabSize);
       Temp = (LPBYTE) malloc(nTabSize);
       Icc ->Read(Temp, 1, nTabSize, Icc -> stream);

       NewLUT -> T = PtrW;
       NewLUT -> Tsize = nTabSize * sizeof(WORD);

       for (i = 0; i < nTabSize; i++) {

                     *PtrW++ = TO16_TAB(Temp[i]);
       }
       free(Temp);


       // Copy output tables

       Temp = (LPBYTE) malloc(256);
       AllLinear = 0;
       for (i=0; i < NewLUT -> OutputChan; i++) {

              PtrW = (LPWORD) malloc(sizeof(WORD) * 256);
              NewLUT -> L2[i] = PtrW;
              Icc ->Read(Temp, 1, 256, Icc -> stream);              
              for (j=0; j < 256; j++)
                     PtrW[j] = TO16_TAB(Temp[j]);
                     AllLinear += cmsIsLinear(NewLUT -> L2[i], 256);
              }

       // Linear input, so ignore full step

       if (AllLinear == NewLUT -> OutputChan) {

              NewLUT -> wFlags &= ~LUT_HASTL2;
       }


       free(Temp);

       cmsCalcL16Params(NewLUT -> InputEntries,  &NewLUT -> In16params);
       cmsCalcL16Params(NewLUT -> OutputEntries, &NewLUT -> Out16params);
       cmsCalcCLUT16Params(NewLUT -> cLutPoints,  NewLUT -> InputChan,
                                                  NewLUT -> OutputChan,
                                                  &NewLUT -> CLut16params);
       // Fixup
       FixLUT8(NewLUT, sig, nTabSize);

}




// Case LUT 16

static
void ReadLUT16(LPLCMSICCPROFILE Icc, LPLUT NewLUT)
{
    icLut16 LUT16;
    size_t nTabSize;
    unsigned int i;
    unsigned int AllLinear;
    LPWORD PtrW;


       Icc ->Read(&LUT16, sizeof(icLut16)- SIZEOF_UINT16_ALIGNED, 1, Icc -> stream);

       NewLUT -> wFlags        = LUT_HASTL1 | LUT_HASTL2 | LUT_HAS3DGRID;
       NewLUT -> cLutPoints    = LUT16.clutPoints;
       NewLUT -> InputChan     = LUT16.inputChan;
       NewLUT -> OutputChan    = LUT16.outputChan;

       AdjustEndianess16((LPBYTE) &LUT16.inputEnt);
       AdjustEndianess16((LPBYTE) &LUT16.outputEnt);

       NewLUT -> InputEntries  = LUT16.inputEnt;
       NewLUT -> OutputEntries = LUT16.outputEnt;


       // Matrix handling

       AdjustEndianess32((LPBYTE) &LUT16.e00);
       AdjustEndianess32((LPBYTE) &LUT16.e01);
       AdjustEndianess32((LPBYTE) &LUT16.e02);
       AdjustEndianess32((LPBYTE) &LUT16.e10);
       AdjustEndianess32((LPBYTE) &LUT16.e11);
       AdjustEndianess32((LPBYTE) &LUT16.e12);
       AdjustEndianess32((LPBYTE) &LUT16.e20);
       AdjustEndianess32((LPBYTE) &LUT16.e21);
       AdjustEndianess32((LPBYTE) &LUT16.e22);

       NewLUT -> Matrix.v[0].n[0] = (Fixed32) LUT16.e00;
       NewLUT -> Matrix.v[0].n[1] = (Fixed32) LUT16.e01;
       NewLUT -> Matrix.v[0].n[2] = (Fixed32) LUT16.e02;
       NewLUT -> Matrix.v[1].n[0] = (Fixed32) LUT16.e10;
       NewLUT -> Matrix.v[1].n[1] = (Fixed32) LUT16.e11;
       NewLUT -> Matrix.v[1].n[2] = (Fixed32) LUT16.e12;
       NewLUT -> Matrix.v[2].n[0] = (Fixed32) LUT16.e20;
       NewLUT -> Matrix.v[2].n[1] = (Fixed32) LUT16.e21;
       NewLUT -> Matrix.v[2].n[2] = (Fixed32) LUT16.e22;

       // Only operates if not identity...

       if (!MAT3isIdentity(&NewLUT -> Matrix, 0.0001)) {

              NewLUT -> wFlags |= LUT_HASMATRIX;
       }


       // Copy input tables

       AllLinear = 0;
       for (i=0; i < NewLUT -> InputChan; i++) {

              PtrW = (LPWORD) malloc(sizeof(WORD) * NewLUT -> InputEntries);
              NewLUT -> L1[i] = PtrW;
              Icc ->Read(PtrW, sizeof(WORD), NewLUT -> InputEntries, Icc -> stream);
              AdjustEndianessArray16(PtrW, NewLUT -> InputEntries);
              AllLinear += cmsIsLinear(NewLUT -> L1[i], NewLUT -> InputEntries);
              }

       // Linear input, so ignore full step

       if (AllLinear == NewLUT -> InputChan) {

              NewLUT -> wFlags &= ~LUT_HASTL1;
       }


       // Copy 3D CLUT

       nTabSize = (NewLUT -> OutputChan * uipow(NewLUT->cLutPoints,
                                                NewLUT->InputChan));
       PtrW = (LPWORD) malloc(sizeof(WORD) * nTabSize);

       NewLUT -> T = PtrW;
       NewLUT -> Tsize = nTabSize * sizeof(WORD);

       Icc -> Read(PtrW, sizeof(WORD), nTabSize, Icc -> stream);
       AdjustEndianessArray16(NewLUT -> T, nTabSize);

       // Copy output tables

       AllLinear = 0;
       for (i=0; i < NewLUT -> OutputChan; i++) {

              PtrW = (LPWORD) malloc(sizeof(WORD) * NewLUT -> OutputEntries);
              NewLUT -> L2[i] = PtrW;
              Icc ->Read(PtrW, sizeof(WORD), NewLUT -> OutputEntries, Icc -> stream);
              AdjustEndianessArray16(PtrW, NewLUT -> OutputEntries);
              AllLinear += cmsIsLinear(NewLUT -> L2[i], NewLUT -> OutputEntries);
              }

       // Linear output, ignore step

       if (AllLinear == NewLUT -> OutputChan)
       {
              NewLUT -> wFlags &= ~LUT_HASTL2;
       }


       cmsCalcL16Params(NewLUT -> InputEntries,  &NewLUT -> In16params);
       cmsCalcL16Params(NewLUT -> OutputEntries, &NewLUT -> Out16params);
       cmsCalcCLUT16Params(NewLUT -> cLutPoints,  NewLUT -> InputChan,
                                                  NewLUT -> OutputChan,
                                                  &NewLUT -> CLut16params);
}



LPLUT LCMSEXPORT cmsReadICCLut(cmsHPROFILE hProfile, icTagSignature sig)
{

    LPLCMSICCPROFILE    Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
    icTagBase           Base;
    int                 n;
    size_t              offset;
    LPLUT               NewLUT;

    n = SearchTag(Icc, sig);
    if (n < 0)
    {
    cmsSignalError(LCMS_ERRC_ABORTED, "Tag not found");
    return NULL;
    }


    // If is in memory, the LUT is already there, so throw a copy
    if (!Icc -> stream) {
                
        return cmsDupLUT((LPLUT) Icc ->TagPtrs[n]);
    }

    offset = Icc -> TagOffsets[n];


    if (Icc -> Seek(Icc -> stream, offset))
            return NULL;


    Icc ->Read(&Base, sizeof(icTagBase), 1, Icc -> stream);
    AdjustEndianess32((LPBYTE) &Base.sig);

    NewLUT = cmsAllocLUT();
    if (!NewLUT)
    {
       cmsSignalError(LCMS_ERRC_ABORTED, "cmsAllocLUT() failed");
       return NULL;
    }


    switch (Base.sig) {

    case icSigLut8Type: ReadLUT8(Icc, NewLUT, sig); break;
    case icSigLut16Type: ReadLUT16(Icc, NewLUT); break;

    default:  cmsSignalError(LCMS_ERRC_ABORTED, "Bad tag signature %lx found.", Base.sig);
              cmsFreeLUT(NewLUT);
              return NULL;
    }

    return NewLUT;
}


// Sets the language & country preferences. Used only in ICC 4.0 profiles

void LCMSEXPORT cmsSetLanguage(int LanguageCode, int CountryCode)
{
    GlobalLanguageCode = LanguageCode;
    GlobalCountryCode  = CountryCode;
}



// Take an ASCII item

static
int ReadICCAscii(cmsHPROFILE hProfile, icTagSignature sig, char *Name)
{
    LPLCMSICCPROFILE    Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
    icTagBase           Base;
    size_t              offset, size;
    int                 n;

    n = SearchTag(Icc, sig);
    if (n < 0)
    {
    cmsSignalError(LCMS_ERRC_ABORTED, "Tag not found");
    return -1;
    }


    if (!Icc -> stream)
    {
    CopyMemory(Name, Icc -> TagPtrs[n],
                     Icc -> TagSizes[n]);

    return Icc -> TagSizes[n];
    }

    offset = Icc -> TagOffsets[n];
    size   = Icc -> TagSizes[n];

    if (Icc -> Seek(Icc ->stream, offset))
            return -1;
        
    Icc ->Read(&Base, sizeof(icTagBase), 1, Icc -> stream);
    AdjustEndianess32((LPBYTE) &Base.sig);

    switch (Base.sig) {

    case icSigTextDescriptionType: {

           icUInt32Number  AsciiCount;

           Icc ->Read(&AsciiCount, sizeof(icUInt32Number), 1, Icc -> stream);
           AdjustEndianess32((LPBYTE) &AsciiCount);
           Icc ->Read(Name, 1, AsciiCount, Icc -> stream);
           size = AsciiCount;
           }
           break;


    case icSigCopyrightTag:   // Broken profiles from agfa does store copyright info in such type
    case icSigTextType:

           size -= sizeof(icTagBase);
           Icc -> Read(Name, 1, size, Icc -> stream);
           break;

    // multiLocalizedUnicodeType, V4 only

    case icSigMultiLocalizedUnicodeType: {

        icUInt32Number Count, RecLen;       
        icUInt16Number Language, Country;
        icUInt32Number ThisLen, ThisOffset;
        size_t         Offset = 0;
        size_t         Len    = 0;      
        size_t         i;
        wchar_t*       wchar = L"";
                    

            Icc ->Read(&Count, sizeof(icUInt32Number), 1, Icc -> stream);                   
            AdjustEndianess32((LPBYTE) &Count);
            Icc ->Read(&RecLen, sizeof(icUInt32Number), 1, Icc -> stream);                  
            AdjustEndianess32((LPBYTE) &RecLen);

            if (RecLen != 12) {

                    cmsSignalError(LCMS_ERRC_ABORTED, "multiLocalizedUnicodeType of len != 12 is not supported.");
                    return -1;
            }

            for (i=0; i < Count; i++) {
                
                Icc ->Read(&Language, sizeof(icUInt16Number), 1, Icc -> stream);                    
                AdjustEndianess16((LPBYTE) &Language);
                Icc ->Read(&Country, sizeof(icUInt16Number), 1, Icc -> stream);                 
                AdjustEndianess16((LPBYTE) &Country);
    
                Icc ->Read(&ThisLen, sizeof(icUInt32Number), 1, Icc -> stream);                 
                AdjustEndianess32((LPBYTE) &ThisLen);
    
                Icc ->Read(&ThisOffset, sizeof(icUInt32Number), 1, Icc -> stream);                  
                AdjustEndianess32((LPBYTE) &ThisOffset);
    
                if (Language == GlobalLanguageCode || Offset == 0) {

                    Len = ThisLen; Offset = ThisOffset; 
                    if (Country == GlobalCountryCode) 
                                    break;              // Found                        
                }
                        
            }
            

            if (Offset == 0) {

                    strcpy(Name, "(no info)");
                    break;
            }
            
            // Compute true offset
            Offset -= 12 * Count + 8 + sizeof(icTagBase);

            // Skip unused bytes
            for (i=0; i < Offset; i++) {
                    char Discard;

                    Icc ->Read(&Discard, 1, 1, Icc -> stream);
            }

            wchar = (wchar_t*) malloc(Len+2);
            if (!wchar) return -1;
            
            Icc ->Read(wchar, 1, Len, Icc ->stream);
            AdjustEndianessArray16((LPWORD) wchar, Len / 2);

            wchar[Len / 2] = L'\0';
            wcstombs(Name, wchar, 2047 );                       
            free((void*) wchar);
            }
            break;

    default:
              cmsSignalError(LCMS_ERRC_ABORTED, "Bad tag signature %lx found.", Base.sig);
              return -1;
    }

    return size;
}



// Take an XYZ item

static
int ReadICCXYZ(cmsHPROFILE hProfile, icTagSignature sig, LPcmsCIEXYZ Value, BOOL lIsFatal)
{
    LPLCMSICCPROFILE    Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
    icTagBase           Base;
    size_t              offset;
    int                 n;
    icXYZNumber         XYZ;

    n = SearchTag(Icc, sig);
    if (n < 0)
    {
    // Tag not found
    return -1;
    }


    if (!Icc -> stream) {

    CopyMemory(Value, Icc -> TagPtrs[n],
                      Icc -> TagSizes[n]);

    return Icc -> TagSizes[n];
    }

    offset = Icc -> TagOffsets[n];

    if (Icc -> Seek(Icc->stream, offset))
            return -1;

    
    Icc -> Read(&Base, 1, sizeof(icTagBase), Icc -> stream);
    AdjustEndianess32((LPBYTE) &Base.sig);

    switch (Base.sig)
    {
        
    case 0x7c3b10cL:    // Some apple broken embedded profiles does not have correct type   
    case icSigXYZType:

           Icc ->Read(&XYZ, sizeof(icXYZNumber), 1, Icc -> stream);
           Value -> X = Convert15Fixed16(XYZ.X);
           Value -> Y = Convert15Fixed16(XYZ.Y);
           Value -> Z = Convert15Fixed16(XYZ.Z);
           break;

    // Aug/21-2001 - Monaco 2 does have WRONG values.

    default:
           if (lIsFatal)
                cmsSignalError(LCMS_ERRC_ABORTED, "Bad tag signature %lx found.", Base.sig);
           return -1;
    }

    return 1;
}

// Jun-21-2000: Some profiles (those that comes with W2K) comes
// with the media white (media black?) x 100. Add a sanity check

static
void NormalizeXYZ(LPcmsCIEXYZ Dest)
{
    while (Dest -> X > 2. &&
           Dest -> Y > 2. &&
           Dest -> Z > 2.) {

               Dest -> X /= 10.;
               Dest -> Y /= 10.;
               Dest -> Z /= 10.;
       }
}


// White point must be in XYZ to avoid extra calculation on
// absolute intents

BOOL LCMSEXPORT cmsTakeMediaWhitePoint(LPcmsCIEXYZ Dest, cmsHPROFILE hProfile)
{
       if (ReadICCXYZ(hProfile, icSigMediaWhitePointTag, Dest, FALSE) < 0)
       {
              Dest->X = D50X;   // Default to D50
              Dest->Y = D50Y;
              Dest->Z = D50Z;
              return FALSE;
       }

       NormalizeXYZ(Dest);
       return TRUE;
}


BOOL LCMSEXPORT cmsTakeMediaBlackPoint(LPcmsCIEXYZ Dest, cmsHPROFILE hProfile)
{
       if (ReadICCXYZ(hProfile, icSigMediaBlackPointTag, Dest, FALSE) < 0)
       {
              Dest->X = 0.0;
              Dest->Y = 0.0;
              Dest->Z = 0.0;
              return FALSE;
       }


       NormalizeXYZ(Dest);
       return TRUE;
}

BOOL  LCMSEXPORT cmsTakeIluminant(LPcmsCIEXYZ Dest, cmsHPROFILE hProfile)
{
       LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;

       Dest -> X = Icc -> Illuminant.X;
       Dest -> Y = Icc -> Illuminant.Y;
       Dest -> Z = Icc -> Illuminant.Z;

       NormalizeXYZ(Dest);
       return TRUE;
}

int LCMSEXPORT cmsTakeRenderingIntent(cmsHPROFILE hProfile)
{
       LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;

       return Icc -> RenderingIntent;
}


// Primaries are to be in xyY notation

BOOL LCMSEXPORT cmsTakeColorants(LPcmsCIEXYZTRIPLE Dest, cmsHPROFILE hProfile)
{

       if (ReadICCXYZ(hProfile, icSigRedColorantTag, &Dest -> Red, TRUE) < 0) return FALSE;
       if (ReadICCXYZ(hProfile, icSigGreenColorantTag, &Dest -> Green, TRUE) < 0) return FALSE;
       if (ReadICCXYZ(hProfile, icSigBlueColorantTag, &Dest -> Blue, TRUE) < 0) return FALSE;


       return TRUE;

}


BOOL cmsReadICCMatrixRGB2XYZ(LPMAT3 r, cmsHPROFILE hProfile)
{
       cmsCIEXYZTRIPLE Primaries;

       if (!cmsTakeColorants(&Primaries, hProfile)) return FALSE;

       VEC3init(&r -> v[0], Primaries.Red.X, Primaries.Green.X,  Primaries.Blue.X);
       VEC3init(&r -> v[1], Primaries.Red.Y, Primaries.Green.Y,  Primaries.Blue.Y);
       VEC3init(&r -> v[2], Primaries.Red.Z, Primaries.Green.Z,  Primaries.Blue.Z);

       return TRUE;

}


LPGAMMATABLE LCMSEXPORT cmsReadICCGamma(cmsHPROFILE hProfile, icTagSignature sig)
{
       LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
       LPGAMMATABLE NewGamma;
       icUInt32Number Count;
       icTagBase      Base;
       size_t         offset;
       int            n;


       n = SearchTag(Icc, sig);
       if (n < 0) {

       cmsSignalError(LCMS_ERRC_ABORTED, "Tag not found");
       return NULL;
       }


       if (!Icc -> stream) {

       return cmsDupGamma(Icc -> TagPtrs[n]);
       }

       offset = Icc -> TagOffsets[n];

       if (Icc -> Seek(Icc ->stream, offset))
            return NULL;

       
       Icc ->Read(&Base, 1, sizeof(icTagBase), Icc -> stream);
       AdjustEndianess32((LPBYTE) &Base.sig);


       switch (Base.sig) {


       case 0x9478ee00L:    // Monaco 2 profiler is BROKEN!
       case icSigCurveType:

           Icc ->Read(&Count, sizeof(icUInt32Number), 1, Icc -> stream);
           AdjustEndianess32((LPBYTE) &Count);


           switch (Count) {

           case 0:   // Linear.

                     NewGamma = cmsAllocGamma(2);
                     if (!NewGamma) return NULL;
                     NewGamma -> GammaTable[0] = 0;
                     NewGamma -> GammaTable[1] = 0xFFFF;
                     return NewGamma;

           case 1:  {
                     WORD SingleGammaFixed;

                     Icc ->Read(&SingleGammaFixed, sizeof(WORD), 1, Icc -> stream);
                     AdjustEndianess16((LPBYTE) &SingleGammaFixed);
                     return cmsBuildGamma(4096, Convert8Fixed8(SingleGammaFixed));
                     }

           default: { // Curve
                    
                     NewGamma = cmsAllocGamma(Count);
                     if (!NewGamma) return NULL;

                     Icc ->Read(NewGamma -> GammaTable, sizeof(WORD), Count, Icc -> stream);
                     
                     AdjustEndianessArray16(NewGamma -> GammaTable, Count);

                     return NewGamma;
                    }
              }
              break;

    
       // Parametric curves
       case icSigParametricCurveType: {
           
           int ParamsByType[] = { 1, 3, 4, 5, 7 };
           double Params[10];
           icS15Fixed16Number Num;
           icUInt32Number Reserved;
           icUInt16Number   Type;
           int i;
           
           Icc -> Read(&Type, sizeof(icUInt16Number), 1, Icc -> stream);
           Icc -> Read(&Reserved, sizeof(icUInt16Number), 1, Icc -> stream);
           
           AdjustEndianess16((LPBYTE) &Type);
           if (Type > 5) {

                cmsSignalError(LCMS_ERRC_ABORTED, "Unknown parametric curve type '%d' found.", Type);
                return NULL;
           }
        
          ZeroMemory(Params, 10* sizeof(double));
          n = ParamsByType[Type];

          for (i=0; i < n; i++) {
                Num = 0;
                Icc -> Read(&Num, sizeof(icS15Fixed16Number), 1, Icc -> stream);
                Params[i] = Convert15Fixed16(Num);
          }


           NewGamma = cmsBuildParametricGamma(4096, Type+1, Params);
           return NewGamma;
          }
    

       default:
              cmsSignalError(LCMS_ERRC_ABORTED, "Bad tag signature '%lx' found.", Base.sig);
              return NULL;
       }

       // It would never reach here
       // return NULL;
}



// Some ways have analytical revese. This function accounts for that

LPGAMMATABLE LCMSEXPORT cmsReadICCGammaReversed(cmsHPROFILE hProfile, icTagSignature sig)
{
       LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
       LPGAMMATABLE   NewGamma, ReturnGamma;
       icUInt32Number Count;
       icTagBase      Base;
       size_t         offset;
       int            n;


       n = SearchTag(Icc, sig);
       if (n < 0) {

       cmsSignalError(LCMS_ERRC_ABORTED, "Tag not found");
       return NULL;
       }


       if (!Icc -> stream) {

       return cmsReverseGamma(256, Icc -> TagPtrs[n]);
       }

       offset = Icc -> TagOffsets[n];

       if (Icc -> Seek(Icc ->stream, offset))
            return NULL;
       
       Icc -> Read(&Base, 1, sizeof(icTagBase), Icc -> stream);
       AdjustEndianess32((LPBYTE) &Base.sig);


       switch (Base.sig) {


       case 0x9478ee00L:    // Monaco 2 profiler is BROKEN!
       case icSigCurveType:

           Icc -> Read(&Count, sizeof(icUInt32Number), 1, Icc -> stream);
           AdjustEndianess32((LPBYTE) &Count);


           switch (Count) {

           case 0:   // Linear, reverse is same.

                     NewGamma = cmsAllocGamma(2);
                     if (!NewGamma) return NULL;
                     NewGamma -> GammaTable[0] = 0;
                     NewGamma -> GammaTable[1] = 0xFFFF;
                     return NewGamma;

           case 1:  {
                     WORD SingleGammaFixed;

                     Icc -> Read(&SingleGammaFixed, sizeof(WORD), 1, Icc -> stream);
                     AdjustEndianess16((LPBYTE) &SingleGammaFixed);
                     return cmsBuildGamma(4096, 1./Convert8Fixed8(SingleGammaFixed));
                     }

           default: { // Curve. Do our best to trying to reverse the curve
                    
                     NewGamma = cmsAllocGamma(Count);
                     if (!NewGamma) return NULL;

                     Icc -> Read(NewGamma -> GammaTable, sizeof(WORD), Count, Icc -> stream);
                     
                     AdjustEndianessArray16(NewGamma -> GammaTable, Count);

                     ReturnGamma = cmsReverseGamma(Count, NewGamma);
                     cmsFreeGamma(NewGamma);

                     return ReturnGamma;
                    }
              }
              break;

    
       // Parametric curves
       case icSigParametricCurveType: {
           
           int ParamsByType[] = { 1, 3, 4, 5, 7 };
           double Params[10];
           icS15Fixed16Number Num;
           icUInt32Number Reserved;           
           icUInt16Number   Type;
           int i;


           Icc -> Read(&Type, sizeof(icUInt16Number), 1, Icc -> stream);
           Icc -> Read(&Reserved, sizeof(icUInt16Number), 1, Icc -> stream);
           
           AdjustEndianess16((LPBYTE) &Type);
           if (Type > 5) {

                cmsSignalError(LCMS_ERRC_ABORTED, "Unknown parametric curve type '%d' found.", Type);
                return NULL;
           }
        
          ZeroMemory(Params, 10* sizeof(double));
          n = ParamsByType[Type];

          for (i=0; i < n; i++) {
                Icc -> Read(&Num, sizeof(icS15Fixed16Number), 1, Icc -> stream);
                Params[i] = Convert15Fixed16(Num);
          }


           // Negative type as a mark of reversed curve
           NewGamma = cmsBuildParametricGamma(4096, -(Type+1), Params);
           return NewGamma;
          }
    

       default:
              cmsSignalError(LCMS_ERRC_ABORTED, "Bad tag signature '%lx' found.", Base.sig);
              return NULL;
       }
       
}

// Check Named color header

static
BOOL CheckHeader(LPcmsNAMEDCOLORLIST v, icNamedColor2* nc2)
{
    if (v ->Prefix[0] == 0 && v ->Suffix[0] == 0 && v ->ColorantCount == 0) return TRUE;

    if (stricmp(v ->Prefix, nc2 ->prefix) != 0) return FALSE;
    if (stricmp(v ->Suffix, nc2 ->suffix) != 0) return FALSE;

    return ((int) v ->ColorantCount == (int) nc2 ->nDeviceCoords);
}

// Read named color list

int cmsReadICCnamedColorList(cmsHTRANSFORM xform, cmsHPROFILE hProfile, icTagSignature sig)
{
       _LPcmsTRANSFORM v = (_LPcmsTRANSFORM) xform;

       LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
       int            n;
       icTagBase      Base;
       size_t         offset;
       
       n = SearchTag(Icc, sig);
       if (n < 0) {

       cmsSignalError(LCMS_ERRC_WARNING, "Named color tag not found");
       return 0;
       }

       if (!Icc -> stream) {
            cmsSignalError(LCMS_ERRC_ABORTED, "In memory named color not yet implemented");
            return 0;
       }

       offset = Icc -> TagOffsets[n];

       if (Icc -> Seek(Icc ->stream, offset))
            return 0;

       Icc -> Read(&Base, 1, sizeof(icTagBase), Icc -> stream);
       AdjustEndianess32((LPBYTE) &Base.sig);

       switch (Base.sig) {

        // I never have seen one of these. Probably is not worth of implementing.

       case icSigNamedColorType: {

              cmsSignalError(LCMS_ERRC_WARNING, "Ancient named color profiles are not supported.");
              return 0;                 
            }
           
        // The named color struct
        
       case icSigNamedColor2Type: {

                icNamedColor2 nc2;
                unsigned int i, j;
                                
                Icc -> Read(&nc2, sizeof(icNamedColor2) - SIZEOF_UINT8_ALIGNED, 1, Icc -> stream);              
                AdjustEndianess32((LPBYTE) &nc2.vendorFlag);
                AdjustEndianess32((LPBYTE) &nc2.count);
                AdjustEndianess32((LPBYTE) &nc2.nDeviceCoords);

                if (!CheckHeader(v->NamedColorList, &nc2)) {
                     cmsSignalError(LCMS_ERRC_WARNING, "prefix/suffix/device for named color profiles mismatch.");
                }

                strncpy(v ->NamedColorList->Prefix, nc2.prefix, 32);
                strncpy(v ->NamedColorList->Suffix, nc2.suffix, 32);
                v ->NamedColorList->Prefix[32] = v->NamedColorList->Suffix[32] = 0;
                
                v ->NamedColorList ->ColorantCount = nc2.nDeviceCoords;
                
                for (i=0; i < nc2.count; i++) {

                    WORD PCS[3];
                    WORD Colorant[MAXCHANNELS];
                    char Root[33];

                    ZeroMemory(Colorant, sizeof(WORD) * MAXCHANNELS);
                    Icc -> Read(Root, 1, 32, Icc ->stream);
                    Icc -> Read(PCS,  3, sizeof(WORD), Icc ->stream);

                    for (j=0; j < 3; j++)
                        AdjustEndianess16((LPBYTE) &PCS[j]);
                    
                    Icc -> Read(Colorant, sizeof(WORD), nc2.nDeviceCoords, Icc ->stream);

                    for (j=0; j < nc2.nDeviceCoords; j++) 
                            AdjustEndianess16((LPBYTE) &Colorant[j]);
                    
                    cmsAppendNamedColor(v, Root, PCS, Colorant);
                }

            return v ->NamedColorList->nColors;
            }
            break;

       default:
              cmsSignalError(LCMS_ERRC_WARNING, "Bad tag signature '%lx' found.", Base.sig);
              return 0;
       }

       // It would never reach here
       // return 0;
}



icColorSpaceSignature LCMSEXPORT cmsGetPCS(cmsHPROFILE hProfile)
{
       LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;

       return Icc -> PCS;
}


void LCMSEXPORT cmsSetPCS(cmsHPROFILE hProfile, icColorSpaceSignature pcs)
{
       LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;

       Icc -> PCS = pcs;
}



icColorSpaceSignature LCMSEXPORT cmsGetColorSpace(cmsHPROFILE hProfile)
{
       LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;

       return Icc -> ColorSpace;
}



void LCMSEXPORT cmsSetColorSpace(cmsHPROFILE hProfile, icColorSpaceSignature sig)
{
       LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;

       Icc -> ColorSpace = sig;
}

// Check existance

BOOL LCMSEXPORT cmsIsTag(cmsHPROFILE hProfile, icTagSignature sig)
{
       LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;

       return SearchTag(Icc, sig) >= 0;

}

icProfileClassSignature LCMSEXPORT cmsGetDeviceClass(cmsHPROFILE hProfile)
{
       LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;

       return Icc -> DeviceClass;
}

void LCMSEXPORT cmsSetDeviceClass(cmsHPROFILE hProfile, icProfileClassSignature sig)
{
       LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
       Icc -> DeviceClass = sig;
}



// We compute name with model - manufacturer

const char*  LCMSEXPORT cmsTakeProductName(cmsHPROFILE hProfile)
{
       static char Name[2048];
       char Manufacturer[512], Model[512];

       Name[0] = '\0';
       Manufacturer[0] = Model[0] = '\0';

       if (cmsIsTag(hProfile, icSigDeviceMfgDescTag))
       {
       ReadICCAscii(hProfile, icSigDeviceMfgDescTag, Manufacturer);
       }

       if (cmsIsTag(hProfile, icSigDeviceModelDescTag))
       {
       ReadICCAscii(hProfile, icSigDeviceModelDescTag, Model);
       }

       if (!Manufacturer[0] && !Model[0])
       {
              if (cmsIsTag(hProfile, icSigProfileDescriptionTag))
              {
              ReadICCAscii(hProfile, icSigProfileDescriptionTag, Name);
              return Name;
              }
              else return "{no name}";
       }


       if (!Manufacturer[0] || strncmp(Model, Manufacturer, 8) == 0 ||
                               strlen(Model) > 30)
              strcpy(Name, Model);
       else
              sprintf(Name, "%s - %s", Model, Manufacturer);

       return Name;

}


// We compute desc with manufacturer - model

const char*  LCMSEXPORT cmsTakeProductDesc(cmsHPROFILE hProfile)
{
       static char Name[2048];

       if (cmsIsTag(hProfile, icSigProfileDescriptionTag)) {

              ReadICCAscii(hProfile, icSigProfileDescriptionTag, Name);
       }
       else return cmsTakeProductName(hProfile);

       if (strncmp(Name, "Copyrig", 7) == 0)
              return cmsTakeProductName(hProfile);

       return Name;
}


const char*  LCMSEXPORT cmsTakeProductInfo(cmsHPROFILE hProfile)
{
       // Simply typecast
       LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;

       static char Info[4096];

       Info[0] = '\0';

       if (cmsIsTag(hProfile, icSigProfileDescriptionTag))
       {
       char Desc[1024];

       ReadICCAscii(hProfile, icSigProfileDescriptionTag, Desc);
       strcat(Info, Desc);
       strcat(Info, "\r\n\r\n");
       }


       if (cmsIsTag(hProfile, icSigCopyrightTag))
       {
       char Copyright[2048];

       ReadICCAscii(hProfile, icSigCopyrightTag, Copyright);
       strcat(Info, Copyright);
       strcat(Info, "\r\n\r\n");
       }



// KODAK private tag... But very useful

#define K007         (icTagSignature)0x4B303037

       // MonCal

       if (cmsIsTag(hProfile, K007))
       {
       char MonCal[1024];

       ReadICCAscii(hProfile, K007, MonCal);
       strcat(Info, MonCal);
       strcat(Info, "\r\n\r\n");
       }
       else
       {
       cmsCIEXYZ WhitePt;
       char WhiteStr[1024];

       cmsTakeMediaWhitePoint(&WhitePt, hProfile);
       _cmsIdentifyWhitePoint(WhiteStr, &WhitePt);
       strcat(WhiteStr, "\r\n\r\n");
       strcat(Info, WhiteStr);
       }


       if (Icc -> stream) {
              strcat(Info, Icc -> PhysicalFile);
       }
       return Info;
}

// Extract the target data as a big string. Does not signal if tag is not present.

BOOL LCMSEXPORT cmsTakeCharTargetData(cmsHPROFILE hProfile, char** Data, size_t* len)
{
    LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
    int n;

    *Data = NULL;
    *len  = 0;

    n = SearchTag(Icc, icSigCharTargetTag);
    if (n < 0) return FALSE;                

    *len =  Icc -> TagSizes[n];
    *Data = (char*) malloc(*len + 1);

    if (!*Data) {

        cmsSignalError(-1, "Out of memory allocating CharTarget space!");
        return FALSE;
    }

    if (ReadICCAscii(hProfile, icSigCharTargetTag, *Data) < 0) 
        return FALSE;

    (*Data)[*len] = 0;  // Force a zero marker. Shouldn't be needed, but is 
                        // here for simplify things.

    return TRUE;    
}


// Write profile ------------------------------------------------------------

// Transports to properly encoded values - note that icc profiles does use
// big endian notation.

static
icInt32Number TransportValue32(icInt32Number Value)
{
       icInt32Number Temp = Value;

       AdjustEndianess32((LPBYTE) &Temp);
       return Temp;
}

static
WORD TransportValue16(WORD Value)
{
       WORD Temp = Value;

       AdjustEndianess16((LPBYTE) &Temp);
       return Temp;
}




static
BOOL SaveWordsTable(FILE* OutStream, int nEntries, LPWORD Tab, LPLCMSICCPROFILE Icc)
{
   size_t nTabSize = sizeof(WORD) * nEntries;
   LPWORD PtrW = (LPWORD) malloc(nTabSize);

   if (!PtrW) return FALSE;
   CopyMemory(PtrW, Tab, nTabSize);
   AdjustEndianessArray16(PtrW, nEntries);
   Icc ->Write(OutStream, nTabSize, PtrW);
   free(PtrW);
   
   return TRUE;
}

// Encodes now to date/time field

static
void EncodeDateTime(icDateTimeNumber* DateTime)
{
       time_t timer;
       struct tm *tmstr;

       time(&timer);
       tmstr = localtime(&timer);

       DateTime -> year    = TransportValue16((WORD) (tmstr -> tm_year + 1900));
       DateTime -> month   = TransportValue16((WORD) (tmstr -> tm_mon + 1));
       DateTime -> day     = TransportValue16((WORD) tmstr -> tm_mday);
       DateTime -> hours   = TransportValue16((WORD) tmstr -> tm_hour);
       DateTime -> minutes = TransportValue16((WORD) tmstr -> tm_min);
       DateTime -> seconds = TransportValue16((WORD) tmstr -> tm_sec);

}



// Saves profile header

static
BOOL SaveHeader(void *OutStream, LPLCMSICCPROFILE Icc)
{
  icHeader Header;

       Header.size        = TransportValue32(UsedSpace);
       Header.cmmId       = TransportValue32(lcmsSignature);
       Header.version     = TransportValue32(0x02300000);
       Header.deviceClass = TransportValue32(Icc -> DeviceClass);
       Header.colorSpace  = TransportValue32(Icc -> ColorSpace);
       Header.pcs         = TransportValue32(Icc -> PCS);

       EncodeDateTime(&Header.date);

       Header.magic       = TransportValue32(icMagicNumber);
       Header.platform    = (icPlatformSignature)TransportValue32(icSigMicrosoft);  // Sorry, I must put something here

       Header.flags        = TransportValue32(Icc -> flags);
       Header.manufacturer = TransportValue32(lcmsSignature);
       Header.model        = TransportValue32(0);
       Header.attributes[0]= TransportValue32(0);              // Reflective, Glossy
       Header.attributes[1]= TransportValue32(0);

       Header.renderingIntent = TransportValue32(Icc -> RenderingIntent);

       // Illuminant is D50

       Header.illuminant.X = TransportValue32(DOUBLE_TO_FIXED(Icc -> Illuminant.X));
       Header.illuminant.Y = TransportValue32(DOUBLE_TO_FIXED(Icc -> Illuminant.Y));
       Header.illuminant.Z = TransportValue32(DOUBLE_TO_FIXED(Icc -> Illuminant.Z));

       Header.creator      = TransportValue32(lcmsSignature);

       ZeroMemory(&Header.reserved, sizeof(Header.reserved));


       UsedSpace = 0; // Mark as begin-of-file

       return Icc ->Write(OutStream, sizeof(icHeader), &Header);
}



// Setup base marker

static
BOOL SetupBase(FILE *OutStream, icTagTypeSignature sig, LPLCMSICCPROFILE Icc)
{
    icTagBase  Base;

    Base.sig = TransportValue32(sig);
    ZeroMemory(&Base.reserved, sizeof(Base.reserved));
    return Icc -> Write(OutStream, sizeof(icTagBase), &Base);
}


// Store an XYZ tag

static
BOOL SaveXYZNumber(FILE *OutStream, LPcmsCIEXYZ Value, LPLCMSICCPROFILE Icc)
{

    icXYZNumber XYZ;

    if (!SetupBase(OutStream, icSigXYZType, Icc)) return FALSE;

    XYZ.X = TransportValue32(DOUBLE_TO_FIXED(Value -> X));
    XYZ.Y = TransportValue32(DOUBLE_TO_FIXED(Value -> Y));
    XYZ.Z = TransportValue32(DOUBLE_TO_FIXED(Value -> Z));


    return Icc -> Write(OutStream, sizeof(icXYZNumber), &XYZ);
}



// Store a curve type.

static
BOOL SaveGamma(FILE *OutStream, LPGAMMATABLE Gamma, LPLCMSICCPROFILE Icc)
{
    icInt32Number Count;
    int i;


    if (!SetupBase(OutStream, icSigCurveType, Icc)) return FALSE;

    Count = TransportValue32(Gamma->nEntries);

    if (!Icc ->Write(OutStream, sizeof(icInt32Number), &Count)) return FALSE;

    for (i=0; i < Gamma->nEntries; i++)
    {
    WORD Val = TransportValue16(Gamma->GammaTable[i]);

    if (!Icc -> Write(OutStream, sizeof(WORD), &Val))
              return FALSE;
    }

    return TRUE;
}




// Save an DESC Tag 

static
BOOL SaveDescription(FILE *OutStream, const char *Text, LPLCMSICCPROFILE Icc)
{

    size_t len, Count, TotalSize, AlignedSize, FillerSize;
    char Filler[80];

    len = strlen(Text) + 1;

    TotalSize = sizeof(icTagBase) + sizeof(size_t) + 71 + len;
    AlignedSize = ALIGNLONG(TotalSize);
    FillerSize  = AlignedSize - TotalSize;

    if (!SetupBase(OutStream, icSigTextDescriptionType, Icc)) return FALSE;

    Count = TransportValue32(len);
    if (!Icc ->Write(OutStream, sizeof(size_t), &Count)) return FALSE;
    if (!Icc ->Write(OutStream, len, (LPVOID)Text)) return FALSE;

    ZeroMemory(Filler, 80);
    if (!Icc ->Write(OutStream, FillerSize, Filler)) return FALSE;

    return Icc ->Write(OutStream, 71, Filler);
}

// Save an ASCII Tag 

static
BOOL SaveText(FILE *OutStream, const char *Text, LPLCMSICCPROFILE Icc)
{
    size_t len = strlen(Text) + 1;

    if (!SetupBase(OutStream, icSigTextType, Icc)) return FALSE;
    if (!Icc ->Write(OutStream, len, (LPVOID) Text)) return FALSE;
    return TRUE;
}


// Save one of these new chromaticity values

static
BOOL SaveOneChromaticity(FILE *OutStream, double x, double y, LPLCMSICCPROFILE Icc)
{
       Fixed32 xf, yf;

       xf = TransportValue32(DOUBLE_TO_FIXED(x));
       yf = TransportValue32(DOUBLE_TO_FIXED(y));

       if (!Icc ->Write(OutStream, sizeof(Fixed32), &xf)) return FALSE;
       if (!Icc ->Write(OutStream, sizeof(Fixed32), &yf)) return FALSE;

       return TRUE;
}


// New tag added in Addendum II of old spec.

static
BOOL SaveChromaticities(FILE *OutStream, LPcmsCIExyYTRIPLE chrm, LPLCMSICCPROFILE Icc)
{
       WORD nChans, Table;
       DWORD Reserved = 0;

       if (!SetupBase(OutStream, icSigChromaticityType, Icc)) return FALSE;

       if (!Icc ->Write(OutStream, sizeof(DWORD) , &Reserved)) return FALSE;
       nChans = TransportValue16(3);
       if (!Icc ->Write(OutStream, sizeof(WORD) , &nChans)) return FALSE;
       Table =  TransportValue16(0);
       if (!Icc ->Write(OutStream, sizeof(WORD) , &Table)) return FALSE;

       if (!SaveOneChromaticity(OutStream, chrm -> Red.x, chrm -> Red.y, Icc)) return FALSE;
       if (!SaveOneChromaticity(OutStream, chrm -> Green.x, chrm -> Green.y, Icc)) return FALSE;
       if (!SaveOneChromaticity(OutStream, chrm -> Blue.x, chrm -> Blue.y, Icc)) return FALSE;

       return TRUE;
}


// Does serialization of LUT and writes to disk. I'm always using LUT16 type
// because it seems to me (only a feeling, not a proven fact) that the
// interpolation makes more accurate a table of 16 bps that same with double
// of nodes on LUT8. Anyway, this should be regarded more carefully

static
BOOL SaveLUT(FILE* OutStream, const LPLUT NewLUT, LPLCMSICCPROFILE Icc)
{
       icLut16 LUT16;
       unsigned int i;
       size_t nTabSize;
       WORD NullTbl[2] = { 0, 0xFFFFU};


       if (!SetupBase(OutStream, icSigLut16Type, Icc)) return FALSE;

       LUT16.clutPoints = (icUInt8Number) NewLUT -> cLutPoints;
       LUT16.inputChan  = (icUInt8Number) NewLUT -> InputChan;
       LUT16.outputChan = (icUInt8Number) NewLUT -> OutputChan;

       LUT16.inputEnt   = TransportValue16((WORD) ((NewLUT -> wFlags & LUT_HASTL1) ? NewLUT -> InputEntries  : 2));
       LUT16.outputEnt  = TransportValue16((WORD) ((NewLUT -> wFlags & LUT_HASTL2) ? NewLUT -> OutputEntries : 2));

       if (NewLUT -> wFlags & LUT_HASMATRIX) {

       LUT16.e00 = TransportValue32(NewLUT -> Matrix.v[0].n[0]);
       LUT16.e01 = TransportValue32(NewLUT -> Matrix.v[0].n[1]);
       LUT16.e02 = TransportValue32(NewLUT -> Matrix.v[0].n[2]);
       LUT16.e10 = TransportValue32(NewLUT -> Matrix.v[1].n[0]);
       LUT16.e11 = TransportValue32(NewLUT -> Matrix.v[1].n[1]);
       LUT16.e12 = TransportValue32(NewLUT -> Matrix.v[1].n[2]);
       LUT16.e20 = TransportValue32(NewLUT -> Matrix.v[2].n[0]);
       LUT16.e21 = TransportValue32(NewLUT -> Matrix.v[2].n[1]);
       LUT16.e22 = TransportValue32(NewLUT -> Matrix.v[2].n[2]);
       }
       else {

       LUT16.e00 = TransportValue32(DOUBLE_TO_FIXED(1));
       LUT16.e01 = TransportValue32(DOUBLE_TO_FIXED(0));
       LUT16.e02 = TransportValue32(DOUBLE_TO_FIXED(0));
       LUT16.e10 = TransportValue32(DOUBLE_TO_FIXED(0));
       LUT16.e11 = TransportValue32(DOUBLE_TO_FIXED(1));
       LUT16.e12 = TransportValue32(DOUBLE_TO_FIXED(0));
       LUT16.e20 = TransportValue32(DOUBLE_TO_FIXED(0));
       LUT16.e21 = TransportValue32(DOUBLE_TO_FIXED(0));
       LUT16.e22 = TransportValue32(DOUBLE_TO_FIXED(1));
       }


       // Save header

       Icc -> Write(OutStream,  sizeof(icLut16)- SIZEOF_UINT16_ALIGNED, &LUT16);

       // The prelinearization table

       for (i=0; i < NewLUT -> InputChan; i++) {

        if (NewLUT -> wFlags & LUT_HASTL1) {

               if (!SaveWordsTable(OutStream, 
                                NewLUT -> InputEntries, 
                                NewLUT -> L1[i], Icc)) return FALSE;

        }
        else Icc -> Write(OutStream, sizeof(WORD)* 2, NullTbl);
       }


       nTabSize = (NewLUT -> OutputChan * uipow(NewLUT->cLutPoints,
                                                 NewLUT->InputChan));
       // The 3D CLUT.

       if (!SaveWordsTable(OutStream, nTabSize, NewLUT -> T, Icc)) return FALSE;

       // The postlinearization table

       for (i=0; i < NewLUT -> OutputChan; i++) {

        if (NewLUT -> wFlags & LUT_HASTL2) {

                if (!SaveWordsTable(OutStream,  
                                    NewLUT -> OutputEntries, 
                                    NewLUT -> L2[i], Icc)) return FALSE;
        }
        else Icc -> Write(OutStream, sizeof(WORD)* 2, NullTbl);

       }

        return TRUE;
}

// Saves Tag directory

static
BOOL SaveTagDirectory(void *OutStream, LPLCMSICCPROFILE Icc)
{
       icInt32Number i;
       icTag Tag;       
       icInt32Number Count = 0;

       // Get true count
       for (i=0;  i < Icc -> TagCount; i++) {
            if (Icc ->TagNames[i] != 0)
                    Count++;
       }

       Count = TransportValue32(Count);
       if (!Icc ->Write(OutStream, sizeof(icInt32Number) , &Count)) return FALSE;

       for (i=0; i < Icc -> TagCount; i++) {

          if (Icc ->TagNames[i] == 0) continue;

          Tag.sig    = (icTagSignature)TransportValue32(Icc -> TagNames[i]);
          Tag.offset = TransportValue32(Icc -> TagOffsets[i]);
          Tag.size   = TransportValue32(Icc -> TagSizes[i]);

          if (!Icc ->Write(OutStream, sizeof(icTag), &Tag)) return FALSE;
       }

       return TRUE;
}


// Dump tag contents

static
BOOL SaveTags(void *OutStream, LPLCMSICCPROFILE Icc)
{

    LPBYTE Data;
    icInt32Number i;
    size_t Begin;
    size_t AlignedSpace, FillerSize;


    for (i=0; i < Icc -> TagCount; i++) {

         if (Icc ->TagNames[i] == 0) continue;
        
        // Align to DWORD boundary, following new spec.
        
        AlignedSpace = ALIGNLONG(UsedSpace);
        FillerSize  = AlignedSpace - UsedSpace;
        if (FillerSize > 0)  {
            
            BYTE Filler[20];

            ZeroMemory(Filler, 16);
            if (!Icc ->Write(OutStream, FillerSize, Filler)) return FALSE;
        }
        
        
       Icc -> TagOffsets[i] = Begin = UsedSpace;
       Data = (LPBYTE) Icc -> TagPtrs[i];
       if (!Data)
              continue;

       switch (Icc -> TagNames[i]) {

       case icSigProfileDescriptionTag: 
       case icSigDeviceMfgDescTag:
       case icSigDeviceModelDescTag:
              if (!SaveDescription(OutStream, (const char *) Data, Icc)) return FALSE;
              break;

       case icSigRedColorantTag:
       case icSigGreenColorantTag:
       case icSigBlueColorantTag:
       case icSigMediaWhitePointTag:
       case icSigMediaBlackPointTag:           
               if (!SaveXYZNumber(OutStream, (LPcmsCIEXYZ) Data, Icc)) return FALSE;
               break;


       case icSigRedTRCTag:
       case icSigGreenTRCTag:
       case icSigBlueTRCTag:
       case icSigGrayTRCTag:
               if (!SaveGamma(OutStream, (LPGAMMATABLE) Data, Icc)) return FALSE;
               break;

       case icSigCharTargetTag:
       case icSigCopyrightTag:      
              if (!SaveText(OutStream, (const char *) Data, Icc)) return FALSE;
              break;

       case icSigChromaticityTag:
              if (!SaveChromaticities(OutStream, (LPcmsCIExyYTRIPLE) Data, Icc)) return FALSE;
              break;

       // Save LUT 

       case icSigAToB0Tag:
       case icSigAToB1Tag:
       case icSigAToB2Tag:
       case icSigBToA0Tag:
       case icSigBToA1Tag:
       case icSigBToA2Tag:
       case icSigGamutTag:
       case icSigPreview0Tag:
       case icSigPreview1Tag:
       case icSigPreview2Tag:
              if (!SaveLUT(OutStream, (LPLUT) Data, Icc)) return FALSE;
              break;


       default:
              return FALSE;
       }

       Icc -> TagSizes[i] = (UsedSpace - Begin);
       }

        

       return TRUE;
}

BOOL LCMSEXPORT _cmsAddXYZTag(cmsHPROFILE hProfile, icTagSignature sig, const LPcmsCIEXYZ XYZ)
{
       LPLCMSICCPROFILE Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
       
       InitTag(Icc, sig, sizeof(cmsCIEXYZ), XYZ);
       return TRUE;
}


BOOL LCMSEXPORT _cmsAddTextTag(cmsHPROFILE hProfile, icTagSignature sig, const char* Text)
{
       LPLCMSICCPROFILE Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;

       InitTag(Icc, sig, strlen(Text)+1, (LPVOID) Text);
       return TRUE;
}


static
LPVOID DupBlock(LPLCMSICCPROFILE Icc, LPVOID Block, size_t size)
{
    if (Block != NULL && size > 0) 
        return InitTag(Icc, 0, size, Block);
    else
        return NULL;

}

// This is tricky, since LUT structs does have pointers

BOOL LCMSEXPORT _cmsAddLUTTag(cmsHPROFILE hProfile, icTagSignature sig, LPVOID lut)
{
       LPLCMSICCPROFILE Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
       LPLUT Orig, Stored;
       unsigned int i;
       
       // The struct itself

       Orig   = (LPLUT) lut;
       Stored = InitTag(Icc, sig, sizeof(LUT), lut);

       // dup' the memory blocks
       for (i=0; i < Orig ->InputChan; i++) 
            Stored -> L1[i] = DupBlock(Icc, Orig ->L1[i], sizeof(WORD) * Orig ->In16params.nSamples);

       for (i=0; i < Orig ->OutputChan; i++)
            Stored -> L2[i] = DupBlock(Icc, Orig ->L2[i], sizeof(WORD) * Orig ->Out16params.nSamples);  
       
       Stored -> T     = DupBlock(Icc, Orig ->T, Orig -> Tsize);

       return TRUE;
}


BOOL LCMSEXPORT _cmsAddGammaTag(cmsHPROFILE hProfile, icTagSignature sig, LPGAMMATABLE TransferFunction)
{
    LPLCMSICCPROFILE Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;

    InitTag(Icc, sig, SizeOfGammaTab(TransferFunction), TransferFunction);
    return TRUE;
}


BOOL LCMSEXPORT _cmsAddChromaticityTag(cmsHPROFILE hProfile, icTagSignature sig, LPcmsCIExyYTRIPLE Chrm)
{
    LPLCMSICCPROFILE Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;

    InitTag(Icc, sig, sizeof(cmsCIExyYTRIPLE), Chrm);
    return TRUE;
}


// Add tags to profile structure

BOOL LCMSEXPORT cmsAddTag(cmsHPROFILE hProfile, icTagSignature sig, LPVOID Tag)
{
   switch (sig) {

       case icSigCharTargetTag:
       case icSigCopyrightTag:             
       case icSigProfileDescriptionTag: 
       case icSigDeviceMfgDescTag:
       case icSigDeviceModelDescTag:
              return _cmsAddTextTag(hProfile, sig, (const char*) Tag);

       case icSigRedColorantTag:
       case icSigGreenColorantTag:
       case icSigBlueColorantTag:
       case icSigMediaWhitePointTag:
       case icSigMediaBlackPointTag:           
              return _cmsAddXYZTag(hProfile, sig, (const LPcmsCIEXYZ) Tag);
               

       case icSigRedTRCTag:
       case icSigGreenTRCTag:
       case icSigBlueTRCTag:
       case icSigGrayTRCTag:
              return _cmsAddGammaTag(hProfile, sig, (LPGAMMATABLE) Tag);
                     
       case icSigAToB0Tag:
       case icSigAToB1Tag:
       case icSigAToB2Tag:
       case icSigBToA0Tag:
       case icSigBToA1Tag:
       case icSigBToA2Tag:
       case icSigGamutTag:
       case icSigPreview0Tag:
       case icSigPreview1Tag:
       case icSigPreview2Tag:
              return _cmsAddLUTTag(hProfile, sig, Tag);
              

       case icSigChromaticityTag:
              return _cmsAddChromaticityTag(hProfile, sig, (LPcmsCIExyYTRIPLE) Tag);              
        
       default:
            cmsSignalError(-1, "cmsAddTag: Tag '%x' is unsupported", sig);
            return FALSE;
   }

}

// Low-level save to disk
BOOL LCMSEXPORT _cmsSaveProfile(cmsHPROFILE hProfile, const char* FileName)
{
       FILE *OutStream;
       LPLCMSICCPROFILE Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;


       Icc ->Write       = FileWrite;   // Save to disk
             
       // Pass #1 does compute offsets
     
       if (!SaveHeader(NULL, Icc)) return FALSE;
       if (!SaveTagDirectory(NULL, Icc)) return FALSE;
       if (!SaveTags(NULL, Icc)) return FALSE;

       OutStream = fopen(FileName, "wb");
       if (!OutStream) return FALSE;

       // Pass #2 does save to file
     
       if (!SaveHeader(OutStream, Icc)) goto CleanUp;
       if (!SaveTagDirectory(OutStream, Icc)) goto CleanUp;
       if (!SaveTags(OutStream, Icc)) goto CleanUp;
      
       return (fclose(OutStream) == 0);

   CleanUp:
     
       fclose(OutStream);
       unlink(FileName);
       return FALSE;
}


// Low-level save from open stream
BOOL LCMSEXPORT _cmsSaveProfileToMem(cmsHPROFILE hProfile, void *MemPtr, 
                                                           size_t* BytesNeeded)
{      FILEMEM* OutStream; 
       LPLCMSICCPROFILE Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;     
      

	   Icc ->Write       = MemoryWrite;   

       // Pass #1 does compute offsets

       if (!SaveHeader(NULL, Icc)) return FALSE;
       if (!SaveTagDirectory(NULL, Icc)) return FALSE;
       if (!SaveTags(NULL, Icc)) return FALSE;              

       if (!MemPtr) {

          // update BytesSaved so caller knows how many bytes are needed for MemPtr
           *BytesNeeded = UsedSpace;
           return TRUE;
       }        

       if (*BytesNeeded < UsedSpace) {
           // need at least UsedSpace in MemPtr to continue       
            return FALSE;
       }

       OutStream = MemoryOpen(MemPtr, *BytesNeeded, 'w');
       if (!OutStream) return FALSE;
	   
	   OutStream ->Block = MemPtr;
                  
       // Pass #2 does save to file into supplied stream     
       if (!SaveHeader(OutStream, Icc)) goto CleanUp;
       if (!SaveTagDirectory(OutStream, Icc)) goto CleanUp;
       if (!SaveTags(OutStream, Icc)) goto CleanUp;
      
       // update BytesSaved so caller knows how many bytes put into stream
       *BytesNeeded = UsedSpace;
       
	   free(OutStream);
       return TRUE;

   CleanUp:
           
       free(OutStream);
       return FALSE;
}

