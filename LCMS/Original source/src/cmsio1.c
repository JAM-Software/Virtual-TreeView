//
//  Little cms
//  Copyright (C) 1998-2005 Marti Maria
//
// Permission is hereby granted, free of charge, to any person obtaining 
// a copy of this software and associated documentation files (the "Software"), 
// to deal in the Software without restriction, including without limitation 
// the rights to use, copy, modify, merge, publish, distribute, sublicense, 
// and/or sell copies of the Software, and to permit persons to whom the Software 
// is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in 
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO 
// THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE 
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION 
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION 
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// ICC profile serialization


#include "lcms.h"

// ----------------------------------------------------------------- Tag Serialization

// Alignment of ICC file format uses 4 bytes DWORD

#define ALIGNLONG(x) (((x)+3) & ~(3))         // Aligns to DWORD boundary


static int GlobalLanguageCode;   // Language & country descriptors, for ICC 4.0 support
static int GlobalCountryCode;

                           
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


// from Fixed point 8.8 to double

static
double Convert8Fixed8(WORD fixed8)
{
       BYTE msb, lsb;

       lsb = (BYTE) (fixed8 & 0xff);
       msb = (BYTE) (((WORD) fixed8 >> 8) & 0xff);

       return (double) ((double) msb + ((double) lsb / 256.0));
}


// from Fixed point 15.16 to double
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

    return sign * floater;
}


// Auxiliar-- read base and return type

static
icTagTypeSignature ReadBase(LPLCMSICCPROFILE Icc)
{
    icTagBase Base;

    Icc -> Read(&Base, sizeof(icTagBase), 1, Icc);
    AdjustEndianess32((LPBYTE) &Base.sig);

    return Base.sig;
}


static
void DecodeDateTimeNumber(const icDateTimeNumber *Source, struct tm *Dest)
{
    Dest->tm_sec   = TransportValue16(Source->seconds);
    Dest->tm_min   = TransportValue16(Source->minutes);
    Dest->tm_hour  = TransportValue16(Source->hours);
    Dest->tm_mday  = TransportValue16(Source->day);
    Dest->tm_mon   = TransportValue16(Source->month) - 1;
    Dest->tm_year  = TransportValue16(Source->year) - 1900;
    Dest->tm_wday  = -1;
    Dest->tm_yday  = -1;
    Dest->tm_isdst = 0;
}

static
void EncodeDateTimeNumber(icDateTimeNumber *Dest, const struct tm *Source)
{
    Dest->seconds = TransportValue16((WORD) Source->tm_sec);
    Dest->minutes = TransportValue16((WORD) Source->tm_min);
    Dest->hours   = TransportValue16((WORD) Source->tm_hour);
    Dest->day     = TransportValue16((WORD) Source->tm_mday);
    Dest->month   = TransportValue16((WORD) (Source->tm_mon + 1));
    Dest->year    = TransportValue16((WORD) (Source->tm_year + 1900));
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

// Evaluates a XYZ tristimulous across chromatic adaptation matrix

static
void EvalCHRM(LPcmsCIEXYZ Dest, LPMAT3 Chrm, LPcmsCIEXYZ Src)
{
    VEC3 d, s;

    s.n[VX] = Src -> X;
    s.n[VY] = Src -> Y;
    s.n[VZ] = Src -> Z;

    MAT3eval(&d, Chrm, &s);

    Dest ->X = d.n[VX];
    Dest ->Y = d.n[VY];
    Dest ->Z = d.n[VZ];

}


// Read profile header and validate it

static
LPLCMSICCPROFILE ReadHeader(LPLCMSICCPROFILE Icc, BOOL lIsFromMemory)
{
     icTag Tag;
     icHeader Header;
     icInt32Number TagCount, i;
    
     Icc -> Read(&Header, sizeof(icHeader), 1, Icc);

       // Convert endian

       AdjustEndianess32((LPBYTE) &Header.size);
       AdjustEndianess32((LPBYTE) &Header.cmmId);
       AdjustEndianess32((LPBYTE) &Header.version);
       AdjustEndianess32((LPBYTE) &Header.deviceClass);
       AdjustEndianess32((LPBYTE) &Header.colorSpace);
       AdjustEndianess32((LPBYTE) &Header.pcs);
       AdjustEndianess32((LPBYTE) &Header.magic);
       AdjustEndianess32((LPBYTE) &Header.flags);
	   AdjustEndianess32((LPBYTE) &Header.attributes[0]);
       AdjustEndianess32((LPBYTE) &Header.renderingIntent);

       // Validate it

       if (Header.magic != icMagicNumber) goto ErrorCleanup;
             
      
       if (Icc ->Read(&TagCount, sizeof(icInt32Number), 1, Icc) != 1)
                     goto ErrorCleanup;

       AdjustEndianess32((LPBYTE) &TagCount);

       Icc -> DeviceClass     = Header.deviceClass;
       Icc -> ColorSpace      = Header.colorSpace;
       Icc -> PCS             = Header.pcs;
       Icc -> RenderingIntent = (icRenderingIntent) Header.renderingIntent;
       Icc -> flags           = Header.flags;
	   Icc -> attributes      = Header.attributes[0];
       Icc -> Illuminant.X    = Convert15Fixed16(Header.illuminant.X);
       Icc -> Illuminant.Y    = Convert15Fixed16(Header.illuminant.Y);
       Icc -> Illuminant.Z    = Convert15Fixed16(Header.illuminant.Z);
       Icc -> Version         = Header.version;

       // Get creation date/time

       DecodeDateTimeNumber(&Header.date, &Icc ->Created);

       // Fix illuminant, some profiles are broken in this field!
       
       Icc ->Illuminant = *cmsD50_XYZ();

       // The profile ID are 16 raw bytes

       CopyMemory(Icc ->ProfileID, Header.reserved, 16);

       // Get rid of possible wrong profiles

       NormalizeXYZ(&Icc  -> Illuminant);

       // Read tag directory

       if (TagCount > MAX_TABLE_TAG) {

           cmsSignalError(LCMS_ERRC_ABORTED, "Too many tags (%d)", TagCount);
           goto ErrorCleanup;
       }

       Icc -> TagCount = TagCount;
       for (i=0; i < TagCount; i++) {

              Icc ->Read(&Tag, sizeof(icTag), 1, Icc);

              AdjustEndianess32((LPBYTE) &Tag.offset);
              AdjustEndianess32((LPBYTE) &Tag.size);
              AdjustEndianess32((LPBYTE) &Tag.sig);            // Signature

              Icc -> TagNames[i]   = Tag.sig;
              Icc -> TagOffsets[i] = Tag.offset;
              Icc -> TagSizes[i]   = Tag.size;
       }
          
       return Icc;


ErrorCleanup:

       Icc ->Close(Icc);
       free(Icc);

       if (lIsFromMemory)
             cmsSignalError(LCMS_ERRC_ABORTED, "Corrupted memory profile");              
       else
             cmsSignalError(LCMS_ERRC_ABORTED, "Corrupted profile: '%s'", Icc->PhysicalFile);

       return NULL;
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
//  | 0xffff/0xff00   0                0              |
//  |       0         0xffff/0xff00    0              |
//  |       0         0                0xffff/0xff00  |
//
// The input values got then remapped to adequate domain

static
void FixLUT8(LPLUT Lut, icTagSignature sig, size_t nTabSize)
{   
    MAT3 Fixup, Original, Result;
    LPWORD PtrW;
    size_t i;

    switch (sig) {
 

       case icSigBToA0Tag:
       case icSigBToA1Tag:
       case icSigBToA2Tag:
       case icSigGamutTag:
       case icSigPreview0Tag:
       case icSigPreview1Tag:
       case icSigPreview2Tag: 
               
           
                VEC3init(&Fixup.v[0], (double) 0xFFFF/0xFF00, 0, 0);
                VEC3init(&Fixup.v[1], 0, (double) 0xFFFF/0xFF00, 0);
                VEC3init(&Fixup.v[2], 0, 0, (double) 0xFFFF/0xFF00);
                          

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

// On Lab -> Lab abstract or Lab identities, fix both sides of LUT

static
void FixLUT8bothSides(LPLUT Lut, size_t nTabSize)
{
    MAT3 Fixup, Original, Result;
    LPWORD PtrW;
    size_t i;

        VEC3init(&Fixup.v[0], (double) 0xFFFF/0xFF00, 0, 0);
        VEC3init(&Fixup.v[1], 0, (double) 0xFFFF/0xFF00, 0);
        VEC3init(&Fixup.v[2], 0, 0, (double) 0xFFFF/0xFF00);
                  
        MAT3fromFix(&Original, &Lut->Matrix);               
        MAT3per(&Result, &Original, &Fixup);
        MAT3toFix(&Lut->Matrix, &Result);

        Lut -> wFlags |= LUT_HASMATRIX;

        PtrW = Lut -> T;
        for (i = 0; i < nTabSize; i++) {

                     *PtrW++ &= 0xFF00;
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

       Icc ->Read(&LUT8, sizeof(icLut8) - SIZEOF_UINT8_ALIGNED, 1, Icc);
       
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
              Icc ->Read(Temp, 1, 256, Icc);
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

       if (nTabSize > 0) {

            PtrW = (LPWORD) malloc(sizeof(WORD) * nTabSize);
            Temp = (LPBYTE) malloc(nTabSize);
            Icc ->Read(Temp, 1, nTabSize, Icc);

            NewLUT -> T = PtrW;
            NewLUT -> Tsize = (unsigned int) (nTabSize * sizeof(WORD));

            for (i = 0; i < nTabSize; i++) {

                     *PtrW++ = TO16_TAB(Temp[i]);
            }
            free(Temp);
       }
       else {
           NewLUT ->T = NULL;
           NewLUT ->Tsize = 0;
           NewLUT -> wFlags &= ~LUT_HAS3DGRID;
       }



       // Copy output tables

       Temp = (LPBYTE) malloc(256);
       AllLinear = 0;
       for (i=0; i < NewLUT -> OutputChan; i++) {

              PtrW = (LPWORD) malloc(sizeof(WORD) * 256);
              NewLUT -> L2[i] = PtrW;
              Icc ->Read(Temp, 1, 256, Icc);              
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
       
       if (Icc ->PCS == icSigLabData) {
           
           // Abstract or Lab identity

           if (Icc -> ColorSpace == icSigLabData) 

                FixLUT8bothSides(NewLUT, nTabSize);
           else
                FixLUT8(NewLUT, sig, nTabSize);


           // Now some additional fixup. Lab encoding on 8 bit makes
           // impossible to place gray axis on a exact node. However, 
           // some profiles does claim to do that. Poor lcms will try
           // to detect such condition and fix up "on the fly".
          
           {
               LPWORD WhiteLab, ExpectedWhite;
               WORD WhiteFixed[MAXCHANNELS], WhiteUnfixed[MAXCHANNELS];
               int j, nChannels;
               double Dist, DistFixed, DistUnfixed;
               
               _cmsEndPointsBySpace(icSigLabData, &WhiteLab, NULL, NULL);

               if (_cmsEndPointsBySpace(Icc -> ColorSpace, 
                                &ExpectedWhite, NULL, &nChannels)) {
                   
                   // 1.- Find white obtained by both combinations
                   
                   NewLUT -> FixGrayAxes = FALSE;
                   cmsEvalLUT(NewLUT, WhiteLab, WhiteUnfixed);
                   
                   NewLUT -> FixGrayAxes = TRUE;
                   cmsEvalLUT(NewLUT, WhiteLab, WhiteFixed);
                   
                   // 2.- Which method gives closer white?
                   
                   DistFixed = DistUnfixed = 0;
                   for (j=0; j < nChannels; j++) {
                       
                       Dist = ExpectedWhite[j] - WhiteFixed[j];
                       DistFixed += Dist*Dist;
                       Dist = ExpectedWhite[j] - WhiteUnfixed[j];
                       DistUnfixed += Dist*Dist;
                   }
                   
                   // 3.- Decide method
                   
                   if (sqrt(DistFixed) < sqrt(DistUnfixed))
                       NewLUT -> FixGrayAxes = TRUE;
                   else
                       NewLUT -> FixGrayAxes = FALSE;
               }
               
           }
           
       }

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


       Icc ->Read(&LUT16, sizeof(icLut16)- SIZEOF_UINT16_ALIGNED, 1, Icc);

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
              Icc ->Read(PtrW, sizeof(WORD), NewLUT -> InputEntries, Icc);
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
       if (nTabSize > 0) {

           PtrW = (LPWORD) malloc(sizeof(WORD) * nTabSize);

           NewLUT -> T = PtrW;
           NewLUT -> Tsize = (unsigned int) (nTabSize * sizeof(WORD));

           Icc -> Read(PtrW, sizeof(WORD), nTabSize, Icc);
           AdjustEndianessArray16(NewLUT -> T, nTabSize);
       }
       else {
           NewLUT ->T = NULL;
           NewLUT ->Tsize = 0;
           NewLUT -> wFlags &= ~LUT_HAS3DGRID;
       }

       // Copy output tables

       AllLinear = 0;
       for (i=0; i < NewLUT -> OutputChan; i++) {

              PtrW = (LPWORD) malloc(sizeof(WORD) * NewLUT -> OutputEntries);
              NewLUT -> L2[i] = PtrW;
              Icc ->Read(PtrW, sizeof(WORD), NewLUT -> OutputEntries, Icc);
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


// This is a shared routine for reading curves. It can handle v2 curves
// as linear, single gamma and table-based as well as v4 parametric curves.

static
LPGAMMATABLE ReadCurve(LPLCMSICCPROFILE  Icc)
{    
    icUInt32Number      Count;
    LPGAMMATABLE        NewGamma;    
    icTagTypeSignature  BaseType;
    int                 n;


       BaseType = ReadBase(Icc);
       
       switch (BaseType) {


       case 0x9478ee00L:    // Monaco 2 profiler is BROKEN!
       case icSigCurveType:

           Icc ->Read(&Count, sizeof(icUInt32Number), 1, Icc);
           AdjustEndianess32((LPBYTE) &Count);


           switch (Count) {

           case 0:   // Linear.

                     NewGamma = cmsAllocGamma(2);
                     if (!NewGamma) return NULL;
                     NewGamma -> GammaTable[0] = 0;
                     NewGamma -> GammaTable[1] = 0xFFFF;
                     return NewGamma;

           case 1:  // Specified as the exponent of gamma function
                    {
                     WORD SingleGammaFixed;

                     Icc ->Read(&SingleGammaFixed, sizeof(WORD), 1, Icc);
                     AdjustEndianess16((LPBYTE) &SingleGammaFixed);
                     return cmsBuildGamma(4096, Convert8Fixed8(SingleGammaFixed));
                     }

           default: { // Curve
                    
                     NewGamma = cmsAllocGamma(Count);
                     if (!NewGamma) return NULL;

                     Icc ->Read(NewGamma -> GammaTable, sizeof(WORD), Count, Icc);
                     
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
           
           Icc -> Read(&Type, sizeof(icUInt16Number), 1, Icc);
           Icc -> Read(&Reserved, sizeof(icUInt16Number), 1, Icc);
           
           AdjustEndianess16((LPBYTE) &Type);
           if (Type > 5) {

                cmsSignalError(LCMS_ERRC_ABORTED, "Unknown parametric curve type '%d' found.", Type);
                return NULL;
           }
        
          ZeroMemory(Params, 10* sizeof(double));
          n = ParamsByType[Type];

          for (i=0; i < n; i++) {
                Num = 0;
                Icc -> Read(&Num, sizeof(icS15Fixed16Number), 1, Icc);
                Params[i] = Convert15Fixed16(Num);
          }


           NewGamma = cmsBuildParametricGamma(4096, Type+1, Params);
           return NewGamma;
          }
    

       default:
              cmsSignalError(LCMS_ERRC_ABORTED, "Bad tag signature '%lx' found.", BaseType);
              return NULL;
       }
    
}


// Similar to anterior, but curve is reversed

static
LPGAMMATABLE ReadCurveReversed(LPLCMSICCPROFILE Icc)
{
     
     icTagTypeSignature BaseType;     
     LPGAMMATABLE       NewGamma, ReturnGamma;
     icUInt32Number     Count;
     int                n;
      

       BaseType = ReadBase(Icc);
       
       switch (BaseType) {


       case 0x9478ee00L:    // Monaco 2 profiler is BROKEN!
       case icSigCurveType:

           Icc -> Read(&Count, sizeof(icUInt32Number), 1, Icc);
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

                     Icc -> Read(&SingleGammaFixed, sizeof(WORD), 1, Icc);
                     AdjustEndianess16((LPBYTE) &SingleGammaFixed);
                     return cmsBuildGamma(4096, 1./Convert8Fixed8(SingleGammaFixed));
                     }

           default: { // Curve. Do our best to trying to reverse the curve
                                         
                     NewGamma = cmsAllocGamma(Count);
                     if (!NewGamma) return NULL;

                     Icc -> Read(NewGamma -> GammaTable, sizeof(WORD), Count, Icc);
                     
                     AdjustEndianessArray16(NewGamma -> GammaTable, Count);

                     if (Count < 256) 
                         Count = 256;      // Reverse of simple curve has not necesarely to be simple

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


           Icc -> Read(&Type, sizeof(icUInt16Number), 1, Icc);
           Icc -> Read(&Reserved, sizeof(icUInt16Number), 1, Icc);
           
           AdjustEndianess16((LPBYTE) &Type);
           if (Type > 5) {

                cmsSignalError(LCMS_ERRC_ABORTED, "Unknown parametric curve type '%d' found.", Type);
                return NULL;
           }
        
          ZeroMemory(Params, 10* sizeof(double));
          n = ParamsByType[Type];

          for (i=0; i < n; i++) {
                Icc -> Read(&Num, sizeof(icS15Fixed16Number), 1, Icc);
                Params[i] = Convert15Fixed16(Num);
          }


           // Negative type as a mark of reversed curve
           NewGamma = cmsBuildParametricGamma(4096, -(Type+1), Params);
           return NewGamma;
          }
    

       default:
              cmsSignalError(LCMS_ERRC_ABORTED, "Bad tag signature '%lx' found.", BaseType);
              return NULL;
       }

}


// V4 stuff. Read matrix for LutAtoB and LutBtoA

static
BOOL ReadMatrixOffset(LPLCMSICCPROFILE Icc, size_t Offset, LPLUT NewLUT, DWORD dwFlags)
{

    icS15Fixed16Number All[12];
    int i;
    MAT3 m;
    VEC3 o;

    if (Icc -> Seek(Icc, Offset)) return FALSE;

    Icc ->Read(All, sizeof(icS15Fixed16Number), 12, Icc);

    for (i=0; i < 12;  i++)
              AdjustEndianess32((LPBYTE) &All[i]);


       m.v[0].n[0] = FIXED_TO_DOUBLE((Fixed32) All[0]);
       m.v[0].n[1] = FIXED_TO_DOUBLE((Fixed32) All[1]);
       m.v[0].n[2] = FIXED_TO_DOUBLE((Fixed32) All[2]);
       m.v[1].n[0] = FIXED_TO_DOUBLE((Fixed32) All[3]);
       m.v[1].n[1] = FIXED_TO_DOUBLE((Fixed32) All[4]);
       m.v[1].n[2] = FIXED_TO_DOUBLE((Fixed32) All[5]);
       m.v[2].n[0] = FIXED_TO_DOUBLE((Fixed32) All[6]);
       m.v[2].n[1] = FIXED_TO_DOUBLE((Fixed32) All[7]);
       m.v[2].n[2] = FIXED_TO_DOUBLE((Fixed32) All[8]);

       o.n[0] = FIXED_TO_DOUBLE((Fixed32) All[9]);
       o.n[1] = FIXED_TO_DOUBLE((Fixed32) All[10]);
       o.n[2] = FIXED_TO_DOUBLE((Fixed32) All[11]);

       cmsSetMatrixLUT4(NewLUT, &m, &o, dwFlags);

       return TRUE;
}


//  V4 stuff. Read CLUT part for LutAtoB and LutBtoA

static
BOOL ReadCLUT(LPLCMSICCPROFILE Icc, size_t Offset, LPLUT NewLUT)
{

    icCLutStruct CLUT;

    if (Icc -> Seek(Icc, Offset)) return FALSE;
    Icc ->Read(&CLUT, sizeof(icCLutStruct), 1, Icc);


    cmsAlloc3DGrid(NewLUT, CLUT.gridPoints[0], NewLUT ->InputChan, 
                                               NewLUT ->OutputChan);

    // Precission can be 1 or 2 bytes

    if (CLUT.prec == 1) {

        BYTE v;
        unsigned int i;

        for (i=0; i < NewLUT->Tsize / sizeof(WORD); i++) {
                Icc ->Read(&v, sizeof(BYTE), 1, Icc);
                NewLUT->T[i] = TO16_TAB(v);
        }
                
    }
    else  
        if (CLUT.prec == 2) {
        
         Icc ->Read(NewLUT ->T, sizeof(WORD), 
                    NewLUT->Tsize / sizeof(WORD), Icc);

        AdjustEndianessArray16(NewLUT ->T, NewLUT->Tsize / sizeof(WORD));
    }
    else {
        cmsSignalError(LCMS_ERRC_ABORTED, "Unknow precission of '%d'", CLUT.prec); 
        return FALSE;
    }

    return TRUE;
}


static
void SkipAlignment(LPLCMSICCPROFILE Icc)
{
    BYTE Buffer[4];
    size_t At = Icc ->Tell(Icc);
    int BytesToNextAlignedPos = (int) (At % 4);
    
    Icc ->Read(Buffer, 1, BytesToNextAlignedPos, Icc);
}

// Read a set of curves from specific offset
static
BOOL ReadSetOfCurves(LPLCMSICCPROFILE Icc, size_t Offset, LPLUT NewLUT, int nLocation)
{     
    LPGAMMATABLE Curves[MAXCHANNELS];
    unsigned int i, nCurves;

    if (Icc -> Seek(Icc, Offset)) return FALSE;
    
    if (nLocation == 1 ||  nLocation == 3) 

        nCurves = NewLUT ->InputChan;    
    else 
        nCurves = NewLUT ->OutputChan;    

    for (i=0; i < nCurves; i++) {

        Curves[i] = ReadCurve(Icc);                     
        SkipAlignment(Icc);
    
    }
    
    NewLUT = cmsAllocLinearTable(NewLUT, Curves, nLocation);
    
    for (i=0; i < nCurves; i++) 
        cmsFreeGamma(Curves[i]);

    return TRUE;

}

// V4 stuff. LutAtoB type 
//
//  [L1] -> [CLUT] -> [L4] -> [Mat4] -> [Ofs4] -> [L2]
//
//  Mat, Mat3, Ofs3, L3 are missing
//   L1 = A curves
//   L4 = M curves
//   L2 = B curves

static
BOOL ReadLUT_A2B(LPLCMSICCPROFILE Icc, LPLUT NewLUT, size_t BaseOffset, icTagSignature sig)
{
    icLutAtoB LUT16;
     
       Icc ->Read(&LUT16, sizeof(icLutAtoB), 1, Icc);

       NewLUT -> InputChan     = LUT16.inputChan;
       NewLUT -> OutputChan    = LUT16.outputChan;

       AdjustEndianess32((LPBYTE) &LUT16.offsetB);
       AdjustEndianess32((LPBYTE) &LUT16.offsetMat);
       AdjustEndianess32((LPBYTE) &LUT16.offsetM);
       AdjustEndianess32((LPBYTE) &LUT16.offsetC);
       AdjustEndianess32((LPBYTE) &LUT16.offsetA);
           
      
       if (LUT16.offsetB != 0)                                         
                ReadSetOfCurves(Icc, BaseOffset + LUT16.offsetB, NewLUT, 2);

       if (LUT16.offsetMat != 0)            
            ReadMatrixOffset(Icc, BaseOffset + LUT16.offsetMat, NewLUT, LUT_HASMATRIX4);
       

       if (LUT16.offsetM != 0) 
                ReadSetOfCurves(Icc, BaseOffset + LUT16.offsetM, NewLUT, 4);

       if (LUT16.offsetC != 0) 
           ReadCLUT(Icc, BaseOffset + LUT16.offsetC, NewLUT);

       if (LUT16.offsetA!= 0) 
                ReadSetOfCurves(Icc, BaseOffset + LUT16.offsetA, NewLUT, 1);

        // Convert to v2 PCS

       if (Icc ->PCS == icSigLabData) {

       switch (sig) {

       case icSigAToB0Tag:
       case icSigAToB1Tag:
       case icSigAToB2Tag:
       case icSigGamutTag:
       case icSigPreview0Tag:
       case icSigPreview1Tag:
       case icSigPreview2Tag: 

               NewLUT ->wFlags |= LUT_V4_INPUT_EMULATE_V2;
               break;

       default:;
       }
       }

       
       return TRUE;
}

// V4 stuff. LutBtoA type 

static
BOOL ReadLUT_B2A(LPLCMSICCPROFILE Icc, LPLUT NewLUT,  size_t BaseOffset, icTagSignature sig)
{ 
  icLutBtoA LUT16;
     
       Icc ->Read(&LUT16, sizeof(icLutBtoA), 1, Icc);

       NewLUT -> InputChan     = LUT16.inputChan;
       NewLUT -> OutputChan    = LUT16.outputChan;

       AdjustEndianess32((LPBYTE) &LUT16.offsetB);
       AdjustEndianess32((LPBYTE) &LUT16.offsetMat);
       AdjustEndianess32((LPBYTE) &LUT16.offsetM);
       AdjustEndianess32((LPBYTE) &LUT16.offsetC);
       AdjustEndianess32((LPBYTE) &LUT16.offsetA);
           
      
       if (LUT16.offsetB != 0)                                         
                ReadSetOfCurves(Icc, BaseOffset + LUT16.offsetB, NewLUT, 1);

       if (LUT16.offsetMat != 0)            
            ReadMatrixOffset(Icc, BaseOffset + LUT16.offsetMat, NewLUT, LUT_HASMATRIX3);
       

       if (LUT16.offsetM != 0) 
                ReadSetOfCurves(Icc, BaseOffset + LUT16.offsetM, NewLUT, 3);

       if (LUT16.offsetC != 0) 
           ReadCLUT(Icc, BaseOffset + LUT16.offsetC, NewLUT);

       if (LUT16.offsetA!= 0) 
                ReadSetOfCurves(Icc, BaseOffset + LUT16.offsetA, NewLUT, 2);
      

       // Convert to v2 PCS

       if (Icc ->PCS == icSigLabData) {

       switch (sig) {

       case icSigBToA0Tag:
       case icSigBToA1Tag:
       case icSigBToA2Tag:
       case icSigGamutTag:
       case icSigPreview0Tag:
       case icSigPreview1Tag:
       case icSigPreview2Tag: 

               NewLUT ->wFlags |= LUT_V4_OUTPUT_EMULATE_V2;
               break;

       default:;
       }
       }

       return TRUE;
}

// CLUT main reader

LPLUT LCMSEXPORT cmsReadICCLut(cmsHPROFILE hProfile, icTagSignature sig)
{

    LPLCMSICCPROFILE    Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
    icTagTypeSignature  BaseType;    
    int                 n;
    size_t              offset;
    LPLUT               NewLUT;

    n = _cmsSearchTag(Icc, sig, TRUE);
    if (n < 0)      
        return NULL;
    

    // If is in memory, the LUT is already there, so throw a copy
    if (!Icc -> stream) {
                
        return cmsDupLUT((LPLUT) Icc ->TagPtrs[n]);
    }

    offset = Icc -> TagOffsets[n];

    if (Icc -> Seek(Icc, offset))
            return NULL;

    BaseType = ReadBase(Icc);

    
    NewLUT = cmsAllocLUT();
    if (!NewLUT)
    {
       cmsSignalError(LCMS_ERRC_ABORTED, "cmsAllocLUT() failed");
       return NULL;
    }


    switch (BaseType) {

    case icSigLut8Type:    ReadLUT8(Icc, NewLUT, sig); break;
    case icSigLut16Type:   ReadLUT16(Icc, NewLUT);     break;

    case icSiglutAtoBType: ReadLUT_A2B(Icc, NewLUT, offset, sig); break;
    case icSiglutBtoAType: ReadLUT_B2A(Icc, NewLUT, offset, sig); break;

    default:  cmsSignalError(LCMS_ERRC_ABORTED, "Bad tag signature %lx found.", BaseType);
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



// Some tags (e.g, 'pseq') can have text tags embedded. This function
// handles such special case.

static
int ReadEmbeddedTextTag(LPLCMSICCPROFILE Icc, size_t size, char* Name)
{
    icTagTypeSignature  BaseType;
           

    BaseType = ReadBase(Icc);
        
    size -= sizeof(icTagBase);
    
    switch (BaseType) {

    case icSigTextDescriptionType: {

           icUInt32Number  AsciiCount;
           icUInt32Number  i, UnicodeCode, UnicodeCount;
           icUInt16Number  ScriptCodeCode, Dummy;
           icUInt8Number   ScriptCodeCount;
           
           Icc ->Read(&AsciiCount, sizeof(icUInt32Number), 1, Icc);
           size -= sizeof(icUInt32Number);

           AdjustEndianess32((LPBYTE) &AsciiCount);
           Icc ->Read(Name, 1, AsciiCount, Icc);
           size -= AsciiCount;

           // Skip Unicode code

           Icc ->Read(&UnicodeCode,  sizeof(icUInt32Number), 1, Icc);
           size -= sizeof(icUInt32Number);

           Icc ->Read(&UnicodeCount, sizeof(icUInt32Number), 1, Icc);
           size -= sizeof(icUInt32Number);

           AdjustEndianess32((LPBYTE) &UnicodeCount);

           if (UnicodeCount > size) return (int) size;

           for (i=0; i < UnicodeCount; i++) 
                Icc ->Read(&Dummy, sizeof(icUInt16Number), 1, Icc);
           
           size -= UnicodeCount * sizeof(icUInt16Number);
            
          // Skip ScriptCode code

           Icc ->Read(&ScriptCodeCode,  sizeof(icUInt16Number), 1, Icc);
           size -= sizeof(icUInt16Number);
           Icc ->Read(&ScriptCodeCount, sizeof(icUInt8Number), 1, Icc);
           size -= sizeof(icUInt8Number);

           if (size < 67) return (int) size;

           for (i=0; i < 67; i++) 
                Icc ->Read(&Dummy, sizeof(icUInt8Number), 1, Icc);

           size -= 67;                      
           }
           break;


    case icSigCopyrightTag:   // Broken profiles from agfa does store copyright info in such type
    case icSigTextType:
               
           Icc -> Read(Name, 1, size, Icc);
           break;

    // MultiLocalizedUnicodeType, V4 only

    case icSigMultiLocalizedUnicodeType: {

        icUInt32Number Count, RecLen;       
        icUInt16Number Language, Country;
        icUInt32Number ThisLen, ThisOffset;
        size_t         Offset = 0;
        size_t         Len    = 0;      
        size_t         i;
        wchar_t*       wchar  = L"";
                    

            Icc ->Read(&Count, sizeof(icUInt32Number), 1, Icc);
            AdjustEndianess32((LPBYTE) &Count);
            Icc ->Read(&RecLen, sizeof(icUInt32Number), 1, Icc);
            AdjustEndianess32((LPBYTE) &RecLen);

            if (RecLen != 12) {

                    cmsSignalError(LCMS_ERRC_ABORTED, "multiLocalizedUnicodeType of len != 12 is not supported.");
                    return -1;
            }

            for (i=0; i < Count; i++) {
                
                Icc ->Read(&Language, sizeof(icUInt16Number), 1, Icc);
                AdjustEndianess16((LPBYTE) &Language);
                Icc ->Read(&Country, sizeof(icUInt16Number), 1, Icc);
                AdjustEndianess16((LPBYTE) &Country);
    
                Icc ->Read(&ThisLen, sizeof(icUInt32Number), 1, Icc);
                AdjustEndianess32((LPBYTE) &ThisLen);
    
                Icc ->Read(&ThisOffset, sizeof(icUInt32Number), 1, Icc);
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

                    Icc ->Read(&Discard, 1, 1, Icc);
            }

            wchar = (wchar_t*) malloc(Len+2);
            if (!wchar) return -1;
            
            Icc ->Read(wchar, 1, Len, Icc);
            AdjustEndianessArray16((LPWORD) wchar, Len / 2);

            wchar[Len / 2] = L'\0';
            i = wcstombs(Name, wchar, 2047 );  
            if (i == ((size_t) -1)) {

                Name[0] = 0;    // Error
            }

            free((void*) wchar);
            }
            break;

    default:
            cmsSignalError(LCMS_ERRC_ABORTED, "Bad tag signature %lx found.", BaseType);
            return -1;
    }

    return (int) size;
}


// Take an ASCII item


int LCMSEXPORT cmsReadICCText(cmsHPROFILE hProfile, icTagSignature sig, char *Name)
{
    LPLCMSICCPROFILE    Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
    size_t              offset, size;
    int                 n;

    n = _cmsSearchTag(Icc, sig, TRUE);
    if (n < 0)         
        return -1;
    
    if (!Icc -> stream) {

        CopyMemory(Name, Icc -> TagPtrs[n], Icc -> TagSizes[n]);
        return (int) Icc -> TagSizes[n];
    }

    offset = Icc -> TagOffsets[n];
    size   = Icc -> TagSizes[n];

    if (Icc -> Seek(Icc, offset))
            return -1;
      
    return ReadEmbeddedTextTag(Icc, size, Name);

}



// Take an XYZ item

static
int ReadICCXYZ(cmsHPROFILE hProfile, icTagSignature sig, LPcmsCIEXYZ Value, BOOL lIsFatal)
{
    LPLCMSICCPROFILE    Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
    icTagTypeSignature  BaseType;    
    size_t              offset;
    int                 n;
    icXYZNumber         XYZ;

    n = _cmsSearchTag(Icc, sig, FALSE);
    if (n < 0)    
            return -1;
    
    if (!Icc -> stream) {

         CopyMemory(Value, Icc -> TagPtrs[n], Icc -> TagSizes[n]);       
         return (int) Icc -> TagSizes[n];
    }

    offset = Icc -> TagOffsets[n];

    if (Icc -> Seek(Icc, offset))
            return -1;

    
    BaseType = ReadBase(Icc);
    
    switch (BaseType) {

        
    case 0x7c3b10cL:    // Some apple broken embedded profiles does not have correct type   
    case icSigXYZType:

           Icc ->Read(&XYZ, sizeof(icXYZNumber), 1, Icc);
           Value -> X = Convert15Fixed16(XYZ.X);
           Value -> Y = Convert15Fixed16(XYZ.Y);
           Value -> Z = Convert15Fixed16(XYZ.Z);
           break;

    // Aug/21-2001 - Monaco 2 does have WRONG values.

    default:
           if (lIsFatal)
                cmsSignalError(LCMS_ERRC_ABORTED, "Bad tag signature %lx found.", BaseType);
           return -1;
    }

    return 1;
}


// Read a icSigS15Fixed16ArrayType (currently only a 3x3 matrix)           

static
int ReadICCXYZArray(cmsHPROFILE hProfile, icTagSignature sig, LPMAT3 v)
{
    LPLCMSICCPROFILE    Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;    
    icTagTypeSignature  BaseType; 
    size_t              offset, sz;
    int                 i, n;
    icXYZNumber         XYZ[3];
    cmsCIEXYZ           XYZdbl[3];


    n = _cmsSearchTag(Icc, sig, FALSE);
    if (n < 0)      
            return -1; // Not found
    
    if (!Icc -> stream) {

            CopyMemory(v, Icc -> TagPtrs[n], Icc -> TagSizes[n]);
            return (int) Icc -> TagSizes[n];
    }

    offset = Icc -> TagOffsets[n];

    if (Icc -> Seek(Icc, offset))
            return -1;
    
    BaseType = ReadBase(Icc);
   
    switch (BaseType) {

    case icSigS15Fixed16ArrayType:

         sz = Icc ->TagSizes[n] / sizeof(icXYZNumber);

         if (sz != 3) {
             cmsSignalError(LCMS_ERRC_ABORTED, "Bad array size of %d entries.", sz);
             return -1;
         }
                
         Icc ->Read(XYZ, sizeof(icXYZNumber), 3, Icc);

         for (i=0; i < 3; i++) {

            XYZdbl[i].X = Convert15Fixed16(XYZ[i].X);
            XYZdbl[i].Y = Convert15Fixed16(XYZ[i].Y);
            XYZdbl[i].Z = Convert15Fixed16(XYZ[i].Z);
         }

         CopyMemory(v, XYZdbl, 3*sizeof(cmsCIEXYZ));
         break;

    default:
         cmsSignalError(LCMS_ERRC_ABORTED, "Bad tag signature %lx found.", BaseType);
         return -1;

    }

   return sizeof(MAT3);
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


// Always return a suitable matrix

BOOL cmsReadChromaticAdaptationMatrix(LPMAT3 r, cmsHPROFILE hProfile)
{    
      
    if (ReadICCXYZArray(hProfile, icSigChromaticAdaptationTag, r) < 0) {

       LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;

       // For display profiles, revert to bradford. Else take identity.

       MAT3identity(r);

       // Emissive devices have non-identity chad

       if ((cmsGetDeviceClass(hProfile) == icSigDisplayClass) ||
           cmsTakeHeaderFlags(hProfile) & icTransparency) {

            // NULL for cone defaults to Bradford, from media to D50
            cmsAdaptationMatrix(r, NULL, &Icc ->MediaWhitePoint, &Icc ->Illuminant);        
        }
    }
    
    return TRUE;
}



LPGAMMATABLE LCMSEXPORT cmsReadICCGamma(cmsHPROFILE hProfile, icTagSignature sig)
{
       LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
       size_t         offset;
       int            n;


       n = _cmsSearchTag(Icc, sig, TRUE);
       if (n < 0)   
           return NULL;
       
       if (!Icc -> stream) {

            return cmsDupGamma((LPGAMMATABLE) Icc -> TagPtrs[n]);
       }

       offset = Icc -> TagOffsets[n];

       if (Icc -> Seek(Icc, offset))
            return NULL;

       return ReadCurve(Icc);
     
}


// Some ways have analytical revese. This function accounts for that

LPGAMMATABLE LCMSEXPORT cmsReadICCGammaReversed(cmsHPROFILE hProfile, icTagSignature sig)
{
       LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
       size_t         offset;
       int            n;


       n = _cmsSearchTag(Icc, sig, TRUE);
       if (n < 0) 
            return NULL;       

       if (!Icc -> stream) {

            return cmsReverseGamma(256, (LPGAMMATABLE) Icc -> TagPtrs[n]);
       }

       offset = Icc -> TagOffsets[n];

       if (Icc -> Seek(Icc, offset))
            return NULL;
       
       return ReadCurveReversed(Icc);       
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
       LPLCMSICCPROFILE   Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
       int                n;       
       icTagTypeSignature BaseType;
       size_t             offset;
       
       n = _cmsSearchTag(Icc, sig, TRUE);
       if (n < 0)             
            return 0;
       
       if (!Icc -> stream) {
           
            // This replaces actual named color list.                       
            size_t size   = Icc -> TagSizes[n];  

            if (v ->NamedColorList) cmsFreeNamedColorList(v ->NamedColorList);
            v -> NamedColorList = (LPcmsNAMEDCOLORLIST) malloc(size);           
            CopyMemory(v -> NamedColorList, Icc ->TagPtrs[n], size);    
            return v ->NamedColorList->nColors;           
       }

       offset = Icc -> TagOffsets[n];

       if (Icc -> Seek(Icc, offset))
            return 0;

       BaseType = ReadBase(Icc);
       
       switch (BaseType) {

        // I never have seen one of these. Probably is not worth of implementing.

       case icSigNamedColorType: {

              cmsSignalError(LCMS_ERRC_WARNING, "Ancient named color profiles are not supported.");
              return 0;                 
            }
           
        // The named color struct
        
       case icSigNamedColor2Type: {

                icNamedColor2 nc2;
                unsigned int i, j;
                                
                Icc -> Read(&nc2, sizeof(icNamedColor2) - SIZEOF_UINT8_ALIGNED, 1, Icc);
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
                    Icc -> Read(Root, 1, 32, Icc);
                    Icc -> Read(PCS,  3, sizeof(WORD), Icc);

                    for (j=0; j < 3; j++)
                        AdjustEndianess16((LPBYTE) &PCS[j]);
                    
                    Icc -> Read(Colorant, sizeof(WORD), nc2.nDeviceCoords, Icc);

                    for (j=0; j < nc2.nDeviceCoords; j++) 
                            AdjustEndianess16((LPBYTE) &Colorant[j]);
                    
                    cmsAppendNamedColor(v, Root, PCS, Colorant);
                }

            return v ->NamedColorList->nColors;
            }
            break;

       default:
              cmsSignalError(LCMS_ERRC_WARNING, "Bad tag signature '%lx' found.", BaseType);
              return 0;
       }

       // It would never reach here
       // return 0;
}



// Read colorant tables

LPcmsNAMEDCOLORLIST LCMSEXPORT cmsReadColorantTable(cmsHPROFILE hProfile, icTagSignature sig)
{   
    icInt32Number n, Count, i;
    size_t offset;
    icTagTypeSignature  BaseType;
    LPLCMSICCPROFILE   Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
    LPcmsNAMEDCOLORLIST List;

    n = _cmsSearchTag(Icc, sig, FALSE);
    if (n < 0)      
            return NULL; // Not found
    
    if (!Icc -> stream) {

            size_t size   = Icc -> TagSizes[n];  
            void* v = malloc(size);
            CopyMemory(v, Icc -> TagPtrs[n], size);
            return (LPcmsNAMEDCOLORLIST) v;
    }


    offset = Icc -> TagOffsets[n];

    if (Icc -> Seek(Icc, offset))
            return NULL;
    
    BaseType = ReadBase(Icc);

    if (BaseType != icSigColorantTableType) {
            cmsSignalError(LCMS_ERRC_ABORTED, "Bad tag signature '%lx' found.", BaseType);
            return NULL;
    }


    Icc ->Read(&Count, sizeof(icUInt32Number), 1, Icc);
    AdjustEndianess32((LPBYTE) &Count); 

    List = cmsAllocNamedColorList(Count);
    for (i=0; i < Count; i++) {


        if (!Icc ->Read(List->List[i].Name, 1, 32 , Icc)) goto Error;
        if (!Icc ->Read(List->List[i].PCS, sizeof(icUInt16Number), 3, Icc)) goto Error;
        AdjustEndianessArray16(List->List[i].PCS, 3);       
    }

    return List;

Error:
    cmsFreeNamedColorList(List);
    return NULL;

}



// Uncooked manufacturer

const char* LCMSEXPORT cmsTakeManufacturer(cmsHPROFILE hProfile)
{

    static char Manufacturer[512] = "";

       Manufacturer[0] = 0;   

       if (cmsIsTag(hProfile, icSigDeviceMfgDescTag)) {

            cmsReadICCText(hProfile, icSigDeviceMfgDescTag, Manufacturer);
       }

    return Manufacturer;
}

// Uncooked model

const char* LCMSEXPORT cmsTakeModel(cmsHPROFILE hProfile)
{

    static char Model[512] = "";

       Model[0] = 0;   

       if (cmsIsTag(hProfile, icSigDeviceModelDescTag)) {

            cmsReadICCText(hProfile, icSigDeviceModelDescTag, Model);
       }

    return Model;
}


const char* LCMSEXPORT cmsTakeCopyright(cmsHPROFILE hProfile)
{

    static char Copyright[512] = "";

       Copyright[0] = 0;   

       if (cmsIsTag(hProfile, icSigCopyrightTag)) {

            cmsReadICCText(hProfile, icSigCopyrightTag, Copyright);
       }

    return Copyright;
}


// We compute name with model - manufacturer

const char*  LCMSEXPORT cmsTakeProductName(cmsHPROFILE hProfile)
{
    static char Name[2048];
    char Manufacturer[512], Model[512];
    
    Name[0] = '\0';
    Manufacturer[0] = Model[0] = '\0';
    
    if (cmsIsTag(hProfile, icSigDeviceMfgDescTag)) {
        
        cmsReadICCText(hProfile, icSigDeviceMfgDescTag, Manufacturer);
    }
    
    if (cmsIsTag(hProfile, icSigDeviceModelDescTag)) {
        
        cmsReadICCText(hProfile, icSigDeviceModelDescTag, Model);
    }
    
    if (!Manufacturer[0] && !Model[0]) {
        
        if (cmsIsTag(hProfile, icSigProfileDescriptionTag)) {
            
            cmsReadICCText(hProfile, icSigProfileDescriptionTag, Name);
            return Name;
        }
        else return "{no name}";
    }
    
    
    if (!Manufacturer[0] || 
            strncmp(Model, Manufacturer, 8) == 0 || strlen(Model) > 30)
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

              cmsReadICCText(hProfile, icSigProfileDescriptionTag, Name);
       }
       else return cmsTakeProductName(hProfile);

       if (strncmp(Name, "Copyrig", 7) == 0)
              return cmsTakeProductName(hProfile);

       return Name;
}


const char*  LCMSEXPORT cmsTakeProductInfo(cmsHPROFILE hProfile)
{       
       LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;

       static char Info[4096];

       Info[0] = '\0';

       if (cmsIsTag(hProfile, icSigProfileDescriptionTag))
       {
       char Desc[1024];

       cmsReadICCText(hProfile, icSigProfileDescriptionTag, Desc);
       strcat(Info, Desc);
       strcat(Info, "\r\n\r\n");
       }


       if (cmsIsTag(hProfile, icSigCopyrightTag))
       {
       char Copyright[2048];

       cmsReadICCText(hProfile, icSigCopyrightTag, Copyright);
       strcat(Info, Copyright);
       strcat(Info, "\r\n\r\n");
       }



// KODAK private tag... But very useful

#define K007         (icTagSignature)0x4B303037

       // MonCal

       if (cmsIsTag(hProfile, K007))
       {
       char MonCal[1024];

       cmsReadICCText(hProfile, K007, MonCal);
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

    n = _cmsSearchTag(Icc, icSigCharTargetTag, FALSE);
    if (n < 0) return FALSE;                


    *len =  Icc -> TagSizes[n];
    *Data = (char*) malloc(*len + 1);

    if (!*Data) {

        cmsSignalError(LCMS_ERRC_ABORTED, "Out of memory allocating CharTarget space!");
        return FALSE;
    }

    if (cmsReadICCText(hProfile, icSigCharTargetTag, *Data) < 0) 
        return FALSE;

    (*Data)[*len] = 0;  // Force a zero marker. Shouldn't be needed, but is 
                        // here to simplify things.

    return TRUE;    
}




BOOL LCMSEXPORT cmsTakeCalibrationDateTime(struct tm *Dest, cmsHPROFILE hProfile)
{
    LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;    
    int n;
    
    n = _cmsSearchTag(Icc, icSigCalibrationDateTimeTag, FALSE);
    if (n < 0) return FALSE;
    
    if (!Icc ->stream)
    {
        CopyMemory(Dest, Icc ->TagPtrs[n],  sizeof(struct tm));
    }
    else
    {
        icDateTimeNumber timestamp;

        if (Icc -> Seek(Icc, Icc -> TagOffsets[n] + sizeof(icTagBase)))
            return FALSE;
        
        if (Icc ->Read(&timestamp, 1, sizeof(icDateTimeNumber), Icc) != sizeof(icDateTimeNumber))
            return FALSE;
        
        DecodeDateTimeNumber(&timestamp, Dest);
    }

    
    return TRUE;
}



// PSEQ Tag, used in devicelink profiles

LPcmsSEQ LCMSEXPORT cmsReadProfileSequenceDescription(cmsHPROFILE hProfile)
{
    LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
    int n;
    icUInt32Number     i, Count;
    icDescStruct       DescStruct;    
    icTagTypeSignature BaseType;
    size_t             size, offset;
    LPcmsSEQ           OutSeq;


    n = _cmsSearchTag(Icc, icSigProfileSequenceDescTag, FALSE);
    if (n < 0) return NULL;                
    
    size   = Icc -> TagSizes[n]; 
    if (size < 12)  return NULL;

    if (!Icc -> stream) {
                                   
            OutSeq = (LPcmsSEQ) malloc(size);           
            CopyMemory(OutSeq, Icc ->TagPtrs[n], size);    
            return OutSeq;
    }

    offset = Icc -> TagOffsets[n];
    
    if (Icc -> Seek(Icc, offset))
            return NULL;

    BaseType = ReadBase(Icc);
    
    if (BaseType != icSigProfileSequenceDescType) return NULL;

    Icc ->Read(&Count, sizeof(icUInt32Number), 1, Icc);
    AdjustEndianess32((LPBYTE) &Count);
    
    size = sizeof(int) + Count * sizeof(cmsPSEQDESC);
    OutSeq = (LPcmsSEQ) malloc(size);

    OutSeq ->n = Count;
    
    // Get structures as well

    for (i=0; i < Count; i++) {
        
        LPcmsPSEQDESC sec = &OutSeq -> seq[i];

        Icc -> Read(&DescStruct, sizeof(icDescStruct) - SIZEOF_UINT8_ALIGNED, 1, Icc);

        AdjustEndianess32((LPBYTE) &DescStruct.deviceMfg);
        AdjustEndianess32((LPBYTE) &DescStruct.deviceModel);
        AdjustEndianess32((LPBYTE) &DescStruct.technology);
        AdjustEndianess32((LPBYTE) &DescStruct.attributes[0]);
        AdjustEndianess32((LPBYTE) &DescStruct.attributes[1]);

        sec ->attributes[0] = DescStruct.attributes[0];
        sec ->attributes[1] = DescStruct.attributes[1];
        sec ->deviceMfg     = DescStruct.deviceMfg;
        sec ->deviceModel   = DescStruct.deviceModel;
        sec ->technology    = DescStruct.technology;

        if (ReadEmbeddedTextTag(Icc, size, sec ->Manufacturer) < 0) return NULL;
        if (ReadEmbeddedTextTag(Icc, size, sec ->Model) < 0) return NULL;

    }

    return OutSeq;
}



// Extended gamut -- an HP extension


LPcmsGAMUTEX LCMSEXPORT cmsReadExtendedGamut(cmsHPROFILE hProfile, int index)
{
    LPLCMSICCPROFILE  Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;    
    size_t size, offset;
    icUInt32Number off_samp, off_desc, off_vc;
    int n;
    icTagTypeSignature     BaseType;
    icColorSpaceSignature  CoordSig;
    icUInt16Number         Method, Usage;
    icUInt32Number         GamutCount, SamplesCount;
    LPcmsGAMUTEX gex;
    size_t                 Offsets[256];
    size_t                 i, Actual, Loc;
    icS15Fixed16Number     Num;
    icUInt16Number         Surround;
    

    n = _cmsSearchTag(Icc, icSigHPGamutDescTag, FALSE);
    if (n < 0) return NULL;
    
    if (!Icc ->stream) return NULL;     // In memory is not supported
  
    // Read the header

    offset = Icc -> TagOffsets[n];

    if (Icc -> Seek(Icc, offset))
            return NULL;

    // Here is the beginning of tag
    Actual   = Icc ->Tell(Icc);


    BaseType = ReadBase(Icc);
    
    if (BaseType != icSigHPGamutDescType) {
            cmsSignalError(LCMS_ERRC_ABORTED, "Bad tag signature '%lx' found.", BaseType);
            return NULL;
    }


    // Read the gamut descriptors count
    Icc ->Read(&GamutCount, sizeof(icUInt32Number), 1, Icc);
    AdjustEndianess32((LPBYTE) &GamutCount); 


    if (GamutCount >= 256) {
            cmsSignalError(LCMS_ERRC_ABORTED, "Too many gamut structures '%d'.", GamutCount);
            return NULL;
    }
    
    // Read the directory

    for (i=0; i < GamutCount; i++) {

        Icc ->Read(&Offsets[i], sizeof(icUInt32Number), 1, Icc);
        AdjustEndianess32((LPBYTE) &Offsets[i]); 
    }

    
    // Is there such element?
    if (index >= (int) GamutCount) return NULL; 
    Loc = Actual + Offsets[index];


    // Go to specified index
    if (Icc -> Seek(Icc, Loc))
            return NULL;
   

    // Read all members
    Icc ->Read(&CoordSig, sizeof(icColorSpaceSignature), 1, Icc);
    AdjustEndianess32((LPBYTE) &CoordSig); 

    Icc ->Read(&Method, sizeof(icUInt16Number), 1, Icc);
    AdjustEndianess16((LPBYTE) &Method); 

    Icc ->Read(&Usage, sizeof(icUInt16Number), 1, Icc);
    AdjustEndianess16((LPBYTE) &Usage); 

    Icc ->Read(&SamplesCount, sizeof(icUInt32Number), 1, Icc);
    AdjustEndianess32((LPBYTE) &SamplesCount); 

    Icc ->Read(&off_samp, sizeof(icUInt32Number), 1, Icc);
    AdjustEndianess32((LPBYTE) &off_samp); 

    Icc ->Read(&off_desc, sizeof(icUInt32Number), 1, Icc);
    AdjustEndianess32((LPBYTE) &off_desc); 

    Icc ->Read(&off_vc, sizeof(icUInt32Number), 1, Icc);
    AdjustEndianess32((LPBYTE) &off_vc); 
    

    size = sizeof(cmsGAMUTEX) + (SamplesCount - 1) * sizeof(double);

    gex = (LPcmsGAMUTEX) malloc(size);
	if (gex == NULL) return NULL;


    gex ->CoordSig = CoordSig;
    gex ->Method   = Method;
    gex ->Usage    = Usage;
	gex ->Count    = SamplesCount;


    // Read data
    if (Icc -> Seek(Icc, Loc + off_samp))
            return NULL;

    for (i=0; i < SamplesCount; i++) {          
                Icc -> Read(&Num, sizeof(icS15Fixed16Number), 1, Icc);
                gex ->Data[i] = Convert15Fixed16(Num);
    }


    // Read mluc
    if (Icc -> Seek(Icc, Loc + off_desc)) {

			free(gex);
            return NULL;
	}

    ReadEmbeddedTextTag(Icc, 256, gex ->Description);


    // Read viewing conditions
    if (Icc -> Seek(Icc, Loc + off_vc)) {
			free(gex);
            return NULL;
	}


    Icc -> Read(&Num, sizeof(icS15Fixed16Number), 1, Icc);
    gex ->Vc.whitePoint.X = Convert15Fixed16(Num);

    Icc -> Read(&Num, sizeof(icS15Fixed16Number), 1, Icc);
    gex ->Vc.whitePoint.Y = Convert15Fixed16(Num);

    Icc -> Read(&Num, sizeof(icS15Fixed16Number), 1, Icc);
    gex ->Vc.whitePoint.Z = Convert15Fixed16(Num);

    Icc -> Read(&Num, sizeof(icS15Fixed16Number), 1, Icc);
    gex ->Vc.La = Convert15Fixed16(Num);

    Icc -> Read(&Num, sizeof(icS15Fixed16Number), 1, Icc);
    gex ->Vc.Yb = Convert15Fixed16(Num);

    Icc -> Read(&Num, sizeof(icS15Fixed16Number), 1, Icc);
    gex ->Vc.D_value = Convert15Fixed16(Num);

    Icc -> Read(&Surround, sizeof(icUInt16Number), 1, Icc);
    AdjustEndianess16((LPBYTE) &Surround); 
    gex ->Vc.surround = Surround;


    // All OK
    return gex;

}



void LCMSEXPORT cmsFreeExtendedGamut(LPcmsGAMUTEX gex)
{
    if (gex) 
        free(gex);
}


// Read a few tags that are hardly required


static
void ReadCriticalTags(LPLCMSICCPROFILE Icc)
{
    cmsHPROFILE hProfile = (cmsHPROFILE) Icc;
     
    if (Icc ->Version >= 0x4000000) {

        // v4 profiles

        MAT3 ChrmCanonical;

         if (ReadICCXYZ(hProfile, 
                      icSigMediaWhitePointTag, 
                      &Icc ->MediaWhitePoint, FALSE) < 0) {

              Icc ->MediaWhitePoint = *cmsD50_XYZ();              
       }

       // Read media black

       if (ReadICCXYZ(hProfile, 
                      icSigMediaBlackPointTag, 
                      &Icc ->MediaBlackPoint, FALSE) < 0) {

              Icc ->MediaBlackPoint.X = 0;
              Icc ->MediaBlackPoint.Y = 0; 
              Icc ->MediaBlackPoint.X = 0;

       }

       NormalizeXYZ(&Icc ->MediaWhitePoint);
       NormalizeXYZ(&Icc ->MediaBlackPoint);

      if (ReadICCXYZArray(hProfile, 
                                icSigChromaticAdaptationTag, 
                                &ChrmCanonical) > 0) {

                MAT3inverse(&ChrmCanonical, &Icc ->ChromaticAdaptation);
        
      }
      else {

                MAT3identity(&Icc ->ChromaticAdaptation);
      }
      

      // Convert media white, black to absolute under original illuminant

      EvalCHRM(&Icc ->MediaWhitePoint, &Icc ->ChromaticAdaptation, &Icc ->MediaWhitePoint);
      EvalCHRM(&Icc ->MediaBlackPoint, &Icc ->ChromaticAdaptation, &Icc ->MediaBlackPoint);
      

    }
    else {

        // v2 profiles   

       // Read media white

       if (ReadICCXYZ(hProfile, 
                      icSigMediaWhitePointTag, 
                      &Icc ->MediaWhitePoint, FALSE) < 0) {

              Icc ->MediaWhitePoint = *cmsD50_XYZ();              
       }

       // Read media black

       if (ReadICCXYZ(hProfile, 
                      icSigMediaBlackPointTag, 
                      &Icc ->MediaBlackPoint, FALSE) < 0) {

              Icc ->MediaBlackPoint.X = 0;
              Icc ->MediaBlackPoint.Y = 0; 
              Icc ->MediaBlackPoint.X = 0;

       }

       NormalizeXYZ(&Icc ->MediaWhitePoint);
       NormalizeXYZ(&Icc ->MediaBlackPoint);


       // Take Bradford as default for Display profiles only. 

       if (cmsGetDeviceClass(hProfile) == icSigDisplayClass) {
           

            cmsAdaptationMatrix(&Icc -> ChromaticAdaptation, 
                                NULL, 
                                &Icc -> Illuminant, 
                                &Icc -> MediaWhitePoint);
       }
       else
            MAT3identity(&Icc ->ChromaticAdaptation);

    }

}


// Create profile from disk file

cmsHPROFILE LCMSEXPORT cmsOpenProfileFromFile(const char *lpFileName, const char *sAccess)
{
       LPLCMSICCPROFILE NewIcc;
       cmsHPROFILE hEmpty;
       

       // Open for write means an empty profile

       if (*sAccess == 'W' || *sAccess == 'w') {

           hEmpty = _cmsCreateProfilePlaceholder();
           NewIcc = (LPLCMSICCPROFILE) (LPSTR) hEmpty;
           NewIcc -> IsWrite = TRUE;
           strncpy(NewIcc ->PhysicalFile, lpFileName, MAX_PATH-1);

           // Save LUT as 8 bit

           sAccess++;
           if (*sAccess == '8') NewIcc ->SaveAs8Bits = TRUE;

           return hEmpty;
       }


       // Open for read means a file placeholder
      
       NewIcc = _cmsCreateProfileFromFilePlaceholder(lpFileName);
        if (!NewIcc) return NULL;
      
       if (!ReadHeader(NewIcc, FALSE)) return NULL;
                      
       ReadCriticalTags(NewIcc);

       return (cmsHPROFILE) (LPSTR) NewIcc;
}




// Open from memory block

cmsHPROFILE LCMSEXPORT cmsOpenProfileFromMem(LPVOID MemPtr, DWORD dwSize)
{
       LPLCMSICCPROFILE NewIcc;
        
      
       NewIcc = _cmsCreateProfileFromMemPlaceholder(MemPtr, dwSize); 
       if (!NewIcc) return NULL;
       
       if (!ReadHeader(NewIcc, TRUE)) return NULL;
             
       ReadCriticalTags(NewIcc);

       return (cmsHPROFILE) (LPSTR) NewIcc;

}



BOOL LCMSEXPORT cmsCloseProfile(cmsHPROFILE hProfile)
{
       LPLCMSICCPROFILE Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
       BOOL rc = TRUE;

       if (!Icc) return FALSE;


       // Was open in write mode?   
       if (Icc ->IsWrite) {

           Icc ->IsWrite = FALSE;      // Assure no further writting
           rc = _cmsSaveProfile(hProfile, Icc ->PhysicalFile);        
       }

       
       if (Icc -> stream == NULL) {     // Was a memory (i.e. not serialized) profile?

                                
              icInt32Number i;          // Yes, free tags

              for (i=0; i < Icc -> TagCount; i++) {

                  if (Icc -> TagPtrs[i])
                            free(Icc -> TagPtrs[i]);
              }

       }
       else   Icc -> Close(Icc);    // No, close the stream      
              
            
       free(Icc);   // Free placeholder memory

       return rc;
}



// Write profile ------------------------------------------------------------



static
BOOL SaveWordsTable(int nEntries, LPWORD Tab, LPLCMSICCPROFILE Icc)
{
   size_t nTabSize = sizeof(WORD) * nEntries;
   LPWORD PtrW = (LPWORD) malloc(nTabSize);
   BOOL rc;

   if (!PtrW) return FALSE;
   CopyMemory(PtrW, Tab, nTabSize);
   AdjustEndianessArray16(PtrW, nEntries);
   rc = Icc ->Write(Icc, nTabSize, PtrW);
   free(PtrW);
   
   return rc;
}



// Saves profile header

static
BOOL SaveHeader(LPLCMSICCPROFILE Icc)
{
  icHeader Header;
  time_t now = time(NULL);
  
       Header.size        = TransportValue32((icInt32Number) Icc ->UsedSpace);
       Header.cmmId       = TransportValue32(lcmsSignature);
       Header.version     = TransportValue32((icInt32Number) 0x02300000);
       Header.deviceClass = (icProfileClassSignature) TransportValue32(Icc -> DeviceClass);
       Header.colorSpace  = (icColorSpaceSignature) TransportValue32(Icc -> ColorSpace);
       Header.pcs         = (icColorSpaceSignature) TransportValue32(Icc -> PCS);

       //   NOTE: in v4 Timestamp must be in UTC rather than in local time
       EncodeDateTimeNumber(&Header.date, gmtime(&now));

       Header.magic       = TransportValue32(icMagicNumber);

#ifdef NON_WINDOWS
       Header.platform    = (icPlatformSignature)TransportValue32(icSigMacintosh);  
#else
       Header.platform    = (icPlatformSignature)TransportValue32(icSigMicrosoft);  
#endif

       Header.flags        = TransportValue32(Icc -> flags);
       Header.manufacturer = TransportValue32(lcmsSignature);
       Header.model        = TransportValue32(0);
       Header.attributes[0]= TransportValue32(Icc -> attributes);              
       Header.attributes[1]= TransportValue32(0);

       Header.renderingIntent = TransportValue32(Icc -> RenderingIntent);

       // Illuminant is D50

       Header.illuminant.X = TransportValue32(DOUBLE_TO_FIXED(Icc -> Illuminant.X));
       Header.illuminant.Y = TransportValue32(DOUBLE_TO_FIXED(Icc -> Illuminant.Y));
       Header.illuminant.Z = TransportValue32(DOUBLE_TO_FIXED(Icc -> Illuminant.Z));

       Header.creator      = TransportValue32(lcmsSignature);
        
       ZeroMemory(&Header.reserved, sizeof(Header.reserved));

       // Set profile ID
       CopyMemory(Header.reserved, Icc ->ProfileID, 16);
      

       Icc ->UsedSpace = 0; // Mark as begin-of-file

       return Icc ->Write(Icc, sizeof(icHeader), &Header);
}



// Setup base marker

static
BOOL SetupBase(icTagTypeSignature sig, LPLCMSICCPROFILE Icc)
{
    icTagBase  Base;

    Base.sig = (icTagTypeSignature) TransportValue32(sig);
    ZeroMemory(&Base.reserved, sizeof(Base.reserved));
    return Icc -> Write(Icc, sizeof(icTagBase), &Base);
}


// Store an XYZ tag

static
BOOL SaveXYZNumber(LPcmsCIEXYZ Value, LPLCMSICCPROFILE Icc)
{

    icXYZNumber XYZ;

    if (!SetupBase(icSigXYZType, Icc)) return FALSE;

    XYZ.X = TransportValue32(DOUBLE_TO_FIXED(Value -> X));
    XYZ.Y = TransportValue32(DOUBLE_TO_FIXED(Value -> Y));
    XYZ.Z = TransportValue32(DOUBLE_TO_FIXED(Value -> Z));


    return Icc -> Write(Icc, sizeof(icXYZNumber), &XYZ);
}



// Save a gamma structure as a table

static
BOOL SaveGammaTable(LPGAMMATABLE Gamma, LPLCMSICCPROFILE Icc)
{
	icInt32Number Count;

		if (!SetupBase(icSigCurveType, Icc)) return FALSE;
		  
		Count = TransportValue32(Gamma->nEntries);

		if (!Icc ->Write(Icc, sizeof(icInt32Number), &Count)) return FALSE;

		return SaveWordsTable(Gamma->nEntries, Gamma ->GammaTable, Icc);
}


// Save a gamma structure as a one-value

static
BOOL SaveGammaOneValue(LPGAMMATABLE Gamma, LPLCMSICCPROFILE Icc)
{	
	icInt32Number Count;
	Fixed32 GammaFixed32;
	WORD    GammaFixed8;
		
		if (!SetupBase(icSigCurveType, Icc)) return FALSE;
		  				
		Count = TransportValue32(1);
		if (!Icc ->Write(Icc, sizeof(icInt32Number), &Count)) return FALSE;
		
		GammaFixed32 = DOUBLE_TO_FIXED(Gamma ->Birth.Params[0]);
		GammaFixed8  = (WORD) ((GammaFixed32 >> 8) & 0xFFFF);               
		GammaFixed8  = TransportValue16(GammaFixed8);
		
		return Icc ->Write(Icc, sizeof(icInt16Number), &GammaFixed8);             			    
}

// Save a gamma structure as a parametric gamma

static
BOOL SaveGammaParametric(LPGAMMATABLE Gamma, LPLCMSICCPROFILE Icc)
{	
	icUInt16Number Type, Reserved;
	int i, nParams;
	int ParamsByType[] = { 1, 3, 4, 5, 7 };

	if (!SetupBase(icSigParametricCurveType, Icc)) return FALSE;
	
	nParams = ParamsByType[Gamma -> Birth.Type];

	Type      = (icUInt16Number) TransportValue16((WORD) Gamma -> Birth. Type);
	Reserved  = (icUInt16Number) TransportValue16((WORD) 0);

	Icc -> Write(Icc, sizeof(icInt16Number),  &Type);   	
	Icc -> Write(Icc, sizeof(icUInt16Number), &Reserved);

	for (i=0; i < nParams; i++) {

		icInt32Number val = TransportValue32(DOUBLE_TO_FIXED(Gamma -> Birth.Params[i]));
		Icc ->Write(Icc, sizeof(icInt32Number), &val);             			    
	}

	
	return TRUE;
          
}


// Save a gamma table

static
BOOL SaveGamma(LPGAMMATABLE Gamma, LPLCMSICCPROFILE Icc)
{
		// Is the gamma curve type supported by ICC format?
		
		if (Gamma -> Birth.Type < 0 || Gamma -> Birth.Type > 5 ||
			
			// has been modified by user?

			_cmsCrc32OfGammaTable(Gamma) != Gamma -> Birth.Crc32) {

			return SaveGammaTable(Gamma, Icc);
		}

		if (Gamma -> Birth.Type == 1) return SaveGammaOneValue(Gamma, Icc);

		// Only v4 profiles are allowed to hold parametric curves

		if (cmsGetProfileICCversion((cmsHPROFILE) Icc) >= 0x4000000)
				return SaveGammaParametric(Gamma, Icc);
		
		// Defaults to save as table

		return SaveGammaTable(Gamma, Icc);

}




// Save an DESC Tag 

static
BOOL SaveDescription(const char *Text, LPLCMSICCPROFILE Icc)
{

    icUInt32Number len, Count, TotalSize, AlignedSize;
    char Filler[256];

    len = (icUInt32Number) (strlen(Text) + 1);

    // * icInt8Number         desc[count]     * NULL terminated ascii string
    // * icUInt32Number       ucLangCode;     * UniCode language code
    // * icUInt32Number       ucCount;        * UniCode description length
    // * icInt16Number        ucDesc[ucCount];* The UniCode description
    // * icUInt16Number       scCode;         * ScriptCode code
    // * icUInt8Number        scCount;        * ScriptCode count
    // * icInt8Number         scDesc[67];     * ScriptCode Description

    TotalSize = sizeof(icTagBase) + sizeof(icUInt32Number) + len + 
                sizeof(icUInt32Number) + sizeof(icUInt32Number) +
                sizeof(icUInt16Number) + sizeof(icUInt8Number) + 67; 

    AlignedSize = TotalSize;  // Can be unaligned!!
    
    if (!SetupBase(icSigTextDescriptionType, Icc)) return FALSE;
    AlignedSize -= sizeof(icTagBase);

    Count = TransportValue32(len);
    if (!Icc ->Write(Icc, sizeof(icUInt32Number), &Count)) return FALSE;
    AlignedSize -= sizeof(icUInt32Number);

    if (!Icc ->Write(Icc, len, (LPVOID)Text)) return FALSE;
    AlignedSize -= len;

    ZeroMemory(Filler, AlignedSize);
    if (!Icc ->Write(Icc, AlignedSize, Filler)) return FALSE;

    return TRUE;
}

// Save an ASCII Tag 

static
BOOL SaveText(const char *Text, LPLCMSICCPROFILE Icc)
{
    size_t len = strlen(Text) + 1;

    if (!SetupBase(icSigTextType, Icc)) return FALSE;
    if (!Icc ->Write(Icc, len, (LPVOID) Text)) return FALSE;
    return TRUE;
}


// Save one of these new chromaticity values

static
BOOL SaveOneChromaticity(double x, double y, LPLCMSICCPROFILE Icc)
{
       Fixed32 xf, yf;

       xf = TransportValue32(DOUBLE_TO_FIXED(x));
       yf = TransportValue32(DOUBLE_TO_FIXED(y));

       if (!Icc ->Write(Icc, sizeof(Fixed32), &xf)) return FALSE;
       if (!Icc ->Write(Icc, sizeof(Fixed32), &yf)) return FALSE;

       return TRUE;
}


// New tag added in Addendum II of old spec.

static
BOOL SaveChromaticities(LPcmsCIExyYTRIPLE chrm, LPLCMSICCPROFILE Icc)
{
       WORD nChans, Table;

       if (!SetupBase(icSigChromaticityType, Icc)) return FALSE;
     
       nChans = TransportValue16(3);
       if (!Icc ->Write(Icc, sizeof(WORD) , &nChans)) return FALSE;
       Table =  TransportValue16(0);
       if (!Icc ->Write(Icc, sizeof(WORD) , &Table)) return FALSE;

       if (!SaveOneChromaticity(chrm -> Red.x, chrm -> Red.y, Icc)) return FALSE;
       if (!SaveOneChromaticity(chrm -> Green.x, chrm -> Green.y, Icc)) return FALSE;
       if (!SaveOneChromaticity(chrm -> Blue.x, chrm -> Blue.y, Icc)) return FALSE;

       return TRUE;
}


static
BOOL SaveSequenceDescriptionTag(LPcmsSEQ seq, LPLCMSICCPROFILE Icc)
{
    icUInt32Number nSeqs;
    icDescStruct   DescStruct;
    int i, n = seq ->n;
    LPcmsPSEQDESC pseq = seq ->seq;

    if (!SetupBase(icSigProfileSequenceDescType, Icc)) return FALSE;
    
    nSeqs = TransportValue32(n);

    if (!Icc ->Write(Icc, sizeof(icUInt32Number) , &nSeqs)) return FALSE;

    for (i=0; i < n; i++) {
        
        LPcmsPSEQDESC sec = pseq + i;

       
        DescStruct.deviceMfg    = (icTagTypeSignature) TransportValue32(sec ->deviceMfg);
        DescStruct.deviceModel  = (icTagTypeSignature) TransportValue32(sec ->deviceModel);
        DescStruct.technology   = (icTechnologySignature) TransportValue32(sec ->technology);
        DescStruct.attributes[0]= TransportValue32(sec ->attributes[0]);
        DescStruct.attributes[1]= TransportValue32(sec ->attributes[1]);

        if (!Icc ->Write(Icc, sizeof(icDescStruct) - SIZEOF_UINT8_ALIGNED, &DescStruct)) return FALSE;
        
        if (!SaveDescription(sec ->Manufacturer, Icc)) return FALSE;
        if (!SaveDescription(sec ->Model, Icc)) return FALSE;        
    }

    return TRUE;
}


// Saves a timestamp tag

static
BOOL SaveDateTimeNumber(const struct tm *DateTime, LPLCMSICCPROFILE Icc)
{
    icDateTimeNumber Dest;

    if (!SetupBase(icSigDateTimeType, Icc)) return FALSE;
    EncodeDateTimeNumber(&Dest, DateTime);
    if (!Icc ->Write(Icc, sizeof(icDateTimeNumber), &Dest)) return FALSE;

    return TRUE;
}


// Saves a named color list into a named color profile
static
BOOL SaveNamedColorList(LPcmsNAMEDCOLORLIST NamedColorList, LPLCMSICCPROFILE Icc)
{

    icUInt32Number      vendorFlag;     // Bottom 16 bits for IC use 
    icUInt32Number      count;          // Count of named colors 
    icUInt32Number      nDeviceCoords;  // Num of device coordinates 
    icInt8Number        prefix[32];     // Prefix for each color name 
    icInt8Number        suffix[32];     // Suffix for each color name 
    int i;

    if (!SetupBase(icSigNamedColor2Type, Icc)) return FALSE;

    vendorFlag    = TransportValue32(0);
    count         = TransportValue32(NamedColorList ->nColors);
    nDeviceCoords = TransportValue32(NamedColorList ->ColorantCount);

    strncpy(prefix, NamedColorList->Prefix, 32);
    strncpy(suffix, NamedColorList->Suffix, 32);
                  
    if (!Icc ->Write(Icc, sizeof(icUInt32Number), &vendorFlag)) return FALSE;
    if (!Icc ->Write(Icc, sizeof(icUInt32Number), &count)) return FALSE;
    if (!Icc ->Write(Icc, sizeof(icUInt32Number), &nDeviceCoords)) return FALSE;
    if (!Icc ->Write(Icc, 32 , prefix)) return FALSE;
    if (!Icc ->Write(Icc, 32 , suffix)) return FALSE;

    for (i=0; i < NamedColorList ->nColors; i++) {

          icUInt16Number PCS[3];
          icUInt16Number Colorant[MAXCHANNELS];
          icInt8Number root[32];
          LPcmsNAMEDCOLOR Color;
          int j;

                    Color = NamedColorList ->List + i;

                    strncpy(root, Color ->Name, 32);
                    if (!Icc ->Write(Icc, 32 , root)) return FALSE;
                    
                    for (j=0; j < 3; j++)
                        PCS[j] = TransportValue16(Color ->PCS[j]);

                    if (!Icc ->Write(Icc, 3 * sizeof(icUInt16Number), PCS)) return FALSE;

                    for (j=0; j < NamedColorList ->ColorantCount; j++)
                        Colorant[j] = TransportValue16(Color ->DeviceColorant[j]);
                        
                    if (!Icc ->Write(Icc, 
                            NamedColorList ->ColorantCount * sizeof(icUInt16Number), Colorant)) return FALSE;                                                           
    }


    return TRUE;
}



// Saves a colorant table. It is using the named color structure for simplicity sake

static
BOOL SaveColorantTable(LPcmsNAMEDCOLORLIST NamedColorList, LPLCMSICCPROFILE Icc)
{
     icUInt32Number count;  // Count of named colors 
     int i;

     if (!SetupBase(icSigColorantTableType, Icc)) return FALSE;

     count = TransportValue32(NamedColorList ->nColors);

     if (!Icc ->Write(Icc, sizeof(icUInt32Number), &count)) return FALSE;

     for (i=0; i < NamedColorList ->nColors; i++) {

      icUInt16Number PCS[3];          
      icInt8Number root[32];
      LPcmsNAMEDCOLOR Color;
      int j;

            Color = NamedColorList ->List + i;

            strncpy(root, Color ->Name, 32);
            if (!Icc ->Write(Icc, 32 , root)) return FALSE;
            
            for (j=0; j < 3; j++)
                PCS[j] = TransportValue16(Color ->PCS[j]);

            if (!Icc ->Write(Icc, 3 * sizeof(icUInt16Number), PCS)) return FALSE;

     }


     return TRUE;
}

// Does serialization of LUT16 and writes it. 

static
BOOL SaveLUT(const LUT* NewLUT, LPLCMSICCPROFILE Icc)
{
       icLut16 LUT16;
       unsigned int i;
       size_t nTabSize;
       WORD NullTbl[2] = { 0, 0xFFFFU};


       if (!SetupBase(icSigLut16Type, Icc)) return FALSE;

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

       Icc -> Write(Icc,  sizeof(icLut16)- SIZEOF_UINT16_ALIGNED, &LUT16);

       // The prelinearization table

       for (i=0; i < NewLUT -> InputChan; i++) {

        if (NewLUT -> wFlags & LUT_HASTL1) {

               if (!SaveWordsTable(NewLUT -> InputEntries, 
                                   NewLUT -> L1[i], Icc)) return FALSE;

        }
        else Icc -> Write(Icc, sizeof(WORD)* 2, NullTbl);
       }


       nTabSize = (NewLUT -> OutputChan * uipow(NewLUT->cLutPoints,
                                                 NewLUT->InputChan));
       // The 3D CLUT.

       if (nTabSize > 0) {

             if (!SaveWordsTable((int) nTabSize, NewLUT -> T, Icc)) return FALSE;
       }
       // The postlinearization table

       for (i=0; i < NewLUT -> OutputChan; i++) {

        if (NewLUT -> wFlags & LUT_HASTL2) {

                if (!SaveWordsTable(NewLUT -> OutputEntries, 
                                    NewLUT -> L2[i], Icc)) return FALSE;
        }
        else Icc -> Write(Icc, sizeof(WORD)* 2, NullTbl);

       }

        return TRUE;
}



// Does serialization of LUT8 and writes it

static
BOOL SaveLUT8(const LUT* NewLUT, LPLCMSICCPROFILE Icc)
{
       icLut8 LUT8;
       unsigned int i, j;
       size_t nTabSize;
       BYTE val;

       // Sanity check
       
       if (NewLUT -> wFlags & LUT_HASTL1) {

           if (NewLUT -> InputEntries != 256) {
                cmsSignalError(LCMS_ERRC_ABORTED, "LUT8 needs 256 entries on prelinearization");
                return FALSE;
           }

       }


       if (NewLUT -> wFlags & LUT_HASTL2) {

           if (NewLUT -> OutputEntries != 256) {
                cmsSignalError(LCMS_ERRC_ABORTED, "LUT8 needs 256 entries on postlinearization");
                return FALSE;
           }
       }

       

       if (!SetupBase(icSigLut8Type, Icc)) return FALSE;

       LUT8.clutPoints = (icUInt8Number) NewLUT -> cLutPoints;
       LUT8.inputChan  = (icUInt8Number) NewLUT -> InputChan;
       LUT8.outputChan = (icUInt8Number) NewLUT -> OutputChan;
       

       if (NewLUT -> wFlags & LUT_HASMATRIX) {

       LUT8.e00 = TransportValue32(NewLUT -> Matrix.v[0].n[0]);
       LUT8.e01 = TransportValue32(NewLUT -> Matrix.v[0].n[1]);
       LUT8.e02 = TransportValue32(NewLUT -> Matrix.v[0].n[2]);
       LUT8.e10 = TransportValue32(NewLUT -> Matrix.v[1].n[0]);
       LUT8.e11 = TransportValue32(NewLUT -> Matrix.v[1].n[1]);
       LUT8.e12 = TransportValue32(NewLUT -> Matrix.v[1].n[2]);
       LUT8.e20 = TransportValue32(NewLUT -> Matrix.v[2].n[0]);
       LUT8.e21 = TransportValue32(NewLUT -> Matrix.v[2].n[1]);
       LUT8.e22 = TransportValue32(NewLUT -> Matrix.v[2].n[2]);
       }
       else {

       LUT8.e00 = TransportValue32(DOUBLE_TO_FIXED(1));
       LUT8.e01 = TransportValue32(DOUBLE_TO_FIXED(0));
       LUT8.e02 = TransportValue32(DOUBLE_TO_FIXED(0));
       LUT8.e10 = TransportValue32(DOUBLE_TO_FIXED(0));
       LUT8.e11 = TransportValue32(DOUBLE_TO_FIXED(1));
       LUT8.e12 = TransportValue32(DOUBLE_TO_FIXED(0));
       LUT8.e20 = TransportValue32(DOUBLE_TO_FIXED(0));
       LUT8.e21 = TransportValue32(DOUBLE_TO_FIXED(0));
       LUT8.e22 = TransportValue32(DOUBLE_TO_FIXED(1));
       }


       // Save header

       Icc -> Write(Icc,  sizeof(icLut8)- SIZEOF_UINT8_ALIGNED, &LUT8);

       // The prelinearization table

       for (i=0; i < NewLUT -> InputChan; i++) {

           for (j=0; j < 256; j++) {

               if (NewLUT -> wFlags & LUT_HASTL1)
                        val = (BYTE) floor(NewLUT ->L1[i][j] / 257.0 + .5);
               else
                        val = (BYTE) j;

               Icc ->Write(Icc, 1, &val);
           }
               
       }


       nTabSize = (NewLUT -> OutputChan * uipow(NewLUT->cLutPoints,
                                                 NewLUT->InputChan));
       // The 3D CLUT.

       for (j=0; j < nTabSize; j++) {

              val = (BYTE) floor(NewLUT ->T[j] / 257.0 + .5);
              Icc ->Write(Icc, 1, &val);
       }

       // The postlinearization table

       for (i=0; i < NewLUT -> OutputChan; i++) {

           for (j=0; j < 256; j++) {

               if (NewLUT -> wFlags & LUT_HASTL2)
                    val = (BYTE) floor(NewLUT ->L2[i][j] / 257.0 + .5);
               else
                    val = (BYTE) j;

               Icc ->Write(Icc, 1, &val);
           }

       }

        return TRUE;
}



// Set the LUT bitdepth to be saved

void LCMSEXPORT _cmsSetLUTdepth(cmsHPROFILE hProfile, int depth)
{
    LPLCMSICCPROFILE Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;

    switch (depth) {

    case 8:  Icc ->SaveAs8Bits = TRUE; break;
    case 16: Icc ->SaveAs8Bits = FALSE; break;      
        
    default:
        cmsSignalError(LCMS_ERRC_ABORTED, "%d is an unsupported as bitdepth, use 8 or 16 only.", depth);
    }
}


// Saves Tag directory

static
BOOL SaveTagDirectory(LPLCMSICCPROFILE Icc)
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
       if (!Icc ->Write(Icc, sizeof(icInt32Number) , &Count)) return FALSE;

       for (i=0; i < Icc -> TagCount; i++) {

          if (Icc ->TagNames[i] == 0) continue;

          Tag.sig    = (icTagSignature)TransportValue32(Icc -> TagNames[i]);
          Tag.offset = TransportValue32((icInt32Number) Icc -> TagOffsets[i]);
          Tag.size   = TransportValue32((icInt32Number) Icc -> TagSizes[i]);

          if (!Icc ->Write(Icc, sizeof(icTag), &Tag)) return FALSE;
       }

       return TRUE;
}


// Dump tag contents

static
BOOL SaveTags(LPLCMSICCPROFILE Icc)
{

    LPBYTE Data;
    icInt32Number i;
    size_t Begin;
    size_t AlignedSpace, FillerSize;


    for (i=0; i < Icc -> TagCount; i++) {

         if (Icc ->TagNames[i] == 0) continue;
        
        // Align to DWORD boundary, following new spec.
        
        AlignedSpace = ALIGNLONG(Icc ->UsedSpace);
        FillerSize  = AlignedSpace - Icc ->UsedSpace;
        if (FillerSize > 0)  {
            
            BYTE Filler[20];

            ZeroMemory(Filler, 16);
            if (!Icc ->Write(Icc, FillerSize, Filler)) return FALSE;
        }
        
        
       Icc -> TagOffsets[i] = Begin = Icc ->UsedSpace;
       Data = (LPBYTE) Icc -> TagPtrs[i];
       if (!Data)
              continue;

       switch (Icc -> TagNames[i]) {

       case icSigProfileDescriptionTag: 
       case icSigDeviceMfgDescTag:
       case icSigDeviceModelDescTag:
              if (!SaveDescription((const char *) Data, Icc)) return FALSE;
              break;

       case icSigRedColorantTag:
       case icSigGreenColorantTag:
       case icSigBlueColorantTag:
       case icSigMediaWhitePointTag:
       case icSigMediaBlackPointTag:           
               if (!SaveXYZNumber((LPcmsCIEXYZ) Data, Icc)) return FALSE;
               break;


       case icSigRedTRCTag:
       case icSigGreenTRCTag:
       case icSigBlueTRCTag:
       case icSigGrayTRCTag:
               if (!SaveGamma((LPGAMMATABLE) Data, Icc)) return FALSE;
               break;

       case icSigCharTargetTag:
       case icSigCopyrightTag:      
              if (!SaveText((const char *) Data, Icc)) return FALSE;
              break;

       case icSigChromaticityTag:
              if (!SaveChromaticities((LPcmsCIExyYTRIPLE) Data, Icc)) return FALSE;
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

                if (Icc ->SaveAs8Bits) {

                        if (!SaveLUT8((LPLUT) Data, Icc)) return FALSE;
                }
                else {

                        if (!SaveLUT((LPLUT) Data, Icc)) return FALSE;
                }
                break;

       case icSigProfileSequenceDescTag:               
              if (!SaveSequenceDescriptionTag((LPcmsSEQ) Data, Icc)) return FALSE;              
              break;


       case icSigNamedColor2Tag:
             if (!SaveNamedColorList((LPcmsNAMEDCOLORLIST) Data, Icc)) return FALSE;              
             break;


       case icSigCalibrationDateTimeTag:
             if (!SaveDateTimeNumber((struct tm *) Data, Icc)) return FALSE;              
             break;


       case icSigColorantTableTag:
       case icSigColorantTableOutTag:
             if (!SaveColorantTable((LPcmsNAMEDCOLORLIST) Data, Icc)) return FALSE;    
             break;


       default:
              return FALSE;
       }

       Icc -> TagSizes[i] = (Icc ->UsedSpace - Begin);
       }

        

       return TRUE;
}



// Add tags to profile structure

BOOL LCMSEXPORT cmsAddTag(cmsHPROFILE hProfile, icTagSignature sig, LPVOID Tag)
{
   BOOL rc;

   switch (sig) {

       case icSigCharTargetTag:
       case icSigCopyrightTag:             
       case icSigProfileDescriptionTag: 
       case icSigDeviceMfgDescTag:
       case icSigDeviceModelDescTag:
              rc = _cmsAddTextTag(hProfile, sig, (const char*) Tag);
              break;

       case icSigRedColorantTag:
       case icSigGreenColorantTag:
       case icSigBlueColorantTag:
       case icSigMediaWhitePointTag:
       case icSigMediaBlackPointTag:           
              rc = _cmsAddXYZTag(hProfile, sig, (const cmsCIEXYZ*) Tag);
              break; 

       case icSigRedTRCTag:
       case icSigGreenTRCTag:
       case icSigBlueTRCTag:
       case icSigGrayTRCTag:
              rc =  _cmsAddGammaTag(hProfile, sig, (LPGAMMATABLE) Tag);
              break;
                     
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
              rc =  _cmsAddLUTTag(hProfile, sig, Tag);
              break;

       case icSigChromaticityTag:
              rc =  _cmsAddChromaticityTag(hProfile, sig, (LPcmsCIExyYTRIPLE) Tag);              
              break;
        
       case icSigProfileSequenceDescTag:
              rc = _cmsAddSequenceDescriptionTag(hProfile, sig, (LPcmsSEQ) Tag);
              break;

       case icSigNamedColor2Tag:
              rc = _cmsAddNamedColorTag(hProfile, sig, (LPcmsNAMEDCOLORLIST) Tag);
             break;

       case icSigCalibrationDateTimeTag:
              rc = _cmsAddDateTimeTag(hProfile, sig, (struct tm*) Tag);
              break;
         
       case icSigColorantTableTag:
       case icSigColorantTableOutTag:
              rc = _cmsAddColorantTableTag(hProfile, sig, (LPcmsNAMEDCOLORLIST) Tag);
              break;

       default:
            cmsSignalError(LCMS_ERRC_ABORTED, "cmsAddTag: Tag '%x' is unsupported", sig);
            return FALSE;
   }

   // Check for critical tags

   switch (sig) {

   case icSigMediaWhitePointTag:
   case icSigMediaBlackPointTag:
   case icSigChromaticAdaptationTag:
       
        ReadCriticalTags((LPLCMSICCPROFILE) hProfile);
        break;

   default:;
   }

   return rc;

}

// Low-level save to disk. It closes the profile on exit

BOOL LCMSEXPORT _cmsSaveProfile(cmsHPROFILE hProfile, const char* FileName)
{       
       LPLCMSICCPROFILE Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;
       LCMSICCPROFILE Keep;
       BOOL rc;

        CopyMemory(&Keep, Icc, sizeof(LCMSICCPROFILE));
       _cmsSetSaveToDisk(Icc, NULL);    
             
       // Pass #1 does compute offsets
     
       if (!SaveHeader(Icc)) return FALSE;
       if (!SaveTagDirectory(Icc)) return FALSE;
       if (!SaveTags(Icc)) return FALSE;


       _cmsSetSaveToDisk(Icc, FileName);    


       // Pass #2 does save to file
     
       if (!SaveHeader(Icc)) goto CleanUp;
       if (!SaveTagDirectory(Icc)) goto CleanUp;
       if (!SaveTags(Icc)) goto CleanUp;
      
       rc = (Icc ->Close(Icc) == 0);
       CopyMemory(Icc, &Keep, sizeof(LCMSICCPROFILE));
       return rc;
       

   CleanUp:
     
       Icc ->Close(Icc);
       unlink(FileName);
       CopyMemory(Icc, &Keep, sizeof(LCMSICCPROFILE));
       return FALSE;
}


// Low-level save from open stream
BOOL LCMSEXPORT _cmsSaveProfileToMem(cmsHPROFILE hProfile, void *MemPtr, 
                                                           size_t* BytesNeeded)
{
    LPLCMSICCPROFILE Icc = (LPLCMSICCPROFILE) (LPSTR) hProfile;     
    LCMSICCPROFILE Keep;
    
    
    CopyMemory(&Keep, Icc, sizeof(LCMSICCPROFILE));
    
    _cmsSetSaveToMemory(Icc, NULL, 0);
    
    // Pass #1 does compute offsets
    
    if (!SaveHeader(Icc)) return FALSE;
    if (!SaveTagDirectory(Icc)) return FALSE;
    if (!SaveTags(Icc)) return FALSE;              
    
    if (!MemPtr) {
        
        // update BytesSaved so caller knows how many bytes are needed for MemPtr
        *BytesNeeded = Icc ->UsedSpace;
        return TRUE;
    }        
    
    if (*BytesNeeded < Icc ->UsedSpace) {

        // need at least UsedSpace in MemPtr to continue       
        return FALSE;
    }
    
    _cmsSetSaveToMemory(Icc, MemPtr, *BytesNeeded);
       
       
    // Pass #2 does save to file into supplied stream     
    if (!SaveHeader(Icc)) goto CleanUp;
    if (!SaveTagDirectory(Icc)) goto CleanUp;
    if (!SaveTags(Icc)) goto CleanUp;
       
    // update BytesSaved so caller knows how many bytes put into stream
    *BytesNeeded = Icc ->UsedSpace;
       
    Icc ->Close(Icc);
    CopyMemory(Icc, &Keep, sizeof(LCMSICCPROFILE));
    return TRUE;
       
CleanUp:
       
    Icc ->Close(Icc);
    CopyMemory(Icc, &Keep, sizeof(LCMSICCPROFILE));
    return FALSE;
}

