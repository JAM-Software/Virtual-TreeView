//
//  Little cms
//  Copyright (C) 1998-2000 Marti Maria
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

#include "lcms.h"

// This module handles all formats supported by lcms


// ---------------------------------------------------------------------------


// This macro return words stored as big endian

#define CHANGE_ENDIAN(w)    (WORD) ((WORD) ((w)<<8)|((w)>>8))

// These macros handles reverse flavor

#define REVERSE_FLAVOR_8(x)     ((BYTE) (0xff-(x)))
#define REVERSE_FLAVOR_16(x)    ((WORD)(0xffff-(x)))

// Supress waning about info never being used

#ifdef __BORLANDC__
#pragma warn -par
#endif

#ifdef _MSC_VER
#pragma warning(disable : 4100)
#endif

// -------------------------------------------------------- Unpacking routines.


static
LPBYTE UnrollAnyBytes(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       int nChan = T_CHANNELS(info -> InputFormat);
       register int i;

       for (i=0; i < nChan; i++) {

              wIn[i] = RGB_8_TO_16(*accum); accum++;              
       }

       return accum;
}

static
LPBYTE Unroll4Bytes(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{

       wIn[0] = RGB_8_TO_16(*accum); accum++; // C
       wIn[1] = RGB_8_TO_16(*accum); accum++; // M
       wIn[2] = RGB_8_TO_16(*accum); accum++; // Y
       wIn[3] = RGB_8_TO_16(*accum); accum++; // K

       return accum;
}


static
LPBYTE Unroll4BytesSwapFirst(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{

       wIn[3] = RGB_8_TO_16(*accum); accum++; // K
       wIn[0] = RGB_8_TO_16(*accum); accum++; // C
       wIn[1] = RGB_8_TO_16(*accum); accum++; // M
       wIn[2] = RGB_8_TO_16(*accum); accum++; // Y
       

       return accum;
}



// KYMC
static
LPBYTE Unroll4BytesSwap(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[3] = RGB_8_TO_16(*accum); accum++;  // K
       wIn[2] = RGB_8_TO_16(*accum); accum++;  // Y
       wIn[1] = RGB_8_TO_16(*accum); accum++;  // M
       wIn[0] = RGB_8_TO_16(*accum); accum++;  // C

       return accum;
}


static
LPBYTE Unroll4BytesSwapSwapFirst(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[2] = RGB_8_TO_16(*accum); accum++;  // K
       wIn[1] = RGB_8_TO_16(*accum); accum++;  // Y
       wIn[0] = RGB_8_TO_16(*accum); accum++;  // M
       wIn[3] = RGB_8_TO_16(*accum); accum++;  // C

       return accum;
}


static
LPBYTE UnrollAnyWords(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
     int nChan = T_CHANNELS(info -> InputFormat);
     register int i;
     
     for (i=0; i < nChan; i++) {

              wIn[i] = *(LPWORD) accum; accum += 2;              
     }

     return accum;       
}


static
LPBYTE Unroll4Words(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[0] = *(LPWORD) accum; accum+= 2; // C
       wIn[1] = *(LPWORD) accum; accum+= 2; // M
       wIn[2] = *(LPWORD) accum; accum+= 2; // Y
       wIn[3] = *(LPWORD) accum; accum+= 2; // K

       return accum;
}

static
LPBYTE Unroll4WordsSwapFirst(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[3] = *(LPWORD) accum; accum+= 2; // K
       wIn[0] = *(LPWORD) accum; accum+= 2; // C
       wIn[1] = *(LPWORD) accum; accum+= 2; // M
       wIn[2] = *(LPWORD) accum; accum+= 2; // Y
       
       return accum;
}


// KYMC
static
LPBYTE Unroll4WordsSwap(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[3] = *(LPWORD) accum; accum+= 2; // K
       wIn[2] = *(LPWORD) accum; accum+= 2; // Y
       wIn[1] = *(LPWORD) accum; accum+= 2; // M
       wIn[0] = *(LPWORD) accum; accum+= 2; // C

       return accum;
}

static
LPBYTE Unroll4WordsSwapSwapFirst(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[2] = *(LPWORD) accum; accum+= 2; // K
       wIn[1] = *(LPWORD) accum; accum+= 2; // Y
       wIn[0] = *(LPWORD) accum; accum+= 2; // M
       wIn[3] = *(LPWORD) accum; accum+= 2; // C

       return accum;
}


static
LPBYTE Unroll4WordsBigEndian(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[0] = CHANGE_ENDIAN(*(LPWORD) accum); accum+= 2; //C
       wIn[1] = CHANGE_ENDIAN(*(LPWORD) accum); accum+= 2; //M
       wIn[2] = CHANGE_ENDIAN(*(LPWORD) accum); accum+= 2; //Y
       wIn[3] = CHANGE_ENDIAN(*(LPWORD) accum); accum+= 2; //K

       return accum;
}

// KYMC
static
LPBYTE Unroll4WordsSwapBigEndian(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[3] = CHANGE_ENDIAN(*(LPWORD) accum); accum+= 2; //K
       wIn[2] = CHANGE_ENDIAN(*(LPWORD) accum); accum+= 2; //Y
       wIn[1] = CHANGE_ENDIAN(*(LPWORD) accum); accum+= 2; //M
       wIn[0] = CHANGE_ENDIAN(*(LPWORD) accum); accum+= 2; //C

       return accum;
}

static
LPBYTE Unroll3Bytes(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{

       wIn[0] = RGB_8_TO_16(*accum); accum++;     // R
       wIn[1] = RGB_8_TO_16(*accum); accum++;     // G
       wIn[2] = RGB_8_TO_16(*accum); accum++;     // B

       return accum;
}


// BRG

static
LPBYTE Unroll3BytesSwap(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{

       wIn[2] = RGB_8_TO_16(*accum); accum++;     // B
       wIn[1] = RGB_8_TO_16(*accum); accum++;     // G
       wIn[0] = RGB_8_TO_16(*accum); accum++;     // R

       return accum;
}

static
LPBYTE Unroll3Words(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[0] = *(LPWORD) accum; accum+= 2;  // C R
       wIn[1] = *(LPWORD) accum; accum+= 2;  // M G
       wIn[2] = *(LPWORD) accum; accum+= 2;  // Y B
       return accum;
}


static
LPBYTE Unroll3WordsSwap(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[2] = *(LPWORD) accum; accum+= 2;  // C R
       wIn[1] = *(LPWORD) accum; accum+= 2;  // M G
       wIn[0] = *(LPWORD) accum; accum+= 2;  // Y B
       return accum;
}


static
LPBYTE Unroll3WordsBigEndian(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[0] = CHANGE_ENDIAN(*(LPWORD) accum); accum+= 2;
       wIn[1] = CHANGE_ENDIAN(*(LPWORD) accum); accum+= 2;
       wIn[2] = CHANGE_ENDIAN(*(LPWORD) accum); accum+= 2;
       return accum;
}


static
LPBYTE Unroll3WordsSwapBigEndian(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[2] = CHANGE_ENDIAN(*(LPWORD) accum); accum+= 2;
       wIn[1] = CHANGE_ENDIAN(*(LPWORD) accum); accum+= 2;
       wIn[0] = CHANGE_ENDIAN(*(LPWORD) accum); accum+= 2;
       return accum;
}



// Monochrome duplicates L into RGB for null-transforms

static
LPBYTE Unroll1Byte(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[0] = wIn[1] = wIn[2] = RGB_8_TO_16(*accum); accum++;     // L
       return accum;
}


static
LPBYTE Unroll1ByteSkip2(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[0] = wIn[1] = wIn[2] = RGB_8_TO_16(*accum); accum++;     // L
       accum += 2;
       return accum;
}

static
LPBYTE Unroll1ByteReversed(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[0] = wIn[1] = wIn[2] = REVERSE_FLAVOR_16(RGB_8_TO_16(*accum)); accum++;     // L
       return accum;
}


static
LPBYTE Unroll1Word(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[0] = wIn[1] = wIn[2] = *(LPWORD) accum; accum+= 2;   // L
       return accum;
}

static
LPBYTE Unroll1WordReversed(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[0] = wIn[1] = wIn[2] = REVERSE_FLAVOR_16(*(LPWORD) accum); accum+= 2;
       return accum;
}


static
LPBYTE Unroll1WordBigEndian(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[0] = wIn[1] = wIn[2] = CHANGE_ENDIAN(*(LPWORD) accum); accum+= 2;
       return accum;
}

static
LPBYTE Unroll1WordSkip3(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[0] = wIn[1] = wIn[2] = *accum; 
        
       accum += 8;
       return accum;
}


// Monochrome + alpha. Alpha is lost

static
LPBYTE Unroll2Byte(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[0] = wIn[1] = wIn[2] = RGB_8_TO_16(*accum); accum++;     // L
       wIn[3] = RGB_8_TO_16(*accum); accum++;                       // alpha
       return accum;
}

static
LPBYTE Unroll2ByteSwapFirst(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[3] = RGB_8_TO_16(*accum); accum++;                       // alpha
       wIn[0] = wIn[1] = wIn[2] = RGB_8_TO_16(*accum); accum++;     // L       
       return accum;
}


static
LPBYTE Unroll2Word(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[0] = wIn[1] = wIn[2] = *(LPWORD) accum; accum+= 2;   // L
       wIn[3] = *(LPWORD) accum; accum += 2;                    // alpha

       return accum;
}


static
LPBYTE Unroll2WordSwapFirst(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[3] = *(LPWORD) accum; accum += 2;                    // alpha
       wIn[0] = wIn[1] = wIn[2] = *(LPWORD) accum; accum+= 2;   // L
       
       return accum;
}

static
LPBYTE Unroll2WordBigEndian(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       wIn[0] = wIn[1] = wIn[2] = CHANGE_ENDIAN(*(LPWORD) accum); accum+= 2;
       wIn[3] = CHANGE_ENDIAN(*(LPWORD) accum); accum+= 2;

       return accum;
}




static
LPBYTE UnrollPlanarBytes(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       int nChan = T_CHANNELS(info -> InputFormat);
       register int i;
       LPBYTE Init = accum;

       for (i=0; i < nChan; i++) {

              wIn[i] = RGB_8_TO_16(*accum);
              accum += info -> StrideIn;
       }

       return (Init + 1);
}



static
LPBYTE UnrollPlanarWords(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       int nChan = T_CHANNELS(info -> InputFormat);
       register int i;
       LPBYTE Init = accum;

       for (i=0; i < nChan; i++) {

              wIn[i] = *(LPWORD) accum;
              accum += (info -> StrideIn * 2);
       }

       return (Init + 2);
}



static
LPBYTE UnrollPlanarWordsBigEndian(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
       int nChan = T_CHANNELS(info -> InputFormat);
       register int i;
       LPBYTE Init = accum;

       for (i=0; i < nChan; i++) {

              wIn[i] = CHANGE_ENDIAN(*(LPWORD) accum);
              accum += (info -> StrideIn * 2);
       }

       return (Init + 2);
}


// floating point
static
LPBYTE UnrollLabDouble(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{       
        cmsFloat2LabEncoded(wIn, (LPcmsCIELab) accum);
        accum += sizeof(cmsCIELab);

        return accum;
}

static
LPBYTE UnrollXYZDouble(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{       
        cmsFloat2XYZEncoded(wIn, (LPcmsCIEXYZ) accum);
        accum += sizeof(cmsCIEXYZ);

        return accum;
}

// Inks does come in percentage
static
LPBYTE UnrollInkDouble(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
    double* Inks = (double*) accum;
    int nChan = T_CHANNELS(info -> InputFormat);
    int i;

    for (i=0; i <  nChan; i++) {

        double v = floor(Inks[i] * 655.35 + 0.5);

        if (v > 65535.0) v = 65535.0;
        if (v < 0) v = 0;

        wIn[i] = (WORD) v;
    }

    return accum + (nChan + T_EXTRA(info ->InputFormat)) * sizeof(double);
}


// Remaining cases are between 0..1.0
static
LPBYTE UnrollDouble(register _LPcmsTRANSFORM info, register WORD wIn[], register LPBYTE accum)
{
    double* Inks = (double*) accum;
    int nChan = T_CHANNELS(info -> InputFormat);
    int i;

    for (i=0; i <  nChan; i++) {

        double v = floor(Inks[i] * 65535.0 + 0.5);

        if (v > 65535.0) 
            v = 65535.0;
        if (v < 0) 
            v = 0;

        wIn[i] = (WORD) v;
    }

    return accum + (nChan + T_EXTRA(info ->InputFormat)) * sizeof(double);
}



// ----------------------------------------------------------- Packing routines


// Generic chunky for byte

static
LPBYTE PackNBytes(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       int nChan  = T_CHANNELS(info -> OutputFormat);
       register int i;

       for (i=0; i < nChan;  i++)
              *output++ = RGB_16_TO_8(wOut[i]);

       return output;
}

// Chunky reversed order bytes

static
LPBYTE PackNBytesSwap(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       int nChan  = T_CHANNELS(info -> OutputFormat);
       register int i;

       for (i=nChan-1; i >= 0;  --i)
              *output++ = RGB_16_TO_8(wOut[i]);

       return output;

}


static
LPBYTE PackNWords(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       int nChan  = T_CHANNELS(info -> OutputFormat);
       register int i;

       for (i=0; i < nChan; i++) {
              *(LPWORD) output = wOut[i];
              output += sizeof(WORD);
       }

       return output;
}

static
LPBYTE PackNWordsSwap(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       int nChan  = T_CHANNELS(info -> OutputFormat);
       register int i;

       for (i=nChan-1; i >= 0; --i) {
              *(LPWORD) output = wOut[i];
              output += sizeof(WORD);
       }

       return output;
}



static
LPBYTE PackNWordsBigEndian(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       int nChan  = T_CHANNELS(info -> OutputFormat);
       register int i;

       for (i=0; i < nChan; i++) {
              *(LPWORD) output = CHANGE_ENDIAN(wOut[i]);
              output += sizeof(WORD);
       }

       return output;
}


static
LPBYTE PackNWordsSwapBigEndian(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       int nChan  = T_CHANNELS(info -> OutputFormat);
       register int i;

       for (i=nChan-1; i >= 0; --i) {
              *(LPWORD) output = CHANGE_ENDIAN(wOut[i]);
              output += sizeof(WORD);
       }

       return output;
}


static
LPBYTE PackPlanarBytes(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       int nChan = T_CHANNELS(info -> OutputFormat);
       register int i;
       LPBYTE Init = output;

       for (i=0; i < nChan; i++) {

              *(LPBYTE) output = RGB_16_TO_8(wOut[i]);
              output += info -> StrideOut;
       }

       return (Init + 1);
}


static
LPBYTE PackPlanarWords(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       int nChan = T_CHANNELS(info -> OutputFormat);
       register int i;
       LPBYTE Init = output;

       for (i=0; i < nChan; i++) {

              *(LPWORD) output = wOut[i];
              output += (info -> StrideOut * sizeof(WORD));
       }

       return (Init + 2);
}


// CMYKcm (unrolled for speed)

static
LPBYTE Pack6Bytes(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
         *output++ = RGB_16_TO_8(wOut[0]);
         *output++ = RGB_16_TO_8(wOut[1]);
         *output++ = RGB_16_TO_8(wOut[2]);
         *output++ = RGB_16_TO_8(wOut[3]);
         *output++ = RGB_16_TO_8(wOut[4]);
         *output++ = RGB_16_TO_8(wOut[5]);

         return output;
}

// KCMYcm

static
LPBYTE Pack6BytesSwap(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       *output++ = RGB_16_TO_8(wOut[3]);
       *output++ = RGB_16_TO_8(wOut[0]);
       *output++ = RGB_16_TO_8(wOut[1]);
       *output++ = RGB_16_TO_8(wOut[2]);
       *output++ = RGB_16_TO_8(wOut[4]);
       *output++ = RGB_16_TO_8(wOut[5]);

       return output;
}

// CMYKcm
static
LPBYTE Pack6Words(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       *(LPWORD) output = wOut[0];
       output+= 2;
       *(LPWORD) output = wOut[1];
       output+= 2;
       *(LPWORD) output = wOut[2];
       output+= 2;
       *(LPWORD) output = wOut[3];
       output+= 2;
       *(LPWORD) output = wOut[4];
       output+= 2;
       *(LPWORD) output = wOut[5];
       output+= 2;

       return output;
}

// KCMYcm
static
LPBYTE Pack6WordsSwap(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       *(LPWORD) output = wOut[3];
       output+= 2;
       *(LPWORD) output = wOut[0];
       output+= 2;
       *(LPWORD) output = wOut[1];
       output+= 2;
       *(LPWORD) output = wOut[2];
       output+= 2;
       *(LPWORD) output = wOut[4];
       output+= 2;
       *(LPWORD) output = wOut[5];
       output+= 2;

       return output;
}

// CMYKcm
static
LPBYTE Pack6WordsBigEndian(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       *(LPWORD) output = CHANGE_ENDIAN(wOut[0]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[1]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[2]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[3]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[4]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[5]);
       output+= 2;

       return output;
}

// KCMYcm
static
LPBYTE Pack6WordsSwapBigEndian(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       *(LPWORD) output = CHANGE_ENDIAN(wOut[3]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[0]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[1]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[2]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[4]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[5]);
       output+= 2;

       return output;
}


static
LPBYTE Pack4Bytes(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
         *output++ = RGB_16_TO_8(wOut[0]);
         *output++ = RGB_16_TO_8(wOut[1]);
         *output++ = RGB_16_TO_8(wOut[2]);
         *output++ = RGB_16_TO_8(wOut[3]);

         return output;
}

static
LPBYTE Pack4BytesSwapFirst(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
         *output++ = RGB_16_TO_8(wOut[3]);
         *output++ = RGB_16_TO_8(wOut[0]);
         *output++ = RGB_16_TO_8(wOut[1]);
         *output++ = RGB_16_TO_8(wOut[2]);
         
         return output;
}


// ABGR

static
LPBYTE Pack4BytesSwap(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       *output++ = RGB_16_TO_8(wOut[3]);
       *output++ = RGB_16_TO_8(wOut[2]);
       *output++ = RGB_16_TO_8(wOut[1]);
       *output++ = RGB_16_TO_8(wOut[0]);

       return output;
}


static
LPBYTE Pack4BytesSwapSwapFirst(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       *output++ = RGB_16_TO_8(wOut[2]);
       *output++ = RGB_16_TO_8(wOut[1]);
       *output++ = RGB_16_TO_8(wOut[0]);
       *output++ = RGB_16_TO_8(wOut[3]);

       return output;
}


static
LPBYTE Pack4Words(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       *(LPWORD) output = wOut[0];
       output+= 2;
       *(LPWORD) output = wOut[1];
       output+= 2;
       *(LPWORD) output = wOut[2];
       output+= 2;
       *(LPWORD) output = wOut[3];
       output+= 2;

       return output;
}

// ABGR

static
LPBYTE Pack4WordsSwap(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       *(LPWORD) output = wOut[3];
       output+= 2;
       *(LPWORD) output = wOut[2];
       output+= 2;
       *(LPWORD) output = wOut[1];
       output+= 2;
       *(LPWORD) output = wOut[0];
       output+= 2;

       return output;
}

// CMYK
static
LPBYTE Pack4WordsBigEndian(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       *(LPWORD) output = CHANGE_ENDIAN(wOut[0]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[1]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[2]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[3]);
       output+= 2;

       return output;
}

// KYMC

static
LPBYTE Pack4WordsSwapBigEndian(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       *(LPWORD) output = CHANGE_ENDIAN(wOut[3]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[2]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[1]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[0]);
       output+= 2;

       return output;
}

static
LPBYTE Pack3Bytes(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       *output++ = RGB_16_TO_8(wOut[0]);
       *output++ = RGB_16_TO_8(wOut[1]);
       *output++ = RGB_16_TO_8(wOut[2]);

       return output;
}

static
LPBYTE Pack3BytesSwap(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       *output++ = RGB_16_TO_8(wOut[2]);
       *output++ = RGB_16_TO_8(wOut[1]);
       *output++ = RGB_16_TO_8(wOut[0]);

       return output;
}


static
LPBYTE Pack3Words(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       *(LPWORD) output = wOut[0];
       output+= 2;
       *(LPWORD) output = wOut[1];
       output+= 2;
       *(LPWORD) output = wOut[2];
       output+= 2;

       return output;
}

static
LPBYTE Pack3WordsSwap(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       *(LPWORD) output = wOut[2];
       output+= 2;
       *(LPWORD) output = wOut[1];
       output+= 2;
       *(LPWORD) output = wOut[0];
       output+= 2;

       return output;
}

static
LPBYTE Pack3WordsBigEndian(register _LPcmsTRANSFORM info, register WORD wOut[], register LPBYTE output)
{
       *(LPWORD) output = CHANGE_ENDIAN(wOut[0]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[1]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[2]);
       output+= 2;

       return output;
}


static
LPBYTE Pack3WordsSwapBigEndian(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
       *(LPWORD) output = CHANGE_ENDIAN(wOut[2]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[1]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[0]);
       output+= 2;

       return output;
}


static
LPBYTE Pack3BytesAndSkip1(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
       *output++ = RGB_16_TO_8(wOut[0]);
       *output++ = RGB_16_TO_8(wOut[1]);
       *output++ = RGB_16_TO_8(wOut[2]);
       output++;

       return output;
}


static
LPBYTE Pack3BytesAndSkip1SwapFirst(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
        output++;
       *output++ = RGB_16_TO_8(wOut[0]);
       *output++ = RGB_16_TO_8(wOut[1]);
       *output++ = RGB_16_TO_8(wOut[2]);
       
       return output;
}

static
LPBYTE Pack3BytesAndSkip1Swap(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
        output++;
       *output++ = RGB_16_TO_8(wOut[2]);
       *output++ = RGB_16_TO_8(wOut[1]);
       *output++ = RGB_16_TO_8(wOut[0]);
    
       return output;
}


static
LPBYTE Pack3BytesAndSkip1SwapSwapFirst(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{       
       *output++ = RGB_16_TO_8(wOut[2]);
       *output++ = RGB_16_TO_8(wOut[1]);
       *output++ = RGB_16_TO_8(wOut[0]);
       output++;
    
       return output;
}


static
LPBYTE Pack3WordsAndSkip1(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
       *(LPWORD) output = wOut[0];
       output+= 2;
       *(LPWORD) output = wOut[1];
       output+= 2;
       *(LPWORD) output = wOut[2];
       output+= 2;
       output+= 2;

       return output;
}

static
LPBYTE Pack3WordsAndSkip1Swap(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
       output+= 2;
       *(LPWORD) output = wOut[2];
       output+= 2;
       *(LPWORD) output = wOut[1];
       output+= 2;
       *(LPWORD) output = wOut[0];
       output+= 2;
       

       return output;
}


static
LPBYTE Pack3WordsAndSkip1SwapSwapFirst(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{      
       *(LPWORD) output = wOut[2];
       output+= 2;
       *(LPWORD) output = wOut[1];
       output+= 2;
       *(LPWORD) output = wOut[0];
       output+= 2;
       output+= 2;
       

       return output;
}


static
LPBYTE Pack3WordsAndSkip1BigEndian(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
       *(LPWORD) output = CHANGE_ENDIAN(wOut[0]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[1]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[2]);
       output+= 2;
       output+= 2;

       return output;
}


static
LPBYTE Pack3WordsAndSkip1SwapBigEndian(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
        output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[2]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[1]);
       output+= 2;
       *(LPWORD) output = CHANGE_ENDIAN(wOut[0]);
       output+= 2;
    

       return output;
}



static
LPBYTE Pack1Byte(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
       *output++ = RGB_16_TO_8(wOut[0]);
       return output;
}


static
LPBYTE Pack1ByteAndSkip1(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
       *output++ = RGB_16_TO_8(wOut[0]);
       output++;
       return output;
}


static
LPBYTE Pack1ByteAndSkip1SwapFirst(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
       output++;
       *output++ = RGB_16_TO_8(wOut[0]);
       
       return output;
}

static
LPBYTE Pack1Word(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
       *(LPWORD) output = wOut[0];
       output+= 2;

       return output;
}

static
LPBYTE Pack1WordBigEndian(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
       *(LPWORD) output = CHANGE_ENDIAN(wOut[0]);
       output+= 2;

       return output;
}


static
LPBYTE Pack1WordAndSkip1(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
       *(LPWORD) output = wOut[0];
       output+= 4;

       return output;
}

static
LPBYTE Pack1WordAndSkip1SwapFirst(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
       output += 2; 
       *(LPWORD) output = wOut[0];
       output+= 2;

       return output;
}


static
LPBYTE Pack1WordAndSkip1BigEndian(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
       *(LPWORD) output = CHANGE_ENDIAN(wOut[0]);
       output+= 4;

       return output;
}


// Unencoded Float values -- don't try optimize speed

static
LPBYTE PackLabDouble(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
    cmsLabEncoded2Float((LPcmsCIELab) output, wOut);
    output += sizeof(cmsCIELab);

    return output;
}

static
LPBYTE PackXYZDouble(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
    cmsXYZEncoded2Float((LPcmsCIEXYZ) output, wOut);
    output += sizeof(cmsCIEXYZ);

    return output;
}

static
LPBYTE PackInkDouble(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
    double* Inks = (double*) output;
    int nChan = T_CHANNELS(Info -> OutputFormat);
    int i;

    for (i=0; i <  nChan; i++) {

        Inks[i] = wOut[i] /  655.35;
    }

    return output + (nChan + T_EXTRA(Info ->OutputFormat)) * sizeof(double);

}


static
LPBYTE PackDouble(register _LPcmsTRANSFORM Info, register WORD wOut[], register LPBYTE output)
{
    double* Inks = (double*) output;
    int nChan = T_CHANNELS(Info -> OutputFormat);
    int i;

    for (i=0; i <  nChan; i++) {

        Inks[i] = wOut[i] /  65535.0;
    }

    return output + (nChan + T_EXTRA(Info ->OutputFormat)) * sizeof(double);

}


//  choose routine from Input identifier

_cmsFIXFN _cmsIdentifyInputFormat(_LPcmsTRANSFORM xform, DWORD dwInput)
{
       _cmsFIXFN FromInput = NULL;


       // Check Named Color

       if (xform ->InputProfile) {

           if (cmsGetDeviceClass(xform ->InputProfile) == icSigNamedColorClass) {

                if (dwInput != TYPE_NAMED_COLOR_INDEX) {
                    cmsSignalError(LCMS_ERRC_ABORTED, "Named color needs TYPE_NAMED_COLOR_INDEX");
                    return NULL;
                }
           }
                
       }

       // Unencoded modes

       if (T_BYTES(dwInput) == 0) {

           switch (T_COLORSPACE(dwInput)) {
               
           case PT_Lab: 
                    FromInput = UnrollLabDouble;
                    break;
           case PT_XYZ: 
                    FromInput = UnrollXYZDouble;
                    break;

           // 0.0 .. 1.0 range

           case PT_GRAY:
           case PT_RGB:
           case PT_YCbCr:
           case PT_YUV:
           case PT_YUVK:
           case PT_HSV:
           case PT_HLS:
           case PT_Yxy: 
                    FromInput = UnrollDouble;
                    break;

            // Inks (%) 0.0 .. 100.0

           default:
                    FromInput = UnrollInkDouble;
                    break;
           }
                    
       }
       else     
       
       if (T_PLANAR(dwInput)) {

       switch (T_BYTES(dwInput)) {

              case 1: 
                      FromInput = UnrollPlanarBytes;
                      break;

              case 2:
                  
                      if (T_ENDIAN16(dwInput))
                            FromInput = UnrollPlanarWordsBigEndian;
                      else
                            FromInput = UnrollPlanarWords;                
                      break;

              default:;
       }
       }
       else {

       switch (T_BYTES(dwInput)) {

       case 1: // 1 byte per channel

              switch (T_CHANNELS(dwInput) + T_EXTRA(dwInput))
              {
              case 1: if (T_FLAVOR(dwInput))
                                FromInput = Unroll1ByteReversed;
                            else
                                  FromInput = Unroll1Byte;
                      break;

              case 2: if (T_SWAPFIRST(dwInput))
                        FromInput = Unroll2ByteSwapFirst;
                      else
                        FromInput = Unroll2Byte;
                      break;

              case 3: if (T_DOSWAP(dwInput))
                            FromInput = Unroll3BytesSwap;
                      else {
                            if (T_EXTRA(dwInput) == 2)
                                FromInput = Unroll1ByteSkip2;
                            else
                                FromInput = Unroll3Bytes;
                      }
                      break;
              case 4:
                      if (T_DOSWAP(dwInput)) {
                            if (T_SWAPFIRST(dwInput))

                                FromInput = Unroll4BytesSwapSwapFirst;
                            else 
                                FromInput = Unroll4BytesSwap;
                      }
                      else {
                            if (T_SWAPFIRST(dwInput))
                                FromInput = Unroll4BytesSwapFirst;
                            else
                                FromInput = Unroll4Bytes;
                      }
                      break;

              case 5:
              case 6:
              case 7:
              case 8:
                   if (!T_DOSWAP(dwInput) && !T_SWAPFIRST(dwInput))
                       FromInput = UnrollAnyBytes;
                   break;

                    
              default:;
              }
              break;


       case 2: // 1 word per channel

              switch (T_CHANNELS(dwInput) + T_EXTRA(dwInput))
              {
              case 1: if (T_ENDIAN16(dwInput))
                            FromInput = Unroll1WordBigEndian;
                      else
                          if (T_FLAVOR(dwInput))
                                FromInput = Unroll1WordReversed;
                            else
                                  FromInput = Unroll1Word;
                      break;

              case 2: if (T_ENDIAN16(dwInput))
                            FromInput = Unroll2WordBigEndian;
                        else {
                          if (T_SWAPFIRST(dwInput))
                              FromInput = Unroll2WordSwapFirst;
                          else 
                              FromInput = Unroll2Word;              
                        }
                        break;

              case 3: if (T_DOSWAP(dwInput)) {
                            if (T_ENDIAN16(dwInput))
                                   FromInput = Unroll3WordsSwapBigEndian;
                            else
                                   FromInput = Unroll3WordsSwap;
                      }
                      else {
                            if (T_ENDIAN16(dwInput))
                                   FromInput = Unroll3WordsBigEndian;
                            else
                                   FromInput = Unroll3Words;
                      }
                      break;

              case 4: if (T_DOSWAP(dwInput)) {
                            if (T_ENDIAN16(dwInput))
                                   FromInput = Unroll4WordsSwapBigEndian;
                            else {
                                   
                                    if (T_SWAPFIRST(dwInput))
                                        FromInput = Unroll4WordsSwapSwapFirst;
                                    else
                                        FromInput = Unroll4WordsSwap;
                            }
                            
                      }
                      else {

                            if (T_EXTRA(dwInput) == 3)
                                    FromInput = Unroll1WordSkip3;
                            else

                            if (T_ENDIAN16(dwInput))
                                   FromInput = Unroll4WordsBigEndian;
                            else {
                                  if (T_SWAPFIRST(dwInput))
                                    FromInput = Unroll4WordsSwapFirst;
                                   else 
                                    FromInput = Unroll4Words;
                            }
                      }
                      break;

              case 5:
              case 6:
              case 7:
              case 8:
                    if (!T_DOSWAP(dwInput) && !T_SWAPFIRST(dwInput))
                       FromInput = UnrollAnyWords;
                    break;

              }
              break;

       default:;
       }
       }

       if (!FromInput)
              cmsSignalError(LCMS_ERRC_ABORTED, "Unknown input format");

       return FromInput;
}

//  choose routine from Input identifier

_cmsFIXFN _cmsIdentifyOutputFormat(_LPcmsTRANSFORM xform, DWORD dwOutput)
{
       _cmsFIXFN ToOutput = NULL;


       if (T_BYTES(dwOutput) == 0) {

           switch (T_COLORSPACE(dwOutput)) {
               
           case PT_Lab: 
                    ToOutput = PackLabDouble;
                    break;
           case PT_XYZ: 
                    ToOutput = PackXYZDouble;
                    break;

           // 0.0 .. 1.0 range
           case PT_GRAY:
           case PT_RGB:
           case PT_YCbCr:
           case PT_YUV:
           case PT_YUVK:
           case PT_HSV:
           case PT_HLS:
           case PT_Yxy: 
                    ToOutput = PackDouble;
                    break;

            // Inks (%) 0.0 .. 100.0

           default:
                    ToOutput = PackInkDouble;
                    break;
           }
                    
       }
       else 

       if (T_PLANAR(dwOutput)) {

       switch (T_BYTES(dwOutput)) {

              case 1: ToOutput = PackPlanarBytes;
                      break;

              case 2:if (!T_ENDIAN16(dwOutput))
                            ToOutput = PackPlanarWords;
                      break;

              default:;
       }
       }
       else {

              switch (T_BYTES(dwOutput)) {

              case 1:
                     switch (T_CHANNELS(dwOutput))
                     {
                     case 1:
                            ToOutput = Pack1Byte;
                            if (T_EXTRA(dwOutput) == 1) {
                                if (T_SWAPFIRST(dwOutput))
                                   ToOutput = Pack1ByteAndSkip1SwapFirst;
                                else
                                   ToOutput = Pack1ByteAndSkip1;
                            }
                            break;

                     case 3:
                         switch (T_EXTRA(dwOutput)) {

                         case 0: if (T_DOSWAP(dwOutput))
                                   ToOutput = Pack3BytesSwap;
                             else
                                   ToOutput = Pack3Bytes;
                             break;

                         case 1: if (T_DOSWAP(dwOutput)) {

                                    if (T_SWAPFIRST(dwOutput))  
                                        ToOutput = Pack3BytesAndSkip1SwapSwapFirst;
                                    else
                                        ToOutput = Pack3BytesAndSkip1Swap;
                                 }
                             else {
                                   if (T_SWAPFIRST(dwOutput))
                                    ToOutput = Pack3BytesAndSkip1SwapFirst;
                                   else 
                                    ToOutput = Pack3BytesAndSkip1;
                             }
                             break;

                         default:;
                         }
                         break;

                     case 4: if (T_EXTRA(dwOutput) == 0)
                             {
                             if (T_DOSWAP(dwOutput)) {
                                 if (T_SWAPFIRST(dwOutput))
                                        ToOutput = Pack4BytesSwapSwapFirst;
                                 else
                                        ToOutput = Pack4BytesSwap;
                             }
                             else {
                                 if (T_SWAPFIRST(dwOutput))
                                        ToOutput = Pack4BytesSwapFirst;
                                 else
                                        ToOutput = Pack4Bytes;
                             }
                             }
                             break;

                     // Hexachrome separations.
                     case 6: if (T_EXTRA(dwOutput) == 0)
                            {
                            if( T_DOSWAP(dwOutput))
                                   ToOutput = Pack6BytesSwap;
                            else
                                   ToOutput = Pack6Bytes;
                            }
                            break;

                     case 7:
                     case 8:
                     case 9:
                     case 10:
                     case 11:
                     case 12:
                     case 13:
                     case 14:
                     case 15:

                            if ((T_EXTRA(dwOutput) == 0) && (T_SWAPFIRST(dwOutput) == 0))
                            {
                                   if (T_DOSWAP(dwOutput))
                                          ToOutput = PackNBytesSwap;
                                   else
                                          ToOutput = PackNBytes;
                            }
                            break;

                     default:;
                     }
                     break;


              case 2:

                     switch (T_CHANNELS(dwOutput)) {

                     case 1:
                            if (T_ENDIAN16(dwOutput))

                                   ToOutput = Pack1WordBigEndian;
                            else
                                   ToOutput = Pack1Word;

                            if (T_EXTRA(dwOutput) == 1) {

                               if (T_ENDIAN16(dwOutput))

                                   ToOutput = Pack1WordAndSkip1BigEndian;
                               else {
                                   if (T_SWAPFIRST(dwOutput))
                                      ToOutput = Pack1WordAndSkip1SwapFirst;
                                   else 
                                      ToOutput = Pack1WordAndSkip1;
                               }
                            }
                            break;

                     case 3:

                         switch (T_EXTRA(dwOutput)) {

                         case 0:
                               if (T_DOSWAP(dwOutput)) {

                                   if (T_ENDIAN16(dwOutput))

                                          ToOutput = Pack3WordsSwapBigEndian;
                                   else
                                          ToOutput = Pack3WordsSwap;
                               }
                               else {
                                   if (T_ENDIAN16(dwOutput))

                                      ToOutput = Pack3WordsBigEndian;
                                   else
                                      ToOutput = Pack3Words;
                                   }
                             break;

                         case 1: if (T_DOSWAP(dwOutput)) {

                                   if (T_ENDIAN16(dwOutput))

                                          ToOutput = Pack3WordsAndSkip1SwapBigEndian;
                                   else {
                                       if (T_SWAPFIRST(dwOutput)) 
                                          ToOutput = Pack3WordsAndSkip1SwapSwapFirst;
                                       else 
                                          ToOutput = Pack3WordsAndSkip1Swap;
                                   }
                             }
                             else  {
                                   if (T_ENDIAN16(dwOutput))
                                          ToOutput = Pack3WordsAndSkip1BigEndian;
                                   else
                                          ToOutput = Pack3WordsAndSkip1;
                                   }
                         default:;
                         }
                         break;

                     case 4: if (T_EXTRA(dwOutput) == 0) {

                                   if (T_DOSWAP(dwOutput)) {

                                           if (T_ENDIAN16(dwOutput))
                                                 ToOutput = Pack4WordsSwapBigEndian;
                                           else
                                                 ToOutput = Pack4WordsSwap;
                                   }
                                   else {

                                   if (T_ENDIAN16(dwOutput))
                                          ToOutput = Pack4WordsBigEndian;
                                   else
                                          ToOutput = Pack4Words;
                                   }
                            }
                            break;

                     case 6: if (T_EXTRA(dwOutput) == 0) {

                                   if (T_DOSWAP(dwOutput)) {

                                          if (T_ENDIAN16(dwOutput))
                                                 ToOutput = Pack6WordsSwapBigEndian;
                                          else
                                                 ToOutput = Pack6WordsSwap;
                                   }
                                   else {

                                   if (T_ENDIAN16(dwOutput))
                                          ToOutput = Pack6WordsBigEndian;
                                   else
                                          ToOutput = Pack6Words;
                                   }
                             }
                             break;


                     case 5:
                     case 7:
                     case 8:
                     case 9:
                     case 10:
                     case 11:
                     case 12:
                     case 13:
                     case 14:
                     case 15: if ((T_EXTRA(dwOutput) == 0) && (T_SWAPFIRST(dwOutput) == 0)) {

                                   if (T_DOSWAP(dwOutput)) {

                                          if (T_ENDIAN16(dwOutput))
                                                 ToOutput = PackNWordsSwapBigEndian;
                                          else
                                                 ToOutput = PackNWordsSwap;
                                   }
                                   else {

                                          if (T_ENDIAN16(dwOutput))
                                                 ToOutput = PackNWordsBigEndian;
                                          else
                                                 ToOutput = PackNWords;
                                          }
                             }
                             break;

                     default:;
                     }
                     break;

              default:;
              }
              }

              if (!ToOutput)
                     cmsSignalError(LCMS_ERRC_ABORTED, "Unknown output format");

              return ToOutput;
}

// User formatters for (weird) cases not already included

void LCMSEXPORT cmsSetUserFormatters(cmsHTRANSFORM hTransform, cmsFORMATTER Input, cmsFORMATTER Output)
{
    _LPcmsTRANSFORM xform = (_LPcmsTRANSFORM) (LPSTR) hTransform;
    
    xform ->FromInput = (_cmsFIXFN) Input;
    xform ->ToOutput  = (_cmsFIXFN) Output;
}

void LCMSEXPORT cmsGetUserFormatters(cmsHTRANSFORM hTransform, cmsFORMATTER* Input, cmsFORMATTER* Output)
{
    _LPcmsTRANSFORM xform = (_LPcmsTRANSFORM) (LPSTR) hTransform;
    
    *Input =  (cmsFORMATTER) xform ->FromInput;
    *Output = (cmsFORMATTER) xform ->ToOutput;
}


// Change format of yet existing transform. No colorspace checking is performed

void LCMSEXPORT cmsChangeBuffersFormat(cmsHTRANSFORM hTransform, 
                                        DWORD dwInputFormat, 
                                        DWORD dwOutputFormat)
{

    cmsSetUserFormatters(hTransform, 
                        (cmsFORMATTER) _cmsIdentifyInputFormat(hTransform, dwInputFormat),
                        (cmsFORMATTER) _cmsIdentifyOutputFormat(hTransform, dwOutputFormat));
}
