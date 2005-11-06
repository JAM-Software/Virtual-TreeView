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

#include "lcms.h"

// #define DEBUG 1

// Transformations stuff
// -----------------------------------------------------------------------

// Interface

cmsHTRANSFORM LCMSEXPORT cmsCreateTransform(cmsHPROFILE Input,
                                       DWORD InputFormat,
                                       cmsHPROFILE Output,
                                       DWORD OutputFormat,
                                       int Intent,
                                       DWORD dwFlags);

cmsHTRANSFORM LCMSEXPORT cmsCreateProofingTransform(cmsHPROFILE Input,
                                               DWORD InputFormat,
                                               cmsHPROFILE Output,
                                               DWORD OutputFormat,
                                               cmsHPROFILE Proofing,
                                               int Intent,
                                               int ProofingIntent,
                                               DWORD dwFlags);


void         LCMSEXPORT cmsDeleteTransform(cmsHTRANSFORM hTransform);

void         LCMSEXPORT cmsDoTransform(cmsHTRANSFORM Transform,
                                  LPVOID InputBuffer,
                                  LPVOID OutputBuffer, unsigned int Size);

void         LCMSEXPORT cmsGetAlarmCodes(int *r, int *g, int *b);
void         LCMSEXPORT cmsSetAlarmCodes(int r, int g, int b);
BOOL         LCMSEXPORT cmsIsIntentSupported(cmsHPROFILE hProfile,
                                                int Intent, int UsedDirection);

// -------------------------------------------------------------------------


// Alarm RGB codes

static WORD AlarmR = 0x8fff, AlarmG = 0x8fff, AlarmB = 0x8fff;

// Tag tables, soted by intents

static icTagSignature Device2PCS[] = {icSigAToB0Tag,       // Perceptual
                                      icSigAToB1Tag,       // Relative colorimetric
                                      icSigAToB2Tag,       // Saturation
                                      icSigAToB1Tag };     // Absolute colorimetric
                                                           // (Relative/WhitePoint)

static icTagSignature PCS2Device[] = {icSigBToA0Tag,       // Perceptual
                                      icSigBToA1Tag,       // Relative colorimetric
                                      icSigBToA2Tag,       // Saturation
                                      icSigBToA1Tag };     // Absolute colorimetric
                                                           // (Relative/WhitePoint)


static icTagSignature Preview[]    = {icSigPreview0Tag,
                                      icSigPreview1Tag,
                                      icSigPreview2Tag,
                                      icSigPreview1Tag };





// --------------------------------Stages--------------------------------------

// Following routines does implement several kind of steps inside 
// transform. On building the transform, code chooses adequate.


// From Shaper-Matrix to PCS

static
void ShaperMatrixToPCS(struct _cmstransform_struct *p,
                     WORD In[3], WORD Out[3])
{
       cmsEvalMatShaper(p -> InMatShaper, In, Out);
}

// From LUT to PCS

static
void LUTtoPCS(struct _cmstransform_struct *p,
                     WORD In[], WORD Out[3])
{
       cmsEvalLUT(p -> Device2PCS, In, Out);
}

// From indexed named color to PCS 

static
void NC2toPCS(struct _cmstransform_struct *p,
                     WORD In[], WORD Out[3])
{
    int index = In[0];

    CopyMemory(Out, p ->NamedColorList->List[index].PCS, 3 * sizeof(WORD));
}

// From PCS to Shaper-Matrix

static
void PCStoShaperMatrix(struct _cmstransform_struct *p,
                     WORD In[3], WORD Out[3])
{
       cmsEvalMatShaper(p -> OutMatShaper, In, Out);
}

// From PCS to LUT

static
void PCStoLUT(struct _cmstransform_struct *p,
                     WORD In[3], WORD Out[])
{
       cmsEvalLUT(p -> PCS2Device, In, Out);
}




// ----------------------- TRANSFORMATIONS --------------------------


// Inlining some assignations

#define COPY_3CHANS(to, from) { to[0]=from[0]; to[1]=from[1]; to[2]=from[2]; }


// Null transformation, only hold channels

static
void NullXFORM(_LPcmsTRANSFORM p,
                     LPVOID in,
                     LPVOID out, unsigned int Size)
{
       register LPBYTE accum;
       register LPBYTE output;
       WORD wIn[MAXCHANNELS];
       register unsigned int i, n;


       accum  = (LPBYTE) in;
       output = (LPBYTE) out;
       n = Size;                    // Buffer len

       for (i=0; i < n; i++)
       {
       accum = p -> FromInput(p, wIn, accum);
       output = p -> ToOutput(p, wIn, output);
       }

}


// This is the "normal" proofing transform

static
void NormalXFORM(_LPcmsTRANSFORM p,
                     LPVOID in,
                     LPVOID out, unsigned int Size)
{
       register LPBYTE accum;
       register LPBYTE output;
       WORD wIn[MAXCHANNELS], wOut[MAXCHANNELS];
       WORD wStageABC[3], wPCS[3], wStageLMN[MAXCHANNELS];
       WORD wGamut[1];
       register unsigned int i, n;



       accum  = (LPBYTE) in;
       output = (LPBYTE) out;
       n = Size;                    // Buffer len

       for (i=0; i < n; i++)
       {

       accum = p -> FromInput(p, wIn, accum);

       p -> FromDevice(p, wIn, wStageABC);

       if (p -> Stage1) {

              p -> Stage1(wStageABC, wPCS, &p->m1, &p->of1);

              if (wPCS[0] == 0xFFFF &&
                  wPCS[1] == 0xFFFF &&
                  wPCS[2] == 0xFFFF) {

                     // White cutoff

                     output = p -> ToOutput((_LPcmsTRANSFORM) p,
                                   _cmsWhiteBySpace(cmsGetColorSpace(p -> OutputProfile)),
                                   output);
                     continue;
                     }
              }
       else
              COPY_3CHANS(wPCS, wStageABC);


       if (p->Gamut)
       {

       // Gamut check, enabled across CLUT

       cmsEvalLUT(p -> Gamut, wPCS, wGamut);
       
#ifdef DEBUG         
         printf("Gamut %d\n", wGamut[0]);
#endif

       if (wGamut[0] >= 1) {              // 8 for roundoff 
        
              
              wOut[0] = AlarmR;          // Gamut alarm
              wOut[1] = AlarmG;
              wOut[2] = AlarmB;
              wOut[3] = 0;

              output = p -> ToOutput((_LPcmsTRANSFORM)p, wOut, output);
              continue;
              }
       }

       if (p -> Preview)
       {
              WORD wPreview[3];    // PCS

              cmsEvalLUT(p -> Preview, wPCS, wPreview);
              COPY_3CHANS(wPCS, wPreview);
       }

       if (p -> Stage2) {
              p -> Stage2(wPCS, wStageLMN, &p->m2, &p->of2);
              if (wPCS[0] == 0xFFFF &&
                  wPCS[1] == 0xFFFF &&
                  wPCS[2] == 0xFFFF) {

                     // White cutoff

                     output = p -> ToOutput((_LPcmsTRANSFORM)p,
                                   _cmsWhiteBySpace(cmsGetColorSpace(p -> OutputProfile)),
                                   output);

                     continue;
                     }

              }
       else
              COPY_3CHANS(wStageLMN, wPCS);

       // Here wOut may come as MAXCHANNELS channels

       p -> ToDevice(p, wStageLMN, wOut);

       output = p -> ToOutput((_LPcmsTRANSFORM)p, wOut, output);
       }
}

// Using precalculated LUT

static
void PrecalculatedXFORM(_LPcmsTRANSFORM p,
                     LPVOID in,
                     LPVOID out, unsigned int Size)
{
       register LPBYTE accum;
       register LPBYTE output;
       WORD wIn[MAXCHANNELS], wOut[MAXCHANNELS];
       register unsigned int i, n;


       accum  = (LPBYTE) in;
       output = (LPBYTE) out;
       n = Size;                    // Buffer len

       for (i=0; i < n; i++)
       {
       accum = p -> FromInput(p, wIn, accum);
       cmsEvalLUT(p -> DeviceLink, wIn, wOut);
       output = p -> ToOutput(p, wOut, output);
       }
}


// Using smelted Matrix/Shaper

static
void MatrixShaperXFORM(_LPcmsTRANSFORM p,
                     LPVOID in,
                     LPVOID out, unsigned int Size)
{
       register LPBYTE accum;
       register LPBYTE output;
       WORD wIn[MAXCHANNELS], wOut[MAXCHANNELS];
       register unsigned int i, n;


       accum  = (LPBYTE) in;
       output = (LPBYTE) out;
       n = Size;                    // Buffer len

       for (i=0; i < n; i++)
       {
       accum = p -> FromInput(p, wIn, accum);
       cmsEvalMatShaper(p -> SmeltMatShaper, wIn, wOut);
       output = p -> ToOutput(p, wOut, output);
       }
}


// Using Named color input table

static
void NC2deviceXform(_LPcmsTRANSFORM p,
                     LPVOID in,
                     LPVOID out, unsigned int Size)
{

       register LPBYTE accum;
       register LPBYTE output;
       WORD wIn[MAXCHANNELS], wOut[MAXCHANNELS];
       register unsigned int i;


       accum  = (LPBYTE) in;
       output = (LPBYTE) out;

       for (i=0; i < Size; i++) {

       accum = p -> FromInput(p, wIn, accum);      
       CopyMemory(wOut, p ->NamedColorList->List[wIn[0]].DeviceColorant, sizeof(WORD) * MAXCHANNELS);
       output = p -> ToOutput(p, wOut, output);
       }
        
}



// --------------------------------------------------------------------------
// Build a LUT based on shape-matrix method.

// Monochrome version

static
LPMATSHAPER cmsBuildGrayInputMatrixShaper(cmsHPROFILE hProfile)
{
       cmsCIEXYZ Illuminant;
       LPGAMMATABLE GrayTRC, Shapes[3];
       LPMATSHAPER MatShaper;
       MAT3 Scale;

       GrayTRC = cmsReadICCGamma(hProfile, icSigGrayTRCTag);        // Y
       cmsTakeIluminant(&Illuminant, hProfile);

       Shapes[0] = cmsDupGamma(GrayTRC);
       Shapes[1] = cmsDupGamma(GrayTRC);
       Shapes[2] = cmsDupGamma(GrayTRC); 
       
       if (!Shapes[0] || !Shapes[1] || !Shapes[2])
              return NULL;
      
       cmsFreeGamma(GrayTRC);

       // R=G=B as precondition

       VEC3init(&Scale.v[0], Illuminant.X/3,  Illuminant.X/3,  Illuminant.X/3);
       VEC3init(&Scale.v[1], Illuminant.Y/3,  Illuminant.Y/3,  Illuminant.Y/3);
       VEC3init(&Scale.v[2], Illuminant.Z/3,  Illuminant.Z/3,  Illuminant.Z/3);
      
       
       MatShaper = cmsAllocMatShaper(&Scale, Shapes, MATSHAPER_INPUT);
       cmsFreeGammaTriple(Shapes);
       return MatShaper;

}


// Monochrome as output

static
LPMATSHAPER cmsBuildGrayOutputMatrixShaper(cmsHPROFILE hProfile)
{
       cmsCIEXYZ Illuminant;
       LPGAMMATABLE GrayTRC, Shapes[3];
       LPMATSHAPER MatShaper;
       MAT3 Scale;

       GrayTRC = cmsReadICCGammaReversed(hProfile, icSigGrayTRCTag);   // Y
       cmsTakeIluminant(&Illuminant, hProfile);

       Shapes[0] = cmsDupGamma(GrayTRC);
       Shapes[1] = cmsDupGamma(GrayTRC);
       Shapes[2] = cmsDupGamma(GrayTRC); 
       
       if (!Shapes[0] || !Shapes[1] || !Shapes[2])
              return NULL;
      
       cmsFreeGamma(GrayTRC);

       VEC3init(&Scale.v[0], 0,  1.0/Illuminant.Y,  0);
       VEC3init(&Scale.v[1], 0,  1.0/Illuminant.Y,  0);
       VEC3init(&Scale.v[2], 0,  1.0/Illuminant.Y,  0);
           
       
       MatShaper = cmsAllocMatShaper(&Scale, Shapes, MATSHAPER_OUTPUT);
       cmsFreeGammaTriple(Shapes);
       return MatShaper;

}



// Input matrix, only in XYZ

LPMATSHAPER cmsBuildInputMatrixShaper(cmsHPROFILE InputProfile, LPDWORD dwFlags)
{
       MAT3 DoubleMat;
       LPGAMMATABLE Shapes[3];
       LPMATSHAPER InMatSh;

       // Check if this is a grayscale profile. If so, build
       // appropiate conversion tables. The tables are the PCS
       // iluminant, scaled across GrayTRC

       if (cmsGetColorSpace(InputProfile) == icSigGrayData)
       {
              if (dwFlags) *dwFlags |= cmsFLAGS_NOTPRECALC;
              return cmsBuildGrayInputMatrixShaper(InputProfile);              
       }

       if (!cmsReadICCMatrixRGB2XYZ(&DoubleMat, InputProfile))
                     return NULL;

       Shapes[0] = cmsReadICCGamma(InputProfile, icSigRedTRCTag);
       Shapes[1] = cmsReadICCGamma(InputProfile, icSigGreenTRCTag);
       Shapes[2] = cmsReadICCGamma(InputProfile, icSigBlueTRCTag);

       if (!Shapes[0] || !Shapes[1] || !Shapes[2])
                     return NULL;

       InMatSh = cmsAllocMatShaper(&DoubleMat, Shapes, MATSHAPER_INPUT);

       cmsFreeGammaTriple(Shapes);
      
       return InMatSh;
}


// Output style matrix-shaper


LPMATSHAPER cmsBuildOutputMatrixShaper(cmsHPROFILE OutputProfile, LPDWORD dwFlags)
{
       MAT3 DoubleMat, DoubleInv;
       LPGAMMATABLE InverseShapes[3];
       LPMATSHAPER OutMatSh;

       

       if (cmsGetColorSpace(OutputProfile) == icSigGrayData)
       {
              if (dwFlags) *dwFlags |= cmsFLAGS_NOTPRECALC;
              return cmsBuildGrayOutputMatrixShaper(OutputProfile);              
       }


       if (!cmsReadICCMatrixRGB2XYZ(&DoubleMat, OutputProfile))
                     return NULL;

       if (MAT3inverse(&DoubleMat, &DoubleInv) < 0)
              return NULL;

     
       InverseShapes[0] = cmsReadICCGammaReversed(OutputProfile, icSigRedTRCTag);
       InverseShapes[1] = cmsReadICCGammaReversed(OutputProfile, icSigGreenTRCTag);
       InverseShapes[2] = cmsReadICCGammaReversed(OutputProfile, icSigBlueTRCTag);
       
       OutMatSh = cmsAllocMatShaper(&DoubleInv, InverseShapes, MATSHAPER_OUTPUT);

       cmsFreeGammaTriple(InverseShapes);
       
       return OutMatSh;
}



// This function builds a transform matrix chaining parameters

static
BOOL cmsBuildSmeltMatShaper(_LPcmsTRANSFORM p)
{
       MAT3 From, To, ToInv, Transfer;
       LPGAMMATABLE In[3], InverseOut[3];
       
        
       if (!cmsReadICCMatrixRGB2XYZ(&From, p -> InputProfile))
                     return FALSE;


       if (!cmsReadICCMatrixRGB2XYZ(&To, p -> OutputProfile))
                     return FALSE;

               
       // invert dest
       
       if (MAT3inverse(&To, &ToInv) < 0)
                        return FALSE;

       // Multiply
        MAT3per(&Transfer, &ToInv, &From); 
    
            
        // Read gamma curves

        In[0] = cmsReadICCGamma(p -> InputProfile, icSigRedTRCTag);
        In[1] = cmsReadICCGamma(p -> InputProfile, icSigGreenTRCTag);
        In[2] = cmsReadICCGamma(p -> InputProfile, icSigBlueTRCTag);

        if (!In[0] || !In[1] || !In[2])
                     return FALSE;
            

        InverseOut[0] = cmsReadICCGammaReversed(p -> OutputProfile, icSigRedTRCTag);
        InverseOut[1] = cmsReadICCGammaReversed(p -> OutputProfile, icSigGreenTRCTag);
        InverseOut[2] = cmsReadICCGammaReversed(p -> OutputProfile, icSigBlueTRCTag);

        p -> SmeltMatShaper = cmsAllocMatShaper2(&Transfer, In, InverseOut, MATSHAPER_ALLSMELTED);

        cmsFreeGammaTriple(In);
        
        cmsFreeGammaTriple(InverseOut);
        
        return (p -> SmeltMatShaper != NULL);
}




// Conversion between PCS ------------------------------------------

// Identifies intent archieved by LUT, only absolute colorimetric ones
// are handled separatly

static
int GetPhase(cmsHPROFILE hProfile)
{
       icColorSpaceSignature PCS;
       icRenderingIntent Intent;

       PCS    = cmsGetPCS(hProfile);
       Intent = (icRenderingIntent) cmsTakeRenderingIntent(hProfile);

       switch (PCS) {
       case icSigXYZData:
                     if (Intent == icAbsoluteColorimetric)

                            return XYZAbs;
                     else
                            return XYZRel;

       case icSigLabData:

                     if (Intent == icAbsoluteColorimetric)

                            return LabAbs;
                     else
                            return LabRel;

       default:
                     cmsSignalError(LCMS_ERRC_ABORTED, "Invalid PCS");
       }

       return XYZRel;
}


static
void TakeConversionRoutines(_LPcmsTRANSFORM p)
{
       cmsCIEXYZ BlackPointIn, WhitePointIn, IlluminantIn;
       cmsCIEXYZ BlackPointOut, WhitePointOut, IlluminantOut;
       cmsCIEXYZ BlackPointProof, WhitePointProof, IlluminantProof;


       cmsTakeIluminant(&IlluminantIn,  p -> InputProfile);
       cmsTakeMediaWhitePoint(&WhitePointIn,  p -> InputProfile);
       cmsTakeMediaBlackPoint(&BlackPointIn,  p -> InputProfile);

       cmsTakeIluminant(&IlluminantOut,  p -> OutputProfile);
       cmsTakeMediaWhitePoint(&WhitePointOut,  p -> OutputProfile);
       cmsTakeMediaBlackPoint(&BlackPointOut,  p -> OutputProfile);


       if (p -> Preview == NULL)     // Non-proofing
       {
              cmsChooseCnvrt(p -> Intent == INTENT_ABSOLUTE_COLORIMETRIC,

                 p -> Phase1,
                             &BlackPointIn,
                             &WhitePointIn,
                             &IlluminantIn,

                 p -> Phase3,
                             &BlackPointOut,
                             &WhitePointOut,
                             &IlluminantOut,

                 &p->Stage1,
                 &p->m1, &p->of1);

       }
       else // Proofing
       {

       cmsTakeIluminant(&IlluminantProof,        p -> PreviewProfile);
       cmsTakeMediaWhitePoint(&WhitePointProof,  p -> PreviewProfile);
       cmsTakeMediaBlackPoint(&BlackPointProof,  p -> PreviewProfile);

       cmsChooseCnvrt(p -> Intent == INTENT_ABSOLUTE_COLORIMETRIC,

                 p -> Phase1,
                             &BlackPointIn,
                             &WhitePointIn,
                             &IlluminantIn,

                 p -> Phase2,
                             &BlackPointProof,
                             &WhitePointProof,
                             &IlluminantProof,

                 &p->Stage1,
                 &p->m1, &p->of1);

       cmsChooseCnvrt(p -> ProofIntent == INTENT_ABSOLUTE_COLORIMETRIC,

                 p -> Phase2,
                             &BlackPointProof,
                             &WhitePointProof,
                             &IlluminantProof,

                 p -> Phase3,
                             &BlackPointOut,
                             &WhitePointOut,
                             &IlluminantOut,
                 &p->Stage2,
                 &p->m2, &p->of2);


       }

}


// Black, White compensation

static
void PatchLUT(LPLUT Grid, WORD At[], WORD Value[],
                     int nChannelsOut, int nChannelsIn)
{
       LPL16PARAMS p16  = &Grid -> CLut16params;
       double     px, py, pz, pw;
       int        x0, y0, z0, w0;
       int        i, index;

       px = ((double) At[0] * (p16->Domain)) / 65535.0;
       py = ((double) At[1] * (p16->Domain)) / 65535.0;
       pz = ((double) At[2] * (p16->Domain)) / 65535.0;
       pw = ((double) At[3] * (p16->Domain)) / 65535.0;

       x0 = (int) floor(px);
       y0 = (int) floor(py);
       z0 = (int) floor(pz);
       w0 = (int) floor(pw);

       if (nChannelsIn == 4)

              index = p16 -> opta4 * x0 +
                      p16 -> opta3 * y0 +
                      p16 -> opta2 * z0 +
                      p16 -> opta1 * w0;
       else
              index = p16 -> opta3 * x0 +
                      p16 -> opta2 * y0 +
                      p16 -> opta1 * z0;


       for (i=0; i < nChannelsOut; i++)
              Grid -> T[index + i] = Value[i];

}

// Replace endpoint of prelinearization curves, only
// if these does exist

       
static
void PatchCurves(LPLUT Grid)
{
    unsigned int i;



    if (Grid ->wFlags & LUT_HASTL1) {

        for (i=0; i < Grid ->InputChan; i++) {

        Grid ->L1[i][Grid->In16params.Domain] = 0xFFFF;
        Grid ->L1[i][0] = 0;

        _cmsSmoothEndpoints(Grid->L1[i], Grid->InputEntries);                   
        }
    }       

}



static
void WhiteBlackCompensation(_LPcmsTRANSFORM p)
{

       WORD *WhitePointIn, *WhitePointOut, *BlackPointIn, *BlackPointOut;
       int nOuts, nIns;


       if (!p -> DeviceLink) {

       cmsSignalError(LCMS_ERRC_WARNING,
              "Unable to do Black/White compensation, DeviceLink LUT is missing.");
       return;
       }



       if (!_cmsEndPointsBySpace(p ->EntryColorSpace,
                                   &WhitePointIn, &BlackPointIn, &nIns))
       {
              cmsSignalError(LCMS_ERRC_WARNING,
                     "Unable to do Black/White compensation, unsupported input space.");
              return;
       }


       if (!_cmsEndPointsBySpace(cmsGetColorSpace(p -> OutputProfile),
                                   &WhitePointOut, &BlackPointOut, &nOuts))
       {
              cmsSignalError(LCMS_ERRC_WARNING,
                     "Unable to do Black/White compensation, unsupported output space.");
              return;
       }



       PatchLUT(p -> DeviceLink, WhitePointIn, WhitePointOut, nOuts, nIns);
       PatchLUT(p -> DeviceLink, BlackPointIn, BlackPointOut, nOuts, nIns);
       PatchCurves(p -> DeviceLink);

}



// Check colorspace

static
BOOL IsProperColorSpace(cmsHPROFILE hProfile, DWORD dwFormat)
{
       int Space = T_COLORSPACE(dwFormat);

       if (Space == PT_ANY) return TRUE;

       return (_cmsICCcolorSpace(Space) == cmsGetColorSpace(hProfile));
}


// Auxiliary: allocate transform struct and set to defaults

static
_LPcmsTRANSFORM AllocEmptyTransform(void)
{
    // Allocate needed memory

    _LPcmsTRANSFORM p = (_LPcmsTRANSFORM) malloc(sizeof(_cmsTRANSFORM));
    if (!p) {

          cmsSignalError(LCMS_ERRC_ABORTED, "cmsCreateTransform: malloc() failed");
          return NULL;
    }

    ZeroMemory(p, sizeof(_cmsTRANSFORM));

    // Initialize default methods

    p -> xform          = NULL;
    p -> Intent         = INTENT_PERCEPTUAL;
    p -> ProofIntent    = INTENT_ABSOLUTE_COLORIMETRIC;
    p -> DoGamutCheck   = FALSE;
    p -> InputProfile   = NULL;
    p -> OutputProfile  = NULL;
    p -> PreviewProfile = NULL;
    p -> Preview        = NULL;
    p -> Gamut          = NULL;
    p -> DeviceLink     = NULL;
    p -> InMatShaper    = NULL;
    p -> OutMatShaper   = NULL;
    p -> SmeltMatShaper = NULL;
    p -> NamedColorList = NULL;
    p -> EntryColorSpace = 0;
    p -> ExitColorSpace  = 0;

    return p;
}


// Transform is identified as device-link
static 
cmsHPROFILE CreateDeviceLinkTransform(_LPcmsTRANSFORM p)
{
    
    if (!IsProperColorSpace(p->InputProfile, p->InputFormat)) {
        cmsSignalError(LCMS_ERRC_WARNING, "Device link is operating on wrong colorspace");
    }

    // Device link does only have AToB0Tag (ICC-Spec 1998/09)

    p->DeviceLink = cmsReadICCLut(p->InputProfile, icSigAToB0Tag);

    if (!p->DeviceLink) {

         cmsSignalError(LCMS_ERRC_ABORTED, "Noncompliant device-link profile");
         cmsDeleteTransform((cmsHTRANSFORM) p);
         return NULL;
         }

    if (p ->PreviewProfile != NULL) {
            cmsSignalError(LCMS_ERRC_WARNING, "Proofing not supported on device link transforms");
    }

    if (p ->OutputProfile != NULL) {
            cmsSignalError(LCMS_ERRC_WARNING, "Output profile should be NULL, since this is a device-link transform");
    }

    p -> Phase1 = -1;
    p -> Phase2 = -1;
    p -> Phase3 = -1;
    p -> xform = PrecalculatedXFORM;
    p -> ExitColorSpace = cmsGetPCS(p -> InputProfile);

    // Precalculated device-link profile is ready
    return (cmsHTRANSFORM) p;
}


// Transform that includes proofing
static
void CreateProof(_LPcmsTRANSFORM p,                                        
                        DWORD dwFlags,
                        icTagSignature *ToTagPtr)

{
    icTagSignature ProofTag;
   
    if (dwFlags & cmsFLAGS_SOFTPROOFING) {

      // Apr-15, 2002 - Too much profiles does have bogus content
      // on preview tag, so I do compute it by my own.

      p -> Preview = _cmsComputeSoftProofLUT(p ->PreviewProfile, p ->ProofIntent); 
      p -> Phase2  = LabRel;

      if (p -> Preview == NULL) {
          
        ProofTag = Preview[p -> Intent];

        if (!cmsIsTag(p ->PreviewProfile,  ProofTag)) {

            ProofTag = Preview[0];
            if (!cmsIsTag(p ->PreviewProfile,  ProofTag))
                            ProofTag = (icTagSignature)0;
        }

        if (ProofTag) {

             p -> Preview = cmsReadICCLut(p ->PreviewProfile, ProofTag);
             p -> Phase2 = GetPhase(p ->PreviewProfile);
             *ToTagPtr  = PCS2Device[p->ProofIntent];
             
        }
        else
             {
             p -> Preview = NULL;
             p ->PreviewProfile = NULL;
             cmsSignalError(LCMS_ERRC_WARNING, "Sorry, the proof profile has not previewing capabilities");
             }
      }
                
    }


    // Aug-31, 2001 - Too much profiles does have bogus content
    // on gamut tag, so I do compute it by my own.

    if (dwFlags & cmsFLAGS_GAMUTCHECK) {

                   
             p -> Gamut = _cmsComputeGamutLUT(p->PreviewProfile, p ->ProofIntent);                               
             if (p -> Gamut == NULL) {

                // Profile goes only in one direction... try to see 
                // if profile has the tag, and use it, no matter it
                // could be bogus. This is the last chance!

                if (cmsIsTag(p ->PreviewProfile, icSigGamutTag)) {

                    p -> Gamut = cmsReadICCLut(p ->PreviewProfile, icSigGamutTag);
                    }
                    else   {
                            
                     // Nope, cannot be done.

                     cmsSignalError(LCMS_ERRC_WARNING, "Sorry, the proof profile has not gamut checking capabilities");
                     p -> Gamut = NULL;
                    }
             }

      }

}

// Choose the adequate transform routine

static
_LPcmsTRANSFORM PickTransformRoutine(_LPcmsTRANSFORM p,                       
                                    LPDWORD dwFlagsPtr,
                                    icTagSignature *FromTagPtr,
                                    icTagSignature *ToTagPtr)
{

        // Can we optimize matrix-shaper only transform?

       if (*FromTagPtr == 0 && 
           *ToTagPtr == 0 && 
           !p->PreviewProfile  && 
           p -> Intent != INTENT_ABSOLUTE_COLORIMETRIC && 
           (p -> EntryColorSpace == icSigRgbData) && 
           (p -> ExitColorSpace == icSigRgbData)) {

              // Yes... try to smelt matrix-shapers
              p -> xform = MatrixShaperXFORM;
              *dwFlagsPtr |= cmsFLAGS_NOTPRECALC;

              if (!cmsBuildSmeltMatShaper(p))
              {
                     cmsSignalError(LCMS_ERRC_ABORTED, "unable to smelt shaper-matrix, required tags missing");               
                     return NULL;
              }

              p -> Phase1 = p -> Phase3 = XYZRel;
              return p;

       }
       

        // Is a named color profile?
        if (cmsGetDeviceClass(p->InputProfile) == icSigNamedColorClass) {

                  // Yes, and used as input
                  p ->FromDevice = NC2toPCS;
        }
        else {

                // No, is a transform involving LUT

                if (*FromTagPtr != 0) {

                     p -> FromDevice = LUTtoPCS;
                     p -> Device2PCS = cmsReadICCLut(p -> InputProfile, *FromTagPtr);
                     if (!p -> Device2PCS) {

                            cmsSignalError(LCMS_ERRC_ABORTED, "profile is unsuitable for input");                            
                            return NULL;
                            }

              }
              else
              {
                     p -> FromDevice = ShaperMatrixToPCS;
                     p -> InMatShaper = cmsBuildInputMatrixShaper(p -> InputProfile, dwFlagsPtr);

                     if (!p ->InMatShaper) {
                            cmsSignalError(LCMS_ERRC_ABORTED, "profile is unsuitable for input");                            
                            return NULL;
                            }

                     p -> Phase1 = XYZRel;

              }
              }

              if (*ToTagPtr != 0) {

                     p -> ToDevice = PCStoLUT;
                     p -> PCS2Device = cmsReadICCLut(p -> OutputProfile, *ToTagPtr);
                     if (!p -> PCS2Device) {
                            cmsSignalError(LCMS_ERRC_ABORTED, "profile is unsuitable for output");                            
                            return NULL;
                            }

                     }
              else
              {
                     p -> ToDevice = PCStoShaperMatrix;
                     p -> OutMatShaper = cmsBuildOutputMatrixShaper(p->OutputProfile, dwFlagsPtr);

                     if (!p -> OutMatShaper) {
                            cmsSignalError(LCMS_ERRC_ABORTED, "profile is unsuitable for output");                            
                            return NULL;
                            }
                     p -> Phase3 = XYZRel;
                     
              }
       

       return p;
}




// Create a transform.

cmsHTRANSFORM LCMSEXPORT cmsCreateProofingTransform(cmsHPROFILE InputProfile,
                                               DWORD InputFormat,
                                               cmsHPROFILE OutputProfile,
                                               DWORD OutputFormat,
                                               cmsHPROFILE ProofingProfile,
                                               int nIntent,
                                               int ProofingIntent,
                                               DWORD dwFlags)

{
       _LPcmsTRANSFORM p;
       icTagSignature FromTag;
       icTagSignature ToTag;
       
       if (nIntent < 0 || nIntent > 3 ||
           ProofingIntent < 0 || ProofingIntent > 3)
       {
       cmsSignalError(LCMS_ERRC_ABORTED, "cmsCreateTransform: intent mismatch");
       return NULL;
       }

       p = AllocEmptyTransform();
       if (p == NULL) return NULL;

       p -> xform          = NormalXFORM;
       p -> Intent         = nIntent;
       p -> ProofIntent    = ProofingIntent;
       p -> DoGamutCheck   = FALSE;
       p -> InputProfile   = InputProfile;
       p -> OutputProfile  = OutputProfile;
       p -> PreviewProfile = ProofingProfile;            
       p -> InputFormat    = InputFormat;
       p -> OutputFormat   = OutputFormat;

       p -> FromInput = _cmsIdentifyInputFormat(p, InputFormat);
       p -> ToOutput  = _cmsIdentifyOutputFormat(p, OutputFormat);

       // Null transform can be done without profiles
       if ((dwFlags & cmsFLAGS_NULLTRANSFORM) ||
                        ((InputProfile == NULL) &&
                         (OutputProfile == NULL))) {

            p -> xform = NullXFORM;
            return (cmsHTRANSFORM) p;
       }

       // From here we need at least one input profile
       if (InputProfile == NULL) {
          
          cmsSignalError(LCMS_ERRC_ABORTED, "Input profile cannot be NULL!");
          cmsDeleteTransform((cmsHTRANSFORM) p);
          return NULL;
       }


       //  Device link are means to store precalculated transform grids.
       if (cmsGetDeviceClass(InputProfile) == icSigLinkClass) {

            return CreateDeviceLinkTransform(p);
       }

     
     
       if (!IsProperColorSpace(InputProfile, InputFormat)) {
              cmsSignalError(LCMS_ERRC_WARNING, "Input profile is operating on wrong colorspace");
       }

       p ->EntryColorSpace = cmsGetColorSpace(InputProfile);

       // Oct-21-2002: Added named color transforms
       if (cmsGetDeviceClass(InputProfile) == icSigNamedColorClass) {

        if (p ->NamedColorList == NULL)
                p ->NamedColorList = cmsAllocNamedColorList();

        cmsReadICCnamedColorList(p, InputProfile, icSigNamedColor2Tag);
        
        // Special case. If output profile == NULL, then the transform gives
        // device values from named colors.

        if (OutputProfile == NULL) {

            p ->ExitColorSpace = p -> EntryColorSpace;
            p ->xform = NC2deviceXform;
            return (cmsHTRANSFORM) p;
        }


        // Named color doesn't precalc anything
        dwFlags |= cmsFLAGS_NOTPRECALC;
       }

    
       // From here we need also output profile.
      if (OutputProfile == NULL) {          
          cmsSignalError(LCMS_ERRC_ABORTED, "Output profile cannot be NULL!");
          cmsDeleteTransform((cmsHTRANSFORM) p);
          return NULL;
       }

            
       if (!IsProperColorSpace(OutputProfile, OutputFormat)) {
              cmsSignalError(LCMS_ERRC_WARNING, "Output profile is operating on wrong colorspace");
       }

       p -> ExitColorSpace = cmsGetColorSpace(OutputProfile);

       // Named color only on input
       if (cmsGetDeviceClass(OutputProfile) == icSigNamedColorClass) {

           cmsSignalError(LCMS_ERRC_ABORTED, "Named color profiles are not supported as output");
           cmsDeleteTransform((cmsHTRANSFORM) p);
           return NULL;
       }

       p -> Phase1 = GetPhase(InputProfile);
       p -> Phase2 = -1;
       p -> Phase3 = GetPhase(OutputProfile);

       // Try to locate a LUT

       FromTag  = Device2PCS[nIntent];
       ToTag    = PCS2Device[nIntent];

       if (!cmsIsTag(InputProfile, FromTag))
       {
              FromTag = Device2PCS[0];

              if (!cmsIsTag(InputProfile,  FromTag)) {
                            FromTag = (icTagSignature)0;
              }
       }

       // If proofing is needed, add required tags/parameters
       if (ProofingProfile) 
           CreateProof(p, dwFlags, &ToTag);                       
       


       if (!cmsIsTag(OutputProfile,  ToTag)) {
              ToTag = PCS2Device[0];
              if (!cmsIsTag(OutputProfile,  ToTag))
                            ToTag = (icTagSignature)0;
       }


       if (dwFlags& cmsFLAGS_MATRIXINPUT)
              FromTag = (icTagSignature)0;

       if (dwFlags & cmsFLAGS_MATRIXOUTPUT)
              ToTag = (icTagSignature)0;

       if (dwFlags & cmsFLAGS_GAMUTCHECK)
              p -> DoGamutCheck = TRUE;

     
       if (PickTransformRoutine(p, &dwFlags, &FromTag, &ToTag) == NULL) {

          cmsDeleteTransform((cmsHTRANSFORM) p);
          return NULL;

       }

       TakeConversionRoutines(p);

       if (dwFlags & cmsFLAGS_WHITEBLACKCOMPENSATION)
                            dwFlags &= ~cmsFLAGS_NOTPRECALC;

       if (!(dwFlags & cmsFLAGS_NOTPRECALC)) {

               LPLUT DeviceLink;    

               DeviceLink = _cmsPrecalculateDeviceLink((cmsHTRANSFORM) p, dwFlags);
                                        
                if (DeviceLink) {

                    p ->DeviceLink = DeviceLink;
                }
                else
                {

                     cmsSignalError(LCMS_ERRC_ABORTED,
                                "Cannot precalculate %d->%d channels transform!",
                                T_CHANNELS(InputFormat), T_CHANNELS(OutputFormat));

                     cmsDeleteTransform(p);
                     return NULL;
                }


              p -> xform = PrecalculatedXFORM;

       }

       // Re-Identify formats
       p -> FromInput = _cmsIdentifyInputFormat(p, InputFormat);
       p -> ToOutput  = _cmsIdentifyOutputFormat(p, OutputFormat);

       // Patch devicelink LUT
       if (dwFlags & cmsFLAGS_WHITEBLACKCOMPENSATION)
                                   WhiteBlackCompensation(p);

       return p;
}


// Wrapper por simpler non-proofing transforms.

cmsHTRANSFORM LCMSEXPORT cmsCreateTransform(cmsHPROFILE Input,
                                       DWORD InputFormat,
                                       cmsHPROFILE Output,
                                       DWORD OutputFormat,
                                       int Intent,
                                       DWORD dwFlags)

{
       return cmsCreateProofingTransform(Input, InputFormat,
                                         Output, OutputFormat,
                                         NULL,
                                         Intent, INTENT_ABSOLUTE_COLORIMETRIC,
                                         dwFlags);
}


// Profiles are *NOT* closed

void LCMSEXPORT cmsDeleteTransform(cmsHTRANSFORM hTransform)
{
       _LPcmsTRANSFORM p = (_LPcmsTRANSFORM) (LPSTR) hTransform;

       if (p -> Device2PCS)
              cmsFreeLUT(p -> Device2PCS);
       if (p -> PCS2Device)
              cmsFreeLUT(p -> PCS2Device);
       if (p -> Gamut)
              cmsFreeLUT(p -> Gamut);
       if (p -> Preview)
              cmsFreeLUT(p -> Preview);
       if (p -> DeviceLink)
              cmsFreeLUT(p -> DeviceLink);
       if (p -> InMatShaper)
              cmsFreeMatShaper(p -> InMatShaper);
       if (p -> OutMatShaper)
              cmsFreeMatShaper(p -> OutMatShaper);
       if (p -> SmeltMatShaper)
              cmsFreeMatShaper(p -> SmeltMatShaper);
       if (p ->NamedColorList)
              cmsFreeNamedColorList(p ->NamedColorList);

       free((void *) p);
}


// Apply transform code
void LCMSEXPORT cmsDoTransform(cmsHTRANSFORM Transform,
                    LPVOID InputBuffer,
                    LPVOID OutputBuffer, unsigned int Size)

{

            _LPcmsTRANSFORM p = (_LPcmsTRANSFORM) (LPSTR) Transform;

            p -> StrideIn = p -> StrideOut = Size;
            
            p -> xform(p, InputBuffer, OutputBuffer, Size);

}


void LCMSEXPORT cmsSetAlarmCodes(int r, int g, int b)
{
       AlarmR = RGB_8_TO_16(r);
       AlarmG = RGB_8_TO_16(g);
       AlarmB = RGB_8_TO_16(b);
}

void LCMSEXPORT cmsGetAlarmCodes(int *r, int *g, int *b)
{
       *r = RGB_16_TO_8(AlarmR);
       *g = RGB_16_TO_8(AlarmG);
       *b = RGB_16_TO_8(AlarmB);
}

BOOL LCMSEXPORT cmsIsIntentSupported(cmsHPROFILE hProfile,
                                                int Intent, int UsedDirection)
{

     icTagSignature* TagTable;

     // Device link profiles only implements the intent in header

     if (cmsGetDeviceClass(hProfile) != icSigLinkClass) {

       switch (UsedDirection) {

       case LCMS_USED_AS_INPUT: TagTable = Device2PCS; break;
       case LCMS_USED_AS_OUTPUT:TagTable = PCS2Device; break;
       case LCMS_USED_AS_PROOF: TagTable = Preview; break;

       default:
        cmsSignalError(LCMS_ERRC_WARNING, "Unexpected direction (%d)", UsedDirection);
        return FALSE;
       }

       if (cmsIsTag(hProfile, TagTable[Intent])) return TRUE;
     }

     return (cmsTakeRenderingIntent(hProfile) == Intent);
}

// Multiple profile transform. 
static
int MultiprofileSampler(register WORD In[], register WORD Out[], register LPVOID Cargo)
{
    cmsHTRANSFORM* Transforms = (cmsHTRANSFORM*) Cargo;
    int i;
    
    cmsDoTransform(Transforms[0], In, Out, 1);

    for (i=1; Transforms[i]; i++)
        cmsDoTransform(Transforms[i], Out, Out, 1);


    
    return TRUE;
}


// A multiprofile transform does chain several profiles into a single
// devicelink. It couls also be used to merge named color profiles into
// a single database.

cmsHTRANSFORM LCMSEXPORT cmsCreateMultiprofileTransform(cmsHPROFILE hProfiles[],
                                                                int nProfiles,
                                                                DWORD InputFormat,
                                                                DWORD OutputFormat,
                                                                int Intent,
                                                                DWORD dwFlags)
{
    cmsHTRANSFORM Transforms[257];
    DWORD dwPrecalcFlags = (dwFlags|cmsFLAGS_NOTPRECALC);
    DWORD RawFormat, RawIdentity;
    cmsHPROFILE hLab, hXYZ, hProfile;   
    icColorSpaceSignature FirstColorSpace;
    icColorSpaceSignature ColorSpace;   
    LPLUT Grid;
    int nGridPoints, ChannelsIn, ChannelsOut, i;    
    _LPcmsTRANSFORM p;
    
    icProfileClassSignature Class;
    int nNamedColor;

    if (nProfiles > 255) {
        cmsSignalError(LCMS_ERRC_ABORTED, "What are you trying to do with more that 255 profiles?!?, of course aborted");
        return NULL;
    }

    // Creates a phantom transform for latter filling
    p = cmsCreateTransform(NULL, InputFormat, NULL, OutputFormat, Intent, cmsFLAGS_NULLTRANSFORM);

    // If user wants null one, give it
    if (dwFlags & cmsFLAGS_NULLTRANSFORM) return (cmsHPROFILE) p;

    // Is a bunch of named color profiles? 
    nNamedColor = 0;
    for (i=0; i < nProfiles; i++) {
        if (cmsGetDeviceClass(hProfiles[i]) == icSigNamedColorClass)
                nNamedColor++;
    }


    if (nNamedColor == nProfiles) {

            // Yes, only named color. Create a named color-device
            // and append to named color table

            p = cmsCreateTransform(hProfiles[0], InputFormat, NULL, OutputFormat, Intent, dwFlags);
            for (i=1; i < nProfiles; i++) {
                    cmsReadICCnamedColorList(p, hProfiles[i], icSigNamedColor2Tag);
            }

            return p;   // Ok, done so far
    }

    // We will need a 3DCLUT for device link
    Grid =  cmsAllocLUT();
    if (!Grid) return NULL;

    // This one is our PCS (Always Lab)
    hLab  = cmsCreateLabProfile(NULL);
    hXYZ  = cmsCreateXYZProfile();

    if (!hLab || !hXYZ) goto ErrorCleanup;

    // Take some info....
    p ->EntryColorSpace = FirstColorSpace = cmsGetColorSpace(hProfiles[0]);
    ChannelsIn     = _cmsChannelsOf(FirstColorSpace);
    ChannelsOut    = 3; // Will be updated on loop

    for (i=0; i < nProfiles; i++) {

        // Check colorspace, create transforms
        hProfile    = hProfiles[i];
        ColorSpace  = cmsGetColorSpace(hProfile);
        ChannelsOut = _cmsChannelsOf(ColorSpace);

        // Format: WORDs of current # of channels
        RawFormat    = BYTES_SH(2)|CHANNELS_SH(ChannelsOut);
        RawIdentity  = BYTES_SH(2)|CHANNELS_SH(3);

        if (ColorSpace == FirstColorSpace) {

            FirstColorSpace = cmsGetPCS(hProfile);
            Class           = cmsGetDeviceClass(hProfile);

            if (Class == icSigLinkClass) {

                    Transforms[i]  = cmsCreateTransform(hProfile, RawFormat, 
                                    NULL, RawIdentity, Intent, dwPrecalcFlags);         
            }

            else {

             Transforms[i]  = cmsCreateTransform(hProfile, RawFormat, 
                                    (FirstColorSpace == icSigLabData ? hLab : hXYZ),
                                    RawIdentity, Intent, dwPrecalcFlags);           
            }
        }
        else  // Can come from pcs?
        if (FirstColorSpace == icSigXYZData) {

            Transforms[i] = cmsCreateTransform(hXYZ, RawIdentity, hProfile, RawFormat, Intent, dwPrecalcFlags);
            FirstColorSpace = ColorSpace;
            
        }
        else
        if (FirstColorSpace == icSigLabData) {

            Transforms[i] = cmsCreateTransform(hLab, RawIdentity, hProfile, RawFormat, Intent, dwPrecalcFlags);
            FirstColorSpace = ColorSpace;
        } 
        else {
                cmsSignalError(LCMS_ERRC_ABORTED, "cmsCreateMultiprofileTransform: ColorSpace mismatch");
                goto ErrorCleanup;
        }

    }

    p ->ExitColorSpace = FirstColorSpace;
    Transforms[i] = NULL;   // End marker 

    p ->InputProfile  = hProfiles[0];
    p ->OutputProfile = hProfiles[nProfiles - 1];



    nGridPoints = _cmsReasonableGridpointsByColorspace(p ->EntryColorSpace, dwFlags);
   
    Grid = cmsAlloc3DGrid(Grid, nGridPoints, ChannelsIn, ChannelsOut);

    if ((p ->EntryColorSpace == icSigRgbData) && (p->ExitColorSpace == icSigRgbData))
                    _cmsComputePrelinearizationTablesFromXFORM(Transforms, nProfiles, Grid);
      
    // Compute device link on 16-bit basis                
    if (!cmsSample3DGrid(Grid, MultiprofileSampler, (LPVOID) Transforms, Grid -> wFlags)) {

                cmsFreeLUT(Grid);
                goto ErrorCleanup;
    }

    // All ok, store the newly created LUT   
    p -> DeviceLink   = Grid;
    p -> xform        = PrecalculatedXFORM;


    for (i=nProfiles-1; i >= 0; --i)
        cmsDeleteTransform(Transforms[i]);


    if (hLab) cmsCloseProfile(hLab);
    if (hXYZ) cmsCloseProfile(hXYZ);
    return (cmsHTRANSFORM) p;


ErrorCleanup:

    if (hLab) cmsCloseProfile(hLab);
    if (hXYZ) cmsCloseProfile(hXYZ);
    return NULL;
}
