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

#include "lcms.h"

//  Pipeline of LUT. Enclosed by {} are new revision 4.0 of ICC spec.
//
//  [Mat] -> [L1] -> { [Mat3] -> [Ofs3] -> [L3] ->} [CLUT] { -> [L4] -> [Mat4] -> [Ofs4] } -> [L2]
//
//  Some of these stages would be missing. This implements the totality of 
//  combinations of old and new LUT types as follows:
//
//  Lut8 & Lut16
//  ============
//     [Mat] -> [L1] -> [CLUT] -> [L2]
//
//  Mat2, Ofs2, L3, L3, Mat3, Ofs3 are missing
//
//  LutAToB
//  ========
//
//  [L1] -> [CLUT] -> [L4] -> [Mat4] -> [Ofs4] -> [L2]
//
//  Mat, Mat3, Ofs3, L3 are missing
//   L1 = A curves
//   L4 = M curves
//   L2 = B curves
//
//  LutBToA 
//  =======
// 
//  [L1] -> [Mat3] -> [Ofs3] -> [L3] -> [CLUT] -> [L2]
//
//  Mat, L4, Mat4, Ofs4 are missing
//   L1 = B Curves
//   L3 = M Curves
//   L2 = A curves      
//  
//
//  V2&3 emulation
//  ===============
//
//  For output, Mat is multiplied by
//  
//
//  | 0xff00 / 0xffff      0                    0           | 
//  |        0          0xff00 / 0xffff         0           | 
//  |        0             0                0xff00 / 0xffff | 
//
//
//  For input, an additional matrix is needed at the very last end of the chain 
//  
//
//  | 0xffff / 0xff00      0                     0        | 
//  |        0          0xffff / 0xff00          0        | 
//  |        0             0              0xffff / 0xff00 | 
//
//
//  Which reduces to (val * 257) >> 8

// A couple of macros to convert between revisions

#define FROM_V2_TO_V4(x) (((((x)<<8)+(x))+0x80)>>8)    // BY 65535 DIV 65280 ROUND
#define FROM_V4_TO_V2(x) ((((x)<<8)+0x80)/257)         // BY 65280 DIV 65535 ROUND


// Lut Creation & Destruction

LPLUT LCMSEXPORT cmsAllocLUT(void)
{
       LPLUT NewLUT;

       NewLUT = (LPLUT) malloc(sizeof(LUT));
       if (NewLUT)
              ZeroMemory(NewLUT, sizeof(LUT));

       return NewLUT;
}

void LCMSEXPORT cmsFreeLUT(LPLUT Lut)
{
       unsigned int i;

       if (!Lut) return;

       if (Lut -> T) free(Lut -> T);

       for (i=0; i < Lut -> OutputChan; i++)
       {
              if (Lut -> L2[i]) free(Lut -> L2[i]);
       }

       for (i=0; i < Lut -> InputChan; i++)
       {

              if (Lut -> L1[i]) free(Lut -> L1[i]);
       }


       if (Lut ->wFlags & LUT_HASTL3) {

            for (i=0; i < Lut -> InputChan; i++) {

              if (Lut -> L3[i]) free(Lut -> L3[i]);
            }
       }

       if (Lut ->wFlags & LUT_HASTL4) {

            for (i=0; i < Lut -> OutputChan; i++) {

              if (Lut -> L4[i]) free(Lut -> L4[i]);
            }
       }

       if (Lut ->CLut16params.p8)
           free(Lut ->CLut16params.p8);

       free(Lut);
}


static
LPVOID DupBlockTab(LPVOID Org, size_t size)
{
    LPVOID mem = malloc(size);

    CopyMemory(mem, Org, size);
    return mem;
}


LPLUT LCMSEXPORT cmsDupLUT(LPLUT Orig)
{
    LPLUT NewLUT = cmsAllocLUT();
    unsigned int i;
    
       CopyMemory(NewLUT, Orig, sizeof(LUT));

       for (i=0; i < Orig ->InputChan; i++) 
            NewLUT -> L1[i] = (LPWORD) DupBlockTab((LPVOID) Orig ->L1[i], 
                                        sizeof(WORD) * Orig ->In16params.nSamples);

       for (i=0; i < Orig ->OutputChan; i++)
            NewLUT -> L2[i] = (LPWORD) DupBlockTab((LPVOID) Orig ->L2[i], 
                                        sizeof(WORD) * Orig ->Out16params.nSamples);   
       
       NewLUT -> T = (LPWORD) DupBlockTab((LPVOID) Orig ->T, Orig -> Tsize);

       return NewLUT;
}


static
unsigned int UIpow(unsigned int a, unsigned int b)
{
        unsigned int rv = 1;

        for (; b > 0; b--)
                rv *= a;

        return rv;
}


LPLUT LCMSEXPORT cmsAlloc3DGrid(LPLUT NewLUT, int clutPoints, int inputChan, int outputChan)
{
    DWORD nTabSize;

       NewLUT -> wFlags       |= LUT_HAS3DGRID;  
       NewLUT -> cLutPoints    = clutPoints;
       NewLUT -> InputChan     = inputChan;
       NewLUT -> OutputChan    = outputChan;


       nTabSize = (NewLUT -> OutputChan * UIpow(NewLUT->cLutPoints,
                                                NewLUT->InputChan)
                                                * sizeof(WORD));

       NewLUT -> T = (LPWORD) malloc(nTabSize);
       ZeroMemory(NewLUT -> T, nTabSize);
       NewLUT ->Tsize = nTabSize;
       

       cmsCalcCLUT16Params(NewLUT -> cLutPoints,  NewLUT -> InputChan,
                                                  NewLUT -> OutputChan,
                                                  &NewLUT -> CLut16params);

       return NewLUT;
}




LPLUT LCMSEXPORT cmsAllocLinearTable(LPLUT NewLUT, LPGAMMATABLE Tables[], int nTable)
{
       unsigned int i;
       LPWORD PtrW;

       switch (nTable) {


       case 1: NewLUT -> wFlags |= LUT_HASTL1;
               cmsCalcL16Params(Tables[0] -> nEntries, &NewLUT -> In16params);
               NewLUT -> InputEntries = Tables[0] -> nEntries;

               for (i=0; i < NewLUT -> InputChan; i++) {

                     PtrW = (LPWORD) malloc(sizeof(WORD) * NewLUT -> InputEntries);
                     NewLUT -> L1[i] = PtrW;
                     CopyMemory(PtrW, Tables[i]->GammaTable, sizeof(WORD) * NewLUT -> InputEntries);
					 CopyMemory(&NewLUT -> LCurvesBirth[0][i], &Tables[i] -> Birth, sizeof(LCMSGAMMAPARAMS));
               }
			   

               break;

       case 2: NewLUT -> wFlags |= LUT_HASTL2;
               cmsCalcL16Params(Tables[0] -> nEntries, &NewLUT -> Out16params);
               NewLUT -> OutputEntries = Tables[0] -> nEntries;
               for (i=0; i < NewLUT -> OutputChan; i++) {

                     PtrW = (LPWORD) malloc(sizeof(WORD) * NewLUT -> OutputEntries);
                     NewLUT -> L2[i] = PtrW;
                     CopyMemory(PtrW, Tables[i]->GammaTable, sizeof(WORD) * NewLUT -> OutputEntries);
					 CopyMemory(&NewLUT -> LCurvesBirth[1][i], &Tables[i] -> Birth, sizeof(LCMSGAMMAPARAMS));
               }
               break;


       // 3 & 4 according ICC 4.0 spec

       case 3:
               NewLUT -> wFlags |= LUT_HASTL3;
               cmsCalcL16Params(Tables[0] -> nEntries, &NewLUT -> L3params);
               NewLUT -> L3Entries = Tables[0] -> nEntries;

               for (i=0; i < NewLUT -> InputChan; i++) {

                     PtrW = (LPWORD) malloc(sizeof(WORD) * NewLUT -> L3Entries);
                     NewLUT -> L3[i] = PtrW;
                     CopyMemory(PtrW, Tables[i]->GammaTable, sizeof(WORD) * NewLUT -> L3Entries);
					 CopyMemory(&NewLUT -> LCurvesBirth[2][i], &Tables[i] -> Birth, sizeof(LCMSGAMMAPARAMS));
               }
               break;

       case 4:
               NewLUT -> wFlags |= LUT_HASTL4;
               cmsCalcL16Params(Tables[0] -> nEntries, &NewLUT -> L4params);
               NewLUT -> L4Entries = Tables[0] -> nEntries;
               for (i=0; i < NewLUT -> OutputChan; i++) {

                     PtrW = (LPWORD) malloc(sizeof(WORD) * NewLUT -> L4Entries);
                     NewLUT -> L4[i] = PtrW;
                     CopyMemory(PtrW, Tables[i]->GammaTable, sizeof(WORD) * NewLUT -> L4Entries);
					 CopyMemory(&NewLUT -> LCurvesBirth[3][i], &Tables[i] -> Birth, sizeof(LCMSGAMMAPARAMS));
               }
               break;
               

       default:;
       }

       return NewLUT;
}


// Set the LUT matrix

LPLUT LCMSEXPORT cmsSetMatrixLUT(LPLUT Lut, LPMAT3 M)
{
        MAT3toFix(&Lut ->Matrix, M);

        if (!MAT3isIdentity(&Lut->Matrix, 0.0001))
            Lut ->wFlags |= LUT_HASMATRIX;

        return Lut;
}


// Set matrix & offset, v4 compatible

LPLUT LCMSEXPORT cmsSetMatrixLUT4(LPLUT Lut, LPMAT3 M, LPVEC3 off, DWORD dwFlags)
{
    WMAT3 WMat;
    WVEC3 Woff;
    VEC3  Zero = {{0, 0, 0}};

        MAT3toFix(&WMat, M);

        if (off == NULL)
                off = &Zero;

        VEC3toFix(&Woff, off);

        // Nop if identity
        if (MAT3isIdentity(&WMat, 0.0001) && 
            (Woff.n[VX] == 0 && Woff.n[VY] == 0 && Woff.n[VZ] == 0))
            return Lut;

        switch (dwFlags) {

        case LUT_HASMATRIX:
                Lut ->Matrix = WMat;                
                Lut ->wFlags |= LUT_HASMATRIX;
                break;

        case LUT_HASMATRIX3:
                Lut ->Mat3 = WMat;
                Lut ->Ofs3 = Woff;
                Lut ->wFlags |= LUT_HASMATRIX3;
                break;

        case LUT_HASMATRIX4:
                Lut ->Mat4 = WMat;
                Lut ->Ofs4 = Woff;
                Lut ->wFlags |= LUT_HASMATRIX4;
                break;


        default:;
        }

        return Lut;
}



// The full evaluator

void LCMSEXPORT cmsEvalLUT(LPLUT Lut, WORD In[], WORD Out[])
{
       register unsigned int i;
       WORD StageABC[MAXCHANNELS], StageLMN[MAXCHANNELS];

        
       // Try to speedup things on plain devicelinks       
       if (Lut ->wFlags == LUT_HAS3DGRID) {

            Lut ->CLut16params.Interp3D(In, Out, Lut -> T, &Lut -> CLut16params);
            return;
       }
       

       // Nope, evaluate whole LUT

       for (i=0; i < Lut -> InputChan; i++)
                            StageABC[i] = In[i];

       
       if (Lut ->wFlags & LUT_V4_OUTPUT_EMULATE_V2) {
           
           // Clamp Lab to avoid overflow
           if (StageABC[0] > 0xFF00)
               StageABC[0] = 0xFF00;

           StageABC[0] = (WORD) FROM_V2_TO_V4(StageABC[0]);
           StageABC[1] = (WORD) FROM_V2_TO_V4(StageABC[1]);
           StageABC[2] = (WORD) FROM_V2_TO_V4(StageABC[2]);
           
       }

       if (Lut ->wFlags & LUT_V2_OUTPUT_EMULATE_V4) {
           
           StageABC[0] = (WORD) FROM_V4_TO_V2(StageABC[0]);
           StageABC[1] = (WORD) FROM_V4_TO_V2(StageABC[1]);
           StageABC[2] = (WORD) FROM_V4_TO_V2(StageABC[2]);           
       }


       // Matrix handling. 

       if (Lut -> wFlags & LUT_HASMATRIX) {

              WVEC3 InVect, OutVect;
              
              // In LUT8 here comes the special gray axis fixup

              if (Lut ->FixGrayAxes) {

                  StageABC[1] = _cmsClampWord(StageABC[1] - 128);
                  StageABC[2] = _cmsClampWord(StageABC[2] - 128);
              }

              // Matrix 

              InVect.n[VX] = ToFixedDomain(StageABC[0]);
              InVect.n[VY] = ToFixedDomain(StageABC[1]);
              InVect.n[VZ] = ToFixedDomain(StageABC[2]);
              

              MAT3evalW(&OutVect, &Lut -> Matrix, &InVect);

              // PCS in 1Fixed15 format, adjusting

              StageABC[0] = _cmsClampWord(FromFixedDomain(OutVect.n[VX]));
              StageABC[1] = _cmsClampWord(FromFixedDomain(OutVect.n[VY]));
              StageABC[2] = _cmsClampWord(FromFixedDomain(OutVect.n[VZ]));
       }
       

       // First linearization

       if (Lut -> wFlags & LUT_HASTL1)
       {
              for (i=0; i < Lut -> InputChan; i++)
                     StageABC[i] = cmsLinearInterpLUT16(StageABC[i],
                                                   Lut -> L1[i],
                                                   &Lut -> In16params);
       }


       //  Mat3, Ofs3, L3 processing
             
       if (Lut ->wFlags & LUT_HASMATRIX3) {

              WVEC3 InVect, OutVect;

              InVect.n[VX] = ToFixedDomain(StageABC[0]);
              InVect.n[VY] = ToFixedDomain(StageABC[1]);
              InVect.n[VZ] = ToFixedDomain(StageABC[2]);

              MAT3evalW(&OutVect, &Lut -> Mat3, &InVect);              

              OutVect.n[VX] += Lut ->Ofs3.n[VX];
              OutVect.n[VY] += Lut ->Ofs3.n[VY];
              OutVect.n[VZ] += Lut ->Ofs3.n[VZ];

              StageABC[0] = _cmsClampWord(FromFixedDomain(OutVect.n[VX]));
              StageABC[1] = _cmsClampWord(FromFixedDomain(OutVect.n[VY]));
              StageABC[2] = _cmsClampWord(FromFixedDomain(OutVect.n[VZ]));

       }
       
       if (Lut ->wFlags & LUT_HASTL3) {

             for (i=0; i < Lut -> InputChan; i++)
                     StageABC[i] = cmsLinearInterpLUT16(StageABC[i],
                                                   Lut -> L3[i],
                                                   &Lut -> L3params);

       }



       if (Lut -> wFlags & LUT_HAS3DGRID) {

            Lut ->CLut16params.Interp3D(StageABC, StageLMN, Lut -> T, &Lut -> CLut16params);

       }
       else
       {              

              for (i=0; i < Lut -> InputChan; i++)
                            StageLMN[i] = StageABC[i];

       }


       // Mat4, Ofs4, L4 processing
     
       if (Lut ->wFlags & LUT_HASTL4) {

            for (i=0; i < Lut -> OutputChan; i++)
                     StageLMN[i] = cmsLinearInterpLUT16(StageLMN[i],
                                                   Lut -> L4[i],
                                                   &Lut -> L4params);
       }
        
       if (Lut ->wFlags & LUT_HASMATRIX4) {

              WVEC3 InVect, OutVect;

              InVect.n[VX] = ToFixedDomain(StageLMN[0]);
              InVect.n[VY] = ToFixedDomain(StageLMN[1]);
              InVect.n[VZ] = ToFixedDomain(StageLMN[2]);

              MAT3evalW(&OutVect, &Lut -> Mat4, &InVect);              

              OutVect.n[VX] += Lut ->Ofs4.n[VX];
              OutVect.n[VY] += Lut ->Ofs4.n[VY];
              OutVect.n[VZ] += Lut ->Ofs4.n[VZ];

              StageLMN[0] = _cmsClampWord(FromFixedDomain(OutVect.n[VX]));
              StageLMN[1] = _cmsClampWord(FromFixedDomain(OutVect.n[VY]));
              StageLMN[2] = _cmsClampWord(FromFixedDomain(OutVect.n[VZ]));

       }

       // Last linearitzation

       if (Lut -> wFlags & LUT_HASTL2)
       {
              for (i=0; i < Lut -> OutputChan; i++)
                     Out[i] = cmsLinearInterpLUT16(StageLMN[i],
                                                   Lut -> L2[i],
                                                   &Lut -> Out16params);
       }
       else
       {
       for (i=0; i < Lut -> OutputChan; i++)
              Out[i] = StageLMN[i];
       }

       

       if (Lut ->wFlags & LUT_V4_INPUT_EMULATE_V2) {
           
           Out[0] = (WORD) FROM_V4_TO_V2(Out[0]);
           Out[1] = (WORD) FROM_V4_TO_V2(Out[1]);
           Out[2] = (WORD) FROM_V4_TO_V2(Out[2]);
           
       }

       if (Lut ->wFlags & LUT_V2_INPUT_EMULATE_V4) {
           
           Out[0] = (WORD) FROM_V2_TO_V4(Out[0]);
           Out[1] = (WORD) FROM_V2_TO_V4(Out[1]);
           Out[2] = (WORD) FROM_V2_TO_V4(Out[2]);           
       }
}


// Precomputes tables for 8-bit on input devicelink. 
// 
LPLUT _cmsBlessLUT8(LPLUT Lut)
{
   int i, j;
   WORD StageABC[3];
   Fixed32 v1, v2, v3;
   LPL8PARAMS p8; 
   LPL16PARAMS p = &Lut ->CLut16params;

  
   p8 = (LPL8PARAMS) malloc(sizeof(L8PARAMS));
   if (p8 == NULL) return NULL;

  // values comes * 257, so we can safely take first byte (x << 8 + x)
  // if there are prelinearization, is already smelted in tables

   for (i=0; i < 256; i++) {

           StageABC[0] = StageABC[1] = StageABC[2] = RGB_8_TO_16(i);

           if (Lut ->wFlags & LUT_HASTL1) {

              for (j=0; j < 3; j++)
                     StageABC[i] = cmsLinearInterpLUT16(StageABC[i],
                                                        Lut -> L1[i],
                                                       &Lut -> In16params);
              Lut ->wFlags &= ~LUT_HASTL1;
           }
    
               
           v1 = ToFixedDomain(StageABC[0] * p -> Domain);
           v2 = ToFixedDomain(StageABC[1] * p -> Domain);
           v3 = ToFixedDomain(StageABC[2] * p -> Domain);

           p8 ->X0[i] = p->opta3 * FIXED_TO_INT(v1);
           p8 ->Y0[i] = p->opta2 * FIXED_TO_INT(v2);
           p8 ->Z0[i] = p->opta1 * FIXED_TO_INT(v3);

           p8 ->rx[i] = (WORD) FIXED_REST_TO_INT(v1);
           p8 ->ry[i] = (WORD) FIXED_REST_TO_INT(v2);
           p8 ->rz[i] = (WORD) FIXED_REST_TO_INT(v3);
  
  }

   Lut -> CLut16params.p8 = p8;
   Lut -> CLut16params.Interp3D = cmsTetrahedralInterp8;

   return Lut;

}




// ----------------------------------------------------------- Reverse interpolation


// Here's how it goes. The derivative Df(x) of the function f is the linear 
// transformation that best approximates f near the point x. It can be represented 
// by a matrix A whose entries are the partial derivatives of the components of f 
// with respect to all the coordinates. This is know as the Jacobian
//
// The best linear approximation to f is given by the matrix equation: 
// 
// y-y0 = A (x-x0) 
// 
// So, if x0 is a good "guess" for the zero of f, then solving for the zero of this 
// linear approximation will give a "better guess" for the zero of f. Thus let y=0, 
// and since y0=f(x0) one can solve the above equation for x. This leads to the 
// Newton's method formula: 
//
// xn+1 = xn - A-1 f(xn) 
// 
// where xn+1 denotes the (n+1)-st guess, obtained from the n-th guess xn in the 
// fashion described above. Iterating this will give better and better approximations 
// if you have a "good enough" initial guess. 


#define JACOBIAN_EPSILON            0.001
#define INVERSION_MAX_ITERATIONS    30


// Evaluates the CLUT part of a LUT (3x3 only)

static
void EvalLUTdoubleK(LPLUT Lut, const VEC3* In, WORD FixedK, LPVEC3 Out)
{
    WORD wIn[4], wOut[3];

    wIn[0] = (WORD) floor(In ->n[0] * 65535.0 + 0.5);
    wIn[1] = (WORD) floor(In ->n[1] * 65535.0 + 0.5);
    wIn[2] = (WORD) floor(In ->n[2] * 65535.0 + 0.5);
    wIn[3] = FixedK;

    cmsEvalLUT(Lut, wIn, wOut);     

    Out ->n[0] = (double) wOut[0] / 65535.0;
    Out ->n[1] = (double) wOut[1] / 65535.0;
    Out ->n[2] = (double) wOut[2] / 65535.0;    
}


// Increment with reflexion on boundary

static 
void IncDelta(double *Val)
{
    if (*Val < (1.0 - JACOBIAN_EPSILON)) 

        *Val += JACOBIAN_EPSILON;
    
    else 
        *Val -= JACOBIAN_EPSILON;
    
}


// Builds a Jacobian CMY->Lab

static
void ComputeJacobian(LPLUT Lut, LPMAT3 Jacobian, const VEC3* Colorant, WORD K)
{
    VEC3 ColorantD;
    double DeltaColorant;
    VEC3 Lab, LabD;
    int  i, j;
            
    EvalLUTdoubleK(Lut, Colorant, K, &Lab);
    

    for (j = 0; j < 3; j++) {

        ColorantD.n[0] = Colorant ->n[0];
        ColorantD.n[1] = Colorant ->n[1];
        ColorantD.n[2] = Colorant ->n[2];
        
        IncDelta(&ColorantD.n[j]);

        EvalLUTdoubleK(Lut, &ColorantD, K, &LabD);

        DeltaColorant = (Colorant->n[j] - ColorantD.n[j]);

        for (i = 0; i < 3; i++) {

            double DeltaTristim  = (Lab.n[i] - LabD.n[i]);
            
            Jacobian->v[i].n[j] = (DeltaTristim / DeltaColorant);
        }
    }
}


static
void ToEncoded(WORD Encoded[3], LPVEC3 Float)
{
    Encoded[0] = (WORD) floor(Float->n[0] * 65535.0 + 0.5);
    Encoded[1] = (WORD) floor(Float->n[1] * 65535.0 + 0.5);
    Encoded[2] = (WORD) floor(Float->n[2] * 65535.0 + 0.5);
}

static
void FromEncoded(LPVEC3 Float, WORD Encoded[3])
{
    Float->n[0] = Encoded[0] / 65535.0;
    Float->n[1] = Encoded[1] / 65535.0;
    Float->n[2] = Encoded[2] / 65535.0;
}


// Evaluate a LUT in reverse direction. It only searches on 3->3 LUT, but It 
// can be used on CMYK -> Lab LUT to obtain black preservation. 
// Target holds LabK in this case


LCMSAPI double LCMSEXPORT cmsEvalLUTreverse(LPLUT Lut, WORD Target[], WORD Result[], LPWORD Hint)
{
    int      i;
    double   error, LastError;
    VEC3     Guess;
    VEC3     Goal, TestPoint, Colorant;
    MAT3     Jacobian, JacobianInverse;
    WORD     FixedK;
    WORD     LastResult[4];
    
        
    // This is our Lab goal
    FromEncoded(&Goal, Target);
    
    // Special case for CMYK->Lab 

    if (Lut ->InputChan == 4)
            FixedK = Target[3];
    else
            FixedK = 0;
        
    
    // Take the hint as starting point if specified

    if (Hint == NULL) {

        // Begin at any point, we choose 1/3 of neutral CMY gray

        Colorant.n[0] = 
        Colorant.n[1] = 
        Colorant.n[2] = 0.3;

    }
    else {
        FromEncoded(&Colorant, Hint);
    }
    
            
    LastError = 1E20;

    // Iterate
    
    for (i = 0; i < INVERSION_MAX_ITERATIONS; i++) {

        // Get beginning guess
        EvalLUTdoubleK(Lut, &Colorant, FixedK, &Guess);
    
        // Compute error
        error = VEC3distance(&Guess, &Goal);
                        
        // If not convergent, return last safe value
        if (error >= LastError) 
            break;

        // Keep latest values
        LastError = error;

        ToEncoded(LastResult, &Colorant);
        LastResult[3] = FixedK;

                
        // Obtain slope
        ComputeJacobian(Lut, &Jacobian, &Colorant, FixedK);

        // Evaluate jacobian
        MAT3eval(&TestPoint, &Jacobian, &Colorant);
        
        // Move testpoint
        TestPoint.n[0] += (Goal.n[0] - Guess.n[0]);
        TestPoint.n[1] += (Goal.n[1] - Guess.n[1]);
        TestPoint.n[2] += (Goal.n[2] - Guess.n[2]);

        VEC3saturate(&TestPoint);       
        
        // Jacobian ^ -1
        if (MAT3inverse(&Jacobian, &JacobianInverse) < 0) {

            // Singular matrix, 
            break;
        }


        // Obtain colorant from current point
        MAT3eval(&Colorant, &JacobianInverse, &TestPoint);

        // Some clipping....
        VEC3saturate(&Colorant);                
    }

    Result[0] = LastResult[0];
    Result[1] = LastResult[1];
    Result[2] = LastResult[2];
    Result[3] = LastResult[3];

    return LastError;    
    
}





