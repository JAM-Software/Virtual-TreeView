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

//  Pipeline of LUT. Enclosed by {} are new revision 4.0 of ICC spec.
//
//  [Mat] -> [L1] -> { [Mat3] -> [Ofs3] -> [L3] ->} [CLUT] { -> [L4] -> [Mat4] -> [Ofs4] } -> [L2]
//
//  Some of these stages would be missing. This implements the totality of 
//  combinations of old and new LUT types as follows:
//
//  Lut8 & Lut16
//  ============
//	   [Mat] -> [L1] -> [CLUT] -> [L2]
//
//  Mat2, Ofs2, L3, L3, Mat3, Ofs3 are missing
//
//  LutAToB
//  ========
//
//  [L1] -> [CLUT] -> [L4] -> [Mat4] -> [Ofs4] -> [L2]
//
//	Mat, Mat3, Ofs3, L3 are missing
//   L1 = A curves
//	 L4 = M curves
//	 L2 = B curves
//
//  LutBToA 
//  =======
// 
//	[L1] -> [Mat3] -> [Ofs3] -> [L3] -> [CLUT] -> [L2]
//
//  Mat, L4, Mat4, Ofs4 are missing
//	 L1 = B Curves
//	 L3 = M Curves
//   L2 = A curves		
//	


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

       free(Lut);
}


static
LPVOID DupBlock(LPVOID Org, size_t size)
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
			NewLUT -> L1[i] = DupBlock(Orig ->L1[i], sizeof(WORD) * Orig ->In16params.nSamples);

	   for (i=0; i < Orig ->OutputChan; i++)
			NewLUT -> L2[i] = DupBlock(Orig ->L2[i], sizeof(WORD) * Orig ->Out16params.nSamples);	
	   
	   NewLUT -> T = DupBlock(Orig ->T, Orig -> Tsize);

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
    size_t nTabSize;

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
               }
               break;

       case 2: NewLUT -> wFlags |= LUT_HASTL2;
               cmsCalcL16Params(Tables[0] -> nEntries, &NewLUT -> Out16params);
               NewLUT -> OutputEntries = Tables[0] -> nEntries;
               for (i=0; i < NewLUT -> OutputChan; i++) {

                     PtrW = (LPWORD) malloc(sizeof(WORD) * NewLUT -> OutputEntries);
                     NewLUT -> L2[i] = PtrW;
                     CopyMemory(PtrW, Tables[i]->GammaTable, sizeof(WORD) * NewLUT -> OutputEntries);
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


// Eval gray LUT having only one input channel 
static
void Eval1Input(WORD StageABC[], WORD StageLMN[], WORD LutTable[], LPL16PARAMS p16)
{
	   Fixed32 fk;
       Fixed32 k0, k1, rk, K0, K1;
	   int OutChan;

	   fk = ToFixedDomain((Fixed32) StageABC[0] * p16 -> Domain);
       k0 = FIXED_TO_INT(fk);
       rk = (WORD) FIXED_REST_TO_INT(fk);

	   k1 = k0 + (StageABC[0] != 0xFFFFU ? 1 : 0);

	   K0 = p16 -> opta1 * k0;
       K1 = p16 -> opta1 * k1;

	   for (OutChan=0; OutChan < p16->nOutputs; OutChan++)
	   {

		   StageLMN[OutChan] = (WORD) FixedLERP(rk, LutTable[K0+OutChan],
											        LutTable[K1+OutChan]);
	   }
}



// For more that 3 inputs (i.e., CMYK)
// evaluate two 3-dimensional interpolations and then linearly interpolate between them.
static
void Eval4Inputs(WORD StageABC[], WORD StageLMN[], WORD LutTable[], LPL16PARAMS p16)
{       
       Fixed32 fk;
       Fixed32 k0, rk;
       int K0, K1;
       LPWORD T;
       int i;
       WORD Tmp1[MAXCHANNELS], Tmp2[MAXCHANNELS];

       
       fk = ToFixedDomain((Fixed32) StageABC[0] * p16 -> Domain);
       k0 = FIXED_TO_INT(fk);
       rk = FIXED_REST_TO_INT(fk);

       K0 = p16 -> opta4 * k0;
       K1 = p16 -> opta4 * (k0 + (StageABC[0] != 0xFFFFU ? 1 : 0));

       p16 -> nInputs = 3;

       T = LutTable + K0;
#ifdef USE_TETRAHEDRAL
       cmsTetrahedralInterp16(StageABC + 1,  Tmp1, T, p16);
#else
       cmsTrilinearInterp16(StageABC + 1,  Tmp1, T, p16);
#endif

       T = LutTable + K1;
#ifdef USE_TETRAHEDRAL
       cmsTetrahedralInterp16(StageABC + 1,  Tmp2, T, p16);
#else
       cmsTrilinearInterp16(StageABC + 1,  Tmp2, T, p16);
#endif

       p16 -> nInputs = 4;
       for (i=0; i < p16 -> nOutputs; i++)
       {
              StageLMN[i] = (WORD) FixedLERP(rk, Tmp1[i], Tmp2[i]);
			  
       }

}


static
void Eval5Inputs(WORD StageABC[], WORD StageLMN[], WORD LutTable[], LPL16PARAMS p16)
{       
       Fixed32 fk;
       Fixed32 k0, rk;
       int K0, K1;
       LPWORD T;
       int i;
       WORD Tmp1[MAXCHANNELS], Tmp2[MAXCHANNELS];

       
       fk = ToFixedDomain((Fixed32) StageABC[0] * p16 -> Domain);
       k0 = FIXED_TO_INT(fk);
       rk = FIXED_REST_TO_INT(fk);

       K0 = p16 -> opta5 * k0;
       K1 = p16 -> opta5 * (k0 + (StageABC[0] != 0xFFFFU ? 1 : 0));

       p16 -> nInputs = 4;

       T = LutTable + K0;

	   Eval4Inputs(StageABC + 1, Tmp1, T, p16);

       T = LutTable + K1;

	   Eval4Inputs(StageABC + 1, Tmp2, T, p16);

       p16 -> nInputs = 5;
       for (i=0; i < p16 -> nOutputs; i++)
       {
              StageLMN[i] = (WORD) FixedLERP(rk, Tmp1[i], Tmp2[i]);			  
			  
       }

}


static
void Eval6Inputs(WORD StageABC[], WORD StageLMN[], WORD LutTable[], LPL16PARAMS p16)
{       
       Fixed32 fk;
       Fixed32 k0, rk;
       int K0, K1;
       LPWORD T;
       int i;
       WORD Tmp1[MAXCHANNELS], Tmp2[MAXCHANNELS];

       
       fk = ToFixedDomain((Fixed32) StageABC[0] * p16 -> Domain);
       k0 = FIXED_TO_INT(fk);
       rk = FIXED_REST_TO_INT(fk);

       K0 = p16 -> opta6 * k0;
       K1 = p16 -> opta6 * (k0 + (StageABC[0] != 0xFFFFU ? 1 : 0));

       p16 -> nInputs = 5;

       T = LutTable + K0;

	   Eval5Inputs(StageABC + 1, Tmp1, T, p16);

       T = LutTable + K1;

	   Eval5Inputs(StageABC + 1, Tmp2, T, p16);

       p16 -> nInputs = 6;
       for (i=0; i < p16 -> nOutputs; i++)
       {
              StageLMN[i] = (WORD) FixedLERP(rk, Tmp1[i], Tmp2[i]);
       }

}

static
void Eval7Inputs(WORD StageABC[], WORD StageLMN[], WORD LutTable[], LPL16PARAMS p16)
{       
       Fixed32 fk;
       Fixed32 k0, rk;
       int K0, K1;
       LPWORD T;
       int i;
       WORD Tmp1[MAXCHANNELS], Tmp2[MAXCHANNELS];

       
       fk = ToFixedDomain((Fixed32) StageABC[0] * p16 -> Domain);
       k0 = FIXED_TO_INT(fk);
       rk = FIXED_REST_TO_INT(fk);

       K0 = p16 -> opta7 * k0;
       K1 = p16 -> opta7 * (k0 + (StageABC[0] != 0xFFFFU ? 1 : 0));

       p16 -> nInputs = 6;

       T = LutTable + K0;

	   Eval6Inputs(StageABC + 1, Tmp1, T, p16);

       T = LutTable + K1;

	   Eval6Inputs(StageABC + 1, Tmp2, T, p16);

       p16 -> nInputs = 7;
       for (i=0; i < p16 -> nOutputs; i++)
       {
              StageLMN[i] = (WORD) FixedLERP(rk, Tmp1[i], Tmp2[i]);
       }

}

static
void Eval8Inputs(WORD StageABC[], WORD StageLMN[], WORD LutTable[], LPL16PARAMS p16)
{       
       Fixed32 fk;
       Fixed32 k0, rk;
       int K0, K1;
       LPWORD T;
       int i;
       WORD Tmp1[MAXCHANNELS], Tmp2[MAXCHANNELS];

       
       fk = ToFixedDomain((Fixed32) StageABC[0] * p16 -> Domain);
       k0 = FIXED_TO_INT(fk);
       rk = FIXED_REST_TO_INT(fk);

       K0 = p16 -> opta8 * k0;
       K1 = p16 -> opta8 * (k0 + (StageABC[0] != 0xFFFFU ? 1 : 0));

       p16 -> nInputs = 7;

       T = LutTable + K0;

	   Eval7Inputs(StageABC + 1, Tmp1, T, p16);

       T = LutTable + K1;

	   Eval7Inputs(StageABC + 1, Tmp2, T, p16);

       p16 -> nInputs = 8;
       for (i=0; i < p16 -> nOutputs; i++)
       {
              StageLMN[i] = (WORD) FixedLERP(rk, Tmp1[i], Tmp2[i]);
       }

}



void LCMSEXPORT cmsEvalLUT(LPLUT Lut, WORD In[], WORD Out[])
{
       register unsigned int i;
       WORD StageABC[MAXCHANNELS], StageLMN[MAXCHANNELS];


       // Matrix handling

       if (Lut -> wFlags & LUT_HASMATRIX)
       {
              WVEC3 InVect, OutVect;

              InVect.n[VX] = ToFixedDomain(In[0]);
              InVect.n[VY] = ToFixedDomain(In[1]);
              InVect.n[VZ] = ToFixedDomain(In[2]);

              MAT3evalW(&OutVect, &Lut -> Matrix, &InVect);

              // PCS in 1Fixed15 format, adjusting

              StageABC[0] = Clamp_RGB(FromFixedDomain(OutVect.n[VX]));
              StageABC[1] = Clamp_RGB(FromFixedDomain(OutVect.n[VY]));
              StageABC[2] = Clamp_RGB(FromFixedDomain(OutVect.n[VZ]));
       }
       else
       {
              for (i=0; i < Lut -> InputChan; i++)
                                   StageABC[i] = In[i];
       }


       // First linearization

       if (Lut -> wFlags & LUT_HASTL1)
       {
              for (i=0; i < Lut -> InputChan; i++)
                     StageABC[i] = cmsLinearInterpLUT16(StageABC[i],
                                                   Lut -> L1[i],
                                                   &Lut -> In16params);
       }


	   // TODO: Mat2, Ofs2, L3 processing


       if (Lut -> wFlags & LUT_HAS3DGRID)
       {
              // If it is 4 channels input (like CMYK, for example),
              // evaluate two 3-dimensional interpolations and then,
              // linearly interpolate between them.

		   switch (Lut -> InputChan) {


		   case 1: // Gray LUT

			   Eval1Input(StageABC, StageLMN, Lut->T, &Lut->CLut16params);
			   break;

           case 3:
              
#ifdef USE_TETRAHEDRAL
				cmsTetrahedralInterp16(StageABC, StageLMN, Lut -> T, &Lut -> CLut16params);
#else

				cmsTrilinearInterp16(StageABC, StageLMN, Lut -> T, &Lut -> CLut16params);
#endif
			    break;

		   case 4: 
				Eval4Inputs(StageABC, StageLMN, Lut->T, &Lut -> CLut16params);
				break;

		   case 5:
				Eval5Inputs(StageABC, StageLMN, Lut->T, &Lut -> CLut16params);
				break;           

		   case 6:
				Eval6Inputs(StageABC, StageLMN, Lut->T, &Lut -> CLut16params);
				break;           
			case 7:
			   Eval7Inputs(StageABC, StageLMN, Lut->T, &Lut -> CLut16params);
			   break;

		   case 8:
			   Eval8Inputs(StageABC, StageLMN, Lut->T, &Lut -> CLut16params);
			   break;

		   default:
				cmsSignalError(LCMS_ERRC_ABORTED, "Unsupported restoration (%d channels)", Lut -> InputChan);
		   }

       }
       else
       {              

              for (i=0; i < Lut -> InputChan; i++)
                            StageLMN[i] = StageABC[i];

       }


	   // TODO: Mat3, Ofs3, L3 processing

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

}
