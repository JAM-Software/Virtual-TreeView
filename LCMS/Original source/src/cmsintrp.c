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

// Interpolation

#include "lcms.h"

void cmsCalcL16Params(int nSamples, LPL16PARAMS p)
{
       p -> nSamples = nSamples;
       p -> Domain   = (WORD) (nSamples - 1);
       p -> nInputs = p -> nOutputs = 1;

}


void cmsCalcCLUT16Params(int nSamples, int InputChan, int OutputChan, LPL16PARAMS p)
{
       int clutPoints;

       cmsCalcL16Params(nSamples, p);
       p -> nInputs  = InputChan;
       p -> nOutputs = OutputChan;

       clutPoints = p -> Domain + 1;

       p -> opta1 = p -> nOutputs;				// Z
       p -> opta2 = p -> opta1 * clutPoints;    // Y
       p -> opta3 = p -> opta2 * clutPoints;    // X
       p -> opta4 = p -> opta3 * clutPoints;    // Used only in 4 inputs LUT
	   p -> opta5 = p -> opta4 * clutPoints;
	   p -> opta6 = p -> opta5 * clutPoints;	// Used only on 6 inputs LUT
	   p -> opta7 = p -> opta6 * clutPoints;	// Used only on 7 inputs LUT
	   p -> opta8 = p -> opta7 * clutPoints;	// Used only on 8 inputs LUT
}



#ifdef USE_FLOAT


// Floating-point version

WORD cmsLinearInterpLUT16(WORD Value, WORD LutTable[], LPL16PARAMS p)
{
       double y1, y0;
       double y;
       double val2, rest;
       int cell0, cell1;

       // if last value...

       if (Value == 0xffff) return LutTable[p -> Domain];

       val2 = p -> Domain * ((double) Value / 65535.0);

       cell0 = (int) floor(val2);
       // cell1 = cell0 + 1;
	   cell1 = (int) ceil(val2);

       // Rest is 16 LSB bits

       rest = val2 - cell0;

       y0 = LutTable[cell0] ;
       y1 = LutTable[cell1] ;

       y = y0 + (y1 - y0) * rest;


       return (WORD) floor(y+.5);
}

#endif


//
//  Linear interpolation (Fixed-point optimized, but C source)
//


#ifdef USE_C

WORD cmsLinearInterpLUT16(WORD Value1, WORD LutTable[], LPL16PARAMS p)
{
       WORD y1, y0;
       WORD y;
       int dif, a1;
       int cell0, rest;
       int val3, Value;

       // if last value...


       Value = Value1;
       if (Value == 0xffff) return LutTable[p -> Domain];

       val3 = p -> Domain * Value;
       val3 = ToFixedDomain(val3);              // To fixed 15.16

       cell0 = FIXED_TO_INT(val3);             // Cell is 16 MSB bits
       rest  = FIXED_REST_TO_INT(val3);        // Rest is 16 LSB bits

       y0 = LutTable[cell0] ;
       y1 = LutTable[cell0+1] ;

       dif = (int) y1 - y0;        // dif is in domain -ffff ... ffff

       if (dif >= 0)
       {
       a1 = ToFixedDomain(dif * rest);
       a1 += 0x8000;
       }
       else
       {
              a1 = ToFixedDomain((- dif) * rest);
              a1 -= 0x8000;
              a1 = -a1;
       }

       y = y0 + FIXED_TO_INT(a1);

       return (WORD) y;
}

#endif

// Linear interpolation (asm by hand optimized)

#ifdef USE_ASSEMBLER

#ifdef _MSC_VER
#pragma warning(disable : 4033)
#endif

WORD cmsLinearInterpLUT16(WORD Value, WORD LutTable[], LPL16PARAMS p)
{
       int xDomain = p -> Domain;


       if (Value == 0xffff) return LutTable[p -> Domain];
       else
       ASM {
              xor       eax, eax
              mov       ax, word ptr ss:Value
              mov       edx, ss:xDomain
              mul       edx                         //  val3 = p -> Domain * Value;
              shld      edx, eax, 16                // Convert it to fixed 15.16
              shl       eax, 16                     // * 65536 / 65535
              mov       ebx, 0x0000ffff
              div       ebx
              mov       ecx, eax
              sar       ecx, 16                        // ecx = cell0
              mov       edx, eax                       // rest = (val2 & 0xFFFFU)
              and       edx, 0x0000ffff                // edx = rest
              mov       ebx, ss:LutTable
              lea       eax, dword ptr [ebx+2*ecx]     // Ptr to LUT
              xor       ebx, ebx
              mov        bx, word  ptr [eax]           // EBX = y0
              movzx     eax, word  ptr [eax+2]         // EAX = y1
              sub       eax, ebx                       // EAX = y1-y0
              js        IsNegative
              mul       edx                            // EAX = EAX * rest
              shld      edx, eax, 16                   // Pass it to fixed
              sal       eax, 16                        // * 65536 / 65535
              mov       ecx, 0x0000ffff
              div       ecx
              add       eax, 0x8000                    // Rounding
              sar       eax, 16
              add       eax, ebx                       // Done!
              }

              RET((WORD) _EAX);

       IsNegative:

              ASM {
              neg       eax
              mul       edx                            // EAX = EAX * rest
              shld      edx, eax, 16                   // Pass it to fixed
              sal       eax, 16                        // * 65536 / 65535
              mov       ecx, 0x0000ffff
              div       ecx
              sub       eax, 0x8000
              neg       eax
              sar       eax, 16
              add       eax, ebx                       // Done!
              }

              RET((WORD) _EAX);
}

#ifndef __BORLANDC__
#pragma warning(default : 4033)
#endif

#endif

Fixed32 cmsLinearInterpFixed(WORD Value1, WORD LutTable[], LPL16PARAMS p)
{
       Fixed32 y1, y0;
       int cell0;
       int val3, Value;

       // if last value...


       Value = Value1;
       if (Value == 0xffffU) return LutTable[p -> Domain];

       val3 = p -> Domain * Value;
       val3 = ToFixedDomain(val3);              // To fixed 15.16

       cell0 = FIXED_TO_INT(val3);             // Cell is 16 MSB bits

       y0 = LutTable[cell0] ;
       y1 = LutTable[cell0+1] ;


       return y0 + FixedMul((y1 - y0), (val3 & 0xFFFFL));
}


// Reverse Lineal interpolation (16 bits)
// Im using a sort of binary search here, this is not a time-critical function

WORD cmsReverseLinearInterpLUT16(WORD Value, WORD LutTable[], LPL16PARAMS p)
{
        register int l = 1;
        register int r = 0x10000;
        register int x = 0, res;       // 'int' Give spacing for negative values
        int NumZeroes, NumPoles;

        // July/27 2001 - Expanded to handle degenerated curves with an arbitrary
        // number of elements containing 0 at the begining of the table (Zeroes)
        // and another arbitrary number of poles (FFFFh) at the end.
        // First the zero and pole extents are computed, then value is compared.

        NumZeroes = 0;
        while (LutTable[NumZeroes] == 0 && NumZeroes < p -> Domain)
                        NumZeroes++;

        NumPoles = 0;
        while (LutTable[p -> Domain - NumPoles] == 0xFFFF && NumPoles < p -> Domain)
                        NumPoles++;

        // Does the curve belong to this case?
        if (NumZeroes > 1 || NumPoles > 1)
        {				
                int a, b;

				// Identify if value fall downto 0 or FFFF zone				
				if (Value == 0) return 0;
				if (Value == 0xFFFF) return 0xFFFF;

				// else restrict to valid zone

                a = ((NumZeroes-1) * 0xFFFF) / p->Domain;				
                b = ((p -> Domain - NumPoles) * 0xFFFF) / p ->Domain;
				                                				
				l = a - 1;
				r = b + 1;
        }


        // Seems not a degenerated case... apply binary search

        while (r > l) {

                x = (l+r)/2;
                res = (int) cmsLinearInterpLUT16((WORD) (x-1), LutTable, p);
                if (res == Value) return (WORD) x;
                if (res > Value) r = x - 1;
                else l = x + 1;
        }

        return (WORD) x ;
}




// Trilinear interpolation (16 bits) - float version

#ifdef USE_FLOAT
void cmsTrilinearInterp16(WORD Input[], WORD Output[],
                            WORD LutTable[], LPL16PARAMS p)

{
#   define LERP(a,l,h)  (double) ((l)+(((h)-(l))*(a)))
#   define DENS(X, Y, Z)    (double) (LutTable[TotalOut*((Z)+clutPoints*((Y)+clutPoints*(X)))+OutChan])



    double     px, py, pz;
    int        x0, y0, z0,
               x1, y1, z1;
               int clutPoints, TotalOut, OutChan;
    double     fx, fy, fz,
               d000, d001, d010, d011,
               d100, d101, d110, d111,
               dx00, dx01, dx10, dx11,
               dxy0, dxy1, dxyz;


    clutPoints = p -> Domain + 1;
    TotalOut   = p -> nOutputs;

    px = ((double) Input[0] * (p->Domain)) / 65535.0;
    py = ((double) Input[1] * (p->Domain)) / 65535.0;
    pz = ((double) Input[2] * (p->Domain)) / 65535.0;

    x0 = (int) floor(px); fx = px - (double) x0;
    y0 = (int) floor(py); fy = py - (double) y0;
    z0 = (int) floor(pz); fz = pz - (double) z0;

    x1 = x0 + (Input[0] != 0xFFFFU ? 1 : 0);
    y1 = y0 + (Input[1] != 0xFFFFU ? 1 : 0);
    z1 = z0 + (Input[2] != 0xFFFFU ? 1 : 0);


    for (OutChan = 0; OutChan < TotalOut; OutChan++)
    {

        d000 = DENS(x0, y0, z0);
        d001 = DENS(x0, y0, z1);
        d010 = DENS(x0, y1, z0);
        d011 = DENS(x0, y1, z1);

        d100 = DENS(x1, y0, z0);
        d101 = DENS(x1, y0, z1);
        d110 = DENS(x1, y1, z0);
        d111 = DENS(x1, y1, z1);


    dx00 = LERP(fx, d000, d100);
    dx01 = LERP(fx, d001, d101);
    dx10 = LERP(fx, d010, d110);
    dx11 = LERP(fx, d011, d111);

    dxy0 = LERP(fy, dx00, dx10);
    dxy1 = LERP(fy, dx01, dx11);

    dxyz = LERP(fz, dxy0, dxy1);

    Output[OutChan] = (WORD) floor(dxyz + .5);
    }


#   undef LERP
#   undef DENS
}


#endif


#ifndef USE_FLOAT

// Trilinear interpolation (16 bits) - optimized version

void cmsTrilinearInterp16(WORD Input[], WORD Output[],
                            WORD LutTable[], LPL16PARAMS p)

{
#define DENS(i,j,k) (LutTable[(i)+(j)+(k)+OutChan])
#define LERP(a,l,h)     (WORD) (l+ ROUND_FIXED_TO_INT(((h-l)*a)))


           int        OutChan, TotalOut;
           Fixed32    fx, fy, fz;
  register WORD       rx, ry, rz;
           int        x0, y0, z0, x1, y1, z1;
  register int        X0, X1, Y0, Y1, Z0, Z1;
           int        d000, d001, d010, d011,
                      d100, d101, d110, d111,
                      dx00, dx01, dx10, dx11,
                      dxy0, dxy1, dxyz;


    TotalOut   = p -> nOutputs;

    fx = ToFixedDomain((int) Input[0] * p -> Domain);
    x0  = FIXED_TO_INT(fx);
    rx  = (WORD) FIXED_REST_TO_INT(fx);    // Rest in 0..1.0 domain


    fy = ToFixedDomain((int) Input[1] * p -> Domain);
    y0  = FIXED_TO_INT(fy);
    ry  = (WORD) FIXED_REST_TO_INT(fy);

    fz = ToFixedDomain((int) Input[2] * p -> Domain);
    z0 = FIXED_TO_INT(fz);
    rz = (WORD) FIXED_REST_TO_INT(fz);

    x1 = x0 + (Input[0] != 0xFFFFU ? 1 : 0);
    y1 = y0 + (Input[1] != 0xFFFFU ? 1 : 0);
    z1 = z0 + (Input[2] != 0xFFFFU ? 1 : 0);

    Z0 = p -> opta1 * z0;
    Z1 = p -> opta1 * z1;
    Y0 = p -> opta2 * y0;
    Y1 = p -> opta2 * y1;
    X0 = p -> opta3 * x0;
    X1 = p -> opta3 * x1;


    for (OutChan = 0; OutChan < TotalOut; OutChan++)
    {

        d000 = DENS(X0, Y0, Z0);
        d001 = DENS(X0, Y0, Z1);
        d010 = DENS(X0, Y1, Z0);
        d011 = DENS(X0, Y1, Z1);

        d100 = DENS(X1, Y0, Z0);
        d101 = DENS(X1, Y0, Z1);
        d110 = DENS(X1, Y1, Z0);
        d111 = DENS(X1, Y1, Z1);


        dx00 = LERP(rx, d000, d100);
        dx01 = LERP(rx, d001, d101);
        dx10 = LERP(rx, d010, d110);
        dx11 = LERP(rx, d011, d111);

        dxy0 = LERP(ry, dx00, dx10);
        dxy1 = LERP(ry, dx01, dx11);

        dxyz = LERP(rz, dxy0, dxy1);

        Output[OutChan] = (WORD) dxyz;
    }


#   undef LERP
#   undef DENS
}

#endif


#ifdef USE_FLOAT

#define DENS(X, Y, Z)    (double) (LutTable[TotalOut*((Z)+clutPoints*((Y)+clutPoints*(X)))+OutChan])


// Tetrahedral interpolation, using Sakamoto algorithm. This was under
// patent, but the patent is now expired.

void cmsTetrahedralInterp16(WORD Input[],
                            WORD Output[],
                            WORD LutTable[],
                            LPL16PARAMS p)
{
    double     px, py, pz;
    int        x0, y0, z0,
               x1, y1, z1;
    double     fx, fy, fz;
    double     c1=0, c2=0, c3=0;
    int        clutPoints, OutChan, TotalOut;


    clutPoints = p -> Domain + 1;
    TotalOut   = p -> nOutputs;


    px = ((double) Input[0] * p->Domain) / 65535.0;
    py = ((double) Input[1] * p->Domain) / 65535.0;
    pz = ((double) Input[2] * p->Domain) / 65535.0;

    x0 = (int) floor(px); fx = (px - (double) x0);
    y0 = (int) floor(py); fy = (py - (double) y0);
    z0 = (int) floor(pz); fz = (pz - (double) z0);


    x1 = x0 + (Input[0] != 0xFFFFU ? 1 : 0);
    y1 = y0 + (Input[1] != 0xFFFFU ? 1 : 0);
    z1 = z0 + (Input[2] != 0xFFFFU ? 1 : 0);


    for (OutChan=0; OutChan < TotalOut; OutChan++)
    {

       // These are the 6 Tetrahedral

       if (fx >= fy && fy >= fz)
       {
              c1 = DENS(x1, y0, z0) - DENS(x0, y0, z0);
              c2 = DENS(x1, y1, z0) - DENS(x1, y0, z0);
              c3 = DENS(x1, y1, z1) - DENS(x1, y1, z0);
       }
       else
       if (fx >= fz && fz >= fy)
       {
              c1 = DENS(x1, y0, z0) - DENS(x0, y0, z0);
              c2 = DENS(x1, y1, z1) - DENS(x1, y0, z1);
              c3 = DENS(x1, y0, z1) - DENS(x1, y0, z0);
       }
       else
       if (fz >= fx && fx >= fy)
       {
              c1 = DENS(x1, y0, z1) - DENS(x0, y0, z1);
              c2 = DENS(x1, y1, z1) - DENS(x1, y0, z1);
              c3 = DENS(x0, y0, z1) - DENS(x0, y0, z0);
       }
       else
       if (fy >= fx && fx >= fz)
       {
              c1 = DENS(x1, y1, z0) - DENS(x0, y1, z0);
              c2 = DENS(x0, y1, z0) - DENS(x0, y0, z0);
              c3 = DENS(x1, y1, z1) - DENS(x1, y1, z0);

       }
       else
       if (fy >= fz && fz >= fx)
       {
              c1 = DENS(x1, y1, z1) - DENS(x0, y1, z1);
              c2 = DENS(x0, y1, z0) - DENS(x0, y0, z0);
              c3 = DENS(x0, y1, z1) - DENS(x0, y1, z0);
       }
       else
       if (fz >= fy && fy >= fx)
       {
              c1 = DENS(x1, y1, z1) - DENS(x0, y1, z1);
              c2 = DENS(x0, y1, z1) - DENS(x0, y0, z1);
              c3 = DENS(x0, y0, z1) - DENS(x0, y0, z0);
       }
       else
              assert(FALSE);


       Output[OutChan] = (WORD) floor((double) DENS(x0,y0,z0) + c1 * fx + c2 * fy + c3 * fz + .5);
       }

}

#else



//
//     p(x,y,z) = p000 + c1*Dx/(x1-x0) + c2*Dy/(y1-y0) + c3*Dz/(z1-z0)
//
//

#define CELLX(n)     (x##n)
#define CELLY(n)     (y##n)
#define CELLZ(n)     (z##n)

#define DENS(i,j,k) ((Fixed32) LutTable[(i)+(j)+(k)+OutChan] << 16)

#define EVAL  Rest = FixedMul(c1,rx)+FixedMul(c2,ry)+FixedMul(c3,rz);\
              Output[OutChan] = (WORD) FIXED_TO_INT(((int) DENS(X0,Y0,Z0) + FixedDiv(Rest, 0xFFFFU)))



void cmsTetrahedralInterp16(WORD Input[],
                            WORD Output[],
                            WORD LutTable[],
                            LPL16PARAMS p)
{

       Fixed32    fx, fy, fz;
       Fixed32    rx, ry, rz;
       int        x0, y0, z0;
       int        x1, y1, z1;
       Fixed32    c1, c2, c3, Rest;
       int        OutChan;
       register   Fixed32    X0, X1, Y0, Y1, Z0, Z1;
       int TotalOut = p -> nOutputs;



    fx = ToFixedDomain((int) Input[0] * p -> Domain);
    x0  = FIXED_TO_INT(fx);
    rx  = FIXED_REST_TO_INT(fx);    // Rest in 0..1.0 domain


    fy = ToFixedDomain((int) Input[1] * p -> Domain);
    y0  = FIXED_TO_INT(fy);
    ry  = FIXED_REST_TO_INT(fy);

    fz = ToFixedDomain((int) Input[2] * p -> Domain);
    z0 = FIXED_TO_INT(fz);
    rz = FIXED_REST_TO_INT(fz);

    x1 = x0 + (Input[0] != 0xFFFFU ? 1 : 0);
    y1 = y0 + (Input[1] != 0xFFFFU ? 1 : 0);
    z1 = z0 + (Input[2] != 0xFFFFU ? 1 : 0);


    Z0 = p -> opta1 * z0;
    Z1 = p -> opta1 * z1;
    Y0 = p -> opta2 * y0;
    Y1 = p -> opta2 * y1;
    X0 = p -> opta3 * x0;
    X1 = p -> opta3 * x1;


       // These are the 6 Tetrahedral

       if (rx >= ry && ry >= rz)
       {
              for (OutChan=0; OutChan < TotalOut; OutChan++)
              {
              c1 = DENS(X1, Y0, Z0) - DENS(X0, Y0, Z0);
              c2 = DENS(X1, Y1, Z0) - DENS(X1, Y0, Z0);
              c3 = DENS(X1, Y1, Z1) - DENS(X1, Y1, Z0);
              EVAL;
              }

       }
       else
       if (rx >= rz && rz >= ry)
       {
              for (OutChan=0; OutChan < TotalOut; OutChan++)
              {

              c1 = DENS(X1, Y0, Z0) - DENS(X0, Y0, Z0);
              c2 = DENS(X1, Y1, Z1) - DENS(X1, Y0, Z1);
              c3 = DENS(X1, Y0, Z1) - DENS(X1, Y0, Z0);
              EVAL;
              }

       }
       else
       if (rz >= rx && rx >= ry)
       {
              for (OutChan=0; OutChan < TotalOut; OutChan++)
              {
              c1 = DENS(X1, Y0, Z1) - DENS(X0, Y0, Z1);
              c2 = DENS(X1, Y1, Z1) - DENS(X1, Y0, Z1);
              c3 = DENS(X0, Y0, Z1) - DENS(X0, Y0, Z0);
              EVAL;
              }

       }
       else
       if (ry >= rx && rx >= rz)
       {
              for (OutChan=0; OutChan < TotalOut; OutChan++)
              {
              c1 = DENS(X1, Y1, Z0) - DENS(X0, Y1, Z0);
              c2 = DENS(X0, Y1, Z0) - DENS(X0, Y0, Z0);
              c3 = DENS(X1, Y1, Z1) - DENS(X1, Y1, Z0);
              EVAL;
              }

       }
       else
       if (ry >= rz && rz >= rx)
       {
              for (OutChan=0; OutChan < TotalOut; OutChan++)
              {
              c1 = DENS(X1, Y1, Z1) - DENS(X0, Y1, Z1);
              c2 = DENS(X0, Y1, Z0) - DENS(X0, Y0, Z0);
              c3 = DENS(X0, Y1, Z1) - DENS(X0, Y1, Z0);
              EVAL;
              }
       }
       else
       if (rz >= ry && ry >= rx)
       {
              for (OutChan=0; OutChan < TotalOut; OutChan++)
              {
              c1 = DENS(X1, Y1, Z1) - DENS(X0, Y1, Z1);
              c2 = DENS(X0, Y1, Z1) - DENS(X0, Y0, Z1);
              c3 = DENS(X0, Y0, Z1) - DENS(X0, Y0, Z0);
              EVAL;
              }
       }
       else
              assert(FALSE);


}

#endif



/*

  Experimental, doesn't work!

// volume = x, y, z  (x fastest)

#   define DENS(X, Y, Z)    (double) (LutTable[TotalOut*((Z)+clutPoints*((Y)+clutPoints*(X)))+OutChan])
#   define CUBE(x)   ((x) * (x) * (x))
#   define SQR(x)    ((x) * (x))


void cmsTricubicInterp16(WORD Input[],
                            WORD Output[],
                            WORD LutTable[],
                            LPL16PARAMS p)

float           TriCubic (Point p, float *volume, int xDim, int yDim, int zDim)
{
  int             x0, y0, z0;
  register int    i, j, k;
  float           fx, fy, fz;
  register float *pv;
  float           u[4], v[4], w[4];
  float           r[4], q[4];
  float           vox = 0;
  int             xyDim;

  xyDim = xDim * yDim;

  clutPoints = p -> Domain + 1;
  TotalOut   = p -> nOutputs;

  px = ((double) Input[0] * (p->Domain)) / 65535.0;
  py = ((double) Input[1] * (p->Domain)) / 65535.0;
  pz = ((double) Input[2] * (p->Domain)) / 65535.0;

  x0 = (int) floor(px);  fx = px - (double) x0;
  y0 = (int) floor(py);  fy = py - (double) y0;
  z0 = (int) floor(pz);  fz = pz - (double) z0;

  if (x0 < 0 || x0 >= xDim || y0 < 0 || y0 >= yDim || z0 < 0 || z0 >= zDim)
    return (0);

  pv = volume + (x0 - 1) + (y0 - 1) * xDim + (z0 - 1) * xyDim;



  // factors for Catmull-Rom interpolation

  u[0] = -0.5 * CUBE (fx) + SQR (fx) - 0.5 * fx;
  u[1] = 1.5 * CUBE (fx) - 2.5 * SQR (fx) + 1;
  u[2] = -1.5 * CUBE (fx) + 2 * SQR (fx) + 0.5 * fx;
  u[3] = 0.5 * CUBE (fx) - 0.5 * SQR (fx);

  v[0] = -0.5 * CUBE (fy) + SQR (fy) - 0.5 * fy;
  v[1] = 1.5 * CUBE (fy) - 2.5 * SQR (fy) + 1;
  v[2] = -1.5 * CUBE (fy) + 2 * SQR (fy) + 0.5 * fy;
  v[3] = 0.5 * CUBE (fy) - 0.5 * SQR (fy);

  w[0] = -0.5 * CUBE (fz) + SQR (fz) - 0.5 * fz;
  w[1] = 1.5 * CUBE (fz) - 2.5 * SQR (fz) + 1;
  w[2] = -1.5 * CUBE (fz) + 2 * SQR (fz) + 0.5 * fz;
  w[3] = 0.5 * CUBE (fz) - 0.5 * SQR (fz);


  for (OutChan = 0; OutChan < TotalOut; OutChan++) {


  for (k = 0; k < 4; k++)
  {
    q[k] = 0;
    for (j = 0; j < 4; j++)
    {
      r[j] = 0;
      for (i = 0; i < 4; i++)
      {
        r[j] += u[i] * *pv;
        pv++;
      }
      q[k] += v[j] * r[j];
      pv += xDim - 4;
    }
    vox += w[k] * q[k];
    pv += xyDim - 4 * xDim;
  }


  return (vox < 0 ? 0.0 : vox);
}

*/
