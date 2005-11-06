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

// Vector & Matrix stuff

#include "lcms.h"


void cdecl VEC3init(LPVEC3 r, double x, double y, double z);
void cdecl VEC3initF(LPWVEC3 r, double x, double y, double z);
void cdecl VEC3toFix(LPWVEC3 r, LPVEC3 v);
void cdecl VEC3scaleFix(LPWORD r, LPWVEC3 Scale);
void cdecl VEC3swap(LPVEC3 a, LPVEC3 b);
void cdecl VEC3divK(LPVEC3 r, LPVEC3 v, double d);
void cdecl VEC3perK(LPVEC3 r, LPVEC3 v, double d);
void cdecl VEC3perComp(LPVEC3 r, LPVEC3 a, LPVEC3 b);
void cdecl VEC3minus(LPVEC3 r, LPVEC3 a, LPVEC3 b);
void cdecl VEC3scaleAndCut(LPWVEC3 r, LPVEC3 v, double d);

void cdecl MAT3identity(LPMAT3 a);
void cdecl MAT3per(LPMAT3 r, LPMAT3 a, LPMAT3 b);
int  cdecl MAT3inverse(LPMAT3 a, LPMAT3 b);
void cdecl MAT3eval(LPVEC3 r, LPMAT3 a, LPVEC3 v);
void cdecl MAT3toFix(LPWMAT3 r, LPMAT3 v);
void cdecl MAT3evalW(LPWVEC3 r, LPWMAT3 a, LPWVEC3 v);
void cdecl MAT3perK(LPMAT3 r, LPMAT3 v, double d);
void cdecl MAT3scaleAndCut(LPWMAT3 r, LPMAT3 v, double d);

// --------------------- Implementation ----------------------------

#define DSWAP(x, y)     {double tmp = (x); (x)=(y); (y)=tmp;}


// Fixed Mul/Div stuff. I guess that inlining these will improve
// the overall speed, but my compiler (BorlandC 4.5) does not have
// this feature functional. Optimizer gets confused and strange errors
// appear. Best make them public functions.

#ifdef USE_ASSEMBLER


#ifdef _MSC_VER
#pragma warning(disable : 4033)
#endif



Fixed32 FixedMul(Fixed32 a, Fixed32 b)
{
       ASM {
              
              mov    eax, ss:a
              mov    edx, ss:b
              imul   edx
              add    eax, 0x8000
              adc    edx, 0
              shrd   eax, edx, 16
              
       }

       RET(_EAX);
}


Fixed32 FixedDiv(Fixed32 Dividend, Fixed32 Divisor)
{
       ASM {
        
        sub     cx,cx           // assume positive result
        mov     eax,Dividend
        and     eax,eax         // positive dividend?
        jns     short FDP1      // yes
        inc     cx              // mark it's a negative dividend
        neg     eax             // make the dividend positive
        }

FDP1:
       ASM {
        sub     edx,edx         //make it a 64-bit dividend, then shift
                                // left 16 bits so that result will be
                                // in EAX
        rol     eax,16          // put fractional part of dividend in
                                // high word of EAX
        mov     dx,ax           // put whole part of dividend in DX
        sub     ax,ax           // clear low word of EAX
        mov     ebx,Divisor
        and     ebx,ebx         // positive divisor?
        jns     short FDP2      //yes
        dec     cx              //mark it's a negative divisor
        neg     ebx             // make divisor positive
        }


FDP2:
        ASM {
        div     ebx             // divide
        shr     ebx,1           // divisor/2, minus 1 if the divisor is
        adc     ebx,0           // even
        dec     ebx
        cmp     ebx,edx         // set Carry if the remainder is at least
        adc     eax,0           // half as large as the divisor, then
                                // use that to round up if necessary
        and     cx,cx           // should the result be made negative?
        jz      short FDP3      // no
        neg     eax             // yes, negate it
        }
FDP3:

        ASM {
        shld    edx,eax,16      // whole part of result in DX;
                                // fractional part is already in AX     
        }

        RET(_EAX);
}




Fixed32 FixedSquare(Fixed32 a)
{
       ASM {
              pushf
              push   edx
              mov    eax, ss:a
              imul   eax
              add    eax, 0x8000
              adc    edx, 0
              shrd   eax, edx, 16
              sar    eax, 16
              pop    edx
              popf
       }

       RET(_EAX);
}



// Perform (a * 65536.0) / 65535.0. This give us a fixed-point
// value normalized into 0...1.0 range


Fixed32 ToFixedDomain(int a)
{
       ASM {
           
           xor       edx, edx
           mov       eax, ss:a
           shld      edx, eax, 16
           sal       eax, 16
           mov       ebx, 0x0000ffff
           div       ebx
           add       dx, 0x8000
           adc       eax, 0
           
       }

       RET(_EAX);
}


// a * 65535.0 + .5
// from 0..1.0 fixed point to 0xffff.

int FromFixedDomain(Fixed32 a)
{
       ASM {
           
           xor       edx, edx
           mov       eax, ss:a
           mov       ebx, 0x0000ffff
           imul      ebx
           add       eax, 0x8000
           adc       edx, 0
           shrd      eax, edx, 16
           
       }

       RET(_EAX);
}


// Linear intERPolation
// a * (h - l) >> 16 + l

Fixed32 FixedLERP(Fixed32 a, Fixed32 l, Fixed32 h)
{
       ASM {
              mov    eax, dword ptr ss:h
              mov    edx, dword ptr ss:l
              push   edx
              mov    ecx, dword ptr ss:a
              sub    eax, edx
              imul   ecx
              shrd   eax, edx, 16
              pop    edx
              add    eax, edx
       }

       RET(_EAX);
}


// a as word is scaled by s as float

WORD FixedScale(WORD a, Fixed32 s)
{
       ASM {

              xor    eax,eax
              mov    ax, ss:a        // This is faster that movzx  eax, ss:a
              sal    eax, 16
              mov    edx, ss:s
              mul    edx
              add    eax, 0x8000
              adc    edx, 0
              mov    eax, edx
       }

       RET(_EAX);
}

#ifdef _MSC_VER
#pragma warning(default : 4033)
#endif

#else

// These are floating point versions for compilers that doesn't
// support asm at all. Use with care, since this will slow down
// all operations

Fixed32 FixedMul(Fixed32 a, Fixed32 b)
{

// If newest "long long" defined, then use it

#ifdef USE_INT64
       LONGLONG l = (LONGLONG) a * b + (LONGLONG) 0x8000;
       return (Fixed32) (l >> 16);
#else
       return DOUBLE_TO_FIXED(FIXED_TO_DOUBLE(a) * FIXED_TO_DOUBLE(b));
#endif
}

Fixed32 FixedDiv(Fixed32 Dividend, Fixed32 Divisor)
{
// If newest "long long" defined, then use it

#ifdef USE_INT64

    LONGLONG l = ((LONGLONG) Dividend) << 32;
    return (Fixed32) ((l / Divisor) >> 16);

#else
       return DOUBLE_TO_FIXED(
                     FIXED_TO_DOUBLE(Dividend) / FIXED_TO_DOUBLE(Divisor));
#endif
}

Fixed32 FixedSquare(Fixed32 a)
{
       return FixedMul(a, a);
}



Fixed32 ToFixedDomain(int a)
{
       return (Fixed32) (((double) a * 65536.0 ) / 65535.0 + 0.5);
}


int FromFixedDomain(Fixed32 a)
{
       return (int) ((double) a * 65535.0 / 65536.0 + 0.5);
}


Fixed32 FixedLERP(Fixed32 a, Fixed32 l, Fixed32 h)
{

       double dif = h - l;

       dif *= a;
       dif /= 65536.0;
       dif += l;

       return (int) dif;
}


WORD FixedScale(WORD a, Fixed32 s)
{
       return (WORD) (a * FIXED_TO_DOUBLE(s));
}

#endif


// Initiate a vector (double version)


void VEC3init(LPVEC3 r, double x, double y, double z)
{
       r -> n[VX] = x;
       r -> n[VY] = y;
       r -> n[VZ] = z;
}

// Init a vector (fixed version)

void VEC3initF(LPWVEC3 r, double x, double y, double z)
{
       r -> n[VX] = DOUBLE_TO_FIXED(x);
       r -> n[VY] = DOUBLE_TO_FIXED(y);
       r -> n[VZ] = DOUBLE_TO_FIXED(z);
}


// Convert to fixed point encoding is 1.0 = 0xFFFF

void VEC3toFix(LPWVEC3 r, LPVEC3 v)
{
       r -> n[VX] = DOUBLE_TO_FIXED(v -> n[VX]);
       r -> n[VY] = DOUBLE_TO_FIXED(v -> n[VY]);
       r -> n[VZ] = DOUBLE_TO_FIXED(v -> n[VZ]);
}

// Convert from fixed point

void VEC3fromFix(LPVEC3 r, LPWVEC3 v)
{
       r -> n[VX] = FIXED_TO_DOUBLE(v -> n[VX]);
       r -> n[VY] = FIXED_TO_DOUBLE(v -> n[VY]);
       r -> n[VZ] = FIXED_TO_DOUBLE(v -> n[VZ]);
}


// Swap two double vectors

void VEC3swap(LPVEC3 a, LPVEC3 b)
{
        DSWAP(a-> n[VX], b-> n[VX]);
        DSWAP(a-> n[VY], b-> n[VY]);
        DSWAP(a-> n[VZ], b-> n[VZ]);
}

// Divide a vector by a constant

void VEC3divK(LPVEC3 r, LPVEC3 v, double d)
{
        double d_inv = 1./d;

        r -> n[VX] = v -> n[VX] * d_inv;
        r -> n[VY] = v -> n[VY] * d_inv;
        r -> n[VZ] = v -> n[VZ] * d_inv;
}

// Multiply by a constant

void VEC3perK(LPVEC3 r, LPVEC3 v, double d )
{
        r -> n[VX] = v -> n[VX] * d;
        r -> n[VY] = v -> n[VY] * d;
        r -> n[VZ] = v -> n[VZ] * d;
}


void VEC3perComp(LPVEC3 r, LPVEC3 a, LPVEC3 b)
{
       r -> n[VX] = a->n[VX]*b->n[VX];
       r -> n[VY] = a->n[VY]*b->n[VY];
       r -> n[VZ] = a->n[VZ]*b->n[VZ];
}

// Minus


void VEC3minus(LPVEC3 r, LPVEC3 a, LPVEC3 b)
{
  r -> n[VX] = a -> n[VX] - b -> n[VX];
  r -> n[VY] = a -> n[VY] - b -> n[VY];
  r -> n[VZ] = a -> n[VZ] - b -> n[VZ];
}


// Check id two vectors are the same, allowing tolerance

static
BOOL RangeCheck(double l, double h, double v)
{
       return (v >= l && v <= h);
}


BOOL VEC3equal(LPWVEC3 a, LPWVEC3 b, double Tolerance)
{
       int i;
       double c;

       for (i=0; i < 3; i++)
       {
              c = FIXED_TO_DOUBLE(a -> n[i]);
              if (!RangeCheck(c - Tolerance,
                              c + Tolerance,
                              FIXED_TO_DOUBLE(b->n[i]))) return FALSE;
       }

       return TRUE;
}


void VEC3scaleFix(LPWORD r, LPWVEC3 Scale)
{
       if (Scale -> n[VX] == 0x00010000L &&
           Scale -> n[VY] == 0x00010000L &&
           Scale -> n[VZ] == 0x00010000L) return;

       r[0] = (WORD) FixedScale(r[0], Scale -> n[VX]);
       r[1] = (WORD) FixedScale(r[1], Scale -> n[VY]);
       r[2] = (WORD) FixedScale(r[2], Scale -> n[VZ]);

}



// Identity


void MAT3identity(LPMAT3 a)
{
        VEC3init(&a-> v[0], 1.0, 0.0, 0.0);
        VEC3init(&a-> v[1], 0.0, 1.0, 0.0);
        VEC3init(&a-> v[2], 0.0, 0.0, 1.0);
}




// Check if matrix is Identity. Allow a tolerance as %

BOOL MAT3isIdentity(LPWMAT3 a, double Tolerance)
{
       int i;
       MAT3 Idd;
       WMAT3 Idf;

       MAT3identity(&Idd);
       MAT3toFix(&Idf, &Idd);

       for (i=0; i < 3; i++)
              if (!VEC3equal(&a -> v[i], &Idf.v[i], Tolerance)) return FALSE;

       return TRUE;

}

// Multiply two matrices


void MAT3per(LPMAT3 r, LPMAT3 a, LPMAT3 b)
{
#define ROWCOL(i, j) \
    a->v[i].n[0]*b->v[0].n[j] + a->v[i].n[1]*b->v[1].n[j] + a->v[i].n[2]*b->v[2].n[j]

    VEC3init(&r-> v[0], ROWCOL(0,0), ROWCOL(0,1), ROWCOL(0,2));
    VEC3init(&r-> v[1], ROWCOL(1,0), ROWCOL(1,1), ROWCOL(1,2));
    VEC3init(&r-> v[2], ROWCOL(2,0), ROWCOL(2,1), ROWCOL(2,2));

#undef ROWCOL //(i, j)
}



// Inverse of a matrix b = a^(-1)
// Gauss-Jordan elimination with partial pivoting

int MAT3inverse(LPMAT3 a, LPMAT3 b)
{
    register int  i, j, max;

    MAT3identity(b);

    // Loop over cols of a from left to right, eliminating above and below diag
    for (j=0; j<3; j++) {   // Find largest pivot in column j among rows j..2

    max = j;                 // Row with largest pivot candidate
    for (i=j+1; i<3; i++)
        if (fabs(a -> v[i].n[j]) > fabs(a -> v[max].n[j]))
            max = i;

    // Swap rows max and j in a and b to put pivot on diagonal

    VEC3swap(&a -> v[max], &a -> v[j]);
    VEC3swap(&b -> v[max], &b -> v[j]);

    // Scale row j to have a unit diagonal

    if (a -> v[j].n[j]==0.)
        return -1;                 // singular matrix; can't invert

    VEC3divK(&b-> v[j], &b -> v[j], a->v[j].n[j]);
    VEC3divK(&a-> v[j], &a -> v[j], a->v[j].n[j]);

    // Eliminate off-diagonal elems in col j of a, doing identical ops to b
    for (i=0; i<3; i++)

        if (i !=j) {
                  VEC3 temp;

          VEC3perK(&temp, &b -> v[j], a -> v[i].n[j]);
          VEC3minus(&b -> v[i], &b -> v[i], &temp);

          VEC3perK(&temp, &a -> v[j], a -> v[i].n[j]);
          VEC3minus(&a -> v[i], &a -> v[i], &temp);
    }
    }

    return 1;
}


// linear transform


void MAT3eval(LPVEC3 r, LPMAT3 a, LPVEC3 v)
{
    r->n[VX] = a->v[0].n[VX]*v->n[VX] + a->v[0].n[VY]*v->n[VY] + a->v[0].n[VZ]*v->n[VZ];
    r->n[VY] = a->v[1].n[VX]*v->n[VX] + a->v[1].n[VY]*v->n[VY] + a->v[1].n[VZ]*v->n[VZ];
    r->n[VZ] = a->v[2].n[VX]*v->n[VX] + a->v[2].n[VY]*v->n[VY] + a->v[2].n[VZ]*v->n[VZ];
}


// Ok, this is another bottleneck of performance.

#ifdef USE_ASSEMBLER


// ecx:ebx is result in 64 bits format
// edi points to matrix, esi points to input vector
// since only 3 accesses are in output, this is a stack variable


void MAT3evalW(LPWVEC3 r_, LPWMAT3 a_, LPWVEC3 v_)
{

       ASM {


       mov    esi, dword ptr ss:v_
       mov    edi, dword ptr ss:a_


   //     r->n[VX] = FixedMul(a->v[0].n[0], v->n[0]) +


       mov       eax,dword ptr [esi]
       mov       edx,dword ptr [edi]
       imul      edx
       mov       ecx, eax
       mov       ebx, edx

   //          FixedMul(a->v[0].n[1], v->n[1]) +

       mov       eax,dword ptr [esi+4]
       mov       edx,dword ptr [edi+4]
       imul      edx
       add       ecx, eax
       adc       ebx, edx

   //         FixedMul(a->v[0].n[2], v->n[2]);

       mov       eax,dword ptr [esi+8]
       mov       edx,dword ptr [edi+8]
       imul      edx
       add       ecx, eax
       adc       ebx, edx

   //  Back to Fixed 15.16

       add       ecx, 0x8000
       adc       ebx, 0
       shrd      ecx, ebx, 16

       push      edi
       mov       edi, dword ptr ss:r_
       mov       dword ptr [edi], ecx      //  r -> n[VX]
       pop       edi



   //   2nd row ***************************

   //        FixedMul(a->v[1].n[0], v->n[0])

       mov       eax,dword ptr [esi]
       mov       edx,dword ptr [edi+12]
       imul      edx
       mov       ecx, eax
       mov       ebx, edx



   //         FixedMul(a->v[1].n[1], v->n[1]) +

       mov       eax,dword ptr [esi+4]
       mov       edx,dword ptr [edi+16]
       imul      edx
       add       ecx, eax
       adc       ebx, edx


       //     FixedMul(a->v[1].n[2], v->n[2]);

       mov       eax,dword ptr [esi+8]
       mov       edx,dword ptr [edi+20]
       imul      edx
       add       ecx, eax
       adc       ebx, edx

       add       ecx, 0x8000
       adc       ebx, 0
       shrd      ecx, ebx, 16

       push      edi
       mov       edi, dword ptr ss:r_
       mov       dword ptr [edi+4], ecx      // r -> n[VY]
       pop       edi

//     3d row **************************

   //       r->n[VZ] = FixedMul(a->v[2].n[0], v->n[0]) +

       mov       eax,dword ptr [esi]
       mov       edx,dword ptr [edi+24]
       imul      edx
       mov       ecx, eax
       mov       ebx, edx


   //    FixedMul(a->v[2].n[1], v->n[1]) +

       mov       eax,dword ptr [esi+4]
       mov       edx,dword ptr [edi+28]
       imul      edx
       add       ecx, eax
       adc       ebx, edx


   //   FixedMul(a->v[2].n[2], v->n[2]);

       mov       eax,dword ptr [esi+8]
       mov       edx,dword ptr [edi+32]
       imul      edx
       add       ecx, eax
       adc       ebx, edx

       add       ecx, 0x8000
       adc       ebx, 0
       shrd      ecx, ebx, 16

       mov       edi, dword ptr ss:r_
       mov       dword ptr [edi+8], ecx      // r -> n[VZ]
       }
}


#else
void MAT3evalW(LPWVEC3 r, LPWMAT3 a, LPWVEC3 v)
{
    r->n[VX] = FixedMul(a->v[0].n[0], v->n[0]) +
               FixedMul(a->v[0].n[1], v->n[1]) +
               FixedMul(a->v[0].n[2], v->n[2]);

    r->n[VY] = FixedMul(a->v[1].n[0], v->n[0]) +
               FixedMul(a->v[1].n[1], v->n[1]) +
               FixedMul(a->v[1].n[2], v->n[2]);

    r->n[VZ] = FixedMul(a->v[2].n[0], v->n[0]) +
               FixedMul(a->v[2].n[1], v->n[1]) +
               FixedMul(a->v[2].n[2], v->n[2]);
}

#endif


void MAT3perK(LPMAT3 r, LPMAT3 v, double d)
{
       VEC3perK(&r -> v[0], &v -> v[0], d);
       VEC3perK(&r -> v[1], &v -> v[1], d);
       VEC3perK(&r -> v[2], &v -> v[2], d);
}


void MAT3toFix(LPWMAT3 r, LPMAT3 v)
{
       VEC3toFix(&r -> v[0], &v -> v[0]);
       VEC3toFix(&r -> v[1], &v -> v[1]);
       VEC3toFix(&r -> v[2], &v -> v[2]);
}

void MAT3fromFix(LPMAT3 r, LPWMAT3 v)
{
       VEC3fromFix(&r -> v[0], &v -> v[0]);
       VEC3fromFix(&r -> v[1], &v -> v[1]);
       VEC3fromFix(&r -> v[2], &v -> v[2]);
}



// Scale v by d and store it in r giving INTEGER

void VEC3scaleAndCut(LPWVEC3 r, LPVEC3 v, double d)
{
        r -> n[VX] = (int) floor(v -> n[VX] * d + .5);
        r -> n[VY] = (int) floor(v -> n[VY] * d + .5);
        r -> n[VZ] = (int) floor(v -> n[VZ] * d + .5);
}

void MAT3scaleAndCut(LPWMAT3 r, LPMAT3 v, double d)
{
       VEC3scaleAndCut(&r -> v[0], &v -> v[0], d);
       VEC3scaleAndCut(&r -> v[1], &v -> v[1], d);
       VEC3scaleAndCut(&r -> v[2], &v -> v[2], d);
}
