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

// Test Suite for Little cms

#define ICM_COMPARATIVE      1
#define CHECK_SPEED          1

#include "lcms.h"
#include <time.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef NON_WINDOWS
#include <icm.h>
#endif

#define PREC  20

#define TYPE_XYZA_16            (COLORSPACE_SH(PT_XYZ)|CHANNELS_SH(3)|BYTES_SH(2)|EXTRA_SH(1))
#define TYPE_LABA_16            (COLORSPACE_SH(PT_Lab)|CHANNELS_SH(3)|BYTES_SH(2)|EXTRA_SH(1))

typedef struct {BYTE r, g, b, a;}   Scanline_rgb1;
typedef struct {WORD r, g, b, a;}   Scanline_rgb2;

// Print a dot for gauging

static 
void Dot(void)
{
    fprintf(stdout, "."); fflush(stdout);
}


// Are we little or big endian?  From Harbison&Steele.  

static
int CheckEndianess(void)
{
   int BigEndian, IsOk;   
   union {

     long l;
     char c[sizeof (long)];
   
   } u;

   u.l = 1;
   BigEndian = (u.c[sizeof (long) - 1] == 1);

#ifdef USE_BIG_ENDIAN
        IsOk = BigEndian;
#else
        IsOk = !BigEndian;
#endif

        if (!IsOk) {

            printf("\nOOOPPSS! You have USE_BIG_ENDIAN toggle misconfigured!\n\n");
            printf("Please, edit lcms.h and %s the USE_BIG_ENDIAN toggle.\n", BigEndian? "uncomment" : "comment");
            return 0;
        }

        return 1;

}


static
int CheckSwab(void)
{
 // ml: unsigned char conflicts with signed char in swab under BCB 6.
    /*unsigned*/ char Test[] = { 1, 2, 3, 4, 5, 6};

#ifdef USE_CUSTOM_SWAB
        return 1;
#endif

#ifdef USE_BIG_ENDIAN
        return 1;
#endif

    swab(Test, Test, 6);

    if (strncmp(Test, "\x2\x1\x4\x3\x6\x5", 6) != 0)
    {
            printf("\nOOOPPSS! swab() does not work as expected in your machine!\n\n");
            printf("Please, edit lcms.h and uncomment the USE_CUSTOM_SWAB toggle.\n");
            return 0;

    }
    return 1;
}

typedef struct _Stats {
                      double n, x, y, x2, y2, xy;
                      double Peak;
                      } STATS, FAR* LPSTATS;

static void ClearStats(LPSTATS p)
{       
       p -> n = p -> x = p -> y = p -> x2 = p -> y2 = p -> xy 
       = p -> Peak = 0.0;
}

static double Std(LPSTATS p)
{
       return sqrt((p->n*p->x2 - p->x * p->x) / (p->n*(p->n-1)));
}



static
void PrintStatistics(clock_t atime, LPSTATS Stats)
{

       clock_t diff;
       double a;

       diff = clock() - atime;
       a = (double) diff / CLK_TCK;

       // These are statistics of 16 bit, so divide
       // by 257 to get dE relative to 8 bits

       printf("\ndE: mean=%g, SD=%g, max=%g ",
                     (Stats->x / Stats -> n) / 257.,
                     (Std(Stats)) / 257.,
                     Stats -> Peak / 257.);

       if (atime > 0)
            printf("[%d tics, %g sec.]", (int) diff, a);

}


// Simpler fixed-point math

static
void TestFixedPoint(void)
{
       Fixed32 a, b, c, d, e;
       double f;

       a = DOUBLE_TO_FIXED(1.12345);
       b = DOUBLE_TO_FIXED(2.56789);

       c = FixedMul(a, b);
       e = FixedDiv(a, b);

       d = (c & 0xffff);
       f = ((double) d / 0xffff) * 10000.0;

       printf("Testing fixed point: 2.8848960205 = %d.%d\n", FIXED_TO_INT(c), (int) f);

       d = (e & 0xffff);
       f = ((double) d / 0xffff) * 10000.0;

       printf("\t\t0.437499269828536 = %d.%d\n", FIXED_TO_INT(e), (int) f);
}



static
int TestFixedScaling(void)
{
       int i, j, nfl, nfx;
       double FloatFactor;
       Fixed32 FixedFactor;



       printf("Testing fixed scaling...");

       for (j=5; j<100; j++)
       {
       FloatFactor = (double) j / 100.0 ;
       FloatFactor = FIXED_TO_DOUBLE(DOUBLE_TO_FIXED(FloatFactor));
       FixedFactor = DOUBLE_TO_FIXED(FloatFactor);

       for (i=0; i < 0x10000L; i++)
              {
                     nfl = (WORD) ((double) i * FloatFactor);
                     nfx = FixedScale((WORD) i, FixedFactor);

                     if (nfl != nfx) {
                            printf("Failed!\ni=%x (%d), float=%x, fixed=%x", i, i, nfl, nfx);
                            return 0;
                            }
              }

       }

       printf ("pass.\n");
       return 1;
}

// Curve joining test. Joining two high-gamma of 3.0 curves should
// give something like linear

static
int TestJointCurves(void)
{
    LPGAMMATABLE Forward, Reverse, Result;
    BOOL rc;

    printf("Testing curves join ...");

    Forward = cmsBuildGamma(256, 3.0);
    Reverse = cmsBuildGamma(256, 3.0);

    Result = cmsJoinGammaEx(Forward, Reverse, 256);

    cmsFreeGamma(Forward); cmsFreeGamma(Reverse); 

    rc = cmsIsLinear(Result->GammaTable, Result ->nEntries);
    cmsFreeGamma(Result); 

    if (!rc) {
        printf("failed!\n");
        return 0;
    }
    else {
        printf("pass.\n");
        return 1;
    }
    
}


// Check reversing of gamma curves

#define NPOINTS     1024

static
int TestReversingOfCurves(void)
{
    LPGAMMATABLE Gamma, Reverse, Computed;
    int i;
    double dE;
    STATS Stats;

    printf("Testing reversing of curves ...");
    ClearStats(&Stats);


    Gamma   = cmsBuildGamma(NPOINTS, 3.0);
    Reverse = cmsBuildGamma(NPOINTS, 1.0/3.0);

    Computed = cmsReverseGamma(NPOINTS, Gamma);
    
    for (i=0; i < NPOINTS; i++) {

            dE = fabs(Reverse->GammaTable[i] - Computed->GammaTable[i]);

            Stats.x += dE;                                   
            Stats.x2 += (dE * dE);
            Stats.n += 1.0;
            if (dE > Stats.Peak) 
                Stats.Peak = dE;

            if (dE > 0x1100) {
                printf("Coarse error! %x", (int) dE);
                return 0;
            }
                                   
    }

    PrintStatistics(0, &Stats); 
    printf(" pass.\n");
    cmsFreeGamma(Gamma);
    cmsFreeGamma(Reverse);
    cmsFreeGamma(Computed);
    return 1;
}

// Linear interpolation test. Here I check the cmsLinearInterpLUT16
// Tables are supposed to be monotonic, but the algorithm works on
// non-monotonic as well.

static
int TestLinearInterpolation(int lExhaustive)
{
       static WORD Tab[4098];
       int j, i, k;
       L16PARAMS p;
       int n;
       clock_t time;

       printf("Testing linear interpolation ...");

       // First I will check exact values. Since prime factors of 65535 (FFFF) are,
       //
       //            0xFFFF = 1 * 3 * 5 * 17 * 257
       //
       // I test tables of 2, 4, 6, and 18 points, that will be exact.
       // Then, a table of 3 elements are tested. Error must be < 1
       // Since no floating point is involved, This will be a measure of speed.


       // Perform 10 times, so i can measure average times

       time = clock();
       for (j=0; j < 10; j++)
       {

       // 2 points - exact

       Tab[0] = 0;
       Tab[1] = 0xffffU;

       cmsCalcL16Params(2, &p);

       for (i=0; i <= 0xffffL; i++)
       {
              n = cmsLinearInterpLUT16((WORD) i, Tab, &p);
              if (n != i)
                     {
                     printf("Error in Linear interpolation (2p): Must be i=%x, But is n=%x\n", i, n);
                     return 0;
                     }

       }


       // 3 points - Here the error must be <= 1, since
       // 2 == (3 - 1)  is not a factor of 0xffff

       Tab[0] = 0;
       Tab[1] = 0x7FFF;
       Tab[2] = 0xffffU;

       cmsCalcL16Params(3, &p);

       for (i=0; i <= 0xffffL; i++)
       {
              n = cmsLinearInterpLUT16((WORD) i, Tab, &p);
              if (abs(n - i) > 1)
                     {
                     printf("Error in Linear interpolation (3p): Must be i=%x, But is n=%x\n", i, n);
                     return 0;
                     }

       }


       // 4 points - exact

       Tab[0] = 0;
       Tab[1] = 0x5555U;
       Tab[2] = 0xAAAAU;
       Tab[3] = 0xffffU;

       cmsCalcL16Params(4, &p);

       for (i=0; i <= 0xffffL; i++)
       {
              n = cmsLinearInterpLUT16((WORD) i, Tab, &p);
              if (n != i) {
                     printf("Error in Linear interpolation (4p): Must be i=%x, But is n=%x\n", i, n);
                     return 0;
                     }

       }


       // 6 - points

       Tab[0] = 0;
       Tab[1] = 0x3333U;
       Tab[2] = 0x6666U;
       Tab[3] = 0x9999U;
       Tab[4] = 0xCCCCU;
       Tab[5] = 0xFFFFU;

       cmsCalcL16Params(6, &p);

       for (i=0; i <= 0xffffL; i++)
       {
              n = cmsLinearInterpLUT16((WORD) i, Tab, &p);
              if (n != i) {
                     printf("Error in Linear interpolation (6p): Must be i=%x, But is n=%x\n", i, n);
                     return 0;
                     }

       }


       // 18 points

       for (i=0; i < 18; i++)
              Tab[i] = (WORD) (0x0f0fU*i);

       cmsCalcL16Params(18, &p);

       for (i=0; i <= 0xffffL; i++)
       {
              n = cmsLinearInterpLUT16((WORD) i, Tab, &p);
              if (n != i) {
                     printf("Error in Linear interpolation (18p): Must be i=%x, But is n=%x\n", i, n);
                     return 0;
                     }
       }
       }



       printf("pass. (%d tics)\n", (int) (clock() - time));

       // Now test descending tables
       printf("Testing descending tables (linear interpolation)...");

       // 2 points - exact

       Tab[1] = 0;
       Tab[0] = 0xffffU;

       cmsCalcL16Params(2, &p);

       for (i=0xffffL; i > 0; --i)
       {
              n = cmsLinearInterpLUT16((WORD) i, Tab, &p);
              if ((0xffffL - n) != i) {

                     printf("Error in Linear interpolation (descending) (2p): Must be i=%x, But is n=%x\n", i, 0xffff - n);
                     return 0;
                     }
       }


       // 3 points - Here the error must be <= 1, since
       // 2 = (3 - 1)  is not a factor of 0xffff

       Tab[2] = 0;
       Tab[1] = 0x7FFF;
       Tab[0] = 0xffffU;

       cmsCalcL16Params(3, &p);

       for (i=0xffffL; i > 0; --i)
       {
              n = cmsLinearInterpLUT16((WORD) i, Tab, &p);
              if (abs((0xffffL - n) - i) > 1) {

                     printf("Error in Linear interpolation (descending) (3p): Must be i=%x, But is n=%x\n", i, n);
                     return 0;
                     }
       }


       // 4 points - exact

       Tab[3] = 0;
       Tab[2] = 0x5555U;
       Tab[1] = 0xAAAAU;
       Tab[0] = 0xffffU;

       cmsCalcL16Params(4, &p);

       for (i=0xffffL; i > 0; --i)
       {
              n = cmsLinearInterpLUT16((WORD) i, Tab, &p);
              if ((0xffffL - n) != i) {

                     printf("Error in Linear interpolation (descending) (4p): Must be i=%x, But is n=%x\n", i, n);
                     return 0;
                     }
       }


       // 6 - points

       Tab[5] = 0;
       Tab[4] = 0x3333U;
       Tab[3] = 0x6666U;
       Tab[2] = 0x9999U;
       Tab[1] = 0xCCCCU;
       Tab[0] = 0xFFFFU;

       cmsCalcL16Params(6, &p);

       for (i=0xffffL; i > 0; --i)
       {
              n = cmsLinearInterpLUT16((WORD) i, Tab, &p);
              if ((0xffffL - n) != i) {
                     printf("Error in Linear interpolation (descending) (6p): Must be i=%x, But is n=%x\n", i, n);
                     return 0;
                     }

       }


       // 18 points

       for (i=0; i < 18; i++)
              Tab[17-i] = (WORD) (0x0f0fU*i);

       cmsCalcL16Params(18, &p);

       for (i=0xffffL; i > 0; --i)
       {
              n = cmsLinearInterpLUT16((WORD) i, Tab, &p);
              if ((0xffffL - n) != i) {

                     printf("Error in Linear interpolation (descending) (18p): Must be i=%x, But is n=%x\n", i, n);
                     return 0;
                     }
       }

       printf("pass.\n");

       if (!lExhaustive) return 1;

       printf("Now, checking interpolation errors for tables of n elements ...\n");

       for (j=10; j < 4096; j ++)
       {
       printf("%d\r", j);

       for (i=0; i <= j; i++)
              {
              Tab[i] = (WORD) floor((((double) i / ((double) j-1)) * 65535.0) + .5);
              }

       k =0;
       cmsCalcL16Params(j, &p);
       for (i=0; i <= 0xffffL; i++)
       {
              n = cmsLinearInterpLUT16((WORD) i, Tab, &p);
              if (n != i) k++;

       }
       if (k > 0) printf("\r%d: %d errors\n", j, k);
       }

       return 1;
}



static
int IsGood(const char *frm, WORD in, WORD out)
{

        if ((abs(in - out) > 2)) {

              printf("error %s %x - %x\n", frm, in, out);
              return 0;
              }

       return 1;
}

static
BOOL TestReverseLinearInterpolation(void)
{
        WORD Tab[20];
        L16PARAMS p;
        int i, n, v;

        printf("Testing reverse linear interpolation\n");


        cmsCalcL16Params(16, &p);

        // ml: WORD() type cast added to prevent BCB 6 compiler from complaining.
        for (i=0; i < 16; i++) Tab[i] = WORD(i * 0x1111);

        printf("\ton normal monotonic curve...");
        for (i=0; i < 16; i++)
        {
              v = (i * 0x1111);
              n = cmsReverseLinearInterpLUT16((WORD) v, Tab, &p);
              if (!IsGood("unexpected result", (WORD) v, (WORD) n))
                        return FALSE;
       }
       printf("pass.\n");


        Tab[0] = 0;
        Tab[1] = 0;
        Tab[2] = 0;
        Tab[3] = 0;
        Tab[4] = 0;
        Tab[5] = 0x5555;
        Tab[6] = 0x6666;
        Tab[7] = 0x7777;
        Tab[8] = 0x8888;
        Tab[9] = 0x9999;
        Tab[10]= 0xffff;
        Tab[11]= 0xffff;
        Tab[12]= 0xffff;
        Tab[13]= 0xffff;
        Tab[14]= 0xffff;
        Tab[15]= 0xffff;


        printf("\ton degenerated curve ...");

        for (i=0; i < 16; i++)
        {
              v = (i * 0x1111);
              n = cmsReverseLinearInterpLUT16((WORD) v, Tab, &p);

              if (i > 5 && i <= 9) {

              if (!IsGood("unexpected result", (WORD)  v, (WORD) n))
                        return FALSE;
              }
       }

       printf("pass.\n");

     return TRUE;
}




// 3D LUT test

static
int Test3D(void)
{
   LPLUT MyLut;
   LPWORD Table;
   WORD In[3], Out[3];
   int r, g, b, i;
   double *SampleTablePtr, SampleTable[] = { //R     G    B

                                              0,    0,   0,     // B=0,G=0,R=0
                                              0,    0,  .25,    // B=1,G=0,R=0

                                              0,   .5,    0,    // B=0,G=1,R=0
                                              0,   .5,  .25,    // B=1,G=1,R=0

                                              1,    0,    0,    // B=0,G=0,R=1
                                              1,    0,  .25,     // B=1,G=0,R=1

                                              1,    .5,   0,    // B=0,G=1,R=1
                                              1,    .5,  .25    // B=1,G=1,R=1

                                              };


      printf("Testing 3D interpolation on LUT...");

      // 1.- Allocate an empty LUT

      MyLut = cmsAllocLUT();

      // 2.- In this LUT, allocate a 3D grid of 2 points, from 3 components (RGB)
      //     to 3 components. First 3 is input dimension, last 3 is output one.
      //         2 is number of grid points.

      MyLut = cmsAlloc3DGrid(MyLut, 2, 3, 3);

      // 3.- Fill the LUT table with values.

      Table = MyLut -> T;

      SampleTablePtr = SampleTable;

      for (i= 0; i < 3; i++)
       for (r = 0; r < 2; r++)
         for (g = 0; g < 2; g++)
          for (b = 0; b < 2; b++) {

            WORD a =  (WORD) floor(*SampleTablePtr++ * 65535. + .5);

            *Table++ = a;
        }


   // The sample table gives
   //
   //        r = input,
   //           g = input divided by 2
   //            b = input divided by 4
   //
   // So, I should obtain on output r, g/2, g/4


   for (i=0; i < 0xffff; i++) {

       In[0] = In[1] = In[2] = (WORD) i;

       cmsEvalLUT(MyLut, In, Out);

      // Check results, I will tolerate error <= 1  for rounding

       // printf("%x, %x\n", In[0], Out[0]);

       if (!IsGood("Channel 1", Out[0], In[0])) return 0;
       if (!IsGood("Channel 2", Out[1], (WORD) ((double) In[1] / 2))) return 0;
       if (!IsGood("Channel 3", Out[2], (WORD) ((double) In[2] / 4))) return 0;

      }

   // Last, remember free stuff

   cmsFreeLUT(MyLut);

   printf("pass.\n");
   return 1;
}



static
void PrintMatrix(LPMAT3 lpM)
{
       int i, j;

       for (i=0; i < 3; i++) {
              printf ("[ ");
              for (j=0; j < 3; j++)
                     {
                            printf("%1.6f  ", (*lpM).v[i].n[j]);
                     }
              printf("]\n");
       }

       printf("\n");

}


static
BOOL CmpMatrix(LPMAT3 lpM1, LPMAT3 lpM2, double tolerance)
{
       int i, j;

       for (i=0; i < 3; i++) {
              for (j=0; j < 3; j++) {
                        if (fabs(lpM1 -> v[i].n[j] - lpM2 -> v[i].n[j]) > tolerance)
                                        return FALSE;
                     }

       }

       return TRUE;

}


static
BOOL TestMatrixCreation(void)
{
       MAT3 Mat;
       int rc;

       cmsCIExyY WhitePt =  {0.3127, 0.3290, 1.0};
       cmsCIExyYTRIPLE Primaries = {
                                   {0.6400, 0.3300, 1.0},
                                   {0.3000, 0.6000, 1.0},
                                   {0.1500, 0.0600, 1.0}
                                   };
       MAT3 sRGB = {{
                   {{ 0.436066,  0.385147,  0.143066 }},
                   {{ 0.222488,  0.716873,  0.060608 }},
                   {{ 0.013916,  0.097076,  0.714096 }}
                   }};


       printf("Testing virtual profiles (Emulating sRGB)...");

       rc = cmsBuildRGB2XYZtransferMatrix(&Mat,
                                          &WhitePt,
                                          &Primaries);
       cmsAdaptMatrixToD50(&Mat, &WhitePt);

       if (rc < 0)
       {
       printf("TestMatrixCreation failed, rc = %d\n", rc);
       return FALSE;
       }
      

       if (!CmpMatrix(&Mat, &sRGB, 0.001)) {
                printf("FAILED!\n");
                printf("sRGB final matrix is:\n");
                PrintMatrix(&sRGB);
                printf("\nlcms calculated matrix is:\n");
                PrintMatrix(&Mat);
                return FALSE;
       }

       printf("pass.\n");
       return TRUE;


}


/*

       Used for debug purposes
*/

#if 0
static
void AdaptationMatrixTest(void)
{
       cmsCIExyY D65 = {0.3127, 0.329001, 1.0};   // D65
       MAT3 sRGB, TosRGB;


       VEC3init(&sRGB.v[0], 0.4124,  0.3576,  0.1805);
       VEC3init(&sRGB.v[1], 0.2126,  0.7152,  0.0722);
       VEC3init(&sRGB.v[2], 0.0193,  0.1192,  0.9505);

       cmsAdaptMatrixToD50(&sRGB, &D65);
       printf("Adaptation matrix D65 -> D50 (to PCS)\n");
       PrintMatrix(&sRGB);

       MAT3inverse(&sRGB, &TosRGB);
       printf("inverse\n");
       PrintMatrix(&TosRGB);

       cmsAdaptMatrixFromD50(&TosRGB, &D65);
       printf("adaptated to D65\n");
       PrintMatrix(&TosRGB);
}
#endif
// #endif



static
double VecDist(Scanline_rgb2 *bin, Scanline_rgb2 *bout)
{
       double rdist, gdist, bdist;

       rdist = fabs(bout -> r - bin -> r);
       gdist = fabs(bout -> g - bin -> g);
       bdist = fabs(bout -> b - bin -> b);

       return (sqrt((rdist*rdist + gdist*gdist + bdist*bdist)));
}






// Perform sampling in the full spectrum & acotate error.
// I choose red for the lowest incidence in eye.
// Green is most lightful, eye is most accurate on blue.

static
int TestFullSpectrum(cmsHTRANSFORM xform, int nRedInterv, int MaxErr)
{
       int r, g, b;
       double err;
       Scanline_rgb2 *bin, *bout;
       STATS Stats;
       clock_t t;


       bin  = (Scanline_rgb2 *) malloc(256*sizeof(Scanline_rgb2));
       bout = (Scanline_rgb2 *) malloc(256*sizeof(Scanline_rgb2));


       ClearStats(&Stats);

       Stats.x = 0.0; Stats.n = 0.0; // GCC BUG HERE!!!!

       t = clock();

       for (r=0; r < 256; r+= nRedInterv)
       {
              // printf("\r%02x:", r);

              Dot();
              for (g=0; g < 256; g++)
                     {

                            for (b=0; b < 256; b++)
                            {
                            // ml: WORD() type cast added to prevent BCB 6 compiler from complaining.
                            bin[b].r = WORD(r << 8);          // For L 0nly to 0xFF00
                            bin[b].g = RGB_8_TO_16(g);
                            bin[b].b = RGB_8_TO_16(b);
                            }

                            cmsDoTransform(xform, bin, bout, 256);

                            // I'm using b as index

                            for (b=0; b < 256; b ++)
                            {
                                   // I measure the error using vector distance

                                   err = VecDist(bin+b, bout+b);
                                   Stats.x += (double) err;
                                   Stats.x2 += (double) err * err;
                                   Stats.n += 1.0;
                                   if (err > Stats.Peak)
                                          Stats.Peak = err;


                                   if (err > MaxErr)
                                   {
                                          printf("Coarse error! : In=(%x,%x,%x) Out=(%x,%x,%x)\n",
                                                        bin[b].r, bin[b].g, bin[b].b,
                                                        bout[b].r, bout[b].g, bout[b].b);
                                          free(bin);
                                          free(bout);
                                          return 0;
                                   }
                            }

                     }

       }


       PrintStatistics(t, &Stats);
       free(bin);
       free(bout);

       return 1;
}




static
int TestInducedError(DWORD Type)
{
  // ml: cmsHPROFILE is already a pointer type. Don't use a pointer to a pointer here. cmsCreate... returns cmsHPROFILE.
  cmsHPROFILE In, Out;
       //cmsHPROFILE *In, *Out;
       cmsHTRANSFORM xform;
       int nMaxError;

       In  = cmsCreateLabProfile(NULL);
       Out = cmsCreateLabProfile(NULL);

       printf("Error Induced by the CMM due to roundoff (dE) ");

       xform = cmsCreateTransform(In,  Type,
                                  Out, Type,
                                  INTENT_RELATIVE_COLORIMETRIC, 0);
              

       nMaxError = TestFullSpectrum(xform, 31, 0x1000L);

       printf("\n");

       cmsDeleteTransform(xform);
       cmsCloseProfile(In);
       cmsCloseProfile(Out);

       return nMaxError;
}


static
double ConvertL(WORD v)
{
       int fix32;

       fix32 = v;
       return (double)fix32/652.800;    /* 0xff00/100.0 */
}


static
double Convertab(WORD v)
{
       int fix32;


       fix32 = v;

       return ((double)fix32/256.0)-128.0;
}


#define BASE  255

static
int CompareTransforms(cmsHTRANSFORM xform1, cmsHTRANSFORM xform2, 
                      int nRedInterv, int lWithStats, BOOL lIsLab)
{
       int r, g, b;
       double err;
       Scanline_rgb2 *bin, *bout1, *bout2;
       STATS Stats;
       int OutOfGamut = 0;


       bin   = (Scanline_rgb2 *) malloc(256*sizeof(Scanline_rgb2));
       bout1 = (Scanline_rgb2 *) malloc(256*sizeof(Scanline_rgb2));
       bout2 = (Scanline_rgb2 *) malloc(256*sizeof(Scanline_rgb2));


       ClearStats(&Stats);
       Stats.x = 0.0; Stats.n = 0.0; // GCC BUG HERE!!!!


       for (r=0; r < BASE; r+= nRedInterv)
       {
              // printf("\r%02x:", r);

              Dot();
              for (g=0; g < BASE; g++)
                     {
                            // I will test random LSB

                            for (b=0; b < BASE; b++)     // 256
                            {

                            bin[b].r = RGB_8_TO_16(r);
                            bin[b].g = RGB_8_TO_16(g);
                            bin[b].b = RGB_8_TO_16(b);
                            }

                            cmsDoTransform(xform1, bin, bout1, 256);
                            cmsDoTransform(xform2, bin, bout2, 256);

                            // I'm using b as index

                            for (b=0; b < BASE; b ++) {

                                   // I measure the error using vector distance
                                   // Only if encodable values

                              if (bout1[b].r != 0xffff && bout1[b].g != 0xffff && bout1[b].b != 0xffff)
                              {

                                   err = VecDist(bout1+b, bout2+b);


                                   if (err > 0x1000L)
                                   {

                                       if (lIsLab) {
                                          printf("Coarse error: In=(%x,%x,%x) Out1=(%g,%g,%g) Out2=(%g,%g,%g)\n",
                                                        bin[b].r, bin[b].g, bin[b].b,
                                                        ConvertL(bout1[b].r), Convertab(bout1[b].g), Convertab(bout1[b].b),
                                                        ConvertL(bout2[b].r), Convertab(bout2[b].g), Convertab(bout2[b].b));
                                       }
                                       else
                                       {
                                        printf("Coarse error: In=(%x,%x,%x) Out1=(%x,%x,%x) Out2=(%x,%x,%x)\n",
                                                        bin[b].r, bin[b].g, bin[b].b,
                                                        bout1[b].r, bout1[b].g, bout1[b].b,
                                                        bout2[b].r, bout2[b].g, bout2[b].b);
                                       }
                                       return 0;

                                   }

                                   else
                                   {
                                   Stats.x += (double) err;
                                   Stats.x2 += (double) err * err;
                                   Stats.n += 1.0;
                                   if (err > Stats.Peak)
                                          Stats.Peak = err;
                                   }
                              } else
                                   OutOfGamut++;
                            }

                     }

       }


       if (lWithStats) {

        PrintStatistics(0, &Stats);
        printf(" pass.\n");
       }

       if (OutOfGamut > 0)
                printf("Out of encodeable representation=%d\n\n", OutOfGamut);
     

       free(bin);
       free(bout1);
       free(bout2);

       return 1;
}



static
BOOL CheckXYZ(LPcmsCIEXYZ Check, double X, double Y, double Z)
{
    return ((fabs(Check->X - X) < 0.001) && 
            (fabs(Check->Y - Y) < 0.001) &&
            (fabs(Check->Z - Z) < 0.001));
}


static
LPGAMMATABLE Build_sRGBGamma(void)
{
    double Parameters[5];

    Parameters[0] = 2.4;
    Parameters[1] = 1. / 1.055;
    Parameters[2] = 0.055 / 1.055;
    Parameters[3] = 1. / 12.92;
    Parameters[4] = 0.04045;    // d

    return cmsBuildParametricGamma(1024, 4, Parameters);
}

static
BOOL Check_sRGBGamma(LPGAMMATABLE Shape)
{
    LPGAMMATABLE sRGB = Build_sRGBGamma();
    int i;

    if (Shape ->nEntries != 1024) { 
        printf("because wrong sizes (%d != 1024), ", Shape -> nEntries); 
        return 0;
    }


    for (i=0; i < Shape -> nEntries; i++) {
        double nErr = Shape ->GammaTable[i] - sRGB ->GammaTable[i];

        if (fabs(nErr) > 1.0) {
                    int j;
                    printf("because %x != %x on index %d\n", Shape ->GammaTable[i], sRGB ->GammaTable[i], i);
                    printf("table dump follows:\n");
                    for (j=0; j < 10; j++)
                            printf("%d) %X\n", j, Shape ->GammaTable[j]);
                    printf("\nso, ");

                    return 0;
        }
    }

    cmsFreeGamma(sRGB);
    return 1;
}


static
int GetInfoTest(void)
{
    cmsHPROFILE hProfile;
    cmsCIEXYZ WhitePoint;
    cmsCIEXYZTRIPLE Primaries;
    const char* Product;
    LPGAMMATABLE Shapes[3];


    printf("Testing profile decoding (sRGB)");
    hProfile = cmsOpenProfileFromFile("sRGB Color Space Profile.icm", "rb");
    cmsTakeMediaWhitePoint(&WhitePoint, hProfile);

    Dot();
    if (!CheckXYZ(&WhitePoint, 0.95045, 1.0, 1.08905)) {
        printf("White point read failed!\n");
        return 0;
    }

    Dot();
    cmsTakeColorants(&Primaries, hProfile);

    if (!CheckXYZ(&Primaries.Red, 0.43607, 0.22249, 0.01392)) {
        printf("Red colorant failed!\n");
        return 0;
    }

    if (!CheckXYZ(&Primaries.Green, 0.38515,0.71617, 0.09708)) {
        printf("Green colorant failed!\n");
        return 0;
    }

    if (!CheckXYZ(&Primaries.Blue, 0.14307, 0.06061, 0.71410)) {
        printf("Blue colorant failed!\n");
        return 0;
    }

    Dot();
    Product = cmsTakeProductName(hProfile);
    if (strcmp(Product, "IEC 61966-2.1 Default RGB colour space - sRGB") != 0) {        
    printf("Product name mismatch!\n");
    }

    Dot();
    Shapes[0] = cmsReadICCGamma(hProfile, icSigRedTRCTag);
    Shapes[1] = cmsReadICCGamma(hProfile, icSigGreenTRCTag);
    Shapes[2] = cmsReadICCGamma(hProfile, icSigBlueTRCTag);

    if (!Check_sRGBGamma(Shapes[0]) ||
        !Check_sRGBGamma(Shapes[1]) ||
        !Check_sRGBGamma(Shapes[2])) {
            printf("Gamma curves mismatch!\n");
            return 0;
    }


    cmsFreeGammaTriple(Shapes);
    

    Dot();
    cmsCloseProfile(hProfile);
    printf("pass.\n");
    return 1;

}
static
int Test_sRGB(void)
{
       cmsHPROFILE In1, In2, Out1, Out2;
       cmsHTRANSFORM xform1, xform2;
       int nMaxErr;

       printf("Testing sRGB built-in space");
       
       In1   = cmsOpenProfileFromFile("sRGB Color Space Profile.icm", "rb");
       Out1  = cmsCreateXYZProfile();

       In2   = cmsCreate_sRGBProfile();
       Out2  = cmsCreateXYZProfile();

       xform1 = cmsCreateTransform(In1, TYPE_RGBA_16, Out1, TYPE_XYZA_16, 0, cmsFLAGS_NOTPRECALC);
       xform2 = cmsCreateTransform(In2, TYPE_RGBA_16, Out2, TYPE_XYZA_16, 0, cmsFLAGS_NOTPRECALC);

       nMaxErr = CompareTransforms(xform1, xform2, 31, TRUE, FALSE);

       cmsDeleteTransform(xform1);
       cmsCloseProfile(In1);
       cmsCloseProfile(Out1);

       cmsDeleteTransform(xform2);
       cmsCloseProfile(In2);
       cmsCloseProfile(Out2);

       return nMaxErr;

}

static
int RealProfilesTest(void)
{
       cmsHPROFILE In1, In2, Out1, Out2;
       cmsHTRANSFORM xform1, xform2;
       int nMaxErr;

       printf("Using  two real profiles");

       // sRGB is a simpler, public domain XYZ PCS profile
       // sRGBSpac comes with Win95 Platform SDK, in the public domain.
       //            (not latest revisions)

       // Using LAB identity as output profile, I'm forcing an
       // implicit XYZ => L*a*b conversion in xform1.
       // xform2 is 8 bits - LUT based, and PCS is L*a*b

       In1   = cmsOpenProfileFromFile("sRGB Color Space Profile.icm", "rb");
       Out1  = cmsCreateXYZProfile();

       In2   = cmsOpenProfileFromFile("sRGBSpac.icm", "rb");
       Out2  = cmsCreateXYZProfile();

      
       // Since LUT is 8-bits width,
       xform1 = cmsCreateTransform(In1, TYPE_RGBA_16, Out1, TYPE_XYZA_16, 0, cmsFLAGS_NOTPRECALC|cmsFLAGS_MATRIXINPUT);
       xform2 = cmsCreateTransform(In2, TYPE_RGBA_16, Out2, TYPE_XYZA_16, 0, cmsFLAGS_NOTPRECALC);

       nMaxErr = CompareTransforms(xform1, xform2, 31, FALSE, FALSE);

       printf("pass\n");

       cmsDeleteTransform(xform1);
       cmsCloseProfile(In1);
       cmsCloseProfile(Out1);

       cmsDeleteTransform(xform2);
       cmsCloseProfile(In2);
       cmsCloseProfile(Out2);

       return nMaxErr;
}



// ---------------------------------------------------------


static
int TestPreview(void)
{
       cmsHPROFILE In, Out, Proof;
       cmsHTRANSFORM xform;
       int nMaxErr;

       printf("Testing preview");

       In    = cmsCreateLabProfile(NULL);
       Out   = cmsCreateLabProfile(NULL);
       Proof = cmsCreateLabProfile(NULL);

       xform = cmsCreateProofingTransform(In, TYPE_LABA_16, Out, TYPE_LABA_16, Proof, 0, 0, cmsFLAGS_SOFTPROOFING);

       nMaxErr = TestFullSpectrum(xform, 31, 0x1000L);

       cmsDeleteTransform(xform);
       cmsCloseProfile(In);
       cmsCloseProfile(Out);
       cmsCloseProfile(Proof);
       printf("\n");

       return nMaxErr;
}


// Check induced error on multiprofile transforms

static
int TestMultiprofile(void)
{

    cmsHPROFILE hsRGB, hXYZ, hLab;
    cmsHTRANSFORM hXForm;
    cmsHPROFILE Profiles[10];
    int nMaxErr;

    hsRGB = cmsCreate_sRGBProfile();
    hLab = cmsCreateLabProfile(NULL);
    hXYZ = cmsCreateXYZProfile();

    Profiles[0] = hsRGB;
    Profiles[1] = hLab;
    Profiles[2] = hsRGB;        
    Profiles[3] = hsRGB;        

    Profiles[4] = hLab;
    Profiles[5] = hXYZ;
    Profiles[6] = hsRGB;

    hXForm = cmsCreateMultiprofileTransform(Profiles, 7, TYPE_RGBA_16, TYPE_RGBA_16, INTENT_RELATIVE_COLORIMETRIC, cmsFLAGS_HIGHRESPRECALC);
    
    printf("Testing multiprofile transforms (6 profiles)");

    nMaxErr = TestFullSpectrum(hXForm, 31, 0x1000L);

    cmsDeleteTransform(hXForm);
    cmsCloseProfile(hsRGB);
    cmsCloseProfile(hXYZ);
    cmsCloseProfile(hLab);

    printf("\n");

    return nMaxErr;
}

// Check linearization and other goodies

static
int TestLinearizationDevicelink()
{
    LPGAMMATABLE Transfer[3];
    cmsHPROFILE hLin1, hLin2;
    cmsHTRANSFORM hXForm;
    cmsHPROFILE Profiles[10];
    int nMaxErr;

    printf("Testing linearization devicelink");

    Transfer[0] = cmsBuildGamma(256, 1./2.2);
    Transfer[1] = cmsBuildGamma(256, 1./2.2);
    Transfer[2] = cmsBuildGamma(256, 1./2.2);

    hLin1 = cmsCreateLinearizationDeviceLink(icSigRgbData, Transfer);

    cmsFreeGammaTriple(Transfer);
    


    Transfer[0] = cmsBuildGamma(256, 2.2);
    Transfer[1] = cmsBuildGamma(256, 2.2);
    Transfer[2] = cmsBuildGamma(256, 2.2);

    hLin2 = cmsCreateLinearizationDeviceLink(icSigRgbData, Transfer);

    cmsFreeGammaTriple(Transfer);

    Profiles[0] = hLin1;
    Profiles[1] = hLin2;

    hXForm = cmsCreateMultiprofileTransform(Profiles, 2, TYPE_RGBA_16, TYPE_RGBA_16, INTENT_RELATIVE_COLORIMETRIC, cmsFLAGS_HIGHRESPRECALC);
    if (!hXForm) {

        printf("Error!\n");
        return 1;
    }
    

    nMaxErr = TestFullSpectrum(hXForm, 31, 0x1000L);

    cmsDeleteTransform(hXForm);
    cmsCloseProfile(hLin1);
    cmsCloseProfile(hLin2);

    printf("pass.\n");

    return nMaxErr;
}



static
int TestInkLimiting()
{
    cmsHPROFILE hIL;
    cmsHTRANSFORM hXForm;
    BYTE In[4], Out[4];
    int i, j, k, l;

    printf("Testing ink limiting ");
    
    hIL = cmsCreateInkLimitingDeviceLink(icSigCmykData, 100);

    
    hXForm = cmsCreateTransform(hIL, TYPE_CMYK_8, NULL, TYPE_CMYK_8, INTENT_RELATIVE_COLORIMETRIC, 0);
    if (!hXForm) {

        printf("Error!\n");
        return 0;
    }
    
    for (l=0; l < 255; l += 8) {
        Dot();      
        for (k=0; k < 255; k += 8) 
            for (j=0; j < 255; j += 8) 
                for (i=0; i < 255; i += 8) {

                    // ml: include BYTE type cast to prevent BCB 6 compiler from complaining.
                    In[0] = BYTE(i); In[1] = BYTE(j); In[2] = BYTE(k); In[3] = BYTE(l);

                    cmsDoTransform(hXForm, In, Out, 1);

                    if (Out[0] + Out[1] + Out[2] + Out[3] > 0x100) {
            
                        printf("Failed!\n");
                        return 0;   
                    }
        }
    }

    cmsDeleteTransform(hXForm);
    cmsCloseProfile(hIL);
    printf("pass.\n");

    return 1;
}


static
int TestDeviceLinkGeneration()
{
    cmsHTRANSFORM hXForm, hIdentity;
    cmsHPROFILE hDevLink, hsRGB;
    int nMaxErr;


    printf("Checking devicelink generation");

    hsRGB     = cmsOpenProfileFromFile("sRGB Color Space Profile.icm", "r");
    hIdentity = cmsCreateTransform(hsRGB, TYPE_RGBA_16, hsRGB, TYPE_RGBA_16, INTENT_RELATIVE_COLORIMETRIC, 0);
    hDevLink  = cmsTransform2DeviceLink(hIdentity, 0);
    _cmsSaveProfile(hDevLink, "devicelink.icm");

    cmsCloseProfile(hDevLink);
    cmsCloseProfile(hsRGB);
    cmsDeleteTransform(hIdentity);

    hDevLink = cmsOpenProfileFromFile("devicelink.icm", "r");
    hXForm   = cmsCreateTransform(hDevLink, TYPE_RGBA_16, NULL, TYPE_RGBA_16, INTENT_RELATIVE_COLORIMETRIC, 0); 
    nMaxErr  = TestFullSpectrum(hXForm, 31, 0x1000L);

    cmsDeleteTransform(hXForm); 
    cmsCloseProfile(hDevLink);
    
    printf("\n");
    unlink("devicelink.icm");

    return nMaxErr;
}




static
void CheckPlanar(void)
{
    cmsHTRANSFORM xform;
    cmsHPROFILE hsRGB;
    int i;
    BYTE Out[12];
    BYTE Bmp[] = { 0x00, 0x10, 0x20, 0x30,   // R Plane  
                   0x00, 0x10, 0x20, 0x30,   // G Plane
                   0x00, 0x10, 0x20, 0x30 }; // B Plane
    

    
            hsRGB = cmsCreate_sRGBProfile();
            xform = cmsCreateTransform(hsRGB, TYPE_RGB_8_PLANAR, 
                                       hsRGB, TYPE_RGB_8_PLANAR, 
                                       INTENT_PERCEPTUAL, cmsFLAGS_NOTPRECALC);

            cmsDoTransform(xform, Bmp, Out, 4);

            for (i=0; i < 12; i += 3) {
                    printf("RGB=(%x, %x, %x)\n", Out[i+0], Out[i+1], Out[i+2]);
            }

        cmsDeleteTransform(xform);
        cmsCloseProfile(hsRGB);
}

#ifdef ICM_COMPARATIVE
#ifndef NON_WINDOWS

static
void CompareWithICM(void)
{

    HTRANSFORM hICMxform;
    HPROFILE   hICMProfileFrom, hICMProfileTo;
    LOGCOLORSPACE LogColorSpace;
    COLOR In, Out;
    int r, g, b;
    PROFILE Profile;
    clock_t atime;
    double seconds, diff;
    cmsHPROFILE hlcmsProfileIn, hlcmsProfileOut;
    cmsHTRANSFORM hlcmsxform;


    printf("\n\nComparative with MS-Windows ICM:\n");
    

    Profile.dwType = PROFILE_FILENAME;
    Profile.pProfileData = "sRGBSpac.ICM";
    Profile.cbDataSize   = strlen("sRGBSpac.ICM");

    hICMProfileFrom = OpenColorProfile(&Profile, PROFILE_READ, FILE_SHARE_READ, OPEN_EXISTING);

    Profile.pProfileData = "sRGBSpac.ICM";
    Profile.cbDataSize   = strlen("sRGBSpac.ICM");
    hICMProfileTo   = OpenColorProfile(&Profile, PROFILE_READ, FILE_SHARE_READ, OPEN_EXISTING);

    ZeroMemory(&LogColorSpace, sizeof(LOGCOLORSPACE));

    LogColorSpace.lcsSignature = LCS_SIGNATURE;
    LogColorSpace.lcsVersion   = 0x400;
    LogColorSpace.lcsCSType    = LCS_CALIBRATED_RGB;
    strcpy(LogColorSpace.lcsFilename, "sRGBSpac.ICM");

    hICMxform = CreateColorTransform(&LogColorSpace, hICMProfileTo, NULL, BEST_MODE);

    printf("Windows ICM is transforming full spectrum...");

    atime = clock();

    for (r=0; r < 255; r++)
        for (g=0; g < 255; g++)
            for (b=0; b < 255; b++) {

        In.rgb.red   = (r << 8) | r;
        In.rgb.green = (g << 8) | g;
        In.rgb.blue  = (b << 8) | b;

        TranslateColors( hICMxform, &In, 1, COLOR_RGB, &Out, COLOR_RGB);
    }

    diff = clock() - atime;
    seconds = (double) diff / CLK_TCK;


    printf("done. [%d tics, %g sec.]\n", (int) diff, seconds);
  
    CloseColorProfile(hICMProfileFrom);
    CloseColorProfile(hICMProfileTo);
    DeleteColorTransform(hICMxform);

    hlcmsProfileIn  = cmsOpenProfileFromFile("sRGBSpac.ICM", "r");
    hlcmsProfileOut = cmsOpenProfileFromFile("sRGBSpac.ICM", "r");

    hlcmsxform  = cmsCreateTransform(hlcmsProfileIn, TYPE_RGB_16, hlcmsProfileOut, TYPE_RGB_16, INTENT_PERCEPTUAL, 0);

    printf("lcms is transforming full spectrum...");

    atime = clock();

    for (r=0; r < 255; r++)
        for (g=0; g < 255; g++)
            for (b=0; b < 255; b++) {

        In.rgb.red   = (r << 8) | r;
        In.rgb.green = (g << 8) | g;
        In.rgb.blue  = (b << 8) | b;

        cmsDoTransform(hlcmsxform, &In.rgb, &Out.rgb, 1);
    }

    diff = clock() - atime;
    seconds = (double) diff / CLK_TCK;

    printf("done. [%d tics, %g sec.]\n", (int) diff, seconds);

    cmsDeleteTransform(hlcmsxform);
    cmsCloseProfile(hlcmsProfileIn);
    cmsCloseProfile(hlcmsProfileOut);

}

#endif
#endif


#ifdef CHECK_SPEED
static
void SpeedTest(void)
{

    int r, g, b;
    clock_t atime;
    double seconds, diff;
    cmsHPROFILE hlcmsProfileIn, hlcmsProfileOut;
    cmsHTRANSFORM hlcmsxform;
    COLOR In, Out;
   
   
    printf("\n\nRaw speed check:\n");

    hlcmsProfileIn  = cmsOpenProfileFromFile("sRGB Color Space Profile.ICM", "r");
    hlcmsProfileOut = cmsOpenProfileFromFile("sRGB Color Space Profile.ICM", "r");

    hlcmsxform  = cmsCreateTransform(hlcmsProfileIn, TYPE_RGB_16, hlcmsProfileOut, TYPE_RGB_16, INTENT_PERCEPTUAL, 0);

    printf("lcms is transforming full spectrum...");

    atime = clock();

    for (r=0; r < 255; r++)
        for (g=0; g < 255; g++)
            for (b=0; b < 255; b++) {

        In.rgb.red   = (r << 8) | r;
        In.rgb.green = (g << 8) | g;
        In.rgb.blue  = (b << 8) | b;

        cmsDoTransform(hlcmsxform, &In.rgb, &Out.rgb, 1);
    }

    diff = clock() - atime;
    seconds = (double) diff / CLK_TCK;
    
    printf("done.\n[%d tics, %g sec, %g Mb/sec.]\n", (int) diff, seconds, 32. / seconds );

    cmsDeleteTransform(hlcmsxform);
    cmsCloseProfile(hlcmsProfileIn);
    cmsCloseProfile(hlcmsProfileOut);

}

#endif




int main(int argc, char *argv[])
{
       int lExhaustive = 0;   
                       

       printf("little cms testbed. Ver 1.10 [build %s %s]\n\n", __DATE__, __TIME__);
      
#ifndef LCMS_DLL

       if (!CheckEndianess()) return 1;
       if (!CheckSwab()) return 1;

       TestFixedPoint();
     
       if (!TestFixedScaling()) return 1;          
       if (!TestJointCurves()) return 1;
       if (!TestReversingOfCurves()) return 1;
       if (!TestLinearInterpolation(lExhaustive)) return 1;
       if (!TestReverseLinearInterpolation()) return 1;

       

       if (!Test3D()) return 1;
       if (!TestMatrixCreation()) return 1;
       if (!GetInfoTest()) return 1;

#endif

       if (!Test_sRGB()) return 1;
       if (!RealProfilesTest()) return 1;
       if (!TestInducedError(TYPE_LABA_16)) return 1;

       if (!TestPreview()) return 1;
       if (!TestMultiprofile()) return 1;
       if (!TestLinearizationDevicelink()) return 1;
       if (!TestInkLimiting()) return 1;
       if (!TestDeviceLinkGeneration()) return 1;

#ifdef ICM_COMPARATIVE
#ifndef NON_WINDOWS
       CompareWithICM();
#endif
#endif

#ifdef CHECK_SPEED    
       SpeedTest();
#endif

       printf("\nSuccess.\n");

       return 0;

}


