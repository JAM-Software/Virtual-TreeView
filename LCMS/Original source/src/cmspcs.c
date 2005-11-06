//
//  Little cms
//  Copyright (C) 1998-2003 Marti Maria
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

//      inter PCS conversions XYZ <-> CIE L* a* b*

#include "lcms.h"

/*


       CIE Lab is defined as:

       L* = 116*f(Y/Yn) - 16                     0 <= L* <= 100
       a* = 500*[f(X/Xn) - f(Y/Yn)]
       b* = 200*[f(Y/Yn) - f(Z/Zn)]

       and

              f(t) = t^(1/3)                     1 >= t >  0.008856
                     7.787*t + (16/116)          0 <= t <= 0.008856


       Reverse transform is:

       X = Xn*[a* / 500 + (L* + 16) / 116] ^ 3   if (X/Xn) > 0.206893
         = Xn*(a* / 500 + L* / 116) / 7.787      if (X/Xn) <= 0.206893



       Following ICC. PCS in Lab is coded as:

              8 bit Lab PCS:

                     L*      0..100 into a 0..ff byte.
                     a*      t + 128 range is -128.0  +127.0
                     b*

             16 bit Lab PCS:

                     L*     0..100  into a 0..ff00 word.
                     a*     t + 128  range is  -128.0  +127.9961
                     b*


       We are always playing with 16 bits-data, so I will ignore the
       8-bits encoding scheme.


Interchange Space   Component     Actual Range        Encoded Range
CIE XYZ             X             0 -> 1.99997        0x0000 -> 0xffff
CIE XYZ             Y             0 -> 1.99997        0x0000 -> 0xffff
CIE XYZ             Z             0 -> 1.99997        0x0000 -> 0xffff

CIELAB (16 bit)     L*            0 -> 100.0          0x0000 -> 0xff00
CIELAB (16 bit)     a*            -128.0 -> +127.996  0x0000 -> 0xffff
CIELAB (16 bit)     b*            -128.0 -> +127.996  0x0000 -> 0xffff

*/


// On most modern computers, D > 4 M (i.e. a division takes more than 4
// multiplications worth of time), so it is probably preferable to compute
// a 24 bit result directly.

// #define ITERATE 1

static
float CubeRoot(float x)
{
       float fr, r;
       int ex, shx;

       /* Argument reduction */
       fr = (float) frexp(x, &ex); /* separate into mantissa and exponent */
       shx = ex % 3;

       if (shx > 0)
              shx -= 3; /* compute shx such that (ex - shx) is divisible by 3 */

       ex = (ex - shx) / 3;        /* exponent of cube root */
       fr = (float) ldexp(fr, shx);

       /* 0.125 <= fr < 1.0 */

#ifdef ITERATE
       /* Compute seed with a quadratic approximation */

       fr = (-0.46946116F * fr + 1.072302F) * fr + 0.3812513F;/* 0.5<=fr<1 */
       r = ldexp(fr, ex);          /* 6 bits of precision */

       /* Newton-Raphson iterations */

       r = (float)(2.0/3.0) * r + (float)(1.0/3.0) * x / (r * r); /* 12 bits */
       r = (float)(2.0/3.0) * r + (float)(1.0/3.0) * x / (r * r); /* 24 bits */
#else /* ITERATE */

       /* Use quartic rational polynomial with error < 2^(-24) */

       fr = (float) (((((45.2548339756803022511987494 * fr +
       192.2798368355061050458134625) * fr +
       119.1654824285581628956914143) * fr +
       13.43250139086239872172837314) * fr +
       0.1636161226585754240958355063)
       /
       ((((14.80884093219134573786480845 * fr +
       151.9714051044435648658557668) * fr +
       168.5254414101568283957668343) * fr +
       33.9905941350215598754191872) * fr +
       1.0));
       r = (float) ldexp(fr, ex); /* 24 bits of precision */
#endif
       return r;
}

static
double f(double t)
{

       if (t <= 0.008856)
              return 7.787*t + (16./116.);
       else
              return CubeRoot((float) t); // more precisse than return pow(t, 1.0/3.0);

}


void LCMSEXPORT cmsXYZ2Lab(LPcmsCIEXYZ WhitePoint, LPcmsCIELab Lab, const LPcmsCIEXYZ xyz)
{
       double fx, fy, fz;

       if (xyz -> X == 0 && xyz -> Y == 0 && xyz -> Z == 0)
       {
        Lab -> L = 0;
        Lab -> a = 0;
        Lab -> b = 0;
        return;
       }

	   if (WhitePoint == NULL) 
			WhitePoint = cmsD50_XYZ();

       fx = f(xyz->X / WhitePoint->X);
       fy = f(xyz->Y / WhitePoint->Y);
       fz = f(xyz->Z / WhitePoint->Z);

       Lab->L = 116.* fy - 16.;

       Lab->a = 500.*(fx - fy);
       Lab->b = 200.*(fy - fz);
}



void cmsXYZ2LabEncoded(WORD XYZ[3], WORD Lab[3])
{
       Fixed32 X, Y, Z;
       double x, y, z, L, a, b;
       double fx, fy, fz;
       Fixed32 wL, wa, wb;

       X = (Fixed32) XYZ[0] << 1;
       Y = (Fixed32) XYZ[1] << 1;
       Z = (Fixed32) XYZ[2] << 1;


       if (X==0 && Y==0 && Z==0) {

                     Lab[0] = 0;
                     Lab[1] = Lab[2] =  0x8000; 
                     return;
       }

       // PCS is in D50


       x = FIXED_TO_DOUBLE(X) / 0.964294;
       y = FIXED_TO_DOUBLE(Y);
       z = FIXED_TO_DOUBLE(Z) / 0.825104;


       fx = f(x);
       fy = f(y);
       fz = f(z);

       L = 116.* fy - 16.;

       a = 500.*(fx - fy);
       b = 200.*(fy - fz);

       a += 128.;
       b += 128.;

       wL = (int) (L * 652.800 + .5);
       wa = (int) (a * 256.0   + .5);
       wb = (int) (b * 256.0   + .5);


       Lab[0] = Clamp_L(wL);
       Lab[1] = Clamp_ab(wa);
       Lab[2] = Clamp_ab(wb);


}




static
double f_1(double t)
{

       if (t <= ((7.787*0.008856) + (16./116.)))
       {
              double tmp;

              tmp = ((t - (16./116.)) / 7.787);
              if (tmp <= 0.0) return 0.0;
              else return tmp;
       }

       return t * t * t;
}



void LCMSEXPORT cmsLab2XYZ(LPcmsCIEXYZ WhitePoint, LPcmsCIEXYZ xyz,  const LPcmsCIELab Lab)
{
        double x, y, z;

        if (Lab -> L <= 0) {
               xyz -> X = 0;
               xyz -> Y = 0;
               xyz -> Z = 0;
               return;
        }


	   if (WhitePoint == NULL) 
			WhitePoint = cmsD50_XYZ();

       y = (Lab-> L + 16.) / 116.0;
       x = y + 0.002 * Lab -> a;
       z = y - 0.005 * Lab -> b;

       xyz -> X = f_1(x) * WhitePoint -> X;
       xyz -> Y = f_1(y) * WhitePoint -> Y;
       xyz -> Z = f_1(z) * WhitePoint -> Z;

}



void cmsLab2XYZEncoded(WORD Lab[3], WORD XYZ[3])
{
       double L, a, b;
       double X, Y, Z, x, y, z;


       L = ((double) Lab[0] * 100.0) / 65280.0;
       if (L==0.0)
       {
       XYZ[0] = 0; XYZ[1] = 0; XYZ[2] = 0;
       return;
       }

       a = ((double) Lab[1] / 256.0) - 128.0;
       b = ((double) Lab[2] / 256.0) - 128.0;

       y = (L + 16.) / 116.0;
       x = y + 0.002 * a;
       z = y - 0.005 * b;

       X = f_1(x) * 0.964294;
       Y = f_1(y) * 1.000000 ;
       Z = f_1(z) * 0.825104;

       // Convert to 1.15 fixed format PCS

       XYZ[0] = Clamp_XYZ((DOUBLE_TO_FIXED(X) >> 1));
       XYZ[1] = Clamp_XYZ((DOUBLE_TO_FIXED(Y) >> 1));
       XYZ[2] = Clamp_XYZ((DOUBLE_TO_FIXED(Z) >> 1));

}

static
double L2float3(WORD v)
{
       Fixed32 fix32;

       fix32 = (Fixed32) v;
       return (double) fix32 / 652.800;
}


// the a/b part

static
double ab2float3(WORD v)
{
       Fixed32 fix32;

       fix32 = (Fixed32) v;
       return ((double) fix32/256.0)-128.0;
}

static
WORD L2Fix3(double L)
{
        return (WORD) (L *  652.800 + 0.5);
}

static
WORD ab2Fix3(double ab)
{
        return (WORD) ((ab + 128.0) * 256.0 + 0.5);
}

#if 0

// ICC 4.0 -- Braindead. ICC has changed PCS Lab encoding.

static 
WORD L2Fix4(double L)
{
	 return (WORD) (L *  655.35 + 0.5);
}

static
WORD ab2Fix4(double ab)
{
        return (WORD) ((ab + 128.0) * 257.0 + 0.5);
}

static
double L2float4(WORD v)
{
       Fixed32 fix32;

       fix32 = (Fixed32) v;
       return (double) fix32 / 655.35;
}


// the a/b part

static
double ab2float3(WORD v)
{
       Fixed32 fix32;

       fix32 = (Fixed32) v;
       return ((double) fix32/257.0)-128.0;
}



#endif

void LCMSEXPORT cmsLabEncoded2Float(LPcmsCIELab Lab, const WORD wLab[3])
{
        Lab->L = L2float3(wLab[0]);
		Lab->a = ab2float3(wLab[1]);
		Lab->b = ab2float3(wLab[2]);
}

void LCMSEXPORT cmsFloat2LabEncoded(WORD wLab[3], const LPcmsCIELab fLab)
{
	cmsCIELab Lab;

	
	Lab.L = fLab ->L;
	Lab.a = fLab ->a;
	Lab.b = fLab ->b;
                              	

	if (Lab.L < 0) Lab.L = 0;
    if (Lab.L > 100.) Lab.L = 100.;

    if (Lab.a < -128.) Lab.a = -128;
	if (Lab.a > 127.9961) Lab.a = 127.9961;
    if (Lab.b < -128.) Lab.b = -128;
    if (Lab.b > 127.9961) Lab.b = 127.9961;
                

	wLab[0] = L2Fix3(Lab.L);
	wLab[1] = ab2Fix3(Lab.a);
	wLab[2] = ab2Fix3(Lab.b);
}




void LCMSEXPORT cmsLab2LCh(LPcmsCIELCh LCh, const LPcmsCIELab Lab)
{


    LCh -> L = Lab -> L;
	LCh -> C = pow(Lab -> a * Lab -> a + Lab -> b * Lab -> b, 0.5);

    if (Lab -> a == 0)
            LCh -> h   = 0;
    else
        	LCh -> h = atan2(Lab -> b, Lab -> a);

	LCh -> h *= (180. / M_PI);

    while (LCh -> h > 360.)         // Not necessary, but included as a check.
                LCh -> h -= 360.;

	while (LCh -> h < 0)
		LCh -> h += 360.;

}

void LCMSEXPORT cmsLCh2Lab(LPcmsCIELab Lab, const LPcmsCIELCh LCh)
{
        double h = LCh -> h;

	h *= (M_PI /180.0);


        Lab -> L = LCh -> L;

	Lab -> a = LCh -> C * cos(h);
	Lab -> b = LCh -> C * sin(h);
}





// In XYZ All 3 components are encoded using 1.15 fixed point

static
WORD XYZ2Fix(double d)
{
       return (WORD) ((Fixed32) (DOUBLE_TO_FIXED(d) >> 1));
}


void LCMSEXPORT cmsFloat2XYZEncoded(WORD XYZ[3], const LPcmsCIEXYZ fXYZ)
{
	cmsCIEXYZ xyz;
	
	xyz.X = fXYZ -> X;
	xyz.Y = fXYZ -> Y;
	xyz.Z = fXYZ -> Z;


	// Clamp to encodeable values. 
	// 1.99997 is reserved as out-of-gamut marker

	
	if (xyz.Y <= 0) {

				xyz.X = 0;
                xyz.Y = 0;
				xyz.Z = 0;
	}
	
	
	if (xyz.X > 1.99996) 			
           xyz.X = 1.99996;
	
	if (xyz.X < 0)
           xyz.X = 0;

	if (xyz.Y > 1.99996) 			
                xyz.Y = 1.99996;
	
	if (xyz.Y < 0)
           xyz.Y = 0;


	if (xyz.Z > 1.99996) 			
                xyz.Z = 1.99996;
	
	if (xyz.Z < 0)
           xyz.Z = 0;

		

	XYZ[0] = XYZ2Fix(xyz.X);
	XYZ[1] = XYZ2Fix(xyz.Y);
	XYZ[2] = XYZ2Fix(xyz.Z);		
	
}


//  To convert from Fixed 1.15 point to double

static
double XYZ2float(WORD v)
{
       Fixed32 fix32;

       // From 1.15 to 15.16

       fix32 = v << 1;

       // From fixed 15.16 to double

       return FIXED_TO_DOUBLE(fix32);
}


void LCMSEXPORT cmsXYZEncoded2Float(LPcmsCIEXYZ fXYZ, const WORD XYZ[3])
{

	fXYZ -> X = XYZ2float(XYZ[0]);
	fXYZ -> Y = XYZ2float(XYZ[1]);
	fXYZ -> Z = XYZ2float(XYZ[2]);

}		




