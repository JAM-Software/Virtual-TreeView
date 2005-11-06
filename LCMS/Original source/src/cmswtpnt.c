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


// Conversions

void LCMSEXPORT cmsXYZ2xyY(LPcmsCIExyY Dest, const LPcmsCIEXYZ Source)
{
       double ISum;

       ISum = 1./(Source -> X + Source -> Y + Source -> Z);

       Dest -> x = (Source -> X) * ISum;
       Dest -> y = (Source -> Y) * ISum;
       Dest -> Y = Source -> Y;
}


void LCMSEXPORT cmsxyY2XYZ(LPcmsCIEXYZ Dest, const LPcmsCIExyY Source)
{

        Dest -> X = (Source -> x / Source -> y) * Source -> Y;
        Dest -> Y = Source -> Y;
        Dest -> Z = ((1 - Source -> x - Source -> y) / Source -> y) * Source -> Y;
}



// Obtains WhitePoint from Temperature

BOOL LCMSEXPORT cmsWhitePointFromTemp(int TempK, LPcmsCIExyY WhitePoint)
{
       double x, y;
       double T, T2, T3;
       // double M1, M2;


       // No optimization provided.

       T = TempK;
       T2 = T*T;            // Square
       T3 = T2*T;           // Cube

       // For correlated color temperature (T) between 4000K and 7000K:

       if (T >= 4000. && T <= 7000.)
       {
              x = -4.6070*(1E9/T3) + 2.9678*(1E6/T2) + 0.09911*(1E3/T) + 0.244063;
       }
       else
              // or for correlated color temperature (T) between 7000K and 25000K:

       if (T > 7000.0 && T <= 25000.0)
       {
              x = -2.0064*(1E9/T3) + 1.9018*(1E6/T2) + 0.24748*(1E3/T) + 0.237040;
       }
       else {
              cmsSignalError(LCMS_ERRC_ABORTED, "cmsWhitePointFromTemp: invalid temp");
              return FALSE;
              }

       // Obtain y(x)

       y = -3.000*(x*x) + 2.870*x - 0.275;

       // wave factors (not used, but here for futures extensions)

       // M1 = (-1.3515 - 1.7703*x + 5.9114 *y)/(0.0241 + 0.2562*x - 0.7341*y);
       // M2 = (0.0300 - 31.4424*x + 30.0717*y)/(0.0241 + 0.2562*x - 0.7341*y);



       // Fill WhitePoint struct

       WhitePoint -> x = x;
       WhitePoint -> y = y;
       WhitePoint -> Y = 1.0;

       return TRUE;
}

// Build a White point, primary chromas transfer matrix from RGB to CIE XYZ
// This is just an approximation, I am not handling all the non-linear
// aspects of the RGB to XYZ process, and assumming that the gamma correction
// has transitive property in the tranformation chain.
//
// the alghoritm:
//
//            - First I build the absolute conversion matrix using
//              primaries in XYZ. This matrix is next inverted
//            - Then I eval the source white point across this matrix
//              obtaining the coeficients of the transformation
//            - Then, I apply these coeficients to the original matrix


BOOL LCMSEXPORT cmsBuildRGB2XYZtransferMatrix(LPMAT3 r, LPcmsCIExyY WhitePt,
                                            LPcmsCIExyYTRIPLE Primrs)
{
        VEC3 WhitePoint, Coef;
        MAT3 Result, Primaries;
        double xn, yn;
        double xr, yr;
        double xg, yg;
        double xb, yb;


        xn = WhitePt -> x;
        yn = WhitePt -> y;
        xr = Primrs -> Red.x;
        yr = Primrs -> Red.y;
        xg = Primrs -> Green.x;
        yg = Primrs -> Green.y;
        xb = Primrs -> Blue.x;
        yb = Primrs -> Blue.y;


        // Build Primaries matrix

        VEC3init(&Primaries.v[0], xr,        xg,         xb);
        VEC3init(&Primaries.v[1], yr,        yg,         yb);
        VEC3init(&Primaries.v[2], (1-xr-yr), (1-xg-yg),  (1-xb-yb));


        // Result = Primaries ^ (-1) inverse matrix

        if (!MAT3inverse(&Primaries, &Result))
                        return FALSE;


        VEC3init(&WhitePoint, xn/yn, 1.0, (1.0-xn-yn)/yn);

        // Across inverse primaries ...

        MAT3eval(&Coef, &Result, &WhitePoint);

        // Give us the Coefs, then I build transformation matrix

        VEC3init(&r -> v[0], Coef.n[VX]*xr,          Coef.n[VY]*xg,          Coef.n[VZ]*xb);
        VEC3init(&r -> v[1], Coef.n[VX]*yr,          Coef.n[VY]*yg,          Coef.n[VZ]*yb);
        VEC3init(&r -> v[2], Coef.n[VX]*(1.0-xr-yr), Coef.n[VY]*(1.0-xg-yg), Coef.n[VZ]*(1.0-xb-yb));
       

        return TRUE;
}



/*
 TODO: Use CMCCAT2000

     0.7982  0.3389 -0.1371 
    -0.5918  1.5512  0.0406 
     0.0008  0.0239  0.9753 

 */

// This procedure implements the Lam & Rigg Bradford chromatic adaptation.

static
void BradfordLamRiggChromaticAdaptation(LPMAT3 Conversion,
                                        LPcmsCIEXYZ SourceWhitePoint,
                                        LPcmsCIEXYZ DestWhitePoint)

{
       MAT3 LamRigg   = {{ // Bradford matrix
                     {{  0.8951,  0.2664, -0.1614 }},
                     {{ -0.7502,  1.7135,  0.0367 }},
                     {{  0.0389, -0.0685,  1.0296 }}
                     }};
       MAT3 LamRigg_1 = {{ // |Bradford|^^-1
                     {{  0.9870, -0.1471,  0.1600 }},
                     {{  0.4323,  0.5184,  0.0493 }},
                     {{ -0.0085,  0.0400,  0.9685 }},
                     }};

        VEC3 ConeSourceXYZ, ConeSourceRGB;
        VEC3 ConeDestXYZ, ConeDestRGB;
        MAT3 Cone, Tmp;

        VEC3init(&ConeSourceXYZ, SourceWhitePoint -> X,
                                 SourceWhitePoint -> Y,
                                 SourceWhitePoint -> Z);

        VEC3init(&ConeDestXYZ,   DestWhitePoint -> X,
                                 DestWhitePoint -> Y,
                                 DestWhitePoint -> Z);

        MAT3eval(&ConeSourceRGB, &LamRigg, &ConeSourceXYZ);
        MAT3eval(&ConeDestRGB,   &LamRigg, &ConeDestXYZ);

        // Build matrix

        VEC3init(&Cone.v[0], ConeDestRGB.n[0]/ConeSourceRGB.n[0],    0.0,  0.0);
        VEC3init(&Cone.v[1], 0.0,   ConeDestRGB.n[1]/ConeSourceRGB.n[1],   0.0);
        VEC3init(&Cone.v[2], 0.0,   0.0,   ConeDestRGB.n[2]/ConeSourceRGB.n[2]);


        // Normalize
        MAT3per(&Tmp, &Cone, &LamRigg);
        MAT3per(Conversion, &LamRigg_1, &Tmp);

}


BOOL cmsAdaptationMatrix(LPMAT3 r, LPcmsCIEXYZ FromIll, LPcmsCIEXYZ ToIll)
{
    
        BradfordLamRiggChromaticAdaptation(r, FromIll, ToIll);
        return TRUE;

}


BOOL cmsAdaptMatrixToD50(LPMAT3 r, LPcmsCIExyY SourceWhitePt)
{
        cmsCIEXYZ Dn;      
        MAT3 Bradford;
        MAT3 Tmp;

        cmsxyY2XYZ(&Dn, SourceWhitePt);

        BradfordLamRiggChromaticAdaptation(&Bradford, &Dn, cmsD50_XYZ());
        Tmp = *r;
        MAT3per(r, &Bradford, &Tmp);

        return TRUE;
}

BOOL cmsAdaptMatrixFromD50(LPMAT3 r, LPcmsCIExyY DestWhitePt)
{
        cmsCIEXYZ Dn;       
        MAT3 Bradford;
        MAT3 Tmp;

        cmsxyY2XYZ(&Dn, DestWhitePt);

        BradfordLamRiggChromaticAdaptation(&Bradford, cmsD50_XYZ(), &Dn);
        Tmp = *r;
        MAT3per(r, &Bradford, &Tmp);

        return TRUE;
}


// Adapts a color to a given illuminant. Original color is expected to have
// a SourceWhitePt white point. (Currently uses a von-kries simplification
// of Bradford transform).

BOOL LCMSEXPORT cmsAdaptToIlluminant(LPcmsCIEXYZ Result, LPcmsCIEXYZ SourceWhitePt, LPcmsCIEXYZ Illuminant, LPcmsCIEXYZ Value)
{
        MAT3 Bradford;
        VEC3 In, Out;
    
        BradfordLamRiggChromaticAdaptation(&Bradford, SourceWhitePt, Illuminant);

        VEC3init(&In, Value -> X, Value -> Y, Value -> Z);
        MAT3eval(&Out, &Bradford, &In);

        Result -> X = Out.n[0];
        Result -> Y = Out.n[1];
        Result -> Z = Out.n[2];

        return TRUE;
}



typedef struct {

    double mirek;  // temp (in microreciprocal kelvin) 
    double ut;     // u coord of intersection w/ blackbody locus  
    double vt;     // v coord of intersection w/ blackbody locus 
    double tt;     // slope of ISOTEMPERATURE. line 

    } ISOTEMPERATURE,FAR* LPISOTEMPERATURE;

static ISOTEMPERATURE isotempdata[] = {
//  {Mirek, Ut,       Vt,      Tt      } 
    {0,     0.18006,  0.26352,  -0.24341},
    {10,    0.18066,  0.26589,  -0.25479},
    {20,    0.18133,  0.26846,  -0.26876},
    {30,    0.18208,  0.27119,  -0.28539},
    {40,    0.18293,  0.27407,  -0.30470},
    {50,    0.18388,  0.27709,  -0.32675},
    {60,    0.18494,  0.28021,  -0.35156},
    {70,    0.18611,  0.28342,  -0.37915},
    {80,    0.18740,  0.28668,  -0.40955},
    {90,    0.18880,  0.28997,  -0.44278},
    {100,   0.19032,  0.29326,  -0.47888},
    {125,   0.19462,  0.30141,  -0.58204},
    {150,   0.19962,  0.30921,  -0.70471},
    {175,   0.20525,  0.31647,  -0.84901},
    {200,   0.21142,  0.32312,  -1.0182 },
    {225,   0.21807,  0.32909,  -1.2168 },
    {250,   0.22511,  0.33439,  -1.4512 },
    {275,   0.23247,  0.33904,  -1.7298 },
    {300,   0.24010,  0.34308,  -2.0637 },
    {325,   0.24702,  0.34655,  -2.4681 },
    {350,   0.25591,  0.34951,  -2.9641 },
    {375,   0.26400,  0.35200,  -3.5814 },
    {400,   0.27218,  0.35407,  -4.3633 },
    {425,   0.28039,  0.35577,  -5.3762 },
    {450,   0.28863,  0.35714,  -6.7262 },
    {475,   0.29685,  0.35823,  -8.5955 },
    {500,   0.30505,  0.35907,  -11.324 },
    {525,   0.31320,  0.35968,  -15.628 },
    {550,   0.32129,  0.36011,  -23.325 },
    {575,   0.32931,  0.36038,  -40.770 },
    {600,   0.33724,  0.36051, -116.45  }
};

#define NISO sizeof(isotempdata)/sizeof(ISOTEMPERATURE)


// Robertson's method

static 
double Robertson(LPcmsCIExyY v)
{
    int j;
    double us,vs;
    double uj,vj,tj,di,dj,mi,mj;
    double Tc = -1, xs, ys;

    di = mi = 0;
    xs = v -> x;
    ys = v -> y;

    // convert (x,y) to CIE 1960 (u,v) 

    us = (2*xs) / (-xs + 6*ys + 1.5);
    vs = (3*ys) / (-xs + 6*ys + 1.5);


    for (j=0; j < NISO; j++) {

        uj = isotempdata[j].ut;
        vj = isotempdata[j].vt;
        tj = isotempdata[j].tt;
        mj = isotempdata[j].mirek;

        dj = ((vs - vj) - tj * (us - uj)) / sqrt(1 + tj*tj);



        if ((j!=0) && (di/dj < 0.0)) {
            Tc = 1000000.0 / (mi + (di / (di - dj)) * (mj - mi));
            break;
        }

        di = dj;
        mi = mj;
    }


    if (j == NISO) return -1;   
    return Tc;
}



static
BOOL InRange(LPcmsCIExyY a, LPcmsCIExyY b, double tolerance)
{
       double dist_x, dist_y;

       dist_x = fabs(a->x - b->x);
       dist_y = fabs(a->y - b->y);

       return (tolerance >= dist_x * dist_x + dist_y * dist_y);

}


typedef struct {
                char Name[30];
                cmsCIExyY Val;

              } WHITEPOINTS,FAR *LPWHITEPOINTS;

static
int FromD40toD150(LPWHITEPOINTS pts)
{
       int i, n;

       n = 0;
       for (i=40; i < 150; i ++)
       {
              sprintf(pts[n].Name, "D%d", i);
              cmsWhitePointFromTemp((int) (i*100.0), &pts[n].Val);
              n++;
       }

   return n;
}


void _cmsIdentifyWhitePoint(char *Buffer, LPcmsCIEXYZ WhitePt)
{
       int i, n;
       cmsCIExyY Val;
       double T;
       WHITEPOINTS SomeIlluminants[140] = {

                                   {"CIE illuminant A", {0.4476, 0.4074, 1.0}},
                                   {"CIE illuminant C", {0.3101, 0.3162, 1.0}},
                                   {"D65 (daylight)",   {0.3127, 0.3291, 1.0}},
                                   };

              n = FromD40toD150(&SomeIlluminants[3]) + 3;

              cmsXYZ2xyY(&Val, WhitePt);

              Val.Y = 1.;
              for (i=0; i < n; i++)
              {

                            if (InRange(&Val, &SomeIlluminants[i].Val, 0.000005))
                            {
                                strcpy(Buffer, "WhitePoint : ");
                                strcat(Buffer, SomeIlluminants[i].Name);                                
                                return;
                            }
              }

              T = Robertson(&Val);

              if (T > 0) 
                sprintf(Buffer, "White point near %dK", (int) T);                                                     
              else
              {
              sprintf(Buffer, "Unknown white point (X:%1.2g, Y:%1.2g, Z:%1.2g)",
                                          WhitePt -> X, WhitePt -> Y, WhitePt -> Z);
              
              }
              
}

