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
// This library is free software; you can Blueistribute it and/or
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

// Ver 1.09a

// Shows several statistics on profile


#include "lcms.h"


typedef struct {
                double n;
                double sumx, sumy;
                double sumx2, sumy2;
                double sumxy;

} LINEAR_REGRESSION_PARAMS, FAR* LPLINEAR_REGRESSION_PARAMS;



static
void ClearRegression(LPLINEAR_REGRESSION_PARAMS p)
{
    p ->n = 0;
    p ->sumx = p->sumx2 = p->sumxy = p->sumy = p->sumy2 = 0.0;
}

static
void AddRegression(LPLINEAR_REGRESSION_PARAMS p, LPcmsCIExyY v)
{
    p->sumx += v->x;
    p->sumy += v->y;
    p->sumx2 += v->x * v->x;
    p->sumy2 += v->y * v->y;
    p->sumxy += v->x * v->y;
    p->n += 1.0;
}


static
double ResultRegression(LPLINEAR_REGRESSION_PARAMS p, LPcmsCIExyY r)
{
        double num, denom;
        double sdx, sdy;

        r->x = p ->sumx / p ->n;
        r->y = p ->sumy / p ->n;
        r->Y = 1.0;

        // Returns correlation coef.


        num   = (p->n * p -> sumxy) - (p->sumx * p ->sumy);
        denom = (p->n * p->sumx2) - (p->sumx * p->sumx) * ((p->n * p->sumy2) - (p->sumy*p->sumy));


        sdx = (p->n * p->sumx2 - p->sumx*p->sumx) / ((p->n)*(p->n-1));
        sdy = (p->n * p->sumy2 - p->sumy*p->sumy) / ((p->n)*(p->n-1));

        return sqrt(sdx * sdx + sdy * sdy);
}

// Assure XYZ is positive

static
BOOL IsGood(LPcmsCIEXYZ xyz)
{
    if (xyz ->X == 0 && xyz->Y == 0 && xyz->Z == 0) return FALSE;
    return (xyz->X >= 0 && xyz->Y >= 0 && xyz->Z >= 0);
}

//
// This procedure runs linear regression analise of chromacity 
// on R/G/B axis. Intends to locate primaries

static
double LocatePrimaries(cmsHPROFILE hProfile, LPcmsCIExyYTRIPLE p)
{
    cmsCIExyYTRIPLE Primaries;
    cmsCIEXYZ RedXYZ, GreenXYZ, BlueXYZ;   
    cmsCIEXYZ ARedXYZ, AGreenXYZ, ABlueXYZ, MediaWhite;   
    cmsCIExyY WhitePoint;
    cmsHTRANSFORM xform;
    cmsHPROFILE hXYZ;
    WORD RedRGB[3], GreenRGB[3], BlueRGB[3];
    WORD XYZEncoded[3];
    BOOL Good;
    WORD s;
    LINEAR_REGRESSION_PARAMS lr_red, lr_green, lr_blue;
    double Corr, Corg, Corb;

    hXYZ = cmsCreateXYZProfile();
    xform = cmsCreateTransform(hProfile, TYPE_RGB_16, hXYZ, TYPE_XYZ_16,
                              INTENT_RELATIVE_COLORIMETRIC, cmsFLAGS_NOTPRECALC);

    ClearRegression(&lr_red);
    ClearRegression(&lr_green);
    ClearRegression(&lr_blue);
    cmsTakeMediaWhitePoint(&MediaWhite, hProfile);
    cmsXYZ2xyY(&WhitePoint, &MediaWhite);

    s = 0xFFFF;     
    do {

    RedRGB[0]   = s; RedRGB[1]   = 0; RedRGB[2]   = 0;   
    GreenRGB[0] = 0; GreenRGB[1] = s; GreenRGB[2] = 0;   
    BlueRGB[0]  = 0; BlueRGB[1]  = 0; BlueRGB[2]  = s;   
    
    cmsDoTransform(xform, RedRGB, XYZEncoded, 1);
    cmsXYZEncoded2Float(&RedXYZ, XYZEncoded);
    cmsDoTransform(xform, GreenRGB, XYZEncoded, 1);
    cmsXYZEncoded2Float(&GreenXYZ, XYZEncoded);
    cmsDoTransform(xform, BlueRGB, XYZEncoded, 1);
    cmsXYZEncoded2Float(&BlueXYZ, XYZEncoded);

    Good = (IsGood(&RedXYZ) && IsGood(&GreenXYZ) && IsGood(&BlueXYZ));
    
    if (Good) {

        cmsAdaptToIlluminant(&ARedXYZ,   cmsD50_xyY(), &WhitePoint, &RedXYZ);
        cmsAdaptToIlluminant(&AGreenXYZ, cmsD50_xyY(), &WhitePoint, &GreenXYZ);
        cmsAdaptToIlluminant(&ABlueXYZ,  cmsD50_xyY(), &WhitePoint, &BlueXYZ);

        cmsXYZ2xyY(&Primaries.Red,   &ARedXYZ);
        cmsXYZ2xyY(&Primaries.Green, &AGreenXYZ);
        cmsXYZ2xyY(&Primaries.Blue,  &ABlueXYZ);

        // Take statistics
        AddRegression(&lr_red,   &Primaries.Red);
        AddRegression(&lr_green, &Primaries.Green);
        AddRegression(&lr_blue,  &Primaries.Blue);      
    }

    s -= 0xff;

    } while (s > 0x7f00);

    // Compute regression params
    Corr = ResultRegression(&lr_red,   &Primaries.Red);
    Corg = ResultRegression(&lr_green, &Primaries.Green);
    Corb = ResultRegression(&lr_blue, &Primaries.Blue);

    cmsDeleteTransform(xform);
    cmsCloseProfile(hXYZ);

    *p = Primaries;

    return sqrt(Corr*Corr + Corg*Corg + Corb*Corb); 
}


static
void ShowWhitePoint(LPcmsCIEXYZ WtPt)
{       
       cmsCIELab Lab;
       cmsCIELCh LCh;
       cmsCIExyY xyY;
       char Buffer[1024];

        
       _cmsIdentifyWhitePoint(Buffer, WtPt);
       printf("%s\n", Buffer);
       
       cmsXYZ2Lab(NULL, &Lab, WtPt);
       cmsLab2LCh(&LCh, &Lab);
       cmsXYZ2xyY(&xyY, WtPt);

       printf("XYZ=(%3.3f, %3.3f, %3.3f)\n", WtPt->X * 100., WtPt->Y * 100., WtPt->Z * 100.);
       printf("Lab=(%3.3f, %3.3f, %3.3f)\n", Lab.L, Lab.a, Lab.b);
       printf("(x,y)=(%3.3f, %3.3f)\n", xyY.x, xyY.y);
       printf("Hue=%3.2f, Chroma=%3.2f\n", LCh.h, LCh.C);
       printf("\n");
       
}



static
void PrintPrimaries(double s, LPcmsCIExyYTRIPLE Primaries)
{
    printf("Primaries(%f): R(%1.3f, %1.3f) G(%1.3f, %1.3f) B(%1.3f, %1.3f)\n", 
                s,
                Primaries->Red.x, Primaries->Red.y,
                Primaries->Green.x, Primaries->Green.y,
                Primaries->Blue.x, Primaries->Blue.y
                );

}


static
void ShowPrimaries(cmsHPROFILE hProfile)
{

    /*cmsCIExyYTRIPLE Primaries;
    cmsCIEXYZ RedXYZ, GreenXYZ, BlueXYZ;   
    cmsHTRANSFORM xform;
    cmsHPROFILE hXYZ;
    WORD RedRGB[3], GreenRGB[3], BlueRGB[3];
    WORD XYZEncoded[3];
    BOOL Good;
    WORD s;

    hXYZ = cmsCreateXYZProfile();
    xform = cmsCreateTransform(hProfile, TYPE_RGB_16, hXYZ, TYPE_XYZ_16,
                              INTENT_ABSOLUTE_COLORIMETRIC, cmsFLAGS_NOTPRECALC);


    // Try until positive value
    s = 0xFFFF;     
    do {

    RedRGB[0]   = s; RedRGB[1]   = 0x0000; RedRGB[2]    = 0x0000;   
    GreenRGB[0] = 0x0000; GreenRGB[1] = s; GreenRGB[2] = 0x0000;   
    BlueRGB[0]  = 0x0000; BlueRGB[1]  = 0x0000; BlueRGB[2]  = s;   
    
    cmsDoTransform(xform, RedRGB, XYZEncoded, 1);
    cmsXYZEncoded2Float(&RedXYZ, XYZEncoded);
    cmsDoTransform(xform, GreenRGB, XYZEncoded, 1);
    cmsXYZEncoded2Float(&GreenXYZ, XYZEncoded);
    cmsDoTransform(xform, BlueRGB, XYZEncoded, 1);
    cmsXYZEncoded2Float(&BlueXYZ, XYZEncoded);

    Good = (IsGood(&RedXYZ) && IsGood(&GreenXYZ) && IsGood(&BlueXYZ));
    
    if (Good) {

        cmsXYZ2xyY(&Primaries.Red,   &RedXYZ);
        cmsXYZ2xyY(&Primaries.Green, &GreenXYZ);
        cmsXYZ2xyY(&Primaries.Blue,  &BlueXYZ);
        PrintPrimaries(s, &Primaries);
    }

    s--;


    } while (s > 0);

    if (Good) {

        cmsXYZ2xyY(&Primaries.Red,   &RedXYZ);
        cmsXYZ2xyY(&Primaries.Green, &GreenXYZ);
        cmsXYZ2xyY(&Primaries.Blue,  &BlueXYZ);
        PrintPrimaries(s, &Primaries);

    } else printf("Couldn't find suitable primaries!\n");

    cmsDeleteTransform(xform);
    cmsCloseProfile(hXYZ);
    */

    cmsCIExyYTRIPLE Primaries;

    double CorrelationCoef = LocatePrimaries(hProfile, &Primaries);
    PrintPrimaries(CorrelationCoef, &Primaries);
}

int main (int argc, char *argv[])
{
       printf("Show media white and primaries of profile, identifying black body locus. v2.1\n\n");


       if (argc == 2) {
                  cmsCIEXYZ WtPt;
                  cmsHPROFILE hProfile = cmsOpenProfileFromFile(argv[1], "r");

                  printf("%s\n", cmsTakeProductName(hProfile));
                  cmsTakeMediaWhitePoint(&WtPt, hProfile);
                  ShowWhitePoint(&WtPt);
                  ShowPrimaries(hProfile);
                  cmsCloseProfile(hProfile);
              }
       else
              {
              cmsCIEXYZ xyz;
              
              printf("usage:\n\nIf no parameters are given, then this program will\n");
              printf("ask for XYZ value of media white. If parameter given, it must be\n");
              printf("the profile to inspect.\n\n");

              printf("X? "); scanf("%lf", &xyz.X);
              printf("Y? "); scanf("%lf", &xyz.Y);
              printf("Z? "); scanf("%lf", &xyz.Z);

              ShowWhitePoint(&xyz);
              }

       return 0;
}

