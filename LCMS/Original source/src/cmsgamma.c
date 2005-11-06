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

// Gamma handling. I'm encoding gamma as 0..ffff value

LPGAMMATABLE LCMSEXPORT cmsAllocGamma(int nEntries);
void         LCMSEXPORT cmsFreeGamma(LPGAMMATABLE Gamma);
void         LCMSEXPORT cmsFreeGammaTriple(LPGAMMATABLE Gamma[3]);
LPGAMMATABLE LCMSEXPORT cmsBuildGamma(int nEntries, double Gamma);
LPGAMMATABLE LCMSEXPORT cmsDupGamma(LPGAMMATABLE Src);
LPGAMMATABLE LCMSEXPORT cmsReverseGamma(int nResultSamples, LPGAMMATABLE InGamma);
LPGAMMATABLE LCMSEXPORT cmsJoinGamma(LPGAMMATABLE InGamma, LPGAMMATABLE OutGamma);
LPGAMMATABLE LCMSEXPORT cmsJoinGammaEx(LPGAMMATABLE InGamma, LPGAMMATABLE OutGamma, int nPoints);
BOOL         LCMSEXPORT cmsSmoothGamma(LPGAMMATABLE Tab, double lambda);

BOOL         cdecl _cmsSmoothEndpoints(LPWORD Table, int nPoints);


// Sampled curves

LPSAMPLEDCURVE cdecl cmsAllocSampledCurve(int nItems);
void           cdecl cmsFreeSampledCurve(LPSAMPLEDCURVE p);
void           cdecl cmsEndpointsOfSampledCurve(LPSAMPLEDCURVE p, double* Min, double* Max);
void           cdecl cmsClampSampledCurve(LPSAMPLEDCURVE p, double Min, double Max);
BOOL           cdecl cmsSmoothSampledCurve(LPSAMPLEDCURVE Tab, double SmoothingLambda);
void           cdecl cmsRescaleSampledCurve(LPSAMPLEDCURVE p, double Min, double Max, int nPoints);

LPSAMPLEDCURVE cdecl cmsJoinSampledCurves(LPSAMPLEDCURVE X, LPSAMPLEDCURVE Y, int nResultingPoints);


// ----------------------------------------------------------------------------------------

// #define DEBUG 1

#define MAX_KNOTS   4096
typedef float vec[MAX_KNOTS+1];


// default gamma function

#if 1

// No slope limiting

static
double FGamma(double R, double x)
{
       return pow(R, x);
}

#else

// Quantization becomes evident below 
//
//      R^x * 65535 < 1.0
//
// So, we apply linear tram on
// 
//      R^x < 1 / 65535
//
//      x * log(R) < log(1) - log(65535) 
//
//      R < (1 /exp(log(65535) / x)
//
//  Somehow arbitrarely, we adopt 0.018, which corresponds
//  with gamma 2.7 This is adequate for most situations.
//
//  The resulting line must be:
//
//  f(0) = 0
//  f(0.018) = pow(0.018, x)
//
//  so, 
//  
//  f() = pow(0.018, x) / 0.018

static 
double FGamma(double R, double x)
{
       if (R < 0.018) 
           return (R * pow(0.018, x)) / 0.018;

       return pow(R, x);
}


#endif

LPGAMMATABLE LCMSEXPORT cmsAllocGamma(int nEntries)
{
       LPGAMMATABLE p;
       size_t size;

       size = sizeof(GAMMATABLE) + (sizeof(WORD) * (nEntries-1));

       p = (LPGAMMATABLE) malloc(size);
       if (!p) return NULL;

       p -> nEntries = nEntries;
       ZeroMemory(p -> GammaTable, nEntries * sizeof(WORD));

       return p;
}

void LCMSEXPORT cmsFreeGamma(LPGAMMATABLE Gamma)
{
       free(Gamma);
}



void LCMSEXPORT cmsFreeGammaTriple(LPGAMMATABLE Gamma[3])
{
    cmsFreeGamma(Gamma[0]);
    cmsFreeGamma(Gamma[1]);
    cmsFreeGamma(Gamma[2]);
    Gamma[0] = Gamma[1] = Gamma[2] = NULL;
}

// Build a gamma table based on gamma constant

LPGAMMATABLE LCMSEXPORT cmsBuildGamma(int nEntries, double Gamma)
{
       LPGAMMATABLE p;
       LPWORD Table;
       int i;
       double R, Val;

       if (nEntries > 65530) {
                cmsSignalError(LCMS_ERRC_WARNING, "Couldn't create gammatable of more than 65530 entries; 65530 assumed");
                nEntries = 65530;
       }

       p = cmsAllocGamma(nEntries);
       if (!p) return NULL;

       Table = p -> GammaTable;
       if (Gamma == 0.0)
       {
              ZeroMemory(Table, nEntries*sizeof(WORD));
              return p;
       }

       if (Gamma == 1.0) {

           for (i=0; i < nEntries; i++) {

            Table[i] = _cmsQuantizeVal(i, nEntries);              

           }
           return p;
       }

       for (i=0; i < nEntries; i++)
       {
              R   = (double) i / (nEntries-1);
              Val = FGamma(R, Gamma);

              Table[i] = (WORD) floor(Val * 65535. + .5);
       }

       return p;
}


// Duplicate a gamma table

LPGAMMATABLE  LCMSEXPORT cmsDupGamma(LPGAMMATABLE In)
{
       LPGAMMATABLE Ptr;

       Ptr = cmsAllocGamma(In -> nEntries);
       if (Ptr == NULL) return NULL;

       CopyMemory(Ptr -> GammaTable,
                  In -> GammaTable,
                  In -> nEntries * sizeof(WORD));

       return Ptr;
}


// Handle gamma using interpolation tables. The resulting curves can become
// very stange, but are pleasent to eye.

LPGAMMATABLE LCMSEXPORT cmsJoinGamma(LPGAMMATABLE InGamma,
                          LPGAMMATABLE OutGamma)
{
       register int i;
       L16PARAMS L16In, L16Out;
       LPWORD InPtr, OutPtr;
       LPGAMMATABLE p;

       p = cmsAllocGamma(256);
       if (!p) return NULL;

       cmsCalcL16Params(InGamma -> nEntries, &L16In);
       InPtr  = InGamma -> GammaTable;

       cmsCalcL16Params(OutGamma -> nEntries, &L16Out);
       OutPtr = OutGamma-> GammaTable;

       for (i=0; i < 256; i++)
       {
              WORD wValIn, wValOut;

              wValIn  = cmsLinearInterpLUT16(RGB_8_TO_16(i), InPtr, &L16In);
              wValOut = cmsReverseLinearInterpLUT16(wValIn, OutPtr, &L16Out);

              p -> GammaTable[i] = wValOut;
       }

       return p;
}



// New method, using smoothed parametric curves. This works FAR better.
// We want to get 
//
//      y = f(g^-1(x))      ; f = ingamma, g = outgamma
//
// And this can be parametrized as
//
//      y = f(t)
//      x = g(t)


LPGAMMATABLE LCMSEXPORT cmsJoinGammaEx(LPGAMMATABLE InGamma,
                                       LPGAMMATABLE OutGamma, int nPoints)
{

    LPSAMPLEDCURVE x, y, r;
    LPGAMMATABLE res;
    
    x = cmsConvertGammaToSampledCurve(InGamma,  nPoints);
    y = cmsConvertGammaToSampledCurve(OutGamma, nPoints);
    r = cmsJoinSampledCurves(y, x, nPoints);

    // Does clean "hair"
    cmsSmoothSampledCurve(r, 0.001);
    
    cmsFreeSampledCurve(x);
    cmsFreeSampledCurve(y);

    res = cmsConvertSampledCurveToGamma(r, 65535.0);
    cmsFreeSampledCurve(r);

    return res;
}



// Reverse a gamma table

LPGAMMATABLE LCMSEXPORT cmsReverseGamma(int nResultSamples, LPGAMMATABLE InGamma)
{
       register int i;
       L16PARAMS L16In;
       LPWORD InPtr;
       LPGAMMATABLE p;

       p = cmsAllocGamma(nResultSamples);
       if (!p) return NULL;

       cmsCalcL16Params(InGamma -> nEntries, &L16In);
       InPtr  = InGamma -> GammaTable;

       for (i=0; i < nResultSamples; i++)
       {
              WORD wValIn, wValOut;

              wValIn = _cmsQuantizeVal(i, nResultSamples);           
              wValOut = cmsReverseLinearInterpLUT16(wValIn, InPtr, &L16In);
              p -> GammaTable[i] = wValOut;
       }


       // Does eliminate "hair" on curve      
       cmsSmoothGamma(p, 0.001);           
           
       return p;
}



// Parametric curves
//
// Parameters goes as: Gamma, a, b, c, d, e, f
// Type is the ICC type +1
// if type is negative, then the curve is analyticaly inverted

LPGAMMATABLE LCMSEXPORT cmsBuildParametricGamma(int nEntries, int Type, double Params[])
{
        LPGAMMATABLE Table;
        double R, Val, dval, e;
        int i;

        Table = cmsAllocGamma(nEntries);
        if (NULL == Table) return NULL;


        for (i=0; i < nEntries; i++) {

                R   = (double) i / (nEntries-1);

                switch (Type) {

                // X = Y ^ Gamma
                case 1:
                      Val = pow(R, Params[0]);
                      break;

                // Type 1 Reversed: X = Y ^1/gamma
                case -1:
                      Val = pow(R, 1/Params[0]);
                      break;

                // CIE 122-1966
                // Y = (aX + b)^Gamma  | X >= -b/a
                // Y = 0               | else
                case 2:
                    if (R >= -Params[2] / Params[1]) {
                              
                              e = Params[1]*R + Params[2];

                              if (e > 0)
                                Val = pow(e, Params[0]);
                              else
                                Val = 0;
                    }
                    else
                              Val = 0;
                      break;

                // Type 2 Reversed
                // X = (Y ^1/g  - b) / a
                case -2: 
                    
                    Val = (pow(R, 1.0/Params[0]) - Params[2]) / Params[1];
                    if (Val < 0)
                            Val = 0;                            
                    break;


                // IEC 61966-3
                // Y = (aX + b)^Gamma | X <= -b/a
                // Y = c              | else
                case 3:
                    if (R >= -Params[2] / Params[1]) {
                            
                      e = Params[1]*R + Params[2];                    
                      Val = pow(e, Params[0]) + Params[3];
                    }
                    else
                      Val = Params[3];
                    break;


                // Type 3 reversed
                // X=((Y-c)^1/g - b)/a      | (Y>=c)
                // X=-b/a                   | (Y<c) 
                    
                case -3:
                    if (R >= Params[3])  {
                        e = R - Params[3];
                        Val = (pow(e, 1/Params[0]) - Params[2]) / Params[1];
                        if (Val < 0) Val = 0;
                    }
                    else {
                        Val = -Params[2] / Params[1];
                    }
                    break;


                // IEC 61966-2.1 (sRGB)
                // Y = (aX + b)^Gamma | X >= d
                // Y = cX             | X < d
                case 4:
                    if (R >= Params[4]) {
                              
                              e = Params[1]*R + Params[2];
                              if (e > 0)
                                Val = pow(e, Params[0]);
                              else
                                Val = 0;
                    }
                      else
                              Val = R * Params[3];
                      break;

                // Type 4 reversed
                // X=((Y^1/g-b)/a)    | Y >= (ad+b)^g
                // X=Y/c              | Y< (ad+b)^g

                case -4:
                    if (R >= pow(Params[1] * Params[4] + Params[2], Params[0])) {

                        Val = (pow(R, 1.0/Params[0]) - Params[2]) / Params[1];
                    }
                    else {
                        Val = R / Params[3];
                    }
                    break;
                


                // Y = (aX + b)^Gamma + e | X <= d
                // Y = cX + f             | else
                case 5:
                    if (R >= Params[4]) {
                             
                        e = Params[1]*R + Params[2];
                        Val = pow(e, Params[0]) + Params[5];
                    }        
                    else
                        Val = R*Params[3] + Params[6];
                    break;


                // Reversed type 5
                // X=((Y-e)1/g-b)/a   | Y >=(ad+b)^g+e)
                // X=(Y-f)/c          | else
                case -5:

                if (R >= pow(Params[1] * Params[4], Params[0]) + Params[5]) {

                    Val = pow(R - Params[5], 1/Params[0]) - Params[2] / Params[1];
                }
                else {
                    Val = (R - Params[6]) / Params[3];
                }
                break;

                default:
                        cmsSignalError(-1, "Unsupported parametric curve type=%d", abs(Type)-1);
                        cmsFreeGamma(Table);
                        return NULL;
                }


        // Saturate

        dval = Val * 65535.0 + .5;
        if (dval > 65535.) dval = 65535.0;
        if (dval < 0) dval = 0;

        Table->GammaTable[i] = (WORD) floor(dval);
        }

        return Table;
}


// From: Eilers, P.H.C. (1994) Smoothing and interpolation with finite
// differences. in: Graphic Gems IV, Heckbert, P.S. (ed.), Academic press.
//
// Smoothing and interpolation with second differences.
//
//   Input:  weights (w), data (y): vector from 1 to m.
//   Input:  smoothing parameter (lambda), length (m).
//   Output: smoothed vector (z): vector from 1 to m.


static
void smooth2(vec w, vec y, vec z, float lambda, int m)
{
  int i, i1, i2;
  vec c, d, e;
  d[1] = w[1] + lambda;
  c[1] = -2 * lambda / d[1];
  e[1] = lambda /d[1];
  z[1] = w[1] * y[1];
  d[2] = w[2] + 5 * lambda - d[1] * c[1] *  c[1];
  c[2] = (-4 * lambda - d[1] * c[1] * e[1]) / d[2];
  e[2] = lambda / d[2];
  z[2] = w[2] * y[2] - c[1] * z[1];
  for (i = 3; i < m - 1; i++) {
    i1 = i - 1; i2 = i - 2;
    d[i]= w[i] + 6 * lambda - c[i1] * c[i1] * d[i1] - e[i2] * e[i2] * d[i2];
    c[i] = (-4 * lambda -d[i1] * c[i1] * e[i1])/ d[i];
    e[i] = lambda / d[i];
    z[i] = w[i] * y[i] - c[i1] * z[i1] - e[i2] * z[i2];
  }
  i1 = m - 2; i2 = m - 3;
  d[m - 1] = w[m - 1] + 5 * lambda -c[i1] * c[i1] * d[i1] - e[i2] * e[i2] * d[i2];
  c[m - 1] = (-2 * lambda - d[i1] * c[i1] * e[i1]) / d[m - 1];
  z[m - 1] = w[m - 1] * y[m - 1] - c[i1] * z[i1] - e[i2] * z[i2];
  i1 = m - 1; i2 = m - 2;
  d[m] = w[m] + lambda - c[i1] * c[i1] * d[i1] - e[i2] * e[i2] * d[i2];
  z[m] = (w[m] * y[m] - c[i1] * z[i1] - e[i2] * z[i2]) / d[m];
  z[m - 1] = z[m - 1] / d[m - 1] - c[m - 1] * z[m];
  for (i = m - 2; 1<= i; i--)
     z[i] = z[i] / d[i] - c[i] * z[i + 1] - e[i] * z[i + 2];
}



// Smooths a curve sampled at regular intervals

BOOL LCMSEXPORT cmsSmoothGamma(LPGAMMATABLE Tab, double lambda)

{
    vec w, y, z;
    int i, nItems, Zeros, Poles;


    if (cmsIsLinear(Tab->GammaTable, Tab->nEntries)) return FALSE; // Nothing to do

    nItems = Tab -> nEntries;

    if (nItems > MAX_KNOTS) {
                cmsSignalError(LCMS_ERRC_ABORTED, "cmsSmoothGamma: too many points.");
                return FALSE;
                }

    ZeroMemory(w, nItems * sizeof(float));
    ZeroMemory(y, nItems * sizeof(float));
    ZeroMemory(z, nItems * sizeof(float));

    for (i=0; i < nItems; i++)
    {
        y[i+1] = (float) Tab -> GammaTable[i];
        w[i+1] = 1.0;
    }

    smooth2(w, y, z, (float) lambda, nItems);

    // Do some reality - checking...
    Zeros = Poles = 0;
    for (i=nItems; i > 1; --i) {

            if (z[i] == 0.) Zeros++;
            if (z[i] >= 65535.) Poles++;
            if (z[i] < z[i-1]) return FALSE; // Non-Monotonic
    }

    if (Zeros > (nItems / 3)) return FALSE;  // Degenerated, mostly zeros
    if (Poles > (nItems / 3)) return FALSE;  // Degenerated, mostly poles

    // Seems ok

    for (i=0; i < nItems; i++) {

        // Clamp to WORD

        float v = z[i+1];

        if (v < 0) v = 0;
        if (v > 65535.) v = 65535.;

        Tab -> GammaTable[i] = (WORD) floor(v + .5);
        }

    return TRUE;
}


// Check if curve is exponential, return gamma if so.

double LCMSEXPORT cmsEstimateGammaEx(LPWORD GammaTable, int nEntries, double Thereshold)
{
    double gamma, sum, sum2;
    double n, x, y, Std;
    int i;

    sum = sum2 = n = 0;
    
    // Does exclude endpoints   
    for (i=1; i < nEntries - 1; i++) {

            x = (double) i / (nEntries - 1);
            y = (double) GammaTable[i] / 65535.;
            
            // Avoid 7% on lower part to prevent 
            // artifacts due to linear ramps

            if (y > 0. && y < 1. && x > 0.07) {

            gamma = log(y) / log(x);
            sum  += gamma;
            sum2 += gamma * gamma;
            n++;
            }
            
    }

    // Take a look on SD to see if gamma isn't exponential at all
    Std = sqrt((n * sum2 - sum * sum) / (n*(n-1)));
    

    if (Std > Thereshold)
        return -1.0;

    return (sum / n);   // The mean
}


double LCMSEXPORT cmsEstimateGamma(LPGAMMATABLE t)
{
        return cmsEstimateGammaEx(t->GammaTable, t->nEntries, 0.7);
}


// -----------------------------------------------------------------Sampled curves

// Allocate a empty curve

LPSAMPLEDCURVE cmsAllocSampledCurve(int nItems)
{
    LPSAMPLEDCURVE pOut;

    pOut = (LPSAMPLEDCURVE) malloc(sizeof(SAMPLEDCURVE));
    if (pOut == NULL)
            return NULL;

    if((pOut->Values = (double *) malloc(nItems * sizeof(double))) == NULL)
    {
        free(pOut);
        return NULL;
    }

    pOut->nItems = nItems;
    ZeroMemory(pOut->Values, nItems * sizeof(double));

    return pOut;
}


void cmsFreeSampledCurve(LPSAMPLEDCURVE p)
{
    free((LPVOID) p -> Values);
    free((LPVOID) p);
}



// Does duplicate a sampled curve

LPSAMPLEDCURVE cmsDupSampledCurve(LPSAMPLEDCURVE p)
{
    LPSAMPLEDCURVE out;

    out = cmsAllocSampledCurve(p -> nItems);
    if (!out) return NULL;

    CopyMemory(out ->Values, p ->Values, p->nItems * sizeof(double));

    return out;
}


// Take min, max of curve

void cmsEndpointsOfSampledCurve(LPSAMPLEDCURVE p, double* Min, double* Max)
{
        int i;

        *Min = 65536.;
        *Max = 0.;

        for (i=0; i < p -> nItems; i++) {

                double v = p -> Values[i];

                if (v < *Min)
                        *Min = v;

                if (v > *Max)
                        *Max = v;
        }

        if (*Min < 0) *Min = 0;
        if (*Max > 65535.0) *Max = 65535.0;
}

// Clamps to Min, Max

void cmsClampSampledCurve(LPSAMPLEDCURVE p, double Min, double Max)
{

        int i;

        for (i=0; i < p -> nItems; i++) {

                double v = p -> Values[i];

                if (v < Min)
                        v = Min;

                if (v > Max)
                        v = Max;

                p -> Values[i] = v;

        }

}



// Smooths a curve sampled at regular intervals

BOOL cmsSmoothSampledCurve(LPSAMPLEDCURVE Tab, double lambda)
{
    vec w, y, z;
    int i, nItems;

    nItems = Tab -> nItems;

    if (nItems > MAX_KNOTS) {
                cmsSignalError(LCMS_ERRC_ABORTED, "cmsSmoothSampledCurve: too many points.");
                return FALSE;
                }

    ZeroMemory(w, nItems * sizeof(float));
    ZeroMemory(y, nItems * sizeof(float));
    ZeroMemory(z, nItems * sizeof(float));

    for (i=0; i < nItems; i++)
    {
        float value = (float) Tab -> Values[i];

        y[i+1] = value;
        w[i+1] = (float) ((value < 0.0) ?  0 : 1);
    }


    smooth2(w, y, z, (float) lambda, nItems);

    for (i=0; i < nItems; i++) {

        Tab -> Values[i] = z[i+1];;
     }

    return TRUE;

}


// Scale a value v, within domain Min .. Max 
// to a domain 0..(nPoints-1)

static
double ScaleVal(double v, double Min, double Max, int nPoints)
{

        double a, b;

        if (v < 0) return 0;
    
        a = (double) (nPoints - 1) / (Max - Min);
        b = a * Min;

        return (a * v) - b;

}


// Does rescale a sampled curve to fit in a 0..(nPoints-1) domain

void cmsRescaleSampledCurve(LPSAMPLEDCURVE p, double Min, double Max, int nPoints)
{

        int i;

        for (i=0; i < p -> nItems; i++) {

                double v = p -> Values[i];

                p -> Values[i] = ScaleVal(v, Min, Max, nPoints);
        }

}


// Joins two sampled curves for X and Y. Curves should be sorted.

LPSAMPLEDCURVE cmsJoinSampledCurves(LPSAMPLEDCURVE X, LPSAMPLEDCURVE Y, int nResultingPoints)
{
    int i, j;   
    LPSAMPLEDCURVE out;
    double MinX, MinY, MaxX, MaxY;
    double x, y, x1, y1, x2, y2, a, b;

    out = cmsAllocSampledCurve(nResultingPoints);
    if (out == NULL)
        return NULL;

    if (X -> nItems != Y -> nItems) {

        cmsSignalError(LCMS_ERRC_ABORTED, "cmsJoinSampledCurves: invalid curve.");
        cmsFreeSampledCurve(out);
        return NULL;
    }

    // Get endpoints of sampled curves
    cmsEndpointsOfSampledCurve(X, &MinX, &MaxX);
    cmsEndpointsOfSampledCurve(Y, &MinY, &MaxY);

    
    // Set our points
    out ->Values[0] = MinY; 
    for (i=1; i < nResultingPoints; i++) {

        // Scale t to x domain
        x = (i * (MaxX - MinX) / (nResultingPoints-1)) + MinX;

        // Find interval in which t is within (always up, 
        // since fn should be monotonic at all)

        j = 1;
        while ((j < X ->nItems - 1) && X ->Values[j] < x)
            j++;
            
        // Now x is within X[j-1], X[j]
        x1 = X ->Values[j-1]; x2 = X ->Values[j];
        y1 = Y ->Values[j-1]; y2 = Y ->Values[j];

        // Interpolate  the value
        a = (y1 - y2) / (x1 - x2);
        b = y1 - a * x1;
        y = a* x + b;
        
        out ->Values[i] = y;
    }
    

    cmsClampSampledCurve(out, MinY, MaxY);
    return out;
}



// Convert between curve types

LPGAMMATABLE cmsConvertSampledCurveToGamma(LPSAMPLEDCURVE Sampled, double Max)
{
    LPGAMMATABLE Gamma;
    int i, nPoints;
    

    nPoints = Sampled ->nItems;

    Gamma = cmsAllocGamma(nPoints);
    for (i=0; i < nPoints; i++) {
        
        Gamma->GammaTable[i] = (WORD) floor(ScaleVal(Sampled ->Values[i], 0, Max, 65536) + .5);
    }

    return Gamma;

}

// Inverse of anterior

LPSAMPLEDCURVE cmsConvertGammaToSampledCurve(LPGAMMATABLE Gamma, int nPoints)
{
    LPSAMPLEDCURVE Sampled;
    L16PARAMS L16;
    int i;
    WORD wQuant, wValIn;

    if (nPoints > 4096) {

        cmsSignalError(-1, "cmsConvertGammaToSampledCurve: too many points (max=4096)");
        return NULL;
    }

    cmsCalcL16Params(Gamma -> nEntries, &L16);
       
    Sampled = cmsAllocSampledCurve(nPoints);
    for (i=0; i < nPoints; i++) {
            wQuant  = _cmsQuantizeVal(i, nPoints);
            wValIn  = cmsLinearInterpLUT16(wQuant, Gamma ->GammaTable, &L16);
            Sampled ->Values[i] = (float) wValIn;
    }

    return Sampled;
}


#ifdef DEBUG
static
void ASAVE(LPWORD Table, int nEntries, const char* dump)
{
    FILE* f;
    int i;

        f = fopen(dump, "wt");
        if (!f)
                return;

        

    for (i=0; i < nEntries; i++)
        fprintf(f, "%g\n", (double) Table[i]);
        

    fclose(f);
}
#endif


// Smooth endpoints (used in Black/White compensation)

BOOL _cmsSmoothEndpoints(LPWORD Table, int nEntries)
{
    vec w, y, z;
    int i, Zeros, Poles;

#ifdef DEBUG
        ASAVE(Table, nEntries, "nonsmt.txt");
#endif


    if (cmsIsLinear(Table, nEntries)) return FALSE; // Nothing to do

    
    if (nEntries > MAX_KNOTS) {
                cmsSignalError(LCMS_ERRC_ABORTED, "_cmsSmoothEndpoints: too many points.");
                return FALSE;
                }

    ZeroMemory(w, nEntries * sizeof(float));
    ZeroMemory(y, nEntries * sizeof(float));
    ZeroMemory(z, nEntries * sizeof(float));

    for (i=0; i < nEntries; i++)
    {
        y[i+1] = (float) Table[i];
        w[i+1] = 1.0;
    }

    w[1]        = 65535.0;
    w[nEntries] = 65535.0;

    smooth2(w, y, z, (float) nEntries, nEntries);

    // Do some reality - checking...
    Zeros = Poles = 0;
    for (i=nEntries; i > 1; --i) {

            if (z[i] == 0.) Zeros++;
            if (z[i] >= 65535.) Poles++;
            if (z[i] < z[i-1]) return FALSE; // Non-Monotonic
    }

    if (Zeros > (nEntries / 3)) return FALSE;  // Degenerated, mostly zeros
    if (Poles > (nEntries / 3)) return FALSE;    // Degenerated, mostly poles

    // Seems ok

    for (i=0; i < nEntries; i++) {

        // Clamp to WORD

        float v = z[i+1];

        if (v < 0) v = 0;
        if (v > 65535.) v = 65535.;

        Table[i] = (WORD) floor(v + .5);
        }

#ifdef DEBUG
        ASAVE(Table, nEntries, "smoothed.txt");
#endif
    return TRUE;
}
