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

// Postscript level 2 operators


#include "lcms.h"
#include <time.h>
#include <stdarg.h>

// PostScript ColorRenderingDictionary and ColorSpaceArray

LCMSAPI DWORD LCMSEXPORT cmsGetPSCSA(cmsHPROFILE hProfile, int Intent, LPVOID Buffer, DWORD dwBufferLen);
LCMSAPI DWORD LCMSEXPORT cmsGetPSCRD(cmsHPROFILE hProfile, int Intent, LPVOID Buffer, DWORD dwBufferLen);

// -------------------------------------------------------------------- Implementation

#define MAXPSCOLS   60      // Columns on tables

/*
    Implementation
    --------------

  PostScript does use XYZ as its internal PCS. But since PostScript 
  interpolation tables are limited to 8 bits, I use Lab as a way to 
  improve the accurancy, favoring perceptual results. So, for the creation 
  of each CRD, CSA the profiles are converted to Lab via a device 
  link between  profile -> Lab or Lab -> profile. The PS code necessary to
  convert Lab <-> XYZ is also added.



  Color Space Arrays (CSA) 
  ==================================================================================

  In order to obtain precision, code chooses between three ways to implement
  the device -> XYZ transform. These cases identifies monochrome profiles (often
  implemented as a set of curves), matrix-shaper and LUT-based.

  Monochrome 
  -----------

  This is implemented as /CIEBasedA CSA. The prelinearization curve is 
  placed into /DecodeA section, and matrix equals to D50. Since here is
  no interpolation tables, I do the conversion directly to XYZ

  NOTE: CLUT-based monochrome profiles are NOT supported. So, cmsFLAGS_MATRIXINPUT
  flag is forced on such profiles.

    [ /CIEBasedA
      <<            
            /DecodeA { transfer function } bind
            /MatrixA [D50]  
            /RangeLMN [ 0.0 D50X 0.0 D50Y 0.0 D50Z ]
            /WhitePoint [D50]
            /BlackPoint [Perfect absorber]
            /RenderingIntent (intent)
      >>
    ] /Current exch /ColorSpace defineresource pop

   On simpler profiles, the PCS is already XYZ, so no conversion is required.

 
   Matrix-shaper based
   -------------------

   This is implemented both with /CIEBasedABC or /CIEBasedDEF on dependig
   of profile implementation. Since here is no interpolation tables, I do 
   the conversion directly to XYZ



    [ /CIEBasedABC
            <<
                /DecodeABC [ {transfer1} {transfer2} {transfer3} ]
                /MatrixABC [Matrix]
                /RangeLMN [ 0.0 D50X 0.0 D50Y 0.0 D50Z ]
                /DecodeLMN [ { / 2} dup dup ]
                /WhitePoint [D50]
                /BlackPoint [Perfect absorber]
                /RenderingIntent (intent)
            >>
    ] /Current exch /ColorSpace defineresource pop


    CLUT based
    ----------

     Lab is used in such cases.

    [ /CIEBasedDEF
            <<
            /DecodeDEF [ <prelinearization> ]
            /Table [ p p p [<...>]]
            /RangeABC [ 0 1 0 1 0 1]
            /DecodeABC[ <postlinearization> ]
            /MatrixABC [ 1 1 1 1 0 0 0 0 -1]
            /WhitePoint [D50]
            /BlackPoint [Perfect absorber]
            /RenderingIntent (intent)
    ] 


  Color Rendering Dictionaries (CRD)
  ==================================
  These are always implemented as CLUT, and always are using Lab.

  <<
    /ColorRenderingType 1
    /WhitePoint [ D50 ]
    /BlackPoint [Perfect absorber]
    /MatrixPQR [ Bradford ]
    /RangePQR [-0.125 1.375 -0.125 1.375 -0.125 1.375 ]
    /TransformPQR [            
    {4 index 3 get div 2 index 3 get mul exch pop exch pop exch pop exch pop } bind
    {4 index 4 get div 2 index 4 get mul exch pop exch pop exch pop exch pop } bind
    {4 index 5 get div 2 index 5 get mul exch pop exch pop exch pop exch pop } bind
    ]
	/MatrixABC <...>
    /EncodeABC <...>
    /RangeABC  <.. used for  XYZ -> Lab>
	/EncodeLMN
    /RenderTable [ p p p [<...>]]   
    
    /RenderingIntent (Perceptual)
  >>
  /Current exch /ColorRendering defineresource pop


  The following stages are used to convert from XYZ to Lab
  --------------------------------------------------------  

  Input is given at LMN stage on X, Y, Z

  Encode LMN gives us f(X/Xn), f(Y/Yn), f(Z/Zn)

  /EncodeLMN [
    { 0.964200  div dup 0.008856 le {7.787 mul 16 116 div add}{1 3 div exp} ifelse } bind
    { 1.000000  div dup 0.008856 le {7.787 mul 16 116 div add}{1 3 div exp} ifelse } bind
    { 0.824900  div dup 0.008856 le {7.787 mul 16 116 div add}{1 3 div exp} ifelse } bind
    ]
	
	  
  MatrixABC is used to compute f(Y/Yn), f(X/Xn) - f(Y/Yn), f(Y/Yn) - f(Z/Zn)

  | 0  1  0|
  | 1 -1  0|
  | 0  1 -1|

  /MatrixABC [ 0 1 0 1 -1 1 0 0 -1 ]

 EncodeABC finally gives Lab values.

  /EncodeABC [
    { 116 mul  16 sub 100 div  } bind
    { 500 mul 128 add 255 div  } bind
    { 200 mul 128 add 255 div  } bind
    ]   
	
  The following stages are used to convert Lab to XYZ
  ----------------------------------------------------

  	/RangeABC [ 0 1 0 1 0 1]
	/DecodeABC [ { 100 mul 16 add 116 div } bind
				 { 255 mul 128 sub 500 div } bind
				 { 255 mul 128 sub 200 div } bind 
			   ]
	
	/MatrixABC [ 1 1 1 1 0 0 0 0 -1]
	/DecodeLMN [
				{dup 6 29 div ge {dup dup mul mul} {4 29 div sub 108 841 div mul} ifelse 0.964200 mul} bind
				{dup 6 29 div ge {dup dup mul mul} {4 29 div sub 108 841 div mul} ifelse } bind
				{dup 6 29 div ge {dup dup mul mul} {4 29 div sub 108 841 div mul} ifelse 0.824900 mul} bind
				]


*/

/*

 PostScript algorithms discussion.
 ======================================================================================

  1D interpolation algorithm 


  1D interpolation (float)
  ------------------------
    
    val2 = Domain * Value;

    cell0 = (int) floor(val2);
    cell1 = (int) ceil(val2);

    rest = val2 - cell0;

    y0 = LutTable[cell0] ;
    y1 = LutTable[cell1] ;

    y = y0 + (y1 - y0) * rest;


  
  PostScript code                   Stack
  ================================================

  {                                 % v
    <check 0..1.0>
    [array]                         % v tab
    dup                             % v tab tab
    length 1 sub                    % v tab dom

    3 -1 roll                       % tab dom v

    mul                             % tab val2
    dup                             % tab val2 val2
    dup                             % tab val2 val2 val2
    floor cvi                       % tab val2 val2 cell0
    exch                            % tab val2 cell0 val2
    ceiling cvi                     % tab val2 cell0 cell1

    3 index                         % tab val2 cell0 cell1 tab  
    exch                            % tab val2 cell0 tab cell1
    get                             % tab val2 cell0 y1

    4 -1 roll                       % val2 cell0 y1 tab
    3 -1 roll                       % val2 y1 tab cell0 
    get                             % val2 y1 y0 

    dup                             % val2 y1 y0 y0
    3 1 roll                        % val2 y0 y1 y0 

    sub                             % val2 y0 (y1-y0)
    3 -1 roll                       % y0 (y1-y0) val2
    dup                             % y0 (y1-y0) val2 val2
    floor cvi                       % y0 (y1-y0) val2 floor(val2) 
    sub                             % y0 (y1-y0) rest
    mul                             % y0 t1
    add                             % y
    65535 div                       % result

  } bind


*/

static icTagSignature Device2PCS[] = {icSigAToB0Tag,       // Perceptual
                                      icSigAToB1Tag,       // Relative colorimetric
                                      icSigAToB2Tag,       // Saturation
                                      icSigAToB1Tag };     // Absolute colorimetric
                                                           // (Relative/WhitePoint)


// --------------------------------------------------------------- Memory Stream
//
// This struct holds the memory block currently being write
//

typedef struct {
                LPBYTE Block;
                LPBYTE Ptr;
                DWORD  dwMax;
                DWORD  dwUsed;          
                int    MaxCols;
                int    Col;
                int    HasError;
                
            } MEMSTREAM, FAR* LPMEMSTREAM;


typedef struct {
                LPLUT Lut;
                LPMEMSTREAM m;
                
				int FirstComponent;
				int SecondComponent;
				
                int   bps;
				char* PreMaj;
				char* PostMaj;
				char* PreMin;
				char* PostMin;

            } SAMPLERCARGO, FAR* LPSAMPLERCARGO;

// Creates a ready to use memory stream
static
LPMEMSTREAM CreateMemStream(LPBYTE Buffer, DWORD dwMax, int MaxCols)
{
    LPMEMSTREAM m = (LPMEMSTREAM) malloc(sizeof(MEMSTREAM));

    ZeroMemory(m, sizeof(MEMSTREAM));
    m -> Block   = m -> Ptr = Buffer;
    m -> dwMax   = dwMax;
    m -> dwUsed  = 0;
    m -> MaxCols = MaxCols;
    m -> Col     = 0;

    m -> HasError = 0;

    return m;
}



// Convert to byte

static
BYTE Word2Byte(WORD w)
{
    return (BYTE) floor((double) w / 257.0 + 0.5);
}

// Write a raw, uncooked byte. Check for space
static
void WriteRawByte(LPMEMSTREAM m, BYTE b)
{
    
    if (m -> dwUsed + 1 > m -> dwMax) {
        m -> HasError = 1;      
    }

    if (!m ->HasError && m ->Block) {
        *m ->Ptr++ = b;     
    }
        
    m -> dwUsed++;
}

// Write a cooked byte
static
void WriteByte(LPMEMSTREAM m, BYTE b)
{
    static const BYTE Hex[] = "0123456789ABCDEF";
    BYTE c;

        c = Hex[(b >> 4) & 0x0f];
        WriteRawByte(m, c);
    
        c = Hex[b & 0x0f];
        WriteRawByte(m, c);

        m -> Col += 2;

        if (m -> Col > m -> MaxCols) {

            WriteRawByte(m, '\n');
            m -> Col = 0;           
        }       

}

// Does write a formatted string
static
void Writef(LPMEMSTREAM m, const char *frm, ...)
{
        va_list args;
        LPBYTE pt;
        BYTE Buffer[2048];

        va_start(args, frm);

        vsprintf((char*) Buffer, frm, args);

        for (pt = Buffer; *pt; pt++)  {

            WriteRawByte(m, *pt);               
        }

        va_end(args);       
}



// ----------------------------------------------------------------- PostScript generation


// Removes offending Carriage returns
static 
char* RemoveCR(const char* txt)
{
	static char Buffer[2048];
	char* pt;

	strncpy(Buffer, txt, 2047);
	Buffer[2047] = 0;
	for (pt = Buffer; *pt; pt++)
			if (*pt == '\n' || *pt == '\r') *pt = ' ';

	return Buffer;

}

static
void EmitHeader(LPMEMSTREAM m, const char* Title, cmsHPROFILE hProfile)
{

	time_t timer;
    
    time(&timer);

	Writef(m, "%%!PS-Adobe-3.0\n");
	Writef(m, "%%\n");
	Writef(m, "%% %s\n", Title);
	Writef(m, "%% Source: %s\n", RemoveCR(cmsTakeProductName(hProfile)));
	Writef(m, "%% Description: %s\n", RemoveCR(cmsTakeProductDesc(hProfile)));
	Writef(m, "%% Created: %s", ctime(&timer)); // ctime appends a \n!!!
	Writef(m, "%%\n");
	Writef(m, "%%%%BeginResource\n");

}

static
void EmitWhiteBlackD50(LPMEMSTREAM m)
{

    Writef(m, "/BlackPoint [0.0 0.0 0.0]\n" 
              "/WhitePoint [%f %f %f]\n", cmsD50_XYZ()->X, 
                                          cmsD50_XYZ()->Y,
                                          cmsD50_XYZ()->Z);
}

static
void EmitRangeCheck(LPMEMSTREAM m)
{
    Writef(m, "dup 0.0 lt { pop 0.0 } if "
              "dup 1.0 gt { pop 1.0 } if ");

}

// Does write the intent

static
void EmitIntent(LPMEMSTREAM m, int RenderingIntent)
{
    const char *intent;

    switch (RenderingIntent) {

        case INTENT_PERCEPTUAL:            intent = "Perceptual"; break;
        case INTENT_RELATIVE_COLORIMETRIC: intent = "RelativeColorimetric"; break;
        case INTENT_ABSOLUTE_COLORIMETRIC: intent = "AbsoluteColorimetric"; break;
        case INTENT_SATURATION:            intent = "Saturation"; break;

        default: intent = "Undefined"; break;
    }

    Writef(m, "/RenderingIntent (%s)\n", intent );    
}

//
//  Convert L* to Y
//
//      Y = Yn*[ (L* + 16) / 116] ^ 3   if (L*) >= 6 / 29
//        = Yn*( L* / 116) / 7.787      if (L*) < 6 / 29
//

/*
static
void EmitL2Y(LPMEMSTREAM m)
{
    Writef(m, 
            "{ " 
                "100 mul 16 add 116 div "               // (L * 100 + 16) / 116
                 "dup 6 29 div ge "                     // >= 6 / 29 ?          
                 "{ dup dup mul mul } "                 // yes, ^3 and done
                 "{ 4 29 div sub 108 841 div mul } "    // no, slope limiting
            "ifelse } bind "); 
}
*/


// Lab -> XYZ

static
void EmitLab2XYZ(LPMEMSTREAM m)
{
	Writef(m, "/RangeABC [ 0 1 0 1 0 1]\n");
	Writef(m, "/DecodeABC [\n");
	Writef(m, "{100 mul  16 add 116 div } bind\n");
	Writef(m, "{255 mul 128 sub 500 div } bind\n");
	Writef(m, "{255 mul 128 sub 200 div } bind\n");
	Writef(m, "]\n");
	Writef(m, "/MatrixABC [ 1 1 1 1 0 0 0 0 -1]\n");
	Writef(m, "/DecodeLMN [\n");
	Writef(m, "{dup 6 29 div ge {dup dup mul mul} {4 29 div sub 108 841 div mul} ifelse 0.964200 mul} bind\n");
	Writef(m, "{dup 6 29 div ge {dup dup mul mul} {4 29 div sub 108 841 div mul} ifelse } bind\n");
	Writef(m, "{dup 6 29 div ge {dup dup mul mul} {4 29 div sub 108 841 div mul} ifelse 0.824900 mul} bind\n");
	Writef(m, "]\n");
}



// Outputs a table of words. It does use 16 bits

static
void Emit1Gamma(LPMEMSTREAM m, LPWORD Table, int nEntries)
{
    int i;
    double gamma;


    if (nEntries <= 0) return;  // Empty table

    // Suppress whole if identity
    if (cmsIsLinear(Table, nEntries)) return;

    // Check if is really an exponential. If so, emit
     gamma = cmsEstimateGammaEx(Table, nEntries, 0.001);
     if (gamma > 0) {
            Writef(m, "{ %g exp } bind ", gamma);
            return;
     }

    Writef(m, "{ ");

    // Bounds check
    EmitRangeCheck(m);
    
    // Emit intepolation code

    // PostScript code                      Stack
    // ===============                      ========================
                                            // v
    Writef(m, " [");

    // TODO: Check for endianess!!!

    for (i=0; i < nEntries; i++) {
            Writef(m, "%d ", Table[i]);
    }

    Writef(m, "] ");                        // v tab

    Writef(m, "dup ");                      // v tab tab        
    Writef(m, "length 1 sub ");             // v tab dom
    Writef(m, "3 -1 roll ");                // tab dom v
    Writef(m, "mul ");                      // tab val2
    Writef(m, "dup ");                      // tab val2 val2
    Writef(m, "dup ");                      // tab val2 val2 val2
    Writef(m, "floor cvi ");                // tab val2 val2 cell0
    Writef(m, "exch ");                     // tab val2 cell0 val2
    Writef(m, "ceiling cvi ");              // tab val2 cell0 cell1
    Writef(m, "3 index ");                  // tab val2 cell0 cell1 tab 
    Writef(m, "exch ");                     // tab val2 cell0 tab cell1
    Writef(m, "get ");                      // tab val2 cell0 y1
    Writef(m, "4 -1 roll ");                // val2 cell0 y1 tab
    Writef(m, "3 -1 roll ");                // val2 y1 tab cell0 
    Writef(m, "get ");                      // val2 y1 y0 
    Writef(m, "dup ");                      // val2 y1 y0 y0
    Writef(m, "3 1 roll ");                 // val2 y0 y1 y0 
    Writef(m, "sub ");                      // val2 y0 (y1-y0)
    Writef(m, "3 -1 roll ");                // y0 (y1-y0) val2
    Writef(m, "dup ");                      // y0 (y1-y0) val2 val2
    Writef(m, "floor cvi ");                // y0 (y1-y0) val2 floor(val2) 
    Writef(m, "sub ");                      // y0 (y1-y0) rest
    Writef(m, "mul ");                      // y0 t1
    Writef(m, "add ");                      // y
    Writef(m, "65535 div ");                // result

    Writef(m, " } bind ");
}


// Compare gamma table

static
BOOL GammaTableEquals(LPWORD g1, LPWORD g2, int nEntries)
{    
    return memcmp(g1, g2, nEntries* sizeof(WORD)) == 0;
}


// Does write a set of gamma curves

static
void EmitNGamma(LPMEMSTREAM m, int n, LPWORD g[], int nEntries)                  
{
    int i;
    
    for( i=0; i < n; i++ )
    {                
        if (i > 0 && GammaTableEquals(g[i-1], g[i], nEntries)) {

            Writef(m, "dup ");
        }
        else {    
            Emit1Gamma(m, g[i], nEntries);
        }
    }
    
}


// Extract the curve Input -> Y. Useful on Lut-based monochrome profiles


// Check whatever a profile has CLUT tables (only on input)

static
BOOL IsLUTbased(cmsHPROFILE hProfile, int Intent)
{
    icTagSignature Tag;

    // Check if adequate tag is present
    Tag = Device2PCS[Intent];
        
    if (cmsIsTag(hProfile, Tag)) return 1;

    // If not present, revert to default (perceptual)
    Tag = icSigAToB0Tag;
    
    // If no tag present, try matrix-shaper
    return cmsIsTag(hProfile, Tag);
}

// Following code dumps a LUT onto memory stream
        

// This is the sampler. Intended to work in SAMPLER_INSPECT mode,
// that is, the callback will be called for each knot with
//
//          In[]  The grid location coordinates, normalized to 0..ffff
//          Out[] The LUT values, normalized to 0..ffff
//
//  Returning a value other than 0 does terminate the sampling process
//
//  Each row contains LUT values for all but first component. So, I 
//  detect row changing by keeping a copy of last value of first 
//  component. -1 is used to mark begining of whole block.

static
int OutputValueSampler(register WORD In[], register WORD Out[], register LPVOID Cargo)
{
    LPSAMPLERCARGO sc = (LPSAMPLERCARGO) Cargo;
    unsigned int i;

	// Hadle the parenthesis on rows

    if (In[0] != sc ->FirstComponent) {
            
            if (sc ->FirstComponent != -1) {

					Writef(sc ->m, sc ->PostMin);
					sc ->SecondComponent = -1;
               		Writef(sc ->m, sc ->PostMaj);			
            }

            // Begin block  
            sc->m->Col = 0;
        			
			Writef(sc ->m, sc ->PreMaj);			
            sc ->FirstComponent = In[0]; 
    }


	  if (In[1] != sc ->SecondComponent) {
            
            if (sc ->SecondComponent != -1) {

               		Writef(sc ->m, sc ->PostMin);			
            }
            		
			Writef(sc ->m, sc ->PreMin);			
            sc ->SecondComponent = In[1]; 
    }

    
    // Dump table. Could be Word or byte based on
    // depending on bps member (16 bps mode is not currently 
	// being used at all, but is here for future ampliations)

    for (i=0; i < sc -> Lut ->OutputChan; i++) {

        WORD wWordOut = Out[i];

        if (sc ->bps == 8) {

            // Value as byte
            BYTE wByteOut = Word2Byte(wWordOut);
            WriteByte(sc -> m, wByteOut );
        }
        else {

            // Value as word    
            WriteByte(sc -> m, (BYTE) (wWordOut & 0xFF));
            WriteByte(sc -> m, (BYTE) ((wWordOut >> 8) & 0xFF));
        }
     }

    return 1;
}

// Writes a LUT on memstream. Could be 8 or 16 bits based

static
void WriteCLUT(LPMEMSTREAM m, LPLUT Lut, int bps, char* PreMaj, 
												  char* PostMaj,
												  char* PreMin,
												  char* PostMin)
{
    unsigned int i;
    SAMPLERCARGO sc;

    sc.FirstComponent = -1;
	sc.SecondComponent = -1;
    sc.Lut = Lut;
    sc.m   = m;
    sc.bps = bps;
    sc.PreMaj = PreMaj;
	sc.PostMaj= PostMaj;

	sc.PreMin = PreMin;
	sc.PostMin= PostMin;

    Writef(m, "[");

    for (i=0; i < Lut ->InputChan; i++)
            Writef(m, " %d ", Lut ->cLutPoints);

    Writef(m, " [\n");

  

    cmsSample3DGrid(Lut, OutputValueSampler, (LPVOID) &sc, SAMPLER_INSPECT);
    
    
	Writef(m, PostMin);
	Writef(m, PostMaj);
	Writef(m, "] ");

	

}


// Dumps CIEBasedA Color Space Array

static
int EmitCIEBasedA(LPMEMSTREAM m, LPWORD Tab, int nEntries)
{
            
        Writef(m, "[ /CIEBasedA\n");
        Writef(m, "  <<\n");

        Writef(m, "/DecodeA [ ");

        Emit1Gamma(m,Tab, nEntries);

        Writef(m, " ]\n");

        Writef(m, "/MatrixA [ 0.9642 1.0000 0.8249 ]\n");
        Writef(m, "/RangeLMN [ 0.0 0.9642 0.0 1.0000 0.0 0.8249 ]\n");
        
        EmitWhiteBlackD50(m);
        EmitIntent(m, INTENT_PERCEPTUAL);

        Writef(m, ">>\n");        
        Writef(m, "] /Current exch /ColorSpace defineresource pop\n");
        
        return 1;
}


// Dumps CIEBasedABC Color Space Array

static
int EmitCIEBasedABC(LPMEMSTREAM m, LPWORD L[], int nEntries, LPWMAT3 Matrix)
{
    int i;

        Writef(m, "[ /CIEBasedABC\n");
        Writef(m, "<<\n");
        Writef(m, "/DecodeABC [ ");
 
        EmitNGamma(m, 3, L, nEntries);

        Writef(m, "]\n");
        
        Writef(m, "/MatrixABC [ " );

        for( i=0; i < 3; i++ ) {
            
            Writef(m, "%.6f %.6f %.6f ", 
                        FIXED_TO_DOUBLE(Matrix->v[0].n[i]),
                        FIXED_TO_DOUBLE(Matrix->v[1].n[i]),
                        FIXED_TO_DOUBLE(Matrix->v[2].n[i]));            
        }

    
        Writef(m, "]\n");

        Writef(m, "/RangeLMN [ 0.0 0.9642 0.0 1.0000 0.0 0.8249 ]\n");
		
        EmitWhiteBlackD50(m);
        EmitIntent(m, INTENT_PERCEPTUAL);

        Writef(m, ">>\n");       
        Writef(m, "] /Current exch /ColorSpace defineresource pop\n");

        return 1;
}


static
int EmitCIEBasedDEF(LPMEMSTREAM m, LPLUT Lut, int Intent)
{
	char* PreMaj;
	char* PostMaj;
	char* PreMin, *PostMin;

    switch (Lut ->InputChan) {
    case 3:

            Writef(m, "[ /CIEBasedDEF\n");
			PreMaj ="<"; 
			PostMaj= ">\n";
			PreMin = PostMin = "";
            break;
    case 4:
            Writef(m, "[ /CIEBasedDEFG\n");
			PreMaj = "[";
			PostMaj = "]\n";
			PreMin = "<";
			PostMin = ">\n";
            break;
    default:
            return 0;

    }

    Writef(m, "<<\n");

    if (Lut ->wFlags & LUT_HASTL1) {
    
        Writef(m, "/DecodeDEF [ ");
        EmitNGamma(m, Lut ->InputChan, Lut ->L1, Lut ->CLut16params.nSamples);
        Writef(m, "]\n");
    }



    if (Lut ->wFlags & LUT_HAS3DGRID) {

            Writef(m, "/Table ");    
            WriteCLUT(m, Lut, 8, PreMaj, PostMaj, PreMin, PostMin);
            Writef(m, "]\n");
    }
	   
	EmitLab2XYZ(m);
    EmitWhiteBlackD50(m);
    EmitIntent(m, Intent);

    Writef(m, "   >>\n");       
    Writef(m, "] /Current exch /ColorSpace defineresource pop\n");

    return 1;
}



// Because PostScrip has only 8 bits in /Table, we should use
// a more perceptually uniform space... I do choose Lab.

static
int WriteInputLUT(LPMEMSTREAM m, cmsHPROFILE hProfile, int Intent)
{
    cmsHPROFILE hLab;
    cmsHTRANSFORM xform;
    icColorSpaceSignature ColorSpace;
    int nChannels;
    DWORD InputFormat;
    int rc;
    cmsHPROFILE Profiles[2];

    // Does create a device-link based transform. 
    // The DeviceLink is next dumped as working CSA.
    
    hLab        = cmsCreateLabProfile(NULL);
    ColorSpace  =  cmsGetColorSpace(hProfile);
    nChannels   = _cmsChannelsOf(ColorSpace);
    InputFormat = CHANNELS_SH(nChannels) | BYTES_SH(2);

    // Is a devicelink profile?
    if (cmsGetDeviceClass(hProfile) == icSigLinkClass) {

        // if devicelink output already Lab, use it directly

        if (cmsGetPCS(hProfile) == icSigLabData) {
            
            xform = cmsCreateTransform(hProfile, InputFormat, NULL, 
                            TYPE_Lab_DBL, Intent, 0);
        }
        else {

            // Nope, adjust output to Lab if possible

            Profiles[0] = hProfile;
            Profiles[1] = hLab;

            xform = cmsCreateMultiprofileTransform(Profiles, 2,  InputFormat, 
                                    TYPE_Lab_DBL, Intent, 0);
        }


    }
    else {

        // This is a normal profile
        xform = cmsCreateTransform(hProfile, InputFormat, hLab, 
                            TYPE_Lab_DBL, Intent, 0);
    }


    
    if (xform == NULL) {
                        
            cmsSignalError(LCMS_ERRC_ABORTED, "Cannot create transform Profile -> Lab");
            return 0;
    }

    // Only 1, 3 and 4 channels are allowed

    switch (nChannels) {

    case 1: {

            // LPGAMMATABLE Gray2Y = ExtractGray2Y(xform);
            // rc = EmitCIEBasedA(m, Gray2Y->GammaTable, Gray2Y ->nEntries, hProfile);
            // cmsFreeGamma(Gray2Y);

			cmsSignalError(LCMS_ERRC_ABORTED, "Monochrome LUT-based currently unsupported for CSA generation");
            }
            break;

    case 3: 
    case 4: {
            LPLUT DeviceLink;
            _LPcmsTRANSFORM v = (_LPcmsTRANSFORM) xform;

            if (v ->DeviceLink) 
                rc = EmitCIEBasedDEF(m, v->DeviceLink, Intent);
            else {
                DeviceLink = _cmsPrecalculateDeviceLink(xform, 0);
                rc = EmitCIEBasedDEF(m, DeviceLink, Intent);
                cmsFreeLUT(DeviceLink);
            }
            }
            break;

    default:

            cmsSignalError(LCMS_ERRC_ABORTED, "Only 3, 4 channels supported for CSA. This profile has %d channels.", nChannels);
            return 0;
    }
    

    cmsDeleteTransform(xform);
    cmsCloseProfile(hLab);
    return 1;
}



// Does create CSA based on matrix-shaper. Allowed types are gray and RGB based

static
int WriteInputMatrixShaper(LPMEMSTREAM m, cmsHPROFILE hProfile)
{
    icColorSpaceSignature ColorSpace;
    LPMATSHAPER MatShaper;
    int rc;

    ColorSpace = cmsGetColorSpace(hProfile);
    MatShaper  = cmsBuildInputMatrixShaper(hProfile, NULL);

    if (MatShaper == NULL) {

                cmsSignalError(LCMS_ERRC_ABORTED, "This profile is not suitable for input");
                return 0;
    }

    if (ColorSpace == icSigGrayData) {
            
            rc = EmitCIEBasedA(m, MatShaper ->L[0], 
                                  MatShaper ->p16.nSamples);
        
    }
    else
        if (ColorSpace == icSigRgbData) {

        
            rc = EmitCIEBasedABC(m, MatShaper->L, 
                                        MatShaper ->p16.nSamples, 
                                        &MatShaper ->Matrix);      
        }
        else  {

            cmsSignalError(LCMS_ERRC_ABORTED, "Profile is not suitable for CSA. Unsupported colorspace.");
            return 0;
        }

    cmsFreeMatShaper(MatShaper);
    return rc;
}



// Creates a PostScript color list from a named profile data. 
// This is a HP extension, and it works in Lab instead of XYZ

static
int WriteNamedColorCSA(LPMEMSTREAM m, cmsHPROFILE hNamedColor, int Intent)
{
    cmsHTRANSFORM xform;
    cmsHPROFILE   hLab;
    int i, nColors;
    char ColorName[32];


    hLab  = cmsCreateLabProfile(NULL);
    xform = cmsCreateTransform(hNamedColor, TYPE_NAMED_COLOR_INDEX, 
                        hLab, TYPE_Lab_DBL, Intent, cmsFLAGS_NOTPRECALC);
    if (xform == NULL) return 0;


    Writef(m, "<<\n");
    Writef(m, "(colorlistcomment) (%s)\n", "Named color CSA");
    Writef(m, "(Prefix) [ (Pantone ) (PANTONE ) ]\n");
    Writef(m, "(Suffix) [ ( CV) ( CVC) ( C) ]\n");

    nColors   = cmsNamedColorCount(xform);


    for (i=0; i < nColors; i++) {
        
        WORD In[1];
        cmsCIELab Lab;

        In[0] = (WORD) i;

        if (!cmsNamedColorInfo(xform, i, ColorName, NULL, NULL))
                continue;

        cmsDoTransform(xform, In, &Lab, 1);     
        Writef(m, "  (%s) [ %.3f %.3f %.3f ]\n", ColorName, Lab.L, Lab.a, Lab.b);
    }


    
    Writef(m, ">> /Current exch /HPSpotTable defineresource pop\n");

    cmsDeleteTransform(xform);
    cmsCloseProfile(hLab);
    return 1;
}


// Does create a Color Space Array on XYZ colorspace for PostScript usage

DWORD LCMSEXPORT cmsGetPostScriptCSA(cmsHPROFILE hProfile, 
                              int Intent, 
                              LPVOID Buffer, DWORD dwBufferLen)
{
    
    LPMEMSTREAM mem;
    DWORD dwBytesUsed;
    
    // Set up the serialization engine
    mem = CreateMemStream(Buffer, dwBufferLen, MAXPSCOLS);
    if (!mem) return 0;

	EmitHeader(mem, "Color Space Array (CSA)", hProfile);

    // Is a named color profile?
    if (cmsGetDeviceClass(hProfile) == icSigNamedColorClass) {

        if (!WriteNamedColorCSA(mem, hProfile, Intent)) {

                    free((void*) mem);
                    return 0;
        }
    }
    else {


    // Any profile class are allowed (including devicelink), but
    // output (PCS) colorspace must be XYZ or Lab
    icColorSpaceSignature ColorSpace = cmsGetPCS(hProfile);

    if (ColorSpace != icSigXYZData &&
        ColorSpace != icSigLabData) {

            cmsSignalError(LCMS_ERRC_ABORTED, "Invalid output color space");
            free((void*) mem);
                    return 0;
    }
    
    // Is there any CLUT?
    if (IsLUTbased(hProfile, Intent)) {

        // Yes, so handle as LUT-based
        if (!WriteInputLUT(mem, hProfile, Intent)) {

                    free((void*) mem);
                    return 0;
        }
    }
    else {
        
        // No, try Matrix-shaper (this only works on XYZ)

        if (!WriteInputMatrixShaper(mem, hProfile)) {

                    free((void*) mem);  // Something went wrong
                    return 0;
        }
    }
    }

	Writef(mem, "%%%%EndResource\n");
	Writef(mem, "\n%% CSA End\n");

    // Done, keep memory usage
    dwBytesUsed = mem ->dwUsed;

    // Get rid of memory stream
    free((void*) mem);

    // Finally, return used byte count
    return dwBytesUsed;
}

// ------------------------------------------------------ Color Rendering Dictionary (CRD)


static
void EmitPQRStage(LPMEMSTREAM m)
{
    Writef(m, "/MatrixPQR [0.40024 -0.2263 0.0 0.7076 1.16532 0.0 -0.08081 0.0457 0.91822 ]\n");
    Writef(m, "/RangePQR [-0.125 1.375 -0.125 1.375 -0.125 1.375 ]\n");
    Writef(m, "/TransformPQR [\n");
    Writef(m, "{4 index 3 get div 2 index 3 get mul exch pop exch pop exch pop exch pop } bind\n");
    Writef(m, "{4 index 4 get div 2 index 4 get mul exch pop exch pop exch pop exch pop } bind\n");
    Writef(m, "{4 index 5 get div 2 index 5 get mul exch pop exch pop exch pop exch pop } bind\n");
    Writef(m, "]\n");
}


static
void EmitXYZ2Lab(LPMEMSTREAM m)
{
    Writef(m, "/EncodeLMN [\n");
    Writef(m, "{ 0.964200  div dup 0.008856 le {7.787 mul 16 116 div add}{1 3 div exp} ifelse } bind\n");
    Writef(m, "{ 1.000000  div dup 0.008856 le {7.787 mul 16 116 div add}{1 3 div exp} ifelse } bind\n");
    Writef(m, "{ 0.824900  div dup 0.008856 le {7.787 mul 16 116 div add}{1 3 div exp} ifelse } bind\n");
    Writef(m, "]\n");
    Writef(m, "/MatrixABC [ 0 1 0 1 -1 1 0 0 -1 ]\n");
    Writef(m, "/EncodeABC [\n");
    Writef(m, "{ 116 mul  16 sub 100 div  } bind\n");
    Writef(m, "{ 500 mul 128 add 255 div  } bind\n");
    Writef(m, "{ 200 mul 128 add 255 div  } bind\n");
    Writef(m, "]\n");
    // Writef(m, "/RangeABC [ 0 1 0 1 0 1]\n");

}

// Due to impedance mismatch between XYZ and almost all RGB and CMYK spaces
// I choose to dump LUTS in Lab instead of XYZ. There is still a lot of wasted
// space on 3D CLUT, but since space seems not to be a problem here, 33 points
// would give a reasonable accurancy. Note also that CRD tables must operate in 
// 8 bits.

static
int WriteOutputLUT(LPMEMSTREAM m, cmsHPROFILE hProfile, int Intent)
{
    cmsHPROFILE hLab;
    cmsHTRANSFORM xform;
    icColorSpaceSignature ColorSpace;
    int i, nChannels;
    DWORD OutputFormat;
    _LPcmsTRANSFORM v;
    LPLUT DeviceLink;
    cmsHPROFILE Profiles[3];
	BOOL lFreeDeviceLink = FALSE;

    
    hLab         = cmsCreateLabProfile(NULL);
    ColorSpace  =  cmsGetColorSpace(hProfile);
    nChannels   = _cmsChannelsOf(ColorSpace);
    OutputFormat = CHANNELS_SH(nChannels) | BYTES_SH(2);

    
    // Is a devicelink profile?
    if (cmsGetDeviceClass(hProfile) == icSigLinkClass) {

        // if devicelink input already in Lab, use it directly

        if (ColorSpace == icSigLabData) {
            
            xform = cmsCreateTransform(hProfile, TYPE_Lab_DBL, NULL, 
                            OutputFormat, Intent, 0);
        }
        else {

            // Nope, adjust input to Lab if possible

            Profiles[0] = hLab;
            Profiles[1] = hProfile;

            xform = cmsCreateMultiprofileTransform(Profiles, 2, TYPE_Lab_DBL, 
                                    OutputFormat, Intent, 0);
        }


    }
    else {

        // This is a normal profile
        xform = cmsCreateTransform(hLab, TYPE_Lab_DBL, 
                                  hProfile, OutputFormat, 
                                  Intent, 0);
    }

    if (xform == NULL) {
                        
            cmsSignalError(LCMS_ERRC_ABORTED, "Cannot create transform Lab -> Profile");
            return 0;
    }

    // Get the internal precalculated devicelink

    v = (_LPcmsTRANSFORM) xform;
    DeviceLink = v ->DeviceLink;
	
	if (!DeviceLink) {

		DeviceLink = _cmsPrecalculateDeviceLink(xform, 0);
		lFreeDeviceLink = TRUE;
	}

    Writef(m, "<<\n");
    Writef(m, "/ColorRenderingType 1\n");

    // Emit headers, etc.
    EmitWhiteBlackD50(m);
    EmitPQRStage(m);
    EmitXYZ2Lab(m);
        
    if (DeviceLink ->wFlags & LUT_HASTL1) {

        // Shouldn't happen
        cmsSignalError(LCMS_ERRC_ABORTED, "Internal error (prelinearization on CRD)");

        /*
        Writef(m, "/EncodeABC [ ");
        EmitNGamma(m, DeviceLink ->InputChan, DeviceLink ->L1, DeviceLink ->CLut16params.nSamples);
        Writef(m, "]\n");
        */
    }
    
    Writef(m, "/RenderTable ");
    
    WriteCLUT(m, DeviceLink, 8, "<", ">\n", "", "");

    Writef(m, " %d {} bind ", nChannels);

	for (i=1; i < nChannels; i++)
			Writef(m, "dup ");

    Writef(m, "]\n");

        
    EmitIntent(m, Intent);

    Writef(m, ">>\n");
    Writef(m, "/Current exch /ColorRendering defineresource pop\n");

    if (lFreeDeviceLink) cmsFreeLUT(DeviceLink);
    cmsDeleteTransform(xform);
    cmsCloseProfile(hLab);

    return 1;   
}


// Builds a ASCII string containing colorant list in 0..1.0 range
static
void BuildColorantList(char *Colorant, int nColorant, WORD Out[])
{
    char Buff[32];
    int j;

    Colorant[0] = 0;
    for (j=0; j < nColorant; j++) {

                sprintf(Buff, "%.3f", Out[j] / 65535.0);
                strcat(Colorant, Buff);
                if (j < nColorant -1) 
                        strcat(Colorant, " ");

        }       
}


// Creates a PostScript color list from a named profile data. 
// This is a HP extension.

static
int WriteNamedColorCRD(LPMEMSTREAM m, cmsHPROFILE hNamedColor, int Intent)
{
    cmsHTRANSFORM xform;    
    int i, nColors, nColorant;
    DWORD OutputFormat;
    char ColorName[32];
    char Colorant[128];

    nColorant = _cmsChannelsOf(cmsGetColorSpace(hNamedColor));
    OutputFormat = CHANNELS_SH(nColorant) | BYTES_SH(2);

    xform = cmsCreateTransform(hNamedColor, TYPE_NAMED_COLOR_INDEX, 
                        NULL, OutputFormat, Intent, cmsFLAGS_NOTPRECALC);
    if (xform == NULL) return 0;


    Writef(m, "<<\n");
    Writef(m, "(colorlistcomment) (%s) \n", "Named profile");
    Writef(m, "(Prefix) [ (Pantone ) (PANTONE ) ]\n");
    Writef(m, "(Suffix) [ ( CV) ( CVC) ( C) ]\n");

    nColors   = cmsNamedColorCount(xform);
    

    for (i=0; i < nColors; i++) {
        
        WORD In[1];
        WORD Out[MAXCHANNELS];

        In[0] = (WORD) i;

        if (!cmsNamedColorInfo(xform, i, ColorName, NULL, NULL))
                continue;

        cmsDoTransform(xform, In, Out, 1);      
        BuildColorantList(Colorant, nColorant, Out);
        Writef(m, "  (%s) [ %s ]\n", ColorName, Colorant);
    }

    
    Writef(m, "   >> /Current exch /HPSpotTable defineresource pop\n");

    cmsDeleteTransform(xform);  
    return 1;
}



// This one does create a Color Rendering Dictionary. 
// CRD are always LUT-Based, no matter if profile is
// implemented as matrix-shaper.

DWORD LCMSEXPORT cmsGetPostScriptCRD(cmsHPROFILE hProfile, 
                              int Intent, 
                              LPVOID Buffer, DWORD dwBufferLen)
{
    
    LPMEMSTREAM mem;
    DWORD dwBytesUsed;

    // Set up the serialization artifact
    mem = CreateMemStream(Buffer, dwBufferLen, MAXPSCOLS);
    if (!mem) return 0;

    
	EmitHeader(mem, "Color Rendering Dictionary (CRD)", hProfile);

    // Is a named color profile?
    if (cmsGetDeviceClass(hProfile) == icSigNamedColorClass) {

        if (!WriteNamedColorCRD(mem, hProfile, Intent)) {

                    free((void*) mem);
                    return 0;
        }
    }
    else {
        
    // CRD are always implemented as LUT. 


    if (!WriteOutputLUT(mem, hProfile, Intent)) {
        free((void*) mem);
        return 0;
    }
    }
     
	Writef(mem, "%%%%EndResource\n");
	Writef(mem, "\n%% CRD End\n");

    // Done, keep memory usage
    dwBytesUsed = mem ->dwUsed;

    // Get rid of memory stream
    free((void*) mem);

    // Finally, return used byte count
    return dwBytesUsed;
}

