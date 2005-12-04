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

#include <stdarg.h>
#include <ctype.h>

// xgetopt() interface -----------------------------------------------------

extern int   xoptind;
extern char *xoptarg;
extern int   xopterr;
extern char  SW;
int    cdecl xgetopt(int argc, char *argv[], char *optionS);

// ------------------------------------------------------------------------

// Stock profiles function
extern cmsHPROFILE OpenStockProfile(const char* File);


static BOOL InHexa          = FALSE;
static BOOL Verbose         = FALSE;
static BOOL GamutCheck      = FALSE;

static char *cInProf   = NULL;
static char *cOutProf  = NULL;
static char *cProofing = NULL;

static LCMSHANDLE hIT8in;		// CGATS input 

static char CGATSPatch[1024];	// Patch Name

static int Intent           = INTENT_PERCEPTUAL;
static int ProofingIntent   = INTENT_PERCEPTUAL;

static BOOL Width16 = FALSE;
static BOOL BlackPointCompensation = FALSE;

static int PrecalcMode             = 0;
static BOOL lIsDeviceLink          = FALSE;
static BOOL lTerse     = FALSE;

static cmsHPROFILE hInput, hOutput, hProof, hLab, hXYZ;
static cmsHTRANSFORM hTrans, hTransXYZ, hTransLab;

static icColorSpaceSignature InputColorSpace, OutputColorSpace;
static cmsCIEXYZ xyz;
static cmsCIELab Lab;

#ifdef _MS_VER
#define xisatty(x) _isatty( _fileno( (x) ) )
#else
#define xisatty(x) isatty( fileno( (x) ) )
#endif



static
void FatalError(const char *frm, ...)
{
       va_list args;

       va_start(args, frm);
       vfprintf(stderr, frm, args);
       va_end(args);

       exit(1);
}

static
void Warning(const char *frm, ...)
{
       va_list args;

       va_start(args, frm);
       vfprintf(stderr, frm, args);
       va_end(args);
}

static
int MyErrorHandler(int ErrorCode, const char *ErrorText)
{
    FatalError("icctrans: %s", ErrorText);
    return 0;
}



// The toggles stuff

static
void HandleSwitches(int argc, char *argv[])
{
       int s;
      
       while ((s = xgetopt(argc,argv,"%C:c:VvWwxXhHbBnNI:i:O:o:T:t:L:l:p:P:m:M:gG")) != EOF) {

       switch (s){
       
       case 'b':
       case 'B': 
           BlackPointCompensation = TRUE;
           break;

     
       case 'c':
       case 'C':
            PrecalcMode = atoi(xoptarg);
            if (PrecalcMode < 0 || PrecalcMode > 3)
                    FatalError("Unknown precalc mode '%d'", PrecalcMode);
            break;

       case 'x':
       case 'X':
            InHexa = TRUE;
            break;

       case 'v':
       case 'V':
            Verbose = TRUE;
            break;

       case 'n':
       case 'N':
           lTerse = TRUE;
           break;

       case 'i':
       case 'I':
            if (lIsDeviceLink)
                   FatalError("Device-link already specified");

            cInProf = xoptarg;
            break;

       case 'o':
       case 'O':
            if (lIsDeviceLink)
                   FatalError("Device-link already specified"); 
            cOutProf = xoptarg;
            break;


       case 't':
       case 'T':
            Intent = atoi(xoptarg);
            if (Intent > 3) Intent = 3;
            if (Intent < 0) Intent = 0;
            break;
     
       case 'W':
       case 'w':
            Width16 = TRUE;
            break;

       case 'l':
       case 'L': 
            cInProf = xoptarg;
            lIsDeviceLink = TRUE;
            break;


       case 'p':
       case 'P':
           cProofing = xoptarg;
           break;

       case 'm':
       case 'M':
            ProofingIntent = atoi(xoptarg);
            if (ProofingIntent > 3) ProofingIntent = 3;
            if (ProofingIntent < 0) ProofingIntent = 0;
            break;

        case 'g':
        case 'G':
            GamutCheck = TRUE;
            break;

  default:

       FatalError("Unknown option - run without args to see valid ones.\n");
    }
       
    }
}




// Creates all needed color transforms

static
void OpenTransforms(void)
{

    DWORD dwIn, dwOut, dwFlags;

    dwFlags = 0;
    
    
    if (lIsDeviceLink) {

            hInput  = cmsOpenProfileFromFile(cInProf, "r");
            hOutput = NULL;
            InputColorSpace   = cmsGetColorSpace(hInput);
            OutputColorSpace = cmsGetPCS(hInput);
        
            
       }
    else {

            hInput  = OpenStockProfile(cInProf);
            hOutput = OpenStockProfile(cOutProf);    
            hProof  = NULL;

            if (cProofing != NULL) {

                   hProof = OpenStockProfile(cProofing);
                   dwFlags |= cmsFLAGS_SOFTPROOFING;
            }

            InputColorSpace   = cmsGetColorSpace(hInput);
            OutputColorSpace  = cmsGetColorSpace(hOutput);

            if (cmsGetDeviceClass(hInput) == icSigLinkClass ||
                cmsGetDeviceClass(hOutput) == icSigLinkClass)   
                        FatalError("Use %cl flag for devicelink profiles!\n", SW);
    
    }

       hXYZ    = cmsCreateXYZProfile();
       hLab    = cmsCreateLabProfile(NULL);


       if (Verbose) {

            printf("From: %s\n", cmsTakeProductName(hInput));
            printf("Desc: %s\n", cmsTakeProductDesc(hInput));
            if (hOutput) printf("To  : %s\n\n", cmsTakeProductName(hOutput));

            if (cmsIsTag(hInput, icSigColorantTableTag)) {

                LPcmsNAMEDCOLORLIST list = cmsReadColorantTable(hInput, icSigColorantTableTag);
                int i;

                for (i=0; i < list ->nColors; i++)
                    printf("%s\n", list ->List[i].Name);

                cmsFreeNamedColorList(list);

            }

            if (cmsIsTag(hInput, icSigColorantTableOutTag)) {
                printf("...");
            }
       }

      
       dwIn  = BYTES_SH(2) | CHANNELS_SH(_cmsChannelsOf(InputColorSpace));
       dwOut = BYTES_SH(2) | CHANNELS_SH(_cmsChannelsOf(OutputColorSpace));
              
       switch (PrecalcMode) {
           
       case 0: dwFlags |= cmsFLAGS_NOTPRECALC; break;
       case 2: dwFlags |= cmsFLAGS_HIGHRESPRECALC; break;
	   case 3: dwFlags |= cmsFLAGS_LOWRESPRECALC; break;
	   case 1: break;

       default: FatalError("Unknown precalculation mode '%d'", PrecalcMode);
       }
       
       
       if (BlackPointCompensation) 
            dwFlags |= cmsFLAGS_BLACKPOINTCOMPENSATION;

       if (GamutCheck) {

            if (hProof == NULL)
                FatalError("I need proofing profile -p for gamut checking!");

            cmsSetAlarmCodes(0xFF, 0xFF, 0xFF);
            dwFlags |= cmsFLAGS_GAMUTCHECK;            
       }

       if (cmsGetDeviceClass(hInput) == icSigNamedColorClass) {
           dwIn = TYPE_NAMED_COLOR_INDEX;
       }

       hTrans     = cmsCreateProofingTransform(hInput,  dwIn,
                                               hOutput, dwOut,
                                               hProof,
                                               Intent, ProofingIntent, dwFlags);


       
       
       hTransXYZ = NULL; hTransLab = NULL;
       if (hOutput && Verbose) {

            hTransXYZ = cmsCreateTransform(hInput, dwIn,
                                      hXYZ,  TYPE_XYZ_16,
                                      Intent, cmsFLAGS_NOTPRECALC);

            hTransLab = cmsCreateTransform(hInput, dwIn,
                                      hLab,  TYPE_Lab_16,
                                      Intent, cmsFLAGS_NOTPRECALC);    
       }
       
}


static
void CloseTransforms(void)
{
       cmsDeleteTransform(hTrans);
       if (hTransLab) cmsDeleteTransform(hTransLab);
       if (hTransXYZ) cmsDeleteTransform(hTransXYZ);
       cmsCloseProfile(hInput);
       if (hOutput) cmsCloseProfile(hOutput); 
       if (hProof) cmsCloseProfile(hProof);
       cmsCloseProfile(hXYZ);
       cmsCloseProfile(hLab);

}


static
void PrintRange(const char* C, double v, double Range)
{
    char Prefix[20];

    Prefix[0] = 0;
    if (!lTerse)
        sprintf(Prefix, "%s=", C);

    if (InHexa)
    {
        if (Width16)
            printf("%s0x%x ", Prefix, (int) floor(v + .5));
        else
            printf("%s0x%x ", Prefix, (int) floor(v / 257. + .5));

    }
    else
    {       

        double out = (v * Range) / 65535.0;        
        printf("%s%.2f ", Prefix, out);
    }
}


static
void PrintOne(const char* C, double v)
{
       PrintRange(C, v, 255.0);
}

static
void Print100(const char* C, double v)
{
       PrintRange(C, v, 100.0);
}

static
void PrintCooked(const char* C, double v)
{
    if (lTerse)
        printf("%.4f ", v);
    else
        printf("%s=%.4f ", C, v);
}


static
void PrintResults(WORD Encoded[], icColorSpaceSignature ColorSpace)
{
    int i;

    switch (ColorSpace) {

    case icSigXYZData:
                    cmsXYZEncoded2Float(&xyz, Encoded);
                    PrintCooked("X", xyz.X * 100.); 
                    PrintCooked("Y", xyz.Y * 100.); 
                    PrintCooked("Z", xyz.Z * 100.);
                    break;

    case icSigLabData:
                    cmsLabEncoded2Float(&Lab, Encoded);
                    PrintCooked("L*", Lab.L); 
                    PrintCooked("a*", Lab.a); 
                    PrintCooked("b*", Lab.b);
                    break;

    case icSigLuvData:
                    PrintOne("L", Encoded[0]); 
                    PrintOne("u", Encoded[1]); 
                    PrintOne("v", Encoded[2]);
                    break;

    case icSigYCbCrData:
                    PrintOne("Y", Encoded[0]); 
                    PrintOne("Cb", Encoded[1]); 
                    PrintOne("Cr", Encoded[2]);
                    break;


    case icSigYxyData:
                    PrintOne("Y", Encoded[0]); 
                    PrintOne("x", Encoded[1]); 
                    PrintOne("y", Encoded[2]);
                    break;

    case icSigRgbData:
                    PrintOne("R", Encoded[0]); 
                    PrintOne("G", Encoded[1]); 
                    PrintOne("B", Encoded[2]);
                    break;

    case icSigGrayData:
                    PrintOne("G", Encoded[0]); 
                    break;

    case icSigHsvData:
                    PrintOne("H", Encoded[0]); 
                    PrintOne("s", Encoded[1]); 
                    PrintOne("v", Encoded[2]);
                    break;

    case icSigHlsData:
                    PrintOne("H", Encoded[0]); 
                    PrintOne("l", Encoded[1]); 
                    PrintOne("s", Encoded[2]);
                    break;

    case icSigCmykData:
                    Print100("C", Encoded[0]); 
                    Print100("M", Encoded[1]); 
                    Print100("Y", Encoded[2]); 
                    Print100("K", Encoded[3]);
                    break;

    case icSigCmyData:                        
                    Print100("C", Encoded[0]); 
                    Print100("M", Encoded[1]); 
                    Print100("Y", Encoded[2]); 
                    break;

    case icSigHexachromeData:
    case icSig6colorData:
                            
                    Print100("C", Encoded[0]); 
                    Print100("M", Encoded[1]); 
                    Print100("Y", Encoded[2]); 
                    Print100("K", Encoded[3]); 
                    Print100("c", Encoded[1]); 
                    Print100("m", Encoded[2]); 
                    break;

    default:

        for (i=0; i < _cmsChannelsOf(OutputColorSpace); i++) {
        
            char Buffer[10];
            sprintf(Buffer, "CHAN%d", i + 1);
            PrintOne(Buffer, Encoded[i]);           
        }   
    }

    printf("\n");
}



static
void GetLine(char* Buffer)
{    
    scanf("%s", Buffer);
    
    if (toupper(Buffer[0]) == 'Q') { // Quit?

        CloseTransforms();

        if (xisatty(stdin))  
            printf("Done.\n");

        exit(0);        
    }

}


static
double GetAnswer(const char* Prompt, double Range)
{
    char Buffer[4096];
    double val = 0.0;
    
    
    if (Range == 0.0) {              // Range 0 means double value
        
        if (xisatty(stdin)) printf("%s? ", Prompt);
        GetLine(Buffer);
        return atof(Buffer);
        
    }
    else {
        
        if (InHexa) {   // Hexadecimal
            
            int hexa;
            
            if (Width16)
                Range = 0xFFFF;
            else
                Range = 0xFF;
            
            if (xisatty(stdin)) printf("%s (0..%X)? ", Prompt, (int) Range);
            GetLine(Buffer);
            sscanf(Buffer, "%x", &hexa);
            val = hexa;
        }
        else {                      // Normal usage
            
            if (xisatty(stdin)) printf("%s (0..%d)? ", Prompt, (int) Range);
            GetLine(Buffer);
            sscanf(Buffer, "%lf", &val);
        }
        
        // Normalize to 0..0xffff
        
        return floor((val * 65535.0) / Range + 0.5);            
    }    
}


static
WORD Get100(const char* AskFor)
{
    return (WORD) GetAnswer(AskFor, 100.0);
}


static 
WORD GetVal(const char* AskFor)
{
    return (WORD) GetAnswer(AskFor, 255.0);
}


static
double GetDbl(const char* AskFor)
{
    return GetAnswer(AskFor, 0.0);
}


static
WORD GetIndex(void)
{
    char Buffer[4096], Name[40], Prefix[40], Suffix[40];
    int index, max;

    max = cmsNamedColorCount(hTrans)-1;

    if (xisatty(stdin)) printf("Color index (0..%d)? ", max);

    GetLine(Buffer);
    index = atoi(Buffer);

    if (index > max)
            FatalError("Named color %d out of range!", index);

    cmsNamedColorInfo(hTrans, index, Name, Prefix, Suffix);

    printf("\n%s %s %s: ", Prefix, Name, Suffix);

    return index;
}



// Read values from a text file or terminal

static
void TakeTextValues(WORD Encoded[])
{

    if (xisatty(stdin))
        printf("\nEnter values, 'q' to quit\n");

    if (cmsGetDeviceClass(hInput) == icSigNamedColorClass) {
        Encoded[0] = GetIndex();
        return;
    }

    switch (InputColorSpace) {

    case icSigXYZData:
        xyz.X = GetDbl("X"); 
        xyz.Y = GetDbl("Y"); 
        xyz.Z = GetDbl("Z");
        cmsFloat2XYZEncoded(Encoded, &xyz);                 
        break;

    case icSigLabData:
        Lab.L = GetDbl("L*"); 
        Lab.a = GetDbl("a*"); 
        Lab.b = GetDbl("b*");
        cmsFloat2LabEncoded(Encoded, &Lab);                 
        break;

    case icSigLuvData:
        Encoded[0] = GetVal("L"); 
        Encoded[1] = GetVal("u"); 
        Encoded[2] = GetVal("v"); 
        break;

    case icSigYCbCrData:
        Encoded[0] = GetVal("Y"); 
        Encoded[1] = GetVal("Cb"); 
        Encoded[2] = GetVal("Cr"); 
        break;


    case icSigYxyData:
        Encoded[0] = GetVal("Y"); 
        Encoded[1] = GetVal("x"); 
        Encoded[2] = GetVal("y"); 
        break;

    case icSigRgbData:
        Encoded[0] = GetVal("R"); 
        Encoded[1] = GetVal("G"); 
        Encoded[2] = GetVal("B"); 
        break;

    case icSigGrayData:
        Encoded[0] = GetVal("G");
        break;

    case icSigHsvData:
        Encoded[0] = GetVal("H"); 
        Encoded[1] = GetVal("s"); 
        Encoded[2] = GetVal("v"); 
        break;

    case icSigHlsData:
        Encoded[0] = GetVal("H"); 
        Encoded[1] = GetVal("l"); 
        Encoded[2] = GetVal("s"); 
        break;

    case icSigCmykData:
        Encoded[0] = Get100("C"); 
        Encoded[1] = Get100("M"); 
        Encoded[2] = Get100("Y"); 
        Encoded[3] = Get100("K"); 
        break;

    case icSigCmyData:                        
        Encoded[0] = Get100("C"); 
        Encoded[1] = Get100("M"); 
        Encoded[2] = Get100("Y"); 
        break;

    case icSigHexachromeData:    
        Encoded[0] = Get100("C"); Encoded[1] = Get100("M"); 
        Encoded[2] = Get100("Y"); Encoded[3] = Get100("K"); 
        Encoded[4] = Get100("c"); Encoded[5] = Get100("m");                                       
        break;

    case icSigHeptachromeData:
    case icSigOctachromeData:    
    case icSig2colorData:
    case icSig3colorData:
    case icSig4colorData:
    case icSig5colorData:
    case icSig6colorData:
    case icSig7colorData:
    case icSig8colorData: {
        int i;

        for (i=0; i < _cmsChannelsOf(InputColorSpace); i++) {

            char Name[100];

            sprintf(Name, "Channel #%d", i+1);
            Encoded[i] = GetVal(Name);
        }

                          }
                          break;

    default:              
        FatalError("Unsupported %d channel profile", _cmsChannelsOf(InputColorSpace));
    }

    if (xisatty(stdin))
        printf("\n");

}



// Take a value and scale it accordly to fill a WORD (0..FFFF)

static
WORD GetIT8Val(const char* Name, double Max)
{
	double CGATSfactor = 65535.0 / Max;
	const char* Val = cmsIT8GetData(hIT8in, CGATSPatch, Name);

	if (Val == NULL)
		FatalError("Field '%s' not found", Name);
	 

	return (WORD) floor(atof(Val) * CGATSfactor + 0.5);

}


// Read input values from CGATS file.

static
void TakeCGATSValues(int nPatch, WORD Encoded[])
{
    // At first take the name if SAMPLE_ID is present
	cmsIT8GetPatchName(hIT8in, nPatch, CGATSPatch);

    // Special handling for named color profiles. 
    // Lookup the name in the names database (the transform)

	if (cmsGetDeviceClass(hInput) == icSigNamedColorClass) {

        int index = cmsNamedColorIndex(hTrans, CGATSPatch);
        if (index < 0) 
            FatalError("Named color '%s' not found in the profile", CGATSPatch); 

        Encoded[0] = (WORD) index;
        return;
    }
    
    // Color is not a spot color, proceed.

	switch (InputColorSpace) {


    // Encoding should follow CGATS specification.

	case icSigXYZData:
                    xyz.X = cmsIT8GetDataDbl(hIT8in, CGATSPatch, "XYZ_X") / 100.0;
                    xyz.Y = cmsIT8GetDataDbl(hIT8in, CGATSPatch, "XYZ_Y") / 100.0;
                    xyz.Z = cmsIT8GetDataDbl(hIT8in, CGATSPatch, "XYZ_Z") / 100.0;
                    cmsFloat2XYZEncoded(Encoded, &xyz);                 
                    break;

    case icSigLabData:
                    Lab.L = cmsIT8GetDataDbl(hIT8in, CGATSPatch, "LAB_L");
                    Lab.a = cmsIT8GetDataDbl(hIT8in, CGATSPatch, "LAB_A");
                    Lab.b = cmsIT8GetDataDbl(hIT8in, CGATSPatch, "LAB_B");
                    cmsFloat2LabEncoded(Encoded, &Lab);                 
                    break;

        
    case icSigRgbData:
                    Encoded[0] = GetIT8Val("RGB_R", 255.0);
                    Encoded[1] = GetIT8Val("RGB_G", 255.0);
                    Encoded[2] = GetIT8Val("RGB_B", 255.0);
                    break;

    case icSigGrayData:
                    Encoded[0] = GetIT8Val("GRAY", 255.0);
                    break;
    
    case icSigCmykData:
                    Encoded[0] = GetIT8Val("CMYK_C", 100.0);
                    Encoded[1] = GetIT8Val("CMYK_M", 100.0);
                    Encoded[2] = GetIT8Val("CMYK_Y", 100.0);
                    Encoded[3] = GetIT8Val("CMYK_K", 100.0);
                    break;

    case icSigCmyData:                        
                    Encoded[0] = GetIT8Val("CMY_C", 100.0);
                    Encoded[1] = GetIT8Val("CMY_M", 100.0);
                    Encoded[2] = GetIT8Val("CMY_Y", 100.0);
                    break;
    
	default:
		FatalError("Unsupported %d channel profile for CGATS", _cmsChannelsOf(InputColorSpace));

	}

}




static
void Help(void)
{
             
     fprintf(stderr, "usage: icctrans [flags] [CGATS input file]\n\n");

     fprintf(stderr, "flags:\n\n");
     fprintf(stderr, "%cv - Verbose\n", SW);
     fprintf(stderr, "%ci<profile> - Input profile (defaults to sRGB)\n", SW);
     fprintf(stderr, "%co<profile> - Output profile (defaults to sRGB)\n", SW);   
     fprintf(stderr, "%cl<profile> - Transform by device-link profile\n", SW);   

     fprintf(stderr, "\nYou can use '*Lab' and '*xyz' as built-in profiles\n\n");

     fprintf(stderr, "%ct<0,1,2,3> - Intent (0=Perceptual, 1=Colorimetric, 2=Saturation, 3=Absolute)\n", SW);    
     
     fprintf(stderr, "%cw - use 16 bits\n", SW);     
     fprintf(stderr, "%cx - Hexadecimal\n\n", SW);
     fprintf(stderr, "%cb - Black point compensation\n", SW);
     fprintf(stderr, "%cc<0,1,2,3> - Precalculates transform (0=Off, 1=Normal, 2=Hi-res, 3=LoRes) [defaults to 0]\n", SW);     
     fprintf(stderr, "%cn - Terse output, intended for pipe usage\n\n", SW);

     fprintf(stderr, "%cp<profile> - Soft proof profile\n", SW);
     fprintf(stderr, "%cm<0,1,2,3> - Soft proof intent\n", SW);
     fprintf(stderr, "%cg - Marks out-of-gamut colors on softproof\n\n", SW);
     
     fprintf(stderr, "This program is intended to be a demo of the little cms\n"
                     "engine. Both lcms and this program are freeware. You can\n"
                     "obtain both in source code at http://www.littlecms.com\n"
                     "For suggestions, comments, bug reports etc. send mail to\n"
                     "info@littlecms.com\n\n");
     exit(0);
}



static
void PrintPCS(WORD Input[], WORD PCSxyz[], WORD PCSLab[])
{
    if (Verbose && hTransXYZ && hTransLab) {
                        
            if (hTransXYZ) cmsDoTransform(hTransXYZ, Input, PCSxyz, 1);
            if (hTransLab) cmsDoTransform(hTransLab, Input, PCSLab, 1);

            PrintResults(PCSxyz, icSigXYZData); 
            PrintResults(PCSLab, icSigLabData); 
          }
}



int main(int argc, char *argv[])
{
    WORD Input[MAXCHANNELS], Output[MAXCHANNELS], PCSLab[MAXCHANNELS], PCSxyz[MAXCHANNELS];
	int nPatch = 0;
    int nMaxPatches;

    fprintf(stderr, "little cms ColorSpace conversion calculator - v2.0\n\n");

      if (argc == 1)  
              Help();              

      HandleSwitches(argc, argv);

      cmsSetErrorHandler(MyErrorHandler);

	  // Open CGATS input if specified

	  if (argc - xoptind == 1)  {
		  hIT8in = cmsIT8LoadFromFile(argv[xoptind]);
          if (hIT8in == NULL) 
              FatalError("'%s' is not recognized as a CGATS file", argv[xoptind]);
          
		  nMaxPatches = (int) cmsIT8GetPropertyDbl(hIT8in, "NUMBER_OF_SETS");

	  }

      OpenTransforms();
      
      for(;;) {

		  if (feof(stdin))
              break;
          
		  if (hIT8in == NULL) {
			
			TakeTextValues(Input);          

		  }
		  else {

			if (nPatch >= nMaxPatches) break;
			TakeCGATSValues(nPatch++, Input);
		  }

          cmsDoTransform(hTrans, Input, Output, 1);          
          PrintResults(Output, OutputColorSpace); 
          PrintPCS(Input, PCSxyz, PCSLab);             
      }


	  if (hIT8in)
		  cmsIT8Free(hIT8in);

      return 0;     
}


