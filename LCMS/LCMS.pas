unit LCMS;

//----------------------------------------------------------------------------------------------------------------------
//  Little cms Delphi wrapper
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
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY of ANY KIND, 
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO 
// THE WARRANTIES of MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE 
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION 
// of CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT of OR IN CONNECTION 
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
//----------------------------------------------------------------------------------------------------------------------
//
// Modified by Mike Lischke (www.soft-gems.net) to support use of object files.
// Additionally, source code reformatting was done to be conformal to common Delphi guidelines and those
// set at Delphi Gems.
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$include Compilers.inc}

{$ifdef COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
  // We switch off warnings about that fact. We know it and we accept it.
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CAST off}
  {$warn UNSAFE_CODE off}
{$endif COMPILER_7_UP}

{$align 4} // Structures are aligned on quad word boundaries.
           // Note: This must be the same in the project settings for compiling the C sources!

uses
  Windows,
  ColorTypes;

const
  // Intents
  INTENT_PERCEPTUAL               =  0;
  INTENT_RELATIVE_COLORIMETRIC    =  1;
  INTENT_SATURATION               =  2;
  INTENT_ABSOLUTE_COLORIMETRIC    =  3;

  // Flags
  cmsFLAGS_MATRIXINPUT            = $0001;
  cmsFLAGS_MATRIXOUTPUT           = $0002;
  cmsFLAGS_MATRIXONLY             = (cmsFLAGS_MATRIXINPUT OR cmsFLAGS_MATRIXOUTPUT);

  cmsFLAGS_NOPRELINEARIZATION     = $0010; // Don't create prelinearization tables
                                           // on precalculated transforms (internal use)                   

  cmsFLAGS_NOTPRECALC             = $0100;
  cmsFLAGS_NULLTRANSFORM          = $0200;  // Don't transform anyway
  cmsFLAGS_HIGHRESPRECALC         = $0400;  // Use more memory to give better accurancy
  cmsFLAGS_LOWRESPRECALC          = $0800;  // Use less memory to minimize resouces

    
  cmsFLAGS_GAMUTCHECK             = $1000;  // Mark Out of Gamut as alarm color (on proofing transform)
  cmsFLAGS_SOFTPROOFING           = $4000;  // Softproof of proofing profile

  cmsFLAGS_WHITEBLACKCOMPENSATION = $2000; 
  cmsFLAGS_BLACKPOINTCOMPENSATION = $2000;

  cmsFLAGS_NODEFAULTRESOURCEDEF   = $10000;   // PostScript

  // Format descriptors
  TYPE_GRAY_8                 = $30009;
  TYPE_GRAY_8_REV             = $32009;
  TYPE_GRAY_16                = $3000A;
  TYPE_GRAY_16_REV            = $3200A;
  TYPE_GRAY_16_SE             = $3080A;
  TYPE_GRAYA_8                = $30089;
  TYPE_GRAYA_16               = $3008A;
  TYPE_GRAYA_16_SE            = $3088A;
  TYPE_GRAYA_8_PLANAR         = $31089;
  TYPE_GRAYA_16_PLANAR        = $3108A;
  TYPE_RGB_8                  = $40019;
  TYPE_RGB_8_PLANAR           = $41019;
  TYPE_BGR_8                  = $40419;
  TYPE_BGR_8_PLANAR           = $41419;
  TYPE_RGB_16                 = $4001A;
  TYPE_RGB_16_PLANAR          = $4101A;
  TYPE_RGB_16_SE              = $4081A;
  TYPE_BGR_16                 = $4041A;
  TYPE_BGR_16_PLANAR          = $4141A;
  TYPE_BGR_16_SE              = $40C1A;
  TYPE_RGBA_8                 = $40099;
  TYPE_RGBA_8_PLANAR          = $41099;
  TYPE_RGBA_16                = $4009A;
  TYPE_RGBA_16_PLANAR         = $4109A;
  TYPE_RGBA_16_SE             = $4089A;
  TYPE_ARGB_8                 = $44099;
  TYPE_ARGB_16                = $4409A;
  TYPE_ABGR_8                 = $40499;
  TYPE_ABGR_16                = $4049A;
  TYPE_ABGR_16_PLANAR         = $4149A;
  TYPE_ABGR_16_SE             = $40C9A;
  TYPE_BGRA_8                 = $44499;
  TYPE_BGRA_16                = $4449A;
  TYPE_BGRA_16_SE             = $4489A;
  TYPE_CMY_8                  = $50019;
  TYPE_CMY_8_PLANAR           = $51019;
  TYPE_CMY_16                 = $5001A;
  TYPE_CMY_16_PLANAR          = $5101A;
  TYPE_CMY_16_SE              = $5081A;
  TYPE_CMYK_8                 = $60021;
  TYPE_CMYK_8_REV             = $62021;
  TYPE_YUVK_8                 = $62021;
  TYPE_CMYK_8_PLANAR          = $61021;
  TYPE_CMYK_16                = $60022;
  TYPE_CMYK_16_REV            = $62022;
  TYPE_YUVK_16                = $62022;
  TYPE_CMYK_16_PLANAR         = $61022;
  TYPE_CMYK_16_SE             = $60822;
  TYPE_KYMC_8                 = $60421;
  TYPE_KYMC_16                = $60422;
  TYPE_KYMC_16_SE             = $60C22;
  TYPE_KCMY_8                 = $64021;
  TYPE_KCMY_8_REV             = $66021;
  TYPE_KCMY_16                = $64022;
  TYPE_KCMY_16_REV            = $66022;
  TYPE_KCMY_16_SE             = $64822;
  TYPE_CMYKcm_8               = $0031;
  TYPE_CMYKcm_8_PLANAR        = $1031;
  TYPE_CMYKcm_16              = $0032;
  TYPE_CMYKcm_16_PLANAR       = $1032;
  TYPE_CMYKcm_16_SE           = $0832;
  TYPE_CMYK7_8                = $0039;
  TYPE_CMYK7_16               = $003A;
  TYPE_CMYK7_16_SE            = $083A;
  TYPE_KYMC7_8                = $0439;
  TYPE_KYMC7_16               = $043A;
  TYPE_KYMC7_16_SE            = $0C3A;
  TYPE_CMYK8_8                = $0041;
  TYPE_CMYK8_16               = $0042;
  TYPE_CMYK8_16_SE            = $0842;
  TYPE_KYMC8_8                = $0441;
  TYPE_KYMC8_16               = $0442;
  TYPE_KYMC8_16_SE            = $0C42;
  TYPE_CMYK9_8                = $0049;
  TYPE_CMYK9_16               = $004A;
  TYPE_CMYK9_16_SE            = $084A;
  TYPE_KYMC9_8                = $0449;
  TYPE_KYMC9_16               = $044A;
  TYPE_KYMC9_16_SE            = $0C4A;
  TYPE_CMYK10_8               = $0051;
  TYPE_CMYK10_16              = $0052;
  TYPE_CMYK10_16_SE           = $0852;
  TYPE_KYMC10_8               = $0451;
  TYPE_KYMC10_16              = $0452;
  TYPE_KYMC10_16_SE           = $0C52;
  TYPE_CMYK11_8               = $0059;
  TYPE_CMYK11_16              = $005A;
  TYPE_CMYK11_16_SE           = $085A;
  TYPE_KYMC11_8               = $0459;
  TYPE_KYMC11_16              = $045A;
  TYPE_KYMC11_16_SE           = $0C5A;
  TYPE_CMYK12_8               = $0061;
  TYPE_CMYK12_16              = $0062;
  TYPE_CMYK12_16_SE           = $0862;
  TYPE_KYMC12_8               = $0461;
  TYPE_KYMC12_16              = $0462;
  TYPE_KYMC12_16_SE           = $0C62;

  TYPE_XYZ_16                 = $9001A;
  TYPE_Lab_8                  = $A0019;
  TYPE_LabA_8                 = $A0099;
  TYPE_Lab_16                 = $A001A;
  TYPE_Yxy_16                 = $E001A;
  TYPE_YCbCr_8                = $70019;
  TYPE_YCbCr_8_PLANAR         = $71019;
  TYPE_YCbCr_16               = $7001A;
  TYPE_YCbCr_16_PLANAR        = $7101A;
  TYPE_YCbCr_16_SE            = $7081A;
  TYPE_YUV_8                  = $80019;
  TYPE_YUV_8_PLANAR           = $81019;
  TYPE_YUV_16                 = $8001A;
  TYPE_YUV_16_PLANAR          = $8101A;
  TYPE_YUV_16_SE              = $8081A;
  TYPE_HLS_8                  = $D0019;
  TYPE_HLS_8_PLANAR           = $D1019;
  TYPE_HLS_16                 = $D001A;
  TYPE_HLS_16_PLANAR          = $D101A;
  TYPE_HLS_16_SE              = $D081A;
  TYPE_HSV_8                  = $C0019;
  TYPE_HSV_8_PLANAR           = $C1019;
  TYPE_HSV_16                 = $C001A;
  TYPE_HSV_16_PLANAR          = $C101A;
  TYPE_HSV_16_SE              = $C081A;
  TYPE_NAMED_COLOR_INDEX      = $000A;

  TYPE_XYZ_DBL                = $90018;
  TYPE_Lab_DBL                = $A0018;
  TYPE_GRAY_DBL               = $30008;
  TYPE_RGB_DBL                = $40018;
  TYPE_CMYK_DBL               = $60020;

  // Some utility functions to compute new descriptors
  function COLORSPACE_SH(e: Cardinal): Integer;
  function SWAPFIRST_SH(e: Boolean): Integer;
  function FLAVOR_SH(e: Boolean): Integer;
  function PLANAR_SH(e: Boolean): Integer;
  function ENDIAN16_SH(e: Boolean): Integer;
  function DOSWAP_SH(e: Boolean): Integer;
  function EXTRA_SH(e: Integer): Integer;
  function CHANNELS_SH(c: Integer): Integer;
  function BYTES_SH(b: Integer): Integer;

type
  icTagSignature         = DWORD;
  icColorSpaceSignature  = DWORD;
  icProfileClassSignature= DWORD;

const
  // icc color space signatures
  icSigXYZData              = $58595A20;
  icSigLabData              = $4C616220;
  icSigLuvData              = $4C757620;
  icSigYCbCrData            = $59436272;
  icSigYxyData              = $59787920;
  icSigRgbData              = $52474220;
  icSigGrayData             = $47524159;
  icSigHsvData              = $48535620;
  icSigHlsData              = $484C5320;
  icSigCmykData             = $434D594B;
  icSigCmyData              = $434D5920;

  // icc Profile class signatures
  icSigInputClass           = $73636E72;
  icSigDisplayClass         = $6D6E7472;
  icSigOutputClass          = $70727472;
  icSigLinkClass            = $6C696E6B;
  icSigAbstractClass        = $61627374;
  icSigColorSpaceClass      = $73706163;
  icSigNamedColorClass      = $6e6d636c;


  // Added by lcms
  lcmsSignature                = $6c636d73;
  icSigLuvKData                = $4C75764B;  {'LuvK'}
  icSigChromaticityTag         = $6368726d;  { As per Addendum 2 to Spec. ICC.1:1998-09 }
  icSigChromaticAdaptationTag  = $63686164;

type
  cmsHPROFILE    = Pointer;
  TCMSHPROFILE   = cmsHPROFILE;
  cmsHTRANSFORM  = Pointer;
  TCMSHTRANSFORM = cmsHTRANSFORM;
  LCMSHANDLE     = Pointer;

  LCMSGAMMAPARAMS = packed record
    Crc32: DWORD;
    TheType: Integer;
    Params: array [0..9] of Double;                        
  end;

  GAMMATABLE = packed record
    Birth: LCMSGAMMAPARAMS;
    nEntries: DWORD;
    GammaTable: array [0..1] of Word;
  end;
  LPGAMMATABLE = ^GAMMATABLE;
  PGammaTable = LPGAMMATABLE;

  TGammaTableArray = array[0..3] of PGammaTable;
  
  // Colorimetric spaces
  cmsCIEXYZ = TCIEXYZ;
  LPcmsCIEXYZ = ^cmsCIEXYZ;

  cmsCIEXYZTRIPLE = packed record
    Red, Green, Blue : cmsCIEXYZ;
  end;
  LPcmsCIEXYZTRIPLE = ^cmsCIEXYZTRIPLE;

  cmsCIExyY = TCIExyY;
  LPcmsCIExyY = ^cmsCIExyY;

  cmsCIExyYTRIPLE = packed record
    Red, Green, Blue : cmsCIExyY;
  end;
  LPcmsCIExyYTRIPLE = ^cmsCIExyYTRIPLE;
  TCIExyYTriple = cmsCIExyYTRIPLE;

  cmsCIELab = packed record
    L, a, b: Double;
  end;
  LPcmsCIELab = ^cmsCIELab;

  cmsCIELCh = packed record
    L, C, h : Double;
  end;
  LPcmsCIELCh = ^cmsCIELCh;

  // CIECAM97s
  cmsJCh   = packed record
     J, C, h : Double;
   end;
  LPcmsJCh  = ^cmsJCh;

  LPLUT = Pointer;

const
  AVG_SURROUND_4     = 0;
  AVG_SURROUND       = 1;
  DIM_SURROUND       = 2;
  DARK_SURROUND      = 3;
  CUTSHEET_SURROUND  = 4;

  D_CALCULATE           =  -1;
  D_CALCULATE_DISCOUNT  =  -2;

const
   PT_ANY   =   0;   // Don't check colorspace
                     // 1 & 2 are reserved
   PT_GRAY  =   3;
   PT_RGB   =   4;
   PT_CMY   =   5;
   PT_CMYK  =   6;
   PT_YCbCr =   7;
   PT_YUV   =   8;     // Lu'v'
   PT_XYZ   =   9;
   PT_Lab   =   10;
   PT_YUVK  =   11;    // Lu'v'K
   PT_HSV   =   12;
   PT_HLS   =   13;
   PT_Yxy   =   14;

type
  cmsViewingConditions = packed record
    WhitePoint: cmsCIEXYZ;
    Yb: Double;
    La: Double;
    surround: Integer;
    D_value: Double;
  end;
  LPcmsViewingConditions = ^cmsViewingConditions;

  cmsErrorHandler =  function (Severity: Integer; Msg:PChar): Integer; cdecl;
  LCMSARRAYOFPCHAR = array of PChar;

// Note: there is a reason that I use stdcall as calling convention here, although obj files are usually created
//       with cdecl calling convention. However in order to ease life with underscore decoration by the C compiler
//       and to have a DLL together with the object files I compiled LCMS with LCMS_DLL and LCMS_DLL_BUILD enabled.
//       This in turn uses stdcall for all exported functions. Hence its use here.

// Input/Output
function cmsOpenProfileFromFile(ICCProfile: PChar; sAccess: PChar) : cmsHPROFILE; stdcall;
function cmsOpenProfileFromMem(MemPtr: Pointer; dwSize: DWORD) : cmsHPROFILE; stdcall;
function cmsCloseProfile(hProfile : cmsHPROFILE) : Boolean; stdcall;
function cmsCreateRGBProfile(WhitePoint : LPcmsCIExyY; Primaries: LPcmsCIExyYTRIPLE;
  TransferFunction: array of LPGAMMATABLE) : cmsHPROFILE; stdcall;
function cmsCreateGrayProfile(WhitePoint: LPcmsCIExyY; TransferFunction: LPGAMMATABLE) :  cmsHPROFILE; stdcall;
function cmsCreateLinearizationDeviceLink(ColorSpace: icColorSpaceSignature;
  TransferFunction: array of LPGAMMATABLE) : cmsHPROFILE; stdcall;
function cmsCreateInkLimitingDeviceLink(ColorSpace: icColorSpaceSignature; Limit: Double) : cmsHPROFILE; stdcall;
function cmsCreateNULLProfile : cmsHPROFILE; stdcall;
function cmsCreateLabProfile(WhitePoint: LPcmsCIExyY): cmsHPROFILE; stdcall;
function cmsCreateLab4Profile(WhitePoint: LPcmsCIExyY): cmsHPROFILE; stdcall;
function cmsCreateXYZProfile:cmsHPROFILE; stdcall;
function cmsCreate_sRGBProfile:cmsHPROFILE; stdcall;
function cmsCreateBCHSWabstractProfile(nLUTPoints: Integer; Bright, Contrast, Hue, Saturation: Double; TempSrc,
  TempDest: Integer): cmsHPROFILE; stdcall;

// Utils
procedure cmsXYZ2xyY(var Dest: cmsCIExyY; const Source: cmsCIEXYZ); stdcall;
procedure cmsxyY2XYZ(Dest: LPcmsCIEXYZ; Source: LPcmsCIExyY); stdcall;
procedure cmsXYZ2Lab(WhitePoint: LPcmsCIEXYZ; xyz: LPcmsCIEXYZ; Lab: LPcmsCIELab); stdcall;
procedure cmsLab2XYZ(WhitePoint: LPcmsCIEXYZ; Lab: LPcmsCIELab; xyz: LPcmsCIEXYZ); stdcall;
procedure cmsLab2LCh(LCh: LPcmsCIELCh; Lab: LPcmsCIELab); stdcall;
procedure cmsLCh2Lab(Lab: LPcmsCIELab; LCh: LPcmsCIELCh); stdcall;

// CIELab handling
function cmsDeltaE(Lab1, Lab2: LPcmsCIELab): Double; stdcall;
function cmsCIE94DeltaE(Lab1, Lab2: LPcmsCIELab): Double; stdcall;
function cmsBFDdeltaE(Lab1, Lab2: LPcmsCIELab): Double; stdcall;
function cmsCMCdeltaE(Lab1, Lab2: LPcmsCIELab): Double; stdcall;
function cmsCIE2000DeltaE(Lab1, Lab2: LPcmsCIELab; Kl, Kc, Kh: Double): Double; stdcall;

procedure cmsClampLab(Lab: LPcmsCIELab; amax, amin,  bmax, bmin: Double); stdcall;

// White point
function  cmsWhitePointFromTemp(TempK: Integer; WhitePoint: LPcmsCIExyY) : Boolean; stdcall;

// CIECAM97s
function cmsCIECAM97sInit(pVC : LPcmsViewingConditions ) : Pointer; stdcall;
procedure cmsCIECAM97sDone(hModel : Pointer); stdcall;

procedure cmsCIECAM97sForward(hModel: Pointer; pIn: LPcmsCIEXYZ; pOut: LPcmsJCh ); stdcall;
procedure cmsCIECAM97sReverse(hModel: Pointer; pIn: LPcmsJCh;   pOut: LPcmsCIEXYZ ); stdcall;

// CIECAM02
function cmsCIECAM02Init(pVC : LPcmsViewingConditions ) : Pointer; stdcall;
procedure cmsCIECAM02Done(hModel : Pointer); stdcall;

procedure cmsCIECAM02Forward(hModel: Pointer; pIn: LPcmsCIEXYZ; pOut: LPcmsJCh ); stdcall;
procedure cmsCIECAM02Reverse(hModel: Pointer; pIn: LPcmsJCh;   pOut: LPcmsCIEXYZ ); stdcall;

// Gamma curves
function cmsBuildGamma(nEntries : Integer; Gamma: Double) : LPGAMMATABLE; stdcall;
function cmsAllocGamma(nEntries : Integer): LPGAMMATABLE; stdcall;
procedure cmsFreeGamma(Gamma: LPGAMMATABLE); stdcall;
procedure cmsFreeGammaTriple(Gamma: array of LPGAMMATABLE); stdcall;
function cmsReverseGamma(nResultSamples: Integer; InGamma : LPGAMMATABLE): LPGAMMATABLE; stdcall;
function cmsJoinGamma(InGamma, OutGamma: LPGAMMATABLE): LPGAMMATABLE; stdcall;
function cmsJoinGammaEx(InGamma, OutGamma: LPGAMMATABLE; nPoints: Integer): LPGAMMATABLE; stdcall;
function cmsSmoothGamma(Gamma: LPGAMMATABLE; SmoothingLambda: Double): Boolean; stdcall;
function cmsDupGamma(Src: LPGAMMATABLE): LPGAMMATABLE; stdcall;
function cmsEstimateGamma(Src: LPGAMMATABLE): Double; stdcall;
function cmsEstimateGammaEx(Src: LPGAMMATABLE; Thereshold: Double): Double; stdcall;
function cmsReadICCGamma(hProfile: cmsHPROFILE; Sig: icTagSignature): LPGAMMATABLE; stdcall;
function cmsReadICCGammaReversed(hProfile: cmsHPROFILE; Sig: icTagSignature): LPGAMMATABLE; stdcall;

const
  lcmsParametricCurveExp           = 0;
  lcmsParametricCurveCIE_122_1966  = 1;
  lcmsParametricCurveIEC_61966_3   = 2;
  lcmsParametricCurveIEC_61966_2_1 = 3;

function cmsBuildParametricGamma(nEntries: Integer; TheType: Integer; Params: array of Double) : LPGAMMATABLE; stdcall;

// Access to Profile data.
procedure cmsSetLanguage(LanguageCode: Integer; CountryCode: Integer); stdcall;

function cmsTakeMediaWhitePoint(Dest: LPcmsCIEXYZ; hProfile: cmsHPROFILE): Boolean; stdcall;
function cmsTakeMediaBlackPoint(Dest: LPcmsCIEXYZ; hProfile: cmsHPROFILE): Boolean; stdcall;
function cmsTakeIluminant(Dest: LPcmsCIEXYZ; hProfile: cmsHPROFILE): Boolean; stdcall;
function cmsTakeColorants(Dest: LPcmsCIEXYZTRIPLE; hProfile: cmsHPROFILE): Boolean; stdcall;
function cmsTakeHeaderFlags(hProfile: cmsHPROFILE): DWORD; stdcall;

function cmsTakeProductName(hProfile: cmsHPROFILE): PChar; stdcall;
function cmsTakeProductDesc(hProfile: cmsHPROFILE): PChar; stdcall;

function cmsTakeManufacturer(hProfile: cmsHPROFILE): PChar; stdcall;
function cmsTakeModel(hProfile: cmsHPROFILE): PChar; stdcall;
function cmsTakeCopyright(hProfile: cmsHPROFILE): PChar; stdcall;
function cmsTakeProfileID(hProfile: cmsHPROFILE): PByte; stdcall;

function cmsIsTag(hProfile: cmsHPROFILE; sig: icTagSignature): Boolean; stdcall;
function cmsTakeRenderingIntent(hProfile: cmsHPROFILE): Integer; stdcall;
function cmsIsIntentSupported(hProfile: cmsHPROFILE; Intent, UsedDirection : Integer): Integer; stdcall;
function cmsTakeCharTargetData(hProfile: cmsHPROFILE; var Data : PChar; var len: Cardinal): Boolean; stdcall;

function _cmsICCcolorSpace(OurNotation: Integer) : icColorSpaceSignature; stdcall;
function _cmsLCMScolorSpace(ProfileSpace: icColorSpaceSignature): Integer; stdcall;
function _cmsChannelsOf(ColorSpace: icColorSpaceSignature): Integer; stdcall;

function cmsGetPCS(hProfile: cmsHPROFILE): icColorSpaceSignature; stdcall;
function cmsGetColorSpace(hProfile: cmsHPROFILE): icColorSpaceSignature; stdcall;
function cmsGetDeviceClass( hProfile: cmsHPROFILE): icProfileClassSignature; stdcall;
function cmsGetProfileICCversion( hProfile: cmsHPROFILE): DWORD; stdcall;

// Profile creation
procedure cmsSetDeviceClass(hProfile: cmsHPROFILE; sig: icProfileClassSignature ); stdcall;
procedure cmsSetColorSpace(hProfile: cmsHPROFILE; sig: icProfileClassSignature ); stdcall;
procedure cmsSetPCS(hProfile: cmsHPROFILE; pcs: icColorSpaceSignature); stdcall;
procedure cmsSetRenderingIntent(hProfile: cmsHPROFILE; Intent: Integer); stdcall;
procedure cmsSetHeaderFlags(hProfile: cmsHPROFILE; dwFlags: DWORD); stdcall;
procedure cmsSetProfileID(hProfile: cmsHPROFILE; ProfileID: PByte); stdcall;

function  cmsAddTag(hProfile: cmsHPROFILE; Sig: icTagSignature; Data: Pointer): Boolean; stdcall;
function  _cmsSaveProfile(hProfile: cmsHPROFILE; FileName: PChar): Boolean; stdcall;
function  _cmsSaveProfileToMem(hProfile: cmsHPROFILE; MemPtr: Pointer; var BytesNeeded: DWORD): Boolean; stdcall;

const
  LCMS_USED_AS_INPUT   =   0;
  LCMS_USED_AS_OUTPUT  =   1;
  LCMS_USED_AS_PROOF   =   2;

// Transforms
function cmsCreateTransform(Input: cmsHPROFILE; InputFormat: DWORD; Output: cmsHPROFILE; OutputFormat: DWORD;
  Intent: Integer; dwFlags: DWORD): cmsHTRANSFORM; stdcall;
function cmsCreateProofingTransform(Input: cmsHPROFILE; InputFormat: DWORD; Output: cmsHPROFILE; OutputFormat: DWORD;
  Proofing: cmsHPROFILE; Intent: Integer; ProofingIntent: Integer; dwFlags: DWORD): cmsHTRANSFORM; stdcall;
function cmsCreateMultiprofileTransform(hProfiles : array of cmsHPROFILE; nProfiles : Integer; InputFormat: DWORD;
  OutputFormat: DWORD; Intent: Integer; dwFlags: DWORD): cmsHTRANSFORM; stdcall;
procedure cmsDeleteTransform( hTransform: cmsHTRANSFORM); stdcall;
procedure cmsDoTransform( Transform: cmsHTRANSFORM; InputBuffer: Pointer; OutputBuffer: Pointer; Size: LongInt); stdcall;
procedure cmsChangeBuffersFormat(hTransform: cmsHTRANSFORM; dwInputFormat, dwOutputFormat: DWORD); stdcall;

// Devicelink generation
function cmsTransform2DeviceLink(hTransform: cmsHTRANSFORM; dwFlags: DWORD): cmsHPROFILE; stdcall;
procedure _cmsSetLUTdepth(hProfile: cmsHPROFILE; depth: Integer); stdcall;

// Named color support
function cmsNamedColorCount(xform: cmsHTRANSFORM): Integer; stdcall;
function cmsNamedColorInfo(xform: cmsHTRANSFORM; nColor: Integer; Name, Prefix, Suffix: PChar) : Boolean; stdcall;
function cmsNamedColorIndex(xform: cmsHTRANSFORM; Name: PChar): Integer; stdcall;

// PostScript ColorRenderingDictionary and ColorSpaceArray
function cmsGetPostScriptCSA(hProfile: cmsHPROFILE; Intent: Integer; Buffer: Pointer; dwBufferLen: DWORD): DWORD; stdcall;
function cmsGetPostScriptCRD(hProfile: cmsHPROFILE; Intent: Integer; Buffer: Pointer; dwBufferLen: DWORD): DWORD; stdcall;
function cmsGetPostScriptCRDEx(hProfile: cmsHPROFILE; Intent: Integer; dwFlags: DWORD; Buffer: Pointer;
  dwBufferLen: DWORD): DWORD; stdcall;

// Gamut check
procedure cmsSetAlarmCodes(r, g, b: Integer); stdcall;
procedure cmsGetAlarmCodes(var r, g, b: Integer); stdcall;

// Error handling
const
  LCMS_ERROR_ABORT   =  0;
  LCMS_ERROR_SHOW    =  1;
  LCMS_ERROR_IGNORE  =  2;

procedure cmsErrorAction(nAction: Integer); stdcall;
procedure cmsSetErrorHandler(ErrorHandler: cmsErrorHandler); stdcall;

// CGATS.13 parser
function cmsIT8Alloc: LCMSHANDLE; stdcall;
procedure cmsIT8Free(hIT8: LCMSHANDLE); stdcall;

// Tables
function cmsIT8TableCount(hIT8: LCMSHANDLE): Integer; stdcall;
function cmsIT8SetTable(hIT8: LCMSHANDLE; nTable: Integer): Integer; stdcall;

// Persistence
function cmsIT8LoadFromFile(cFileName: PChar): LCMSHANDLE; stdcall;
function cmsIT8LoadFromMem(Ptr: Pointer; size :DWORD): LCMSHANDLE; stdcall;

function cmsIT8SaveToFile(hIT8: LCMSHANDLE; cFileName: PChar): Boolean; stdcall;

// Properties

function cmsIT8GetSheetType(hIT8: LCMSHANDLE): PChar; stdcall;
function cmsIT8SetSheetType(hIT8: LCMSHANDLE; TheType: PChar): Boolean; stdcall;

function cmsIT8SetComment(hIT8: LCMSHANDLE; cComment: PChar): Boolean; stdcall;

function cmsIT8SetPropertyStr(hIT8: LCMSHANDLE; cProp, Str: PChar): Boolean; stdcall;
function cmsIT8SetPropertyDbl(hIT8: LCMSHANDLE; cProp: PChar; Val: Double): Boolean; stdcall;
function cmsIT8SetPropertyHex(hIT8: LCMSHANDLE; cProp: PChar; Val: Integer): Boolean; stdcall;
function cmsIT8SetPropertyUncooked(hIT8: LCMSHANDLE; Key, Buffer: PChar): Boolean; stdcall;

function cmsIT8GetProperty(hIT8: LCMSHANDLE; cProp: PChar): PChar; stdcall;
function cmsIT8GetPropertyDbl(hIT8: LCMSHANDLE; cProp: PChar): Double; stdcall;
function cmsIT8EnumProperties(hIT8: LCMSHANDLE; var PropertyNames: LCMSARRAYOFPCHAR): Integer; stdcall;

// Datasets
function cmsIT8GetDataRowCol(hIT8: LCMSHANDLE; row, col: Integer): PChar; stdcall;
function cmsIT8GetDataRowColDbl(hIT8: LCMSHANDLE; row, col: Integer): Double; stdcall;

function cmsIT8SetDataRowCol(hIT8: LCMSHANDLE; row, col: Integer; Val: PChar): Boolean; stdcall;
function cmsIT8SetDataRowColDbl(hIT8: LCMSHANDLE; row, col: Integer; Val: Double): Boolean; stdcall;

function cmsIT8GetData(hIT8: LCMSHANDLE; cPatch, cSample: PChar): PChar; stdcall;

function cmsIT8GetDataDbl(hIT8: LCMSHANDLE;cPatch, cSample: PChar): Double; stdcall;

function cmsIT8SetData(hIT8: LCMSHANDLE; cPatch, cSample, Val: PChar): Boolean; stdcall;

function cmsIT8SetDataDbl(hIT8: LCMSHANDLE; cPatch, cSample: PChar; Val: Double): Boolean; stdcall;

function cmsIT8SetDataFormat(hIT8: LCMSHANDLE; n: Integer; Sample: PChar): Boolean; stdcall;
function cmsIT8EnumDataFormat(hIT8: LCMSHANDLE; var SampleNames: LCMSARRAYOFPCHAR): Integer; stdcall;
function cmsIT8GetPatchName(hIT8: LCMSHANDLE; nPatch: Integer; Buffer: PChar): PChar; stdcall;

// The LABEL extension
function cmsIT8SetTableByLabel(hIT8: LCMSHANDLE; cSet, cField, ExpectedType: PChar): Integer; stdcall;

procedure cmsLabEncoded2Float(Lab: LPcmsCIELab; wLab: Pointer); stdcall;
procedure cmsFloat2LabEncoded(wLab: Pointer; Lab: LPcmsCIELab); stdcall;
procedure cmsXYZEncoded2Float(fxyz : LPcmsCIEXYZ; XYZ: Pointer); stdcall;
procedure cmsFloat2XYZEncoded(XYZ: Pointer; fXYZ: LPcmsCIEXYZ); stdcall;

function _cmsAddTextTag(hProfile: cmsHPROFILE; sig: icTagSignature; Text: PChar): Boolean; stdcall;
function _cmsAddXYZTag(hProfile: cmsHPROFILE;  sig: icTagSignature;  XYZ: LPcmsCIEXYZ): Boolean; stdcall;
function _cmsAddLUTTag(hProfile: cmsHPROFILE;  sig: icTagSignature; lut: PByte): Boolean; stdcall;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  LibStub;

{$L obj\cmscnvrt.obj}
{$L obj\cmscam02.obj}
{$L obj\cmscam97.obj}
{$L obj\cmscgats.obj}
{$L obj\cmserr.obj}
{$L obj\cmsgamma.obj}
{$L obj\cmsgmt.obj}
{$L obj\cmsintrp.obj}
{$L obj\cmsio0.obj}
{$L obj\cmsio1.obj}
{$L obj\cmslut.obj}
{$L obj\cmsmatsh.obj}
{$L obj\cmsmtrx.obj}
{$L obj\cmsnamed.obj}
{$L obj\cmspack.obj}
{$L obj\cmspcs.obj}
{$L obj\cmsps2.obj}
{$L obj\cmssamp.obj}
{$L obj\cmsvirt.obj}
{$L obj\cmswtpnt.obj}
{$L obj\cmsxform.obj}

type
  // Number definitions
  // Unsigned integer numbers
  icUInt8Number = Byte;
  icUInt16Number = Word;
  icUInt32Number = Cardinal;
  icUInt64Number = Int64;

  // Signed numbers
  icInt8Number = Char;
  icInt16Number = SmallInt;
  icInt32Number = Integer;
  icInt64Number = Int64;

  // Fixed numbers
  icS15Fixed16Number = Integer;
  icU16Fixed16Number = Cardinal;

  PLCMSICCPROFILE = Pointer; // Opaque type, internally used.

//----------------------------------------------------------------------------------------------------------------------

// Helping functions to build format descriptors (C uses them as macros)

function COLORSPACE_SH(e: Cardinal): Integer; begin Result := e shl 16; end;
function SWAPFIRST_SH(e: Boolean): Integer;  begin Result := Ord(e) shl 13; end;
function FLAVOR_SH(e: Boolean): Integer;     begin Result := Ord(e) shl 13; end;
function PLANAR_SH(e: Boolean): Integer;     begin Result := Ord(e) shl 12; end;
function ENDIAN16_SH(e: Boolean): Integer;   begin Result := Ord(e) shl 11; end;
function DOSWAP_SH(e: Boolean): Integer;     begin Result := Ord(e) shl 10; end;
function EXTRA_SH(e: Integer): Integer;      begin Result := e shl  7; end;
function CHANNELS_SH(c: Integer): Integer;   begin Result := c shl  3; end;
function BYTES_SH(b: Integer): Integer;      begin Result := b end;

function cmsOpenProfileFromFile(ICCProfile : PChar; sAccess: PChar) : cmsHPROFILE; external;
function cmsOpenProfileFromMem(MemPtr: Pointer; dwSize: DWORD) : cmsHPROFILE; external;
function cmsWhitePointFromTemp(TempK: Integer; WhitePoint: LPcmsCIExyY) : Boolean; external;

function cmsBuildGamma(nEntries : Integer;  Gamma: Double) : LPGAMMATABLE; external;
function cmsAllocGamma(nEntries : Integer): LPGAMMATABLE; external;
procedure cmsFreeGamma(Gamma: LPGAMMATABLE); external;
procedure cmsFreeGammaTriple(Gamma: array of LPGAMMATABLE); external;

function cmsReverseGamma(nResultSamples : Integer; InGamma : LPGAMMATABLE): LPGAMMATABLE; external;
function cmsBuildParametricGamma(nEntries: Integer; TheType: Integer; Params: array of Double) : LPGAMMATABLE; external;

function cmsJoinGamma(InGamma, OutGamma : LPGAMMATABLE): LPGAMMATABLE; external;
function cmsJoinGammaEx(InGamma, OutGamma: LPGAMMATABLE; nPoints: Integer): LPGAMMATABLE; external;

function cmsSmoothGamma(Gamma: LPGAMMATABLE; SmoothingLambda: Double): Boolean; external;

function cmsDupGamma(Src: LPGAMMATABLE): LPGAMMATABLE; external;
function cmsEstimateGamma(Src: LPGAMMATABLE): Double; external;
function cmsEstimateGammaEx(Src: LPGAMMATABLE; Thereshold: Double): Double; external;
function cmsReadICCGamma(hProfile: cmsHPROFILE; Sig: icTagSignature): LPGAMMATABLE; external;
function cmsReadICCGammaReversed(hProfile: cmsHPROFILE; Sig: icTagSignature): LPGAMMATABLE; external;


function cmsCreateRGBProfile(WhitePoint: LPcmsCIExyY; Primaries: LPcmsCIExyYTRIPLE;
  TransferFunction: array of LPGAMMATABLE): cmsHPROFILE; external;
function cmsCreateGrayProfile(WhitePoint: LPcmsCIExyY; TransferFunction: LPGAMMATABLE) : cmsHPROFILE; external;
function cmsCreateLinearizationDeviceLink(ColorSpace: icColorSpaceSignature;
  TransferFunction: array of LPGAMMATABLE) : cmsHPROFILE; external;
function cmsCreateInkLimitingDeviceLink(ColorSpace: icColorSpaceSignature; Limit: Double) : cmsHPROFILE; external;

function cmsCreateNULLProfile : cmsHPROFILE; external;
function cmsCreateLabProfile(WhitePoint: LPcmsCIExyY): cmsHPROFILE; external;
function cmsCreateLab4Profile(WhitePoint: LPcmsCIExyY): cmsHPROFILE; external;

function cmsCreateXYZProfile: cmsHPROFILE; external;
function cmsCreate_sRGBProfile: cmsHPROFILE; external;
function cmsCreateBCHSWabstractProfile(nLUTPoints: Integer; Bright, Contrast, Hue, Saturation: Double; TempSrc,
  TempDest: Integer): cmsHPROFILE; external;

function cmsCloseProfile( hProfile : cmsHPROFILE) : Boolean; external;

procedure cmsSetLanguage(LanguageCode: Integer; CountryCode: Integer); external;

function cmsTakeMediaWhitePoint(Dest: LPcmsCIEXYZ; hProfile: cmsHPROFILE): Boolean; external;
function cmsTakeMediaBlackPoint(Dest: LPcmsCIEXYZ; hProfile: cmsHPROFILE): Boolean; external;

function cmsTakeIluminant(Dest: LPcmsCIEXYZ; hProfile: cmsHPROFILE): Boolean; external;
function cmsTakeColorants(Dest: LPcmsCIEXYZTRIPLE; hProfile: cmsHPROFILE): Boolean; external;
function cmsTakeHeaderFlags(hProfile: cmsHPROFILE): DWORD; external;

function cmsTakeProductName(hProfile: cmsHPROFILE): PChar; external;
function cmsTakeProductDesc(hProfile: cmsHPROFILE): PChar; external;
function cmsTakeManufacturer(hProfile: cmsHPROFILE): PChar; external;
function cmsTakeModel(hProfile: cmsHPROFILE): PChar; external;
function cmsTakeCopyright(hProfile: cmsHPROFILE): PChar; external;
function cmsTakeProfileID(hProfile: cmsHPROFILE): PByte; external;

function cmsIsTag(hProfile: cmsHPROFILE; sig: icTagSignature): Boolean; external;
function cmsTakeRenderingIntent( hProfile: cmsHPROFILE): Integer; external;
function cmsGetPCS(hProfile: cmsHPROFILE): icColorSpaceSignature; external;
function cmsGetColorSpace(hProfile: cmsHPROFILE): icColorSpaceSignature; external;
function cmsGetDeviceClass( hProfile: cmsHPROFILE): icProfileClassSignature; external;
function cmsGetProfileICCversion( hProfile: cmsHPROFILE): DWORD; external;
function cmsTakeCharTargetData(hProfile: cmsHPROFILE; var Data: PChar; var len: Cardinal): Boolean; external;

function _cmsICCcolorSpace(OurNotation: Integer) : icColorSpaceSignature; external;
function _cmsLCMScolorSpace(ProfileSpace: icColorSpaceSignature): Integer; external;
function _cmsChannelsOf(ColorSpace: icColorSpaceSignature): Integer; external;

procedure cmsSetDeviceClass(hProfile: cmsHPROFILE; sig: icProfileClassSignature ); external;
procedure cmsSetColorSpace(hProfile: cmsHPROFILE; sig: icProfileClassSignature ); external;
procedure cmsSetPCS(hProfile: cmsHPROFILE; pcs: icColorSpaceSignature); external;
procedure cmsSetRenderingIntent(hProfile: cmsHPROFILE; Intent: Integer); external;
procedure cmsSetHeaderFlags(hProfile: cmsHPROFILE; dwFlags: DWORD); external;
procedure cmsSetProfileID(hProfile: cmsHPROFILE; ProfileID: PByte); external;

function cmsAddTag(hProfile: cmsHPROFILE; Sig: icTagSignature; Data: Pointer): Boolean; external;

function _cmsSaveProfile(hProfile: cmsHPROFILE; FileName: PChar): Boolean; external;
function _cmsSaveProfileToMem(hProfile: cmsHPROFILE; MemPtr: Pointer; var BytesNeeded: DWORD): Boolean; external;

function  cmsIsIntentSupported(hProfile: cmsHPROFILE; Intent, UsedDirection : Integer): Integer; external;

function  cmsCreateTransform(Input: cmsHPROFILE; InputFormat: DWORD; Output: cmsHPROFILE; OutputFormat: DWORD;
  Intent: Integer; dwFlags: DWORD): cmsHTRANSFORM; external;
function  cmsCreateProofingTransform(Input: cmsHPROFILE; InputFormat: DWORD; Output: cmsHPROFILE; OutputFormat: DWORD;
  Proofing: cmsHPROFILE; Intent: Integer; ProofingIntent: Integer; dwFlags: DWORD): cmsHTRANSFORM; external;
function cmsCreateMultiprofileTransform(hProfiles : array of cmsHPROFILE; nProfiles : Integer; InputFormat: DWORD;
  OutputFormat: DWORD; Intent: Integer; dwFlags: DWORD): cmsHTRANSFORM; external;

procedure cmsDeleteTransform( hTransform: cmsHTRANSFORM); external;
procedure cmsDoTransform( Transform: cmsHTRANSFORM; InputBuffer: Pointer; OutputBuffer: Pointer;
  Size: LongInt); external;
procedure cmsChangeBuffersFormat(hTransform: cmsHTRANSFORM; dwInputFormat, dwOutputFormat: DWORD);  external;

function cmsTransform2DeviceLink(hTransform: cmsHTRANSFORM; dwFlags: DWORD): cmsHPROFILE; external;
procedure _cmsSetLUTdepth(hProfile: cmsHPROFILE; depth: Integer);  external;
function cmsNamedColorCount(xform: cmsHTRANSFORM): Integer; external;
function cmsNamedColorInfo(xform: cmsHTRANSFORM; nColor: Integer;  Name, Prefix, Suffix: PChar) : Boolean; external;
function cmsNamedColorIndex(xform: cmsHTRANSFORM; Name: PChar): Integer; external;

function cmsGetPostScriptCSA(hProfile: cmsHPROFILE; Intent: Integer; Buffer: Pointer;
  dwBufferLen: DWORD): DWORD; external;
function cmsGetPostScriptCRD(hProfile: cmsHPROFILE; Intent: Integer; Buffer: Pointer;
  dwBufferLen: DWORD): DWORD; external;
function cmsGetPostScriptCRDEx(hProfile: cmsHPROFILE; Intent: Integer; dwFlags: DWORD; Buffer: Pointer;
  dwBufferLen: DWORD): DWORD; external;

function cmsCIECAM97sInit(pVC: LPcmsViewingConditions ): Pointer; external;
procedure cmsCIECAM97sDone(hModel: Pointer); external;

procedure cmsCIECAM97sForward(hModel: Pointer; pIn: LPcmsCIEXYZ; pOut: LPcmsJCh ); external;
procedure cmsCIECAM97sReverse(hModel: Pointer; pIn: LPcmsJCh;   pOut: LPcmsCIEXYZ ); external;

// CIECAM02
function  cmsCIECAM02Init(pVC : LPcmsViewingConditions ) : Pointer; external;
procedure cmsCIECAM02Done(hModel : Pointer); external;

procedure cmsCIECAM02Forward(hModel: Pointer; pIn: LPcmsCIEXYZ; pOut: LPcmsJCh); external;
procedure cmsCIECAM02Reverse(hModel: Pointer; pIn: LPcmsJCh; pOut: LPcmsCIEXYZ); external;

// Utils
procedure cmsXYZ2xyY(var Dest: cmsCIExyY; const Source: cmsCIEXYZ); external;
procedure cmsxyY2XYZ(Dest: LPcmsCIEXYZ; Source: LPcmsCIExyY); external;

procedure cmsXYZ2Lab(WhitePoint: LPcmsCIEXYZ; xyz: LPcmsCIEXYZ; Lab: LPcmsCIELab); external;
procedure cmsLab2XYZ(WhitePoint: LPcmsCIEXYZ; Lab: LPcmsCIELab; xyz: LPcmsCIEXYZ); external;
procedure cmsLab2LCh(LCh: LPcmsCIELCh; Lab: LPcmsCIELab); external;
procedure cmsLCh2Lab(Lab: LPcmsCIELab; LCh: LPcmsCIELCh); external;

// CIELab handling
function cmsDeltaE(Lab1, Lab2: LPcmsCIELab): Double; external;
function cmsCIE94DeltaE(Lab1, Lab2: LPcmsCIELab): Double; external;
function cmsBFDdeltaE(Lab1, Lab2: LPcmsCIELab): Double; external;
function cmsCMCdeltaE(Lab1, Lab2: LPcmsCIELab): Double; external;
function cmsCIE2000DeltaE(Lab1, Lab2: LPcmsCIELab; Kl, Kc, Kh: Double): Double; external;

procedure cmsClampLab(Lab: LPcmsCIELab; amax, amin,  bmax, bmin: Double); external;

procedure cmsSetAlarmCodes(r, g, b: Integer); external;
procedure cmsGetAlarmCodes(var r, g, b: Integer); external;

procedure cmsErrorAction(nAction: Integer); external;

procedure cmsSetErrorHandler(ErrorHandler: cmsErrorHandler); external;

function  cmsIT8Alloc: LCMSHANDLE; external;
procedure cmsIT8Free(hIT8: LCMSHANDLE); external;

// Tables
function  cmsIT8TableCount(hIT8: LCMSHANDLE): Integer; external;
function  cmsIT8SetTable(hIT8: LCMSHANDLE; nTable: Integer): Integer; external;

// Persistence
function  cmsIT8LoadFromFile(cFileName: PChar): LCMSHANDLE; external;
function  cmsIT8LoadFromMem(Ptr: Pointer; size :DWORD): LCMSHANDLE; external;

function cmsIT8SaveToFile(hIT8: LCMSHANDLE; cFileName: PChar): Boolean; external;

// Properties
function cmsIT8GetSheetType(hIT8: LCMSHANDLE): PChar; external;
function cmsIT8SetSheetType(hIT8: LCMSHANDLE; TheType: PChar): Boolean; external;

function cmsIT8SetComment(hIT8: LCMSHANDLE; cComment: PChar): Boolean; external;

function cmsIT8SetPropertyStr(hIT8: LCMSHANDLE; cProp, Str: PChar): Boolean; external;
function cmsIT8SetPropertyDbl(hIT8: LCMSHANDLE; cProp: PChar; Val: Double): Boolean; external;
function cmsIT8SetPropertyHex(hIT8: LCMSHANDLE; cProp: PChar; Val: Integer): Boolean; external;
function cmsIT8SetPropertyUncooked(hIT8: LCMSHANDLE; Key, Buffer: PChar): Boolean; external;

function cmsIT8GetProperty(hIT8: LCMSHANDLE; cProp: PChar): PChar; external;
function cmsIT8GetPropertyDbl(hIT8: LCMSHANDLE; cProp: PChar): Double; external;
function cmsIT8EnumProperties(hIT8: LCMSHANDLE; var PropertyNames: LCMSARRAYOFPCHAR): Integer; external;

// Datasets
function cmsIT8GetDataRowCol(hIT8: LCMSHANDLE; row, col: Integer): PChar; external;
function cmsIT8GetDataRowColDbl(hIT8: LCMSHANDLE; row, col: Integer): Double; external;

function cmsIT8SetDataRowCol(hIT8: LCMSHANDLE; row, col: Integer; Val: PChar): Boolean; external;
function cmsIT8SetDataRowColDbl(hIT8: LCMSHANDLE; row, col: Integer; Val: Double): Boolean; external;

function cmsIT8GetData(hIT8: LCMSHANDLE; cPatch, cSample: PChar): PChar; external;                                                

function cmsIT8GetDataDbl(hIT8: LCMSHANDLE;cPatch, cSample: PChar): Double; external;

function cmsIT8SetData(hIT8: LCMSHANDLE; cPatch, cSample, Val: PChar): Boolean; external;

function cmsIT8SetDataDbl(hIT8: LCMSHANDLE; cPatch, cSample: PChar; Val: Double): Boolean; external;                                                

function cmsIT8SetDataFormat(hIT8: LCMSHANDLE; n: Integer; Sample: PChar): Boolean; external;
function cmsIT8EnumDataFormat(hIT8: LCMSHANDLE; var SampleNames: LCMSARRAYOFPCHAR): Integer; external;
function cmsIT8GetPatchName(hIT8: LCMSHANDLE; nPatch: Integer; Buffer: PChar): PChar; external;

// The LABEL extension
function cmsIT8SetTableByLabel(hIT8: LCMSHANDLE; cSet, cField, ExpectedType: PChar): Integer; external;

procedure cmsLabEncoded2Float(Lab: LPcmsCIELab; wLab: Pointer); external;
procedure cmsFloat2LabEncoded(wLab: Pointer; Lab: LPcmsCIELab); external;
procedure cmsXYZEncoded2Float(fxyz : LPcmsCIEXYZ; XYZ: Pointer); external;
procedure cmsFloat2XYZEncoded(XYZ: Pointer; fXYZ: LPcmsCIEXYZ); external;

function _cmsAddTextTag(hProfile: cmsHPROFILE; sig: icTagSignature; Text: PChar): Boolean; external;
function _cmsAddXYZTag(hProfile: cmsHPROFILE;  sig: icTagSignature;  XYZ: LPcmsCIEXYZ): Boolean; external;
function _cmsAddLUTTag(hProfile: cmsHPROFILE;  sig: icTagSignature; lut: PByte): Boolean; external;

// Others needed for compilation with obj files.
// Actual function/procedure types as well as parameters don't matter here.
// Only the symbol as such is important.
procedure cmsD50_XYZ; external;
procedure _cmsCalcCLUT16Params; external;
procedure __cmsEndPointsBySpace; external;
procedure __cmsSearchTag; external;
procedure __cmsCreateProfilePlaceholder; external;
procedure __cmsCreateProfileFromFilePlaceholder; external;
procedure __cmsCreateProfileFromMemPlaceholder; external;
procedure __cmsCrc32OfGammaTable; external;
procedure _cmsAddGammaTag; external;
procedure _cmsAddChromaticityTag; external;
procedure _cmsAddSequenceDescriptionTag; external;
procedure _cmsAddNamedColorTag; external;
procedure _cmsAddDateTimeTag; external;
procedure _cmsAddColorantTableTag; external;
procedure __cmsSetSaveToDisk; external;
procedure __cmsSetSaveToMemory; external;
procedure _cmsTetrahedralInterp8; external;
procedure _cmsLinearInterpFixed; external;
procedure _Clamp_L; external;
procedure _Clamp_ab; external;
procedure _cmsFreeMatShaper; external;
procedure cmsSetProfileICCversion; external;
procedure __cmsIdentifyInputFormat; external;
procedure __cmsIdentifyOutputFormat; external;
procedure __cmsComputePrelinearizationTablesFromXFORM; external;
procedure cmsEvalLUTreverse; external;
procedure __cmsBuildKToneCurve; external;
procedure cmsReadICCLut; external;
procedure cmsD50_xyY; external;
procedure _cmsEvalMatShaper; external;
procedure __cmsWhiteBySpace; external;
procedure _cmsAllocMatShaper; external;
procedure _cmsReadICCMatrixRGB2XYZ; external;
procedure _cmsAllocMatShaper2; external;
procedure _cmsReadChromaticAdaptationMatrix; external;
procedure _cmsChooseCnvrt; external;
procedure _cmsTetrahedralInterp16; external;
procedure __cmsComputeSoftProofLUT; external;
procedure __cmsComputeGamutLUT; external;
procedure _cmsReadICCnamedColorList; external;
procedure __cmsPrecalculateBlackPreservingDeviceLink; external;
procedure __cmsPrecalculateGamutCheck; external;
procedure _cmsCalcCLUT16ParamsEx; external;
procedure __cmsBlessLUT8; external;
procedure __cmsFixWhiteMisalignment; external;

//----------------------------------------------------------------------------------------------------------------------

end.
