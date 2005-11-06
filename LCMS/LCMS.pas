unit LCMS;

//----------------------------------------------------------------------------------------------------------------------
//  Little cms                                                   
//  Copyright (C) 1998-2001 Marti Maria
//
// THIS SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY of ANY KIND,
// EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
// WARRANTY of MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
//
// IN NO EVENT SHALL MARTI MARIA BE LIABLE FOR ANY SPECIAL, INCIDENTAL,
// INDIRECT OR CONSEQUENTIAL DAMAGES of ANY KIND,
// OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS of USE, DATA OR PROFITS,
// WHETHER OR NOT ADVISED of THE POSSIBILITY of DAMAGE, AND ON ANY THEORY of
// LIABILITY, ARISING OUT of OR IN CONNECTION WITH THE USE OR PERFORMANCE
// of THIS SOFTWARE.
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
//
//----------------------------------------------------------------------------------------------------------------------
//
// ver 1.10 - modified by Mike Lischke (www.delphi-gems.com) to support use of object files.
//            Additionally, source code reformatting was done to be conformal to common Delphi guidelines and those
//            set at Delphi Gems.
//----------------------------------------------------------------------------------------------------------------------

interface

{$Include Compilers.inc}

{$ifdef COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
  // We switch off warnings about that fact. We know it and we accept it.
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CAST off}
  {$warn UNSAFE_CODE off}
{$endif COMPILER_7_UP}

{$Align 4} // Structures are aligned on quad word boundaries.
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
  cmsFLAGS_MATRIXONLY             = (cmsFLAGS_MATRIXINPUT or cmsFLAGS_MATRIXOUTPUT);

  cmsFLAGS_NOTPRECALC             = $0100;
  cmsFLAGS_NULLTRANSFORM          = $0200;  // Don't transform.

  cmsFLAGS_HIGHRESPRECALC         = $0400;  // Use more memory to produce better accuracy.
  cmsFLAGS_LOWRESPRECALC          = $0800;  // Use less memory to minimize resouces.

  cmsFLAGS_GAMUTCHECK             = $1000;  // Mark Out of gamut as alarm color (on proofing transform).
  cmsFLAGS_SOFTPROOFING           = $4000;  // Soft proof of proofing profile.

  cmsFLAGS_WHITEBLACKCOMPENSATION = $2000;  // Matches black and white on precalculated transforms.

  // Pixel types.
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

  // Prebuilt format descriptors
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

  // ICC color space signatures.
  icSigXYZData                = $58595A20;
  icSigLabData                = $4C616220;
  icSigLuvData                = $4C757620;
  icSigYCbCrData              = $59436272;
  icSigYxyData                = $59787920;
  icSigRgbData                = $52474220;
  icSigGrayData               = $47524159;
  icSigHsvData                = $48535620;
  icSigHlsData                = $484C5320;
  icSigCmykData               = $434D594B;
  icSigCmyData                = $434D5920;

  // ICC Profile class signatures.
  icSigInputClass             = $73636E72;
  icSigDisplayClass           = $6D6E7472;
  icSigOutputClass            = $70727472;
  icSigLinkClass              = $6C696E6B;
  icSigAbstractClass          = $61627374;
  icSigColorSpaceClass        = $73706163;
  icSigNamedColorClass        = $6E6D636C;

  // Added by lcms.
  lcmsSignature               = $6c636d73;
  icSigLuvKData               = $4C75764B;  // 'LuvK'
  icSigChromaticityTag        = $6368726D;  // As per Addendum 2 to Spec. ICC.1:1998-09
  icSigChromaticityType       = $6368726D;

  AVG_SURROUND_4              = 0;
  AVG_SURROUND                = 1;
  DIM_SURROUND                = 2;
  DARK_SURROUND               = 3;
  CUTSHEET_SURROUND           = 4;

  D_CALCULATE                 = -1;
  D_CALCULATE_DISCOUNT        = -2;

  lcmsParametricCurveExp           = 0;
  lcmsParametricCurveCIE_122_1966  = 1;
  lcmsParametricCurveIEC_61966_3   = 2;
  lcmsParametricCurveIEC_61966_2_1 = 3;

  LCMS_USED_AS_INPUT          = 0;
  LCMS_USED_AS_OUTPUT         = 1;
  LCMS_USED_AS_PROOF          = 2;

  // Error handling
  LCMS_ERROR_ABORT            = 0;
  LCMS_ERROR_SHOW             = 1;
  LCMS_ERROR_IGNORE           = 2;

  // Vector members
  VX                          = 0;
  VY                          = 1;
  VZ                          = 2;

  // Shaper/Matrix handling
  MATSHAPER_HASMATRIX         = $0001; // Do-ops flags
  MATSHAPER_HASSHAPER         = $0002;
  MATSHAPER_INPUT             = $0004; // Behaviour
  MATSHAPER_OUTPUT            = $0008;
  MATSHAPER_HASINPSHAPER      = $0010;
  MATSHAPER_ALLSMELTED        = MATSHAPER_INPUT or MATSHAPER_OUTPUT;

  MAXCHANNELS                 = 16; // Maximum number of channels

type
  TTagSignature = Cardinal;
  TColorSpaceSignature = Cardinal;
  TProfileClassSignature = Cardinal;

  TCMSHPROFILE = Pointer;
  TCMSHTRANSFORM = Pointer;
  TFixed32 = Integer;       // Fixed 15.16 whith sign

  // Records are not packed because both sides (Delphi and BCB) are set to DWORD alignment.
  PGammaTable = ^TGammaTable;
  TGammaTable = record
    nEntries: Cardinal;
    GammaTable: array [0..1] of Word;
  end;

  TGammaTableArray = array[0..3] of PGammaTable;
  
  // Vectors
  PVEC3 = ^TVEC3;
  TVEC3 = record // Float Vector
    n: array[0..2] of Double;
  end;

  PMAT3 = ^TMAT3;
  TMAT3 = record // Matrix
    v: array[0..2] of TVEC3;
  end;

  // Colorimetric spaces.
  PCIEXYZTriple = ^TCIEXYZTriple;
  TCIEXYZTriple = record
    Red,
    Green,
    Blue: TCIEXYZ;
  end;

  PCIExyYTriple = ^TCIExyYTriple;
  TCIExyYTriple = record
    Red,
    Green,
    Blue: TCIExyY;
  end;

  // CIECAM97s.
  PLUT = Pointer;

  PViewingConditions = ^TViewingConditions;
  TViewingConditions = record
    WhitePoint: TCIEXYZ;
    Yb: Double;
    La: Double;
    Surround: Integer;
    D_value: Double
  end;

  // Used on 16 bits interpolations.
  PL16PARAMS = ^TL16PARAMS;
  TL16PARAMS = record
    nSamples: Integer;       // Valid on all kinds of tables
    nInputs: Integer;        // != 1 only in 3D interpolation
    nOutputs: Integer;       // != 1 only in 3D interpolation
    Domain: Word;
    opta1, opta2: Integer;
    opta3, opta4: Integer;   // Optimization for 3D LUT
    opta5, opta6: Integer;
    opta7, opta8: Integer;
  end;

  // Fixed 15.16 bits vector
  PWVEC3 = ^TWVEC3;
  TWVEC3 = record
    n: array[0..2] of TFixed32;
  end;

  // Matrix (Fixed 15.16)
  PWMAT3 = ^TWMAT3;
  TWMAT3 = record
    v: array[0..2] of TWVEC3;
  end;

  PMatShaper = ^TMatShaper;
  TMatShaper = record
    dwFlags: Cardinal;
    Matrix: TWMAT3;
    p16: TL16PARAMS;   // Primary curve
    L: array[0..2] of PWord;
    p2_16: TL16PARAMS; // Secondary curve (used as input in smelted ones)
    L2: array[0..2] of PWord;
  end;

  // Internal xform structure.
  Ptransform_struct = ^Ttransform_struct;
  Ttransform_struct = record
  end;

  TcmsFixFunction = function(info: Ptransform_struct; ToUnroll: array of Word; Buffer: PByte): PByte; pascal;
  TcmsADJProcedure = procedure(Input, Output: array of Word; m: PWMAT3; b: PWVEC3); cdecl;

  // Another internal transform structure.
  PcmsTRANSFORM = ^TcmsTRANSFORM;
  TcmsTRANSFORM = record
  end;

  TPCSArray = array[0..2] of Word;
  TColorantArray = array[0..MAXCHANNELS - 1] of Word;
  
  PNamedColor = ^TNamedColor;
  TNamedColor = record
    Name: array[0..MAX_PATH - 1] of Char;
    PCS: TPCSArray;
    DeviceColorant: TColorantArray;
  end;

  PNamedColorList = ^TNamedColorList;
  TNamedColorList = record
    nColors: Integer;
    Allocated: Integer;
    ColorantCount: Integer;
    List: PNamedColor;
    Prefix: array[0..32] of Char;
    Suffix: array[0..32] of Char;
  end;

// Some utility functions to compute new descriptors (derived from C macros).
function COLORSPACE_SH(e: Cardinal): Cardinal;
function SWAPFIRST_SH(e: Boolean): Cardinal;
function FLAVOR_SH(e: Boolean): Cardinal;
function PLANAR_SH(e: Boolean): Cardinal;
function ENDIAN16_SH(e: Boolean): Cardinal;
function DOSWAP_SH(e: Boolean): Cardinal;
function EXTRA_SH(e: Cardinal): Cardinal;
function CHANNELS_SH(c: Cardinal): Cardinal;
function BYTES_SH(b: Cardinal): Cardinal;                         

// Input/Output.
function cmsOpenProfileFromFile(ICCProfile: PChar; sAccess: PChar): TCMSHPROFILE; cdecl;
function cmsOpenProfileFromMem(MemPtr: Pointer; dwSize: Cardinal): TCMSHPROFILE; cdecl;
function cmsCloseProfile(hProfile: TCMSHPROFILE): Boolean; cdecl;
function cmsCreateRGBProfile(WhitePoint: PCIExyY; Primaries: PCIExyYTriple;
  TransferFunction: TGammaTableArray): TCMSHPROFILE; cdecl;                                        
function cmsCreateGrayProfile(WhitePoint: PCIExyY; TransferFunction: PGammaTable): TCMSHPROFILE; cdecl;
function cmsCreateLinearizationDeviceLink(ColorSpace: TColorSpaceSignature;
  TransferFunction: TGammaTableArray): TCMSHPROFILE; cdecl;
function cmsCreateInkLimitingDeviceLink(ColorSpace: TColorSpaceSignature; Limit: Double): TCMSHPROFILE; cdecl;
function cmsCreateLabProfile(WhitePoint: PCIExyY): TCMSHPROFILE; cdecl;
function cmsCreateXYZProfile: TCMSHPROFILE; cdecl;
function cmsCreate_sRGBProfile: TCMSHPROFILE; cdecl;

// Utilities.
procedure cmsXYZ2xyY(var Dest: TCIExyY; const Source: TCIEXYZ); cdecl;
procedure cmsxyY2XYZ(var Dest: TCIEXYZ; const Source: TCIExyY); cdecl;
procedure cmsXYZ2Lab(const WhitePoint: TCIEXYZ; var Lab: TCIELab; const xyz: TCIEXYZ); cdecl;
procedure cmsLab2XYZ(const WhitePoint: TCIEXYZ; var xyz: TCIEXYZ; const Lab: TCIELab); cdecl;
procedure cmsLab2LCh(var LCh: TCIELCh; const Lab: TCIELab); cdecl;
procedure cmsLCh2Lab(var Lab: TCIELab; const LCh: TCIELCh); cdecl;
function cmsD50_XYZ: PCIEXYZ; cdecl;
function cmsD50_xyY: PCIExyY; cdecl;
function Clamp_XYZ(Value: Integer): Word; cdecl;
function Clamp_RGB(Value: Integer): Word; cdecl;
function Clamp_L(Value: TFixed32): Word; cdecl;
function Clamp_ab(Value: TFixed32): Word; cdecl;

// CIELab handling
function cmsDeltaE(Lab1, Lab2: PCIELab): Double; cdecl;
function cmsCIE94DeltaE(Lab1, Lab2: PCIELab): Double; cdecl;
function cmsBFDdeltaE(Lab1, Lab2: PCIELab): Double; cdecl;
function cmsCMCdeltaE(Lab1, Lab2: PCIELab): Double; cdecl;
procedure cmsClampLab(Lab: PCIELab; amax, amin, bmax, bmin: Double); cdecl;

// White point.
function cmsWhitePointFromTemp(TempK: Integer; WhitePoint: PCIExyY): Boolean; cdecl;

// CIECAM97s
function cmsCIECAM97sInit(pVC: PViewingConditions): Pointer; cdecl;
procedure cmsCIECAM97sDone(hModel : Pointer); cdecl;
procedure cmsCIECAM97sForward(hModel: Pointer; pIn: PCIEXYZ; pOut: PJCh); cdecl;
procedure cmsCIECAM97sReverse(hModel: Pointer; pIn: PJCh; pOut: PCIEXYZ); cdecl;

// Gamma curves.
function cmsBuildGamma(nEntries: Integer; Gamma: Double): PGammaTable; cdecl;
function cmsAllocGamma(nEntries: Integer): PGammaTable; cdecl;
procedure cmsFreeGamma(Gamma: PGammaTable); cdecl;
procedure cmsFreeGammaTriple(Gamma: TGammaTableArray); cdecl;
function cmsReverseGamma(nResultSamples: Integer; InGamma : PGammaTable): PGammaTable; cdecl;
function cmsJoinGamma(InGamma, OutGamma: PGammaTable): PGammaTable; cdecl;
function cmsJoinGammaEx(InGamma, OutGamma: PGammaTable; nPoints: Integer): PGammaTable; cdecl;
function cmsSmoothGamma(Gamma: PGammaTable; SmoothingLambda: Double): Boolean; cdecl;
function cmsDupGamma(Src: PGammaTable): PGammaTable; cdecl;
function cmsEstimateGamma(Src: PGammaTable): Double; cdecl;
function cmsEstimateGammaEx(Src: PGammaTable; Thereshold: Double): Double; cdecl;
function cmsReadICCGamma(hProfile: TCMSHPROFILE; Sig: TTagSignature): PGammaTable; cdecl;
function cmsReadICCGammaReversed(hProfile: TCMSHPROFILE; Sig: TTagSignature): PGammaTable; cdecl;
function cmsBuildParametricGamma(nEntries: Integer; TheType: Integer; Params: array of Double): PGammaTable; cdecl;

// Access to profile data.
procedure cmsSetLanguage(LanguageCode: Integer; CountryCode: Integer); cdecl;
function cmsTakeMediaWhitePoint(Dest: PCIEXYZ; hProfile: TCMSHPROFILE): Boolean; cdecl;
function cmsTakeMediaBlackPoint(Dest: PCIEXYZ; hProfile: TCMSHPROFILE): Boolean; cdecl;
function cmsTakeIluminant(Dest: PCIEXYZ; hProfile: TCMSHPROFILE): Boolean; cdecl;
function cmsTakeColorants(Dest: PCIEXYZTriple; hProfile: TCMSHPROFILE): Boolean; cdecl;
function cmsTakeProductName(hProfile: TCMSHPROFILE): PChar; cdecl;
function cmsTakeProductDesc(hProfile: TCMSHPROFILE): PChar; cdecl;
function cmsIsTag(hProfile: TCMSHPROFILE; sig: TTagSignature): Boolean; cdecl;
function cmsTakeRenderingIntent(hProfile: TCMSHPROFILE): Integer; cdecl;
function cmsIsIntentSupported(hProfile: TCMSHPROFILE; Intent, UsedDirection: Integer): Integer; cdecl;
function cmsTakeCharTargetData(hProfile: TCMSHPROFILE; var Data: PChar; var len: Cardinal): Boolean; cdecl;
function _cmsICCcolorSpace(OurNotation: Integer): TColorSpaceSignature; cdecl;
function _cmsChannelsOf(ColorSpace: TColorSpaceSignature): Integer; cdecl;
function cmsGetPCS(hProfile: TCMSHPROFILE): TColorSpaceSignature; cdecl;
function cmsGetColorSpace(hProfile: TCMSHPROFILE): TColorSpaceSignature; cdecl;
function cmsGetDeviceClass( hProfile: TCMSHPROFILE): TProfileClassSignature; cdecl;

// Profile creation.
procedure cmsSetDeviceClass(hProfile: TCMSHPROFILE; sig: TProfileClassSignature); cdecl;
procedure cmsSetColorSpace(hProfile: TCMSHPROFILE; sig: TColorSpaceSignature); cdecl;
procedure cmsSetPCS(hProfile: TCMSHPROFILE; pcs: TColorSpaceSignature); cdecl;
function cmsAddTag(hProfile: TCMSHPROFILE; Sig: TTagSignature; Data: Pointer): Boolean; cdecl;
function _cmsSaveProfile(hProfile: TCMSHPROFILE; FileName: PChar): Boolean; cdecl;
function _cmsSaveProfileToMem(hProfile: TCMSHPROFILE; MemPtr: Pointer; var BytesNeeded: Cardinal): Boolean; cdecl;

// Transforms.
function cmsCreateTransform(Input: TCMSHPROFILE; InputFormat: Cardinal; Output: TCMSHPROFILE; OutputFormat: Cardinal;
  Intent: Integer; dwFlags: Cardinal): TCMSHTRANSFORM; cdecl;
function cmsCreateProofingTransform(Input: TCMSHPROFILE; InputFormat: Cardinal; Output: TCMSHPROFILE;
  OutputFormat: Cardinal; Proofing: TCMSHPROFILE; Intent: Integer; ProofingIntent: Integer;
  dwFlags: Cardinal): TCMSHTRANSFORM; cdecl;
function cmsCreateMultiprofileTransform(hProfiles: array of TCMSHPROFILE; nProfiles: Integer; InputFormat: Cardinal;
  OutputFormat: Cardinal; Intent: Integer; dwFlags: Cardinal): TCMSHTRANSFORM; cdecl;
procedure cmsDeleteTransform(hTransform: TCMSHTRANSFORM); cdecl;
procedure cmsDoTransform(Transform: TCMSHTRANSFORM; InputBuffer: Pointer; OutputBuffer: Pointer; Size: Integer); cdecl;
procedure cmsChangeBuffersFormat(hTransform: TCMSHTRANSFORM; dwInputFormat, dwOutputFormat: Cardinal); cdecl;

// Devicelink generation.
function cmsTransform2DeviceLink(hTransform: TCMSHTRANSFORM; dwFlags: Cardinal): TCMSHPROFILE; cdecl;

// Named color support.
function cmsNamedColorCount(xform: TCMSHTRANSFORM): Integer; cdecl;
function cmsNamedColorInfo(xform: TCMSHTRANSFORM; nColor: Integer; Name, Prefix, Suffix: PChar): Boolean; cdecl;
function cmsNamedColorIndex(xform: TCMSHTRANSFORM; Name: PChar): Integer; cdecl;
function cmsAllocNamedColorList(): PNamedColorList; cdecl;
function cmsReadICCnamedColorList(xform: TcmsHTRANSFORM; hProfile: TcmsHPROFILE; sig: TTagSignature): Integer; cdecl;
procedure cmsFreeNamedColorList(List: PNamedColorList); cdecl;
function cmsAppendNamedColor(xform: TcmsHTRANSFORM; const Name: PChar; PCS: TPCSArray;
  Colorant: TColorantArray): BOOL; cdecl;

// PostScript ColorRenderingDictionary and ColorSpaceArray.
function cmsGetPostScriptCSA(hProfile: TCMSHPROFILE; Intent: Integer; Buffer: Pointer;
  dwBufferLen: Cardinal): Cardinal; cdecl;
function cmsGetPostScriptCRD(hProfile: TCMSHPROFILE; Intent: Integer; Buffer: Pointer;
  dwBufferLen: Cardinal): Cardinal; cdecl;

// Gamut check.
procedure cmsSetAlarmCodes(r, g, b: Integer); cdecl;
procedure cmsGetAlarmCodes(var r, g, b: Integer); cdecl;

// Error handling
procedure cmsErrorAction(nAction: Integer); cdecl;

// Provided for compatibility with anterior revisions.
procedure cmsLabEncoded2Float(Lab: PCIELab; wLab: Pointer); cdecl;
procedure cmsFloat2LabEncoded(wLab: Pointer; Lab: PCIELab); cdecl;
procedure cmsXYZEncoded2Float(fxyz : PCIEXYZ; XYZ: Pointer); cdecl;
procedure cmsFloat2XYZEncoded(XYZ: Pointer; fXYZ: PCIEXYZ); cdecl;
function _cmsAddTextTag(hProfile: TCMSHPROFILE; sig: TTagSignature; Text: PChar): Boolean; cdecl;
function _cmsAddXYZTag(hProfile: TCMSHPROFILE; sig: TTagSignature; XYZ: PCIEXYZ): Boolean; cdecl;
function _cmsAddLUTTag(hProfile: TCMSHPROFILE; sig: TTagSignature; lut: PByte): Boolean; cdecl;

// LUT/interpolation support
procedure cmsCalcL16Params(nSamples: Integer; p: PL16PARAMS); cdecl;
procedure cmsCalcCLUT16Params(nSamples, InputChan, OutputChan: Integer; p: PL16PARAMS); cdecl;
function cmsLinearInterpLUT16(Value: Word; LutTable: array of Word; p: PL16PARAMS): Word; cdecl;
function cmsLinearInterpFixed(Value1: Word; LutTable: array of Word; p: PL16PARAMS): TFixed32; cdecl;
function cmsReverseLinearInterpLUT16(Value: Word; LutTable: array of Word; p: PL16PARAMS): Word; cdecl;
procedure cmsTrilinearInterp16(Input, Output, LutTable: array of Word; p: PL16PARAMS); cdecl;
procedure cmsTetrahedralInterp16(Input, Output, LutTable: array of Word; p: PL16PARAMS); cdecl;

// Shaper/Matrix handling
function cmsAllocMatShaper(matrix: PMAT3; Shaper: TGammaTableArray; Behaviour: Cardinal): PMatShaper; cdecl;
function cmsAllocMatShaper2(matrix: PMAT3; Input, Output: TGammaTableArray; Behaviour: Cardinal): PMatShaper; cdecl;
procedure cmsFreeMatShaper(MatShaper: PMatShaper); cdecl;
procedure cmsEvalMatShaper(MatShaper: PMatShaper; Input, Output: array of Word); cdecl;
function cmsReadICCMatrixRGB2XYZ(r: PMAT3; hProfile: TcmsHPROFILE): BOOL; cdecl;
function cmsBuildInputMatrixShaper(InputProfile: TcmsHPROFILE; dwFlags: PCardinal): PMatShaper; cdecl;
function cmsBuildOutputMatrixShaper(OutputProfile: TcmsHPROFILE; dwFlags: PCardinal): PMatShaper; cdecl;

function _cmsIdentifyInputFormat(xform: PcmsTRANSFORM;  dwInput: Cardinal): TcmsFixFunction; cdecl;
function _cmsIdentifyOutputFormat(xform: PcmsTRANSFORM; dwOutput: Cardinal): TcmsFixFunction; cdecl;

// LUT support
function cmsAllocLUT(): PLUT; cdecl;
function cmsAllocLinearTable(NewLUT: PLUT; Tables: TGammaTableArray; nTable: Integer): PLUT; cdecl;
function cmsAlloc3DGrid(Lut: PLUT; clutPoints, inputChan, outputChan: Integer): PLUT; cdecl;
function cmsSetMatrixLUT(Lut: PLUT; M: PMAT3):PLUT; cdecl;
procedure cmsFreeLUT(Lut: PLUT); cdecl;
procedure cmsEvalLUT(Lut: PLUT; Input: array of Integer; Output: array of Word); cdecl;
function cmsReadICCLut(hProfile: TcmsHPROFILE; sig: TTagSignature): PLUT; cdecl;
function cmsDupLUT(Orig: PLUT): PLUT; cdecl;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  LibStub;

{$L cmsgmt.obj}
{$L cmscam97.obj}
{$L cmscnvrt.obj}
{$L cmserr.obj}
{$L cmsgamma.obj}
{$L cmsintrp.obj}
{$L cmsio1.obj}
{$L cmslut.obj}
{$L cmsmatsh.obj}
{$L cmsmtrx.obj}
{$L cmsnamed.obj}
{$L cmspack.obj}
{$L cmspcs.obj}
{$L cmsps2.obj}
{$L cmssamp.obj}
{$L cmswtpnt.obj}
{$L cmsxform.obj}

//----------------------------------------------------------------------------------------------------------------------

function cmsOpenProfileFromFile(ICCProfile : PChar; sAccess: PChar): TCMSHPROFILE; external;
function cmsOpenProfileFromMem(MemPtr: Pointer; dwSize: Cardinal): TCMSHPROFILE; external;
function cmsWhitePointFromTemp(TempK: Integer; WhitePoint: PCIExyY): Boolean; external;

function cmsBuildGamma(nEntries: Integer; Gamma: Double): PGammaTable; external;
function cmsAllocGamma(nEntries: Integer): PGammaTable; external;
procedure cmsFreeGamma(Gamma: PGammaTable); external;
procedure cmsFreeGammaTriple(Gamma: TGammaTableArray); external;

function cmsReverseGamma(nResultSamples: Integer; InGamma: PGammaTable): PGammaTable; external;
function cmsBuildParametricGamma(nEntries: Integer; TheType: Integer; Params: array of Double): PGammaTable; external;

function cmsJoinGamma(InGamma, OutGamma: PGammaTable): PGammaTable; external;
function cmsJoinGammaEx(InGamma, OutGamma: PGammaTable; nPoints: Integer): PGammaTable; external;

function cmsSmoothGamma(Gamma: PGammaTable; SmoothingLambda: Double): Boolean; external;

function cmsDupGamma(Src: PGammaTable): PGammaTable; external;
function cmsEstimateGamma(Src: PGammaTable): Double; external;
function cmsEstimateGammaEx(Src: PGammaTable; Thereshold: Double): Double; external;
function cmsReadICCGamma(hProfile: TCMSHPROFILE; Sig: TTagSignature): PGammaTable; external;
function cmsReadICCGammaReversed(hProfile: TCMSHPROFILE; Sig: TTagSignature): PGammaTable; external;

function cmsCreateRGBProfile( WhitePoint : PCIExyY; Primaries: PCIExyYTriple;
  TransferFunction: TGammaTableArray): TCMSHPROFILE; external;
function cmsCreateGrayProfile(WhitePoint: PCIExyY; TransferFunction: PGammaTable): TCMSHPROFILE; external;
function cmsCreateLinearizationDeviceLink(ColorSpace: TColorSpaceSignature;
  TransferFunction: TGammaTableArray): TCMSHPROFILE; external;
function cmsCreateInkLimitingDeviceLink(ColorSpace: TColorSpaceSignature; Limit: Double): TCMSHPROFILE; external;

function cmsCreateLabProfile(WhitePoint: PCIExyY): TCMSHPROFILE; external;

function cmsCreateXYZProfile: TCMSHPROFILE; external;
function cmsCreate_sRGBProfile: TCMSHPROFILE; external;

function cmsCloseProfile( hProfile : TCMSHPROFILE): Boolean; external;

procedure cmsSetLanguage(LanguageCode: Integer; CountryCode: Integer); external;

function cmsTakeMediaWhitePoint(Dest: PCIEXYZ; hProfile: TCMSHPROFILE): Boolean; external;
function cmsTakeMediaBlackPoint(Dest: PCIEXYZ; hProfile: TCMSHPROFILE): Boolean; external;

function cmsTakeIluminant(Dest: PCIEXYZ; hProfile: TCMSHPROFILE): Boolean; external;
function cmsTakeColorants(Dest: PCIEXYZTriple; hProfile: TCMSHPROFILE): Boolean; external;

function cmsTakeProductName(hProfile: TCMSHPROFILE): PChar; external;
function cmsTakeProductDesc(hProfile: TCMSHPROFILE): PChar; external;
function cmsIsTag(hProfile: TCMSHPROFILE; sig: TTagSignature): Boolean; external;
function cmsTakeRenderingIntent( hProfile: TCMSHPROFILE): Integer; external;
function cmsGetPCS(hProfile: TCMSHPROFILE): TColorSpaceSignature; external;
function cmsGetColorSpace(hProfile: TCMSHPROFILE): TColorSpaceSignature; external;
function cmsGetDeviceClass( hProfile: TCMSHPROFILE): TProfileClassSignature; external;
function cmsTakeCharTargetData(hProfile: TCMSHPROFILE; var Data: PChar; var len: Cardinal): Boolean; external;

function _cmsICCcolorSpace(OurNotation: Integer): TColorSpaceSignature; external;
function _cmsChannelsOf(ColorSpace: TColorSpaceSignature): Integer; external;

procedure cmsSetDeviceClass(hProfile: TCMSHPROFILE; sig: TProfileClassSignature); external;
procedure cmsSetColorSpace(hProfile: TCMSHPROFILE; sig: TColorSpaceSignature); external;
procedure cmsSetPCS(hProfile: TCMSHPROFILE; pcs: TColorSpaceSignature); external;

function cmsAddTag(hProfile: TCMSHPROFILE; Sig: TTagSignature; Data: Pointer): Boolean; external;

function _cmsSaveProfile(hProfile: TCMSHPROFILE; FileName: PChar): Boolean; external;
function _cmsSaveProfileToMem(hProfile: TCMSHPROFILE; MemPtr: Pointer; var BytesNeeded: Cardinal): Boolean; external;

function cmsIsIntentSupported(hProfile: TCMSHPROFILE; Intent, UsedDirection : Integer): Integer; external;

function cmsCreateTransform(Input: TCMSHPROFILE; InputFormat: Cardinal; Output: TCMSHPROFILE; OutputFormat: Cardinal;
  Intent: Integer; dwFlags: Cardinal): TCMSHTRANSFORM; external;
function cmsCreateProofingTransform(Input: TCMSHPROFILE; InputFormat: Cardinal; Output: TCMSHPROFILE;
  OutputFormat: Cardinal; Proofing: TCMSHPROFILE; Intent: Integer; ProofingIntent: Integer;
  dwFlags: Cardinal): TCMSHTRANSFORM; external;
function cmsCreateMultiprofileTransform(hProfiles : array of TCMSHPROFILE; nProfiles : Integer; InputFormat: Cardinal;
  OutputFormat: Cardinal; Intent: Integer; dwFlags: Cardinal): TCMSHTRANSFORM; external;
procedure cmsDeleteTransform(hTransform: TCMSHTRANSFORM); external;
procedure cmsDoTransform(Transform: TCMSHTRANSFORM; InputBuffer: Pointer; OutputBuffer: Pointer;
  Size: Integer); external;
procedure cmsChangeBuffersFormat(hTransform: TCMSHTRANSFORM; dwInputFormat, dwOutputFormat: Cardinal);  external;

function cmsTransform2DeviceLink(hTransform: TCMSHTRANSFORM; dwFlags: Cardinal): TCMSHPROFILE; external;

function cmsNamedColorCount(xform: TCMSHTRANSFORM): Integer; external;
function cmsNamedColorInfo(xform: TCMSHTRANSFORM; nColor: Integer; Name, Prefix, Suffix: PChar): Boolean; external;
function cmsNamedColorIndex(xform: TCMSHTRANSFORM; Name: PChar): Integer; external;
function cmsAllocNamedColorList(): PNamedColorList; external;
function cmsReadICCnamedColorList(xform: TcmsHTRANSFORM; hProfile: TcmsHPROFILE; sig: TTagSignature): Integer; external;
procedure cmsFreeNamedColorList(List: PNamedColorList); external;
function cmsAppendNamedColor(xform: TcmsHTRANSFORM; const Name: PChar; PCS: TPCSArray;
  Colorant: TColorantArray): BOOL; external;

function cmsGetPostScriptCSA(hProfile: TCMSHPROFILE; Intent: Integer; Buffer: Pointer;
  dwBufferLen: Cardinal): Cardinal; external;
function cmsGetPostScriptCRD(hProfile: TCMSHPROFILE; Intent: Integer; Buffer: Pointer;
  dwBufferLen: Cardinal): Cardinal; external;

function cmsCIECAM97sInit(pVC: PViewingConditions): Pointer; external;
procedure cmsCIECAM97sDone(hModel: Pointer); external;

procedure cmsCIECAM97sForward(hModel: Pointer; pIn: PCIEXYZ; pOut: PJCh); external;
procedure cmsCIECAM97sReverse(hModel: Pointer; pIn: PJCh; pOut: PCIEXYZ); external;

procedure cmsXYZ2xyY(var Dest: TCIExyY; const Source: TCIEXYZ); external;
procedure cmsxyY2XYZ(var Dest: TCIEXYZ; const Source: TCIExyY); external;
procedure cmsXYZ2Lab(const WhitePoint: TCIEXYZ; var Lab: TCIELab; const xyz: TCIEXYZ); external;
procedure cmsLab2XYZ(const WhitePoint: TCIEXYZ; var xyz: TCIEXYZ; const Lab: TCIELab); external;
procedure cmsLab2LCh(var LCh: TCIELCh; const Lab: TCIELab); external;
procedure cmsLCh2Lab(var Lab: TCIELab; const LCh: TCIELCh); external;
function cmsD50_XYZ: PCIEXYZ; external;
function cmsD50_xyY: PCIExyY; external;
function Clamp_XYZ(Value: Integer): Word; external;
function Clamp_RGB(Value: Integer): Word; external;
function Clamp_L(Value: TFixed32): Word; external;
function Clamp_ab(Value: TFixed32): Word; external;

function cmsDeltaE(Lab1, Lab2: PCIELab): Double; external;
function cmsCIE94DeltaE(Lab1, Lab2: PCIELab): Double; external;
function cmsBFDdeltaE(Lab1, Lab2: PCIELab): Double; external;
function cmsCMCdeltaE(Lab1, Lab2: PCIELab): Double; external;

procedure cmsClampLab(Lab: PCIELab; amax, amin, bmax, bmin: Double); external;

procedure cmsSetAlarmCodes(r, g, b: Integer); external;
procedure cmsGetAlarmCodes(var r, g, b: Integer); external;

procedure cmsErrorAction(nAction: Integer); external;

procedure cmsLabEncoded2Float(Lab: PCIELab; wLab: Pointer); external;
procedure cmsFloat2LabEncoded(wLab: Pointer; Lab: PCIELab); external;
procedure cmsXYZEncoded2Float(fxyz : PCIEXYZ; XYZ: Pointer); external;
procedure cmsFloat2XYZEncoded(XYZ: Pointer; fXYZ: PCIEXYZ); external;

function _cmsAddTextTag(hProfile: TCMSHPROFILE; sig: TTagSignature; Text: PChar): Boolean; external;
function _cmsAddXYZTag(hProfile: TCMSHPROFILE;  sig: TTagSignature;  XYZ: PCIEXYZ): Boolean; external;
function _cmsAddLUTTag(hProfile: TCMSHPROFILE;  sig: TTagSignature; lut: PByte): Boolean; external;

procedure cmsCalcL16Params(nSamples: Integer; p: PL16PARAMS); external;
procedure cmsCalcCLUT16Params(nSamples, InputChan, OutputChan: Integer; p: PL16PARAMS); external;
function cmsLinearInterpLUT16(Value: Word; LutTable: array of Word; p: PL16PARAMS): Word; external;
function cmsLinearInterpFixed(Value1: Word; LutTable: array of Word; p: PL16PARAMS): TFixed32; external;
function cmsReverseLinearInterpLUT16(Value: Word; LutTable: array of Word; p: PL16PARAMS): Word; external;
procedure cmsTrilinearInterp16(Input, Output, LutTable: array of Word; p: PL16PARAMS); external;
procedure cmsTetrahedralInterp16(Input, Output, LutTable: array of Word; p: PL16PARAMS); external;

// Shaper/Matrix handling
function cmsAllocMatShaper(matrix: PMAT3; Shaper: TGammaTableArray; Behaviour: Cardinal): PMatShaper; external;
function cmsAllocMatShaper2(matrix: PMAT3; Input, Output: TGammaTableArray; Behaviour: Cardinal): PMatShaper; external;
procedure cmsFreeMatShaper(MatShaper: PMatShaper); external;
procedure cmsEvalMatShaper(MatShaper: PMatShaper; Input, Output: array of Word); external;
function cmsReadICCMatrixRGB2XYZ(r: PMAT3; hProfile: TcmsHPROFILE): BOOL; external;
function cmsBuildInputMatrixShaper(InputProfile: TcmsHPROFILE; dwFlags: PCardinal): PMatShaper; external;
function cmsBuildOutputMatrixShaper(OutputProfile: TcmsHPROFILE; dwFlags: PCardinal): PMatShaper; external;

function _cmsIdentifyInputFormat(xform: PcmsTRANSFORM;  dwInput: Cardinal): TcmsFixFunction; external;
function _cmsIdentifyOutputFormat(xform: PcmsTRANSFORM; dwOutput: Cardinal): TcmsFixFunction; external;

function cmsAllocLUT(): PLUT; external;
function cmsAllocLinearTable(NewLUT: PLUT; Tables: TGammaTableArray; nTable: Integer): PLUT; external;
function cmsAlloc3DGrid(Lut: PLUT; clutPoints, inputChan, outputChan: Integer): PLUT; external;
function cmsSetMatrixLUT(Lut: PLUT; M: PMAT3):PLUT; external;
procedure cmsFreeLUT(Lut: PLUT); external;
procedure cmsEvalLUT(Lut: PLUT; Input: array of Integer; Output: array of Word); external;
function cmsReadICCLut(hProfile: TcmsHPROFILE; sig: TTagSignature): PLUT; external;
function cmsDupLUT(Orig: PLUT): PLUT; external;

// Others (used internally)
procedure _cmsComputePrelinearizationTablesFromXFORM(h: array of TcmsHTRANSFORM; nTransforms: Integer;
  Grid: PLUT); cdecl; external;
function MAT3inverse(a, b: PMAT3): Integer; cdecl; external;
function _cmsWhiteBySpace(Space: TColorSpaceSignature): PWord; cdecl; external;
function cmsChooseCnvrt(_Absolute, Phase1: Integer; BlackPointIn, WhitePointIn, IlluminantIn: PCIEXYZ;
  Phase2: Integer; BlackPointOut, WhitePointOut, IlluminantOut: PCIEXYZ; fn1: TcmsADJProcedure; wm: PWMAT3;
  wof: PWVEC3): Integer; cdecl; external;
function _cmsSmoothEndpoints(Table: PWORD; nEntries: Integer): BOOL; cdecl; external;
function _cmsEndPointsBySpace(Space: TColorSpaceSignature; var White, Black: PWord;
  var nOutputs: Integer): BOOL; cdecl; external;
function _cmsComputeSoftProofLUT(hProfile: TcmsHPROFILE; nIntent: Integer): PLUT; cdecl; external;
function _cmsComputeGamutLUT(hProfile: TcmsHPROFILE; Intent: Integer): PLUT; cdecl; external;
function _cmsReasonableGridpointsByColorspace(Colorspace: TColorSpaceSignature; dwFlags: Cardinal): Integer; cdecl; external;

//----------------- Helper functions -----------------------------------------------------------------------------------

function COLORSPACE_SH(e: Cardinal):Cardinal;

begin
  Result := e shl 16;
end;

//----------------------------------------------------------------------------------------------------------------------

function SWAPFIRST_SH(e: Boolean): Cardinal;

begin
  if e then
    Result := 1 shl 14
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function FLAVOR_SH(e: Boolean): Cardinal;

begin
  if e then
    Result := 1 shl 13
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function PLANAR_SH(e: Boolean): Cardinal;

begin
  if e then
    Result := 1 shl 12
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function ENDIAN16_SH(e: Boolean): Cardinal;

begin
  if e then
    Result := 1 shl 11
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function DOSWAP_SH(e: Boolean): Cardinal;

begin
  if e then
    Result := 1 shl 10
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function EXTRA_SH(e: Cardinal): Cardinal;

begin
  Result := e shl 7;
end;

//----------------------------------------------------------------------------------------------------------------------

function CHANNELS_SH(c: Cardinal): Cardinal;

begin
  Result := c shl 3;
end;

//----------------------------------------------------------------------------------------------------------------------

function BYTES_SH(b: Cardinal): Cardinal;

begin
  Result := b;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
