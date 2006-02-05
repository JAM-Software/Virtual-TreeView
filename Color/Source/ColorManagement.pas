unit ColorManagement;

//----------------------------------------------------------------------------------------------------------------------
// ColorManagement contains general purpose functions for handling colors and requires Delphi 6 or better to compile.
// The calculations contained here are optimized for precision, not for speed, while still keeping complexity at a
// reasonable level. Unless otherwise specified all XYZ coordinates are given for a 2° observer.
// For color management an additional unit is used (LCMS, Little Color Management System by Marti Maria (www.littlecms.com).
//
//----------------------------------------------------------------------------------------------------------------------
//
// This unit is released under the MIT license:
// Copyright (c) 1999-2005 Mike Lischke (support@soft-gems.net, www.soft-gems.net).
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
// documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the
// Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
// WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
// OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
// You are asked to give the author(s) the due credit. This means that you acknowledge the work of the author(s)
// in the product documentation, about box, help or wherever a prominent place is.
//
//----------------------------------------------------------------------------------------------------------------------
// The original code is ColorManagement.pas, released 1. June 2003.
//
// The initial developer of the original code is:
//   Mike Lischke, Delphi Gems software solutions (support@delphi-gems.com, www.delphi-gems.com).
//
// References:
// [1] Foley, van Dam, Feiner, Hughs, "Computer graphics, principles and practice", second edition, Addison-Wesley 1990
// [2] Bruce Lindbloom, www.brucelindbloom.com
// [3] Earl F. Glynn, www.efg2.com/Lab
// [4] Gernot Hoffmann, "CIE Lab Color Space" PDF document, http://www.fho-emden.de/~hoffmann/
// [5] X-Rite, "A guide to understanding color communication", PDF document, http://www.x-rite.com
// [6] J A Stephen Viggiano, "Modeling the Color of Multi-Colored Halftones", PDF document (1990_TAGA.pdf)
//
// Portions created by Delphi Gems are
//   (C) 1999-2004 Delphi Gems. All Rights Reserved.
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

uses
  Windows, Classes, ColorTypes, LCMS, ColorTools;

type
  // TColorProfile is the base class for color profiles used in color conversions
  // in conjunction with color managment. Each color format supported by the CMS gets its own
  // color profile class.
  TColorProfile = class
  private
    FProfile: TCMSHPROFILE;
  protected
    function CreateProfile: TCMSHPROFILE; virtual; abstract;
    function GetChannels: Cardinal; virtual; 
    function GetColorFormat: TColorFormat; virtual;
    function GetProfile: TCMSHPROFILE;
  public
    destructor Destroy; override;

    property Channels: Cardinal read GetChannels;
    property ColorFormat: TColorFormat read GetColorFormat;
    property Profile: TCMSHPROFILE read GetProfile;
  end;

  // TExternalProfile is a color profile class, which uses an external ICM profile file. The color format and other
  // properties are given by the file.
  TExternalProfile = class(TColorProfile)
  private
    FProfileFile: string;
    FProfileStream: TResourceStream; // Used only if the profile is in the resource.
  protected
    function CreateProfile: TCMSHPROFILE; override;
  public
    constructor Create(ICMFile: string); virtual;
    destructor Destroy; override;
  end;

  TCMYKProfile = class(TExternalProfile)
  private
    FUseBlack: Boolean;       // True: CMYK, False: CMY.
  protected
    function GetChannels: Cardinal; override;
    function GetColorFormat: TColorFormat; override;
  public
    constructor Create(ICMFile: string; UseBlack: Boolean = True); reintroduce; virtual;
  end;

  TLabProfile = class(TColorProfile)
  private
    FWhitePoint: TCIExyY;
  protected
    function CreateProfile: TCMSHPROFILE; override;
    function GetChannels: Cardinal; override;
    function GetColorFormat: TColorFormat; override;
  public
    constructor Create(WhitePoint: TWhitePoint); overload;
    constructor Create(WhitePoint: TCIEXYZ); overload;
    constructor Create(WhitePoint: TCIExyY); overload;
  end;            

  TsRGBProfile = class(TColorProfile)
  protected
    function CreateProfile: TCMSHPROFILE; override;
    function GetColorFormat: TColorFormat; override;
  end;
  
  TRGBProfile = class(TColorProfile)
  private
    FWhitePoint: TCIExyY;
    FGamma: Double;
    FUseAlpha: Boolean; // True: RGBA; False: RGB
    FPrimaries: TPrimaries;
  protected
    function CreateProfile: TCMSHPROFILE; override;
    function GetChannels: Cardinal; override;
    function GetColorFormat: TColorFormat; override;
  public
    constructor Create(WhitePoint: TWhitePoint; Primaries: TPrimaries = primHDTV; Gamma: Double = 1;
      UseAlpha: Boolean = False); overload;
    constructor Create(WhitePoint: TCIEXYZ; Primaries: TPrimaries = primHDTV; Gamma: Double = 1;
      UseAlpha: Boolean = False); overload;
    constructor Create(WhitePoint: TCIExyY; Primaries: TPrimaries = primHDTV; Gamma: Double = 1;
      UseAlpha: Boolean = False); overload;
  end;

  TGrayProfile = class(TColorProfile)
  private
    FWhitePoint: TCIExyY;
    FGamma: Double;
  protected
    function CreateProfile: TCMSHPROFILE; override;
    function GetChannels: Cardinal; override;
    function GetColorFormat: TColorFormat; override;
  public
    constructor Create(WhitePoint: TWhitePoint; Gamma: Double); overload;
    constructor Create(WhitePoint: TCIEXYZ; Gamma: Double); overload;
    constructor Create(WhitePoint: TCIExyY; Gamma: Double); overload;
  end;

  TXYZProfile = class(TColorProfile)
  protected
    function CreateProfile: TCMSHPROFILE; override;
    function GetChannels: Cardinal; override;
    function GetColorFormat: TColorFormat; override;
  end;

  // A display profile is used when the target of a color conversion is the screen. This class tries to open the ICC
  // profile currently associated with the display. If there is no ICC profile associated then sRGB is used.
  TDisplayProfile = class(TColorProfile)
  protected
    function CreateProfile: TCMSHPROFILE; override;
    function GetChannels: Cardinal; override;
    function GetColorFormat: TColorFormat; override;
  end;

  // These options control the source and target color formats.
  TProfileOptions = record
    MinIsWhite: Boolean;     // If true then maximum values are denoted with 0 (e.g. in CMYK white is the combination
                             // of 0 for all inks).
    Planar: Boolean;         // True if colors are given in planes, False for interleaved values.
    BigEndian: Boolean;      // True if 16 bpp values must be swapped first (not used for 8 bpp).
    SwapComponents: Boolean; // True for reverse order, e.g. BGR, KYMC etc.
    ExtraSamples: Integer;   // Number of extra samples, which are not converted (e.g. alpha in RGBA is 1 extra channel).
    Channels: Integer;       // Number of color components (samples per pixel).
    BytesPerSample: Integer; // Number of bytes for each color component. Currently 0, 1 and 2 supported, where 0 means
                             // each component is a double floating point number. The values 1 and 2 denote 8 and 16 bits
                             // per pixel, respectively.
    SwapFirst: Boolean;      // True if first component must be moved to the end (e.g. BGRA to ABGR and KCMY to CMYK).
  end;

  // The rendering intent describes how conversion of colors should be done with respect to out-of-gamut colors.
  // Read the help file about rendering intents. Default is "perceptual" if the given intent is not supported
  // by a certain color profile.
  TRenderingIntent = (
    riPerceptual,
    riRelativeColorimetric,
    riSaturation,
    riAbsoluteColorimetric
  );

  // TColorConverter is a general purpose class to convert one or more colors in a buffer from one color scheme to another.
  // The class uses CMS for conversion and works therefore only with proper color profiles. See help for more details.
  TColorConverter = class
  private
    FTransformation: TCMSHTRANSFORM;
    FSourceProfile,
    FTargetProfile: TColorProfile;
    FSourceOptions,
    FTargetOptions: TProfileOptions;
    FRenderingIntent: TRenderingIntent;
  protected
    procedure CreateTransformation;
  public
    constructor Create(SourceProfile: TColorProfile; const SourceOptions: TProfileOptions; TargetProfile: TColorProfile;
      const TargetOptions: TProfileOptions; Intent: TRenderingIntent); virtual;
    destructor Destroy; override;

    procedure Convert(const Source, Target: Pointer; Count: Integer);

    property RenderingIntent: TRenderingIntent read FRenderingIntent;
  end;

// Helper functions for color management.
function GetFullColorProfileFileName(const FileName: string): string;
function GetProfileColorSpace(FileName: string): icColorSpaceSignature;

// APIs, which are either not yet defined or are defined wrongly.
function GetColorDirectory(pMachineName: PChar; pBuffer: PChar; var pdwSize: Cardinal): BOOL; stdcall;
  external 'Mscms.dll' name 'GetColorDirectoryA';

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Math, SysUtils;

const
  // Do not modify the copyright in any way! Usage of this unit is prohibited without the copyright notice
  // in the compiled binary file.
  Copyright: string = 'Color Management © 2003, 2004 Mike Lischke, Delphi Gems software solutions';

//----------------- Color managment helper functions -------------------------------------------------------------------

function GetFullColorProfileFileName(const FileName: string): string;

// Returns a fully qualified path to a color profile, whose base name is given in FileName. If FileName already
// contains a full path (or can be found from the current directory) then it is simply returned, otherwise the name is
// combined with the system's color directory.

var
  Buffer: array[0..MAX_PATH] of Char;
  BufferSize: Cardinal;

begin
  Result := FileName;
  if not FileExists(FileName) then
  begin
    BufferSize := SizeOf(Buffer);
    GetColorDirectory(nil, Buffer, BufferSize);
    if FileExists(IncludeTrailingPathDelimiter(Buffer) + FileName) then
      Result := IncludeTrailingPathDelimiter(Buffer) + FileName;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetProfileColorSpace(FileName: string): icColorSpaceSignature;

// Determines the color space the given profile is for. The Filename can either be a full path or a file name only,
// in which case the file is loaded from the system's color directory.
// A third way of specification is to preceed the name with a pound symbol (#). This tells the method the color profile
// should be loaded from the application's resource (RC_DATA section).

var
  Profile: TCMSHPROFILE;
  Stream: TResourceStream;

begin
  if Length(FileName) > 1 then
  begin
    Stream := nil;
    if FileName[1] = '#' then
    begin
      Stream := TResourceStream.Create(HInstance, Copy(FileName, 2, Length(FileName)), RT_RCDATA);
      Profile := cmsOpenProfileFromMem(Stream.Memory, Stream.Size);
    end
    else
    begin
      FileName := GetFullColorProfileFileName(FileName);
      Profile := cmsOpenProfileFromFile(PChar(FileName), 'r');
    end;
    Result := cmsGetColorSpace(Profile);
    cmsCloseProfile(Profile);
    Stream.Free;
  end
  else
    Result := 0;
end;

//----------------- TColorProfile --------------------------------------------------------------------------------------

destructor TColorProfile.Destroy;

begin
  if Assigned(FProfile) then
    cmsCloseProfile(FProfile);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorProfile.GetChannels: Cardinal;

begin
  Result := FormatToChannels[ColorFormat]
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorProfile.GetColorFormat: TColorFormat;

const
  SignatureToFormat: array[TColorFormat] of Cardinal = (
    0,              // cfUnknown
    icSigGrayData,  // cfGray
    icSigRgbData,   // cfRGB
    icSigCmyData,   // cfCMY
    icSigCmykData,  // cfCMYK
    icSigLabData,   // cfCIELab
    icSigYCbCrData, // cfYCbCr
    icSigXYZData,   // cfXYZ
    icSigLuvKData,  // cfYUVK
    icSigLuvData,   // cfYUV
    icSigHsvData,   // cfHSV
    icSigHlsData,   // cfHLS
    icSigYxyData    // cfYxy
  );

var
  Colorspace: Cardinal;

begin
  ColorSpace := cmsGetColorSpace(Profile);
  for Result := Low(TColorFormat) to High(TColorFormat) do
    if SignatureToFormat[Result] = ColorSpace then
      Exit;

  Result := cfUnknown;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorProfile.GetProfile: TCMSHPROFILE;

begin
  if FProfile = nil then
    FProfile := CreateProfile;
  Result := FProfile;
end;

//----------------- TExternalProfile -----------------------------------------------------------------------------------

constructor TExternalProfile.Create(ICMFile: string);

begin
  // Note: the ICC file name may contain a pound (#) sign as the first letter, which indicates not to look for a
  // real file but to load the profile from the resource.
  FProfileFile := ICMFile;
end;

//----------------------------------------------------------------------------------------------------------------------

function TExternalProfile.CreateProfile: TCMSHPROFILE;

var
  FileName: string;

begin
  Result := nil;
  if Length(FProfileFile) > 1 then
  begin
    if FProfileFile[1] = '#' then
    begin
      // Profile is in the resource. Open from memory.
      FProfileStream := TResourceStream.Create(HInstance, Copy(FProfileFile, 2, Length(FProfileFile)), RT_RCDATA);
      Result := cmsOpenProfileFromMem(FProfileStream.Memory, FProfileStream.Size);
    end
    else
    begin
      FileName := '';
      // If a normal file name is given then either open it from the given location or from the system's
      // color directory, if the former fails.
      FileName := GetFullColorProfileFileName(FProfileFile);
      if FileName <> '' then
        Result := cmsOpenProfileFromFile(PChar(FileName), 'r');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TExternalProfile.Destroy;

begin
  inherited;

  FProfileStream.Free;
end;

//----------------- TCMYKProfile ---------------------------------------------------------------------------------------

constructor TCMYKProfile.Create(ICMFile: string; UseBlack: Boolean);

begin
  inherited Create(ICMFile);

  // UseBlack determines whether we use CMY or CMYK here.
  FUseBlack := UseBlack;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCMYKProfile.GetChannels: Cardinal;

begin
  if FUseBlack then
    Result := 4
  else
    Result := 3;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCMYKProfile.GetColorFormat: TColorFormat;

begin
  if FUseBlack then
    Result := cfCMYK
  else
    Result := cfCMY;
end;

//----------------- TLabProfile ----------------------------------------------------------------------------------------

constructor TLabProfile.Create(WhitePoint: TWhitePoint);

begin
  cmsXYZ2xyY(FWhitePoint, WhitePoints[WhitePoint]);
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TLabProfile.Create(WhitePoint: TCIEXYZ);

begin
  cmsXYZ2xyY(FWhitePoint, WhitePoint);
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TLabProfile.Create(WhitePoint: TCIExyY);

begin
  FWhitePoint := WhitePoint;
end;

//----------------------------------------------------------------------------------------------------------------------

function TLabProfile.CreateProfile: TCMSHPROFILE;

begin
  Result := cmsCreateLabProfile(@FWhitePoint);
end;

//----------------------------------------------------------------------------------------------------------------------

function TLabProfile.GetChannels: Cardinal;

begin
  Result := 3;
end;

//----------------------------------------------------------------------------------------------------------------------

function TLabProfile.GetColorFormat: TColorFormat;

begin
  Result := cfCIELab;
end;

//----------------- TsRGBProfile ---------------------------------------------------------------------------------------

function TsRGBProfile.CreateProfile: TCMSHPROFILE;

begin
  Result := cmsCreate_sRGBProfile;
end;

//----------------------------------------------------------------------------------------------------------------------

function TsRGBProfile.GetColorFormat: TColorFormat;

begin
  Result := cfRGB;
end;

//----------------- TRGBProfile ----------------------------------------------------------------------------------------

constructor TRGBProfile.Create(WhitePoint: TWhitePoint; Primaries: TPrimaries; Gamma: Double; UseAlpha: Boolean);

begin
  cmsXYZ2xyY(FWhitePoint, WhitePoints[WhitePoint]);
  FPrimaries := Primaries;
  FGamma := Gamma;
  FUseAlpha := UseAlpha;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TRGBProfile.Create(WhitePoint: TCIEXYZ; Primaries: TPrimaries; Gamma: Double; UseAlpha: Boolean);

begin
  cmsXYZ2xyY(FWhitePoint, WhitePoint);
  FPrimaries := Primaries;
  FGamma := Gamma;
  FUseAlpha := UseAlpha;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TRGBProfile.Create(WhitePoint: TCIExyY; Primaries: TPrimaries; Gamma: Double; UseAlpha: Boolean);

begin
  FWhitePoint := WhitePoint;
  FPrimaries := Primaries;
  FGamma := Gamma;
  FUseAlpha := UseAlpha;
end;

//----------------------------------------------------------------------------------------------------------------------

function TRGBProfile.CreateProfile: TCMSHPROFILE;

var
  GammaTables: TGammaTableArray;
  Primaries: TCIExyYTriple;

begin
  GammaTables[0] := cmsBuildGamma(256, FGamma);
  GammaTables[1] := cmsBuildGamma(256, FGamma);
  GammaTables[2] := cmsBuildGamma(256, FGamma);

  cmsXYZ2xyY(Primaries.red, PrimariesValues[FPrimaries].Red);
  cmsXYZ2xyY(Primaries.green, PrimariesValues[FPrimaries].Green);
  cmsXYZ2xyY(Primaries.blue, PrimariesValues[FPrimaries].Blue);

  Result := cmsCreateRGBProfile(@FWhitePoint, @Primaries, GammaTables);

  cmsFreeGamma(GammaTables[0]);
  cmsFreeGamma(GammaTables[1]);
  cmsFreeGamma(GammaTables[2]);
end;

//----------------------------------------------------------------------------------------------------------------------

function TRGBProfile.GetChannels: Cardinal;

begin
  Result := 3;
end;

//----------------------------------------------------------------------------------------------------------------------

function TRGBProfile.GetColorFormat: TColorFormat;

begin
  Result := cfRGB;
end;

//----------------- TGrayProfile ---------------------------------------------------------------------------------------

constructor TGrayProfile.Create(WhitePoint: TWhitePoint; Gamma: Double);

begin
  cmsXYZ2xyY(FWhitePoint, WhitePoints[WhitePoint]);
  FGamma := Gamma;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TGrayProfile.Create(WhitePoint: TCIEXYZ; Gamma: Double);

begin
  cmsXYZ2xyY(FWhitePoint, WhitePoint);
  FGamma := Gamma;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TGrayProfile.Create(WhitePoint: TCIExyY; Gamma: Double);

begin
  FWhitePoint := WhitePoint;
  FGamma := Gamma;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrayProfile.CreateProfile: TCMSHPROFILE;

var
  GammaTable: PGammaTable;

begin
  GammaTable := cmsBuildGamma(256, FGamma);
  Result := cmsCreateGrayProfile(@FWhitePoint, GammaTable);
  cmsFreeGamma(GammaTable);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrayProfile.GetChannels: Cardinal;

begin
  Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrayProfile.GetColorFormat: TColorFormat;

begin
  Result := cfGray;
end;

//----------------- TXYZProfile ----------------------------------------------------------------------------------------

function TXYZProfile.CreateProfile: TCMSHPROFILE;

begin
  Result := cmsCreateXYZProfile();
end;

//----------------------------------------------------------------------------------------------------------------------

function TXYZProfile.GetChannels: Cardinal;

begin
  Result := 3;
end;

//----------------------------------------------------------------------------------------------------------------------

function TXYZProfile.GetColorFormat: TColorFormat;

begin
  Result := cfXYZ;
end;

//----------------- TDisplayProfile ------------------------------------------------------------------------------------

function TDisplayProfile.CreateProfile: TCMSHPROFILE;

var
  DC: HDC;
  Buffer: array[0..MAX_PATH] of Char;
  BufferSize: Cardinal;

begin
  // Get the screen DC and read its current ICM profile. If there is none then create an sRGB profile instead.
  DC := GetDC(0);
  try
    BufferSize := SizeOf(Buffer);
    // Retrieve the default ICM profile for the display.
    if GetICMProfile(DC, BufferSize, Buffer) then
      Result := cmsOpenProfileFromFile(Buffer, 'r')
    else
      Result := cmsCreate_sRGBProfile;
  finally
    ReleaseDC(0, DC);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDisplayProfile.GetChannels: Cardinal;

const
  FormatToChannels: array[TColorFormat] of Cardinal = (
    0, // cfUnknown
    1, // cfGray
    3, // cfRGB
    3, // cfCMY
    4, // cfCMYK
    3, // cfCIELab
    3, // cfYCbCr
    3, // cfXYZ
    4, // cfYUVK
    3, // cfYUV
    3, // cfHSV
    3, // cfHLS
    3  // cfYxy
  );
  
begin
  Result := FormatToChannels[ColorFormat]
end;

//----------------------------------------------------------------------------------------------------------------------

function TDisplayProfile.GetColorFormat: TColorFormat;

const
  SignatureToFormat: array[TColorFormat] of Cardinal = (
    0,              // cfUnknown
    icSigGrayData,  // cfGray
    icSigRgbData,   // cfRGB
    icSigCmyData,   // cfCMY
    icSigCmykData,  // cfCMYK
    icSigLabData,   // cfCIELab
    icSigYCbCrData, // cfYCbCr
    icSigXYZData,   // cfXYZ
    icSigLuvKData,  // cfYUVK
    icSigLuvData,   // cfYUV
    icSigHsvData,   // cfHSV
    icSigHlsData,   // cfHLS
    icSigYxyData    // cfYxy
  );

var
  Colorspace: Cardinal;

begin
  ColorSpace := cmsGetColorSpace(Profile);
  for Result := Low(TColorFormat) to High(TColorFormat) do
    if SignatureToFormat[Result] = ColorSpace then
      Exit;

  Result := cfUnknown;
end;

//----------------- TColorConverter ------------------------------------------------------------------------------------

constructor TColorConverter.Create(SourceProfile: TColorProfile; const SourceOptions: TProfileOptions;
  TargetProfile: TColorProfile; const TargetOptions: TProfileOptions; Intent: TRenderingIntent);

begin
  FSourceProfile := SourceProfile;
  FSourceOptions := SourceOptions;
  FTargetProfile := TargetProfile;
  FTargetOptions := TargetOptions;
  FRenderingIntent := Intent;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorConverter.CreateTransformation;

  //--------------- local functions --------------------------------------------

  function MakeConvertFormat(ColorFormat: TColorFormat; Options: TProfileOptions): Cardinal;

  // Creates a format descriptor based on the given parameters.

  const
    ColorFormatToPixelType: array[TColorFormat] of Cardinal = (
      PT_ANY, PT_GRAY, PT_RGB, PT_CMY, PT_CMYK, PT_Lab, PT_YCbCr, PT_XYZ, PT_YUV, PT_YUVK, PT_HSV, PT_HLS, PT_Yxy    // cfYxy
    );
    
  begin
    Result := COLORSPACE_SH(ColorFormatToPixelType[ColorFormat]) or
      SWAPFIRST_SH(Options.SwapFirst) or
      FLAVOR_SH(Options.MinIsWhite) or
      PLANAR_SH(Options.Planar) or
      ENDIAN16_SH(Options.BigEndian) or
      DOSWAP_SH(Options.SwapComponents) or
      EXTRA_SH(Options.ExtraSamples) or
      CHANNELS_SH(Options.Channels) or
      BYTES_SH(Options.BytesPerSample);
  end;

  //--------------- end local functions ----------------------------------------

var
  InputFormat,
  OutputFormat: Cardinal;
  
begin
  InputFormat := MakeConvertFormat(FSourceProfile.ColorFormat, FSourceOptions);
  OutputFormat := MakeConvertFormat(FTargetProfile.ColorFormat, FTargetOptions);
  FTransformation := cmsCreateTransform(FSourceProfile.Profile, InputFormat, FTargetProfile.Profile, OutputFormat,
    Ord(FRenderingIntent), 0);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TColorConverter.Destroy;

begin
  if Assigned(FTransformation) then
    cmsDeleteTransform(FTransformation);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorConverter.Convert(const Source, Target: Pointer; Count: Integer);

begin
  if FTransformation = nil then
    CreateTransformation;
    
  if Assigned(FTransformation) then
    cmsDoTransform(FTransformation, Source, Target, Count)
  else
    raise Exception.Create('ColorConverter: Could not create transformation.');
end;

//----------------------------------------------------------------------------------------------------------------------

end.
