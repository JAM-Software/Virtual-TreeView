unit ColorPickerTypes;

//----------------------------------------------------------------------------------------------------------------------
// ColorPickerTypes contains commonly used types and constants for the Color Picker component.
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
// The original code is ColorPickerTypes.pas, released 1. April 2003.
//
// The initial developer of the original code is:
//   Dipl. Ing. Mike Lischke, Delphi Gems software solutions (public@delphi-gems.com, www.delphi-gems.com).
//
// Portions created by Delphi Gems are
//   (C) 1999-2003 Delphi Gems. All Rights Reserved.
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
  Windows, Graphics, Classes, Controls, Contnrs, Messages,
  ColorTools, ColorPicker, ColorManagement, ColorTypes;

type
  // A color picker widget represents an element of a color picker control, which actually allows to
  // select a color. Such a widget is responsible for all input handling and painting.
  // The base implementation takes also care for color managment and base input handling.
  TColorPickerWidget = class(TBaseColorPickerWidget)
  private
    // Color management support.
    FDisplayGamma: Single;             // Gamma value for RGB display values.
    FWhitePoint: TWhitePoint;          // White point of the source color scheme.
    FCMYKProfileFile: string;          // For CMYK we need an external color profile for correct results.
    FNativeProfile: TColorProfile;
    FDisplayProfile: TColorProfile;    // ICM profiles used to convert native colors to a display color and vice versa.
    FNativeToDisplayConverter: TColorConverter; // Class for the actual color conversion from native to display format.
    FDisplayToNativeConverter: TColorConverter; // Class for the actual color conversion from display to native format.
    FXYZProfile: TColorProfile;        // Profile to convert the native color format to a neutral format (for exchange
                                       // with other color pickers etc.).
    FNativeToXYZConverter: TColorConverter; // Class for the actual color conversion from native to CIE XYZ format.
    FXYZToNativeConverter: TColorConverter; // Class for the actual color conversion from CIE XYZ to native format.
    FColorFormat: TColorFormat;        // The format (e.g. RGB, CMYK, CIE L*a*b* etc.) of the current color set managed
                                       // by this widget instance.
    FDisplayColorFormat: TColorFormat; // Copy of the color format in the display profile (for quick checks).

    // Color selection.
    FHotColorInfo: TColorInfo;         // The color format always corresponds to the widget's color format and is not
                                       // used otherwise.
    FSelectedColorInfo: TColorInfo;

    procedure SetCMYKProfileFile(const Value: string);
    procedure SetDisplayGamma(const Value: Single);
    procedure SetSelectedColor(const Value: TColorComponents);
    procedure SetSelectedColorName(Value: WideString);
    procedure SetWhitePoint(const Value: TWhitePoint);

    procedure WNColorChange(var Data: TWidgetNotification); message WN_COLORCHANGE;
    procedure WNHotColorChange(var Data: TWidgetNotification); message WN_HOTCOLORCHANGE;
  protected
    procedure ClearNativeProfile; virtual;
    function ColorFromPoint(P: TPoint): TColorInfo; virtual;
    procedure ColorProfileChanged; virtual;
    procedure CreateNativeProfile; virtual;
    procedure ColorChanged(NewColor: TXYZ); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    function DisplayConversionAvailable: Boolean; virtual;
    procedure DisplayGammaChanged; virtual;
    function FindColor(const Name: WideString; var Components: TColorComponents): Boolean; virtual;
    function GetNativeColorFormat: TColorFormat; virtual;
    procedure HotColorChanged(NewColor: TXYZ); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; P: TPoint); override;
    procedure MouseMove(const P: TPoint); override;
    procedure ReadSelectedColor(Reader: TReader);
    procedure RecreateNativeProfile; virtual;
    procedure WriteSelectedColor(Writer: TWriter);
    function XYZConversionAvailable: Boolean; virtual;

    property CMYKProfileFile: string read FCMYKProfileFile write SetCMYKProfileFile;
    property DisplayGamma: Single read FDisplayGamma write SetDisplayGamma;
    property WhitePoint: TWhitePoint read FWhitePoint write SetWhitePoint default wpD65;
  public
    constructor Create(Owner: TControl); override;
    destructor Destroy; override;

    procedure Clear; virtual;
    function ConvertFromCIEXYZ(const Color: TXYZ): TColorComponents; virtual;
    function ConvertToCIEXYZ(const Color: TColorComponents): TXYZ; virtual;
    function ConvertToColorRef(const Color: TColorComponents): COLORREF;
    function ConvertToDisplayColor(const Color: TColorComponents): TColorComponents; virtual;
    procedure SysColorChanged; virtual;

    property DisplayProfile: TColorProfile read FDisplayProfile;
    property HotColorInfo: TColorInfo read FHotColorInfo;
    property NativeProfile: TColorProfile read FNativeProfile;
    property SelectedColorInfo: TColorInfo read FSelectedColorInfo;
    property XYZProfile: TColorProfile read FXYZProfile;
  published
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property ColorFormat: TColorFormat read FColorFormat;
    property Cursor;
    property DropShadow;
    property Margin;
    property SelectedColor: TColorComponents read FSelectedColorInfo.Color write SetSelectedColor;
    property SelectedColorName: WideString read FSelectedColorInfo.Name write SetSelectedColorName;
    property Title;
  end;

  // Determines how to display a list of named colors.
  TCpDiscreteLayout = (
    cdlCaptionList,          // The same like cdlList but with captions for each color.
    cdlList,                 // A listbox like list of color rectangles.
    cdlSquaresCollection,    // n x m squares.
    cdlHorizontalStripes,    // A stripe for each color. Orientation is from left to right.
    cdlVerticalStripes       // A stripe for each color. Orientation is from top to bottom.
  );

  TCpDiscreteBorderStyle = (
    cdbNone,                 // No border around the color cells.
    cdbSingle,               // A single black frame.
    cdbSeparate              // A separated frame in the color of the cell.
  );

  PPreviewColors = ^TPreviewColors;
  TPreviewColors = array[0..1000] of COLORREF;

  TColorInfoArray = array of TColorInfo;

  // TDiscreteColorPickerWidget is the base class for all widgets, which provide color selection from grids, lists
  // and other collections of colors with individual names.
  TDiscreteColorPickerWidget = class(TColorPickerWidget)
  private
    FColors: TColorInfoArray;
    FLayout: TCpDiscreteLayout;
    FBoxSize: Integer;
    FSpacing: Integer;
    FListOffset: Integer;              // Vertical position for list layouts.
    FBorderStyle: TCpDiscreteBorderStyle;
    FHotIndex,
    FSelectedIndex: Integer;
    procedure SetBorderStyle(const Value: TCpDiscreteBorderStyle);
    procedure SetBoxSize(const Value: Integer);
    procedure SetLayout(const Value: TCpDiscreteLayout);
    procedure SetSpacing(const Value: Integer);
  protected
    procedure AddDefaultColors; virtual; abstract;
    function ColorFromPoint(P: TPoint): TColorInfo; override;
    procedure ColorProfileChanged; override;
    procedure DisplayGammaChanged; override;
    procedure DrawColorBox(const Canvas: TCanvas; R: TRect; Color: COLORREF); virtual;
    function FindColor(const AName: WideString; var Components: TColorComponents): Boolean; override;
    function FindColorIndex(const Components: TColorComponents): Integer; virtual;
    procedure RecalculateDisplayColors;
  public
    constructor Create(Owner: TControl); override;
    destructor Destroy; override;

    function AddColor(ColorName: WideString; WindowsColor: COLORREF): Integer; overload;
    function AddColor(ColorName: WideString; NativeFormat: TColorFormat;
      const Values: TColorComponents): Integer; overload;
    procedure Clear; override;
    class procedure GetPreviewColors(var Colors: PPreviewColors; var Count: Integer); virtual;
    procedure Paint(const Canvas: TCanvas); override;
    class procedure RenderPreview(Canvas: TCanvas; const R: TRect); override;

    property Colors: TColorInfoArray read FColors;
  published
    property BorderStyle: TCpDiscreteBorderStyle read FBorderStyle write SetBorderStyle default cdbSingle;
    property BoxSize: Integer read FBoxSize write SetBoxSize default 12;
    property ColorFormat;
    property CMYKProfileFile;
    property DisplayGamma;
    property Height;
    property Layout: TCpDiscreteLayout read FLayout write SetLayout default cdlSquaresCollection;
    property Options;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Width;
    property WhitePoint;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  SysUtils, Math, ColorPickerStrings, GraphicTools;

const
  DefaultPreviewColors: array[0..49] of COLORREF = (
    $05D1E7, $05CDEA, $13BCED, $23B6EE, $2EA2F3, $2D85F4, $3574EF,
    $365DE8, $364DE0, $3C4DE2, $4956E4, $4B49D9, $4D4FC8, $4F4C99,
    $3F48DB, $4846D5, $5D40C8, $7536D2, $8834A6, $834985, $844575,
    $5E5555, $9C6948, $665E5A, $915D58, $955E57, $966E45, $897D52,
    $A18738, $9B9337, $A0A25A, $8E9826, $7AAC2C, $5C9E39, $62933D,
    $4C9140, $3AA569, $46765E, $3B8788, $3E9071, $43A256, $3DB560,
    $30C389, $24BE82, $289EE1, $2D94DB, $3D5B73, $3D5979, $426091,
    $000000
  );

//----------------- Helper functions -----------------------------------------------------------------------------------

function KeysToShiftState(Keys: Word): TShiftState;

begin
  Result := [];
  if Keys and MK_SHIFT <> 0 then
    Include(Result, ssShift);
  if Keys and MK_CONTROL <> 0 then
    Include(Result, ssCtrl);
  if Keys and MK_LBUTTON <> 0 then
    Include(Result, ssLeft);
  if Keys and MK_RBUTTON <> 0 then
    Include(Result, ssRight);
  if Keys and MK_MBUTTON <> 0 then
    Include(Result, ssMiddle);
  if GetKeyState(VK_MENU) < 0 then
    Include(Result, ssAlt);
end;


//----------------- TColorPickerWidget ---------------------------------------------------------------------------------

constructor TColorPickerWidget.Create(Owner: TControl);

begin
  inherited;

  FDisplayGamma := 1;
  FCMYKProfileFile := '';
  FWhitePoint := wpD65;
  FColorFormat := GetNativeColorFormat;
  FSelectedColorInfo.Format := FColorFormat;
  FHotColorInfo.Format := FColorFormat;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TColorPickerWidget.Destroy;

begin
  Clear;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.SetCMYKProfileFile(const Value: string);

var
  NeedRecalculation: Boolean;

begin
  if FCMYKProfileFile <> Value then
  begin
    NeedRecalculation := FColorFormat in [cfCMYK, cfCMY];

    FCMYKProfileFile := Value;
    if NeedRecalculation then
    begin
      ClearNativeProfile;
      ColorProfileChanged;
      Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.SetDisplayGamma(const Value: Single);

begin
  if FDisplayGamma <> Value then
  begin
    FDisplayGamma := Value;
    if not (csReading in ColorPicker.ComponentState) then
      DisplayGammaChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.SetSelectedColor(const Value: TColorComponents);

begin
  // The actual color scheme/format used in the given components is determined by the ColorFormat property.
  FSelectedColorInfo.Color := Copy(Value, 0, Length(Value) * SizeOf(Double));
  ColorPicker.Notify(Self, wnrSelectedColor);
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.SetSelectedColorName(Value: WideString);

var
  Color: Integer;
  I: Integer;
  Components: TColorComponents;

begin
  Components := nil;
  Trim(Value);

  // First try if the string contains a single (hexadecimal) integer, which indicates a color
  // given by its components. Since this only leaves room for at most 255 per component you obviously cannot
  // use this string format for color formats using floating point values.
  // See if the new name is one of the predefined colors if it is not a simple integer.
  if TryStrToInt(Value, Color) or IdentToColor(Value, Color) then
  begin
    // Adjust length of color components array.
    if (Color and $FF000000) <> 0 then
      SetLength(FSelectedColorInfo.Color, 4)
    else
      SetLength(FSelectedColorInfo.Color, FNativeProfile.Channels);
    for I := 0 to High(FSelectedColorInfo.Color) do
    begin
      FSelectedColorInfo.Color[I] := Color and $FF;
      Color := Color shr 8;
    end;
    FSelectedColorInfo.Name := Value;
  end
  else
  begin
    // Looks like the value is a special name. Search via descendants to find a corresponding color.
    if FindColor(Value, Components) then
    begin
      FSelectedColorInfo.Name := Value;
      FSelectedColorInfo.Color := Components;
      Invalidate;
    end
    else
      ColorPickerError(SWrongColorName, [Value]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.SetWhitePoint(const Value: TWhitePoint);

begin
  if FWhitePoint <> Value then
  begin
    FWhitePoint := Value;

    if FColorFormat in [cfGray, cfCIELab, cfRGB] then
    begin
      // Recreate color converters to account for the new white point if we currently use a
      // color format, which requires the source white point.
      RecreateNativeProfile;
      Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.WNColorChange(var Data: TWidgetNotification);

var
  Info: PColorInfo;

begin
  Info := Data.Data;
  ColorChanged(Info.XYZ);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.WNHotColorChange(var Data: TWidgetNotification);

var
  Info: PColorInfo;

begin
  Info := Data.Data;
  HotColorChanged(Info.XYZ);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.ClearNativeProfile;

// Deletes the source profile and all related structures.

begin
  FreeAndNil(FNativeToDisplayConverter);
  FreeAndNil(FDisplayToNativeConverter);
  FreeAndNil(FNativeProfile);
  FreeAndNil(FNativeToXYZConverter);
  FreeAndNil(FXYZToNativeConverter);

  Finalize(FHotColorInfo);
  FillChar(FHotColorInfo, SizeOf(FHotColorInfo), 0);
  Finalize(FSelectedColorInfo);
  FillChar(FSelectedColorInfo, SizeOf(FSelectedColorInfo), 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.ColorProfileChanged;

begin
  Finalize(FHotColorInfo);
  FillChar(FHotColorInfo, SizeOf(FHotColorInfo), 0);
  Finalize(FSelectedColorInfo);
  FillChar(FSelectedColorInfo, SizeOf(FSelectedColorInfo), 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorPickerWidget.ColorFromPoint(P: TPoint): TColorInfo;

begin
  Finalize(Result);
  FillChar(Result, SizeOf(Result), 0);
  Result.Format := FColorFormat;
  Result.XYZ := MakeXYZ(-1, -1, -1);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.CreateNativeProfile;

// If the current color format is supported by the widget then
// a new source profile is created here, which is used to create the color transformation.

begin
  case FColorFormat of
    cfUnknown:
      ; // Do not raise an exception or tell otherwise. The unknown color format might be what is intended. 
    cfGray:
      // Let the gamma value be 1. We consider display gamma later in the process of converting to
      // a display color.
      FNativeProfile := TGrayProfile.Create(FWhitePoint, 1);
    cfRGB:
      // Photoshop uses the sRGB color space for its RGB color swatches, so we go for the same space too.
      FNativeProfile := TsRGBProfile.Create; // TRGBProfile.Create(FWhitePoint);
    cfCMY:
      if FCMYKProfileFile <> '' then
        FNativeProfile := TCMYKProfile.Create(FCMYKProfileFile, False);
    cfCMYK:
      if FCMYKProfileFile <> '' then
        FNativeProfile := TCMYKProfile.Create(FCMYKProfileFile, True);
    cfCIELab:
      FNativeProfile := TLabProfile.Create(FWhitePoint);
    cfXYZ:
      FNativeProfile := TXYZProfile.Create;
  else
    ColorPickerError(SUnsupportedColorFormat, [ColorFormatStrings[FColorFormat]]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.ColorChanged(NewColor: TXYZ);

begin
  if woTrackSelectedColor in Options then
  begin
    // Convert to local color format.
    FSelectedColorinfo.Color := ConvertFromCIEXYZ(NewColor);
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.DefineProperties(Filer: TFiler);

// Need to add support for special properties (dynamic arrays).

begin
  inherited;

  Filer.DefineProperty('StreamedSelectedColor', ReadSelectedColor, WriteSelectedColor, True);
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorPickerWidget.DisplayConversionAvailable: Boolean;

// Checks if all necessary structures are allocated, which are needed to convert from native to display color format
// and back.
// Returns True if so otherwise False.

var
  NativeOptions,
  DisplayOptions: TProfileOptions;

begin
  Result := FColorFormat <> cfUnknown;
  if Result and ((FNativeToDisplayConverter = nil) or (FDisplayToNativeConverter = nil)) then
  begin
    if FNativeProfile = nil then
      CreateNativeProfile;

    if Assigned(FNativeProfile) then
    begin
      FillChar(NativeOptions, SizeOf(NativeOptions), 0);
      with NativeOptions do
      begin
        BytesPerSample := 0; // double values!
        Channels := FNativeProfile.Channels;
      end;

      if FDisplayProfile = nil then
      begin
        FDisplayProfile := TDisplayProfile.Create;
        FDisplayColorFormat := FDisplayProfile.ColorFormat;
      end;

      if Assigned(FDisplayProfile) then
      begin
        FillChar(DisplayOptions, SizeOf(DisplayOptions), 0);
        with DisplayOptions do
        begin
          BytesPerSample := 0; // double values!
          Channels := FDisplayProfile.Channels;
        end;

        if (FNativeToDisplayConverter = nil) and Assigned(FNativeProfile) and Assigned(FDisplayProfile) then
          FNativeToDisplayConverter := TColorConverter.Create(FNativeProfile, NativeOptions, FDisplayProfile,
            DisplayOptions, riPerceptual);
        if (FDisplayToNativeConverter = nil) and Assigned(FNativeProfile) and Assigned(FDisplayProfile) then
          FDisplayToNativeConverter := TColorConverter.Create(FDisplayProfile, DisplayOptions, FNativeProfile,
            NativeOptions, riPerceptual);
      end;
    end;
  end;

  Result := Assigned(FNativeToDisplayConverter) and Assigned(FDisplayToNativeConverter);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.DisplayGammaChanged;

begin
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorPickerWidget.FindColor(const Name: WideString; var Components: TColorComponents): Boolean;

begin
  // By default colors are not named so we cannot search for them. However discrete colors do have a name as they
  // represent a fixed range of predefined colors.
  Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorPickerWidget.GetNativeColorFormat: TColorFormat;

begin
  Result := cfUnknown;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.MouseDown(Button: TMouseButton; Shift: TShiftState; P: TPoint);

begin
  FSelectedColorInfo := ColorFromPoint(P);
  ColorPicker.Notify(Self, wnrSelectedColor);
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.MouseMove(const P: TPoint);

var
  NewColor: TColorInfo;

begin
  NewColor := ColorFromPoint(P);
  if not SameColor(NewColor.Color, FHotColorInfo.Color) then
  begin
    FHotColorInfo := NewColor;
    ColorPicker.Notify(Self, wnrHotColor);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.ReadSelectedColor(Reader: TReader);

begin
  SelectedColor := ColorPicker.StringToColor(Reader.ReadString);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.RecreateNativeProfile;

// Frees the current native profile and all related structures and immediately creates a new set while preserving
// the current hot and selected colors.

var
  LastHotColor,
  LastSelectedColor: TColorInfo;

begin
  LastHotColor := FHotColorInfo;
  LastSelectedColor := FSelectedColorInfo;

  ClearNativeProfile;
  if DisplayConversionAvailable then
  begin
    FHotColorInfo := LastHotColor;
    FSelectedColorInfo := LastSelectedColor;
    // The actual value has not changed but if another color picker tracks these colors than they might have changed
    // in the conversion between the color formats.
    ColorPicker.Notify(Self, wnrHotColor);
    ColorPicker.Notify(Self, wnrSelectedColor);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.WriteSelectedColor(Writer: TWriter);

begin
  Writer.WriteString(ColorPicker.ColorToString(SelectedColor));
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorPickerWidget.XYZConversionAvailable: Boolean;

// Checks if all necessary structures are allocated, which are needed to convert from CIE XYZ to display color format
// and back.
// Returns True if so otherwise False.

var
  NativeOptions,
  XYZOptions: TProfileOptions;

begin
  Result := FColorFormat <> cfUnknown;
  if Result and ((FNativeToXYZConverter = nil) or (FXYZToNativeConverter = nil)) then
  begin
    if FNativeProfile = nil then
      CreateNativeProfile;

    if Assigned(FNativeProfile) then
    begin
      FillChar(NativeOptions, SizeOf(NativeOptions), 0);
      with NativeOptions do
      begin
        BytesPerSample := 0; // double values!
        Channels := FNativeProfile.Channels;
      end;

      if FXYZProfile = nil then
        FXYZProfile := TXYZProfile.Create;

      if Assigned(FXYZProfile) then
      begin
        FillChar(XYZOptions, SizeOf(XYZOptions), 0);
        with XYZOptions do
        begin
          BytesPerSample := 0; // double values!
          Channels := FXYZProfile.Channels;
        end;

        if Assigned(FNativeProfile) and Assigned(FXYZProfile) then
        begin
          if FNativeToXYZConverter = nil then
            FNativeToXYZConverter := TColorConverter.Create(FNativeProfile, NativeOptions, FXYzProfile, XYZOptions,
              riPerceptual);
          if FXYZToNativeConverter = nil then
            FXYZToNativeConverter := TColorConverter.Create(FXYzProfile, XYZOptions, FNativeProfile, NativeOptions,
              riPerceptual);
        end;
      end;
    end;
  end;

  Result := Assigned(FNativeToXYZConverter) and Assigned(FXYZToNativeConverter);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.Clear;

begin
  ClearNativeProfile;
  FreeAndNil(FDisplayProfile);
  FColorFormat := GetNativeColorFormat;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorPickerWidget.ConvertFromCIEXYZ(const Color: TXYZ): TColorComponents;

// Converts the given CIE XYZ color into color components in the current native color format.

begin
  if XYZConversionAvailable and (Color.X >= 0) and (Color.Y >= 0) and (Color.Z >= 0) then
  begin
    SetLength(Result, FNativeProfile.Channels);
    FXYZToNativeConverter.Convert(@Color, Result, 1);
  end
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorPickerWidget.ConvertToCIEXYZ(const Color: TColorComponents): TXYZ;

// Converts the given color components in the current native color format to a CIE XYZ color.

begin
  if Assigned(Color) and XYZConversionAvailable then
    FNativeToXYZConverter.Convert(Color, @Result, 1)
  else
    Result := MakeXYZ(-1, -1, -1);
end;

//----------------------------------------------------------------------------------------------------------------------

type
  TControlCast = class(TControl)
  end;

function TColorPickerWidget.ConvertToColorRef(const Color: TColorComponents): COLORREF;

// Converts the given color to a COLORREF value using the current gamma value.
// The given color is interpreted as RGB color and the current color format must be cfRGB to make the conversion happen.
// Otherwise you have to do a manual step to convert the color from the special color scheme to RGB.

begin
  if (FDisplayColorFormat = cfRGB) and Assigned(Color) then
    Result := MakeColorRef(MakeRGB(Color), FDisplayGamma)
  else
    Result := ColorToRGB(TControlCast(ColorPicker).Color);
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorPickerWidget.ConvertToDisplayColor(const Color: TColorComponents): TColorComponents;

// Converts the given color, which is considered as being in the current color format to a display color.
// The display color format depends on the current display settings in the system but is usually RGB or sRGB.
// Use ConvertToColorRef to create a gamma corrected COLORREF value from the display color returned here.

begin
  if Assigned(Color) and DisplayConversionAvailable then
  begin
    SetLength(Result, FDisplayProfile.Channels);
    FNativeToDisplayConverter.Convert(Color, Result, 1);
  end
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.HotColorChanged(NewColor: TXYZ);

// Called by the owning color picker after a hot color in another widget has changed.
// Native is nil if no color was found there.

begin   
  if woTrackHotColor in Options then
  begin
    // Convert to local color format.
    if (NewColor.X >= 0) and (NewColor.Y >= 0) and (NewColor.Z >= 0) then
      FHotColorInfo.Color := ConvertFromCIEXYZ(NewColor)
    else
      FHotColorInfo.Color := nil;
    if woHotColorAsSelected in Options then
      FSelectedColorInfo.Color := Copy(FHotColorInfo.Color, 0, Length(FHotColorInfo.Color) * SizeOf(Double));
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerWidget.SysColorChanged;

// Called when the user changed the color scheme of its operation system.
// descendants may optionally override this method.

begin
end;

//----------------- TDiscreteColorPickerWidget -----------------------------------------------------------------------------

constructor TDiscreteColorPickerWidget.Create(Owner: TControl);

begin
  inherited;

  FColors := nil;
  FBoxSize := 12;
  FSpacing := 4;
  FLayout := cdlSquaresCollection;
  FBorderStyle := cdbSingle;
  FHotIndex := -1;
  FSelectedIndex := -1;
  
  AddDefaultColors;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TDiscreteColorPickerWidget.Destroy;

begin
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDiscreteColorPickerWidget.SetBorderStyle(const Value: TCpDiscreteBorderStyle);

begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    StructureChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDiscreteColorPickerWidget.SetBoxSize(const Value: Integer);

begin
  if FBoxSize <> Value then
  begin
    FBoxSize := Value;
    StructureChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDiscreteColorPickerWidget.SetLayout(const Value: TCpDiscreteLayout);

begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    StructureChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDiscreteColorPickerWidget.SetSpacing(const Value: Integer);

begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    StructureChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDiscreteColorPickerWidget.ColorFromPoint(P: TPoint): TColorInfo;

// Computes the color at the given position (given in widget coordinates).
// Returns an empty set if no color box is hit.

var
  ColumnCount: Integer;
  RowCount: Integer;
  R: TRect;
  CellSize: Integer;
  FontHeight: Integer;

  HitPos: TPoint;
  HitIndex: Integer;

begin
  Result := inherited ColorFromPoint(P);

  if Result.Color = nil then
  begin
    HitIndex := -1;
    if Length(FColors) > 0 then
    begin
      CellSize := FBoxSize + FSpacing;
      R := Rect(0, 0, Width - 2 * Margin, Height - 2 * Margin);
      Dec(P.X, Margin);
      Dec(P.Y, Margin);
      case FLayout of
        cdlSquaresCollection:
          begin
            ColumnCount := (R.Right - R.Left + FSpacing) div (FBoxSize + FSpacing);
            RowCount := Ceil((R.Bottom - R.Top) / CellSize);
            if PtInRect(Rect(0, 0, ColumnCount * CellSize, RowCount * CellSize), P) then
            begin
              // HitPos is in columns x rows.
              HitPos.X := P.X div CellSize;
              HitPos.Y := P.Y div CellSize;
              if (P.Y mod CellSize <= FBoxSize) and (P.X mod CellSize <= FBoxSize) then
                HitIndex := HitPos.Y * ColumnCount + HitPos.X;
            end;
          end;
        cdlList: // A listbox like list of color rectangles.
          begin
            HitPos.Y := P.Y div CellSize;
            if P.Y mod CellSize <= FBoxSize then
              HitIndex := HitPos.Y;
          end;
        cdlCaptionList: // The same like cldList but with captions for each rectangle.
          begin
            // Since we have to output the description for each color we have to take the font height into account.
            // Add 2 pixels above and below the text.
            FontHeight := Abs(TControlCast(ColorPicker).Font.Height);
            Inc(CellSize, FontHeight + 4);
            HitPos.Y := P.Y div CellSize;
            if P.Y mod CellSize <= FBoxSize then
              HitIndex := HitPos.Y;
          end;
        cdlVerticalStripes:
          if PtInRect(R, P) then
            HitIndex := Round(P.X * Length(FColors) / (R.Right - R.Left));
        cdlHorizontalStripes:
          if PtInRect(R, P) then
            HitIndex := Round(P.Y * Length(FColors) / (R.Bottom - R.Top));
      end;
    end;
    if (HitIndex > -1) and (HitIndex < Length(FColors)) then
      Result := FColors[HitIndex];
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDiscreteColorPickerWidget.ColorProfileChanged;

// One of the color profiles (e.g. for CMYK, Lab etc.) has changed. We need to recalculate all display colors.

begin
  inherited;

  RecalculateDisplayColors;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDiscreteColorPickerWidget.DisplayGammaChanged;

// The display gamma has changed. We need to recalculate all display colors.

begin
  inherited;

  RecalculateDisplayColors;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDiscreteColorPickerWidget.DrawColorBox(const Canvas: TCanvas; R: TRect; Color: COLORREF);

begin
  Canvas.Brush.Color := Color;
  case FBorderStyle of
    cdbNone:
      Canvas.FillRect(R);
    cdbSingle:
      begin
        Canvas.Pen.Color := clBlack;
        Canvas.Rectangle(R);
      end;
    cdbSeparate:
      begin
        Canvas.FrameRect(R);
        InflateRect(R, -2, -2);
        Canvas.FillRect(R);
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDiscreteColorPickerWidget.FindColor(const AName: WideString; var Components: TColorComponents): Boolean;

// Searchs the list of registered colors for the given name and returns the associated color components if found.
// Result is True in this case or False if the color name is not in the list.

var
  I: Integer;

begin
  Result := inherited FindColor(AName, Components);
  if not Result then
  begin
    for I := 0 to High(FColors) do
      with FColors[I] do
        if AName = Name then
        begin
          Components := Copy(Color, 0, Length(Color) * SizeOf(Double));
          Result := True;
          Break;
        end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDiscreteColorPickerWidget.FindColorIndex(const Components: TColorComponents): Integer;

// Searches the list of registered colors for the given components and returns the index of the color if found.
// Otherwise -1 is returned. The passed color components must be in the current color format of the
// color picker widget and must have at least as many entries as need for this format (e.g. 3 for RGB).
// Note: the comparison is done with a small delta so the components does not need to fit 100%.

const
  // This is the minimum difference between two color components to consider them as being different.
  ColorDelta = 0.001;

var
  I, J: Integer;
  IsDifferent: Boolean;

begin
  Result := -1;
  if Cardinal(Length(Components)) = FNativeProfile.Channels then
  begin
    for I := 0 to High(FColors) do
      with FColors[I] do
      begin
        IsDifferent := False;
        for J := 0 to High(Components) do
          if Abs(Color[J] - Components[J]) > ColorDelta then
          begin
            IsDifferent := True;
            Break;
          end;

        if not IsDifferent then
        begin
          Result := I;
          Break;
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDiscreteColorPickerWidget.RecalculateDisplayColors;

var
  I: Integer;

begin
  inherited;

  for I := 0 to High(FColors) do
    FColors[I].DisplayColor := ConvertToDisplayColor(FColors[I].Color);
end;

//----------------------------------------------------------------------------------------------------------------------

function TDiscreteColorPickerWidget.AddColor(ColorName: WideString; WindowsColor: COLORREF): Integer;

// Color addition using a 32 bit Windows color.

var
  Components: TColorComponents;

begin
  SetLength(Components, 3);
  Components[0] := GetRValue(WindowsColor) / 255;
  Components[1] := GetGValue(WindowsColor) / 255;
  Components[2] := GetBValue(WindowsColor) / 255;
  Result := AddColor(ColorName, cfRGB, Components);
end;

//----------------------------------------------------------------------------------------------------------------------

function TDiscreteColorPickerWidget.AddColor(ColorName: WideString; NativeFormat: TColorFormat;
  const Values: TColorComponents): Integer;

// General color addition method.

begin
  Assert((FColorFormat = cfUnknown) or (FColorFormat = NativeFormat), 'DiscreteColorPickerWidget: All colors must be ' +
    'in the same color format.');
  FColorFormat := NativeFormat;

  SetLength(FColors, Length(FColors) + 1);
  with FColors[High(FColors)] do
  begin
    Name := ColorName;
    Format := NativeFormat;
    Color := Values;
    DisplayColor := ConvertToDisplayColor(Color);
    XYZ := ConvertToCIEXYZ(Color);
  end;
  Result := High(FColors);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDiscreteColorPickerWidget.Clear;

begin
  inherited;

  FColors := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TDiscreteColorPickerWidget.GetPreviewColors(var Colors: PPreviewColors; var Count: Integer);

begin
  Colors := @DefaultPreviewColors;
  Count := 50;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDiscreteColorPickerWidget.Paint(const Canvas: TCanvas);

// Paints the entire color set to Canvas.

var
  Row, Column: Integer;
  RowCount,
  ColumnCount: Integer;
  CurrentIndex: Integer;
  BoxRect: TRect;
  R: TRect;
  CellSize: Integer;
  FontHeight: Integer;
  S: WideString;
  Multiplier: Double;

begin
  if Length(Colors) > 0 then
  begin
    CellSize := FBoxSize + FSpacing;
    R := Rect(0, 0, Width, Height);
    InflateRect(R, -Margin, -Margin);
    case FLayout of
      cdlSquaresCollection:
        begin
          ColumnCount := (R.Right - R.Left + FSpacing) div (FBoxSize + FSpacing);
          RowCount := Ceil((R.Bottom - R.Top) / CellSize);

          CurrentIndex := 0;
          for Row := 0 to RowCount - 1 do
          begin
            if (R.Top + Row * CellSize + BoxSize > ColorPicker.ClientHeight - Margin) then
              Exit;
            for Column := 0 to ColumnCount - 1 do
            begin
              if CurrentIndex = Length(Colors) then
                Exit;
              BoxRect.TopLeft := Point(R.Left + Column * CellSize, R.Top + Row * CellSize);
              BoxRect.Bottom := BoxRect.Top + BoxSize;
              BoxRect.Right := BoxRect.Left + BoxSize;
              DrawColorBox(Canvas, BoxRect, ConvertToColorRef(FColors[CurrentIndex].DisplayColor));
              Inc(CurrentIndex);
            end;
          end;
        end;
      cdlList: // A listbox like list of color rectangles.
        begin
          RowCount := Ceil((R.Bottom - R.Top) / CellSize);
          R.Bottom := R.Top + FBoxSize;
          CurrentIndex := FListOffset;
          for Row := 0 to RowCount - 1 do
          begin
            DrawColorBox(Canvas, R, ConvertToColorRef(FColors[CurrentIndex].DisplayColor));
            Inc(CurrentIndex);
            OffsetRect(R, 0, CellSize);
          end;
        end;
      cdlCaptionList: // The same like cldList but with captions for each rectangle.
        begin
          // Since we have to output the description for each color we have to take the font height into account.
          // Add 2 pixels above and below the text.
          FontHeight := Abs(Canvas.Font.Height);
          Inc(CellSize, FontHeight + 4);
          RowCount := Ceil((R.Bottom - R.Top) / CellSize);
          R.Bottom := R.Top + FBoxSize;
          CurrentIndex := FListOffset;
          for Row := 0 to RowCount - 1 do
          begin
            DrawColorBox(Canvas, R, ConvertToColorRef(FColors[CurrentIndex].DisplayColor));
            S := FColors[CurrentIndex].Name;
            SetBkMode(Canvas.Handle, TRANSPARENT);
            ExtTextOutW(Canvas.Handle, R.Left, R.Bottom, 0, nil, PWideChar(S), Length(S), nil);
            OffsetRect(R, 0, CellSize);
            Inc(CurrentIndex);
          end;
        end;
      cdlVerticalStripes: // Each color is represented by a stripe.
        begin
          // Compute a transformation factor to map the available pixels in the widget to a color.
          Multiplier := Length(FColors) / (R.Right - R.Left);
          for Column := R.Left to R.Right do
          begin
            CurrentIndex := Round((Column - R.Left) * Multiplier);
            if CurrentIndex > High(FColors) then
              Break;
            Canvas.Pen.Color := ConvertToColorRef(FColors[CurrentIndex].DisplayColor);
            Canvas.MoveTo(Column, R.Top);
            Canvas.LineTo(Column, R.Bottom);
          end;
        end;
      cdlHorizontalStripes: // Each color is represented by a stripe.
        begin
          // Compute a transformation factor to map the available pixels in the widget to a color.
          Multiplier := Length(FColors) / (R.Bottom - R.Top);
          for Row := R.Top to R.Bottom do
          begin
            CurrentIndex := Round((Row - R.Top) * Multiplier);
            if CurrentIndex > High(FColors) then
              Break;
            Canvas.Pen.Color := ConvertToColorRef(FColors[CurrentIndex].DisplayColor);
            Canvas.MoveTo(R.Left, Row);
            Canvas.LineTo(R.Right, Row);
          end;
        end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TDiscreteColorPickerWidget.RenderPreview(Canvas: TCanvas; const R: TRect);

// Paints a small part of the entire color set to Canvas. This is mainly used at design time to see how such a color
// picker widget would look like. If not overidden then the same color grid is drawn for all descendants.

const
  BoxSize = 10; // Size of one color box in pixels.
  Spacing = 2; // Space between two color boxes.
  CellSize = BoxSize + Spacing;

var
  Row, Column: Integer;
  RowCount,
  ColumnCount: Integer;
  CurrentIndex: Integer;
  BoxRect: TRect;
  Colors: PPreviewColors;
  ColorCount: Integer;

begin
  ColumnCount := (R.Right - R.Left + Spacing) div (BoxSize + Spacing);
  RowCount := Floor((R.Bottom - R.Top) / CellSize);

  CurrentIndex := 0;
  GetPreviewColors(Colors, ColorCount);
  for Row := 0 to RowCount - 1 do
  begin
    for Column := 0 to ColumnCount - 1 do
    begin
      if CurrentIndex = ColorCount then
        Exit;
      BoxRect.TopLeft := Point(R.Left + Column * CellSize, R.Top + Row * CellSize);
      BoxRect.Bottom := BoxRect.Top + BoxSize;
      BoxRect.Right := BoxRect.Left + BoxSize;

      Canvas.Brush.Color := Colors[CurrentIndex];
      Canvas.Pen.Color := clBlack;
      Canvas.Rectangle(BoxRect);
      Inc(CurrentIndex);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
