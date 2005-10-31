unit DefaultWidgets;

//----------------------------------------------------------------------------------------------------------------------
// DefaultWidges contains standard color picker widgets for the color picker control.
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
// The original code is DefaultWidgets.pas, released 1. April 2003.
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
  Windows, ColorPicker, Classes, Controls, Graphics,
  ColorPickerTypes, ColorTools;

type
  TOfficeCpWidget = class(TDiscreteColorPickerWidget)
  protected
    procedure AddDefaultColors; override;
  public
    class function GetDescription: string; override;
    class procedure GetPreviewColors(var Colors: PPreviewColors; var Count: Integer); override;
    class function GetSummary: string; override;
  end;

  TSystemColorsCpWidget = class(TDiscreteColorPickerWidget)
  protected
    procedure AddDefaultColors; override;
    function FindColor(const Name: WideString; var Components: TColorComponents): Boolean; override;
  public
    class function GetDescription: string; override;
    class function GetSummary: string; override;
    procedure SysColorChanged; override;
  end;

  // This widget includes the same colors as that for the system colors but adds the VCL colors too.
  TDelphiColorsCpWidget = class(TSystemColorsCpWidget)
  protected
    procedure AddDefaultColors; override;
  public
    class function GetDescription: string; override;
    class function GetSummary: string; override;
  end;

  // A color picker widget for the 31 recommended Windows XP icon colors.
  TWindowsXPIconColorsCpWidget = class(TDiscreteColorPickerWidget)
  protected
    procedure AddDefaultColors; override;
  public
    class function GetDescription: string; override;
    class procedure GetPreviewColors(var Colors: PPreviewColors; var Count: Integer); override;
    class function GetSummary: string; override;
  end;

  TColorCompareCpWidget = class(TColorPickerWidget)
  private
    FAlternativeColorInfo: TColorInfo;
    FBorderColor: TColor;
    function GetAlternativeColor: TColorComponents;
    procedure SetAlternativeColor(const Value: TColorComponents);
    procedure SetBorderColor(const Value: TColor);
  protected
    function ColorFromPoint(P: TPoint): TColorInfo; override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetNativeColorFormat: TColorFormat; override;
    procedure ReadAlternativeColor(Reader: TReader);
    procedure WriteAlternativeColor(Writer: TWriter);
  public
    constructor Create(Owner: TControl); override;

    class function GetDescription: string; override;
    class function GetSummary: string; override;
    procedure Paint(const Canvas: TCanvas); override;
    class procedure RenderPreview(Canvas: TCanvas; const R: TRect); override;

    property AlternativeColorInfo: TColorInfo read FAlternativeColorInfo;
  published
    property AlternativeColor: TColorComponents read GetAlternativeColor write SetAlternativeColor;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property DisplayGamma;
    property Height;
    property Options;
    property WhitePoint;
    property Width;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Types, Math, SysUtils, ColorPickerStrings, GraphicTools;

//----------------- TOfficeCpWidget ------------------------------------------------------------------------------------

const
  OfficeColors: array[0..39] of COLORREF = (
    $000000,
    $003399,
    $003333,
    $003300,
    $663300,
    $800000,
    $993333,
    $333333,
    $000080,
    $0066FF,
    $008080,
    $008000,
    $808000,
    $FF0000,
    $996666,
    $808080,
    $0000FF,
    $0099FF,
    $00CC99,
    $669933,
    $CCCC33,
    $FF6633,
    $800080,
    $969696,
    $FF00FF,
    $00CCFF,
    $00FFFF,
    $00FF00,
    $FFFF00,
    $FFCC00,
    $663399,
    $C0C0C0,
    $CC99FF,
    $99CCFF,
    $99FFFF,
    $CCFFCC,
    $FFFFCC,
    $FFCC99,
    $FF99CC,
    $FFFFFF
  );
  
  OfficeColorNames: array[0..39] of string = (
    'Black',
    'Brown',
    'Olive Green',
    'Dark Green',
    'Dark Teal',
    'Dark blue',
    'Indigo',
    'Gray-80%',
    'Dark Red',
    'Orange',
    'Dark Yellow',
    'Green',
    'Teal',
    'Blue',
    'Blue-Gray',
    'Gray-50%',
    'Red',
    'Light Orange',
    'Lime',
    'Sea Green',
    'Aqua',
    'Light Blue',
    'Violet',
    'Gray-40%',
    'Pink',
    'Gold',
    'Yellow',
    'Bright Green',
    'Turquoise',
    'Sky Blue',
    'Plum',
    'Gray-25%',
    'Rose',
    'Tan',
    'Light Yellow',
    'Light Green',
    'Light Turquoise',
    'Pale Blue',
    'Lavender',
    'White'
  );

procedure TOfficeCpWidget.AddDefaultColors;

var
  I: Integer;

begin
  for I := 0 to 39 do
    AddColor(OfficeColorNames[I], OfficeColors[I]);
end;

//----------------------------------------------------------------------------------------------------------------------

class function TOfficeCpWidget.GetDescription: string;

begin
  Result := SOfficeCpWidgetDescription;
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TOfficeCpWidget.GetPreviewColors(var Colors: PPreviewColors; var Count: Integer);

begin
  Colors := @OfficeColors;
  Count := 40;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TOfficeCpWidget.GetSummary: string;

begin
  Result := SOfficeCPWidgetSummary;
end;

//----------------- TSystemColorsCpWidget ------------------------------------------------------------------------------

procedure TSystemColorsCpWidget.AddDefaultColors;

begin
  AddColor('clScrollBar', ColorToRGB(clScrollBar));
  AddColor('clBackground', ColorToRGB(clBackground));
  AddColor('clActiveCaption', ColorToRGB(clActiveCaption));
  AddColor('clInactiveCaption', ColorToRGB(clInactiveCaption));
  AddColor('clMenu', ColorToRGB(clMenu));
  AddColor('clWindow', ColorToRGB(clWindow));
  AddColor('clWindowFrame', ColorToRGB(clWindowFrame));
  AddColor('clMenuText', ColorToRGB(clMenuText));
  AddColor('clWindowText', ColorToRGB(clWindowText));
  AddColor('clCaptionText', ColorToRGB(clCaptionText));
  AddColor('clActiveBorder', ColorToRGB(clActiveBorder));
  AddColor('clInactiveBorder', ColorToRGB(clInactiveBorder));
  AddColor('clAppWorkSpace', ColorToRGB(clAppWorkSpace));
  AddColor('clHighlight', ColorToRGB(clHighlight));
  AddColor('clHighlightText', ColorToRGB(clHighlightText));
  AddColor('clBtnFace', ColorToRGB(clBtnFace));
  AddColor('clBtnShadow', ColorToRGB(clBtnShadow));
  AddColor('clGrayText', ColorToRGB(clGrayText));
  AddColor('clBtnText', ColorToRGB(clBtnText));
  AddColor('clInactiveCaptionText', ColorToRGB(clInactiveCaptionText));
  AddColor('clBtnHighlight', ColorToRGB(clBtnHighlight));
  AddColor('cl3DDkShadow', ColorToRGB(cl3DDkShadow));
  AddColor('cl3DLight', ColorToRGB(cl3DLight));
  AddColor('clInfoText', ColorToRGB(clInfoText));
  AddColor('clInfoBk', ColorToRGB(clInfoBk));
  AddColor('clHotLight', ColorToRGB(clHotLight));
  AddColor('clGradientActiveCaption', ColorToRGB(clGradientActiveCaption));
  AddColor('clGradientInactiveCaption', ColorToRGB(clGradientInactiveCaption));
  AddColor('clMenuHighlight', ColorToRGB(clMenuHighlight));
  AddColor('clMenuBar', ColorToRGB(clMenuBar));
end;

//----------------------------------------------------------------------------------------------------------------------

function TSystemColorsCpWidget.FindColor(const Name: WideString; var Components: TColorComponents): Boolean;

var
  Color: Integer;

begin
  Result := IdentToColor(Name, Color);
  if Result then
  begin
    Color := ColorToRGB(Color);
    SetLength(Components, 3);
    Components[0] := GetRValue(Color);
    Components[1] := GetGValue(Color);
    Components[2] := GetBValue(Color);
  end
  else
    Result := inherited FindColor(Name, Components);
end;

//----------------------------------------------------------------------------------------------------------------------

class function TSystemColorsCpWidget.GetDescription: string;

begin
  Result := SSystemColorsCpWidgetDescription;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TSystemColorsCpWidget.GetSummary: string;

begin
  Result := SSystemColorsCpWidgetSummary;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSystemColorsCpWidget.SysColorChanged;

// Called when the user changed one of the system colors. This means we have to refill our list.

begin
  inherited;

  Clear;
  AddDefaultColors;
end;

//----------------- TDelphiColorsCpWidget ------------------------------------------------------------------------------

procedure TDelphiColorsCpWidget.AddDefaultColors;

begin
  AddColor('clBlack', ColorToRGB(clBlack));
  AddColor('clMaroon', ColorToRGB(clMaroon));
  AddColor('clGreen', ColorToRGB(clGreen));
  AddColor('clOlive', ColorToRGB(clOlive));
  AddColor('clNavy', ColorToRGB(clNavy));
  AddColor('clPurple', ColorToRGB(clPurple));
  AddColor('clTeal', ColorToRGB(clTeal));
  AddColor('clGray', ColorToRGB(clGray));
  AddColor('clSilver', ColorToRGB(clSilver));
  AddColor('clRed', ColorToRGB(clRed));
  AddColor('clLime', ColorToRGB(clLime));
  AddColor('clYellow', ColorToRGB(clYellow));
  AddColor('clBlue', ColorToRGB(clBlue));
  AddColor('clFuchsia', ColorToRGB(clFuchsia));
  AddColor('clAqua', ColorToRGB(clAqua));
  AddColor('clLtGray', ColorToRGB(clLtGray));
  AddColor('clDkGray', ColorToRGB(clDkGray));
  AddColor('clWhite', ColorToRGB(clWhite));
  AddColor('clMoneyGreen', ColorToRGB(clMoneyGreen));
  AddColor('clSkyBlue', ColorToRGB(clSkyBlue));
  AddColor('clCream', ColorToRGB(clCream));
  AddColor('clMedGray', ColorToRGB(clMedGray));
  AddColor('clNone', ColorToRGB(clNone));
  AddColor('clDefault', ColorToRGB(clDefault));

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TDelphiColorsCpWidget.GetDescription: string;

begin
  Result := SDelphiColorsCpWidgetDescription;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TDelphiColorsCpWidget.GetSummary: string;

begin
  Result := SDelphiColorsCpWidgetSummary;
end;

//----------------- TWindowsXPIconColors -------------------------------------------------------------------------------

const
  XPIconColors: array[0..30] of COLORREF = (
    (153 + 102 shl 8 +   0 shl 16),
    (204 + 153 shl 8 +   0 shl 16),
    (255 + 204 shl 8 +   0 shl 16),
    (255 + 255 shl 8 +   0 shl 16),
    (255 + 255 shl 8 + 153 shl 16),
    (255 + 219 shl 8 + 157 shl 16),
    (255 + 204 shl 8 + 102 shl 16),
    (255 + 153 shl 8 +  51 shl 16),
    (255 + 121 shl 8 +  75 shl 16),
    (255 +  51 shl 8 +   0 shl 16),
    (153 +   0 shl 8 +   0 shl 16),
    ( 51 +  51 shl 8 + 102 shl 16),
    (  0 +  51 shl 8 + 153 shl 16),
    (  0 + 102 shl 8 + 204 shl 16),
    (  0 + 131 shl 8 + 215 shl 16),
    (  0 + 153 shl 8 + 255 shl 16),
    ( 62 + 154 shl 8 + 222 shl 16),
    (153 + 204 shl 8 + 255 shl 16),
    (180 + 226 shl 8 + 255 shl 16),
    (222 + 255 shl 8 + 255 shl 16),
    (255 + 204 shl 8 + 255 shl 16),
    (204 + 204 shl 8 + 255 shl 16),
    (153 + 153 shl 8 + 255 shl 16),
    (102 + 102 shl 8 + 204 shl 16),
    (153 + 153 shl 8 + 204 shl 16),
    (102 + 102 shl 8 + 153 shl 16),
    (  0 + 102 shl 8 +   0 shl 16),
    (  0 + 153 shl 8 +   0 shl 16),
    (102 + 204 shl 8 +  51 shl 16),
    (153 + 255 shl 8 + 102 shl 16),
    (204 + 255 shl 8 + 204 shl 16)
  );

procedure TWindowsXPIconColorsCpWidget.AddDefaultColors;

var
  I: Integer;
  Color: COLORREF;
  
begin
  for I := 0 to 30 do
  begin
    Color := XPIconColors[I];
    AddColor(Format('R: %d G: %d B: %d', [GetRValue(Color), GetGValue(Color), GetBValue(Color)]), Color);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TWindowsXPIconColorsCpWidget.GetDescription: string;

begin
  Result := SWindowsXPIconColorsCpWidgetDescription;
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TWindowsXPIconColorsCpWidget.GetPreviewColors(var Colors: PPreviewColors; var Count: Integer);

begin
  Colors := @XPIconColors;
  Count := 31;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TWindowsXPIconColorsCpWidget.GetSummary: string;

begin
  Result := SWindowsXPIconColorsCpWidgetSummary;
end;

//----------------- TColorCompareCpWidget ------------------------------------------------------------------------------

constructor TColorCompareCpWidget.Create(Owner: TControl);

begin
  inherited;
  FBorderColor := clBlack;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorCompareCpWidget.GetAlternativeColor: TColorComponents;

begin
  if FAlternativeColorInfo.Color = nil then
    FAlternativeColorInfo := SelectedColorInfo;
  Result := FAlternativeColorInfo.Color;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorCompareCpWidget.SetAlternativeColor(const Value: TColorComponents);

begin
  FAlternativeColorInfo.Color := Value;
  FAlternativeColorInfo.DisplayColor := ConvertToDisplayColor(Value);
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorCompareCpWidget.SetBorderColor(const Value: TColor);

begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorCompareCpWidget.ColorFromPoint(P: TPoint): TColorInfo;

var
  PaintRect: TRect;
  MidPoint: Integer;

begin
  Result := inherited ColorFromPoint(P);

  PaintRect := Rect(0, 0, Width, Height);
  // Leave some border.
  InflateRect(PaintRect, -5, -5);
  // Convert to widget coordinates.
  OffsetRect(PaintRect, -PaintRect.Left, -PaintRect.Top);
  MidPoint := (PaintRect.Top + PaintRect.Bottom) div 2;

  if PtInRect(Rect(PaintRect.Left, PaintRect.Top, PaintRect.Right, MidPoint), P) then
    Result := SelectedColorinfo;
  if PtInRect(Rect(PaintRect.Left, MidPoint, PaintRect.Right, PaintRect.Bottom), P) then
    Result := FAlternativeColorInfo;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorCompareCpWidget.DefineProperties(Filer: TFiler);

// Need to add support for special properties (dynamic arrays).

begin
  inherited;

  Filer.DefineProperty('StreamedAlternativeColor', ReadAlternativeColor, WriteAlternativeColor, True);
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorCompareCpWidget.GetNativeColorFormat: TColorFormat;

begin
  Result := cfRGB;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorCompareCpWidget.ReadAlternativeColor(Reader: TReader);

begin
  AlternativeColor := TCustomColorPicker(ColorPicker).StringToColor(Reader.ReadString);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorCompareCpWidget.WriteAlternativeColor(Writer: TWriter);

begin
  Writer.WriteString(TCustomColorPicker(ColorPicker).ColorToString(AlternativeColor));
end;

//----------------------------------------------------------------------------------------------------------------------

class function TColorCompareCpWidget.GetDescription: string;

begin
  Result := SColorCompareCpWidgetDescription;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TColorCompareCpWidget.GetSummary: string;

begin
  Result := SColorCompareCpWidgetSummary;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorCompareCpWidget.Paint(const Canvas: TCanvas);

var
  R: TRect;
  MidPoint: Integer;

begin
  R := Rect(0, 0, Width, Height);
  InflateRect(R, -Margin, -Margin);
  MidPoint := Height div 2;

  with Canvas do
  begin
    if Assigned(SelectedColor) then
    begin
      Brush.Color := ConvertToColorRef(ConvertToDisplayColor(SelectedColor));
      FillRect(Rect(R.Left, R.Top, R.Right, MidPoint));
    end;
    if Assigned(AlternativeColor) then
    begin
      Brush.Color := ConvertToColorRef(ConvertToDisplayColor(AlternativeColor));
      FillRect(Rect(R.Left, MidPoint, R.Right, R.Bottom));
    end;
    
    // At last draw a small border around the two fields.
    Brush.Color := FBorderColor;
    FrameRect(R);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TColorCompareCpWidget.RenderPreview(Canvas: TCanvas; const R: TRect);

const
  PreviewColor1 = TColor($00BFFF);
  PreviewColor2 = TColor($FFA000);
  BorderColor = clBlack;

var
  PaintRect: TRect;
  MidPoint: Integer;

begin
  PaintRect := R;
  // Leave some border.
  InflateRect(PaintRect, -5, -5);
  MidPoint := (PaintRect.Top + PaintRect.Bottom) div 2;

  with Canvas do
  begin
    Brush.Color := PreviewColor1;
    FillRect(Rect(PaintRect.Left, PaintRect.Top, PaintRect.Right, MidPoint));
    Brush.Color := PreviewColor2;
    FillRect(Rect(PaintRect.Left, MidPoint, PaintRect.Right, PaintRect.Bottom));

    // Finally draw a small border around the two fields.
    Brush.Color := BorderColor;
    FrameRect(PaintRect);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  RegisterColorPickerWidget(TOfficeCpWidget);
  RegisterColorPickerWidget(TSystemColorsCpWidget);
  RegisterColorPickerWidget(TDelphiColorsCpWidget);
  RegisterColorPickerWidget(TWindowsXPIconColorsCpWidget);
  RegisterColorPickerWidget(TColorCompareCpWidget);
end.
