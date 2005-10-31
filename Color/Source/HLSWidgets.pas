unit HLSWidgets;

//----------------------------------------------------------------------------------------------------------------------
// HLSWidges contains color picker widgets for the color picker control, which use the HLS/HSV/HSB model.
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
// The original code is HLSWidgets.pas, released 1. April 2003.
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
  ColorPickerTypes, ColorTools, ColorTypes;

type
  // Generic HLS widget used to implement actual widgets in various forms.
  // Note: Usually the lumincance value is not adjusted in these classes, but must be assigned by other means.
  //       The reason is that for the luminance the same gray ramp is used as in other widgets or in a standalone manner
  //       so it makes perfect sense to have it separate and so universal.
  THLSColorPickerWidget = class(TColorPickerWidget)
  private
    FRGB2XYZMatrix,
    FXYZ2RGBMatrix: TColorMatrix;
    FCurrentLuminance: Double;
    procedure SetCurrentLuminance(const Value: Double);
  public
    constructor Create(Owner: TControl); override;
    destructor Destroy; override;

    function ConvertFromCIEXYZ(const Color: TXYZ): TColorComponents; override;
    function ConvertToCIEXYZ(const Color: TColorComponents): TXYZ; override;
    function ConvertToDisplayColor(const Color: TColorComponents): TColorComponents; override;
    function GetNativeColorFormat: TColorFormat; override;

    property CurrentLuminance: Double read FCurrentLuminance write SetCurrentLuminance;
  published
    property DisplayGamma;
    property Height;
    property Options;
    property Width;
    property WhitePoint;
  end;

  // This HLS widget displays the well known color circle, which represents the hue and saturation of a color.
  THLSCircleWidget = class(THLSColorPickerWidget)
  protected
    function ColorFromPoint(P: TPoint): TColorInfo; override;
  public
    class function GetDescription: string; override;
    class function GetSummary: string; override;
    procedure Paint(const Canvas: TCanvas); override;
    class procedure RenderPreview(Canvas: TCanvas; const R: TRect); override;
  end;

  {
  TCombEntry = record
    Position: TPoint;
    Color: THLS;
    DisplayColor: COLORREF;
  end;

  TCombArray = array of TCombEntry;
  TCombLevels = 1..100;

  // An HSL widget, which uses a comb style to visualize colors.
  THLSCombWidget = class(THLSColorPickerWidget)
  private
    FLevels: TCombLevels;
    FColorCombs: TCombArray;
    FCombSize: Integer;
    procedure SetLevels(const Value: TCombLevels);
  protected
    class function CalculateCombLayout(var Combs: TCombArray; Levels: TCombLevels; const Bounds: TRect;
      Luminance: Single): Integer;
    function ColorFromPoint(P: TPoint): TColorInfo; override;
    function CombIndexFromPoint(P: TPoint): Integer;
    procedure DisplayGammaChanged; override;
    function PtInComb(const Comb: TCombEntry; P: TPoint; Scale: Integer): Boolean;
  public
    constructor Create(Owner: TControl); override;

    class function GetDescription: string; override;
    class function GetSummary: string; override;
    procedure Paint(const Canvas: TCanvas); override;
    class procedure RenderPreview(Canvas: TCanvas; const R: TRect); override;
    procedure StructureChanged; override;
  published
    property Levels: TCombLevels read FLevels write SetLevels default 7;
  end;
  }

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Math, SysUtils,
  ColorPickerStrings, GraphicTools;

const
  // Certain fixed colors for the comb HLS widget.
  DefCenterColor: TRGB = (R: 1; G: 1; B: 1); // White
  DefColors: array[0..5] of TRGB = (
    (R: 1; G: 0; B: 0), // Red
    (R: 1; G: 0; B: 1), // Magenta
    (R: 0; G: 0; B: 1), // Blue
    (R: 0; G: 1; B: 1), // Cyan
    (R: 0; G: 1; B: 0), // Green
    (R: 1; G: 1; B: 0)  // Yellow
    );
  DefCenter: TFloatPoint = (X: 0; Y: 0);

//----------------- THLSColorPickerWidget ------------------------------------------------------------------------------

constructor THLSColorPickerWidget.Create(Owner: TControl);

begin
  inherited;

  FRGB2XYZMatrix := MakeRGB2XYZColorMatrix(primHDTV, WhitePoint);
  FXYZ2RGBMatrix := MakeXYZ2RGBColorMatrix(primHDTV, WhitePoint);
  FCurrentLuminance := 0.5;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor THLSColorPickerWidget.Destroy;

begin
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THLSColorPickerWidget.SetCurrentLuminance(const Value: Double);

begin
  FCurrentLuminance := Value;
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

function THLSColorPickerWidget.ConvertFromCIEXYZ(const Color: TXYZ): TColorComponents;

// Since HLS is not a color space (but a color order model) we need special computations,
// as there is no CMS profile available.

var
  HLS: THLS;
  
begin
  if (Color.X >= 0) and (Color.Y >= 0) and (Color.Z >= 0) then
  begin
    HLS := RGBToHLS(XYZToRGB(Color, FXYZ2RGBMatrix));
    SetLength(Result, 3);
    Result[0] := HLS.H;
    Result[1] := HLS.L;
    Result[2] := HLS.S;
  end
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function THLSColorPickerWidget.ConvertToCIEXYZ(const Color: TColorComponents): TXYZ;

begin
  if Length(Color) >= 3 then
    Result := RGBToXYZ(HLSToRGB(MakeHLS(Color)), FRGB2XYZMatrix)
  else
    Result := MakeXYZ(-1, -1, -1);
end;

//----------------------------------------------------------------------------------------------------------------------

function THLSColorPickerWidget.ConvertToDisplayColor(const Color: TColorComponents): TColorComponents;

// It is assumed that the display uses RGB as color format. Other conversions are not available here, but
// usually also not needed.

var
  RGB: TRGB;

begin
  RGB := HLSToRGB(MakeHLS(Color));
  SetLength(Result, 3);
  Result[0] := RGB.R;
  Result[1] := RGB.G;
  Result[2] := RGB.B;
end;

//----------------------------------------------------------------------------------------------------------------------

function THLSColorPickerWidget.GetNativeColorFormat: TColorFormat;

begin
  Result := cfHLS;
end;

//----------------- THLSCircleWidget -----------------------------------------------------------------------------------

function THLSCircleWidget.ColorFromPoint(P: TPoint): TColorInfo;

const
  TwoPi = 2 * Pi;

var
  R: TRect;
  Center: TPoint;
  Radius: Integer;
  Distance: Double;

begin
  Result := inherited ColorFromPoint(P);

  // Compute color circle parameters just as in the paint method.
  R := Rect(0, 0, Width, Height);
  InflateRect(R, -Margin, -Margin);
  Center := Point((R.Right + R.Left) div 2, (R.Bottom + R.Top) div 2);
  Radius := Min(Width - 2 * Margin, Height - 2 * Margin) div 2;

  // Compute angle and distance of the given point relative to the circle center.
  // Return a valid color only if the point lies within the circle bounds.
  Distance := Sqrt(Sqr(P.X - Center.X) + Sqr(P.Y - Center.Y));
  if Distance <= Radius then
  begin
    // Hue
    SetLength(Result.Color, 3);
    if Distance = 0 then
      Result.Color[0] := 0 // Achromatic case.
    else
    begin
      Result.Color[0] := Arctan2(Center.Y - P.Y, P.X - Center.X);
      // Convert from +Pi..-Pi to 0..1.
      if Result.Color[0] < 0 then
        Result.Color[0] := TwoPi + Result.Color[0];
      Result.Color[0] := Result.Color[0] / TwoPi;
    end;
    // Luminance
    Result.Color[1] := FCurrentLuminance;
    // Saturation
    Result.Color[2] := Distance / Radius;

    // Fill other fields in the color info structure.
    Result.Format := cfHLS;
    Result.DisplayColor := ConvertToDisplayColor(Result.Color);
    Result.XYZ := ConvertToCIEXYZ(Result.Color);
    Result.Name := Format('Hue: %d°; Luminance: %.2f; Saturation: %.2f', [Round(360 * Result.Color[0]), Result.Color[1],
      Result.Color[2]]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class function THLSCircleWidget.GetDescription: string;

begin
  Result := SHLSCircleWidgetDescription;
end;

//----------------------------------------------------------------------------------------------------------------------

class function THLSCircleWidget.GetSummary: string;

begin
  Result := SHLSCircleWidgetSummary;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THLSCircleWidget.Paint(const Canvas: TCanvas);

var
  R: TRect;
  Center: TPoint;
  Radius: Integer;

begin
  R := Rect(0, 0, Width, Height);
  InflateRect(R, -Margin, -Margin);
  Center := Point((R.Left + R.Right) div 2, (R.Top + R.Bottom) div 2);
  Radius := Min(Width - 2 * Margin, Height - 2 * Margin) div 2;

  DrawColorCircle(Canvas.Handle, Center, MakeRGB(1, 1, 1), Radius, DisplayGamma);
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure THLSCircleWidget.RenderPreview(Canvas: TCanvas; const R: TRect);

var
  Width, Height: Integer;
  Center: TPoint;
  Radius: Integer;

begin
  Width := R.Right - R.Left;
  Height := R.Bottom - R.Top;
  Center := Point((R.Right + R.Left) div 2, (R.Bottom + R.Top) div 2);
  Radius := Min(Width, Height) div 2;

  DrawColorCircle(Canvas.Handle, Center, MakeRGB(1, 1, 1), Radius);
end;

//----------------- THLSCombWidget --------------------------------------------------------------------------------

{
constructor THLSCombWidget.Create(Owner: TControl);

begin
  inherited;

  FLevels := 7;
  FCombSize := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THLSCombWidget.SetLevels(const Value: TCombLevels);

begin
  if FLevels <> Value then
  begin
    FLevels := Value;
    FColorCombs := nil;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class function THLSCombWidget.CalculateCombLayout(var Combs: TCombArray; Levels: TCombLevels; const Bounds: TRect;
  Luminance: Single): Integer;

// Fills the comb array with positions and colors of the combs,
// Note: computation here is done using RGB, but this is only because we know what HLS value would be at a certain
//       position and it would be a waste of time to do frequent conversions HLS -> RGB.
// The function returns the size of one comb, which is to be used for painting, when there should be no room between two
// adjacent combs.

var
  CurrentIndex: Cardinal;
  CurrentColor: THLS;
  CurrentPos: TFloatPoint;
  CombCount: Cardinal;
  I, J,
  Level: Cardinal;
  Scale: Double;

  // Triangle vars.
  CenterColor: TRGB;
  Pos1, Pos2: TFloatPoint;
  dPos1, dPos2: TFloatPoint;
  Color1, Color2: TRGB;
  dColor1, dColor2: TRGB;
  dPos: TFloatPoint;
  dColor: TRGB;

  Center: TPoint;
  Radius: Integer;
  LevelFactor: Single;

begin
  Result := 0;
  if Levels > 1 then
  begin
    // To draw perfectly aligned combs we split the final comb into six triangles (sextants)
    // and calculate each separately. The center comb is stored as first entry in the array
    // and will not be considered twice (as with the other shared combs too).
    CurrentIndex := 0;
    CurrentColor := MakeRGB(1, 1, 1);

    // The number of combs depends on the level deepness.
    CombCount := 1 + 6 * (((Levels - 1) * Levels) div 2);
    SetLength(Combs, CombCount);

    // Store the center values. Positions are relative to (0, 0). Final positions will be computed when drawing.
    Combs[CurrentIndex].Position := Point(0, 0);
    Combs[CurrentIndex].DisplayColor := MakeColorRef(CurrentColor, Gamma);
    Inc(CurrentIndex);

    Center := Point((Bounds.Left + Bounds.Right) div 2, (Bounds.Top + Bounds.Bottom) div 2);
    CenterColor := DefCenterColor;
    Radius := Min(Bounds.Right - Bounds.Left, Bounds.Bottom - Bounds.Top) div 2;
    LevelFactor := 1 / (Levels - 1);
    // Compute the maximum size of one comb.
    Result := Trunc(Radius * LevelFactor);
    // Recalculate the radius to fit the combs nicely.
    Radius := Trunc(Result / LevelFactor);

    // Now go for each sextant. The generic corners have been calculated already at creation
    // time for a comb with diameter 1.
    //              ------
    //             /\  1 /\
    //            /  \  /  \
    //           / 2  \/  0 \
    //           -----------
    //           \ 3  /\  5 /
    //            \  /  \  /
    //             \/  4 \/
    //              ------

    for I := 0 to 5 do
    begin
      // initialize triangle corner values
      //
      //                center (always at 0,0)
      //                 /\
      //     dPos1      /  \    dPos2
      //     dColor1   /    \   dColor2
      //              / dPos \
      //             /--------\ (span)
      //            /  dColor  \
      //           /____________\
      //    comb corner 1     comb corner 2
      //
      // Pos1, Pos2, Color1, Color2 are running terms for both sides of the triangle
      // incremented by dPos1/2 and dColor1/2.
      // dPos and dColor are used to interpolate a span between the values just mentioned.
      //
      // The small combs are actually oriented with corner 0 at top (i.e. mirrored at y = x,
      // compared with the values in FCombCorners), we can achieve that by simply exchanging
      // X and Y values.

      Scale := 2 * Radius * cos(Pi / 6);
      Pos1.X := CombCorners[I].Y * Scale;
      Pos1.Y := CombCorners[I].X * Scale;
      Color1 := DefColors[I];
      if I = 5 then
      begin
        Pos2.X := CombCorners[0].Y * Scale;
        Pos2.Y := CombCorners[0].X * Scale;
        Color2 := DefColors[0];
      end
      else
      begin
        Pos2.X := CombCorners[I + 1].Y * Scale;
        Pos2.Y := CombCorners[I + 1].X * Scale;
        Color2 := DefColors[I + 1];
      end;
      dPos1.X := LevelFactor * Pos1.X;
      dPos1.Y := LevelFactor * Pos1.Y;
      dPos2.X := LevelFactor * Pos2.X;
      dPos2.Y := LevelFactor * Pos2.Y;

      dColor1.R := LevelFactor * (Color1.R - CenterColor.R);
      dColor1.G := LevelFactor * (Color1.G - CenterColor.G);
      dColor1.B := LevelFactor * (Color1.B - CenterColor.B);

      dColor2.R := LevelFactor * (Color2.R - CenterColor.R);
      dColor2.G := LevelFactor * (Color2.G - CenterColor.G);
      dColor2.B := LevelFactor * (Color2.B - CenterColor.B);

      Pos1 := DefCenter;
      Pos2 := DefCenter;
      Color1 := CenterColor;
      Color2 := CenterColor;

      // Now that we have finished the initialization for this step we'll go
      // through a loop for each level to calculate the spans.
      // We can ignore level 0 (as this is the center we already have determined), as well
      // as the last step of each span (as this is the start value in the next triangle and will
      // be calculated there). We have, though, to take them into the calculation of the running terms.
      for Level := 0 to Levels - 1 do
      begin
        if Level > 0 then
        begin
          // Initialize span values.
          dPos.X := (Pos2.X - Pos1.X) / Level;
          dPos.Y := (Pos2.Y - Pos1.Y) / Level;
          dColor.R := (Color2.R - Color1.R) / Level;
          dColor.G := (Color2.G - Color1.G) / Level;
          dColor.B := (Color2.B - Color1.B) / Level;
          CurrentPos := Pos1;
          CurrentColor := Color1;

          for J := 0 to Level - 1 do
          begin
            // Store current values in the array.
            Combs[CurrentIndex].Position.X := Round(CurrentPos.X);
            Combs[CurrentIndex].Position.Y := Round(CurrentPos.Y);
            CurrentPos.X := Combs[CurrentIndex].Position.X;
            CurrentPos.Y := Combs[CurrentIndex].Position.Y;
            Combs[CurrentIndex].Color := MakeColorRef(CurrentColor, Gamma);
            Inc(CurrentIndex);

            // Advance in span.
            CurrentPos.X := CurrentPos.X + dPos.X;
            CurrentPos.Y := CurrentPos.Y + dPos.Y;

            CurrentColor.R := CurrentColor.R + dColor.R;
            CurrentColor.G := CurrentColor.G + dColor.G;
            CurrentColor.B := CurrentColor.B + dColor.B;
          end;
        end;
        // Advance running terms,
        Pos1.X := Pos1.X + dPos1.X;
        Pos1.Y := Pos1.Y + dPos1.Y;
        Pos2.X := Pos2.X + dPos2.X;
        Pos2.Y := Pos2.Y + dPos2.Y;

        Color1.R := Color1.R + dColor1.R;
        Color1.G := Color1.G + dColor1.G;
        Color1.B := Color1.B + dColor1.B;

        Color2.R := Color2.R + dColor2.R;
        Color2.G := Color2.G + dColor2.G;
        Color2.B := Color2.B + dColor2.B;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function THLSCombWidget.ColorFromPoint(P: TPoint): TColorInfo;

var
  Index: Integer;
  
begin
  // Let ancestor preset the result structure.
  Result := inherited ColorFromPoint(P);
  Index := CombIndexFromPoint(P);
  if Index > -1 then
    with FColorCombs[Index] do
    begin
      Result.Color
      Result.DisplayColor
      Result.XYZ
      Result.Name
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function THLSCombWidget.CombIndexFromPoint(P: TPoint): Integer;

// Returns the index of the comb whose bounds contain the given point.
// If no comb was found then -1 is returned.

var
  I: Integer;

begin
  Result := -1;
  for I := 0 to High(FColorCombs) do
  begin
    if PtInComb(FColorCombs[I], P, FCombSize) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THLSCombWidget.DisplayGammaChanged;

begin
  inherited;

  FColorCombs := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function THLSCombWidget.PtInComb(const Comb: TCombEntry; P: TPoint; Scale: Integer): Boolean;

// Simplyfied "PointInPolygon" test, We know a comb is nearly a circle.

begin
  Result := (Sqr(Comb.Position.X - P.X) + Sqr(Comb.Position.Y - P.Y)) <= (Scale * Scale);
end;

//----------------------------------------------------------------------------------------------------------------------

class function THLSCombWidget.GetDescription: string;

begin
  Result := SHLSCombWidgetDescription;
end;

//----------------------------------------------------------------------------------------------------------------------

class function THLSCombWidget.GetSummary: string;

begin
  Result := SHLSCombWidgetSummary;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THLSCombWidget.Paint(const Canvas: TCanvas);

var
  R: TRect;
  I: Integer;
  Offset: TPoint;

begin
  if FColorCombs = nil then
  begin
    R := Rect(0, 0, Width, Height);
    InflateRect(R, -Margin, -Margin);
    FCombSize := CalculateCombLayout(FColorCombs, FLevels, R, CurrentLuminance, DisplayGamma);
  end;

  Canvas.Pen.Style := psClear;
  Offset.X := Width div 2;
  Offset.Y := Height div 2;
  for I := 0 to High(FColorCombs) do
  begin
    Canvas.Brush.Color := FColorCombs[I].Color;
    DrawComb(Canvas, Point(FColorCombs[I].Position.X + Offset.X, FColorCombs[I].Position.Y + Offset.Y), FCombSize);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure THLSCombWidget.RenderPreview(Canvas: TCanvas; const R: TRect);

const
  PreviewLevels = 4;

var
  Combs: TCombArray;
  I: Integer;
  Offset: TPoint;
  CombSize: Integer;

begin
  CombSize := CalculateCombLayout(Combs, PreviewLevels, R, 1, 1);
  Canvas.Pen.Style := psClear;
  Offset.X := (R.Left + R.Right) div 2;
  Offset.Y := (R.Top + R.Bottom) div 2;
  for I := 0 to High(Combs) do
  begin
    Canvas.Brush.Color := Combs[I].Color;
    DrawComb(Canvas, Point(Combs[I].Position.X + Offset.X, Combs[I].Position.Y + Offset.Y), CombSize);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THLSCombWidget.StructureChanged;

begin
  inherited;

  FColorCombs := nil;
end;

//----------------------------------------------------------------------------------------------------------------------
}

initialization
  RegisterColorPickerWidget(THLSCircleWidget);
  //RegisterColorPickerWidget(THLSCombWidget);
end.

