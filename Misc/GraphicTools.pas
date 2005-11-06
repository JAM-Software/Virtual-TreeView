unit GraphicTools;

//----------------------------------------------------------------------------------------------------------------------
//
// Description:
//   This unit contains helper routines related to computer graphics, like drop shadows, gradient fills etc.
//   Note: The code here requires OS support, which is only available with Windows 98 and up and Windows 2000 and up.
//         Hence including this unit will discontinue support of your application for Windows 95, Windows NT 4.0 and
//         lower.
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
// The original code is GraphicTools.pas, released 1. October 2003.
//
// The initial developer of the original code is:
//   Mike Lischke, Delphi Gems software solutions (support@delphi-gems.com, www.delphi-gems.com).
//
// Portions created by Delphi Gems are
//   (C) 1999-2003 Delphi Gems. All Rights Reserved.
//
// Credits:
//   - LordCRC for providing the gaussian blur code.
//----------------------------------------------------------------------------------------------------------------------
//
// August 2003:
//   Initial implementation.
//
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
  Windows, Graphics, Classes, ColorTypes, ColorTools;

const
  // Maximum size of the gaussian kernel used for blurring.
  MaxKernelSize = 100;

type
  TFloatPoint = record
    X, Y: Single;
  end;

  COLOR16 = Word;

  PTriVertex = ^TTriVertex;
  {$EXTERNALSYM _TRIVERTEX}
  _TRIVERTEX = packed record
    x: Integer;
    y: Integer;
    Red: COLOR16;
    Green: COLOR16;
    Blue: COLOR16;                   
    Alpha: COLOR16;
  end;
  TTriVertex = _TRIVERTEX;
  {$EXTERNALSYM TRIVERTEX}
  TRIVERTEX = _TRIVERTEX;

  TKernelSize = 1..MaxKernelSize;

  TKernel = record
    Size: TKernelSize;
    Weights: array[-MaxKernelSize..MaxKernelSize] of Single;
  end;

  // TDropShadow is a comfortable helper class to create and draw a bitmap with a drop shadow.
  // Note: this class only works with Windows 98/Me, Windows 2000 or better as it relies on the AlphaBlend API.
  TDropShadow = class(TPersistent)
  private
    FGrayRampPalette: HPALETTE;             // Gray scale palette for the 8 bit shadow.
    FColor: TColor;                         // The color to use for the shadow.
    FOffset: Integer;                       // Distance of the shadow from the original pixels in the given direction.
    FSize: Integer;                         // Size of the shadow (directly determines the Gauss kernel size).
    FAlpha: Single;                         // Overall translucency (strength) of the shadow (1 - fully opaque).
    FSourceAlpha: Single;                   // Translucency of the source image.
    FDirection: Integer;                    // Direction (in degrees) into which to throw the shadow (e.g. -45° for lower right)

    FOnChange: TNotifyEvent;
    procedure SetAlpha(const Value: Single);
    procedure SetColor(const Value: TColor);
    procedure SetDirection(const Value: Integer);
    procedure SetOffset(const Value: Integer);
    procedure SetSourceAlpha(const Value: Single);
    procedure SetSize(Value: Integer);
  protected
    procedure AverageMeanBlur(Bitmap: TBitmap);
    procedure DoChange; virtual;
    procedure SetGrayScalePalette(Bitmap: TBitmap);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Draw(Source: TBitmap; SourceRect: TRect; Target: HDC; TargetPos: TPoint);
    procedure InitializeBitmap(Source: TBitmap);
  published
    property Color: TColor read FColor write SetColor default clBtnShadow;
    property Offset: Integer read FOffset write SetOffset default 4;
    property Size: Integer read FSize write SetSize;
    property Direction: Integer read FDirection write SetDirection default -45;
    property ShadowAlpha: Single read FAlpha write SetAlpha;
    property SourceAlpha: Single read FSourceAlpha write SetSourceAlpha;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


// Gradient functions.
procedure DrawColorCircle(DC: HDC; Center: TPoint; CenterColor: TRGB; Radius: Integer; Gamma: Double = 1);
procedure DrawGradientBox(DC: HDC; const R: TRect; Colors: array of TRGB);

// Simple paint functions.
procedure DrawComb(Canvas: TCanvas; Center: TPoint; Size: Integer);

// Other support functions.
procedure GaussianBlur(Source: TBitmap; Radius: Double);

// APIs, which are either not yet defined or are defined wrongly.
function GradientFill(DC: HDC; const Vertex {PTriVertex}; NumVertex: ULONG; const Mesh {PGradientTriangle}; NumMesh,
  Mode: ULONG): BOOL; stdcall; external 'msimg32.dll';

var
  CombCorners: array[0..5] of TFloatPoint;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Math, Types, SysUtils;

const
  // Do not modify the copyright in any way! Usage of this unit is prohibited without the copyright notice
  // in the compiled binary file.
  Copyright: string = 'Graphic Tools © 2003 Mike Lischke, Delphi Gems software solutions';

type
  PRGBTriple = ^TRGBTriple;
  TRGBTriple = packed record
    B, G, R: Byte;
  end;
  TRGBArray = array of TRGBTriple;

  PRGBRow = ^TRGBRow;
  TRGBRow = array[0..10000] of TRGBTriple;
  TRGBRows = array of PRGBRow;

  PGrayValue = ^Byte;
  TGArray = array of Byte;

  PGrayRow = ^TGrayRow;
  TGrayRow = array[0..10000] of Byte;
  TGrayRows = array of PGrayRow;

  PRGBAQuadrupel = ^TRGBAQuadrupel;
  TRGBAQuadrupel = packed record
    B, G, R, A: Byte;
  end;

//----------------------------------------------------------------------------------------------------------------------

function ComputeComponent(Sector: Double): Double;

// Computes a component value depending on the given sector.
// The entire distribution is spread over 6 sectors in total. In sector 1 the component's value raises linearly from 0 to 1,
// In sector 2 and 3 it stays constant. In sector 4 the value is decreased linearly to zero and during the last
// two sectors the value remains zero.

begin
  // Limit value to the range [0..6).
  while (Sector < 0) do
    Sector := Sector + 5;
  while (Sector >= 6) do
    Sector := Sector - 6;

  if Sector < 1 then
    Result := Sector
  else
    if Sector < 3 then
        Result := 1
    else
      if Sector < 4 then
        Result := 4 - Sector
      else
        Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure DrawColorCircle(DC: HDC; Center: TPoint; CenterColor: TRGB; Radius: Integer; Gamma: Double);

// Draws a circle filled with a color gradient, which spans the entire rainbow.

const
  RadFactor = Pi / 180;
  ComponentFactor = 6 / 360.0;

var
  Segments: Integer;
  Vertices: array of TTriVertex;
  Triangles: array of TGradientTriangle;
  Angle: Double;
  I: Integer;
  Sector: Double;
  RGB: TRGB;

begin
  // Determine number of segments (filled triangles) to draw, depending on the radius.
  Segments := Max(6, Min(Radius, 180));

  // Allocate memory for vertices and triangles.
  // Center point plus one vertex for each triangle. Two triangles share one vertex.
  SetLength(Vertices, Segments + 1);
  SetLength(Triangles, Segments);

  // Center point is special.
  Vertices[0].x := Center.X;
  Vertices[0].y := Center.Y;
  GammaCorrection(CenterColor, Gamma);
  Vertices[0].Red := Round($FF00 * CenterColor.R);
  Vertices[0].Green := Round($FF00 * CenterColor.G);
  Vertices[0].Blue := Round($FF00 * CenterColor.B);
  Vertices[0].Alpha := 0;

  Angle := 0;
  for I := 1 to Segments do
  begin
    Vertices[I].x :=  Center.X + Round(Radius * cos(Angle * RadFactor));
    Vertices[I].y :=  Center.Y + Round(Radius * sin(Angle * RadFactor));
    Sector := Angle * ComponentFactor;
    RGB.R := ComputeComponent(Sector + 2);
    RGB.G := ComputeComponent(Sector + 4);
    RGB.B := ComputeComponent(Sector);
    GammaCorrection(RGB, Gamma);
    Vertices[I].Red := Round(RGB.R * $FF00);
    Vertices[I].Green := Round(RGB.G * $FF00);
    Vertices[I].Blue := Round(RGB.B * $FF00);
    Vertices[I].Alpha := 0;

    Triangles[I - 1].Vertex1 := 0; // Center
    Triangles[I - 1].Vertex2 := I;
    Triangles[I - 1].Vertex3 := I + 1;

    Angle := Angle + 360.0 / Segments;
  end;
  // Connect the last triangle with the first one.
  Triangles[Segments - 1].Vertex3 := 1;

  GradientFill(DC, Vertices[0], Segments + 1, Triangles[0], Segments, GRADIENT_FILL_TRIANGLE);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure DrawGradientBox(DC: HDC; const R: TRect; Colors: array of TRGB);

// Draws a box with a gradient for all four corners. The Colors parameter carries the required colors where
// index 0 contains the color for the left-upper corner and the others keep the colors for the remaining vertices
// in clock-wise direction. If there are less color than there are corners then the missing colors are derived from the
// last given color by simply copying it.

var
  Vertices: array[0..3] of TTriVertex;
  Triangles: array[0..1] of TGradientTriangle;
  LastColor: TRGB;

  //--------------- local functions --------------------------------------------

  procedure FillVertex(Index: Integer; X, Y: Integer);

  begin
    Vertices[Index].x := X;
    Vertices[Index].y := Y;                           
    if Length(Colors) > Index then
    begin
      Vertices[Index].Red := Round($FF00 * Colors[Index].R);
      Vertices[Index].Green := Round($FF00 * Colors[Index].G);
      Vertices[Index].Blue := Round($FF00 * Colors[Index].B);
      LastColor := MakeRGB(Vertices[Index].Red, Vertices[Index].Green, Vertices[Index].Blue);
    end
    else
    begin
      Vertices[Index].Red := Round(LastColor.R);
      Vertices[Index].Green := Round(LastColor.R);
      Vertices[Index].Blue := Round(LastColor.R);
    end;
    // Alpha is ignored by GradientFill.
    Vertices[Index].Alpha := 0;
  end;

  //--------------- end local functions ----------------------------------------

begin
  // Colors in GradientFill are scaled by 256 to enhance resolution.
  LastColor := MakeRGB($FF00, $FF00, $FF00);

  // Fill 4 vertices...
  FillVertex(0, R.Left, R.Top);
  FillVertex(1, R.Right, R.Top);
  FillVertex(2, R.Right, R.Bottom);
  FillVertex(3, R.Left, R.Bottom);

  // ... and 2 triangles. Two vertices are shared between both triangles.
  Triangles[0].Vertex1 := 0;
  Triangles[0].Vertex2 := 1;
  Triangles[0].Vertex3 := 2;

  Triangles[1].Vertex1 := 2;
  Triangles[1].Vertex2 := 3;
  Triangles[1].Vertex3 := 0;

  GradientFill(DC, Vertices[0], 4, Triangles[0], 2, GRADIENT_FILL_TRIANGLE);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure MakeGaussianKernel(var K: TKernel; Radius: Double; MaxData, DataGranularity: Double);

// Makes K into a gaussian kernel with standard deviation = radius. For the current application you set MaxData = 255,
// DataGranularity = 1. Now the procedure sets the value of K.Size so that when we use K we will ignore the Weights
// that are so small they can't possibly matter. Small size is good because the execution time is going to be
// propertional to K.Size.

var
  I: Integer;
  Temp, Delta: Double;
  KernelSize: TKernelSize;

begin
  for I := Low(K.Weights) to High(K.Weights) do
  begin
    Temp := I / Radius;
    K.Weights[I]:= exp(-Temp * Temp / 2);
  end;

  // Divide by constant so sum(Weights) is 1.
  Temp := 0;
  for I := Low(K.Weights) to High(K.Weights) do
    Temp := Temp + K.Weights[I];

  for I:= Low(K.Weights) to High(K.Weights) do
    K.Weights[I]:= K.Weights[I] / Temp;

  // Discard (or rather mark as ignorable by setting Size) the entries that are too small to matter. This is important,
  // otherwise a blur with a small radius will take as long as with a large radius.
  KernelSize := MaxKernelSize;
  Delta := DataGranularity / (2 * MaxData);
  Temp := 0;
  while (Temp < Delta) and (KernelSize > 1) do
  begin
    Temp := Temp + 2 * K.Weights[KernelSize];
    Dec(KernelSize);
  end;
  K.Size := KernelSize;

  // Now just to be correct go back and jiggle again so the sum of the entries we'll be using is exactly 1.
  Temp := 0;
  for I := -K.Size to K.Size do
    Temp := Temp + K.Weights[I];
  for I := -K.Size to K.Size do
    K.Weights[I] := K.Weights[I] / Temp;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure BlurRowRGB(var Row: array of TRGBTriple; const K: TKernel; P: TRGBArray);

var
  I, N: Integer;
  Red, Green, Blue: Double;
  W: Double;
  
begin
  for I := 0 to K.Size - 1 do
  begin
    Red := 0;
    Green := 0;
    Blue := 0;
    for N := -K.Size to K.Size do
    begin
      W := K.Weights[N];
      // The Max keeps us from running off the edge of the row.
      with Row[Max(0, I + N)] do
      begin
        Blue := Blue + W * B;
        Green := Green + W * G;
        Red := Red + W * R;
      end;
    end;
    with P[I] do
    begin
      B := Round(Blue);
      G := Round(Green);
      R := Round(Red);
    end;
  end;

  for I := K.Size to High(Row) - K.Size - 1 do
  begin
    Blue := 0;
    Green := 0;
    Red := 0;
    for N:= -K.Size to K.Size do
    begin
      W := K.Weights[N];
      with Row[I + N] do
      begin
        Blue := Blue + W * B;
        Green := Green + W * G;
        Red := Red + W * R;
      end;
    end;
    with P[I] do
    begin
      B := Round(Blue);
      G := Round(Green);
      R := Round(Red);
    end;
  end;

  for I := High(Row) - K.Size to High(Row) do
  begin
    Blue := 0;
    Green := 0;
    Red := 0;
    for N := -K.Size to K.Size do
    begin
      W := K.Weights[N];
      // The Min keeps us from running off the edge of the row.
      with Row[Min(High(Row), I + N)] do
      begin
        Blue := Blue + W * B;
        Green := Green + W * G;
        Red := Red + W * R;
      end;
    end;
    with P[I] do
    begin
      B := Round(Blue);
      G := Round(Green);
      R := Round(Red);
    end;
  end;

  Move(P[0], Row[0], (High(Row) + 1) * Sizeof(TRGBTriple));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure BlurRowGray(var Row: array of Byte; const K: TKernel; P: TGArray);

var
  I, N: Integer;
  Gray: Double;

begin
  for I := 0 to K.Size - 1 do
  begin
    Gray := 0;
    for N := -K.Size to K.Size do                               
      // The Max keeps us from running off the edge of the row.
      Gray := Gray + K.Weights[N] * Row[Max(0, I + N)];
    P[I] := Round(Gray);
  end;

  for I := K.Size to High(Row) - K.Size - 1 do
  begin
    Gray := 0;
    for N:= -K.Size to K.Size do
      Gray := Gray + K.Weights[N] * Row[I + N];
    P[I] := Round(Gray);
  end;

  for I := High(Row) - K.Size to High(Row) do
  begin
    Gray := 0;
    for N := -K.Size to K.Size do
      // The Min keeps us from running off the edge of the row.
      Gray := Gray + K.Weights[N] * Row[Min(High(Row), I + N)];
    P[I] := Round(Gray);
  end;

  Move(P[0], Row[0], Length(Row));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure DrawComb(Canvas: TCanvas; Center: TPoint; Size: Integer);

// Draws one single comb at position X, Y and with size Size.
// Fill and border colors must already be set on call (Pen/Brush color).

var
  I: Integer;
  P: array[0..5] of TPoint;

begin
  for I := 0 to 5 do
  begin
    P[I].X := Round(CombCorners[I].X * Size + Center.X);
    P[I].Y := Round(CombCorners[I].Y * Size + Center.Y);
  end;
  Canvas.Polygon(P);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure GaussianBlur(Source: TBitmap; Radius: Double);

// Blurrs the content of Source by the amount given in Radius.
// Note: the source bitmap must be either 24 bits per pixel or 8 bits per pixel grayscale (palette is not considered).

var
  Row, Col: Integer;
  K: TKernel;
  RGBRows: TRGBRows;
  RGBColumn,
  RGBHelp: TRGBArray;
  GRows: TGrayRows;
  GColumn,
  GHelp: TGArray;

begin
  Assert(Source.PixelFormat in [pf8Bit, pf24Bit], 'GaussianBlur: Input source bitmap must be either 24 bpp or 8 bpp.');

  MakeGaussianKernel(K, Radius, 128, 1);
  if Source.PixelFormat = pf24Bit then
  begin
    SetLength(RGBRows, Source.Height);

    // Record the location of the bitmap data.
    for Row := 0 to Source.Height - 1 do
      RGBRows[Row] := Source.Scanline[Row];

    // Blur each row. Allocate temporary memory here to avoid frequent (re)allocation in BlurRow.
    SetLength(RGBHelp, Source.Width);
    for Row:= 0 to Source.Height - 1 do
      BlurRowRGB(Slice(RGBRows[Row]^, Source.Width), K, RGBHelp);

    // Blur each column. Allocate temporary memory here to avoid frequent (re)allocation in BlurRow.
    SetLength(RGBHelp, Source.Height);
    SetLength(RGBColumn, Source.Height);
    for Col := 0 to Source.Width - 1 do
    begin
      // First read the column into a row.
      for Row := 0 to Source.Height - 1 do
        RGBColumn[Row] := RGBRows[Row][Col];

      BlurRowRGB(RGBColumn, K, RGBHelp);

      // Finally put that column back into the data.
      for Row := 0 to Source.Height - 1 do
        RGBRows[Row][Col] := RGBColumn[Row];
    end;
  end
  else
  begin
    SetLength(GRows, Source.Height);

    // Record the location of the bitmap data.
    for Row := 0 to Source.Height - 1 do
      GRows[Row] := Source.Scanline[Row];

    // Blur each row. Allocate temporary memory here to avoid frequent (re)allocation in BlurRow.
    SetLength(GHelp, Source.Width);
    for Row:= 0 to Source.Height - 1 do
      BlurRowGray(Slice(GRows[Row]^, Source.Width), K, GHelp);
    
    // Blur each column. Allocate temporary memory here to avoid frequent (re)allocation in BlurRow.
    SetLength(GHelp, Source.Height);
    SetLength(GColumn, Source.Height);
    for Col := 0 to Source.Width - 1 do
    begin
      // First read the column into a row.
      for Row := 0 to Source.Height - 1 do
        GColumn[Row] := GRows[Row][Col];

      BlurRowGray(GColumn, K, GHelp);

      // Finally put that column back into the data.
      for Row := 0 to Source.Height - 1 do
        GRows[Row][Col] := GColumn[Row];
    end;

  end;
  // Memory for dynamic arrays is freed implicitely.
end;

//----------------- TDropShadow ----------------------------------------------------------------------------------------

constructor TDropShadow.Create;

begin
  FColor := clBtnShadow;
  FOffset := 4;
  FSize := 3;
  FAlpha := 1;
  FDirection := -45;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TDropShadow.Destroy;

begin
  if FGrayRampPalette <> 0 then
    DeleteObject(FGrayRampPalette);
    
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropShadow.SetAlpha(const Value: Single);

// Alpha must be in the range of 0..1 and allows to specify the translucency of the shadow with 1 being fully opaque
// and 0 being fully transparent.

begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    DoChange;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropShadow.SetColor(const Value: TColor);

begin
  if FColor <> Value then
  begin
    FColor := Value;
    DoChange;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropShadow.SetDirection(const Value: Integer);

// Direction indicates into which direction the shadow should expand.

begin
  if FDirection <> Value then
  begin
    FDirection := Value;
    DoChange;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropShadow.SetOffset(const Value: Integer);

// Offset specifies the shadow distance and Size the shadow size in pixels.

begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    DoChange;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropShadow.SetSize(Value: Integer);

begin
  if Value < 1 then
    Value := 1;
  // Value must be odd.
  Value := Value or 1;
  if FSize <> Value then
  begin
    FSize := Value;
    DoChange;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropShadow.SetSourceAlpha(const Value: Single);

begin
  FSourceAlpha := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropShadow.DoChange;

begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropShadow.AverageMeanBlur(Bitmap: TBitmap);

// Implementation of a recursive moving average filter to blur the input bitmap.
// Currently only gray scale images (8 bpp) are supported as this method is used to create a soft drop shadow.

var
  BytesPerLine: Integer;     // The physical length of a scanline (Windows bitmaps are 32 bit aligned).
  X, Y, I: Integer;
  Run: PGrayValue;
  RunIndex: Integer;
  RunLow,
  RunHigh: PGrayValue;
  DIB: TDIBSection;          // The bitmap's DIB section.

  //---------------------------------------------------------------------------

  function GetPixelAddress(Column, Row: Integer): Pointer;

  // Computes the address of the pixel given by Row and Column in Bitmap.

  begin
    with DIB, dsbm, dsbmih do
    begin
      if biHeight > 0 then
        Row := bmHeight  - Row - 1;
      Integer(Result) := Integer(bmBits) + Row * Abs(BytesPerLine) + Column {* bytes per pixel};
    end;
  end;

  //---------------------------------------------------------------------------

var
  Accumulator: Integer;
  Buffer: array of Byte;

begin
  if (Bitmap.PixelFormat = pf8Bit) and (GetObject(Bitmap.Handle, SizeOf(DIB), @DIB) = SizeOf(DIB)) then
  begin
    with DIB, dsbm, dsbmih do
    begin
      BytesPerLine := BytesPerScanline(biWidth, biBitCount, 32);
      if biHeight > 0 then
        BytesPerLine := -BytesPerLine;
    end;
    // Start with horizontal spans.
    SetLength(Buffer, Bitmap.Width);
    for Y := 0 to Bitmap.Height - 1 do
    begin
      Run := GetPixelAddress(0, Y);
      // Compute first FSize number of averages. Note: FSize must be odd!
      Accumulator := 0;
      for I := 0 to FSize - 1 do
      begin
        Accumulator :=  Accumulator + Integer(Run^);
        Inc(Run);
      end;
      Buffer[FSize div 2] := Accumulator div FSize;
      RunIndex := (FSize div 2) + 1;

      // Continue with the recursive part.
      RunLow := GetPixelAddress(0, Y);
      RunHigh := GetPixelAddress(FSize, Y);
      for I := 0 to Bitmap.Width - FSize - 1 do
      begin
        Accumulator := Accumulator + RunHigh^ - RunLow^;
        Buffer[RunIndex] := Accumulator div FSize;
        Inc(RunIndex);
        Inc(RunHigh);
        Inc(RunLow);
      end;
      Run := GetPixelAddress(0, Y);
      Move(Buffer[0], Run^, Length(Buffer));
    end;

    SetLength(Buffer, Bitmap.Height);
    for X := 0 to Bitmap.Width - 1 do
    begin
      Run := GetPixelAddress(X, 0);
      // Compute first FSize number of averages. Note: FSize must be odd!
      Accumulator := 0;
      for I := 0 to FSize - 1 do
      begin
        Accumulator :=  Accumulator + Integer(Run^);
        Inc(Run, BytesPerLine);
      end;
      Buffer[FSize div 2] := Accumulator div FSize;
      RunIndex := (FSize div 2) + 1;

      // Continue with the recursive part.
      RunLow := GetPixelAddress(X, 0);
      RunHigh := GetPixelAddress(X, FSize);
      for I := 0 to Bitmap.Height - FSize - 1 do
      begin
        Accumulator := Accumulator + RunHigh^ - RunLow^;
        Buffer[RunIndex] := Accumulator div FSize;
        Inc(RunIndex);
        Inc(RunHigh, BytesPerLine);
        Inc(RunLow, BytesPerLine);
      end;
      Run := GetPixelAddress(X, 0);
      for I := 0 to Bitmap.Height - 1 do
      begin
        Run^ := Buffer[I];
        Inc(Run, BytesPerLine);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropShadow.SetGrayScalePalette(Bitmap: TBitmap);

var
  LogPalette: TMaxLogPalette;
  I: Integer;

begin
  if FGrayRampPalette = 0 then
  begin
    LogPalette.palVersion := $300;
    LogPalette.palNumEntries := 256;
    for I := 0 to 255 do
    begin
      LogPalette.palPalEntry[I].peBlue := I;
      LogPalette.palPalEntry[I].peGreen := I;
      LogPalette.palPalEntry[I].peRed := I;
    end;
    FGrayRampPalette := CreatePalette(PLogPalette(@LogPalette)^);
  end;
  Bitmap.Palette := CopyPalette(FGrayRampPalette);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropShadow.Draw(Source: TBitmap; SourceRect: TRect; Target: HDC; TargetPos: TPoint);

// Draws the given bitmap (which must have an alpha channel) on Target but creates a drop shadow underneath the source pixels.
// SourceRect determines the area of the source bitmap to create a shadow for.
// Target is where the result is to be drawn to and TargetPos gives the final position.
// Note: Usually the alpha value of a pixel must be 255 to be fully opaque and that's how the AlphaBlend API
//       expects the values. However we cannot make all Delphi paint code (see TCanvas methods) to issue an alpha channel
//       value of 255 for its graphic output. Hence the meaning is reversed here. So make everything, which should be
//       transparent 255 and everything else 0. Of course you can also have a partially translucent source image of which
//       also a correct drop shadow will be created. In case of translucent source pixels the shadow shines through but
//       the shadow translucency is not modified by the source translucency.

var
  WorkBitmap: TBitmap;
  ShadowBitmap: TBitmap;
  TargetBitmap: TBitmap;

  X, Y: Integer;
  SourceWidth,
  SourceHeight: Integer;
  Alpha: Byte;

  SourceLine: PRGBAQuadrupel;
  TargetLine: PRGBAQuadrupel;
  ShadowLine: PGrayValue;
  NewShadowColor: TRGBAQuadrupel;

  BlendFunction: TBlendFunction;

begin
  // Source bitmap must have an alpha channel (pf32Bit), where an alpha value of 0 means fully opaque pixels and
  // a value of 255 denotes fully transparent pixels. The alpha channel is used to create the drop shadow outline.
  if Source.PixelFormat <> pf32Bit then
    BitBlt(Target, TargetPos.X, TargetPos.Y, SourceRect.Right - SourceRect.Left, SourceRect.Bottom - SourceRect.Top,
      Source.Canvas.Handle, SourceRect.Left, SourceRect.Top, SRCCOPY)
  else
  begin
    WorkBitmap := TBitmap.Create;
    ShadowBitmap := TBitmap.Create;
    TargetBitmap := TBitmap.Create;
    try
      SourceWidth := SourceRect.Right - SourceRect.Left;
      SourceHeight := SourceRect.Bottom - SourceRect.Top;
      with WorkBitmap do
      begin
        PixelFormat := pf32Bit;
        Width := Source.Width;
        Height := Source.Height;
        InitializeBitmap(WorkBitmap);
        // Determine X and Y offsets from direction and shadow offset.
        X := Round(FOffset * cos(DegToRad(-FDirection)));
        Y := Round(FOffset * sin(DegToRad(-FDirection)));
        // Copy the pixels given by the source rectangle out of the source bitmap.
        BitBlt(Canvas.Handle, X, Y, SourceWidth, SourceHeight, Source.Canvas.Handle, SourceRect.Left, SourceRect.Top,
          SRCCOPY);
      end;

      // Convert alpha channel into a grayscale image. We need a proper palette here before we do the blurring.
      ShadowBitmap.PixelFormat := pf8Bit;
      ShadowBitmap.Width := WorkBitmap.Width;
      ShadowBitmap.Height := WorkBitmap.Height;
      InitializeBitmap(ShadowBitmap);
      SetGrayScalePalette(ShadowBitmap);
      for Y := 0 to ShadowBitmap.Height - 1 do
      begin
        SourceLine := WorkBitmap.Scanline[Y];
        ShadowLine := ShadowBitmap.Scanline[Y];
        for X := 0 to ShadowBitmap.Width - 1 do
        begin
          // Convert the alpha value to its correct range (0 for full opacity).
          ShadowLine^ := not SourceLine.A;
          Inc(SourceLine);
          Inc(ShadowLine);
        end;
      end;
      AverageMeanBlur(ShadowBitmap);
      AverageMeanBlur(ShadowBitmap);
         
      // Convert the shadow bitmap into the target bitmap. Use the values from the shadow map as
      // alpha channel and the shadow color as the bitmap's color value at that pixel.
      with TargetBitmap do
      begin
        PixelFormat := pf32Bit;
        Width := ShadowBitmap.Width;
        Height := ShadowBitmap.Height;
      end;

      // For alpha blending we have to premultiply our shadow color by the alpha value.
      NewShadowColor.R := GetRValue(ColorToRGB(FColor));
      NewShadowColor.G := GetGValue(ColorToRGB(FColor));
      NewShadowColor.B := GetBValue(ColorToRGB(FColor));
      NewShadowColor.A := 0;

      for Y := 0 to ShadowBitmap.Height - 1 do
      begin
        TargetLine := TargetBitmap.Scanline[Y];
        ShadowLine := ShadowBitmap.ScanLine[Y];
        for X := 0 to ShadowBitmap.Width - 1 do
        begin
          TargetLine.R := MulDiv(NewShadowColor.R, ShadowLine^, 255);
          TargetLine.G := MulDiv(NewShadowColor.G, ShadowLine^, 255);
          TargetLine.B := MulDiv(NewShadowColor.B, ShadowLine^, 255);
          TargetLine.A := ShadowLine^;
          Inc(TargetLine);
          Inc(ShadowLine);
        end;
      end;

      // Now do the blending of shadow and source image.
      BlendFunction.BlendOp := AC_SRC_OVER;
      BlendFunction.BlendFlags := 0;
      BlendFunction.AlphaFormat := AC_SRC_ALPHA;

      // First blend shadow to the target DC.
      BlendFunction.SourceConstantAlpha := Round(255 * FAlpha);
      AlphaBlend(Target, TargetPos.X, TargetPos.Y, ShadowBitmap.Width, ShadowBitmap.Height, TargetBitmap.Canvas.Handle,
        SourceRect.Left, SourceRect.Top, ShadowBitmap.Width, ShadowBitmap.Height, BlendFunction);

      // Premultiply the source colors.
      for Y := 0 to Source.Height - 1 do
      begin
        SourceLine := Source.Scanline[Y];
        for X := 0 to Source.Width - 1 do
        begin
          Alpha := not SourceLine.A;
          SourceLine.R := MulDiv(SourceLine.R, Alpha, 255);
          SourceLine.G := MulDiv(SourceLine.G, Alpha, 255);
          SourceLine.B := MulDiv(SourceLine.B, Alpha, 255);
          SourceLine.A := Alpha;
          Inc(SourceLine);
        end;
      end;

      // Finally blend the source image data to the target DC.
      BlendFunction.SourceConstantAlpha := Round(255 * FSourceAlpha);
      AlphaBlend(Target, TargetPos.X, TargetPos.Y, SourceWidth, SourceHeight, Source.Canvas.Handle, SourceRect.Left,
        SourceRect.Top, SourceWidth, SourceHeight, BlendFunction);
    finally
      WorkBitmap.Free;
      TargetBitmap.Free;
      ShadowBitmap.Free;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropShadow.InitializeBitmap(Source: TBitmap);

// Sets all component values of all pixels to 255. Particularly the alpha channel value is important for us.
// Source must be a 32 bit pixel format bitmap.

var
  DIB: TDIBSection;

begin
  case Source.PixelFormat of
    pf32Bit:
      begin
        if GetObject(Source.Handle, SizeOf(DIB), @DIB) = SizeOf(DIB) then
          FillChar(DIB.dsBm.bmBits^, Source.Width * Source.Height * 4, 255);
      end;
    pf8Bit:
      begin
        if GetObject(Source.Handle, SizeOf(DIB), @DIB) = SizeOf(DIB) then
          FillChar(DIB.dsBm.bmBits^, Source.Width * Source.Height, 255);
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure InitializeGlobalData;

var
  I: Integer;
  
begin
  // Initialize comb calculation. Do this once for quick calculations.
  for I := 0 to 5 do
  begin
    CombCorners[I].X := 0.5 * cos(Pi * (90 - I * 60) / 180);
    CombCorners[I].Y := 0.5 * sin(Pi * (90 - I * 60) / 180);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  InitializeGlobalData;
end.

