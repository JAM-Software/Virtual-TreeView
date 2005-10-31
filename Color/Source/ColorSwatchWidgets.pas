unit ColorSwatchWidgets;

//----------------------------------------------------------------------------------------------------------------------
// ColorSwatchWidgets contains color picker widgets for the color picker control, which deal with Adobe Photoshop
// color swatch files. This includes for instance TRUEMATCH, ANPA, FOCULTONE, PANTONE, TOYO and other industrie standard
// color systems if the respective files are present.
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
// The original code is ColorSwatchWidgets.pas, released 1. April 2003.
//
// The initial developer of the original code is:
//   Dipl. Ing. Mike Lischke, Delphi Gems software solutions (public@delphi-gems.com, www.delphi-gems.com).
//
// Portions created by Delphi Gems are
//   (C) 1999-2003 Delphi Gems. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------

interface

{$I Compilers.inc}

{$ifdef COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
  // We switch off warnings about that fact. We know it and we accept it.
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CAST off}
  {$warn UNSAFE_CODE off}
{$endif COMPILER_7_UP}

uses
  Windows, ColorPicker, ColorPickerTypes, Classes, Graphics;

type
  // Color picker widget to load Photoshop color swatch files.
  TColorSwatchWidget = class(TDiscreteColorPickerWidget)
  private
    FCurrentSwatchFile: string;
    FLoadPending: Boolean;        // True if a swatch file has been set, which needs a color profile (e.g. CMYK), but
                                  // this color profile is not yet available.
    procedure SetSwatchFile(const Value: string);
  protected
    procedure AddDefaultColors; override;
    procedure ColorProfileChanged; override;
    procedure ConvertColor(const Name: WideString; ColorScheme: Integer; const Value1, Value2, Value3, Value4: SmallInt);
    procedure Error(const S: string);
    procedure LoadSwatchFile;
  public
    class function GetDescription: string; override;
    class function GetSummary: string; override;
    class procedure RenderPreview(Canvas: TCanvas; const R: TRect); override;
  published
    property ColorFormat;
    property SwatchFile: string read FCurrentSwatchFile write SetSwatchFile;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R 'Res\Extra.res'}

uses
  Dialogs, SysUtils, ColorTools, ColorPickerStrings;

//----------------------------------------------------------------------------------------------------------------------

procedure Swap(var Value: Word); overload;

asm
                   MOV DX, [Value]
                   XCHG DL, DH
                   MOV [Value], DX
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Swap(var Value: SmallInt); overload;

asm
                   MOV DX, [Value]
                   XCHG DL, DH
                   MOV [Value], DX
end;
                       
//----------------------------------------------------------------------------------------------------------------------

procedure Swap(var S: WideString); overload;

var
  I: Integer;
  Value: Word;

begin
  for I := 1 to Length(S) do
  begin
    Value := Word(S[I]);
    Swap(Value);
    S[I] := WideChar(Value);
  end;
end;

//----------------- TColorSwatchWidget ---------------------------------------------------------------------------------

procedure TColorSwatchWidget.SetSwatchFile(const Value: string);

begin
  if FCurrentSwatchFile <> Value then
  begin
    FCurrentSwatchFile := Value;
    LoadSwatchFile;
    StructureChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorSwatchWidget.AddDefaultColors;

begin
  LoadSwatchFile;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorSwatchWidget.ColorProfileChanged;

begin
  inherited;
  
  if FLoadPending then
    LoadSwatchFile;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorSwatchWidget.ConvertColor(const Name: WideString; ColorScheme: Integer; const Value1, Value2, Value3,
  Value4: SmallInt);

// Adds the given values as new color into the internal list.

var                                                 
  Components: TColorComponents;
  
begin
  case ColorScheme of
    0: // RGB
      begin
        SetLength(Components, 3);
        Components[0] := Word(Value1) / 65535;
        Components[1] := Word(Value2) / 65535;
        Components[2] := Word(Value3) / 65535;
        AddColor(Name, cfRGB, Components); 
      end;
    2: // CMYK
      begin
        SetLength(Components, 4);
        Components[0] := 100 - Word(Value1) / 655.35;
        Components[1] := 100 - Word(Value2) / 655.35;
        Components[2] := 100 - Word(Value3) / 655.35;
        Components[3] := 100 - Word(Value4) / 655.35;
        AddColor(Name, cfCMYK, Components);
      end;
    7: // L*a*b*
      begin
        SetLength(Components, 3);
        Components[0] := Value1 / 100;
        Components[1] := Value2 / 100;
        Components[2] := Value3 / 100;
        AddColor(Name, cfCIELab, Components);
      end;
  else
    Error(SInvalidColorScheme);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorSwatchWidget.Error(const S: string);

begin
  raise Exception.Create(S);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorSwatchWidget.LoadSwatchFile;

// If there is a Photoshop color swatch file given then it is opened and parsed here.

var
  Version: Word;
  ColorCount: Word;
  ColorScheme: Word;
  Value1,
  Value2,
  Value3,
  Value4: Smallint;
  ByteValue: Byte;
  WordValue: Word;
  ANSIName: string;
  UnicodeName: WideString;

begin
  Clear;
  if FileExists(FCurrentSwatchFile) then
  begin
    with TFileStream.Create(FCurrentSwatchFile, fmOpenRead or fmShareDenyWrite) do
    try
      // Photoshop color swatch files are stored in big-endian style. Additionally, there are at least three versions.
      // Version 0 contains color names in ANSI form (pre PS 7).
      // Version 1 does not contain color names at all (all Photoshop versions).
      // Version 3 comes with Unicode strings (PS 7 and up).
      Read(Version, SizeOf(Version));
      Swap(Version);
      if not (Version in [0..2]) then
        Error(SInvalidSwatchFile);

      Read(ColorCount, SizeOf(ColorCount));
      Swap(ColorCount);                         

      // Now read in the colors and convert them to
      while Position < Size do
      begin
        // Color scheme is encoded with a simple number, where following assignments are known currently:
        // 0 - RGB
        // 2 - CMYK
        // 7 - L*a*b*
        // The size of the entry depends on the file version and there is an own marker for each color entry.
        Read(WordValue, SizeOf(WordValue));
        Swap(WordValue);
        ColorScheme := WordValue;

        // Each color component is 16 bits wide and its interpretation (signed/unsigned) depends on the used color scheme.
        Read(Value1, SizeOf(Value1));
        Swap(Value1);
        Read(Value2, SizeOf(Value2));
        Swap(Value2);
        Read(Value3, SizeOf(Value3));
        Swap(Value3);
        Read(Value4, SizeOf(Value4));
        Swap(Value4);

        // The name format also depends on the version.
        // Note: there are no color names for version 1.
        case Version of
          0:
            begin
              // The color name is a short string with a length value and terminated by a 0.
              Read(ByteValue, SizeOf(ByteValue));
              SetLength(ANSIName, ByteValue);
              if ByteValue > 0 then
                Read(PChar(ANSIName)^, ByteValue);
              // There is no 0 terminator.
              ConvertColor(ANSIName, ColorScheme, Value1, Value2, Value3, Value4);
            end;
          1:
            ConvertColor('', ColorScheme, Value1, Value2, Value3, Value4);
          2:
            begin
              // There are two 0 characters whose meaning I don't know. But they aren't important AFAICS -> skip them.
              Seek(2, soFromCurrent);

              // The color name is a Unicode string with a length value and terminated by a 0.
              Read(WordValue, SizeOf(WordValue));
              Swap(WordValue);

              // The 0 terminator is included in the count but shall not appear in the string
              // (there is an implicit terminator already).
              SetLength(UnicodeName, WordValue - 1);
              if WordValue > 0 then
              begin
                Read(PWideChar(UnicodeName)^, 2 * (WordValue - 1));
                Swap(UnicodeName);
              end;

              // Skip the left-over 0 terminator.
              Seek(2, soFromCurrent);

              ConvertColor(UnicodeName, ColorScheme, Value1, Value2, Value3, Value4);
            end;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TColorSwatchWidget.GetDescription: string;

begin
  Result := SColorSwatchWidgetDescription;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TColorSwatchWidget.GetSummary: string;

begin
  Result := SColorSwatchWidgetSummary;
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TColorSwatchWidget.RenderPreview(Canvas: TCanvas; const R: TRect);

var
  Icon: TIcon;
  X, Y: Integer;

begin
  Icon := TIcon.Create;
  try
    Icon.Handle := LoadImage(FindClassHInstance(TColorSwatchWidget), 'SWATCH', IMAGE_ICON, 0, 0, LR_DEFAULTCOLOR);
    // Icon.Width returns 32 instead the true 48 pixels. Any idea what's wrong here?
    X := (R.Right + R.Left - 48) div 2;
    Y := (R.Bottom + R.Top - 48) div 2;
    Canvas.Draw(X, Y, Icon);
  finally
    Icon.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  RegisterColorPickerWidget(TColorSwatchWidget);
end.
 
