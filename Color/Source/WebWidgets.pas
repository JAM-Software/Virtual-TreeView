unit WebWidgets;

//----------------------------------------------------------------------------------------------------------------------
// WebWidges contains color picker widgets for the color picker control related to internet (web browsers etc.).
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
// The original code is WebWidgets.pas, released 1. April 2003.
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
  Windows, ColorPickerTypes;

type
  // Special Netscape color list.
  TNetscapeColorsCpWidget = class(TDiscreteColorPickerWidget)
  protected
    procedure AddDefaultColors; override;
  public
    class function GetDescription: string; override;
    class procedure GetPreviewColors(var Colors: PPreviewColors; var Count: Integer); override;
    class function GetSummary: string; override;
  end;

  // 138 colors used in HTML recognized by IE and Netscape.
  TNamedHTMLColorsCpWidget = class(TDiscreteColorPickerWidget)
  protected
    procedure AddDefaultColors; override;
  public
    class function GetDescription: string; override;
    class procedure GetPreviewColors(var Colors: PPreviewColors; var Count: Integer); override;
    class function GetSummary: string; override;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  ColorPicker, ColorPickerStrings;

//----------------------------------------------------------------------------------------------------------------------

const
  NetscapeColors: array[0..454] of COLORREF = (
    $000000, $1C1C1C, $363636, $4F4F4F, $696969, $696969, $828282, $9C9C9C, $A9A9A9, $B5B5B5, $BEBEBE, $CFCFCF, $D3D3D3,
    $DCDCDC, $E8E8E8, $F5F5F5, $FFFFFF, $89898B, $C9C9CD, $FAFAFF, $FAFAFF, $E9E9EE, $8F8FBC, $C1C1FF, $9B9BCD, $B4B4EE,
    $69698B, $8080F0, $5C5CCD, $3A3A8B, $6363EE, $6A6AFF, $5555CD, $2A2AA5, $23238B, $4040FF, $3333CD, $3B3BEE, $2222B2,
    $3030FF, $1A1A8B, $2626CD, $2C2CEE, $00008B, $00008B, $0000CD, $0000EE, $0000FF, $0000FF, $B5B7CD, $E1E4FF, $E1E4FF,
    $7280FA, $D2D5EE, $7B7D8B, $394FCD, $4763FF, $4763FF, $425CEE, $26368B, $455BCD, $2F3E8B, $5672FF, $506AEE, $6282EE,
    $394C8B, $5470CD, $698CFF, $7A96E9, $00258B, $507FFF, $0037CD, $0040EE, $0045FF, $0045FF, $7295EE, $7AA0FF, $7AA0FF,
    $42578B, $6281CD, $3968CD, $4782FF, $4279EE, $2D52A0, $26478B, $EEF5FF, $EEF5FF, $1D66CD, $247FFF, $2176EE, $1E69D2,
    $13458B, $13458B, $BFC5CD, $DEE5EE, $82868B, $60A4F4, $ADCBEE, $95AFCD, $B9DAFF, $B9DAFF, $65778B, $4FA5FF, $2B5A8B,
    $499AEE, $3F85CD, $3F85CD, $0076EE, $00458B, $0066CD, $007FFF, $E6F0FA, $9EB7CD, $C4E4FF, $C4E4FF, $B7D5EE, $008CFF,
    $B0C0CD, $DBEFFF, $55738B, $CCDFEE, $91C5EE, $9BD3FF, $6B7D8B, $7DAACD, $87B8DE, $D7EBFA, $8CB4D2, $78838B, $A1CFEE,
    $ADDEFF, $ADDEFF, $CDEBFF, $5E798B, $8BB3CD, $D5EFFF, $B5E4FF, $005A8B, $009AEE, $00A5FF, $00A5FF, $667E8B, $0085CD,
    $E6F5FD, $B3DEF5, $BAE7FF, $96BACD, $AED8EE, $F0FAFF, $0FB9FF, $0C95CD, $0EADEE, $0B86B8, $20A5DA, $25C1FF, $14698B,
    $22B4EE, $1D9BCD, $DCF8FF, $DCF8FF, $CDE8EE, $B1C8CD, $82DCEE, $8BECFF, $70BECD, $78888B, $4C818B, $00758B, $82DDEE,
    $00ADCD, $00D7FF, $00D7FF, $00C9EE, $BFE9EE, $A5C9CD, $CDFAFF, $CDFAFF, $AAE8EE, $4E868B, $8FF6FF, $73C6CD, $85E6EE,
    $70898B, $6BB7BD, $838B8B, $C1CDCD, $E0EEEE, $F0FFFF, $F0FFFF, $DCF5F5, $7A8B8B, $B4CDCD, $D1EEEE, $E0FFFF, $E0FFFF,
    $D2FAFA, $008B8B, $00CDCD, $00EEEE, $00FFFF, $00FFFF, $228B69, $238E6B, $3EFFC0, $32CD9A, $32CD9A, $3AEEB3, $2F6B55,
    $70FFCA, $3D8B6E, $5ACDA2, $68EEBC, $2FFFAD, $00CD66, $00FF7F, $00FF7F, $008B45, $00EE76, $00FC7C, $838B83, $C1CDC1,
    $E0EEE0, $F0FFF0, $F0FFF0, $8FBC8F, $C1FFC1, $9BCD9B, $B4EEB4, $698B69, $98FB98, $7CCD7C, $90EE90, $90EE90, $548B54,
    $9AFF9A, $228B22, $32CD32, $006400, $008B00, $00CD00, $00EE00, $00FF00, $00FF00, $9FFF54, $94EE4E, $578B2E, $578B2E,
    $80CD43, $71B33C, $76EE00, $458B00, $66CD00, $7FFF00, $7FFF00, $FAFFF5, $9AFA00, $AACD66, $AACD66, $D4FF7F, $D4FF7F,
    $C6EE76, $748B45, $D0E040, $AAB220, $CCD148, $8B8B83, $CDCDC1, $EEEEE0, $FFFFF0, $FFFFF0, $8B8B7A, $CDCDB4, $EEEED1,
    $FFFFE0, $FFFFE0, $EEEEAF, $8B8B66, $FFFFBB, $CDCD96, $EEEEAE, $4F4F2F, $EEEE8D, $FFFF97, $8B8B52, $CDCD79, $8B8B00,
    $8B8B00, $CDCD00, $EEEE00, $FFFF00, $FFFF00, $D1CE00, $A09E5F, $8B8600, $CDC500, $EEE500, $FFF500, $8B8653, $EEE58E,
    $FFF598, $CDC57A, $E6E0B0, $8B8368, $E6D8AD, $CD9A00, $FFEFBF, $EEDFB2, $8B6800, $EEB200, $FFBF00, $FFBF00, $CDC09A,
    $EBCE87, $CDB68D, $EED3A4, $FFE2B0, $8B7B60, $FACE87, $CDA66C, $FFCE87, $EEC07E, $8B704A, $EEAC5C, $CD944F, $B48246,
    $FFB863, $8B6436, $FFF8F0, $CD7418, $FF901E, $FF901E, $EE861C, $8B4E10, $908070, $998877, $CDB69F, $FFE2C6, $EED3B9,
    $8B7B6C, $8B7B6E, $CDB5A2, $EED2BC, $DEC4B0, $FFE1CA, $ED9564, $CD5F3A, $EE6E43, $FF7648, $E16941, $8B4027, $FFF8F8,
    $FAE6E6, $701919, $800000, $8B0000, $8B0000, $CD0000, $CD0000, $EE0000, $FF0000, $FF0000, $CD5A6A, $FF6F83, $CD5969,
    $FF7084, $EE677A, $8B3C47, $8B3D48, $EE687B, $8B475D, $EE799F, $DB7093, $CD6889, $FF82AB, $FF309B, $E22B8A, $EE2C91,
    $8B1A55, $CD267D, $F020A0, $8B2268, $EE3AB2, $CC3299, $FF3EBF, $CD329A, $D30094, $CD52B4, $FF66E0, $EE5FD1, $8B377A,
    $D355BA, $8B7B8B, $D8BFD8, $CDB5CD, $EED2EE, $FFE1FF, $8B668B, $FFBBFF, $CD96CD, $EEAEEE, $8B658B, $DDA0DD, $EE82EE,
    $8B008B, $8B008B, $CD00CD, $EE00EE, $FF00FF, $FF00FF, $89478B, $D670DA, $FA83FF, $C969CD, $E97AEE, $9020D0, $621C8B,
    $8515C7, $9029CD, $A730EE, $B334FF, $500A8B, $8912EE, $7610CD, $9314FF, $9314FF, $B469FF, $623A8B, $B46EFF, $A76AEE,
    $52228B, $963EFF, $8C3AEE, $7832CD, $9060CD, $86838B, $6030B0, $E5E0EE, $C5C1CD, $F5F0FF, $F5F0FF, $AB82FF, $9370DB,
    $8968CD, $9F79EE, $5D478B, $6C638B, $B8A9EE, $C5B5FF, $9E91CD, $CBC0FF, $C1B6FF, $ADA2EE, $958CCD, $655F8B, $B9AEFF
  );
  
  NetscapeColorNames: array[0..454] of string = (
    'Black', 'Grey11', 'Grey21', 'Grey31', 'DimGrey', 'Grey41', 'Grey51', 'Grey61', 'DarkGrey', 'Grey71', 'Grey',
    'Gray81', 'LightGray', 'Gainsboro', 'Gray91', 'WhiteSmoke', 'White', 'Snow4', 'Snow3', 'Snow', 'Snow1', 'Snow2',
    'RosyBrown', 'RosyBrown1', 'RosyBrown3', 'RosyBrown2', 'RosyBrown4', 'LightCoral', 'IndianRed', 'IndianRed4',
    'IndianRed2', 'IndianRed1', 'IndianRed3', 'Brown', 'Brown4', 'Brown1', 'Brown3', 'Brown2', 'Firebrick',
    'Firebrick1', 'Firebrick4', 'Firebrick3', 'Firebrick2', 'Red4', 'DarkRed', 'Red3', 'Red2', 'Red', 'Red1',
    'MistyRose3', 'MistyRose', 'MistyRose1', 'Salmon', 'MistyRose2', 'MistyRose4', 'Tomato3', 'Tomato', 'Tomato1',
    'Tomato2', 'Tomato4', 'Coral3', 'Coral4', 'Coral1', 'Coral2', 'Salmon2', 'Salmon4', 'Salmon3', 'Salmon1',
    'DarkSalmon', 'OrangeRed4', 'Coral', 'OrangeRed3', 'OrangeRed2', 'OrangeRed', 'OrangeRed1', 'LightSalmon2',
    'LightSalmon', 'LightSalmon1', 'LightSalmon4', 'LightSalmon3', 'Sienna3', 'Sienna1', 'Sienna2', 'Sienna', 'Sienna4',
    'Seashell', 'Seashell1', 'Chocolate3', 'Chocolate1', 'Chocolate2', 'Chocolate', 'SaddleBrown', 'Chocolate4',
    'Seashell3', 'Seashell2', 'Seashell4', 'SandyBrown', 'PeachPuff2', 'PeachPuff3', 'PeachPuff', 'PeachPuff1',
    'PeachPuff4', 'Tan1', 'Tan4', 'Tan2', 'Peru', 'Tan3', 'DarkOrange2', 'DarkOrange4', 'DarkOrange3', 'DarkOrange1',
    'Linen', 'Bisque3', 'Bisque', 'Bisque1', 'Bisque2', 'DarkOrange', 'AntiqueWhite3', 'AntiqueWhite1', 'Burlywood4',
    'AntiqueWhite2', 'Burlywood2', 'Burlywood1', 'Bisque4', 'Burlywood3', 'Burlywood', 'AntiqueWhite', 'Tan',
    'AntiqueWhite4', 'NavajoWhite2', 'NavajoWhite', 'NavajoWhite1', 'BlanchedAlmond', 'NavajoWhite4', 'NavajoWhite3',
    'PapayaWhip', 'Moccasin', 'Orange4', 'Orange2', 'Orange', 'Orange1', 'Wheat4', 'Orange3', 'OldLace', 'Wheat',
    'Wheat1', 'Wheat3', 'Wheat2', 'FloralWhite', 'DarkGoldenrod1', 'DarkGoldenrod3', 'DarkGoldenrod2', 'DarkGoldenrod',
    'Goldenrod', 'Goldenrod1', 'Goldenrod4', 'Goldenrod2', 'Goldenrod3', 'Cornsilk', 'Cornsilk1', 'Cornsilk2',
    'Cornsilk3', 'LightGoldenrod2', 'LightGoldenrod1', 'LightGoldenrod3', 'Cornsilk4', 'LightGoldenrod4', 'Gold4',
    'LightGoldenrod', 'Gold3', 'Gold', 'Gold1', 'Gold2', 'LemonChiffon2', 'LemonChiffon3', 'LemonChiffon', 'LemonChiffon1',
    'PaleGoldenrod', 'Khaki4', 'Khaki1', 'Khaki3', 'Khaki2', 'LemonChiffon4', 'DarkKhaki', 'Ivory4', 'Ivory3', 'Ivory2',
    'Ivory', 'Ivory1', 'Beige', 'LightYellow4', 'LightYellow3', 'LightYellow2', 'LightYellow', 'LightYellow1',
    'LtGoldenrodYellow', 'Yellow4', 'Yellow3', 'Yellow2', 'Yellow', 'Yellow1', 'OliveDrab4', 'OliveDrab', 'OliveDrab1',
    'YellowGreen', 'OliveDrab3', 'OliveDrab2', 'DarkOliveGreen', 'DarkOliveGreen1', 'DarkOliveGreen4', 'DarkOliveGreen3',
    'DarkOliveGreen2', 'GreenYellow', 'Chartreuse3', 'Chartreuse', 'Chartreuse1', 'Chartreuse4', 'Chartreuse2',
    'LawnGreen', 'Honeydew4', 'Honeydew3', 'Honeydew2', 'Honeydew', 'Honeydew1', 'DarkSeaGreen', 'DarkSeaGreen1',
    'DarkSeaGreen3', 'DarkSeaGreen2', 'DarkSeaGreen4', 'PaleGreen', 'PaleGreen3', 'PaleGreen2', 'LightGreen',
    'PaleGreen4', 'PaleGreen1', 'ForestGreen', 'LimeGreen', 'DarkGreen', 'Green4', 'Green3', 'Green2', 'Green', 'Green1',
    'SeaGreen1', 'SeaGreen2', 'SeaGreen', 'SeaGreen4', 'SeaGreen3', 'MediumSeaGreen', 'SpringGreen2', 'SpringGreen4',
    'SpringGreen3', 'SpringGreen', 'SpringGreen1', 'MintCream', 'MedSpringGreen', 'MediumAquamarine', 'Aquamarine3',
    'Aquamarine', 'Aquamarine1', 'Aquamarine2', 'Aquamarine4', 'Turquoise', 'LightSeaGreen', 'MediumTurquoise',
    'Azure4', 'Azure3', 'Azure2', 'Azure', 'Azure1', 'LightCyan4', 'LightCyan3', 'LightCyan2', 'LightCyan', 'LightCyan1',
    'PaleTurquoise', 'PaleTurquoise4', 'PaleTurquoise1', 'PaleTurquoise3', 'PaleTurquoise2', 'DarkSlateGray',
    'DarkSlateGray2', 'DarkSlateGray1', 'DarkSlateGray4', 'DarkSlateGray3', 'Cyan4', 'DarkCyan', 'Cyan3', 'Cyan2',
    'Cyan', 'Cyan1', 'DarkTurquoise', 'CadetBlue', 'Turquoise4', 'Turquoise3', 'Turquoise2', 'Turquoise1', 'CadetBlue4',
    'CadetBlue2', 'CadetBlue1', 'CadetBlue3', 'PowderBlue', 'LightBlue4', 'LightBlue', 'DeepSkyBlue3', 'LightBlue1',
    'LightBlue2', 'DeepSkyBlue4', 'DeepSkyBlue2', 'DeepSkyBlue', 'DeepSkyBlue1', 'LightBlue3', 'SkyBlue', 'LightSkyBlue3',
    'LightSkyBlue2', 'LightSkyBlue1', 'LightSkyBlue4', 'LightSkyBlue', 'SkyBlue3', 'SkyBlue1', 'SkyBlue2', 'SkyBlue4',
    'SteelBlue2', 'SteelBlue3', 'SteelBlue', 'SteelBlue1', 'SteelBlue4', 'AliceBlue', 'DodgerBlue3', 'DodgerBlue',
    'DodgerBlue1', 'DodgerBlue2', 'DodgerBlue4', 'SlateGrey', 'LightSlateGray', 'SlateGray3', 'SlateGray1', 'SlateGray2',
    'SlateGray4', 'LightSteelBlue4', 'LightSteelBlue3', 'LightSteelBlue2', 'LightSteelBlue', 'LightSteelBlue1',
    'CornflowerBlue', 'RoyalBlue3', 'RoyalBlue2', 'RoyalBlue1', 'RoyalBlue', 'RoyalBlue4', 'GhostWhite', 'Lavender',
    'MidnightBlue', 'NavyBlue', 'Blue4', 'DarkBlue', 'MediumBlue', 'Blue3', 'Blue2', 'Blue', 'Blue1', 'SlateBlue',
    'SlateBlue1', 'SlateBlue3', 'LightSlateBlue', 'SlateBlue2', 'SlateBlue4', 'DarkSlateBlue', 'MediumSlateBlue',
    'MediumPurple4', 'MediumPurple2', 'MediumPurple', 'MediumPurple3', 'MediumPurple1', 'Purple1', 'BlueViolet',
    'Purple2', 'Purple4', 'Purple3', 'Purple', 'DarkOrchid4', 'DarkOrchid2', 'DarkOrchid', 'DarkOrchid1', 'DarkOrchid3',
    'DarkViolet', 'MediumOrchid3', 'MediumOrchid1', 'MediumOrchid2', 'MediumOrchid4', 'MediumOrchid', 'Thistle4',
    'Thistle', 'Thistle3', 'Thistle2', 'Thistle1', 'Plum4', 'Plum1', 'Plum3', 'Plum2', 'DarkGoldenrod4', 'Plum', 'Violet',
    'Magenta4', 'DarkMagenta', 'Magenta3', 'Magenta2', 'Magenta', 'Magenta1', 'Orchid4', 'Orchid', 'Orchid1', 'Orchid3',
    'Orchid2', 'VioletRed', 'Maroon4', 'MediumVioletRed', 'Maroon3', 'Maroon2', 'Maroon1', 'DeepPink4', 'DeepPink2',
    'DeepPink3', 'DeepPink', 'DeepPink1', 'HotPink', 'HotPink4', 'HotPink1', 'HotPink2', 'VioletRed4', 'VioletRed1',
    'VioletRed2', 'VioletRed3', 'HotPink3', 'LavenderBlush4', 'Maroon', 'LavenderBlush2', 'LavenderBlush3',
    'LavenderBlush', 'LavenderBlush1', 'PaleVioletRed1', 'PaleVioletRed', 'PaleVioletRed3', 'PaleVioletRed2',
    'PaleVioletRed4', 'Pink4', 'Pink2', 'Pink1', 'Pink3', 'Pink', 'LightPink', 'LightPink2', 'LightPink3', 'LightPink4',
    'LightPink1'
  );

procedure TNetscapeColorsCpWidget.AddDefaultColors;

// From "HYPE's Color Specifier for Netscape v.3" at http://users.rcn.com/giant.interport/COLOR/1ColorSpecifier.html.

var
  I: Integer;

begin
  for I := 0 to 454 do
    AddColor(NetscapeColorNames[I], NetscapeColors[I]);
end;

//----------------------------------------------------------------------------------------------------------------------

class function TNetscapeColorsCpWidget.GetDescription: string;

begin
  Result := SNetscapeColorsCpWidgetDescription;
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TNetscapeColorsCpWidget.GetPreviewColors(var Colors: PPreviewColors; var Count: Integer);

begin
  Colors := @NetscapeColors;
  Count := 455;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TNetscapeColorsCpWidget.GetSummary: string;

begin
  Result := SNetscapeColorsCpWidgetSummary;
end;

//----------------- TNamedHTMLColorsCpWidget ---------------------------------------------------------------------------

// Note: this list of colors is recognized by at least two browsers (IE and Netscape). Since the names are directly
// usable in HTML code there is no meaning to localize them. Hence resource strings aren't used in this widget.

const
  NamedHTMLColors: array[0..137] of COLORREF = (
    $FAFAFF, $FFF8F0, $FFFFF0, $F0FAFF, $FFF8F8, $FAFFF5, $F5F0FF, $FAE6E6, $F0FFF0, $E6F5FD, $EEF5FF, $E6F0FA, $F0FFFF,
    $E0FFFF, $CDFAFF, $DCF8FF, $D5EFFF, $CDEBFF, $DCF5F5, $ADDEFF, $C4E4FF, $D7EBFA, $B5E4FF, $B9DAFF, $B3DEF5, $87B8DE,
    $8CB4D2, $00FFFF, $00D7FF, $00A5FF, $008CFF, $4763FF, $0045FF, $0000FF, $3C14DC, $2222B2, $00008B, $2A2AA5, $13458B,
    $000080, $1E69D2, $2D52A0, $5C5CCD, $60A4F4, $3F85CD, $7280FA, $7AA0FF, $7A96E9, $507FFF, $8080F0, $8F8FBC, $AAE8EE,
    $20A5DA, $0B86B8, $D2FAFA, $8CE6F0, $6BB7BD, $008080, $238E6B, $2F6B55, $228B22, $008000, $006400, $2FFFAD, $32CD9A,
    $32CD32, $00FF7F, $00FC7C, $00FF00, $90EE90, $98FB98, $7FFF00, $D4FF7F, $AACD66, $9AFA00, $578B2E, $71B33C, $8FBC8F,
    $AAB220, $CCD148, $D1CE00, $EEEEAF, $D0E040, $A09E5F, $FFFFE0, $FFFF00, $8B8B00, $E6D8AD, $E6E0B0, $808000, $FACE87,
    $EBCE87, $FFBF00, $ED9564, $E16941, $FF901E, $8B0000, $CD0000, $FF0000, $82004B, $701919, $800000, $D30094, $E22B8A,
    $EE82EE, $CC3299, $D355BA, $D670DA, $FF00FF, $DB7093, $DDA0DD, $8B008B, $800080, $D8BFD8, $8515C7, $9314FF, $B469FF,
    $9370DB, $C1B6FF, $CBC0FF, $DEC4B0, $B48246, $E1E4FF, $EE687B, $CD5A6A, $8B3D48, $998877, $908070, $4F4F2F, $FFFFFF,
    $F5F5F5, $DCDCDC, $D3D3D3, $C0C0C0, $A9A9A9, $808080, $696969, $000000
  );

  NamedHTMLColorNames: array[0..137] of string = (
    'snow', 'aliceblue', 'azure', 'floralwhite', 'ghostwhite', 'mintcream', 'lavenderblush', 'lavender', 'honeydew',
    'oldlace', 'seashell', 'linen', 'ivory', 'lightyellow', 'lemonchiffon', 'cornsilk', 'papayawhip', 'blanchedalmond',
    'beige', 'navajowhite', 'bisque', 'antiquewhite', 'moccasin', 'peachpuff', 'wheat', 'burlywood', 'tan', 'yellow',
    'gold', 'orange', 'darkorange', 'tomato', 'orangered', 'red', 'crimson', 'firebrick', 'darkred', 'brown',
    'saddlebrown', 'maroon', 'chocolate', 'sienna', 'indianred', 'sandybrown', 'peru', 'salmon', 'lightsalmon',
    'darksalmon', 'coral', 'lightcoral', 'rosybrown', 'palegoldenrod', 'goldenrod', 'darkgoldenrod',
    'lightgoldenrodyellow', 'khaki', 'darkkhaki', 'olive', 'olivedrab', 'darkolivegreen', 'forestgreen', 'green',
    'darkgreen', 'greenyellow', 'yellowgreen', 'limegreen', 'chartreuse', 'lawngreen', 'lime', 'lightgreen', 'palegreen',
    'springgreen', 'aquamarine', 'mediumaquamarine', 'mediumspringgreen', 'seagreen', 'mediumseagreen', 'darkseagreen',
    'lightseagreen', 'mediumturquoise', 'darkturquoise', 'paleturquoise', 'turquoise', 'cadetblue', 'lightcyan', 'aqua',
    'darkcyan', 'lightblue', 'powderblue', 'teal', 'lightskyblue', 'skyblue', 'deepskyblue', 'cornflowerblue',
    'royalblue', 'dodgerblue', 'darkblue', 'mediumblue', 'blue', 'indigo', 'midnightblue', 'navy', 'darkviolet',
    'blueviolet', 'violet', 'darkorchid', 'mediumorchid', 'orchid', 'fuchsia', 'mediumpurple', 'plum', 'darkmagenta',
    'purple', 'thistle', 'mediumvioletred', 'deeppink', 'hotpink', 'palevioletred', 'lightpink', 'pink',
    'lightsteelblue', 'steelblue', 'mistyrose', 'mediumslateblue', 'slateblue', 'darkslateblue', 'lightslategray',
    'slategray', 'darkslategray', 'white', 'whitesmoke', 'gainsboro', 'lightgrey', 'silver', 'darkgray', 'gray',
    'dimgray', 'black'
  );

procedure TNamedHTMLColorsCpWidget.AddDefaultColors;

var
  I: Integer;

begin
  for I := 0 to 137 do
    AddColor(NamedHTMLColorNames[I], NamedHTMLColors[I]);
end;

//----------------------------------------------------------------------------------------------------------------------

class function TNamedHTMLColorsCpWidget.GetDescription: string;

begin
  Result := SNamedHTMLColorsCpWidgetDescription;
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TNamedHTMLColorsCpWidget.GetPreviewColors(var Colors: PPreviewColors; var Count: Integer);

begin
  Colors := @NamedHTMLColors;
  Count := 138;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TNamedHTMLColorsCpWidget.GetSummary: string;

begin
  Result := SNamedHTMLColorsCpWidgetSummary;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  RegisterColorPickerWidget(TNetscapeColorsCpWidget);
  RegisterColorPickerWidget(TNamedHTMLColorsCpWidget);
end.

