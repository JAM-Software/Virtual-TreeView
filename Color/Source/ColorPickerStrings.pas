unit ColorPickerStrings;

//----------------------------------------------------------------------------------------------------------------------
// ColorPickerStrings contains common localizable strings for the Color Picker component and color widgets.
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
// The original code is ColorPickerStrings.pas, released 1. September 2003.
//
// The initial developer of the original code is:
//   Dipl. Ing. Mike Lischke, Delphi Gems software solutions (public@delphi-gems.com, www.delphi-gems.com).
//
// Portions created by Delphi Gems are
//   (C) 1999-2003 Delphi Gems. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------

interface

resourcestring
  SUnsupportedColorFormat = 'Color format "%s" is not supported.';
  SUnknownColorFormat = 'Cannot create conversion profile. The color format has not been set yet.';
  SCouldNotCreateXYZConverter = 'Could not create CIE XYZ converter.';
  SColor2StringInvalidFormat = 'Color conversion from string: source format is wrong.';

  // Strings for DefaultWidgets.pas
  SOfficeCpWidgetDescription = 'Office 2000 predefined colors.';
  SOfficeCPWidgetSummary = 'Office 2000 colors';
  SSystemColorsCpWidgetDescription = 'Windows predefined system colors, like window background, button face color etc.';
  SSystemColorsCpWidgetSummary = 'Windows system colors';
  SDelphiColorsCpWidgetDescription = 'Delphi predefined colors, like clFuchsia. This set includes the predefined ' +
    'Windows system colors.';
  SDelphiColorsCpWidgetSummary = 'Delphi/BCB colors';
  SWindowsXPIconColorsCpWidgetDescription = 'Primary colors used in Windows XP icons.';
  SWindowsXPIconColorsCpWidgetSummary = 'Windows XP icon colors';
  SColorCompareCpWidgetDescription = 'Color picker, which takes a current color and an alternative color and displays ' +
    'both side by side to allow a visual comparation.';
  SColorCompareCpWidgetSummary = 'Two field visual color comparation tool';

  // Strings for ColorSwatchWidgets.pas
  SColorSwatchWidgetDescription = 'Color swatch definitions as stored in *.aco files and used e.g. by Photoshop.';
  SColorSwatchWidgetSummary = 'Color swatch file';

  // Strings for WebWidgets.pas
  SNetscapeColorsCpWidgetDescription = 'HYPE''s set of 455 predefined colors for Netscape v.3';
  SNetscapeColorsCpWidgetSummary = 'HYPE''s Netscape colors';
  SNamedHTMLColorsCpWidgetDescription = 'Colors used in HTML encoding. The color names are not part of the offical ' +
    'HTML specification but are recognized by the major browsers. That is, at least Internet Explorer and Netscape Navigator.';
  SNamedHTMLColorsCpWidgetSummary = 'Named HTML colors';

  // Strings for HSLWidgets.pas.
  SHLSCircleWidgetDescription = 'Classical color picker used for the HLS model (Hue, Lightness, Saturation) ' +
    'maintaining a color circle for hue and saturation. You need a separate gray ramp or similar widget to let the user' +
    ' adjust the luminance.';
  SHLSCircleWidgetSummary = 'HLS color circle';
  SHLSCombWidgetDescription = 'Color picker based on the HSL model but offering only a limited number of colors to' +
    ' select';
  SHLSCombWidgetSummary = 'HLS color comb';

  // Error messages.
  SInvalidSwatchFile = 'Cannot load color swatch file. Version does not match.';
  SInvalidColorScheme = 'Cannot convert color. Invalid color scheme found.';
  SWrongColorName = '''%s'' is not a valid color name.';

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
 
