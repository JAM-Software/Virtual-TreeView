unit ColorTypes;

//----------------------------------------------------------------------------------------------------------------------
//
// ColorTools contains general purpose functions for handling colors and requires Delphi 6 or better to compile.
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
// The original code is ColorTypes.pas, released October 1, 2004.
//
// The initial developer of the original code is:
//   Mike Lischke, Delphi Gems software solutions (support@delphi-gems.com, www.delphi-gems.com).
//
// Portions created by Delphi Gems are
//   (C) 1999-2004 Delphi Gems. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------

interface

type
  // Format neutral storage of a color. Can be anything.
  TColorComponents = array of Double;

  // RGB (red, green, blue) color given in the range [0..1] per component.
  TRGB = record
    R, G, B: Double;
  end;

  // RGB color with alpha given in the range [0..1] per component.
  TRGBA = record
    R, G, B, A: Double;
  end;

  // CMYK (cyan, magenta, yellow, black) color given in the range [0..1] per component.
  TCMYK = record
    C, M, Y, K: Double;
  end;

  // Hue, luminance, saturation color with all three components in the range [0..1]
  // (so hue's 0..360° is normalized to 0..1).
  THLS = record
    H, L, S: Double;
  end;

  // CIE L*a*b* color with L in [0..100], a and b in [-127..128].
  TLab = record
    L, a, b: Double;
  end;

  PCIELab = ^TCIELab;
  TCIELab = TLab;
  
  // CIE XYZ color model, which is an abstract and deviced-independent system to convert between different
  // other color models (e.g. from RGB to CIE L*a*b*).
  TXYZ = record
    X, Y, Z: Double;
  end;

  TCIEXYZ = TXYZ;
  PCIEXYZ = ^TCIEXYZ;

  TxyY = record
    x, y, YY: Double;
  end;

  TCIExyY = TxyY;
  PCIExyY = ^TCIExyY;
  
  TLCh = record
    L, C, h: Double;
  end;

  TCIELCh = TLCh;
  PCIELCh = ^TCIELCh;
  
  PJCh  = ^TJCh;
  TJCh   = record
    J, C, h: Double;
  end;

  // Transformation matrix for linear conversions between color schemes (e.g. CIE XYZ to RGB) and for general
  // color manipulations.
  TColorMatrix = array[0..2, 0..2] of Double;

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.

