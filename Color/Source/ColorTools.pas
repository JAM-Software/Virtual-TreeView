unit ColorTools;

//----------------------------------------------------------------------------------------------------------------------
//
// ColorTools contains general purpose functions for handling colors and requires Delphi 6 or better to compile.
// The calculations contained here are optimized for precision, not for speed, while still keeping complexity at a
// reasonable level. Unless otherwise specified all XYZ coordinates are given for a 2° observer. All code here works
// without CMS support but has limits with regard to precision. For highest quality color management support
// include ColorManagement.pas in your project, which will then include Marti Maria's little CMS (www.littlecms.com).
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
// The original code is ColorTools.pas, released 1. June 2003.
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
  Windows, Classes, Graphics, ColorTypes;

type
  // Color formats, which can be handled by the library.
  TColorFormat = (
    cfUnknown,
    cfGray,       // Grayscale.
    cfRGB,        // Red, green, blue.
    cfCMY,        // Cyan, magenta, yellow
    cfCMYK,       // CMY with black
    cfCIELab,     // CIE color format using luminance and chromaticities
    cfYCbCr,      // Another format using luminance and chromaticities.
    cfXYZ,
    cfYUVK,
    cfYUV,
    cfHSV,
    cfHLS,
    cfYxy
  );

  // CIE XYZ color primaries for various "phosphors".
  TPrimaries = (
    primTrinitron,   // Apple RGB
    primCCIR6011,    // SMPTE-C (CCIR 601-1)
    primHDTV,        // sRGB
    primEBUITU,      // EBU/ITU (Pal/Secam)
    primP22EBU,      // P22-EBU (Color Match RGB)
    primAdobeRGB,    // Adobe RGB (1998)
    primNTSC,        // NTSC (1953)
    primCIERGB,      // CIE RGB
    primWideGamutRGB // 700/525/450nm
  );

  TPrimariesValues = record
    Red, Green, Blue: TXYZ;
  end;

  // Reference white points for various illuminants. They are used when converting from and to CIE XYZ.
  TWhitePoint = (
    wpA,           // Illuminant A (CIE) – Incandescent illumination, yellow-orange in color, with a correlated color
                   // temperature of 2856K. It is defined in the wavelength range of 380 to 770nm.
    wpB,           // Illuminant B (CIE) – Direct sunlight.
    wpC,           // Illuminant C (CIE) – Tungsten illumination that simulates average daylight, bluish in color,
                   // with a correlated color temperature of 6774K.
    wpD50,         // Illuminants D (CIE) – Daylight illuminants, defined from 300 to 830nm (the UV portion 300 to 380nm
                   // being necessary to correctly describe colors that contain fluorescent dyes or pigments).
                   // They are designated as D, with a subscript to describe the correlated color temperature; D65 is
                   // the most commonly used, having a correlated color temperature of 6504K, close to that
                   // of illuminant C. They are based on actual measurements of the spectral distribution of daylight.
    wpD55,         // Illuminant D55 (CIE) – Cloudy daylight.
    wpD60,
    wpD65,
    wpD75,
    wpE,           // Illuminant E (CIE) - Normalized reference source.
    wpF2,          // Illuminant F2 (CIE) - Fluorescent illuminant.
    wpF7,
    wpF11,
    wp9300K        // Illuminant with a color temperature of 9300K. Default of some old or low-quality CRT monitors,
                   // very bad for imaging.
  );

  // An ink color describes the CIE XYZ corrdinates of a certain CMYK color component, either pure or overprinted
  // in various combinations. Such an ink color is part of a set of ink colors predefined for certain
  // print media (e.g. AD-LITHO, Eurostandard uncoated) etc. and are the same as those values used in Adobe Photoshop.
  TInkColor = (
    icC,      // Pure cyan.
    icM,      // Pure magenta.
    icY,      // Pure yellow.
    icMY,     // Magenta-yellow overprint, aka pure red.
    icCY,     // Cyan-yellow overprint, aka pure green.
    icCM,     // Cyan-magenta overprint, aka pure blue.
    icCMY,    // Cyan-magenta-yellow overprint, aka pure black from the 3 inks.
    icW,      // White (paper).
    icK       // Black ink.
  );

  // Some predefined ink colors (same as in Adobe Photoshop).
  TInkColorSet = (
    icsADLithoNewsprint,               // AD-LITHO (Newsprint)
    icsDainipponInk,                   // Dainippon Ink
    icsEurostandardCoated,             // Eurostandard (Coated)
    icsEurostandardNewsPrint,          // Eurostandard (Newsprint)
    icsEurostandardUncoated,           // Eurostandard (Uncoated)
    icsSWOPCoated,                     // SWOP (Coated)
    icsSWOPNewsprint,                  // SWOP (Newsprint)
    icsSWOPUncoated,                   // SWOP (Uncoated)
    icsToyoInksCoatedWebOffset,        // Toyo Inks (Coated Web Office)
    icsToyoInksCoated,                 // Toyo Inks (Coated)
    icsToyoInksDullCoated,             // Toyo Inks (Dull coated)
    icsToyoInksUncoated                // Toyo Inks (Uncoated)
  );

  TInkColors = array[TInkColor] of TXYZ;
  TInkColorsRGB = array[TInkColor] of TRGB;

const
  ColorFormatStrings: array[TColorFormat] of string = (
    'Unknown',
    'Grayscale',
    'RGB',
    'CMY',
    'CMYK',
    'CIE L*a*b*',
    'YCbCr',
    'CIE XYZ',
    'Yu''v''K',
    'Yu''v''',
    'HSV',
    'HLS',
    'CIE xyY' 
  );

  ComponentNames: array[TColorFormat] of string = (
    'Unknown',
    'G',
    'R, G, B',
    'C, M, Y',
    'C, M, Y, K',
    'L*, a*, b*',
    'Y, Cb, Cr',
    'X, Y, Z',
    'Y, u'', v'', K',
    'Y, u'', v''',
    'H, S, V',
    'H, L, S',
    'x, y, Y'
  );

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

  // Support for CIE XYZ conversions.
  WhitePoints: array[TWhitePoint] of TXYZ = (
    (X: 1.0986; Y: 1.0000; Z: 0.3559), // wpA
    (X: 0.9908; Y: 1.0000; Z: 0.8532), // wpB
    (X: 0.9810; Y: 1.0000; Z: 1.1835), // wpC
    (X: 0.9642; Y: 1.0000; Z: 0.8251), // wpD50
    (X: 0.9568; Y: 1.0000; Z: 0.9217), // wpD55
    (X: 0.9539; Y: 1.0000; Z: 1.0030), // wpD60
    (X: 0.9504; Y: 1.0000; Z: 1.0890), // wpD65
    (X: 0.9495; Y: 1.0000; Z: 1.2261), // wpD75
    (X: 1.0000; Y: 1.0000; Z: 1.0000), // wpE
    (X: 0.9918; Y: 1.0000; Z: 0.6739), // wpF2
    (X: 0.9504; Y: 1.0000; Z: 1.0876), // wpF7
    (X: 1.0097; Y: 1.0000; Z: 0.6437), // wpF11
    (X: 0.9714; Y: 1.0000; Z: 1.4393)  // wp9300K
  );

  // XYZ values for the various primaries.
  PrimariesValues: array[TPrimaries] of TPrimariesValues = (
    (Red: (X: 1.838235294; Y: 1; Z: 0.10294); Green: (X: 0.47059; Y: 1; Z: 0.21008); Blue: (X:  2.21429; Y: 1; Z: 11.07142857)), // Trinitron
    (Red: (X: 1.852941176; Y: 1; Z: 0.08824); Green: (X: 0.52101; Y: 1; Z: 0.15966); Blue: (X:  2.21429; Y: 1; Z: 11.07142857)), // SMPTE-C (CCIR 601-1)
    (Red: (X: 1.939393939; Y: 1; Z: 0.09091); Green: (X: 0.50000; Y: 1; Z: 0.16667); Blue: (X:  2.50000; Y: 1; Z: 13.16666667)), // HDTV
    (Red: (X: 1.939393939; Y: 1; Z: 0.09091); Green: (X: 0.48333; Y: 1; Z: 0.18333); Blue: (X:  2.50000; Y: 1; Z: 13.16666667)), // EBU/ITU
    (Red: (X: 1.852941176; Y: 1; Z: 0.08824); Green: (X: 0.48760; Y: 1; Z: 0.16529); Blue: (X:  2.01299; Y: 1; Z:  9.97402597)), // P22-EBU
    (Red: (X: 1.939393939; Y: 1; Z: 0.09091); Green: (X: 0.29577; Y: 1; Z: 0.11268); Blue: (X:  2.50000; Y: 1; Z: 13.16666667)), // Adobe RGB (1998)
    (Red: (X: 2.030303030; Y: 1; Z: 0.00000); Green: (X: 0.29577; Y: 1; Z: 0.11268); Blue: (X:  1.75000; Y: 1; Z:  9.75000000)), // NTSC (1953)
    (Red: (X: 2.773584906; Y: 1; Z: 0.00000); Green: (X: 0.38215; Y: 1; Z: 0.01255); Blue: (X: 18.55560; Y: 1; Z: 91.55555556)), // CIE RGB
    (Red: (X: 2.769317753; Y: 1; Z: 0.00000); Green: (X: 0.13940; Y: 1; Z: 0.07067); Blue: (X:  8.84746; Y: 1; Z: 46.64971751))  // Wide Gamut RGB
  );

  // Support for CMYK conversions without color management help.
  // The ink colors below are used in the Yule-Nielsen Modified Neugebauer model
  // to compute a CMYK-to-RGB conversion analytically.
  InkColorSets: array[TInkColorSet] of TInkColors = (
    ( // icsADLithoNewsprint
      (X: 0.2337892; Y: 0.2964000; Z: 0.5238649),   // icC
      (X: 0.3302363; Y: 0.2469000; Z: 0.2864524),   // icM
      (X: 0.4736773; Y: 0.5346000; Z: 0.1433815),   // icY
      (X: 0.2696947; Y: 0.1907000; Z: 0.0959063),   // icMY
      (X: 0.1495673; Y: 0.2395000; Z: 0.1452924),   // icCY
      (X: 0.1323682; Y: 0.1260000; Z: 0.2470469),   // icCM
      (X: 0.1003919; Y: 0.1081000; Z: 0.1035750),   // icCMY
      (X: 0.5726126; Y: 0.6060000; Z: 0.6164111),   // icW
      (X: 0.0657882; Y: 0.0691000; Z: 0.0682873)    // icK
    ),
    ( // icsDainipponInk
      (X: 0.1432582; Y: 0.2206000; Z: 0.5150263),   // icC
      (X: 0.3287591; Y: 0.1647000; Z: 0.1473969),   // icM
      (X: 0.6782831; Y: 0.7282000; Z: 0.0616621),   // icY
      (X: 0.3116207; Y: 0.1643000; Z: 0.0234714),   // icMY
      (X: 0.0755371; Y: 0.1809000; Z: 0.0677565),   // icCY
      (X: 0.4810000; Y: 0.0333000; Z: 0.1498500),   // icCM
      (X: 0.0319366; Y: 0.0337000; Z: 0.0323285),   // icCMY
      (X: 0.8187928; Y: 0.8446000; Z: 0.6827183),   // icW
      (X: 0.2263990; Y: 0.0227000; Z: 0.0147130)    // icK
    ),
    ( // icsEurostandardCoated
      (X: 0.1816082; Y: 0.2380000; Z: 0.6904664),   // icC
      (X: 0.3111163; Y: 0.1711000; Z: 0.2147288),   // icM
      (X: 0.6435414; Y: 0.7333000; Z: 0.1018842),   // icY
      (X: 0.2748426; Y: 0.1598000; Z: 0.0425147),   // icMY
      (X: 0.0698038; Y: 0.1643000; Z: 0.0626809),   // icCY
      (X: 0.0614112; Y: 0.0428000; Z: 0.1847827),   // icCM
      (X: 0.0292002; Y: 0.0316000; Z: 0.0249063),   // icCMY
      (X: 0.8214599; Y: 0.8624000; Z: 0.9745864),   // icW
      (X: 0.0162992; Y: 0.0174000; Z: 0.0207950)    // icK
    ),
    ( // icsEurostandardNewsPrint
      (X: 0.2047723; Y: 0.2646000; Z: 0.5146367),   // icC
      (X: 0.3097918; Y: 0.2106000; Z: 0.2479410),   // icM
      (X: 0.5126939; Y: 0.5799000; Z: 0.1428130),   // icY
      (X: 0.2731338; Y: 0.1954000; Z: 0.0923158),   // icMY
      (X: 0.1353116; Y: 0.2209000; Z: 0.1317495),   // icCY
      (X: 0.1145588; Y: 0.1028000; Z: 0.2280485),   // icCM
      (X: 0.0853078; Y: 0.0939000; Z: 0.0876293),   // icCMY
      (X: 0.6165159; Y: 0.6550000; Z: 0.6526910),   // icW
      (X: 0.0469007; Y: 0.0498000; Z: 0.0497268)    // icK
    ),
    ( // icsEurostandardUncoated
      (X: 0.2362325; Y: 0.2856000; Z: 0.6971175),   // icC
      (X: 0.3294105; Y: 0.2060000; Z: 0.2541640),   // icM
      (X: 0.6466524; Y: 0.7276000; Z: 0.1735034),   // icY
      (X: 0.2845871; Y: 0.1839000; Z: 0.0933828),   // icMY
      (X: 0.1313217; Y: 0.2175000; Z: 0.1522149),   // icCY
      (X: 0.1105097; Y: 0.0951000; Z: 0.2346681),   // icCM
      (X: 0.0746042; Y: 0.0767000; Z: 0.0895877),   // icCMY
      (X: 0.8140339; Y: 0.8459000; Z: 0.9736277),   // icW
      (X: 0.0625035; Y: 0.0647000; Z: 0.0689166)    // icK
    ),
    ( // icsSWOPCoated
      (X: 0.1886437; Y: 0.2625000; Z: 0.6764336),   // icC
      (X: 0.2932074; Y: 0.1450000; Z: 0.1669679),   // icM
      (X: 0.6188278; Y: 0.7120000; Z: 0.0894794),   // icY
      (X: 0.2682443; Y: 0.1409000; Z: 0.0324101),   // icMY
      (X: 0.0792976; Y: 0.1925000; Z: 0.0773771),   // icCY
      (X: 0.0491161; Y: 0.0298000; Z: 0.1604413),   // icCM
      (X: 0.0303961; Y: 0.0279000; Z: 0.0358970),   // icCMY
      (X: 0.7872026; Y: 0.8302000; Z: 0.8824469),   // icW
      (X: 0.0081013; Y: 0.0082000; Z: 0.0089995)    // icK
    ),
    ( // icsSWOPNewsprint
      (X: 0.1825833; Y: 0.2459000; Z: 0.5040157),   // icC
      (X: 0.2744340; Y: 0.1658000; Z: 0.1791160),   // icM
      (X: 0.5275775; Y: 0.5870000; Z: 0.1311781),   // icY
      (X: 0.2500002; Y: 0.1547000; Z: 0.0778325),   // icMY
      (X: 0.1155489; Y: 0.2105000; Z: 0.1306662),   // icCY
      (X: 0.0855147; Y: 0.0748000; Z: 0.1776880),   // icCM
      (X: 0.0626148; Y: 0.0633000; Z: 0.0756134),   // icCMY
      (X: 0.6096216; Y: 0.6445000; Z: 0.6210602),   // icW
      (X: 0.0389047; Y: 0.0403000; Z: 0.0464971)    // icK
    ),
    ( // icsSWOPUncoated
      (X: 0.1886437; Y: 0.2625000; Z: 0.6764336),   // icC
      (X: 0.2932074; Y: 0.1450000; Z: 0.1669679),   // icM
      (X: 0.6188278; Y: 0.7120000; Z: 0.0894794),   // icY
      (X: 0.2682443; Y: 0.1409000; Z: 0.0324101),   // icMY
      (X: 0.0792976; Y: 0.1925000; Z: 0.0773771),   // icCY
      (X: 0.0491161; Y: 0.0298000; Z: 0.1604413),   // icCM
      (X: 0.0303961; Y: 0.0279000; Z: 0.0358970),   // icCMY
      (X: 0.7872026; Y: 0.8302000; Z: 0.8824469),   // icW
      (X: 0.0081013; Y: 0.0082000; Z: 0.0089995)    // icK
    ),
    ( // icsToyoInksCoatedWebOffset
      (X: 0.1291621; Y: 0.2035000; Z: 0.4416910),   // icC
      (X: 0.3025505; Y: 0.1579000; Z: 0.1473247),   // icM
      (X: 0.6071853; Y: 0.6567000; Z: 0.0635859),   // icY
      (X: 0.2879861; Y: 0.1585000; Z: 0.0214796),   // icMY
      (X: 0.0623987; Y: 0.1563000; Z: 0.0569140),   // icCY
      (X: 0.0466041; Y: 0.0376000; Z: 0.1521252),   // icCM
      (X: 0.0252940; Y: 0.0306000; Z: 0.0264969),   // icCMY
      (X: 0.7227407; Y: 0.7496000; Z: 0.5779875),   // icW
      (X: 0.0190025; Y: 0.0194000; Z: 0.0146030)    // icK
    ),
    ( // icsToyoInksCoated
      (X: 0.1442180; Y: 0.2222000; Z: 0.5167298),   // icC
      (X: 0.3214524; Y: 0.1590000; Z: 0.1472617),   // icM
      (X: 0.6644957; Y: 0.7146000; Z: 0.0648324),   // icY
      (X: 0.3178042; Y: 0.1738000; Z: 0.0212314),   // icMY
      (X: 0.0747102; Y: 0.1824000; Z: 0.0694204),   // icCY
      (X: 0.0538125; Y: 0.0397000; Z: 0.1784053),   // icCM
      (X: 0.0271978; Y: 0.0304000; Z: 0.0277955),   // icCMY
      (X: 0.8192851; Y: 0.8456000; Z: 0.6846564),   // icW
      (X: 0.0146001; Y: 0.0150000; Z: 0.0112051)    // icK
    ),
    ( // icsToyoInksDullCoated
      (X: 0.1587067; Y: 0.2400000; Z: 0.5250808),   // icC
      (X: 0.3369794; Y: 0.1718000; Z: 0.1599633),   // icM
      (X: 0.6812333; Y: 0.7318000; Z: 0.0786096),   // icY
      (X: 0.3186991; Y: 0.1703000; Z: 0.0275311),   // icMY
      (X: 0.0835147; Y: 0.1910000; Z: 0.0728843),   // icCY
      (X: 0.0549003; Y: 0.0403000; Z: 0.1641304),   // icCM
      (X: 0.0333022; Y: 0.0339000; Z: 0.0341128),   // icCMY
      (X: 0.8167748; Y: 0.8372000; Z: 0.6937538),   // icW
      (X: 0.0243043; Y: 0.0248000; Z: 0.0188037)    // icK
    ),
    ( // icsToyoInksUncoated
      (X: 0.2498113; Y: 0.3311000; Z: 0.5000550),   // icC
      (X: 0.3945188; Y: 0.2569000; Z: 0.2217967),   // icM
      (X: 0.6826369; Y: 0.7359000; Z: 0.1343223),   // icY
      (X: 0.3656157; Y: 0.2428000; Z: 0.0994560),   // icMY
      (X: 0.1837588; Y: 0.2852000; Z: 0.1439791),   // icCY
      (X: 0.1389863; Y: 0.1306000; Z: 0.2208381),   // icCM
      (X: 0.1136905; Y: 0.1196000; Z: 0.1024759),   // icCMY
      (X: 0.7723548; Y: 0.8045000; Z: 0.5951215),   // icW
      (X: 0.1410827; Y: 0.1444000; Z: 0.1047876)    // icK
    )
  );

  // Yule-Nielsen parameter used in the CMYK to RGB conversion. 
  YuleNielsenParameter = 1.4;

var
  // CMYK ink colors converted to RGB for use in color conversion, which do not use the color management system.
  InkColorSetsRGB: array[TInkColorSet] of TInkColorsRGB;

// Simple color convertion functions not using the color management system.
function CMYKToRGB(const CMYK: TCMYK; const InkSet: TInkColorSet = icsSWOPCoated): TRGB;
function HLSToRGB(const HLS: THLS): TRGB;
function RGBToHLS(const RGB: TRGB): THLS;
function RGBToXYZ(const RGB: TRGB; const Matrix: TColorMatrix): TXYZ;
function LabToXYZ(const Lab: TLab; WhitePoint: TWhitePoint = wpD65): TXYZ;
function XYZToRGB(const XYZ: TXYZ; const Matrix: TColorMatrix): TRGB;
function XYZToLab(const XYZ: TXYZ; WhitePoint: TWhitePoint = wpD65): TLab;

// Color utility functions.
function BrightenColor(const RGB: TRGB; Amount: Double): TRGB; overload;
function ColorInterpolation(const HLS1, HLS2: THLS; Ratio: Double): THLS; overload;
function ColorInterpolation(const RGB1, RGB2: TRGB; Ratio: Double): TRGB; overload;
function DarkenColor(const RGB: TRGB; Amount: Double): TRGB; overload;
procedure GammaCorrection(var RGB: TRGB; Gamma: Double);
function InvertColorMatrix(const M: TColorMatrix): TColorMatrix;
function SameColor(const Color1, Color2: TColorComponents; Delta: Double = 0.001): Boolean;

// Creation function for colors and matrices.
function MakeCMYK(const C, M, Y, K: Double): TCMYK;
function MakeColorRef(RGB: TRGB; Gamma: Double = 1): COLORREF; overload;
function MakeHLS(const H, L, S: Double): THLS; overload;
function MakeHLS(const Values: TColorComponents): THLS; overload;
function MakeLab(const L, a, b: Double): TLab;
function MakeRGB(const Color: TColor): TRGB; overload;
function MakeRGB(const R, G, B: Double): TRGB; overload;
function MakeRGB(const Values: TColorComponents): TRGB; overload;
function MakeRGB2XYZColorMatrix(Primaries: TPrimaries = primHDTV; WhitePoint: TWhitePoint = wpD65): TColorMatrix;
function MakeXYZ(const X, Y, Z: Double): TXYZ;
function MakeXYZ2RGBColorMatrix(Primaries: TPrimaries = primHDTV; WhitePoint: TWhitePoint = wpD65): TColorMatrix;

// Gamut functions.
function MakeSafeColor(var RGB: TRGB): Boolean;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Math, SysUtils;

const
  // Do not modify the copyright in any way! Usage of this unit is prohibited without the copyright notice
  // in the compiled binary file.
  Copyright: string = 'Color Tools © 2003 Mike Lischke, Delphi Gems software solutions';

//----------------- common color conversion functions ------------------------------------------------------------------

function CMYKToRGB(const CMYK: TCMYK; const InkSet: TInkColorSet): TRGB;

// Calculates an RGB triple from a CMYK color using the Yule-Nielsen Modified Neugebauer model and
// the given ink set. Default ink color set is SWOP coated, as used by default in Adobe Photoshop 5 and up.
// Although results are very close to the colors in Photoshop they are not exactly the same.
// Use color management functions to create exact results.
// All CMYK values must be in the range [0..100] (%).

var
  Cn, Cr,
  Mn, Mr,
  Yn, Yr,
  Kn, Kr: Double;

  Ap,
  Ac, Am, Ay,
  Ar, Ag, Ab,
  A3: Double;

  Rtemp, Gtemp, Btemp: Double;

begin
  with CMYK do
  begin
    Cn := C / 100;
    Cr := 1 - Cn;
    Mn := M / 100;
    Mr := 1 - Mn;
    Yn := Y / 100;
    Yr := 1 - Yn;
    Kn := K / 100;
    Kr := 1 - Kn;
  end;

  Ap := Cr * Mr * Yr * Kr;
  Ac := Cn * Mr * Yr * Kr;
  Am := Cr * Mn * Yr * Kr;
  Ay := Cr * Mr * Yn * Kr;
  Ar := Cr * Mn * Yn * Kr;
  Ag := Cn * Mr * Yn * Kr;
  Ab := Cn * Mn * Yr * Kr;
  A3 := Cn * Mn * Yn * Kr;

  // Red.
  // Note: the RGB values in the ink color sets are already raised to the power of the Yule-Nielsen parameter.
  Rtemp := Ap * InkColorSetsRGB[InkSet][icW].R + Ac * InkColorSetsRGB[InkSet][icC].R +
    Am * InkColorSetsRGB[InkSet][icM].R + Ay * InkColorSetsRGB[InkSet][icY].R +
    Ar * InkColorSetsRGB[InkSet][icMY].R + Ag * InkColorSetsRGB[InkSet][icCY].R +
    Ab * InkColorSetsRGB[InkSet][icCM].R + A3 * InkColorSetsRGB[InkSet][icCMY].R;
  Result.R := Power(Rtemp, YuleNielsenParameter);

  // Green.
  Gtemp := Ap * InkColorSetsRGB[InkSet][icW].G + Ac * InkColorSetsRGB[InkSet][icC].G +
    Am * InkColorSetsRGB[InkSet][icM].G + Ay * InkColorSetsRGB[InkSet][icY].G +
    Ar * InkColorSetsRGB[InkSet][icMY].G + Ag * InkColorSetsRGB[InkSet][icCY].G +
    Ab * InkColorSetsRGB[InkSet][icCM].G + A3 * InkColorSetsRGB[InkSet][icCMY].G;
  Result.G := Power(Gtemp, YuleNielsenParameter);

  // Blue.
  Btemp := Ap * InkColorSetsRGB[InkSet][icW].B + Ac * InkColorSetsRGB[InkSet][icC].B +
    Am * InkColorSetsRGB[InkSet][icM].B + Ay * InkColorSetsRGB[InkSet][icY].B +
    Ar * InkColorSetsRGB[InkSet][icMY].B + Ag * InkColorSetsRGB[InkSet][icCY].B +
    Ab * InkColorSetsRGB[InkSet][icCM].B + A3 * InkColorSetsRGB[InkSet][icCMY].B;
  Result.B := Power(Btemp, YuleNielsenParameter);
end;

//----------------------------------------------------------------------------------------------------------------------

function HLSToRGB(const HLS: THLS): TRGB;

// Converts from HLS (hue, luminance, saturation) to RGB.
// Input parameters and result values are all in the range 0..1.
// Note: Hue is normalized so 360° corresponds to 1.

  //--------------- local function --------------------------------------------

  function HueToRGB(m1, m2, Hue: Double): Double;

  begin
    if Hue > 1 then
      Hue := Hue - 1
    else
      if Hue < 0 then
        Hue := Hue + 1;

    if 6 * Hue < 1 then
      Result := m1 + (m2 - m1) * Hue * 6
    else
      if 2 * Hue < 1 then
        Result := m2
      else
        if 3 * Hue < 2 then                    
          Result := m1 + (m2 - m1) * (2 / 3 - Hue) * 6
        else
          Result := m1;
  end;

  //--------------- end local function ----------------------------------------

var
  m1, m2: Double;

begin
  with HLS, Result do
  begin
    if S = 0 then
    begin
      // Achromatic case (no hue).
      R := L;
      G := L;
      B := L;
    end
    else
    begin
      if L <= 0.5 then
        m2 := L * (S + 1)
      else
        m2 := L + S - L * S;
      m1 := 2 * L - m2;

      R := HueToRGB(m1, m2, H + 1 / 3);
      G := HueToRGB(m1, m2, H);
      B := HueToRGB(m1, m2, H - 1 / 3)
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function RGBToHLS(const RGB: TRGB): THLS;

// Converts from RGB to HLS.
// Input parameters and result values are all in the range 0..1.
// Note: Hue is normalized so 360° corresponds to 1.

var
  Delta,
  Max,
  Min:  Double;

begin
  with RGB, Result do
  begin
    Max := MaxValue([R, G, B]);
    Min := MinValue([R, G, B]);

    L := (Max + Min) / 2;

    if Max = Min then
    begin
      // Achromatic case.
      S := 0;
      H := 0; // Undefined
    end
    else
    begin
      Delta := Max - Min;

      if L < 0.5 then
        S := Delta / (Max + Min)
      else
        S := Delta / (2 - (Max + Min));

      if R = Max then
        H := (G - B) / Delta
      else
        if G = Max then
          H := 2 + (B - R) / Delta
        else
          if B = Max then
            H := 4 + (R - G) / Delta;

      H := H / 6;
      if H < 0 then
        H := H + 1;  
    end
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function RGBToXYZ(const RGB: TRGB; const Matrix: TColorMatrix): TXYZ;

// Conversion from RGB to CIE XYZ using the given matrix, which is built from a white point and a certain set of
// primaries.

begin
  with RGB, Result do
  begin
    X := Matrix[0, 0] * R + Matrix[0, 1] * G + Matrix[0, 2] * B;
    Y := Matrix[1, 0] * R + Matrix[1, 1] * G + Matrix[1, 2] * B;
    Z := Matrix[2, 0] * R + Matrix[2, 1] * G + Matrix[2, 2] * B;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function LabToXYZ(const Lab: TLab; WhitePoint: TWhitePoint): TXYZ;

// Converts the given L*a*b* color to CIE XYZ using the given white point (default is D65).
// L must be in the range [0..100] and a as well as b must be [-127..128].

const
  Factor = 16 / 116;
  
var
  X1, Y1, Z1: Double;

begin
  with Lab do
  begin
    Y1 := (L + 16) / 116;
    X1 := a / 500 + Y1;
    Z1 := -b / 200 + Y1;
  end;

  if X1 > 0.206893 then
    X1 := X1 * X1 * X1
  else
    X1 := (X1 - Factor) / 7.787;
  if Y1 > 0.206893 then
    Y1 := Y1 * Y1 * Y1
  else
    Y1 := (Y1 - Factor) / 7.787;
  if Z1 > 0.206893 then
    Z1 := Z1 * Z1 * Z1
  else
    Z1 := (Z1 - Factor) / 7.787;

  with WhitePoints[WhitePoint] do
  begin              
    Result.X := X * X1;
    Result.Y := Y * Y1;
    Result.Z := Z * Z1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function XYZToRGB(const XYZ: TXYZ; const Matrix: TColorMatrix): TRGB;

// Conversion from CIE XYZ to RGB using the given matrix, which is built from a white point and a certain set of
// primaries.

begin
  with XYZ, Result do
  begin
    R := Matrix[0, 0] * X + Matrix[0, 1] * Y + Matrix[0, 2] * Z;
    G := Matrix[1, 0] * X + Matrix[1, 1] * Y + Matrix[1, 2] * Z;
    B := Matrix[2, 0] * X + Matrix[2, 1] * Y + Matrix[2, 2] * Z;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function XYZToLab(const XYZ: TXYZ; WhitePoint: TWhitePoint): TLab;

// Converts the given CIE XYZ color to L*a*b* using the given white point (default is D65).
// XYZ components must be in the range [0..1].

const
  OneThird = 1 / 3;
  Factor = 16 / 116;

var
  X1, Y1, Z1: Double;

begin
  with WhitePoints[WhitePoint] do
  begin
    X1 := XYZ.X / X;
    Y1 := XYZ.Y / Y;
    Z1 := XYZ.Z / Z;
  end;

  if X1 > 0.008856 then
    X1 := Power(X1, OneThird)
  else
    X1 := 7.787 * X1 + Factor;
  if Y1 > 0.008856 then
    Y1 := Power(Y1, OneThird)
  else
    Y1 := 7.787 * Y1 + Factor;
  if Z1 > 0.008856 then
    Z1 := Power(Z1, OneThird)
  else
    Z1 := 7.787 * Z1 + Factor;

  with Result do
  begin
    L := 116 * Y1 - 16;
    a := 500 * (X1 - Y1);
    b := 200 * (Y1 - Z1);
  end;
end;

//----------------- Color utility functions ----------------------------------------------------------------------------

function BrightenColor(const RGB: TRGB; Amount: Double): TRGB;

var
  HLS: THLS;

begin
  HLS := RGBToHLS(RGB);
  HLS.L := (1 + Amount) * HLS.L;
  Result := HLSToRGB(HLS);
end;

//----------------------------------------------------------------------------------------------------------------------

function ColorInterpolation(const HLS1, HLS2: THLS; Ratio: Double): THLS;

// Interpolates linearly from HLS1 to HLS2 with the given ratio.
// Parameters as well as result are in the range 0..1.

begin
  if Ratio <= 0 then
    Result := HLS1
  else
    if Ratio >= 1 then
      Result := HLS2
    else
    begin
      Result.H := HLS1.H + (HLS2.H - HLS1.H) * Ratio;
      Result.L := HLS1.L + (HLS2.L - HLS1.L) * Ratio;
      Result.S := HLS1.S + (HLS2.S - HLS1.S) * Ratio;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function ColorInterpolation(const RGB1, RGB2: TRGB; Ratio: Double): TRGB;

// Interpolates linearly from RGB1 to RGB2 with the given ratio using the HLS color space
// which produces more natural results.
// Parameters as well as result are in the range 0..1.

var
  HLS1, HLS2: THLS;

begin
  if Ratio <= 0 then
    Result := RGB1
  else
    if Ratio >= 1 then
      Result := RGB2
    else
    begin
      HLS1 := RGBToHLS(RGB1);
      HLS2 := RGBToHLS(RGB2);
      HLS2.H := HLS1.H + (HLS1.H - HLS2.H) * Ratio;
      HLS2.L := HLS1.L + (HLS1.L - HLS2.L) * Ratio;
      HLS2.S := HLS1.S + (HLS1.S - HLS2.S) * Ratio;
      Result := HLSToRGB(HLS2);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function DarkenColor(const RGB: TRGB; Amount: Double): TRGB;

var
  HLS: THLS;

begin
  HLS := RGBToHLS(RGB);
  // Darken means to decrease luminance.
  HLS.L := (1 - Amount) * HLS.L;
  Result := HLSToRGB(HLS);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure GammaCorrection(var RGB: TRGB; Gamma: Double);

// Computes the gamma corrected RGB color and ensures the result is in-gamut.

begin
  if Gamma <> 1 then
  begin
    Gamma := 1 / Gamma;
    with RGB do
    begin
      if R > 0 then
        R := Power(R, Gamma)
      else
        R := 0;
      if G > 0 then
        G := Power(G, Gamma)
      else
        G := 0;
      if B > 0 then
        B := Power(B, Gamma)
      else
        B := 0;
    end;
  end;

  MakeSafeColor(RGB);
end;

//----------------------------------------------------------------------------------------------------------------------

function InvertColorMatrix(const M: TColorMatrix): TColorMatrix;

// Simple 3x3 matrix inversion.

var
  D: Double;
  Temp: TColorMatrix;

begin
  // Compute determinant.
  D := M[0, 0] * (M[2, 2] * M[1, 1] - M[2, 1] * M[1, 2]) - M[1, 0] * (M[2, 2] * M[0, 1] - M[2, 1] * M[0, 2]) +
    M[2, 0] * (M[1, 2] * M[0, 1] - M[1, 1] * M[0, 2]);

  // We cannot invert in-place, so make sure source and target matrices are different.
  Temp[0, 0] :=  (M[2, 2] * M[1, 1] - M[2, 1] * M[1, 2]) / D;
  Temp[0, 1] := -(M[2, 2] * M[0, 1] - M[2, 1] * M[0, 2]) / D;
  Temp[0, 2] :=  (M[1, 2] * M[0, 1] - M[1, 1] * M[0, 2]) / D;
  Temp[1, 0] := -(M[2, 2] * M[1, 0] - M[2, 0] * M[1, 2]) / D;
  Temp[1, 1] :=  (M[2, 2] * M[0, 0] - M[2, 0] * M[0, 2]) / D;
  Temp[1, 2] := -(M[1, 2] * M[0, 0] - M[1, 0] * M[0, 2]) / D;
  Temp[2, 0] :=  (M[2, 1] * M[1, 0] - M[2, 0] * M[1, 1]) / D;
  Temp[2, 1] := -(M[2, 1] * M[0, 0] - M[2, 0] * M[0, 1]) / D;
  Temp[2, 2] :=  (M[1, 1] * M[0, 0] - M[1, 0] * M[0, 1]) / D;

  Result := Temp;
end;

//----------------------------------------------------------------------------------------------------------------------

function SameColor(const Color1, Color2: TColorComponents; Delta: Double): Boolean;

// Compares the two given colors and returns True if both are equal (within the range given by Delta).
// Otherwise False is returned. Both colors must have the same number of elements.

var
  I: Integer;
  
begin
  Result := Length(Color1) = Length(Color2);
  if Result then
    for I := 0 to High(Color1) do
      if Abs(Color1[I] - Color2[I]) > Delta then
      begin
        Result := False;
        Break;
      end;
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeCMYK(const C, M, Y, K: Double): TCMYK;

begin
  Result.C := C;
  Result.M := M;
  Result.Y := Y;
  Result.K := K;
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeColorRef(RGB: TRGB; Gamma: Double = 1): COLORREF; overload;

// Converts a floating point RGB color to an 8 bit color reference as used by Windows.
// The function takes care not to produce out-of-gamut colors and allows to apply an optional gamma correction
// (inverse gamma).

begin
  GammaCorrection(RGB, Gamma);
  with RGB do
    Result := Windows.RGB(Round(R * 255), Round(G * 255), Round(B * 255));
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeHLS(const H, L, S: Double): THLS;

begin
  Result.H := H;
  Result.L := L;
  Result.S := S;
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeHLS(const Values: TColorComponents): THLS;

begin
  FillChar(Result, SizeOf(Result), 0);
  if Length(Values) > 0 then
  begin
    Result.H := Values[0];
    if Length(Values) > 1 then
    begin
      Result.L := Values[1];
      if Length(Values) > 2 then
        Result.S := Values[2];
    end
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeLab(const L, a, b: Double): TLab;

begin
  Result.L := L;
  Result.a := a;
  Result.b := b;
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeRGB(const Color: TColor): TRGB;

var
  Reference: COLORREF;

begin
  Reference := ColorToRGB(Color);
  Result.R := GetRValue(Reference) / 255;
  Result.G := GetGValue(Reference) / 255;
  Result.B := GetBValue(Reference) / 255;
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeRGB(const R, G, B: Double): TRGB;

begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeRGB(const Values: TColorComponents): TRGB;

begin
  Result.R := Values[0];
  Result.G := Values[1];
  Result.B := Values[2];
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeRGB2XYZColorMatrix(Primaries: TPrimaries; WhitePoint: TWhitePoint): TColorMatrix;

// Creates a matrix for the conversion from RGB to CIE XYZ using the given primaries and white point.
// Since calculating the transformation matrix is a one-time job followed by many color conversions with the same
// settings it has been moved into an own function, in order to avoid frequent recalculations with the same
// primaries and white point.

var
  Dhelp, Uhelp, Vhelp,
  u, v, w: Double;
  Rn, Gn, Bn, Wn: TXYZ;

begin
  with PrimariesValues[Primaries], WhitePoints[WhitePoint] do
  begin
    // Calculate normalized XYZ values for the three primaries first.
    Dhelp := Red.X + Red.Y + Red.Z;
    Rn.X := Red.X / Dhelp;
    Rn.Y := Red.Y / Dhelp;
    Rn.Z := Red.Z / Dhelp;

    Dhelp := Green.X + Green.Y + Green.Z;
    Gn.X := Green.X / Dhelp;
    Gn.Y := Green.Y / Dhelp;
    Gn.Z := Green.Z / Dhelp;

    Dhelp := Blue.X + Blue.Y + Blue.Z;
    Bn.X := Blue.X / Dhelp;
    Bn.Y := Blue.Y / Dhelp;
    Bn.Z := Blue.Z / Dhelp;

    // White point.
    Dhelp := X + Y + Z;
    Wn.X := X / Dhelp;
    Wn.Y := Y / Dhelp;
    Wn.Z := Z / Dhelp;
  end;

  Dhelp := (Rn.X - Bn.X) * (Gn.Y - Bn.Y) - (Rn.Y - Bn.Y) * (Gn.X - Bn.X);
  Uhelp := (Wn.X - Bn.X) * (Gn.Y - Bn.Y) - (Wn.Y - Bn.Y) * (Gn.X - Bn.X);
  Vhelp := (Rn.X - Bn.X) * (Wn.Y - Bn.Y) - (Rn.Y - Bn.Y) * (Wn.X - Bn.X);

  u := Uhelp / Dhelp;
  v := Vhelp / Dhelp;
  w := 1 - u - v;

  // 1. Row
  Result[0, 0] := u * Rn.X / Wn.Y;
  Result[0, 1] := v * Gn.X / Wn.Y;
  Result[0, 2] := w * Bn.X / Wn.Y;

  // 2. Row
  Result[1, 0] := u * Rn.Y / Wn.Y;
  Result[1, 1] := v * Gn.Y / Wn.Y;
  Result[1, 2] := w * Bn.Y / Wn.Y;

  // 3. Row
  Result[2, 0] := u * Rn.Z / Wn.Y;
  Result[2, 1] := v * Gn.Z / Wn.Y;
  Result[2, 2] := w * Bn.Z / Wn.Y;
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeXYZ(const X, Y, Z: Double): TXYZ;

begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeXYZ2RGBColorMatrix(Primaries: TPrimaries; WhitePoint: TWhitePoint): TColorMatrix;

// Creates a matrix for the conversion from CIE XYZ to RGB using the given primaries and white point.
// Since calculating the transformation matrix is a one-time job followed by many color conversion with the same
// settings it has been moved into an own function, in order to avoid frequent recalculations with the same
// primaries and white point.

begin
  Result := MakeRGB2XYZColorMatrix(Primaries, WhitePoint);
  Result := InvertColorMatrix(Result);
end;

//----------------- Gamut functions ------------------------------------------------------------------------------------

function MakeSafeColor(var RGB: TRGB): Boolean;

// Ensures the given RGB color is in-gamut, that is, no component is < 0 or > 1.
// Returns True if the color had to be adjusted to be in-gamut, otherwise False.

begin
  Result := False;

  if RGB.R < 0 then
  begin
    Result := True;
    RGB.R := 0;
  end;
  if RGB.R > 1 then
  begin
    Result := True;
    RGB.R := 1;
  end;

  if RGB.G < 0 then
  begin
    Result := True;
    RGB.G := 0;
  end;
  if RGB.G > 1 then
  begin
    Result := True;
    RGB.G := 1;
  end;

  if RGB.B < 0 then
  begin
    Result := True;
    RGB.B := 0;
  end;
  if RGB.B > 1 then
  begin
    Result := True;
    RGB.B := 1;
  end;
end;

//----------------- Color managment helper functions -------------------------------------------------------------------

procedure InitializeGlobalData;

var
  ColorSet: TInkColorSet;
  InkColor: TInkColor;
  Matrix: TColorMatrix;
  Parameter: Double;

begin
  // One time conversion of the XYZ values of the CMYK ink colors to RGB for faster computation.
  // The standard working space of Adobe Photoshop is used here to match the results.
  Matrix := MakeXYZ2RGBColorMatrix(primHDTV, wpD65);
  Parameter := 1 / YuleNielsenParameter;
  for ColorSet := Low(TInkColorSet) to High(TInkColorSet) do
    for InkColor := Low(TInkColor) to High(TInkColor) do
    begin
      InkColorSetsRGB[ColorSet][InkColor] := XYZToRGB(InkColorSets[ColorSet][InkColor], Matrix);
      // Correct out-of-gamut values.
      MakeSafeColor(InkColorSetsRGB[ColorSet][InkColor]);
      // Precalculate also the modified RGB values, which are the core of the Yule-Nielsen modification.
      with InkColorSetsRGB[ColorSet][InkColor] do
      begin
        R := Power(R, Parameter);
        G := Power(G, Parameter);
        B := Power(B, Parameter);
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  InitializeGlobalData;
finalization
end.
