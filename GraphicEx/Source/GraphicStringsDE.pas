unit GraphicStrings;

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is GraphicStrings.pas, released November 1, 1999.
//
// The initial developer of the original code is Dipl. Ing. Mike Lischke (Pleißa, Germany, www.delphi-gems.com),
//
// Portions created by Dipl. Ing. Mike Lischke are
// Copyright (C) 1999-2003 Dipl. Ing. Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// GraphicStrings contains the german version of the strings used in GraphicEx, which can be localized.
// Translation done by Mike Lischke (public@delphi-gems.com).
//
// Rename the file to GraphicStrings.pas to use it as your favourite language file.
//
//----------------------------------------------------------------------------------------------------------------------

interface
                   
{$I GraphicConfiguration.inc}

resourcestring
  // image file descriptions
  gesAllImages = 'Alle Bilder';
  gesRegistration = 'Das Format %s ist schon registriert.';

  gesBitmaps = 'Windows bitmaps';
  gesRLEBitmaps = 'Run length encoded Windows bitmaps';
  gesDIBs = 'Geräteunabhängige Windows bitmaps';
  gesIcons = 'Windows icons';
  gesMetaFiles = 'Windows metafiles';
  gesEnhancedMetaFiles = 'Windows erweiterte metafiles';
  gesJPGImages = 'JPG Bilder';
  gesJPEGImages = 'JPEG Bilder';
  gesTruevision = 'Truevision Bilder';
  gesTIFF = 'Tagged image file format';
  gesMacTIFF =  'Macintosh TIFF Bilder';
  gesPCTIF = 'PC TIF Bilder';
  gesSGI = 'SGI Bilder';
  gesSGITrueColor = 'SGI True Color Bilder';
  gesZSoft = 'ZSoft Paintbrush Bilder';
  gesZSoftWord = 'Word 5.x Snapschuss Bilder';
  gesAliasWaveFront = 'Alias/Wavefront Bilder';
  gesSGITrueColorAlpha = 'SGI True Color Bilder mit Transparenz';
  gesSGIMono = 'SGI schwarz/weiss Bilder';
  gesPhotoshop = 'Photoshop Bilder';
  gesPortable = 'Portable map Bilder';
  gesPortablePixel = 'Portable pixel map Bilder';
  gesPortableGray = 'Portable gray map Bilder';
  gesPortableMono = 'Portable bitmap Bilder';
  gesAutoDesk = 'Autodesk Bilder';
  gesKodakPhotoCD = 'Kodak Photo-CD Bilder';
  gesCompuserve = 'CompuServe Bilder';
  gesHalo = 'Dr. Halo Bilder';
  gesPaintShopPro = 'Paintshop Pro Bilder';
  gesPaintshopProFrames = 'Paintshop Pro Frames';
  gesPaintshopProTubes = 'Paintshop Pro Tubes';
  gesPortableNetworkGraphic = 'Portable network graphic Bilder';

  // image specific error messages
  gesInvalidImage = 'Bild konnte nicht geladen werden. Ungültiges oder unerwartetes %s Bildformat.';
  gesInvalidColorFormat = 'Ungültiges Farbformat in %s Bild.';
  gesStreamReadError = 'Stream Lesefehler in %s Datei.';
  gesUnsupportedImage = 'Bild konnte nicht geladen werden. Nicht unterstütztes %s Bildformat.';
  gesUnsupportedFeature = 'Bild konnte nicht geladen werden. %s nicht unterstützt für %s Dateien.';
  gesInvalidCRC = 'Bild konnte nicht geladen werden. Ein CRC Fehler ist in der %s Datei aufgetreten.';
  gesCompression = 'Bild konnte nicht geladen werden. Kompressionsfehler in %s Datei gefunden.';
  gesExtraCompressedData = 'Bild konnte nicht geladen werden. Zuviele komprimierte Daten in %s Datei gefunden.';
  gesInvalidPalette = 'Bild konnte nicht geladen werden. Palette in %s Datei ist ungültig.';

  // features (usually used together with unsupported feature string)
  gesCompressionScheme = 'Das Kompressionsschema wird';
  gesPCDImageSize = 'Bildgrößen außer Base16, Base4 oder Base werden';
  gesRLAPixelFormat = 'Bildformate außer RGB und RGBA werden';
  gesPSPFileType = 'Dateiversionen außer 3 oder 4 werden';

  // errors which apply only to specific image types
  gesUnknownCriticalChunk = 'PNG Bild konnte nicht geladen werden. Unerwarteten, aber notwendigen Chunk gefunden.';

  // color manager error messages
  gesIndexedNotSupported = 'Konversion zwischen indizierten and nicht-indizierten Farbformaten wird nicht unterstützt.';
  gesConversionUnsupported = 'Farbkonversion schlug fehl. Konnte keine Methode zur Konversion finden.';
  gesInvalidSampleDepth = 'Farbtiefe ist ungültig. Bits pro Sample muß entweder 1, 2, 4, 8 or 16 sein.';
  gesInvalidPixelDepth = 'Sample Anzahl pro Pixel korrespondiert nicht mit dem eingestellten Farbschema.';

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
