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
// GraphicStrings contains the english version of the strings used in GraphicEx, which can be localized.
//
// Rename the file to GraphicStrings.pas to use it as your favourite language file.
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$I GraphicConfiguration.inc}

resourcestring
  // image file descriptions
  gesAllImages = 'Wszystkie obrazy';
  gesRegistration = 'Próba podwójnej rejestracji %s.';

  gesBitmaps = 'Mapy bitowe Windows';
  gesRLEBitmaps = 'Mapy bitowe run length encoded Windows';
  gesDIBs = 'Mapy bitowe Device independant Windows';
  gesEPS = 'Obrazy Encapsulated Postscript';
  gesIcons = 'Ikony Windows';
  gesMetaFiles = 'Meta pliki Windows';
  gesEnhancedMetaFiles = 'Ulepszone meta pliki Windows';
  gesJPGImages = 'Obrazy JPG';
  gesJPEGImages = 'Obrazy JPEG';
  gesJPEImages = 'Obrazy JPE';
  gesJFIFImages = 'Obrazy JFIF';
  gesTruevision = 'Obrazy Truevision';
  gesTIFF = 'Tagged image file format images';
  gesMacTIFF =  'Obrazy Macintosh TIFF';
  gesPCTIF = 'Obrazy PC TIF';
  gesGFIFax = 'Obrazy GFI fax';
  gesSGI = 'Obrazy SGI';
  gesSGITrueColor = 'Obrazy SGI true color';
  gesZSoft = 'Obrazy ZSoft Paintbrush';
  gesZSoftWord = 'Zrzut ekranu Word 5.x';
  gesAliasWaveFront = 'Obrazy Alias/Wavefront';
  gesSGITrueColorAlpha = 'Obrazy SGI true color with alpha';
  gesSGIMono = 'Czarno-bia³e obrazy SGI';
  gesPhotoshop = 'Obrazy Photoshop';
  gesPortable = 'Obrazy Portable map';
  gesPortablePixel = 'Obrazy Portable pixel map';
  gesPortableGray = 'Obrazy Portable gray map';
  gesPortableMono = 'Obrazy Portable bitmap';
  gesAutoDesk = 'Obrazy Autodesk';
  gesKodakPhotoCD = 'Obrazy Kodak Photo-CD';
  gesCompuserve = 'Obrazy CompuServe';
  gesHalo = 'Obrazy Dr. Halo';
  gesPaintShopPro = 'Obrazy Paintshop Pro';
  gesPaintshopProFrames = 'Paintshop Pro frames';
  gesPaintshopProTubes = 'Paintshop Pro tubes';
  gesPortableNetworkGraphic = 'Obrazy Portable network graphic';

  // image specific error messages
  gesInvalidImage = 'Nie mogê otworzyæ obrazu. B³êdny lub niespodziewany %s format.';
  gesInvalidColorFormat = 'B³êdny format kolorów w pliku %s.';
  gesStreamReadError = 'B³¹d odczytu strumienia danych z pliku %s.';
  gesUnsupportedImage = 'Nie mogê otworzyæ obrazu. Nieobs³ugiwany format %s.';
  gesUnsupportedFeature = 'Nie mogê otworzyæ obrazu. %s nie obs³uguje plików %s.';
  gesInvalidCRC = 'Nie mogê otworzyæ obrazu. B³¹d CRC w pliku %s.';
  gesCompression = 'Nie mogê otworzyæ obrazu. B³¹d kompresji w pliku %s.';
  gesExtraCompressedData = 'Nie mogê otworzyæ obrazu. Nieznany typ kompresji w pliku %s.';
  gesInvalidPalette = 'Nie mogê otworzyæ obrazu. B³êdna paleta kolorów w pliku %s.';
  gesUnknownCriticalChunk = 'Nie mogê otworzyæ obrazu PNG. Napodkano krytczny wyj¹tek.';

  // features (usually used together with unsupported feature string)
  gesCompressionScheme = 'Metoda kompresji to';
  gesRLAPixelFormat = 'Formaty inne ni¿ RGB i RGBA to';
  gesPSPFileType = 'Pliki w wersji innej ni¿ 3 lub 4 to';

  // color manager error messages
  gesIndexedNotSupported = 'Konwersja pomiêdzy indeksowanym i nieindeksowanym formatem punktów nie jest obs³ugiwana.';
  gesConversionUnsupported = 'B³¹d konwersji koloru. Nie mogê znaleŸæ odpowiedniej metody.';
  gesInvalidSampleDepth = 'B³êdna g³êbia kolorów. Powinno byæ 1, 2, 4, 8 lub 16 bitów na próbkê.';
  gesInvalidPixelDepth = 'Iloœæ próbek na punkt nLanguageie odpowiada danemu schematowi kolorów.';
  gesInvalidSubSampling = 'B³êdna wartoœæ Subsampling. Dozwolona jest 1, 2 i 4.';
  gesVerticalSubSamplingError = 'Pionowa wartoœæ Subsampling nie mo¿e byæ wiêksza ni¿ pozioma.';

  // progress strings
  gesPreparing = 'Przygotowywanie...';
  gesLoadingData = 'Czytanie danych...';
  gesUpsampling = 'Próbkowanie...';
  gesTransfering = 'Transferowanie...';

  // compression errors
  gesLZ77Error = 'B³¹d dekompresji LZ77.';
  gesJPEGEOI = 'B³¹d dekompresji JPEG. Niespodziewany koniec danych wejœciowych.';
  gesJPEGStripSize = 'Niew³aœciwy rozmiar JPEG strip/tile.';
  gesJPEGComponentCount = 'Niew³aœciwy JPEG component count.';
  gesJPEGDataPrecision = 'Niew³aœciwa precyzja danych JPEG.';
  gesJPEGSamplingFactors = 'Niew³aœciwe próbkowanie JPEG.';
  gesJPEGBogusTableField = 'B³¹d zakresu w tablicy JPEG.';
  gesJPEGFractionalLine = 'Fractional JPEG scanline nie jest obs³ugiwany.';

  // miscellaneous
  gesWarning = 'Uwaga';

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
