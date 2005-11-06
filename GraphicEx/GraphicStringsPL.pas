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
// The initial developer of the original code is Dipl. Ing. Mike Lischke (Plei�a, Germany, www.delphi-gems.com),
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
  gesRegistration = 'Pr�ba podw�jnej rejestracji %s.';

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
  gesSGIMono = 'Czarno-bia�e obrazy SGI';
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
  gesInvalidImage = 'Nie mog� otworzy� obrazu. B��dny lub niespodziewany %s format.';
  gesInvalidColorFormat = 'B��dny format kolor�w w pliku %s.';
  gesStreamReadError = 'B��d odczytu strumienia danych z pliku %s.';
  gesUnsupportedImage = 'Nie mog� otworzy� obrazu. Nieobs�ugiwany format %s.';
  gesUnsupportedFeature = 'Nie mog� otworzy� obrazu. %s nie obs�uguje plik�w %s.';
  gesInvalidCRC = 'Nie mog� otworzy� obrazu. B��d CRC w pliku %s.';
  gesCompression = 'Nie mog� otworzy� obrazu. B��d kompresji w pliku %s.';
  gesExtraCompressedData = 'Nie mog� otworzy� obrazu. Nieznany typ kompresji w pliku %s.';
  gesInvalidPalette = 'Nie mog� otworzy� obrazu. B��dna paleta kolor�w w pliku %s.';
  gesUnknownCriticalChunk = 'Nie mog� otworzy� obrazu PNG. Napodkano krytczny wyj�tek.';

  // features (usually used together with unsupported feature string)
  gesCompressionScheme = 'Metoda kompresji to';
  gesRLAPixelFormat = 'Formaty inne ni� RGB i RGBA to';
  gesPSPFileType = 'Pliki w wersji innej ni� 3 lub 4 to';

  // color manager error messages
  gesIndexedNotSupported = 'Konwersja pomi�dzy indeksowanym i nieindeksowanym formatem punkt�w nie jest obs�ugiwana.';
  gesConversionUnsupported = 'B��d konwersji koloru. Nie mog� znale�� odpowiedniej metody.';
  gesInvalidSampleDepth = 'B��dna g��bia kolor�w. Powinno by� 1, 2, 4, 8 lub 16 bit�w na pr�bk�.';
  gesInvalidPixelDepth = 'Ilo�� pr�bek na punkt nLanguageie odpowiada danemu schematowi kolor�w.';
  gesInvalidSubSampling = 'B��dna warto�� Subsampling. Dozwolona jest 1, 2 i 4.';
  gesVerticalSubSamplingError = 'Pionowa warto�� Subsampling nie mo�e by� wi�ksza ni� pozioma.';

  // progress strings
  gesPreparing = 'Przygotowywanie...';
  gesLoadingData = 'Czytanie danych...';
  gesUpsampling = 'Pr�bkowanie...';
  gesTransfering = 'Transferowanie...';

  // compression errors
  gesLZ77Error = 'B��d dekompresji LZ77.';
  gesJPEGEOI = 'B��d dekompresji JPEG. Niespodziewany koniec danych wej�ciowych.';
  gesJPEGStripSize = 'Niew�a�ciwy rozmiar JPEG strip/tile.';
  gesJPEGComponentCount = 'Niew�a�ciwy JPEG component count.';
  gesJPEGDataPrecision = 'Niew�a�ciwa precyzja danych JPEG.';
  gesJPEGSamplingFactors = 'Niew�a�ciwe pr�bkowanie JPEG.';
  gesJPEGBogusTableField = 'B��d zakresu w tablicy JPEG.';
  gesJPEGFractionalLine = 'Fractional JPEG scanline nie jest obs�ugiwany.';

  // miscellaneous
  gesWarning = 'Uwaga';

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
