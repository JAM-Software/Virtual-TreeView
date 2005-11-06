unit GraphicStringsES;

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
// GraphicStringsCT contains the catalan version of the strings used in GraphicEx, which can be localized.
// Translation done by Ivan Llanas (ivan@cbi.es; ivan@llanas.com)
//
// Rename the file to GraphicStrings.pas to use it as your favourite language file.
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$I GraphicConfiguration.inc}

resourcestring
  // image file descriptions
  gesAllImages = 'Totes les imatges';
  gesRegistration = 'Intent de registrar %s per segona vegada.';

  gesBitmaps = 'Imatges de Windows';
  gesRLEBitmaps = 'Imatges RLE de Windows';
  gesDIBs = 'Imatges DIB de Windows';
  gesEPS = 'Imatges Encapsulated Postscript';
  gesIcons = 'Icones de Windows';
  gesMetaFiles = 'Metaarxius de Windows';
  gesEnhancedMetaFiles = 'Metaarxius millorats de Windows';
  gesJPGImages = 'Imatges JPG';
  gesJPEGImages = 'Imatges JPEG';
  gesJPEImages = 'Imatges JPE';
  gesJFIFImages = 'Imatges JFIF';
  gesTruevision = 'Imatges Truevision';
  gesTIFF = 'Imatges en formato Tagged';
  gesMacTIFF =  'Imatges TIFF Macintosh';
  gesPCTIF = 'Imatges TIF PC';
  gesGFIFax = 'Imatges fax GFI fax';
  gesSGI = 'Imatges SGI';
  gesSGITrueColor = 'Imatges SGI de color verdadero';
  gesZSoft = 'Imatges ZSoft Paintbrush';
  gesZSoftWord = 'Imatges de captura de pantalla de Word 5.x';
  gesAliasWaveFront = 'Imatges Alias/Wavefront';
  gesSGITrueColorAlpha = 'Imatges SGI de color verdader amb alfa';
  gesSGIMono = 'Imatges SGI en blanc i negre';
  gesPhotoshop = 'Imatges de Photoshop';
  gesPortable = 'Imatges Portable map';
  gesPortablePixel = 'Imatges Portable pixel map';
  gesPortableGray = 'Imatges Portable gray map';
  gesPortableMono = 'Imatges Portable bitmap';
  gesAutoDesk = 'Imatges de Autodesk';
  gesKodakPhotoCD = 'Imatges Kodak Photo-CD';
  gesCompuserve = 'Imatges GIF de CompuServe';
  gesHalo = 'Imatges de Dr. Halo';
  gesPaintShopPro = 'Imatges de Paintshop Pro';
  gesPaintshopProFrames = 'Paintshop Pro frames';
  gesPaintshopProTubes = 'Paintshop Pro tubes';
  gesPortableNetworkGraphic = 'Imatges Portable network graphic';

  // image specific error messages
  _CNLI = 'No es pot carregar la imatge. ';
  gesInvalidImage = _CNLI + 'Format d''imatge %s invàlid.';
  gesInvalidColorFormat = 'Format de color invàlid en arxiu %s.';
  gesStreamReadError = 'Error de lectura en arxiu %s.';
  gesUnsupportedImage = _CNLI + 'Formato d''imatge %s no soportat.';
  gesUnsupportedFeature = _CNLI + '%s no soportat per arxius %s.';
  gesInvalidCRC = _CNLI + 'Error de CRC en arxiu %s.';
  gesCompression = _CNLI + 'Error de compressió en arxiu %s.';
  gesExtraCompressedData = _CNLI + 'Dades extra comprimides en arxiu %s.';
  gesInvalidPalette = _CNLI + 'La paleta de l''arxiu %s es invàlida.';
  gesUnknownCriticalChunk = _CNLI + 'Paquet inesperat però crític.';

  // features (usually used together with unsupported feature string)
  gesCompressionScheme = 'L''esquema de compressió és';
  gesRLAPixelFormat = 'Altres formats a part de RGB i RGBA són';
  gesPSPFileType = 'Altres versions a part de 3 o 4 són';

  // color manager error messages
  gesIndexedNotSupported = 'La conversió entre formats de pixel indexats i no indexats no està soportada.';
  gesConversionUnsupported = 'Error en la conversió de color. No s''ha pogut trobar el mètode apropiat.';
  gesInvalidSampleDepth = 'Profunditat de color invàlida. Els bits per mostra han de ser 1, 2, 4, 8 o 16.';
  gesInvalidPixelDepth = 'El nombre de mostres per pixel no correspon a l''esquema de color donat.';
  gesInvalidSubSampling = 'Valor de submostrat invàlido. Els valors permesos són 1, 2 i 4.';
  gesVerticalSubSamplingError = 'El valor de submostrat vertical ha de ser <= valor de submostrat horitzontal.';

  // progress strings
  gesPreparing = 'Preparant...';
  gesLoadingData = 'Carregant...';
  gesUpsampling = 'Composant...';
  gesTransfering = 'Transferint...';

  // compression errors
  gesLZ77Error = 'Error en descompressió LZ77.';
  gesJPEGEOI = 'Error en descompressió JPEG. Inesperat final d''entrada.';
  gesJPEGStripSize = 'Mida de tira/quadre JPEG impropi.';
  gesJPEGComponentCount = 'Nombre de component JPEG impropi.';
  gesJPEGDataPrecision = 'Precissió de dades JPEG impròpia.';
  gesJPEGSamplingFactors = 'Factor de mostra JPEG impropi.';
  gesJPEGBogusTableField = 'Camp de taula JPEG Bogus.';
  gesJPEGFractionalLine = 'Línia JPEG fraccional no soportada.';

  // miscellaneous
  gesWarning = 'Atenció';

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
