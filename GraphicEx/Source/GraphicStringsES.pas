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
// GraphicStrings contains the spanish version of the strings used in GraphicEx, which can be localized.
// Translation done by Ivan Llanas (ivan@cbi.es; ivan@llanas.com)
//
// Rename the file to GraphicStrings.pas to use it as your favourite language file.
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$I GraphicConfiguration.inc}

resourcestring
  // image file descriptions
  gesAllImages = 'Todas las imágenes';
  gesRegistration = 'Intento de registrar %s por segunda vez.';

  gesBitmaps = 'Imágenes de Windows';
  gesRLEBitmaps = 'Imágenes RLE de Windows';
  gesDIBs = 'Imágenes DIB de Windows';
  gesEPS = 'Imágenes Encapsulated Postscript';
  gesIcons = 'Iconos de Windows';
  gesMetaFiles = 'Metaarchivos de Windows';
  gesEnhancedMetaFiles = 'Metaarchivos mejorados de Windows';
  gesJPGImages = 'Imágenes JPG';
  gesJPEGImages = 'Imágenes JPEG';
  gesJPEImages = 'Imágenes JPE';
  gesJFIFImages = 'Imágenes JFIF';
  gesTruevision = 'Imágenes Truevision';
  gesTIFF = 'Imágenes en formato Tagged';
  gesMacTIFF =  'Imágenes TIFF Macintosh';
  gesPCTIF = 'Imágenes TIF PC';
  gesGFIFax = 'Imágenes fax GFI fax';
  gesSGI = 'Imágenes SGI';
  gesSGITrueColor = 'Imágenes SGI de color verdadero';
  gesZSoft = 'Imágenes ZSoft Paintbrush';
  gesZSoftWord = 'Imágenes de captura de pantalla de Word 5.x';
  gesAliasWaveFront = 'Imágenes Alias/Wavefront';
  gesSGITrueColorAlpha = 'Imágenes SGI de color verdadero con alfa';
  gesSGIMono = 'Imágenes SGI en blanco y negro';
  gesPhotoshop = 'Imágenes de Photoshop';
  gesPortable = 'Imágenes Portable map';
  gesPortablePixel = 'Imágenes Portable pixel map';
  gesPortableGray = 'Imágenes Portable gray map';
  gesPortableMono = 'Imágenes Portable bitmap';
  gesAutoDesk = 'Imágenes de Autodesk';
  gesKodakPhotoCD = 'Imágenes Kodak Photo-CD';
  gesCompuserve = 'Imágenes GIF de CompuServe';
  gesHalo = 'Imágenes de Dr. Halo';
  gesPaintShopPro = 'Imágenes de Paintshop Pro';
  gesPaintshopProFrames = 'Paintshop Pro frames';
  gesPaintshopProTubes = 'Paintshop Pro tubes';
  gesPortableNetworkGraphic = 'Imágenes Portable network graphic';

  // image specific error messages
  _CNLI = 'No se puede cargar la imagen. ';
  gesInvalidImage = _CNLI + 'Formato de imagen %s inválido.';
  gesInvalidColorFormat = 'Formato de color inválido en archivo %s.';
  gesStreamReadError = 'Error de lectura en archivo %s.';
  gesUnsupportedImage = _CNLI + 'Formato de imagen %s no soportado.';
  gesUnsupportedFeature = _CNLI + '%s no soportado para archivos %s.';
  gesInvalidCRC = _CNLI + 'Error de CRC en archivo %s.';
  gesCompression = _CNLI + 'Error de compresión en archivo %s.';
  gesExtraCompressedData = _CNLI + 'Datos extra comprimidos en archivo %s.';
  gesInvalidPalette = _CNLI + 'La paleta del archivo %s es inválida.';
  gesUnknownCriticalChunk = _CNLI + 'Paquete inesperado pero crítico.';

  // features (usually used together with unsupported feature string)
  gesCompressionScheme = 'El esquema de compresión es';
  gesRLAPixelFormat = 'Otros formatos a parte de RGB y RGBA son';
  gesPSPFileType = 'Otras versiones a parte de 3 o 4 son';

  // color manager error messages
  gesIndexedNotSupported = 'La conversión entre formatos de pixel indexados y no indexados no está soportada.';
  gesConversionUnsupported = 'Error en la conversión de color. No se pudo encontrar el método apropiado.';
  gesInvalidSampleDepth = 'Profundidad de color inválida. Los bits por muestra deben ser 1, 2, 4, 8 o 16.';
  gesInvalidPixelDepth = 'El número de muestras por pixel no corresponde al esquema de color dado.';
  gesInvalidSubSampling = 'Valor de submuestreado inválido. Los valores permitidos son 1, 2 y 4.';
  gesVerticalSubSamplingError = 'El valor de submuestreado vertical debe ser <= valor de submuestreado horizontal.';

  // progress strings
  gesPreparing = 'Preparando...';
  gesLoadingData = 'Cargando...';
  gesUpsampling = 'Componiendo...';
  gesTransfering = 'Transfiriendo...';

  // compression errors
  gesLZ77Error = 'Error en descompresión LZ77.';
  gesJPEGEOI = 'Error en descompresión JPEG. Inesperado final de entrada.';
  gesJPEGStripSize = 'Tamaño de tira/cuadro JPEG impropio.';
  gesJPEGComponentCount = 'Número de componente JPEG impropio.';
  gesJPEGDataPrecision = 'Precisión de datos JPEG impropia.';
  gesJPEGSamplingFactors = 'Factor de muestra JPEG impropio.';
  gesJPEGBogusTableField = 'Campo de tabla JPEG Bogus.';
  gesJPEGFractionalLine = 'Línea JPEG fraccional no soportada.';

  // miscellaneous
  gesWarning = 'Atención';

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
