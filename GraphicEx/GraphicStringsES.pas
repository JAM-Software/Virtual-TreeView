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
// The initial developer of the original code is Dipl. Ing. Mike Lischke (Plei�a, Germany, www.delphi-gems.com),
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
  gesAllImages = 'Todas las im�genes';
  gesRegistration = 'Intento de registrar %s por segunda vez.';

  gesBitmaps = 'Im�genes de Windows';
  gesRLEBitmaps = 'Im�genes RLE de Windows';
  gesDIBs = 'Im�genes DIB de Windows';
  gesEPS = 'Im�genes Encapsulated Postscript';
  gesIcons = 'Iconos de Windows';
  gesMetaFiles = 'Metaarchivos de Windows';
  gesEnhancedMetaFiles = 'Metaarchivos mejorados de Windows';
  gesJPGImages = 'Im�genes JPG';
  gesJPEGImages = 'Im�genes JPEG';
  gesJPEImages = 'Im�genes JPE';
  gesJFIFImages = 'Im�genes JFIF';
  gesTruevision = 'Im�genes Truevision';
  gesTIFF = 'Im�genes en formato Tagged';
  gesMacTIFF =  'Im�genes TIFF Macintosh';
  gesPCTIF = 'Im�genes TIF PC';
  gesGFIFax = 'Im�genes fax GFI fax';
  gesSGI = 'Im�genes SGI';
  gesSGITrueColor = 'Im�genes SGI de color verdadero';
  gesZSoft = 'Im�genes ZSoft Paintbrush';
  gesZSoftWord = 'Im�genes de captura de pantalla de Word 5.x';
  gesAliasWaveFront = 'Im�genes Alias/Wavefront';
  gesSGITrueColorAlpha = 'Im�genes SGI de color verdadero con alfa';
  gesSGIMono = 'Im�genes SGI en blanco y negro';
  gesPhotoshop = 'Im�genes de Photoshop';
  gesPortable = 'Im�genes Portable map';
  gesPortablePixel = 'Im�genes Portable pixel map';
  gesPortableGray = 'Im�genes Portable gray map';
  gesPortableMono = 'Im�genes Portable bitmap';
  gesAutoDesk = 'Im�genes de Autodesk';
  gesKodakPhotoCD = 'Im�genes Kodak Photo-CD';
  gesCompuserve = 'Im�genes GIF de CompuServe';
  gesHalo = 'Im�genes de Dr. Halo';
  gesPaintShopPro = 'Im�genes de Paintshop Pro';
  gesPaintshopProFrames = 'Paintshop Pro frames';
  gesPaintshopProTubes = 'Paintshop Pro tubes';
  gesPortableNetworkGraphic = 'Im�genes Portable network graphic';

  // image specific error messages
  _CNLI = 'No se puede cargar la imagen. ';
  gesInvalidImage = _CNLI + 'Formato de imagen %s inv�lido.';
  gesInvalidColorFormat = 'Formato de color inv�lido en archivo %s.';
  gesStreamReadError = 'Error de lectura en archivo %s.';
  gesUnsupportedImage = _CNLI + 'Formato de imagen %s no soportado.';
  gesUnsupportedFeature = _CNLI + '%s no soportado para archivos %s.';
  gesInvalidCRC = _CNLI + 'Error de CRC en archivo %s.';
  gesCompression = _CNLI + 'Error de compresi�n en archivo %s.';
  gesExtraCompressedData = _CNLI + 'Datos extra comprimidos en archivo %s.';
  gesInvalidPalette = _CNLI + 'La paleta del archivo %s es inv�lida.';
  gesUnknownCriticalChunk = _CNLI + 'Paquete inesperado pero cr�tico.';

  // features (usually used together with unsupported feature string)
  gesCompressionScheme = 'El esquema de compresi�n es';
  gesRLAPixelFormat = 'Otros formatos a parte de RGB y RGBA son';
  gesPSPFileType = 'Otras versiones a parte de 3 o 4 son';

  // color manager error messages
  gesIndexedNotSupported = 'La conversi�n entre formatos de pixel indexados y no indexados no est� soportada.';
  gesConversionUnsupported = 'Error en la conversi�n de color. No se pudo encontrar el m�todo apropiado.';
  gesInvalidSampleDepth = 'Profundidad de color inv�lida. Los bits por muestra deben ser 1, 2, 4, 8 o 16.';
  gesInvalidPixelDepth = 'El n�mero de muestras por pixel no corresponde al esquema de color dado.';
  gesInvalidSubSampling = 'Valor de submuestreado inv�lido. Los valores permitidos son 1, 2 y 4.';
  gesVerticalSubSamplingError = 'El valor de submuestreado vertical debe ser <= valor de submuestreado horizontal.';

  // progress strings
  gesPreparing = 'Preparando...';
  gesLoadingData = 'Cargando...';
  gesUpsampling = 'Componiendo...';
  gesTransfering = 'Transfiriendo...';

  // compression errors
  gesLZ77Error = 'Error en descompresi�n LZ77.';
  gesJPEGEOI = 'Error en descompresi�n JPEG. Inesperado final de entrada.';
  gesJPEGStripSize = 'Tama�o de tira/cuadro JPEG impropio.';
  gesJPEGComponentCount = 'N�mero de componente JPEG impropio.';
  gesJPEGDataPrecision = 'Precisi�n de datos JPEG impropia.';
  gesJPEGSamplingFactors = 'Factor de muestra JPEG impropio.';
  gesJPEGBogusTableField = 'Campo de tabla JPEG Bogus.';
  gesJPEGFractionalLine = 'L�nea JPEG fraccional no soportada.';

  // miscellaneous
  gesWarning = 'Atenci�n';

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
