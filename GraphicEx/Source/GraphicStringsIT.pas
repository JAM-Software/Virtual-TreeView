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
// The initial developer of the original code is Dipl. Ing. Mike Lischke (Pleiﬂa, Germany, www.delphi-gems.com),
//
// Portions created by Dipl. Ing. Mike Lischke are
// Copyright (C) 1999-2003 Dipl. Ing. Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// GraphicStringsIT contains the italian version of the strings used in GraphicEx, which can be localized.
// Translation done by Salvatore Meschini (salvatoremeschini@tiscalinet.it).
//
// Rename the file to GraphicStrings.pas to use it as your favourite language file.
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$I GraphicConfiguration.inc}

resourcestring
  // image file descriptions
  gesAllImages = 'Tutte le immagini';
  gesRegistration = 'Tentata doppia registrazione di %s';

  gesBitmaps = 'Bitmap di Windows';
  gesRLEBitmaps = 'Bitmap di Windows Run Length Encoded';
  gesDIBs = 'Bitmap di Windows Indipendenti dal Dispositivo';
  gesEPS = 'Immagini Encapsulated Postscript';
  gesIcons = 'Icone di Windows';
  gesMetaFiles = 'Metafiles di Windows';
  gesEnhancedMetaFiles = 'Enhanced Meta Files di Windows';
  gesJPGImages = 'Immagini JPG';
  gesJPEGImages = 'Immagini JPEG';
  gesJPEImages = 'Immagini JPE';
  gesJFIFImages = 'Immagini JFIF';
  gesTruevision = 'Immagini Truevision';
  gesTIFF = 'Immagini TIFF';
  gesMacTIFF =  'Immagini Macintosh TIFF';
  gesPCTIF = 'Immagini PC TIF';
  gesGFIFax = 'Immagini GFI fax';
  gesSGI = 'Immagini SGI';
  gesSGITrueColor = 'Immagini SGI true color';
  gesZSoft = 'Immagini ZSoft Paintbrush';
  gesZSoftWord = 'Immagini schermate Word 5.x';
  gesAliasWaveFront = 'Immagini Alias/Wavefront';
  gesSGITrueColorAlpha = 'Immagini SGI true color con alpha';
  gesSGIMono = 'Immagini SGI bianco/nero';
  gesPhotoshop = 'Immagini Photoshop';
  gesPortable = 'Immagini Portable map';
  gesPortablePixel = 'Immagini Portable pixel map';
  gesPortableGray = 'Immagini Portable gray map';
  gesPortableMono = 'Immagini Portable bitmap';
  gesAutoDesk = 'Immagini Autodesk';
  gesKodakPhotoCD = 'Immagini Kodak Photo-CD';
  gesCompuserve = 'Immagini CompuServe';
  gesHalo = 'Immagini Dr. Halo';
  gesPaintShopPro = 'Immagini Paintshop Pro';
  gesPortableNetworkGraphic = 'Immagini Portable network graphic';

  // image specific error messages
  gesInvalidImage = 'Impossibile caricare l''immagine. Il formato %s Ë non valido o non corretto.';
  gesInvalidColorFormat = 'Il formato dei colori del file %s non Ë valido.';
  gesStreamReadError = 'Errore nella lettura del file %s.';
  gesUnsupportedImage = 'Impossibile caricare l''immagine. Il formato %s non Ë supportato.';
  gesUnsupportedFeature = 'Impossibile caricare l''immagine. %s non supportato per file %s.';
  gesInvalidCRC = 'Impossibile caricare l''immagine. Errore CRC nel file %s.';
  gesCompression = 'Impossibile caricare l''immagine. Errore di compressione nel file %s.';
  gesExtraCompressedData = 'Impossibile caricare l''immagine. Trovati dati extra compressi nel file %s.';
  gesInvalidPalette = 'Impossibile caricare l''immagine. Palette nel file %s non valida.';
  gesUnknownCriticalChunk = 'Impossibile caricare l''immagine PNG. Rilevato blocco critico ma non previsto.';

  // features (usually used together with unsupported feature string)
  gesCompressionScheme = 'Lo schema di compressione Ë';
  gesRLAPixelFormat = 'Formati diversi da RGB e RGBA sono';
  gesPSPFileType = 'Versioni dei file diverse da 3 o 4 sono';

  // color manager error messages
  gesIndexedNotSupported = 'La conversione tra formati di pixel indicizzati e non-indicizzati non Ë supportata.';
  gesConversionUnsupported = 'Conversione di colore fallita. Impossibile trovare un metodo adatto.';
  gesInvalidSampleDepth = 'Profondit‡ di colore non valida. I bit per campione devono essere 1, 2, 4, 8 o 16.';
  gesInvalidPixelDepth = 'Il conteggio dei campioni per pixel non corrisponde allo schema dei colori.';
  gesInvalidSubSampling = 'Valore di sottocampionamento non valido. Valori consentiti 1, 2 e 4.';
  gesVerticalSubSamplingError = 'Il valore di sottocampionamento verticale deve essere <= di quello valore orizzontale.';

  // progress strings
  gesPreparing = 'Preparazione...';
  gesLoadingData = 'Caricamento dati...';
  gesUpsampling = 'Campionamento...';
  gesTransfering = 'Trasferimento...';

  // compression errors
  gesLZ77Error = 'Errore decompressione LZ77.';
  gesJPEGEOI = 'Errore decompressione JPEG. Fine del flusso di ingresso non attesa.';
  gesJPEGStripSize = 'Dimensione blocco JPEG impropria.';
  gesJPEGComponentCount = 'Conteggio componente JPEG improprio.';
  gesJPEGDataPrecision = 'Precisione dati JPEG impropria.';
  gesJPEGSamplingFactors = 'Fattori di campionamento JPEG impropri.';
  gesJPEGBogusTableField = 'Campo tabelle JPEG fittizio.';
  gesJPEGFractionalLine = 'Linea di scansione frazionale JPEG non supportata.';

  // miscellaneous
  gesWarning = 'Avvertimento';

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
