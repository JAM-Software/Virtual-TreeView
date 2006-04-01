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
// The initial developer of the original code is Dipl. Ing. Mike Lischke (PleiЯa, Germany, www.delphi-gems.com),
//
// Portions created by Dipl. Ing. Mike Lischke are
// Copyright (C) 1999-2003 Dipl. Ing. Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// GraphicStrings contains the english version of the strings used in GraphicEx, which can be localized.
// Translation done by Shashlov Kirill.
//
// Rename the file to GraphicStrings.pas to use it as your favourite language file.
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$I GraphicConfiguration.inc}

resourcestring
  // Описания файлов изображений
  gesAllImages = 'Все изображения';
  gesRegistration = 'Попытка зарегистрировать %s дважды.';

  gesBitmaps = 'Точечные рисунки Windows';
  gesRLEBitmaps = 'Точечные рисунки RLE';
  gesDIBs = 'Аппаратно-независимые точечные рисунки Windows';
  gesEPS = 'Внедрённые изображения Postscript';
  gesIcons = 'Иконки Windows';
  gesMetaFiles = 'Метафайлы Windows';
  gesEnhancedMetaFiles = 'Расширенные метафайлы Windows';
  gesJPGImages = 'Изображения JPG';
  gesJPEGImages = 'Изображения JPEG';
  gesJPEImages = 'Изображения JPE';
  gesJFIFImages = 'Изображения JFIF';
  gesTruevision = 'Изображения Truevision';
  gesTIFF = 'Изображения TIFF';
  gesMacTIFF =  'Изображения TIFF для Macintosh';
  gesPCTIF = 'Изображения TIF (PC)';
  gesGFIFax = 'Факс-изображения GFI';
  gesSGI = 'Изображения SGI';
  gesSGITrueColor = 'Полноцветные изображения SGI';
  gesZSoft = 'Изображения ZSoft Paintbrush';
  gesZSoftWord = 'Снимки экрана Word 5.x';
  gesAliasWaveFront = 'Изображения Alias/Wavefront';
  gesSGITrueColorAlpha = 'Полноцветные изображения SGI с альфа-каналом';
  gesSGIMono = 'Чёрно-белые изображения SGI';
  gesPhotoshop = 'Изображения Photoshop';
  gesPortable = 'Изображения Portable map';
  gesPortablePixel = 'Изображения Portable pixel map';
  gesPortableGray = 'Изображения Portable gray map';
  gesPortableMono = 'Изображения Portable bitmap';
  gesAutoDesk = 'Изображения Autodesk';
  gesKodakPhotoCD = 'Изображения Kodak Photo-CD';
  gesCompuserve = 'Изображения CompuServe';
  gesHalo = 'Изображения Dr. Halo';
  gesPaintShopPro = 'Изображения Paintshop Pro';
  gesPaintshopProFrames = 'Paintshop Pro frames';
  gesPaintshopProTubes = 'Paintshop Pro tubes';
  gesPortableNetworkGraphic = 'Изображения Portable network graphic (PNG)';

  // Специфичные ошибки при работе с изображениями
  gesInvalidImage = 'Невозможно загрузить изображение. Неверный или неподдерживаемый формат файла %s.';
  gesInvalidColorFormat = 'Неверный формат цвета в файле %s.';
  gesStreamReadError = 'Ошибка чтения потока при чтении файла %s.';
  gesUnsupportedImage = 'Невозможно загрузить изображение. Неподдерживаемый формат изображения %s.';
  gesUnsupportedFeature = 'Невозможно загрузить изображение. %s не поддерживается для файлов %s.';
  gesInvalidCRC = 'Невозможно загрузить изображение. Неверная контрольная сумма файла %s.';
  gesCompression = 'Невозможно загрузить изображение. Ошибка сжатия в файле %s.';
  gesExtraCompressedData = 'Невозможно загрузить изображение. Файл %s содержит лишние данные.';
  gesInvalidPalette = 'Невозможно загрузить изображение. Неверная палитра в файле %s.';
  gesUnknownCriticalChunk = 'Невозможно загрузить PNG изображение. Формат необходимой секции данных не поддерживается.';

  // Параметры (обычно используются вместе с сообщениями об отсутствии поддержки)
  gesCompressionScheme = 'Схема сжатия:';
  gesRLAPixelFormat = 'Форматы изображений, отличные от RGB and RGBA:';
  gesPSPFileType = 'Версии формата файла, отличные от 3й или 4й:';

  // Ошибки при работе с цветом
  gesIndexedNotSupported = 'Преобразование между индексированными и неиндексированными форматами пикселов не поддерживается.';
  gesConversionUnsupported = 'Невозможно преобразовать цвет. Нет подходящего метода.';
  gesInvalidSampleDepth = 'Неверная глубина цвета. Должно быть 1, 2, 4, 8 или 16 бит на сэмпл.';
  gesInvalidPixelDepth = 'Количество сэмплов на пиксел не соответствует данной цветовой схеме.';
  gesInvalidSubSampling = 'Неверное значение сэмплирования. Допустимы 1, 2 и 4.';
  gesVerticalSubSamplingError = 'Значение вертикального сэмплирования должно быть меньше или равно значению горизонтального сэмплирования.';

  // Состояния
  gesPreparing = 'Подготовка...';
  gesLoadingData = 'Загрузка данных...';
  gesUpsampling = 'Пресэмплирование...';
  gesTransfering = 'Передача...';

  // Ошибки сжатия
  gesLZ77Error = 'Ошибка LZ77-декомпрессии.';
  gesJPEGEOI = 'Ошибка декомпрессии JPEG. Неожиданное окончание данных.';
  gesJPEGStripSize = 'Несоответствующий strip/tile размер JPEG.';
  gesJPEGComponentCount = 'Несоответствующее количество компонент JPEG.';
  gesJPEGDataPrecision = 'Несоответствующая точность данных JPEG.';
  gesJPEGSamplingFactors = 'Несоответствующий фактор сэмплирования JPEG.';
  gesJPEGBogusTableField = 'Ошибочное поле JPEG.';
  gesJPEGFractionalLine = 'Частичные скан-линии JPEG не поддерживаются.';

  // Разное
  gesWarning = 'Внимание!';

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
