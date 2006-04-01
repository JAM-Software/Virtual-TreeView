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
// Translation done by Shashlov Kirill.
//
// Rename the file to GraphicStrings.pas to use it as your favourite language file.
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$I GraphicConfiguration.inc}

resourcestring
  // �������� ������ �����������
  gesAllImages = '��� �����������';
  gesRegistration = '������� ���������������� %s ������.';

  gesBitmaps = '�������� ������� Windows';
  gesRLEBitmaps = '�������� ������� RLE';
  gesDIBs = '���������-����������� �������� ������� Windows';
  gesEPS = '��������� ����������� Postscript';
  gesIcons = '������ Windows';
  gesMetaFiles = '��������� Windows';
  gesEnhancedMetaFiles = '����������� ��������� Windows';
  gesJPGImages = '����������� JPG';
  gesJPEGImages = '����������� JPEG';
  gesJPEImages = '����������� JPE';
  gesJFIFImages = '����������� JFIF';
  gesTruevision = '����������� Truevision';
  gesTIFF = '����������� TIFF';
  gesMacTIFF =  '����������� TIFF ��� Macintosh';
  gesPCTIF = '����������� TIF (PC)';
  gesGFIFax = '����-����������� GFI';
  gesSGI = '����������� SGI';
  gesSGITrueColor = '������������ ����������� SGI';
  gesZSoft = '����������� ZSoft Paintbrush';
  gesZSoftWord = '������ ������ Word 5.x';
  gesAliasWaveFront = '����������� Alias/Wavefront';
  gesSGITrueColorAlpha = '������������ ����������� SGI � �����-�������';
  gesSGIMono = '׸���-����� ����������� SGI';
  gesPhotoshop = '����������� Photoshop';
  gesPortable = '����������� Portable map';
  gesPortablePixel = '����������� Portable pixel map';
  gesPortableGray = '����������� Portable gray map';
  gesPortableMono = '����������� Portable bitmap';
  gesAutoDesk = '����������� Autodesk';
  gesKodakPhotoCD = '����������� Kodak Photo-CD';
  gesCompuserve = '����������� CompuServe';
  gesHalo = '����������� Dr. Halo';
  gesPaintShopPro = '����������� Paintshop Pro';
  gesPaintshopProFrames = 'Paintshop Pro frames';
  gesPaintshopProTubes = 'Paintshop Pro tubes';
  gesPortableNetworkGraphic = '����������� Portable network graphic (PNG)';

  // ����������� ������ ��� ������ � �������������
  gesInvalidImage = '���������� ��������� �����������. �������� ��� ���������������� ������ ����� %s.';
  gesInvalidColorFormat = '�������� ������ ����� � ����� %s.';
  gesStreamReadError = '������ ������ ������ ��� ������ ����� %s.';
  gesUnsupportedImage = '���������� ��������� �����������. ���������������� ������ ����������� %s.';
  gesUnsupportedFeature = '���������� ��������� �����������. %s �� �������������� ��� ������ %s.';
  gesInvalidCRC = '���������� ��������� �����������. �������� ����������� ����� ����� %s.';
  gesCompression = '���������� ��������� �����������. ������ ������ � ����� %s.';
  gesExtraCompressedData = '���������� ��������� �����������. ���� %s �������� ������ ������.';
  gesInvalidPalette = '���������� ��������� �����������. �������� ������� � ����� %s.';
  gesUnknownCriticalChunk = '���������� ��������� PNG �����������. ������ ����������� ������ ������ �� ��������������.';

  // ��������� (������ ������������ ������ � ����������� �� ���������� ���������)
  gesCompressionScheme = '����� ������:';
  gesRLAPixelFormat = '������� �����������, �������� �� RGB and RGBA:';
  gesPSPFileType = '������ ������� �����, �������� �� 3� ��� 4�:';

  // ������ ��� ������ � ������
  gesIndexedNotSupported = '�������������� ����� ���������������� � ������������������ ��������� �������� �� ��������������.';
  gesConversionUnsupported = '���������� ������������� ����. ��� ����������� ������.';
  gesInvalidSampleDepth = '�������� ������� �����. ������ ���� 1, 2, 4, 8 ��� 16 ��� �� �����.';
  gesInvalidPixelDepth = '���������� ������� �� ������ �� ������������� ������ �������� �����.';
  gesInvalidSubSampling = '�������� �������� �������������. ��������� 1, 2 � 4.';
  gesVerticalSubSamplingError = '�������� ������������� ������������� ������ ���� ������ ��� ����� �������� ��������������� �������������.';

  // ���������
  gesPreparing = '����������...';
  gesLoadingData = '�������� ������...';
  gesUpsampling = '����������������...';
  gesTransfering = '��������...';

  // ������ ������
  gesLZ77Error = '������ LZ77-������������.';
  gesJPEGEOI = '������ ������������ JPEG. ����������� ��������� ������.';
  gesJPEGStripSize = '����������������� strip/tile ������ JPEG.';
  gesJPEGComponentCount = '����������������� ���������� ��������� JPEG.';
  gesJPEGDataPrecision = '����������������� �������� ������ JPEG.';
  gesJPEGSamplingFactors = '����������������� ������ ������������� JPEG.';
  gesJPEGBogusTableField = '��������� ���� JPEG.';
  gesJPEGFractionalLine = '��������� ����-����� JPEG �� ��������������.';

  // ������
  gesWarning = '��������!';

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
