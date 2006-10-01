unit GraphicEx;

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is GraphicEx.pas, released November 1, 1999.
//
// The initial developer of the original code is Mike Lischke (www.soft-gems.net),
//
// Portions created by Mike Lischke are
// Copyright (C) 1999-2005 Mike Lischke. All Rights Reserved.
//
// Credits:
//   Haukur K. Bragason, Ingo Neumann, Craig Peterson
//----------------------------------------------------------------------------------------------------------------------
//
// See help file for a description of supported image formats.
//
// Version II.1.16
//
// Note: This library can be compiled with Delphi 5 or newer versions.
//
//----------------------------------------------------------------------------------------------------------------------
//
// October 2006
//   - Bug fix: 16 bpp SGI images loading failed
// August 2005
//   - Bug fix: added exceptions for PCX and PCD images in case they cannot be read.
// December 2005
//   - Bug fix: The filter string returned for open dialogs was incorrect, which caused missing files in the dialog.
// November 2005
//   - Bug fix: correct handling of 256 colors in PPM files.
// October 2005
//   - Bug fix: Passing dynamic arrays of zero size to functions using the @ operator fails.
// February 2005
//   - Bug fix: Line offset in TIFF did not consider partly used source bytes (BPP 4 and lower).
//
// January 2005:
//   - Bug fix: color manager must be used for new TIFF reader.
//   - Bug fix: CompBuffer in PSD loader must be set to nil initially.
//   - Bug fix: DoStretch working bitmap is not thread safe, needs Canvas.Lock/Unlock.
//   - Improvement: New standalone function ReadImageProperties.
//
// See help file for a full development history.
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$I GraphicConfiguration.inc}
{$I Compilers.inc}

{$ifdef COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
  // We switch off warnings about that fact. We know it and we accept it.
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CAST off}
  {$warn UNSAFE_CODE off}
{$endif COMPILER_7_UP}

uses
  Windows, Classes, ExtCtrls, Graphics, SysUtils, Contnrs, JPG, TIFF,
  GraphicCompression, GraphicStrings, GraphicColor;

const
  GraphicExVersion = 'II.1.16';

type
  TCardinalArray = array of Cardinal;
  TByteArray = array of Byte;
  TFloatArray = array of Single;

  TImageOptions = set of (
    ioTiled,       // image consists of tiles not strips (TIF)
    ioBigEndian,   // byte order in values >= words is reversed (TIF, RLA, SGI)
    ioMinIsWhite,  // minimum value in grayscale palette is white not black (TIF)
    ioReversed,    // bit order in bytes is reveresed (TIF)
    ioUseGamma     // gamma correction is used
  );

  // describes the compression used in the image file
  TCompressionType = (
    ctUnknown,          // Compression type is unknown.
    ctNone,             // No compression.
    ctRLE,              // Run length encoding.
    ctPackedBits,       // Macintosh packed bits.
    ctLZW,              // Lempel-Zif-Welch.
    ctFax3,             // CCITT T.4 (1D), also known as fax group 3.
    ct2DFax3,           // CCITT T.4 (2D).
    ctFaxRLE,           // Modified Huffman (CCITT T.4 derivative).
    ctFax4,             // CCITT T.6, also known as fax group 4.
    ctFaxRLEW,          // CCITT T.4 with word alignment.
    ctLZ77,             // Hufman inflate/deflate.
    ctJPEG,             // TIF JPEG compression (new version)
    ctOJPEG,            // TIF JPEG compression (old version)
    ctThunderscan,      // TIF thunderscan compression
    ctNext,
    ctIT8CTPAD,
    ctIT8LW,
    ctIT8MP,
    ctIT8BL,
    ctPixarFilm,
    ctPixarLog,
    ctDCS,
    ctJBIG,
    ctPCDHuffmann,      // PhotoCD Hufman compression
    ctPlainZip,         // ZIP compression without prediction
    ctPredictedZip,     // ZIP comression with prediction
    ctSGILog,           // SGI Log Luminance RLE
    ctSGILog24          // SGI Log 24-bit packed
  );

  // properties of a particular image which are set while loading an image or when
  // they are explicitly requested via ReadImageProperties
  PImageProperties = ^TImageProperties;
  TImageProperties = record
    Version: Cardinal;                 // TIF, PSP, GIF
    Options: TImageOptions;            // all images
    Width,                             // all images
    Height: Integer;                   // all images
    ColorScheme: TColorScheme;         // all images
    BitsPerSample,                     // all Images
    SamplesPerPixel,                   // all images
    BitsPerPixel: Byte;                // all images
    Compression: TCompressionType;     // all images
    FileGamma: Single;                 // RLA, PNG
    XResolution,
    YResolution: Single;               // given in dpi (TIF, PCX, PSP)
    Interlaced,                        // GIF, PNG
    HasAlpha: Boolean;                 // TIF, PNG
    ImageCount: Cardinal;              // Number of subimages (PCD, TIF, GIF, MNG).
    Comment: string;                   // Implemented for PNG and GIF.

    // Informational data, used internally and/or by decoders
    // PCD
    Overview: Boolean;                 // true if image is an overview image
    Rotate: Byte;                      // describes how the image is rotated (aka landscape vs. portrait image)

    // GIF
    LocalColorTable: Boolean;          // image uses an own color palette instead of the global one

    // RLA
    BottomUp: Boolean;                 // images is bottom to top

    // PNG
    FilterMode: Byte;

    // TIFF
    Orientation: Word;                 
  end;

  // This mode is used when creating a file mapping. See TFileMapping.
  TFileMappingMode = (
    fmmCreateNew,       // Always create a new file (overwrite any existing). Implicitely gives read/write access.
    fmmOpenOrCreate,    // Open if file exists (implicitely gives read/write access) or create if it does not.
    fmmReadOnly,        // Open existing file read only.
    fmmReadWrite        // Open existing file with read and write access.
  );

  // This class is used to provide direct (mapped) memory access to a file.
  // It is optimized for use in GraphicEx (sequential access).
  TFileMapping = class
  private
    FFileName: string;
    FFileHandle,
    FFileMapping: THandle;
    FFileSize: Int64;
    FMemory: Pointer;
  public
    constructor Create(const FileName: string; Mode: TFileMappingMode); overload;
    constructor Create(Stream: THandleStream); overload;
    destructor Destroy; override;

    property FileName: string read FFileName;
    property Memory: Pointer read FMemory;
    property Size: Int64 read FFileSize;
  end;

  // This is the base class for all image types implemented in GraphicEx.
  // It contains some generally used stuff.
  TGraphicExGraphic = class(TBitmap)
  private
    FColorManager: TColorManager;
    FImageProperties: TImageProperties;

    // Advanced progress display support.
    FProgressStack: TStack;       // Used to manage nested progress sections.
    FProgressRect: TRect;
    FPercentDone: Single;         // Progress over all parts of the load process.
  protected
    Decoder: TDecoder;            // The decoder used to decompress the image data.

    procedure AdvanceProgress(Amount: Single; OffsetX, OffsetY: Integer; DoRedraw: Boolean);
    procedure ClearProgressStack;
    procedure FinishProgressSection(DoRedraw: Boolean);
    procedure InitProgress(Width, Height: Integer);
    procedure StartProgressSection(Size: Single; const S: string);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    class function CanLoad(const FileName: string): Boolean; overload; 
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; overload; virtual;
    class function CanLoad(Stream: TStream): Boolean; overload;
    procedure LoadFromFile(const FileName: string); override;
    procedure LoadFromFileByIndex(const FileName: string; ImageIndex: Cardinal = 0);
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); virtual;
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer; ImageIndex: Cardinal = 0);
    procedure LoadFromResourceName(Instance: THandle; const ResName: string; ImageIndex: Cardinal = 0);
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromStreamByIndex(Stream: TStream; ImageIndex: Cardinal = 0);
    function ReadImageProperties(const Name: string; ImageIndex: Cardinal): Boolean; overload; virtual;
    function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; overload; virtual;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; overload; virtual;

    property ColorManager: TColorManager read FColorManager;
    property ImageProperties: TImageProperties read FImageProperties;
  end;

  TGraphicExGraphicClass = class of TGraphicExGraphic;
   
  {$ifdef AutodeskGraphic}
  // *.cel, *.pic images
  TAutodeskGraphic = class(TGraphicExGraphic)
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif AutodeskGraphic}

  {$ifdef SGIGraphic}
  // *.bw, *.rgb, *.rgba, *.sgi images
  TSGIGraphic = class(TGraphicExGraphic)
  private
    FRowStart,
    FRowSize: TCardinalArray;    // Start and compressed length of the lines if the image is compressed.
    procedure GetComponents(const Memory: Pointer; var Red, Green, Blue, Alpha: Pointer; Row: Integer);
    procedure ReadAndDecode(const Memory: Pointer; Red, Green, Blue, Alpha: Pointer; Row: Integer; BPC: Cardinal);
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif SGIGraphic}

  {$ifdef TIFFGraphic}
  // *.tif, *.tiff images
  TTIFFGraphic = class(TGraphicExGraphic)
  private
    FMemory: PByte;
    FCurrentPointer: PByte;
    FSize: Int64;
  protected
    procedure ReadContiguous(tif: PTIFF);
    procedure ReadTiled(tif: PTIFF);
    function SetOrientation(tif: PTIFF; H: Cardinal): Cardinal;
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size:Int64; ImageIndex: Cardinal): Boolean; override;
  end;

    {$ifdef EPSGraphic}
    TEPSGraphic = class(TTIFFGraphic)
    public
      class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
      procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
      function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; override;
    end;
    {$endif EPSGraphic}
  {$endif TIFFGraphic}

  {$ifdef TargaGraphic}
  // *.tga; *.vst; *.icb; *.vda; *.win images
  TTargaGraphic = class(TGraphicExGraphic)
   public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
    procedure SaveToStream(Stream: TStream); overload; override;
    procedure SaveToStream(Stream: TStream; Compressed: Boolean); reintroduce; overload;
  end;
  {$endif TargaGraphic}

  {$ifdef PCXGraphic}
  // *.pcx; *.pcc; *.scr images
  // Note: Due to the badly designed format a PCX/SCR file cannot be part in a larger stream because the position of the
  //       color palette as well as the decoding size can only be determined by the size of the image.
  //       Hence the image must be the only one in the stream or the last one.
  TPCXGraphic = class(TGraphicExGraphic)
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif PCXGraphic}

  {$ifdef PCDGraphic}
  // *.pcd images
  // Note: By default the BASE resolution of a PCD image is loaded with LoadFromStream. 
  TPCDGraphic = class(TGraphicExGraphic)
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 2); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif PCDGraphic}

  {$ifdef PortableMapGraphic}
  // *.ppm, *.pgm, *.pbm images
  TPPMGraphic = class(TGraphicExGraphic)
  private
    FSource: PChar;
    FRemainingSize: Int64;
    function GetChar: Char;
    function GetNumber: Cardinal;
    function ReadLine: string;
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif PortableMapGraphic}

  {$ifdef CUTGraphic}
  // *.cut (+ *.pal) images
  // Note: Also this format should not be used in a stream unless it is the only image or the last one!
  TCUTGraphic = class(TGraphicExGraphic)
  private
    FPaletteFile: string;
  protected
    procedure LoadPalette;
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromFile(const FileName: string); override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;

    property PaletteFile: string read FPaletteFile write FPaletteFile;
  end;
  {$endif CUTGraphic}

  {$ifdef GIFGraphic}
  // *.gif images
  TGIFGraphic = class(TGraphicExGraphic)
  private
    FSource: PByte;
    FTransparentIndex: Byte;
    function SkipExtensions: Byte;
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif GIFGraphic}

  {$ifdef RLAGraphic}
  // *.rla, *.rpf images
  // Implementation based on code from Dipl. Ing. Ingo Neumann (ingo@delphingo.com).
  TRLAGraphic = class(TGraphicExGraphic)
  private
    procedure SwapHeader(var Header); // start position of the image header in the stream
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif RLAGraphic}

  {$ifdef PhotoshopGraphic}
  // *.psd, *.pdd images
  TPSDLayerBlendMode = (
    lbmNormal,
    lbmDarken,
    lbmLighten,
    lbmHue,
    lbmSaturation,
    lbmColor,
    lbmLuminosity,
    lbmMultiply,
    lbmScreen,
    lbmDissolve,
    lbmOverlay,
    lbmHardLight,
    lbmSoftLight,
    lbmDifference,
    lbmExclusion,
    lbmColorDodge,
    lbmColorBurn
  );

  TPSDLayerClipping = (
    lcBase,
    lcNonBase
  );

  TPSDLayerOptions = set of (
    loTransparencyProtected,
    loHidden,
    loIrrelevantData
  );

  TPSDLayerType = (
    ltBitmap,
    ltText,
    ltMask
  );

  // Flags used for mask data in a Photoshop layer.
  TPSDLayerMaskFlags = set of (
    lmfRelativePosition,     // Position of mask is relative to layer.
    lmfMaskDisabled,         // The layer mask is disabled.
    lmfInvertMask            // Invert layer mask when blending.
  );

  TPSDLayerMaskData = record
    Bounds: TRect;
    DefaultColor: Byte;
    Flags: TPSDLayerMaskFlags;
    UserMaskBackground: Byte;
  end;

  // Currently no info is available for data in this block.
  TPSDCompositeGrayBlend = record
    Black1,
    Black2,
    White1,
    White2: Byte;
  end;

  // Data specific to one channel in a layer.
  // Pixel data is not stored separately for each channel but the layer as a whole.
  TPSDChannel = record
    ChannelID: SmallInt;
    Size: Cardinal;               // Size of channel data when loading or storing.
    BlendSourceRange,
    BlendTargetRange: TPSDCompositeGrayBlend;
    Data: Pointer;                // Temporary storage for the channel's pixel data.
  end;

  // Each layer has a collection of channel data.
  TPSDChannels = array of TPSDChannel;

  // Indirect type declaration here to allow recursive item data structure.
  PPSDItemList = ^TPSDItemList;
  PPSDDescriptor = ^TPSDDescriptor;

  TPSDItemData = record
    ItemType: Cardinal;           // Type of the item. See ReadDescriptor for a list of possible values.
    ClassID: WideString;          // Only valid if property or class item.
    KeyID: WideString;            // Only valid if property or string item.
    Name: WideString;             // Only valid if name or identifier item.
    Units: Integer;               // Only valid if Unit float item.
    Value: Double;                // Only valid if Unit float or double item.
    TypeID: WideString;           // Only valid if enumeration item.
    EnumValue: WideString;        // Only valid if enumeration item.
    Offset: Cardinal;             // Only valid if offset item.
    IntValue: Integer;            // Only valid if integer item.
    BoolValue: Boolean;           // Only valid if boolean item.
    List: PPSDItemList;           // Only valid if (reference) list or item.
    DataSize: Cardinal;           // Only valid if raw data.
    Data: Pointer;                // Only valid if raw data.
    Descriptor: PPSDDescriptor;   // Only valid if the item is again a PSD descriptor.
  end;
  TPSDItemList = array of TPSDItemData;

  // One entry in a PSD descriptor stored as part of e.g. the type tool adjustment layer.
  TPSDDescriptorItem = record
    Key: string;                  // Item name.
    Data: TPSDItemData;           // The value of the item.
  end;

  TPSDDescriptor = record
    ClassID,
    ClassID2: WideString;
    Items: array of TPSDDescriptorItem;
  end;

  TDoubleRect = record
    Left, Top, Right, Bottom: Double;
  end;

  TTypeTransform = record
    XX, XY, YX, YY, TX, TY: Double;
  end;

  TPSDTypeToolInfo = record
    Transform: TTypeTransform;
    TextDescriptor,
    WarpDescriptor: TPSDDescriptor;
    WarpRectangle: TDoubleRect;
  end;
  
  TPSDGraphic = class;
  
  TPhotoshopLayer = class
  private
    FGraphic: TPSDGraphic;
    FBounds: TRect;
    FBlendMode: TPSDLayerBlendMode;
    FOpacity: Byte;                    // 0 = transparent ... 255 = opaque
    FClipping: TPSDLayerClipping;
    FOptions: TPSDLayerOptions;
    FMaskData: TPSDLayerMaskData;
    FCompositeGrayBlendSource,
    FCompositeGrayBlendDestination: TPSDCompositeGrayBlend;
    FChannels: TPSDChannels;
    FName: WideString;
    FImage: TBitmap;
    FType: TPSDLayerType;
    FTypeToolInfo: TPSDTypeToolInfo;   // Only valid if layer is a text layer.
    procedure SetImage(const Value: TBitmap);
  public
    constructor Create(Graphic: TPSDGraphic);
    destructor Destroy; override;

    property BlendMode: TPSDLayerBlendMode read FBlendMode write FBlendMode;
    property Bounds: TRect read FBounds write FBounds;
    property Channels: TPSDChannels read FChannels write FChannels;
    property Clipping: TPSDLayerClipping read FClipping write FClipping;
    property CompositeGrayBlendDestination: TPSDCompositeGrayBlend read FCompositeGrayBlendDestination
      write FCompositeGrayBlendDestination;
    property CompositeGrayBlendSource: TPSDCompositeGrayBlend read FCompositeGrayBlendSource
      write FCompositeGrayBlendSource;
    property Image: TBitmap read FImage write SetImage;
    property LayerType: TPSDLayerType read FType;
    property MaskData: TPSDLayerMaskData read FMaskData write FMaskData;
    property Name: WideString read FName write FName;
    property Opacity: Byte read FOpacity write FOpacity;
    property Options: TPSDLayerOptions read FOptions write FOptions;
  end;

  TPhotoshopLayers = class(TList)
  private
    FGraphic: TPSDGraphic;
    // The following fields are read from the global layer mask info data but
    // their meaning is not well documented or at least obvious from their names.
    FOverlayColorSpace: Word;               // undocumented
    FColorComponents: array[0..3] of Word;  // undocumented
    FLayerMaskOpacity: Word;                // 0 = transparent, 100 = opaque
    FKind: Byte;                            // 0 = Color selected, 1 = Color protected, 128 = use value stored per layer.
                                            // The last one is preferred. The others are for backward compatibility.
  protected
    function Get(Index: Integer): TPhotoshopLayer;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure Put(Index: Integer; Layer: TPhotoshopLayer);
  public
    constructor Create(Graphic: TPSDGraphic);

    function Add(Layer: TPhotoshopLayer): Integer;
    function AddNewLayer: TPhotoshopLayer;
    function Extract(Layer: TPhotoshopLayer): TPhotoshopLayer;
    function First: TPhotoshopLayer;
    function IndexOf(Layer: TPhotoshopLayer): Integer;
    procedure Insert(Index: Integer; Layer: TPhotoshopLayer);
    function Last: TPhotoshopLayer;
    function Remove(Layer: TPhotoshopLayer): Integer;

    property Items[Index: Integer]: TPhotoshopLayer read Get write Put; default;
  end;

  TPSDGuide = record
    Location: Single;        // Either X or Y coordinate of the guide depending on IsHorizontal.
    IsHorizontal: Boolean;   // True if it is a horizontal guide, otherwise False.
  end;
  
  TPSDGridSettings = record
    HorizontalCycle,         // Number of dots per cycle relative to 72 dpi.
    VerticalCycle: Single;
    Guides: array of TPSDGuide;
  end;

  TPSDGraphic = class(TGraphicExGraphic)
  private
    FChannels,     // Original channel count of the image (1..24).
    FMode: Word;   // Original color mode of the image (PSD_*).
    FLayers: TPhotoshopLayers;
    FGridSettings: TPSDGridSettings;
  protected
    procedure CombineChannels(Layer: TPhotoshopLayer);
    function ConvertCompression(Value: Word): TCompressionType;
    function DetermineColorScheme(ChannelCount: Integer): TColorScheme;
    procedure LoadAdjustmentLayer(var Run: PChar; Layer: TPhotoshopLayer);
    procedure ReadChannelData(var Run: PChar; var Channel: TPSDChannel; Width, Height: Integer; IsIrrelevant: Boolean);
    procedure ReadDescriptor(var Run: PChar; var Descriptor: TPSDDescriptor);
    procedure ReadMergedImage(var Source: PChar; Layer: TPhotoshopLayer; Compression: TCompressionType; Channels: Byte);
    procedure ReadLayers(Run: PChar);
    procedure ReadResources(Run: PChar);
    function SetupColorManager(Channels: Integer): TPixelFormat;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;

    property GridSettings: TPSDGridSettings read FGridSettings;
    property Layers: TPhotoshopLayers read FLayers;
  end;
  {$endif PhotoshopGraphic}

  {$ifdef PaintshopProGraphic}
  // *.psp images (file version 3 and 4)
  TPSPGraphic = class(TGraphicExGraphic)
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif PaintshopProGraphic}

  {$ifdef PortableNetworkGraphic}
  // *.png images
  TChunkType = array[0..3] of Char;

  // This header is followed by a variable number of data bytes, which are followed by the CRC for this data.
  // The actual size of this data is given by field length in the chunk header.
  // CRC is Cardinal (4 byte unsigned integer).
  TPNGChunkHeader = packed record
    Length: Cardinal;  // size of data (entire chunk excluding itself, CRC and type)
    ChunkType: TChunkType;
  end;

  TPNGGraphic = class(TGraphicExGraphic)
  private
    FIDATSize: Integer;        // remaining bytes in the current IDAT chunk
    FRawBuffer,                // buffer to load raw chunk data and to check CRC
    FCurrentSource: Pointer;   // points into FRawBuffer for current position of decoding
    FHeader: TPNGChunkHeader;  // header of the current chunk
    FCurrentCRC: Cardinal;     // running CRC for the current chunk
    FSourceBPP: Integer;       // bits per pixel used in the file
    FPalette: HPALETTE;        // used to hold the palette handle until we can set it finally after the pixel format
                               // has been set too (as this destroys the current palette)
    FTransparency: TByteArray; // If the image is indexed then this array might contain alpha values (depends on file)
                               // each entry corresponding to the same palette index as the index in this array.
                               // For grayscale and RGB images FTransparentColor contains the (only) transparent
                               // color.
    FTransparentColor: TColor; // transparent color for gray and RGB
    FBackgroundColor: TColor;  // index or color ref
    procedure ApplyFilter(Filter: Byte; Line, PrevLine, Target: PByte; BPP, BytesPerRow: Integer);
    function IsChunk(ChunkType: TChunkType): Boolean;
    function LoadAndSwapHeader(var Source: PByte): Cardinal;
    procedure LoadBackgroundColor(var Source: PByte; const Description);
    procedure LoadIDAT(var Source: PByte; const Description);
    procedure LoadText(var Source: PByte);
    procedure LoadTransparency(var Source: PByte; const Description);
    procedure ReadDataAndCheckCRC(var Source: PByte);
    procedure ReadRow(var Source: PByte; RowBuffer: Pointer; BytesPerRow: Integer);
    function SetupColorDepth(ColorType, BitDepth: Integer): Integer;
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;

    property BackgroundColor: TColor read FBackgroundColor;
    property Transparency: TByteArray read FTransparency;
  end;
  {$endif PortableNetworkGraphic}

  {$ifdef ArtsAndLettersGraphic}
  // *.ged images (Arts & Letters images)
  TGEDGraphic = class(TGraphicExGraphic)
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif ArtsAndLettersGraphic}

  // ---------- file format management stuff
  TFormatType = (
    ftAnimation,   // format contains an animation (like GIF or AVI)
    ftLayered,     // format supports multiple layers (like PSP, PSD)
    ftMultiImage,  // format can contain more than one image (like TIF or GIF)
    ftRaster,      // format is contains raster data (this is mainly used)
    ftVector       // format contains vector data (like DXF or PSP file version 4)
  );
  TFormatTypes = set of TFormatType;

  TFilterSortType = (
    fstNone,        // do not sort entries, list them as they are registered
    fstBoth,        // sort entries first by description then by extension
    fstDescription, // sort entries by description only
    fstExtension    // sort entries by extension only
  );

  TFilterOption = (
    foCompact,          // use the compact form in filter strings instead listing each extension on a separate line
    foIncludeAll,       // include the 'All image files' filter string
    foIncludeExtension  // add the extension to the description
  );
  TFilterOptions = set of TFilterOption;

  // The file format list is an alternative to Delphi's own poor implementation which does neither allow to filter
  // graphic formats nor to build common entries in filter strings nor does it care for duplicate entries or
  // alphabetic ordering. Additionally, some properties are maintained for each format to do searches, filter particular
  // formats for a certain case etc.
  TFileFormatList = class
  private
    FClassList,
    FExtensionList: TList;
  protected
    function FindExtension(const Extension: string): Integer;
    function FindGraphicClass(GraphicClass: TGraphicClass): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function GetDescription(Graphic: TGraphicClass): string;
    procedure GetExtensionList(List: TStrings);
    function GetGraphicFilter(Formats: TFormatTypes; SortType: TFilterSortType; Options: TFilterOptions;
      GraphicClass: TGraphicClass): string;
    function GraphicFromExtension(S: string): TGraphicClass;
    function GraphicFromContent(const FileName: string): TGraphicExGraphicClass; overload;
    function GraphicFromContent(const Memory: Pointer; Size: Int64): TGraphicExGraphicClass; overload;
    function GraphicFromContent(Stream: TStream): TGraphicExGraphicClass; overload;
    procedure RegisterFileFormat(const Extension, Common, Individual: string; FormatTypes: TFormatTypes;
      Replace: Boolean; GraphicClass: TGraphicClass);
    procedure UnregisterFileFormat(const Extension: string; GraphicClass: TGraphicClass);
  end;

  // Resampling support types.
  TResamplingFilter = (
    sfBox,
    sfTriangle,
    sfHermite,
    sfBell,
    sfSpline,
    sfLanczos3,
    sfMitchell
  );

procedure GraphicExError(ErrorString: string); overload;
procedure GraphicExError(ErrorString: string; Args: array of const); overload;

// Resampling support routines.
procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter; Radius: Single; Source, Target: TBitmap); overload;
procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter; Radius: Single; Source: TBitmap); overload;

function ReadImageProperties(const FileName: string; var Properties: TImageProperties): Boolean;

var
  FileFormatList: TFileFormatList;
  
//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Consts, Math, zLibEx;

type
  {$ifndef COMPILER_6_UP}
    PCardinal = ^Cardinal;
  {$endif COMPILER_6_UP}
  // resampling support types
  TRGBInt = record
    R, G, B: Integer;
  end;

  TRGBAInt = record
    R, G, B, A: Integer;
  end;

  PRGBWord = ^TRGBWord;
  TRGBWord = record
    R, G, B: Word;
  end;

  PRGBAWord = ^TRGBAWord;
  TRGBAWord = record
    R, G, B, A: Word;
  end;

  PBGR = ^TBGR;
  TBGR = packed record
    B, G, R: Byte;
  end;

  PBGRA = ^TBGRA;
  TBGRA = packed record
    B, G, R, A: Byte;
  end;

  PRGB = ^TRGB;
  TRGB = packed record
    R, G, B: Byte;
  end;

  PRGBA = ^TRGBA;
  TRGBA = packed record
    R, G, B, A: Byte;
  end;

  PPixelArray = ^TPixelArray;
  TPixelArray = array[0..0] of TBGR;

  TFilterFunction = function(Value: Single): Single;

  // contributor for a Pixel
  PContributor = ^TContributor;
  TContributor = record
    Weight: Integer; // Pixel Weight
    Pixel: Integer; // Source Pixel
  end;

  TContributors = array of TContributor;

  // list of source pixels contributing to a destination pixel
  TContributorEntry = record
    N: Integer;
    Contributors: TContributors;
  end;

  TContributorList = array of TContributorEntry;

  // An entry of the progress stack for nested progress sections.
  PProgressSection = ^TProgressSection;
  TProgressSection = record
    Position,                     // Current position in percent.
    ParentSize,                   // Size of this section in the context of the parent section (in %).
    TransformFactor: Single;      // Accumulated factor to transform a step in this section to an overall value.
    Message: string;              // Message to display for this section.
  end;
  
const
  DefaultFilterRadius: array[TResamplingFilter] of Single = (0.5, 1, 1, 1.5, 2, 3, 2);

threadvar // globally used cache for current image (speeds up resampling about 10%)
  CurrentLineR: array of Integer;
  CurrentLineG: array of Integer;
  CurrentLineB: array of Integer;
  CurrentLineA: array of Integer;

//----------------------------------------------------------------------------------------------------------------------

{$ifndef COMPILER_6_UP}

  procedure RaiseLastOSError;

  begin
    RaiseLastWin32Error;
  end;

{$endif}

//----------------------------------------------------------------------------------------------------------------------

procedure GraphicExError(ErrorString: string); overload;

begin
  raise EInvalidGraphic.Create(ErrorString);
end;                                                  

//----------------------------------------------------------------------------------------------------------------------

procedure GraphicExError(ErrorString: string; Args: array of const); overload;

begin
  raise EInvalidGraphic.CreateFmt(ErrorString, Args);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Upsample(Width, Height, ScaledWidth: Cardinal; Pixels: PChar);

// Creates a new image that is a integral size greater than an existing one.

var
  X, Y: Cardinal;
  P, Q, R: PChar;

begin
  for Y := 0 to Height - 1 do
  begin
    P := Pixels + (Height - 1 - Y) * ScaledWidth + (Width - 1);
    Q := Pixels + ((Height - 1 - Y) shl 1) * ScaledWidth + ((Width - 1) shl 1);
    Q^ := P^;
    (Q + 1)^ := P^;
    for X := 1 to Width - 1 do
    begin
      Dec(P);
      Dec(Q, 2);
      Q^ := P^;
      (Q + 1)^ := Char((Word(P^) + Word((P + 1)^) + 1) shr 1);
    end;
  end;            

  for Y := 0 to Height - 2 do
  begin
    P := Pixels + (Y shl 1) * ScaledWidth;
    Q := P + ScaledWidth;
    R := Q + ScaledWidth;
    for X := 0 to Width - 2 do
    begin
      Q^ := Char((Word(P^) + Word(R^) + 1) shr 1);
      (Q + 1)^ := Char((Word(P^) + Word((P + 2)^) + Word(R^) + Word((R + 2)^) + 2) shr 2);
      Inc(Q, 2);
      Inc(P, 2);
      Inc(R, 2);
    end;
    Q^ := Char((Word(P^) + Word(R^) + 1) shr 1);
    Inc(P);
    Inc(Q);
    Q^ := Char((Word(P^) + Word(R^) + 1) shr 1);
  end;
  P := Pixels + (2 * Height - 2) * ScaledWidth;
  Q := Pixels + (2 * Height - 1) * ScaledWidth;
  Move(P^, Q^, 2 * Width);
end;

//----------------- filter functions for stretching --------------------------------------------------------------------

function HermiteFilter(Value: Single): Single;

// f(t) = 2|t|^3 - 3|t|^2 + 1, -1 <= t <= 1

begin
  if Value < 0 then
    Value := -Value;
  if Value < 1 then
    Result := (2 * Value - 3) * Sqr(Value) + 1
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function BoxFilter(Value: Single): Single;

// This filter is also known as 'nearest neighbour' Filter.

begin
  if (Value > -0.5) and (Value <= 0.5) then
    Result := 1
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TriangleFilter(Value: Single): Single;

// aka 'linear' or 'bilinear' filter

begin
  if Value < 0 then
    Value := -Value;
  if Value < 1 then
    Result := 1 - Value
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function BellFilter(Value: Single): Single;

begin
  if Value < 0 then
    Value := -Value;
  if Value < 0.5 then
    Result := 0.75 - Sqr(Value)
  else
    if Value < 1.5 then
    begin
      Value := Value - 1.5;
      Result := 0.5 * Sqr(Value);
    end
    else
      Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function SplineFilter(Value: Single): Single;

// B-spline filter

var
  Temp: Single;

begin
  if Value < 0 then
    Value := -Value;
  if Value < 1 then
  begin
    Temp := Sqr(Value);
    Result := 0.5 * Temp * Value - Temp + 2 / 3;
  end
  else
    if Value < 2 then
    begin
      Value := 2 - Value;
      Result := Sqr(Value) * Value / 6;
    end
    else
      Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function Lanczos3Filter(Value: Single): Single;

  //--------------- local function --------------------------------------------

  function SinC(Value: Single): Single;

  begin
    if Value <> 0 then
    begin
      Value := Value * Pi;
      Result := Sin(Value) / Value;
    end
    else
      Result := 1;
  end;

  //---------------------------------------------------------------------------

begin
  if Value < 0 then
    Value := -Value;
  if Value < 3 then
    Result := SinC(Value) * SinC(Value / 3)
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function MitchellFilter(Value: Single): Single;

const
  B = 1 / 3;
  C = 1 / 3;

var Temp: Single;

begin
  if Value < 0 then
    Value := -Value;
  Temp := Sqr(Value);
  if Value < 1 then
  begin
    Value := (((12 - 9 * B - 6 * C) * (Value * Temp))
             + ((-18 + 12 * B + 6 * C) * Temp)
             + (6 - 2 * B));
    Result := Value / 6;
  end
  else
    if Value < 2 then
    begin
      Value := (((-B - 6 * C) * (Value * Temp))
               + ((6 * B + 30 * C) * Temp)
               + ((-12 * B - 48 * C) * Value)
               + (8 * B + 24 * C));
      Result := Value / 6;
    end
    else
      Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

const
  FilterList: array[TResamplingFilter] of TFilterFunction = (
    BoxFilter,
    TriangleFilter,
    HermiteFilter,
    BellFilter,
    SplineFilter,
    Lanczos3Filter,
    MitchellFilter
  );

//----------------------------------------------------------------------------------------------------------------------

procedure FillLineCache(N, Delta: Integer; Line: Pointer; UseAlphaChannel: Boolean);

var
  I: Integer;
  Run: PBGRA;

begin
  Run := Line;
  if UseAlphaChannel then
  begin
    for I := 0 to N - 1 do
    begin
      CurrentLineR[I] := Run.R;
      CurrentLineG[I] := Run.G;
      CurrentLineB[I] := Run.B;
      CurrentLineA[I] := Run.A;
      Inc(PByte(Run), Delta);
    end;
  end
  else
  begin
    for I := 0 to N - 1 do
    begin
      CurrentLineR[I] := Run.R;
      CurrentLineG[I] := Run.G;
      CurrentLineB[I] := Run.B;
      Inc(PByte(Run), Delta);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function ApplyContributors(N: Integer; Contributors: TContributors; UseAlphaChannel: Boolean): TBGRA;

var
  J: Integer;
  RGB: TRGBAInt;
  Total,
  Weight: Integer;
  Pixel: Cardinal;
  Contr: ^TContributor;

begin
  RGB.R := 0;
  RGB.G := 0;
  RGB.B := 0;
  RGB.A := 0;
  Total := 0;
  Contr := @Contributors[0];

  if UseAlphaChannel then
  begin
    for J := 0 to N - 1 do
    begin
      Weight := Contr.Weight;
      Inc(Total, Weight);
      Pixel := Contr.Pixel;
      Inc(RGB.R, CurrentLineR[Pixel] * Weight);
      Inc(RGB.G, CurrentLineG[Pixel] * Weight);
      Inc(RGB.B, CurrentLineB[Pixel] * Weight);
      Inc(RGB.A, CurrentLineA[Pixel] * Weight);

      Inc(Contr);
    end;
  end
  else
  begin
    for J := 0 to N - 1 do
    begin
      Weight := Contr.Weight;
      Inc(Total, Weight);
      Pixel := Contr.Pixel;
      Inc(RGB.R, CurrentLineR[Pixel] * Weight);
      Inc(RGB.G, CurrentLineG[Pixel] * Weight);
      Inc(RGB.B, CurrentLineB[Pixel] * Weight);

      Inc(Contr);
    end;
  end;

  if Total = 0 then
  begin
    Result.R := ClampByte(RGB.R shr 8);
    Result.G := ClampByte(RGB.G shr 8);
    Result.B := ClampByte(RGB.B shr 8);
    if UseAlphaChannel then
      Result.A := ClampByte(RGB.A shr 8);
  end
  else
  begin
    Result.R := ClampByte(RGB.R div Total);
    Result.G := ClampByte(RGB.G div Total);
    Result.B := ClampByte(RGB.B div Total);
    if UseAlphaChannel then
      Result.A := ClampByte(RGB.A div Total);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure DoStretch(Filter: TFilterFunction; Radius: Single; Source, Target: TBitmap);

// This is the actual scaling routine. Target must be allocated already with sufficient size. Source must
// contain valid data, Radius must not be 0 and Filter must not be nil.

var
  ScaleX,
  ScaleY: Single;   // Zoom scale factors
  I, J,
  K, N: Integer;    // Loop variables
  Center: Single;   // Filter calculation variables
  Width: Single;
  Weight: Integer;  // Filter calculation variables
  Left,
  Right: Integer;   // Filter calculation variables
  Work: TBitmap;
  ContributorList: TContributorList;

  SourceLine, DestLine: Pointer; // either points to BGR or BGRA structure
  DestPixel: Pointer;
  UseAlphaChannel: Boolean;
  PixelSize: Integer;
  ContributorResult: TBGRA;

  Delta, DestDelta: Integer;
  SourceHeight,
  SourceWidth,
  TargetHeight,
  TargetWidth: Integer;

begin
  // shortcut variables
  SourceHeight := Source.Height;
  SourceWidth := Source.Width;
  TargetHeight := Target.Height;
  TargetWidth := Target.Width;

  if (SourceHeight = 0) or (SourceWidth = 0) or
     (TargetHeight = 0) or (TargetWidth = 0) then
    Exit;

  UseAlphaChannel := Source.PixelFormat = pf32Bit;

  // Create intermediate image to hold horizontal zoom.
  Work := TBitmap.Create;
  Work.Canvas.Lock;
  try
    Work.PixelFormat := Source.PixelFormat;

    Work.Height := SourceHeight;
    Work.Width := TargetWidth;
    if (SourceWidth = 1) or (TargetWidth = 1) then
      ScaleX :=  TargetWidth / SourceWidth
    else
      ScaleX :=  (TargetWidth - 1) / (SourceWidth - 1);
    if (SourceHeight = 1) or (TargetHeight = 1) then
      ScaleY :=  TargetHeight / SourceHeight
    else
      ScaleY :=  (TargetHeight - 1) / (SourceHeight - 1);

    // pre-calculate filter contributions for a row
    SetLength(ContributorList, TargetWidth);
    // horizontal sub-sampling
    if ScaleX < 1 then
    begin
      // scales from bigger to smaller Width
      Width := Radius / ScaleX;
      for I := 0 to TargetWidth - 1 do
      begin
        ContributorList[I].N := 0;
        SetLength(ContributorList[I].Contributors, Ceil(2 * (Width + 1)));
        Center := I / ScaleX;
        Left := Floor(Center - Width);
        Right := Ceil(Center + Width);
        for J := Left to Right do
        begin
          Weight := Round(Filter((Center - J) * ScaleX) * ScaleX * 256);
          if Weight <> 0 then
          begin
            if J < 0 then
              N := -J
            else
              if J >= SourceWidth then
                N := SourceWidth - J + SourceWidth - 1
              else
                N := J;
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end;
    end
    else
    begin
      // horizontal super-sampling
      // scales from smaller to bigger Width
      for I := 0 to TargetWidth - 1 do
      begin
        ContributorList[I].N := 0;
        SetLength(ContributorList[I].Contributors, Ceil(2 * (Radius + 1)));
        Center := I / ScaleX;
        Left := Floor(Center - Radius);
        Right := Ceil(Center + Radius);
        for J := Left to Right do
        begin
          Weight := Round(Filter(Center - J) * 256);
          if Weight <> 0 then
          begin
            if J < 0 then
              N := -J
            else
              if J >= SourceWidth then
                N := SourceWidth - J + SourceWidth - 1
              else
                N := J;
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end;
    end;

    // now apply filter to sample horizontally from Src to Work
    SetLength(CurrentLineR, SourceWidth);
    SetLength(CurrentLineG, SourceWidth);
    SetLength(CurrentLineB, SourceWidth);
    if UseAlphaChannel then
    begin
      SetLength(CurrentLineA, SourceWidth);
      PixelSize := 4;
    end
    else
      PixelSize := 3;
    for K := 0 to SourceHeight - 1 do
    begin
      SourceLine := Source.ScanLine[K];
      FillLineCache(SourceWidth, PixelSize, SourceLine, UseAlphaChannel);
      DestPixel := Work.ScanLine[K];
      for I := 0 to TargetWidth - 1 do
        with ContributorList[I] do
        begin
          ContributorResult := ApplyContributors(N, ContributorList[I].Contributors, UseAlphaChannel);
          Move(ContributorResult, DestPixel^, PixelSize);
          // move on to next column
          Inc(PByte(DestPixel), PixelSize);
        end;
    end;

    // free the memory allocated for horizontal filter weights, since we need the stucture again
    for I := 0 to TargetWidth - 1 do
      ContributorList[I].Contributors := nil;
    ContributorList := nil;

    // pre-calculate filter contributions for a column
    SetLength(ContributorList, TargetHeight);
    // vertical sub-sampling
    if ScaleY < 1 then
    begin
      // scales from bigger to smaller height
      Width := Radius / ScaleY;
      for I := 0 to TargetHeight - 1 do
      begin
        ContributorList[I].N := 0;
        SetLength(ContributorList[I].Contributors, Ceil(2 * (Width + 1)));
        Center := I / ScaleY;
        Left := Floor(Center - Width);
        Right := Ceil(Center + Width);
        for J := Left to Right do
        begin
          Weight := Round(Filter((Center - J) * ScaleY) * ScaleY * 256);
          if Weight <> 0 then
          begin
            if J < 0 then
              N := -J
            else
              if J >= SourceHeight then
                N := SourceHeight - J + SourceHeight - 1
              else
                N := J;
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end
    end
    else
    begin
      // vertical super-sampling
      // scales from smaller to bigger height
      for I := 0 to TargetHeight - 1 do
      begin
        ContributorList[I].N := 0;
        SetLength(ContributorList[I].Contributors, Ceil(2 * (Radius + 1)));
        Center := I / ScaleY;
        Left := Floor(Center - Radius);
        Right := Ceil(Center + Radius);
        for J := Left to Right do
        begin
          Weight := Round(Filter(Center - J) * 256);
          if Weight <> 0 then
          begin
            if J < 0 then
              N := -J
            else
              if J >= SourceHeight then
                N := SourceHeight - J + SourceHeight - 1
              else
                N := J;                       
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end;
    end;

    // apply filter to sample vertically from Work to Target
    SetLength(CurrentLineR, SourceHeight);
    SetLength(CurrentLineG, SourceHeight);
    SetLength(CurrentLineB, SourceHeight);
    if UseAlphaChannel then
    begin
      SetLength(CurrentLineA, SourceHeight);
      PixelSize := 4;
    end
    else
      PixelSize := 3;

    SourceLine := Work.ScanLine[0];
    Delta := Integer(Work.ScanLine[1]) - Integer(SourceLine);
    DestLine := Target.ScanLine[0];
    DestDelta := Integer(Target.ScanLine[1]) - Integer(DestLine);
    for K := 0 to TargetWidth - 1 do
    begin
      DestPixel := Pointer(DestLine);
      FillLineCache(SourceHeight, Delta, SourceLine, UseAlphaChannel);
      for I := 0 to TargetHeight - 1 do
        with ContributorList[I] do
        begin
          ContributorResult := ApplyContributors(N, ContributorList[I].Contributors, UseAlphaChannel);
          Move(ContributorResult, DestPixel^, PixelSize);
          // move on to next column
          Inc(PByte(DestPixel), DestDelta);
        end;
      Inc(PByte(SourceLine), PixelSize);
      Inc(PByte(DestLine), PixelSize);
    end;

    // free the memory allocated for vertical filter weights
    for I := 0 to TargetHeight - 1 do
      ContributorList[I].Contributors := nil;
    // this one is done automatically on exit, but is here for completeness
    ContributorList := nil;

  finally
    Work.Canvas.Unlock;
    Work.Free;
    CurrentLineR := nil;
    CurrentLineG := nil;
    CurrentLineB := nil;
    if UseAlphaChannel then
      CurrentLineA := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter; Radius: Single; Source, Target: TBitmap);

// Scales the source bitmap to the given size (NewWidth, NewHeight) and stores the Result in Target.
// Filter describes the filter function to be applied and Radius the size of the filter area.
// Is Radius = 0 then the recommended filter area will be used (see DefaultFilterRadius).

begin
  if Source.PixelFormat in [pfDevice, pf8Bit, pf16Bit] then
    raise Exception.Create('Bitmap must have 24 or 32 bpp!');

  if Radius = 0 then
    Radius := DefaultFilterRadius[Filter];
  Target.Height := 0; // Avoid unnecessary image data copies.
  Target.PixelFormat := Source.PixelFormat;
  Target.Width := NewWidth;
  Target.Height := NewHeight;
  DoStretch(FilterList[Filter], Radius, Source, Target);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter; Radius: Single; Source: TBitmap);

var
  Target: TBitmap;

begin
  if Source.PixelFormat in [pfDevice, pf8Bit, pf16Bit] then
    raise Exception.Create('Bitmap must have 24 or 32 bpp!');

  if Radius = 0 then
    Radius := DefaultFilterRadius[Filter];
  Target := TBitmap.Create;
  try
    Target.Width := NewWidth;
    Target.Height := NewHeight;
    Target.PixelFormat := Source.PixelFormat;
    DoStretch(FilterList[Filter], Radius, Source, Target);
    Source.Assign(Target);
  finally
    Target.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function ReadImageProperties(const FileName: string; var Properties: TImageProperties): Boolean;

// Reads the properties of an image given by FileName. It just simplifies to find a proper loader class.
// Reading image properties is a light weight task. Only a small part of the image must be accessed.
// It is not loaded into memory.
// True is returned if the properties could be read. False appears in case on a problem (e.g. read error). 

var
  Extension: string;
  GraphicClass: TGraphicClass;
  NewGraphic: TGraphic;

begin
  Result := False;

  try
    Extension := ExtractFileExt(FileName);
    GraphicClass := FileFormatList.GraphicFromExtension(Extension);

    if (GraphicClass <> nil) and (GraphicClass.ClassParent = TGraphicExGraphic) then
    begin
      NewGraphic := GraphicClass.Create;
      try
        with TGraphicExGraphic(NewGraphic) do
        begin
          ReadImageProperties(FileName, 0);
          Properties := ImageProperties;
          Result := True;
        end;
      finally
        NewGraphic.Free;
      end;
    end;
  except
    // Silent exception, we return False for any error.
  end;
end;

//----------------- support functions for image loading ----------------------------------------------------------------

procedure SwapShort(P: PWord; Count: Cardinal); 

// swaps high and low byte of 16 bit values
// EAX contains P, EDX contains Count

asm
        TEST    EDX, EDX
        JZ      @@Finish
@@Loop:
        MOV     CX, [EAX]
        XCHG    CH, CL
        MOV     [EAX], CX
        ADD     EAX, 2
        DEC     EDX
        JNZ     @@Loop
@@Finish:
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SwapLong(P: PInteger; Count: Cardinal); overload;

// swaps high and low bytes of 32 bit values
// EAX contains P, EDX contains Count

asm
        TEST    EDX, EDX
        JZ      @@Finish
@@Loop:
        MOV     ECX, [EAX]
        BSWAP   ECX
        MOV     [EAX], ECX
        ADD     EAX, 4
        DEC     EDX
        JNZ     @@Loop
@@Finish:
end;

//----------------------------------------------------------------------------------------------------------------------

function SwapLong(Value: Cardinal): Cardinal; overload;

// Swaps high and low bytes of the given 32 bit value.

asm
        BSWAP   EAX
end;

//----------------------------------------------------------------------------------------------------------------------

function SwapLong(Value: Integer): Integer; overload;

// Swaps high and low bytes of the given 32 bit value.

asm
        BSWAP   EAX
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SwapDouble(const Source; var Target);

// Reverses the byte order in Source which must be 8 bytes in size (as well as the target). 

var
  I: Int64;

begin
  I := Int64(Source);
  Int64(Target) := SwapLong(Cardinal(I shr 32)) + Int64(SwapLong(Cardinal(I))) shl 32;
end;

//----------------------------------------------------------------------------------------------------------------------

function ReadBigEndianCardinal(var Run: PChar): Cardinal;

// Reads the next four bytes from the memory pointed to by Run, converts this into a cardinal number (inclusive byte
// order swapping) and advances Run.

begin
  Result := SwapLong(PCardinal(Run)^);
  Inc(PCardinal(Run));
end;

//----------------------------------------------------------------------------------------------------------------------

function ReadBigEndianDouble(var Run: PChar): Double;

// Reads the next two bytes from the memory pointed to by Run, converts this into a word number (inclusive byte
// order swapping) and advances Run.

begin
  SwapDouble(Run^, Result);
  Inc(Run, SizeOf(Double));
end;

//----------------------------------------------------------------------------------------------------------------------

function ReadBigEndianInteger(var Run: PChar): Integer;

// Reads the next four bytes from the memory pointed to by Run, converts this into a cardinal number (inclusive byte
// order swapping) and advances Run.

begin
  Result := SwapLong(PInteger(Run)^);
  Inc(PInteger(Run));
end;

//----------------------------------------------------------------------------------------------------------------------

function ReadBigEndianString(var Run: PChar; Len: Cardinal): WideString; overload;

// Reads the next Len bytes from the memory pointed to by Run, converts this into a Unicode string (inclusive byte
// order swapping) and advances Run.
// Run is not really a PChar type, but an untyped pointer using PChar for easier pointer maths.

begin
  SetString(Result, PWideChar(Run), Len);
  Inc(PWideChar(Run), Len);
  SwapShort(Pointer(Result), Len);
end;

//----------------------------------------------------------------------------------------------------------------------

function ReadBigEndianString(var Run: PChar): WideString; overload;

// Same as ReadBigEndianString with length parameter. However the length must first be retrieved.

var
  Len: Cardinal;

begin
  Len := ReadBigEndianCardinal(Run);
  Result := ReadBigEndianString(Run, Len);
end;

//----------------------------------------------------------------------------------------------------------------------

function ReadBigEndianWord(var Run: PChar): Word;

// Reads the next two bytes from the memory pointed to by Run, converts this into a word number (inclusive byte
// order swapping) and advances Run.

begin
  Result := Swap(PWord(Run)^);
  Inc(Run, SizeOf(Word));
end;

//----------------- various conversion routines ------------------------------------------------------------------------

procedure Depredict1(P: Pointer; Count: Cardinal);

// EAX contains P and EDX Count

asm
@@1:
        MOV     CL, [EAX]
        ADD     [EAX + 1], CL
        INC     EAX
        DEC     EDX
        JNZ     @@1
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Depredict3(P: Pointer; Count: Cardinal); 

// EAX contains P and EDX Count

asm
        MOV     ECX, EDX
        SHL     ECX, 1
        ADD     ECX, EDX  // 3 * Count
@@1:
        MOV     DL, [EAX]
        ADD     [EAX + 3], DL
        INC     EAX
        DEC     ECX
        JNZ     @@1
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Depredict4(P: Pointer; Count: Cardinal);

// EAX contains P and EDX Count

asm
        SHL     EDX, 2 // 4 * Count
@@1:
        MOV     CL, [EAX]
        ADD     [EAX + 4], CL
        INC     EAX
        DEC     EDX
        JNZ     @@1
end;

//----------------- TFileMapping ---------------------------------------------------------------------------------------

constructor TFileMapping.Create(const FileName: string; Mode: TFileMappingMode);

var
  AccessFlags,
  CreationFlag: Cardinal;
  SizeLow,
  SizeHigh: Cardinal;
  
begin
  FFileName := FileName;

  AccessFlags := GENERIC_READ;
  if Mode <> fmmReadOnly then
    AccessFlags := AccessFlags or GENERIC_WRITE;
  case Mode of
    fmmCreateNew:
      CreationFlag := CREATE_ALWAYS;
    fmmOpenOrCreate:
      CreationFlag := OPEN_ALWAYS;
  else
    // fmmReadOnly, fmmReadWrite
    CreationFlag := OPEN_EXISTING;
  end;
  FFileHandle := CreateFile(PChar(FileName), AccessFLags, FILE_SHARE_READ, nil, CreationFlag, FILE_ATTRIBUTE_NORMAL
    or FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if FFileHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError
  else
  begin
    SizeLow := GetFileSize(FFileHandle, @SizeHigh);
    FFileSize := Int64(SizeHigh) shl 32 + SizeLow;
    FFileMapping := CreateFileMapping(FFileHandle, nil, PAGE_READONLY	, 0, 0, nil);
    if FFileMapping = 0 then
      RaiseLastOSError;
    FMemory := MapViewOfFile(FFileMapping, FILE_MAP_READ, 0, 0, 0);
    if FMemory = nil then
      RaiseLastOSError;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TFileMapping.Create(Stream: THandleStream);

// Alternative constructor to create the mapping from a handle stream which is usually a wrapper for a normal file.
// NOTE: you must not change the file content using the stream as long as the mapping exists otherwise inconsitencies
//       will appear! However you can write into the stream using the memory pointer from the mapping.

var
  SizeLow,
  SizeHigh: Cardinal;

begin
  // Set the file handle to invalid so it does not get freed in the destructor.
  FFileHandle := INVALID_HANDLE_VALUE;

  SizeLow := GetFileSize(Stream.Handle, @SizeHigh);
  FFileSize := Int64(SizeHigh) shl 32 + SizeLow;
  FFileMapping := CreateFileMapping(Stream.Handle, nil, PAGE_READONLY, 0, 0, nil);
  if FFileMapping = 0 then
    RaiseLastOSError;
  FMemory := MapViewOfFile(FFileMapping, FILE_MAP_READ, 0, 0, 0);
  if FMemory = nil then
    RaiseLastOSError;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TFileMapping.Destroy;

begin
  if Assigned(FMemory) then
    UnmapViewOfFile(Memory);
  if FFileMapping <> 0 then
    CloseHandle(FFileMapping);
  if FFileHandle <> INVALID_HANDLE_VALUE then
    FileClose(FFileHandle);

  inherited;
end;

//----------------- TGraphicExGraphic ----------------------------------------------------------------------------------

constructor TGraphicExGraphic.Create;

begin
  inherited;
  FColorManager := TColorManager.Create;
  Decoder := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TGraphicExGraphic.Destroy;

begin
  ClearProgressStack;
  FColorManager.Free;
  Decoder.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

// Since loading an image involves often a lot of processing it is diffcult to provide the user with
// usefull progress information. This is mainly due to the impossibility to tell in advance how
// much overall percent a particular part needs and/or has finished.
// TGraphicExGraphic implements an advanced management which takes socalled sections as base interval.
// A section is the amount of percents a process will take up in the whole range of 0..100% relative to its
// "parent section".
// Stepping up the progress always means here to count "locally" (i.e. in the current section).
// This way a particular process can always step from 0 to 100% and the steps are automatically transformed to an
// overall value depending on the section sizes.
//

procedure TGraphicExGraphic.AdvanceProgress(Amount: Single; OffsetX, OffsetY: Integer; DoRedraw: Boolean);

// Steps the current progress section up by Amount percent (0..100%).
// The meaning of the parameters in the method is:
//   Amount   - Value which is used to increase the section's current progress position (0..100%)
//   OffsetX,
//   OffsetY  - Values to offset the progress rectangle with
//   DoRedraw - Tells the application to update its display.

var
  CurrentSection: PProgressSection;

begin
  Assert(Assigned(FProgressStack), 'Start progress display first using InitProgress.');
  Assert(FProgressStack.Count > 0, 'Initialize a progress section first using StartProgressSection.');

  // Advance the top section.
  CurrentSection := FProgressStack.Peek;
  Amount := Amount / 100;
  // Ensure that we never exceed the 100% limit.
  if CurrentSection.Position + Amount > 1 then
  begin
    Amount := 1 - CurrentSection.Position;
    CurrentSection.Position := 1;
  end
  else
    CurrentSection.Position := CurrentSection.Position + Amount;

  // Sum up the section's percents under consideration of the section size.
  FPercentDone := FPercentDone + CurrentSection.TransformFactor * Amount;
  OffsetRect(FProgressRect, OffsetX, OffsetY);
  Progress(Self, psRunning, Round(100 * FPercentDone), DoRedraw, FProgressRect, CurrentSection.Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.ClearProgressStack;

// Empties the current progress stack and frees it afterwards.

var
  CurrentSection: PProgressSection;

begin
  if Assigned(FProgressStack) then
  begin
    while FProgressStack.Count > 0 do
    begin
      CurrentSection := FProgressStack.Pop;
      Dispose(CurrentSection);
    end;
    FreeAndNil(FProgressStack);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.FinishProgressSection(DoRedraw: Boolean);

// Finishes the current section and removes it from the progress stack.
// The parent section is updated assuming this section has exactly used 100% (regardless of the actual amount).

var
  Percent: Single;
  CurrentSection,
  ParentSection: PProgressSection;

begin
  Assert(Assigned(FProgressStack), 'Start progress display first using InitProgress.');
  Assert(FProgressStack.Count > 0, 'Initialize a progress section first using StartProgressSection.');

  CurrentSection := FProgressStack.Pop;
  if FProgressStack.Count = 0 then
    FreeAndNil(FProgressStack)
  else
  begin
    // Update position of the parent section.
    ParentSection := FProgressStack.Peek;
    if ParentSection.Position + CurrentSection.ParentSize > 1 then
      ParentSection.Position := 1
    else
      ParentSection.Position :=  ParentSection.Position + CurrentSection.ParentSize;
  end;

  // Update the overall percent value.
  Percent := 1 - CurrentSection.Position;
  if Percent > 0 then
    FPercentDone := FPercentDone + CurrentSection.TransformFactor * Percent;
  Dispose(CurrentSection);

  if FProgressStack = nil then
    Progress(Self, psEnding, Round(100 * FPercentDone), DoRedraw, FProgressRect, '')
  else
    Progress(Self, psRunning, Round(100 * FPercentDone), DoRedraw, FProgressRect, '');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.InitProgress(Width, Height: Integer);

// Initializes all progress related variables.

begin
  ClearProgressStack;
  FProgressStack := TStack.Create;

  FProgressRect := Rect(0, 0, Width, Height);
  FPercentDone := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.StartProgressSection(Size: Single; const S: string);

// Starts a new progress section within the current section.
// Size determines the amount the new section will take up in the current section and must be given in
// percent (0..100%). If Size is 0 then the full rest of the current section is taken.
// S is the message string to use for the progress event.

var
  CurrentSection,
  NewSection: PProgressSection;

begin
  Assert(Assigned(FProgressStack), 'Start progress display first using InitProgress.');

  New(NewSection);
  if FProgressStack.Count = 0 then
  begin
    // This is the first (root) section.
    NewSection.ParentSize := 1;
    NewSection.TransformFactor := 1;
  end
  else
  begin
    CurrentSection := FProgressStack.Peek;
    if Size = 0 then
      NewSection.ParentSize := 1 - CurrentSection.Position
    else
      NewSection.ParentSize := Size / 100;
    NewSection.TransformFactor := CurrentSection.TransformFactor * NewSection.ParentSize;
  end;

  NewSection.Position := 0;
  NewSection.Message := S;

  FProgressStack.Push(NewSection);
  if FProgressStack.Count = 1 then
    Progress(Self, psStarting, Round(100 * FPercentDone), False, FProgressRect, S)
  else
    Progress(Self, psRunning, Round(100 * FPercentDone), False, FProgressRect, S);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.Assign(Source: TPersistent);

begin
  if Source is TGraphicExGraphic then
    FImageProperties := TGraphicExGraphic(Source).FImageProperties;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TGraphicExGraphic.CanLoad(const FileName: string): Boolean;

var
  Stream: TFileStream;

begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CanLoad(Stream);
  finally
    Stream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TGraphicExGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TGraphicExGraphic.CanLoad(Stream: TStream): Boolean;

begin
  // We can optimize load operations by using direct memory access if possible.
  // For file streams a file mapping is created, memory streams can directly be accessed and
  // other streams (e.g. blob streams) are converted into a memory stream first.
  if Stream is TCustomMemoryStream then
  begin
    // Simple case: memory streams already are in memory.
    with Stream as TCustomMemoryStream do
      Result := CanLoad(Memory, Size);
  end
  else
    if (Stream is THandleStream) and (GetFileType(THandleStream(Stream).Handle) = FILE_TYPE_DISK) then
    begin
      // File streams can be mapped to access their content directly.
      with TFileMapping.Create(Stream as THandleStream) do
      try
        Result := CanLoad(Memory, Size);
      finally
        Free;
      end;
    end
    else
    begin
      // Any other stream is converted into a memory stream first.
      with TMemoryStream.Create do
      try
        CopyFrom(Stream, 0);
        Position := 0;
        Result := CanLoad(Memory, Size);
      finally
        Free;
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.LoadFromFile(const FileName: string);

begin
  LoadFromFileByIndex(FileName, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.LoadFromFileByIndex(const FileName: string; ImageIndex: Cardinal = 0);

begin
  // Create a file mapping for the file to access the data without intermediate buffering.
  with TFileMapping.Create(FileName, fmmReadOnly) do
  try
    LoadFromMemory(Memory, Size, ImageIndex);
  finally
    Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

begin
  FreeAndNil(Decoder);
  Handle := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.LoadFromResourceID(Instance: THandle; ResID: Integer; ImageIndex: Cardinal = 0);

var
  Stream: TResourceStream;

begin
  Stream := TResourceStream.CreateFromID(Instance, ResID, RT_RCDATA);
  try
    // Resource streams are memory streams, so we can directly access their data.
    with Stream do
      LoadFromMemory(Memory, Size, ImageIndex);
  finally
    Stream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.LoadFromResourceName(Instance: THandle; const ResName: string; ImageIndex: Cardinal = 0);

var
  Stream: TResourceStream;

begin
  Stream := TResourceStream.Create(Instance, ResName, RT_RCDATA);
  try
    // Resource streams are memory streams, so we can directly access their data.
    with Stream do
      LoadFromMemory(Memory, Size, ImageIndex);
  finally
    Stream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.LoadFromStream(Stream: TStream);

begin
  LoadFromStreamByIndex(Stream, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.LoadFromStreamByIndex(Stream: TStream; ImageIndex: Cardinal = 0);

begin
  // We can optimize load operations by using direct memory access if possible.
  // For file streams a file mapping is created, memory streams can directly be accessed and
  // other streams (e.g. blob streams) are converted into a memory stream first.
  if Stream is TCustomMemoryStream then
  begin
    // Simple case: memory streams already are in memory.
    with Stream as TCustomMemoryStream do
      LoadFromMemory(Memory, Size, ImageIndex);
  end
  else
    if (Stream is THandleStream) and (GetFileType(THandleStream(Stream).Handle) = FILE_TYPE_DISK) then
    begin
      // File streams can be mapped to access their content directly.
      with TFileMapping.Create(Stream as THandleStream) do
      try
        LoadFromMemory(Memory, Size, ImageIndex);
      finally
        Free;
      end;
    end
    else
    begin
      // Any other stream is converted into a memory stream first.
      with TMemoryStream.Create do
      try
        CopyFrom(Stream, 0);
        Position := 0;
        LoadFromMemory(Memory, Size, ImageIndex);
      finally
        Free;
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGraphicExGraphic.ReadImageProperties(const Name: string; ImageIndex: Cardinal): Boolean;

begin
  // Create a file mapping for the file to access the data without intermediate buffering.
  with TFileMapping.Create(Name, fmmReadOnly) do
  try
    Result := ReadImageProperties(Memory, Size, ImageIndex);
  finally
    Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGraphicExGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

// Initializes the internal image properties structure.
// This is the overloaded variant for streams.

var
  LastPos: Int64;

begin
  LastPos := Stream.Position;
  if Stream is TCustomMemoryStream then
  begin
    // Simple case: memory streams already are in memory.
    with Stream as TCustomMemoryStream do
      Result := ReadImageProperties(Memory, Size, ImageIndex);
  end
  else
    if (Stream is THandleStream) and (GetFileType(THandleStream(Stream).Handle) = FILE_TYPE_DISK) then
    begin
      // File streams can be mapped to access their content directly.
      with TFileMapping.Create(Stream as THandleStream) do
      try
        Result := ReadImageProperties(Memory, Size, ImageIndex);
      finally
        Free;
      end;
    end
    else
    begin
      // Any other stream is converted into a memory stream first.
      with TMemoryStream.Create do
      try
        CopyFrom(Stream, 0);
        Position := 0;
        Result := ReadImageProperties(Memory, Size, ImageIndex);
      finally
        Free;
      end;
    end;

  Stream.Position := LastPos;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGraphicExGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

// Initializes the internal image properties structure.
// Descentants must override this method to fill in the actual values.

begin
  ZeroMemory(@FImageProperties, SizeOf(FImageProperties));
  FImageProperties.FileGamma := 1;
  Result := True;
end;

//----------------- TAutodeskGraphic -----------------------------------------------------------------------------------

{$ifdef AutodeskGraphic}

type
  PAutodeskHeader = ^TAutodeskHeader;
  TAutodeskHeader = packed record
    Width,
    Height,
    XCoord,
    YCoord: Word;
    Depth,
    Compression: Byte;
    DataSize: Cardinal;
    Reserved: array[0..15] of Byte;
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TAutodeskGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

var
  Run: PByte;

begin
  Run := Memory;
  Result := Size > SizeOf(Word) + SizeOf(TAutodeskHeader);
  if Result then
  begin
    // Check file ID.
    Result := PWord(Run)^ = $9119;
    if Result then
    begin
      // Read image dimensions.
      Inc(Run, SizeOf(Word));
      with PAutodeskHeader(Run)^ do
        Result := (Depth = 8) and (Compression = 0);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAutodeskGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  Run: PChar;
  LogPalette: TMaxLogPalette;
  I: Integer;

begin
  inherited;

  Run := Memory;
  
  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    FProgressRect := Rect(0, 0, Width, 1);
    Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);

    // Skip file ID and header.
    Inc(Run, 2 + SizeOf(TAutodeskHeader));

    // Read palette entries and create a palette.
    ZeroMemory(@LogPalette, SizeOf(LogPalette));
    LogPalette.palVersion := $300;
    LogPalette.palNumEntries := 256;
    for I := 0 to 255 do
    begin
      with PPaletteEntry(Run)^ do
      begin
        LogPalette.palPalEntry[I].peBlue := Byte(peBlue shl 2);
        LogPalette.palPalEntry[I].peGreen := Byte(peGreen shl 2);
        LogPalette.palPalEntry[I].peRed := Byte(peRed shl 2);
      end;
      Inc(Run, 3);
    end;

    // Setup bitmap properties.
    PixelFormat := pf8Bit;
    Palette := CreatePalette(PLogPalette(@LogPalette)^);
    Width := FImageProperties.Width;
    Height := FImageProperties.Height;
    // Finally read image data.
    for I := 0 to Height - 1 do
    begin
      Move(Run^, Scanline[I]^, Width);
      Inc(Run, Width);
                                                        
      Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
      OffsetRect(FProgressRect, 0, 1);
    end;

    Progress(Self, psEnding, 0, False, FProgressRect, '');
  end
  else
    GraphicExError(gesInvalidImage, ['Autodesk']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TAutodeskGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Run: PChar;
  Header: PAutodeskHeader;

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);
  if Result then
    with FImageProperties do
    begin
      Run := Memory;
      // Skip file ID. This has been check in the inherited call.
      Header := Pointer(Run + 2);
      ColorScheme := csIndexed;
      Width := Header.Width;
      Height := Header.Height;
      BitsPerSample := 8;
      SamplesPerPixel := 1;
      BitsPerPixel := 8;
      Compression := ctNone;
    end;
end;

{$endif AutodeskGraphic}

//----------------- TSGIGraphic ----------------------------------------------------------------------------------------

{$ifdef SGIGraphic}

const
  SGIMagic = 474;

  SGI_COMPRESSION_VERBATIM = 0;
  SGI_COMPRESSION_RLE = 1;

type
  PSGIHeader = ^TSGIHeader;
  TSGIHeader = packed record
    Magic: SmallInt;         // IRIS image file magic number
    Storage,                 // Storage format
    BPC: Byte;               // Number of bytes per pixel channel (1 or 2)
    Dimension: Word;         // Number of dimensions
                             //   1 - one single scanline (and one channel) of length XSize
                             //   2 - two dimensional (one channel) of size XSize x YSize
                             //   3 - three dimensional (ZSize channels) of size XSize x YSize
    XSize,                   // width of image
    YSize,                   // height of image
    ZSize: Word;             // number of channels/planes in image (3 for RGB, 4 for RGBA etc.)
    PixMin,                  // Minimum pixel value
    PixMax: Cardinal;        // Maximum pixel value
    Dummy: Cardinal;         // ignored
    ImageName: array[0..79] of Char;
    ColorMap: Integer;       // Colormap ID
                             //  0 - default, almost all images are stored with this flag
                             //  1 - dithered, only one channel of data (pixels are packed), obsolete
                             //  2 - screen (palette) image, obsolete
                             //  3 - no image data, palette only, not displayable
    Dummy2: array[0..403] of Byte; // ignored
  end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSGIGraphic.GetComponents(const Memory: Pointer; var Red, Green, Blue, Alpha: Pointer; Row: Integer);

var
  RowWidth: Integer;
  PlaneSize: Integer;

begin
  RowWidth := Row * Width;
  PlaneSize := Width * Height;

  Red := PChar(Memory) + 512 + RowWidth;
  Green := PChar(Memory) + 512 + RowWidth + PlaneSize;
  Blue := PChar(Memory) + 512 + RowWidth + 2 * PlaneSize;
  Alpha := PChar(Memory) + 512 + RowWidth + 3 * PlaneSize;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSGIGraphic.ReadAndDecode(const Memory: Pointer; Red, Green, Blue, Alpha: Pointer; Row: Integer; BPC: Cardinal);

var
  Count: Cardinal;
  Run: PChar;

begin
  if Assigned(Red) then
  begin
    Run := PChar(Memory) + FRowStart[Row + 0 * Height];
    Count := BPC * FRowSize[Row + 0 * Height];
    Decoder.Decode(Pointer(Run), Red, Count, Width);
  end;

  if Assigned(Green) then
  begin
    Run := PChar(Memory) + FRowStart[Row + 1 * Height];
    Count := BPC * FRowSize[Row + 1 * Height];
    Decoder.Decode(Pointer(Run), Green, Count, Width);
  end;

  if Assigned(Blue) then
  begin
    Run := PChar(Memory) + FRowStart[Row + 2 * Height];
    Count := BPC * FRowSize[Row + 2 * Height];
    Decoder.Decode(Pointer(Run), Blue, Count, Width);
  end;

  if Assigned(Alpha) then
  begin
    Run := PChar(Memory) + FRowStart[Row + 3 * Height];
    Count := BPC * FRowSize[Row + 3 * Height];
    Decoder.Decode(Pointer(Run), Alpha, Count, Width);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TSGIGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := Size > SizeOf(TSGIHeader);
  if Result then
    with PSGIHeader(Memory)^ do
    begin
      // There are not many unique fields which can be used for identification, so
      // we do some simple plausibility checks too.
      Result := (Swap(Magic) = SGIMagic) and (BPC in [1, 2]) and (Swap(Dimension) in [1..3]);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSGIGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  Run: PChar;
  Y: Integer;
  RedBuffer,
  GreenBuffer,
  BlueBuffer,
  AlphaBuffer: Pointer;
  Header: TSGIHeader;
  Count: Cardinal;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    Run := Memory;
    with FImageProperties do
    begin
      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      // Read header again. We need some additional information.
      Move(Run^, Header, SizeOf(TSGIHeader));
      Inc(Run, SizeOf(TSGIHeader));

      // SGI images are always stored in big endian style
      ColorManager.SourceOptions := [coNeedByteSwap];
      with Header do
        ColorMap := SwapLong(ColorMap);

      if Compression = ctRLE then
      begin
        Count := Height * SamplesPerPixel;
        SetLength(FRowStart, Count);
        SetLength(FRowSize, Count);
        // Convert line starts and sizes.
        Move(Run^, Pointer(FRowStart)^, Count * SizeOf(Cardinal));
        SwapLong(Pointer(FRowStart), Count);
        Move(Run^, Pointer(FRowSize)^, Count * SizeOf(Cardinal));
        SwapLong(Pointer(FRowSize), Count);
        Decoder := TSGIRLEDecoder.Create(BitsPerSample);
      end
      else
        Decoder := nil;

      // Set pixel format before size to avoid possibly large conversion operation.
      with ColorManager do
      begin
        SourceBitsPerSample := BitsPerSample;
        TargetBitsPerSample := 8;
        SourceSamplesPerPixel := SamplesPerPixel;
        TargetSamplesPerPixel := SamplesPerPixel;
        SourceColorScheme := ColorScheme;
        case ColorScheme of
          csRGBA:
            TargetColorScheme := csBGRA;
          csRGB:
            TargetColorScheme := csBGR;
        else
          TargetColorScheme := csIndexed;
        end;
        PixelFormat := TargetPixelFormat;
      end;
      Self.Width := Width;
      Self.Height := Height;

      Progress(Self, psEnding, 100, True, FProgressRect, '');

      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
      try
        Count := (BitsPerPixel div 8) * Width;
        // read lines and put them into the bitmap
        case ColorScheme of
          csRGBA:
            if Decoder = nil then
            begin
              // Uncompressed storage.
              for  Y := 0 to Height - 1 do
              begin
                GetComponents(Memory, RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer, Y);
                ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer], ScanLine[Height - Y - 1],
                  Width, $FF);
                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            end
            else
            begin
              GetMem(RedBuffer, Count);
              GetMem(GreenBuffer, Count);
              GetMem(BlueBuffer, Count);
              GetMem(AlphaBuffer, Count);
              try
                for  Y := 0 to Height - 1 do
                begin
                  ReadAndDecode(Memory, RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer, Y, Header.BPC);
                  ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer], ScanLine[Height - Y - 1],
                    Width, $FF);
                  Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                  OffsetRect(FProgressRect, 0, 1);
                end;
              finally
                FreeMem(RedBuffer);
                FreeMem(GreenBuffer);
                FreeMem(BlueBuffer);
                FreeMem(AlphaBuffer);
              end;
            end;
          csRGB:
            if Decoder = nil then
            begin
              // Uncompressed storage.
              for  Y := 0 to Height - 1 do
              begin
                GetComponents(Memory, RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer, Y);
                ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer], ScanLine[Height - Y - 1], Width, $FF);
                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            end
            else
            begin
              GetMem(RedBuffer, Count);
              GetMem(GreenBuffer, Count);
              GetMem(BlueBuffer, Count);
              try
                for  Y := 0 to Height - 1 do
                begin
                  ReadAndDecode(Memory, RedBuffer, GreenBuffer, BlueBuffer, nil, Y, Header.BPC);
                  ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer], ScanLine[Height - Y - 1], Width, $FF);
                  Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                  OffsetRect(FProgressRect, 0, 1);
                end;
              finally
                FreeMem(RedBuffer);
                FreeMem(GreenBuffer);
                FreeMem(BlueBuffer);
              end;
            end;
        else
          // Any other format is interpreted as being 256 gray scales.
          Palette := ColorManager.CreateGrayscalePalette(False);
          if Decoder = nil then
          begin
            // Uncompressed storage.
            for  Y := 0 to Height - 1 do
            begin
              GetComponents(Memory, RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer, Y);
              Move(RedBuffer^, ScanLine[Height - Y - 1]^, Width);
              Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end;
          end
          else
          begin
            for  Y := 0 to Height - 1 do
            begin
              ReadAndDecode(Memory, ScanLine[Height - Y - 1], nil, nil, nil, Y, Header.BPC);
              Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end;
          end;
        end;
      finally
        Progress(Self, psEnding, 100, True, FProgressRect, '');
        FreeAndNil(Decoder);
      end;
    end;
  end
  else
    GraphicExError(gesInvalidImage, ['sgi, bw or rgb(a)']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSGIGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Header: TSGIHeader;

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);
  if Result then
    with FImageProperties do
    begin
      Move(Memory^, Header, SizeOf(TSGIHeader));
      if Swap(Header.Magic) = SGIMagic then
      begin
        Options := [ioBigEndian];
        BitsPerSample := Header.BPC * 8;
        Width := Swap(Header.XSize);
        Height := Swap(Header.YSize);
        SamplesPerPixel := Swap(Header.ZSize);
        case SamplesPerPixel of
          4:
            ColorScheme := csRGBA;
          3:
            ColorScheme := csRGB;
        else
          // All other is considered as being 8 bit gray scale.
          ColorScheme := csIndexed;
        end;

        BitsPerPixel := BitsPerSample * SamplesPerPixel;
        if Header.Storage = SGI_COMPRESSION_RLE then
          Compression := ctRLE
        else
          Compression := ctNone;
      end
      else
        Result := False;
    end;
end;

{$endif SGIGraphic}

//----------------- TTIFFGraphic ---------------------------------------------------------------------------------------

{$ifdef TIFFGraphic}

type
  PTIFFHeader = ^TTIFFHeader;
  TTIFFHeader = packed record
    ByteOrder: Word;
    Version: Word;
    FirstIFD: Cardinal;
  end;

//----------------------------------------------------------------------------------------------------------------------

// For the libtiff library we need global functions to do the data retrieval. The setup is so that the currently
// loading TIFF instance is given in the fd parameter.

function TIFFReadProc(fd: thandle_t; buf: tdata_t; size: tsize_t): tsize_t; cdecl;

var
  Graphic: TTIFFGraphic;

begin
  Graphic := TTIFFGraphic(fd);
  Move(Graphic.FCurrentPointer^, Pointer(buf)^, size);
  Inc(Graphic.FCurrentPointer, size);
  Result := size;
end;

//----------------------------------------------------------------------------------------------------------------------

function TIFFWriteProc(fd: thandle_t; buf: tdata_t; size: tsize_t): tsize_t; cdecl;

begin
  Result := 0; // Writing is not supported yet.
end;

//----------------------------------------------------------------------------------------------------------------------

function TIFFSeekProc(fd: thandle_t; off: toff_t; whence: Integer): toff_t; cdecl;

const
  SEEK_SET = 0; // seek to an absolute position
  SEEK_CUR = 1; // seek relative to current position 
  SEEK_END = 2; // seek relative to end of file

var
  Graphic: TTIFFGraphic;

begin
  Graphic := TTIFFGraphic(fd);

  case whence of
    SEEK_CUR:
      Inc(Graphic.FCurrentPointer, off);
    SEEK_END:
      Graphic.FCurrentPointer := Pointer(PChar(Graphic.FMemory) + Graphic.FSize - off);
  else
    Graphic.FCurrentPointer := Pointer(PChar(Graphic.FMemory) + off);
  end;
  Result := toff_t(PChar(Graphic.FCurrentPointer) - PChar(Graphic.FMemory));
end;

//----------------------------------------------------------------------------------------------------------------------

function TIFFCloseProc(fd: thandle_t): Integer; cdecl;

var
  Graphic: TTIFFGraphic;

begin
  Graphic := TTIFFGraphic(fd);
  Graphic.FCurrentPointer := nil;
  Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TIFFSizeProc(fd: thandle_t): toff_t; cdecl;

var
  Graphic: TTIFFGraphic;

begin
  Graphic := TTIFFGraphic(fd);
  Result := Graphic.FSize;
end;

//----------------------------------------------------------------------------------------------------------------------

function TIFFMapProc(fd: thandle_t; var pbase: tdata_t; var psize: toff_t): Integer; cdecl;

begin
  Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIFFUnmapProc(fd: thandle_t; base: tdata_t; size: toff_t); cdecl;

begin
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTIFFGraphic.ReadContiguous(tif: PTIFF);

var
  Row, Y,
  RowsToRead: Integer;
  Pos: Integer;
  Buffer: Pointer;
  FromSkew: Integer;
  RowCount,
  LineSize: Integer;
  RowsPerStrip: Integer;
  Source: PChar;
  Line: Pointer;
  RowInc: Integer;
  LineOffset: Integer;

begin
  GetMem(Buffer, TIFFStripSize(tif));
  with FImageProperties do
  try
    Y := SetOrientation(tif, Height);

    TIFFGetFieldDefaulted(tif, TIFFTAG_ROWSPERSTRIP, @RowsPerStrip);
    if RowsPerStrip = -1 then
      RowsPerStrip := Height;

    LineSize := TIFFScanlineSize(tif);
    if (BitsPerPixel = 1) and ((Width mod 8) <> 0) then
      FromSkew := ((Width + 7) and not 7) - Width
    else
      FromSkew := 0;

    if FImageProperties.Orientation = ORIENTATION_TOPLEFT then
      RowInc := 1
    else
      RowInc := -1;
      
    Row := 0;
    while Row < Height do
    begin
      RowsToRead := RowsPerStrip - Row mod RowsPerStrip;
      if Row + RowsToRead > Height then
        RowCount := Height - Row
      else
        RowCount := RowsToRead;
      TIFFReadEncodedStrip(tif, TIFFComputeStrip(tif, Row, 0), Buffer, (Row mod RowsPerStrip + RowCount) * LineSize);
      Pos := (Row mod RowsPerStrip) * LineSize;

      Source := PChar(Buffer) + Pos;
      Inc(Row, RowCount);
      LineOffset := Ceil(BitsPerPixel * (Width + FromSkew) / 8);
      while RowCount > 0 do
      begin
        Line := Scanline[Y];
        ColorManager.ConvertRow([Source], Line, Width, $FF);
        Inc(Source, LineOffset);
        Inc(Y, RowInc);
        Dec(RowCount);
      end;
      AdvanceProgress(100 * RowCount / Height, 0, 1, True);
    end;
  finally
    FreeMem(Buffer);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTIFFGraphic.ReadTiled(tif: PTIFF);

var
  Column, Row, Y,
  RowsToRead: Integer;
  Pos, Counter: Integer;
  TileWidth, TileHeight: Integer;
  Buffer: Pointer;
  FromSkew: Integer;
  RowCount: Integer;
  PixelCount: Integer;
  Source: PChar;
  Line: PChar;
  RowInc: Integer;
  ColumnOffset: Integer;

begin
  GetMem(Buffer, TIFFTileSize(tif));
  with FImageProperties do
  try
    TIFFGetField(tif, TIFFTAG_TILEWIDTH, @TileWidth);
    TIFFGetField(tif, TIFFTAG_TILELENGTH, @TileHeight);
    if Orientation = ORIENTATION_TOPLEFT then
      RowInc := 1
    else
      RowInc := -1;

    Row := 0;
    while Row < Height do
    begin
      RowsToRead := TileHeight - (Row mod TileHeight);
      if Row + RowsToRead > Height then
        RowCount := Height - Row
      else
        RowCount := RowsToRead;

      Column := 0;
      while Column < Width do
      begin
        TIFFReadTile(tif, Buffer, Column, Row, 0, 0);
        Pos := (Row mod TileHeight) * Integer(TIFFTileRowSize(tif));

        Source := PChar(Buffer) + Pos;

        Y := Row;
        if Column + TileWidth > Width then
        begin
          // Tile is clipped horizontally.  Calculate visible portion and skewing factors.
          PixelCount := Width - Column;
          FromSkew := TileWidth - PixelCount;
          Counter := RowCount;
          ColumnOffset := BitsPerPixel * PixelCount div 8;
          while Counter > 0 do
          begin
            Line := Scanline[Y];
            Inc(Line, ColumnOffset);
            ColorManager.ConvertRow([Source], Line, PixelCount, $FF);
            Inc(Source, BitsPerPixel * (PixelCount + FromSkew) div 8);
            Inc(Y, RowInc);
            Dec(Counter);
          end;
        end
        else
        begin
          Counter := RowCount;
          ColumnOffset := BitsPerPixel * Column div 8;
          while Counter > 0 do
          begin
            Line := Scanline[Y];
            Inc(Line, ColumnOffset);
            ColorManager.ConvertRow([Source], Line, TileWidth, $FF);
            Inc(Source, BitsPerPixel * TileWidth div 8);
            Inc(Y, RowInc);
            Dec(Counter);
          end;
        end;
        Inc(Column, TileWidth);
      end;

      Inc(Row, RowCount);
      AdvanceProgress(100 * RowCount / Height, 0, 1, True);
    end;
  finally
    FreeMem(Buffer);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TTIFFGraphic.SetOrientation(tif: PTIFF; H: Cardinal): Cardinal;

begin
  case FImageProperties.Orientation of
    ORIENTATION_BOTRIGHT,
    ORIENTATION_RIGHTBOT,
    ORIENTATION_LEFTBOT,
    ORIENTATION_BOTLEFT:
        Result := H - 1;
  else
    // ORIENTATION_TOPRIGHT
    // ORIENTATION_RIGHTTOP
    // ORIENTATION_LEFTTOP etc.
    FImageProperties.Orientation := ORIENTATION_TOPLEFT;
    Result := 0;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TTIFFGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

var
  Run: PByte;
  Header: TTIFFHeader;

begin
  Run := Memory;
  Result := Size > SizeOf(TTIFFHeader);
  if Result then
  begin
    Move(Run^, Header, SizeOf(Header));
    with Header do
    begin
      Result := (ByteOrder = TIFF_BIGENDIAN) or (ByteOrder = TIFF_LITTLEENDIAN);
      if Result then
      begin
        if ByteOrder = TIFF_BIGENDIAN then
        begin
          Version := Swap(Header.Version);
          FirstIFD := SwapLong(Header.FirstIFD);
        end;

        Result := (Version = TIFF_VERSION) and (Integer(FirstIFD) < Size);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTIFFGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  TIFFImage: PTIFF;
  Run: PChar;
  Count: Cardinal;
  Pixels: Pointer;
  I: Integer;
  Line: Pointer;

  {$ifndef DELPHI_7_UP}
    // Structure used to build a va_list array.
    ExtraInfo: record
      Value1: Pointer;
      Value2: Pointer;
      Value3: Pointer;
    end;
  {$endif DELPHI_7_UP}
  RedMap,
  GreenMap,
  BlueMap: PWord;
  GotPalette: Integer;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    with FImageProperties do
    try
      // Initialize outermost progress display.
      InitProgress(Width, 1);
      StartProgressSection(0, '');

      // Initialize sub section for image preparation. We give it a (guessed) value of 1%.
      StartProgressSection(1, gesPreparing);

      FMemory := Memory;
      FCurrentPointer := Memory;
      FSize := Size;

      TIFFImage := TIFFClientOpen('', 'r', Cardinal(Self), TIFFReadProc, TIFFWriteProc, TIFFSeekProc, TIFFCloseProc,
        TIFFSizeProc, TIFFMapProc, TIFFUnmapProc);
      try
        // The preparation part is finished. Finish also progress section (which will step the main progress).
        FinishProgressSection(False);

        if Assigned(TIFFImage) then
        begin
          TIFFSetDirectory(TIFFImage, ImageIndex);

          ColorManager.SourceColorScheme := ColorScheme;
          // Split loading image data depending on pixel depth.
          if (SamplesPerPixel = 1) and (ColorScheme in [csIndexed, csG, csIndexedA, csGA]) then
          begin
            // Monochrome or palette images with 1, 2, 4, 8 and 16 bits per pixel.
            ColorManager.SourceBitsPerSample := BitsPerSample;
            ColorManager.SourceSamplesPerPixel := SamplesPerPixel;

            if BitsPerSample in [8, 16] then
              ColorManager.TargetBitsPerSample := 8
            else
              ColorManager.TargetBitsPerSample := BitsPerSample;
            ColorManager.TargetSamplesPerPixel := SamplesPerPixel;

            // Monochrome images are handled just like indexed images (a gray scale palette is used).
            ColorManager.TargetColorScheme := csindexed;
            PixelFormat := ColorManager.TargetPixelFormat;
            Self.Width := Width;
            Self.Height := Height;

            if ColorScheme = csIndexed then
            begin
              {$ifndef DELPHI_7_UP}
                ExtraInfo.Value1 := @RedMap;
                ExtraInfo.Value2 := @GreenMap;
                ExtraInfo.Value3 := @BlueMap;
                GotPalette := TIFFVGetField(TIFFImage, TIFFTAG_COLORMAP, @ExtraInfo);
              {$else}
                GotPalette := TIFFGetField(TIFFImage, TIFFTAG_COLORMAP, @RedMap, @GreenMap, @BlueMap);
              {$endif DELPHI_7_UP}

              if GotPalette > 0 then
              begin
                // Create the palette from the three maps.
                Palette := ColorManager.CreateColorPalette([RedMap, GreenMap, Bluemap], pfPlane16Triple, 1 shl BitsPerPixel, True);
              end
              else // If there was no palette then use a grayscale palette.
                Palette := ColorManager.CreateGrayscalePalette(False);
            end
            else
            begin
              // Gray scale image data.
              Palette := ColorManager.CreateGrayscalePalette(ioMinIsWhite in Options);
            end;

            StartProgressSection(0, gesLoadingData);
            if ioTiled in Options then
              ReadTiled(TIFFImage)
            else
              ReadContiguous(TIFFImage);
            FinishProgressSection(False);
          end
          else
            if Height > 0 then
            begin
              // 3 or more samples per pixel are used for RGB(A), CMYK, L*a*b*, YCbCr etc.
              // All of these will be converted to RGBA.
              PixelFormat := pf32Bit;
              Self.Width := Width;
              Self.Height := Height;

              // We can improve speed a bit by directly loading the image data to the scan lines
              // for bottom-up images (which is usually the case).
              if Integer(Scanline[0]) - Integer(Scanline[1]) > 0 then
              begin
                StartProgressSection(0, gesLoadingData);
                TIFFReadRGBAImage(TIFFImage, Width, Height, Scanline[Height - 1], True);
                FinishProgressSection(True);
              end
              else
              begin
                Count := Width * Height;
                GetMem(Pixels, Count * SizeOf(Cardinal));
                try
                  StartProgressSection(70, gesLoadingData);
                  if TIFFReadRGBAImage(TIFFImage, Width, Height, Pixels, True) then
                  begin
                    FinishProgressSection(False);

                    StartProgressSection(30, gesTransfering);
                    Run := Pointer(Pixels);
                    for I := Height - 1 downto 0 do
                    begin
                      Line := Scanline[I];
                      Move(Run^, Line^, Width * 4);
                      Inc(Run, Width * 4);
                      AdvanceProgress(100 / Height, 0, 1, True);
                    end;
                  end;
                  FinishProgressSection(False);
                finally
                  FreeMem(Pixels);
                end;
              end;
            end;
        end;
      finally
        TIFFClose(TIFFImage);
      end;
    finally
      FinishProgressSection(False);

      if Assigned(Decoder) then
        Decoder.DecodeEnd;
      FreeAndNil(Decoder);
    end;
  end
  else
    GraphicExError(gesInvalidImage, ['TIF/TIFF']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TTIFFGraphic.ReadImageProperties(const Memory: Pointer; Size:Int64; ImageIndex: Cardinal): Boolean;

// Reads all relevant TIF properties of the image of index ImageIndex (zero based).

var
  TIFFImage: PTIFF;
  PhotometricInterpretation: Word;
  ExtraSamples: Word;
  SampleInfo: PWord;
  TIFFValue: Word;
  TIFFCompression: Word;
  ResUnit: Word;
  FillOrder: Word;

  {$ifndef DELPHI_7_UP}
    // Structure used to build a va_list array.
    ExtraInfo: record
      Value1: Pointer;
      Value2: Pointer;
    end;
  {$endif DELPHI_7_UP}
  
begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
  begin
    with FImageProperties do
    begin
      FMemory := Memory;
      FCurrentPointer := Memory;
      FSize := Size;

      TIFFImage := TIFFClientOpen('', 'r', Cardinal(Self), TIFFReadProc, TIFFWriteProc, TIFFSeekProc, TIFFCloseProc,
        TIFFSizeProc, TIFFMapProc, TIFFUnmapProc);
      if Assigned(TIFFImage) then
      try
        // This version is actually a magic number, which does never change.
        Version := TIFF_VERSION;
        try
          // Account for invalid files.
          ImageCount := TIFFNumberOfDirectories(TIFFImage);
        except
          ImageCount := 1;
        end;

        TIFFSetDirectory(TIFFImage, ImageIndex);
        TIFFGetField(TIFFImage, TIFFTAG_IMAGEWIDTH, @Width);
        TIFFGetField(TIFFImage, TIFFTAG_IMAGELENGTH, @Height);
        TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_ORIENTATION, @Orientation);

        // Number of color components per pixel (1 for b&w, 16 and 256 colors, 3 for RGB, 4 for CMYK etc.).
        TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_SAMPLESPERPIXEL, @TIFFValue);
        SamplesPerPixel := TIFFValue;

        // Number of bits per color component.
        TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_BITSPERSAMPLE, @TIFFValue);
        BitsPerSample := TIFFValue;

        // Determine whether image is tiled.
        if TIFFIsTiled(TIFFImage) then
          Include(Options, ioTiled);

        // Photometric interpretation determines the color space.
        TIFFGetField(TIFFImage, TIFFTAG_PHOTOMETRIC, @PhotometricInterpretation);
        // Type of extra information for additional samples per pixel.
        {$ifndef DELPHI_7_UP}
          ExtraInfo.Value1 := @ExtraSamples;
          ExtraInfo.Value2 := @SampleInfo;
          TIFFVGetFieldDefaulted(TIFFImage, TIFFTAG_EXTRASAMPLES, @ExtraInfo);
        {$else}
          TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_EXTRASAMPLES, @ExtraSamples, @SampleInfo);
        {$endif DELPHI_7_UP}

        // Determine whether extra samples must be considered.
        HasAlpha := (ExtraSamples = 1) and
          (SampleInfo^ in [EXTRASAMPLE_ASSOCALPHA, EXTRASAMPLE_UNASSALPHA]);
        
        // Currently all bits per sample values are equal.
        BitsPerPixel := BitsPerSample * SamplesPerPixel;

        // Convert compression identifier.
        TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_COMPRESSION, @TIFFCompression);
        case TIFFCompression of
          COMPRESSION_NONE:
            Compression := ctNone;
          COMPRESSION_LZW:
            Compression := ctLZW;
          COMPRESSION_PACKBITS:
            Compression := ctPackedBits;
          COMPRESSION_CCITTRLE:
            Compression := ctFaxRLE;
          COMPRESSION_CCITTRLEW:
            Compression := ctFaxRLEW;
          COMPRESSION_CCITTFAX3:
            begin
              TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_T4OPTIONS, @TIFFValue);
              if (TIFFValue and GROUP3OPT_2DENCODING) <> 0 then
                Compression := ct2DFax3
              else
                Compression := ctFax3;
            end;
          COMPRESSION_OJPEG:
            Compression := ctOJPEG;
          COMPRESSION_JPEG:
            Compression := ctJPEG;
          COMPRESSION_CCITTFAX4:
            Compression := ctFax4;
          COMPRESSION_NEXT:
            Compression := ctNext;
          COMPRESSION_THUNDERSCAN:
            Compression := ctThunderscan;
          COMPRESSION_IT8CTPAD:
            Compression := ctIT8CTPAD;
          COMPRESSION_IT8LW:
            Compression := ctIT8LW;
          COMPRESSION_IT8MP:
            Compression := ctIT8MP;
          COMPRESSION_IT8BL:
            Compression := ctIT8BL;
          COMPRESSION_PIXARFILM:
            Compression := ctPixarFilm;
          COMPRESSION_PIXARLOG: // also a LZ77 clone
            Compression := ctPixarLog;
          COMPRESSION_ADOBE_DEFLATE,
          COMPRESSION_DEFLATE: 
            Compression := ctLZ77;
          COMPRESSION_DCS:
            Compression := ctDCS;
          COMPRESSION_JBIG:
            Compression := ctJBIG;
          COMPRESSION_SGILOG:
            Compression := ctSGILog;
          COMPRESSION_SGILOG24:
            Compression := ctSGILog24;
        else
          Compression := ctUnknown;
        end;

        case PhotometricInterpretation of
          PHOTOMETRIC_MINISWHITE:
            begin
              ColorScheme := csG;
              Include(Options, ioMinIsWhite);
            end;
          PHOTOMETRIC_MINISBLACK:
            ColorScheme := csG;
          PHOTOMETRIC_RGB:
            begin
              if (SamplesPerPixel < 4) then
                ColorScheme := csRGB
              else
                ColorScheme := csRGBA;
            end;
          PHOTOMETRIC_PALETTE:
            ColorScheme := csIndexed;
          PHOTOMETRIC_SEPARATED:
            ColorScheme := csCMYK;
          PHOTOMETRIC_YCBCR:
            ColorScheme := csYCbCr;
          PHOTOMETRIC_CIELAB:
            ColorScheme := csCIELab;
          PHOTOMETRIC_ITULAB:
            ColorScheme := csITULab;
          PHOTOMETRIC_LOGL:
            ColorScheme := csCIELog2L;
          PHOTOMETRIC_LOGLUV:
            ColorScheme := csCIELog2Luv;
        else
          ColorScheme := csUnknown;
        end;

        TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_XRESOLUTION, @XResolution);
        TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_YRESOLUTION, @YResolution);
        TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_RESOLUTIONUNIT, @ResUnit);
        if ResUnit = RESUNIT_CENTIMETER then
        begin
          // Resolution is given in centimeters -> convert to inches.
          XResolution := XResolution * 2.54;
          YResolution := YResolution * 2.54;
        end;

        // Determine fill order in bytes
        TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_FILLORDER, @FillOrder);
        if FillOrder = FILLORDER_LSB2MSB then
          Include(Options, ioReversed);
      finally
        TIFFClose(TIFFImage);
      end
      else
        Result := False;
    end;
  end;
end;

//----------------- TEPSGraphic ----------------------------------------------------------------------------------------

{$ifdef EPSGraphic}

// Note: This EPS implementation does only read embedded pixel graphics in TIF format (preview).
// Credits to:
//   Olaf Stieleke
//   Torsten Pohlmeyer
//   CPS Krohn GmbH
// for providing the base information about how to read the preview image.

type
  PEPSHeader = ^TEPSHeader;
  TEPSHeader = packed record
    Code: Cardinal;   // alway $C6D3D0C5, if not there then this is not an EPS or it is not a binary EPS
    PSStart,          // Offset PostScript-Code
    PSLen,            // length of PostScript-Code
    MetaPos,          // position of a WMF
    MetaLen,          // length of a WMF 
    TiffPos,          // position of TIFF (preview images should be either WMF or TIF but not both)
    TiffLen: Integer; // length of the TIFF
    Checksum: SmallInt;
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TEPSGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := Size > SizeOf(TEPSHeader);
  if Result then
    with PEPSHeader(Memory)^ do
    begin
      Result := (Code = $C6D3D0C5) and (TiffPos >= SizeOf(TEPSHeader)) and (TiffLen > 0);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEPSGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

begin
  with PEPSHeader(Memory)^ do
  begin
    if Code = $C6D3D0C5 then
      inherited LoadFromMemory(PChar(Memory) + TiffPos, TiffLen)
    else
      GraphicExError(gesInvalidImage, ['EPS']);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TEPSGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

begin
  Result := inherited ReadImageProperties(Stream, ImageIndex);
end;

{$endif EPSGraphic}

{$endif TIFFGraphic}

//----------------- TTargaGraphic --------------------------------------------------------------------------------------

{$ifdef TargaGraphic}

//  FILE STRUCTURE FOR THE ORIGINAL TRUEVISION TGA FILE
//    FIELD 1: NUMBER OF CHARACTERS IN ID FIELD (1 BYTES)
//    FIELD 2: COLOR MAP TYPE (1 BYTES)
//    FIELD 3: IMAGE TYPE CODE (1 BYTES)
//      = 0  NO IMAGE DATA INCLUDED
//      = 1  UNCOMPRESSED, COLOR-MAPPED IMAGE
//      = 2  UNCOMPRESSED, TRUE-COLOR IMAGE
//      = 3  UNCOMPRESSED, BLACK AND WHITE IMAGE (black and white is actually grayscale)
//      = 9  RUN-LENGTH ENCODED COLOR-MAPPED IMAGE
//      = 10 RUN-LENGTH ENCODED TRUE-COLOR IMAGE
//      = 11 RUN-LENGTH ENCODED BLACK AND WHITE IMAGE
//    FIELD 4: COLOR MAP SPECIFICATION (5 BYTES)
//      4.1: COLOR MAP ORIGIN (2 BYTES)
//      4.2: COLOR MAP LENGTH (2 BYTES)
//      4.3: COLOR MAP ENTRY SIZE (1 BYTES)
//    FIELD 5:IMAGE SPECIFICATION (10 BYTES)
//      5.1: X-ORIGIN OF IMAGE (2 BYTES)
//      5.2: Y-ORIGIN OF IMAGE (2 BYTES)
//      5.3: WIDTH OF IMAGE (2 BYTES)
//      5.4: HEIGHT OF IMAGE (2 BYTES)
//      5.5: IMAGE PIXEL SIZE (1 BYTE)
//      5.6: IMAGE DESCRIPTOR BYTE (1 BYTE)
//        bit 0..3: attribute bits per pixel
//        bit 4..5: image orientation:
//          0: bottom left
//          1: bottom right
//          2: top left
//          3: top right
//        bit 6..7: interleaved flag
//          0: two way (even-odd) interleave (e.g. IBM Graphics Card Adapter), obsolete
//          1: four way interleave (e.g. AT&T 6300 High Resolution), obsolete
//    FIELD 6: IMAGE ID FIELD (LENGTH SPECIFIED BY FIELD 1)
//    FIELD 7: COLOR MAP DATA (BIT WIDTH SPECIFIED BY FIELD 4.3 AND
//             NUMBER OF COLOR MAP ENTRIES SPECIFIED IN FIELD 4.2)
//    FIELD 8: IMAGE DATA FIELD (WIDTH AND HEIGHT SPECIFIED IN FIELD 5.3 AND 5.4)

const
  TARGA_NO_COLORMAP = 0;
  TARGA_COLORMAP = 1;

  TARGA_EMPTY_IMAGE = 0;
  TARGA_INDEXED_IMAGE = 1;
  TARGA_TRUECOLOR_IMAGE = 2;
  TARGA_BW_IMAGE = 3;
  TARGA_INDEXED_RLE_IMAGE = 9;
  TARGA_TRUECOLOR_RLE_IMAGE = 10;
  TARGA_BW_RLE_IMAGE = 11;

type
  PTargaHeader = ^TTargaHeader; 
  TTargaHeader = packed record
    IDLength,
    ColorMapType,
    ImageType: Byte;
    ColorMapOrigin,
    ColorMapSize: Word;
    ColorMapEntrySize: Byte;
    XOrigin,
    YOrigin,
    Width,
    Height: Word;
    PixelSize: Byte;
    ImageDescriptor: Byte;
  end;


//----------------------------------------------------------------------------------------------------------------------

class function TTargaGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := Size > SizeOf(TTargaHeader);
  if Result then
    with PTargaHeader(Memory)^ do
    begin
      // Targa images are hard to determine because there is no magic id or something like that.
      // Hence all we can do is to check if all values from the header are within correct limits.
      Result := (ImageType in [TARGA_EMPTY_IMAGE, TARGA_INDEXED_IMAGE, TARGA_TRUECOLOR_IMAGE, TARGA_BW_IMAGE,
        TARGA_INDEXED_RLE_IMAGE, TARGA_TRUECOLOR_RLE_IMAGE, TARGA_BW_RLE_IMAGE]) and
        (ColorMapType in [TARGA_NO_COLORMAP, TARGA_COLORMAP]) and
        (ColorMapEntrySize in [0, 15, 16, 24, 32]) and
        (PixelSize in [8, 15, 16, 24, 32]);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTargaGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  Run,
  Source,
  Buffer: PByte;
  I: Integer;
  LineSize: Integer;
  LineBuffer: Pointer;
  LogPalette: TMaxLogPalette;
  Header: TTargaHeader;
  FlipV: Boolean;
  Decoder: TTargaRLEDecoder;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
    with FImageProperties do
    begin
      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      Move(Memory^, Header, SizeOf(Header));
      FlipV := (Header.ImageDescriptor and $20) <> 0;
      Header.ImageDescriptor := Header.ImageDescriptor and $F;

      // skip image ID
      Source := Pointer(PChar(Memory) + SizeOf(Header) + Header.IDLength);

      with ColorManager do
      begin
        SourceSamplesPerPixel := SamplesPerPixel;
        TargetSamplesPerPixel := SamplesPerPixel;
        SourceColorScheme := ColorScheme;
        SourceOptions := [];
        TargetColorScheme := csBGR;
        SourceBitsPerSample := BitsPerSample;
        TargetBitsPerSample := BitsPerSample;
        PixelFormat := TargetPixelFormat;
      end;
      
      if (Header.ColorMapType = TARGA_COLORMAP) or
         (Header.ImageType in [TARGA_BW_IMAGE, TARGA_BW_RLE_IMAGE]) then
      begin
        if Header.ImageType in [TARGA_BW_IMAGE, TARGA_BW_RLE_IMAGE] then
          Palette := ColorManager.CreateGrayscalePalette(False)
        else
        begin
          LineSize := (Header.ColorMapEntrySize div 8) * Header.ColorMapSize;
          GetMem(LineBuffer, LineSize);
          try
            Move(Source^, LineBuffer^, LineSize);
            Inc(Source, LineSize);
            case Header.ColorMapEntrySize of
              32:
                Palette := ColorManager.CreateColorPalette([LineBuffer], pfInterlaced8Quad, Header.ColorMapSize, True);
              24:
                Palette := ColorManager.CreateColorPalette([LineBuffer], pfInterlaced8Triple, Header.ColorMapSize, True);
            else
              with LogPalette do
              begin
                // read palette entries and create a palette
                ZeroMemory(@LogPalette, SizeOf(LogPalette));
                palVersion := $300;
                palNumEntries := Header.ColorMapSize;

                // 15 and 16 bits per color map entry (handle both like 555 color format
                // but make 8 bit from 5 bit per color component)
                for I := 0 to Header.ColorMapSize - 1 do
                begin
                  palPalEntry[I].peBlue := Byte((PWord(Source)^ and $1F) shl 3);
                  palPalEntry[I].peGreen := Byte((PWord(Source)^ and $3E0) shr 2);
                  palPalEntry[I].peRed := Byte((PWord(Source)^ and $7C00) shr 7);
                  Inc(PWord(Source));
                end;
                Palette := CreatePalette(PLogPalette(@LogPalette)^);
              end;
            end;
          finally
            if Assigned(LineBuffer) then
              FreeMem(LineBuffer);
          end;
        end;
      end;

      Self.Width := Header.Width;
      Self.Height := Header.Height;

      LineSize := Width * (Header.PixelSize div 8);
      Progress(Self, psEnding, 0, False, FProgressRect, '');

      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
      case Header.ImageType of
        TARGA_EMPTY_IMAGE: // nothing to do here
          ;
        TARGA_BW_IMAGE,
        TARGA_INDEXED_IMAGE,
        TARGA_TRUECOLOR_IMAGE:
          begin
            for I := 0 to Height - 1 do
            begin
              if FlipV then
                LineBuffer := ScanLine[I]
              else
                LineBuffer := ScanLine[Header.Height - (I + 1)];
              Move(Source^, LineBuffer^, LineSize);
              Inc(Source, LineSize);
              Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end;
          end;
        TARGA_BW_RLE_IMAGE,
        TARGA_INDEXED_RLE_IMAGE,
        TARGA_TRUECOLOR_RLE_IMAGE:
          begin
            Buffer := nil;
            Decoder := TTargaRLEDecoder.Create(Header.PixelSize);
            try
              // Targa RLE is not line oriented. Convert all the RLE data in one rush.
              GetMem(Buffer, Height * LineSize);
              Run := Buffer;
              Decoder.Decode(Pointer(Source), Pointer(Buffer), Width, Height * Width);

              // Finally put data into the image.
              for I := 0 to Height - 1 do
              begin
                if FlipV then
                  LineBuffer := ScanLine[I]
                else
                  LineBuffer := ScanLine[Header.Height - (I + 1)];
                Move(Run^, LineBuffer^, LineSize);
                Inc(Run, LineSize);
                Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            finally
              if Assigned(Buffer) then
                FreeMem(Buffer);
              FreeAndNil(Decoder);
            end;
          end;
      end;
      Progress(Self, psEnding, 0, False, FProgressRect, '');
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TTargaGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Header: TTargaHeader;

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
    with FImageProperties do
    begin
      Move(Memory^, Header, SizeOf(Header));
      Header.ImageDescriptor := Header.ImageDescriptor and $F;

      Width := Header.Width;
      Height := Header.Height;
      BitsPerSample := 8;

      case Header.PixelSize of
        8:
          begin
            if Header.ImageType in [TARGA_BW_IMAGE, TARGA_BW_RLE_IMAGE] then
              ColorScheme := csG
            else
              ColorScheme := csIndexed;
            SamplesPerPixel := 1;
          end;
        15,
        16: // actually, 16 bit are meant being 15 bit
          begin
            ColorScheme := csRGB;
            BitsPerSample := 5;
            SamplesPerPixel := 3;
          end;
        24:
          begin
            ColorScheme := csRGB;
            SamplesPerPixel := 3;
          end;
        32:
          begin
            ColorScheme := csRGBA;
            SamplesPerPixel := 4;
          end;
      end;

      BitsPerPixel := SamplesPerPixel * BitsPerSample;
      if Header.ImageType in [TARGA_BW_RLE_IMAGE, TARGA_INDEXED_RLE_IMAGE, TARGA_TRUECOLOR_RLE_IMAGE] then
        Compression := ctRLE
      else
        Compression := ctNone;

      Result := True;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTargaGraphic.SaveToStream(Stream: TStream);

begin
  SaveToStream(Stream, True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTargaGraphic.SaveToStream(Stream: TStream; Compressed: Boolean);

// The format of the image to be saved depends on the current properties of the bitmap not
// on the values which may be set in the header during a former load.

var
  RLEBuffer: Pointer;
  I: Integer;
  LineSize: Integer;
  WriteLength: Cardinal;
  LogPalette: TMaxLogPalette;
  BPP: Byte;
  Header: TTargaHeader;
  Encoder: TTargaRLEDecoder;
  
begin
  FProgressRect := Rect(0, 0, Width, 1);
  Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);
  // prepare color depth
  case PixelFormat of
    pf1Bit,
    pf4Bit: // Note: 1 bit and 4 bits per pixel are not supported in the Targa format, an image
            //       with one of these pixel formats is implicitly converted to 256 colors.
      begin
        PixelFormat := pf8Bit;
        BPP := 1;
      end;
    pf8Bit:
      BPP := 1;
    pf15Bit,
    pf16Bit:
      BPP := 2;
    pf24Bit:
      BPP := 3;
    pf32Bit:
      BPP := 4;
  else
    BPP := GetDeviceCaps(Canvas.Handle, BITSPIXEL) div 8;
  end;

  if not Empty then
  begin
    with Header do
    begin
      IDLength := 0;
      if BPP = 1 then
        ColorMapType := 1
      else
        ColorMapType := 0;
      if not Compressed then
        // can't distinct between a B&W and an color indexed image here, so I use always the latter
        if BPP = 1 then
          ImageType := TARGA_INDEXED_IMAGE
        else
          ImageType := TARGA_TRUECOLOR_IMAGE
      else
        if BPP = 1 then
          ImageType := TARGA_INDEXED_RLE_IMAGE
        else
          ImageType := TARGA_TRUECOLOR_RLE_IMAGE;

      ColorMapOrigin := 0;
      XOrigin := 0;
      YOrigin := 0;
      Width := Self.Width;
      Height := Self.Height;
      PixelSize := 8 * BPP;
      // if the image is a bottom-up DIB then indicate this in the image descriptor
      if Cardinal(Scanline[0]) > Cardinal(Scanline[1]) then
        ImageDescriptor := $20
      else
        ImageDescriptor := 0;

      Stream.Write(Header, SizeOf(Header));

      // store color palette if necessary
      if ColorMapType = 1 then
      begin
        with LogPalette do
        begin
          // read palette entries
          GetPaletteEntries(Palette, 0, 256, palPalEntry);
          for I := 0 to 255 do
          begin
            Stream.Write(palPalEntry[I].peBlue, 1);
            Stream.Write(palPalEntry[I].peGreen, 1);
            Stream.Write(palPalEntry[I].peRed, 1);
          end;
        end;
        ColorMapSize := 256;
        ColorMapEntrySize := 24;
      end
      else
      begin
        ColorMapSize := 0;
        ColorMapEntrySize := 0;
      end;
    end;

    LineSize := Width * (Header.PixelSize div 8);
    Progress(Self, psEnding, 0, False, FProgressRect, '');

    Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
    // finally write image data
    if Compressed then
    begin
      RLEBuffer := nil;
      Encoder := TTargaRLEDecoder.Create(Header.PixelSize);
      try
        GetMem(RLEBuffer, 2 * LineSize);
        for I := 0 to Height - 1 do
        begin
          Encoder.Encode(ScanLine[I], RLEBuffer, Width, WriteLength);
          Stream.WriteBuffer(RLEBuffer^, WriteLength);

          Progress(Self, psRunning, 0, False, FProgressRect, '');
          OffsetRect(FProgressRect, 0, 1);
        end;
      finally
        if Assigned(RLEBuffer) then
         FreeMem(RLEBuffer);
        Encoder.Free;
      end;
    end
    else
    begin
      for I := 0 to Height - 1 do
      begin
        Stream.WriteBuffer(ScanLine[I]^, LineSize);

        Progress(Self, psRunning, 0, False, FProgressRect, '');
        OffsetRect(FProgressRect, 0, 1);
      end;
    end;

    Progress(Self, psEnding, 0, False, FProgressRect, '');
  end;
end;

{$endif TargaGraphic}

//----------------- TPCXGraphic ----------------------------------------------------------------------------------------

{$ifdef PCXGraphic}

type
  PPCXHeader = ^TPCXHeader;
  TPCXHeader = record
    FileID: Byte;                      // $0A for PCX files, $CD for SCR files
    Version: Byte;                     // 0: version 2.5; 2: 2.8 with palette; 3: 2.8 w/o palette; 5: version 3
    Encoding: Byte;                    // 0: uncompressed; 1: RLE encoded
    BitsPerPixel: Byte;
    XMin,
    YMin,
    XMax,
    YMax,                              // coordinates of the corners of the image
    HRes,                              // horizontal resolution in dpi
    VRes: Word;                        // vertical resolution in dpi
    ColorMap: array[0..15] of TRGB;    // color table
    Reserved,
    ColorPlanes: Byte;                 // color planes (at most 4)
    BytesPerLine,                      // number of bytes of one line of one plane
    PaletteType: Word;                 // 1: color or b&w; 2: gray scale
    Fill: array[0..57] of Byte;
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TPCXGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := Size > SizeOf(TPCXHeader);
  if Result then
    with PPCXHeader(Memory)^ do
    begin
      Result := (FileID in [$0A, $0C]) and (Version in [0, 2, 3, 5]) and (Encoding in [0, 1]);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPCXGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  Header: TPCXHeader;
  Run: PByte;

  //--------------- local functions -------------------------------------------

  procedure MakePalette;

  var
    PaletteData: PByte;

  begin
    if (Header.Version <> 3) or (PixelFormat = pf1Bit) then
    begin
      case PixelFormat of
        pf1Bit:
          Palette := ColorManager.CreateGrayScalePalette(False);
        pf4Bit:
          with Header do
          begin
            if paletteType = 2 then
              Palette := ColorManager.CreateGrayScalePalette(False)
            else
              Palette := ColorManager.CreateColorPalette([@ColorMap], pfInterlaced8Triple, 16, False);
          end;
        pf8Bit:
          begin
            // 256 colors with 3 components plus one marker byte
            PaletteData := Pointer(PChar(Memory) + Size - 769);
            if PaletteData^ <> $0C then
            begin
              // palette ID is wrong, perhaps gray scale?
              if Header.PaletteType = 2 then
                Palette := ColorManager.CreateGrayScalePalette(False);
              // else ignore palette
            end
            else
            begin
              Inc(PaletteData);
              Palette := ColorManager.CreateColorPalette([PaletteData], pfInterlaced8Triple, 256, False);
            end;
          end;
      end;
    end
    else
    begin
      // version 2.8 without palette information, just use the system palette
      // 256 colors will not be correct with this assignment...
      Palette := SystemPalette16;
    end;
  end;

  //--------------- end local functions ---------------------------------------

var
  PCXSize,
  DataSize: Integer;
  DecodeBuffer: Pointer;
  Plane1,
  Plane2,
  Plane3,
  Plane4: PByte;
  Value,
  Mask: Byte;
  I, J: Integer;
  Line: PByte;
  Increment: Integer;
  NewPixelFormat: TPixelFormat;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    FProgressRect := Rect(0, 0, Width, 1);
    Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

    Run := Memory;
    Move(Run^, Header, SizeOf(Header));
    Inc(Run, SizeOf(Header));
    with Header, FImageProperties do
    begin
      if not (FileID in [$0A, $CD]) then
        GraphicExError(gesInvalidImage, ['PCX, PCC or SCR']);

      with ColorManager do
      begin
        SourceColorScheme := ColorScheme;
        SourceBitsPerSample := BitsPerSample;
        SourceSamplesPerPixel := SamplesPerPixel;
        if ColorScheme = csIndexed then
          TargetColorScheme := csIndexed
        else
          TargetColorScheme := csBGR;
        if BitsPerPixel = 2 then
          TargetBitsPerSample := 4
        else
          TargetBitsPerSample := BitsPerSample;
        // Note: pixel depths of 2 and 4 bits may not be used with more than one plane
        //       otherwise the image will not show up correctly
        TargetSamplesPerPixel := SamplesPerPixel;
      end;

      NewPixelFormat := ColorManager.TargetPixelFormat;
      if NewPixelFormat = pfCustom then
      begin
        // There can be a special case comprising 4 planes each with 1 bit.
        if (SamplesPerPixel = 4) and (BitsPerPixel = 4) then
          NewPixelFormat := pf4Bit
        else
          GraphicExError(gesInvalidColorFormat, ['PCX']);
      end;

      PixelFormat := NewPixelFormat;
      // 256 colors palette is appended to the actual PCX data.
      PCXSize := Size;
      if PixelFormat = pf8Bit then
        Dec(PCXSize, 769);
      if PixelFormat <> pf24Bit then
        MakePalette;

      Self.Width := Width;
      Self.Height := Height;

      // adjust alignment of line
      Increment := SamplesPerPixel * Header.BytesPerLine;

      // allocate pixel data buffer and decode data if necessary
      if Compression = ctRLE then
      begin
        DataSize := Increment * Height;
        GetMem(DecodeBuffer, DataSize);

        with TPCXRLEDecoder.Create do
        try
          Decode(Pointer(Run), DecodeBuffer, PCXSize, DataSize);
        finally
          Free;
        end;
      end
      else
      begin
        GetMem(DecodeBuffer, PCXSize);
        Move(Run^, DecodeBuffer^, PCXSize);
      end;
      Progress(Self, psEnding, 0, False, FProgressRect, '');

      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
      try
        Run := DecodeBuffer;

        if (SamplesPerPixel = 4) and (BitsPerPixel = 4) then
        begin
          // 4 planes with one bit

          for I := 0 to Height - 1 do
          begin
            Plane1 := Run;
            PChar(Plane2) := PChar(Run) + Increment div 4;
            PChar(Plane3) := PChar(Run) + 2 * (Increment div 4);
            PChar(Plane4) := PChar(Run) + 3 * (Increment div 4);

            Line := ScanLine[I];
            // number of bytes to write
            DataSize := (Width * BitsPerPixel + 7) div 8;
            Mask := 0;
            while DataSize > 0 do
            begin
              Value := 0;
              for J := 0 to 1 do
              asm
                MOV AL, [Value]

                MOV EDX, [Plane4]             // take the 4 MSBs from the 4 runs and build a nibble
                SHL BYTE PTR [EDX], 1         // read MSB and prepare next run at the same time
                RCL AL, 1                     // MSB from previous shift is in CF -> move it to AL

                MOV EDX, [Plane3]             // now do the same with the other three runs
                SHL BYTE PTR [EDX], 1
                RCL AL, 1

                MOV EDX, [Plane2]
                SHL BYTE PTR [EDX], 1
                RCL AL, 1

                MOV EDX, [Plane1]
                SHL BYTE PTR [EDX], 1
                RCL AL, 1

                MOV [Value], AL
              end;
              Line^ := Value;
              Inc(Line);
              Dec(DataSize);

              // two runs above (to construct two nibbles -> one byte), now update marker
              // to know when to switch to next byte in the planes
              Mask := (Mask + 2) mod 8;
              if Mask = 0 then
              begin
                Inc(Plane1);
                Inc(Plane2);
                Inc(Plane3);
                Inc(Plane4);
              end;
            end;
            Inc(Run, Increment);
            
            Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
            OffsetRect(FProgressRect, 0, 1);
          end;
        end
        else
          if PixelFormat = pf24Bit then
          begin
            // true color
            for I := 0 to Height - 1 do
            begin
              Line := ScanLine[I];
              Plane1 := Run;
              PChar(Plane2) := PChar(Run) + Increment div 3;
              PChar(Plane3) := PChar(Run) + 2 * (Increment div 3);
              ColorManager.ConvertRow([Plane1, Plane2, Plane3], Line, Width, $FF);
              Inc(Run, Increment);

              Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end
          end
          else
          begin
            // other indexed formats
            for I := 0 to Height - 1 do
            begin
              Line := ScanLine[I];
              ColorManager.ConvertRow([Run], Line, Width, $FF);
              Inc(Run, Increment);

              Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end;
          end;
      finally
        if Assigned(DecodeBuffer) then
          FreeMem(DecodeBuffer);
      end;
      Progress(Self, psEnding, 0, False, FProgressRect, '');
    end;
  end
  else
    GraphicExError(gesInvalidImage, ['PCX, PCC or SCR']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPCXGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Header: PPCXHeader;

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
  begin
    Header := Memory;
    with FImageProperties do
    begin
      if Header.FileID in [$0A, $CD] then
      begin
        Width := Header.XMax - Header.XMin + 1;
        Height := Header.YMax - Header.YMin + 1;

        SamplesPerPixel := Header.ColorPlanes;
        BitsPerSample := Header.BitsPerPixel;
        BitsPerPixel := BitsPerSample * SamplesPerPixel;
        if BitsPerPixel <= 8 then
          ColorScheme := csIndexed
        else
          ColorScheme := csRGB;
        if Header.Encoding = 1 then
          Compression := ctRLE
        else
          Compression := ctNone;
        XResolution := Header.HRes;
        YResolution := Header.VRes;

        Result := True;
      end
      else
        Result := False;
    end;
  end;
end;

{$endif PCXGraphic}

//----------------- TPCDGraphic ----------------------------------------------------------------------------------------

{$ifdef PCDGraphic}

const
  PCD_BEGIN_BASE16 = 8192;
  PCD_BEGIN_BASE4 = 47104;
  PCD_BEGIN_BASE = 196608;
  PCD_BEGIN_ORIENTATION = 194635;
  PCD_BEGIN = 2048;

  PCD_MAGIC = 'PCD_IPI';

//----------------------------------------------------------------------------------------------------------------------

class function TPCDGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

var
  ID1, ID2 : PChar;

begin
  Result := Size > 3 * $800;
  if Result then
  begin
    ID1 := Memory;
    ID2 := ID1 + $800;
    Result := (StrLComp(ID1, 'PCD_OPA', 7) = 0) or (StrLComp(ID2, 'PCD', 3) = 0);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPCDGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 2);

var
  C1, C2, YY: PChar;
  YCbCrData: array[0..2] of PChar;
  {SourceDummy,
  DestDummy: Pointer;}

  Offset, I,
  X, Y,
  Rows: Integer;
  Columns: Cardinal;
  ScanLines: array of Pointer;

  LineBuffer: Pointer;
  Line,
  Run: PBGR;
  Decoder: TPCDDecoder;

  Source: PByte;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    with FImageProperties do
    begin
      Source := Memory;
      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);
      Columns := 192 shl Min(ImageIndex, 2);
      Rows := 128 shl Min(ImageIndex, 2);

      // since row and columns might be swapped because of rotated images
      // we determine the final dimensions once more
      Width := 192 shl ImageIndex;
      Height := 128 shl ImageIndex;

      ZeroMemory(@YCbCrData, SizeOf(YCbCrData));
      try
        GetMem(YCbCrData[0], Width * Height);
        GetMem(YCbCrData[1], Width * Height);
        GetMem(YCbCrData[2], Width * Height);

        // advance to image data 
        Offset := 96;
        if Overview then
          Offset := 5
        else
          if ImageIndex = 1 then
            Offset := 23
          else
            if ImageIndex = 0 then
              Offset := 4;
        Inc(Source, Offset * $800);

        // color conversion setup
        with ColorManager do
        begin
          SourceColorScheme := csPhotoYCC;
          SourceBitsPerSample := 8;
          SourceSamplesPerPixel := 3;
          TargetColorScheme := csBGR;
          TargetBitsPerSample := 8;
          TargetSamplesPerPixel := 3;
        end;
        PixelFormat := pf24Bit;
        // PhotoYCC format uses CCIR Recommendation 709 coefficients and is subsampled
        // by factor 2 vertically and horizontally
        ColorManager.SetYCbCrParameters([0.2125, 0.7154, 0.0721], 2, 2);

        Progress(Self, psEnding, 0, False, FProgressRect, '');

        if Overview then
        begin
          // if Overview then ... no info yet about overview image structure
        end
        else
        begin
          YY := YCbCrData[0];
          C1 := YCbCrData[1];
          C2 := YCbCrData[2];
          I := 0;
          Progress(Self, psStarting, 0, False, FProgressRect, gesLoadingData);
          while I < Rows do
          begin
            Progress(Self, psRunning, MulDiv(I, 100, Rows), False, FProgressRect, '');

            Move(Source^, YY^, Columns);
            Inc(YY, Width);
            Inc(Source, Columns);

            Move(Source^, YY^, Columns);
            Inc(YY, Width);
            Inc(Source, Columns);

            Move(Source^, C1^, Columns shr 1);
            Inc(C1, Width);
            Inc(Source, Columns shr 1);

            Move(Source^, C2^, Columns shr 1);
            Inc(C2, Width);
            Inc(Source, Columns shr 1);

            Inc(I, 2);
          end;
          Progress(Self, psEnding, 0, False, FProgressRect, '');

          Progress(Self, psStarting, 0, False, FProgressRect, gesUpsampling);
          // Y stands here for maximum number of upsample calls.
          Y := 5;
          if ImageIndex >= 3 then
          begin
            Inc(Y, 3 * (ImageIndex - 3));

            Decoder := TPCDDecoder.Create(Source);
            //SourceDummy := @YCbCrData;
            //DestDummy := nil;
            try
              // Recover luminance deltas for 1536 x 1024 image.
              Progress(Self, psRunning, MulDiv(0, 100, Y), False, FProgressRect, '');
              Upsample(768, 512, Width, YCbCrData[0]);
              Progress(Self, psRunning, MulDiv(1, 100, Y), False, FProgressRect, '');
              Upsample(384, 256, Width, YCbCrData[1]);
              Progress(Self, psRunning, MulDiv(2, 100, Y), False, FProgressRect, '');
              Upsample(384, 256, Width, YCbCrData[2]);

              // The decoder does not work as expected. Larger resolutions are not loaded but created by scaling.
              //Decoder.Decode(SourceDummy, DestDummy, Width, 1024);
              if ImageIndex >= 4 then
              begin
                // recover luminance deltas for 3072 x 2048 image
                Progress(Self, psRunning, MulDiv(3, 100, Y), False, FProgressRect, '');
                Upsample(1536, 1024, Width, YCbCrData[0]);
                Progress(Self, psRunning, MulDiv(4, 100, Y), False, FProgressRect, '');
                Upsample(768, 512, Width, YCbCrData[1]);
                Progress(Self, psRunning, MulDiv(5, 100, Y), False, FProgressRect, '');
                Upsample(768, 512, Width, YCbCrData[2]);

                //Decoder.Decode(SourceDummy, DestDummy, Width, 2048);
                if ImageIndex = 5 then
                begin
                  // recover luminance deltas for 6144 x 4096 image (vaporware)
                  Progress(Self, psRunning, MulDiv(6, 100, Y), False, FProgressRect, '');
                  Upsample(3072, 2048, Width, YCbCrData[1]);
                  Progress(Self, psRunning, MulDiv(7, 100, Y), False, FProgressRect, '');
                  Upsample(1536, 1024, Width, YCbCrData[1]);
                  Progress(Self, psRunning, MulDiv(8, 100, Y), False, FProgressRect, '');
                  Upsample(1536, 1024, Width, YCbCrData[2]);
                end;
              end;
            finally
              FreeAndNil(Decoder);
            end;
          end;

          Progress(Self, psRunning, MulDiv(Y - 1, 100, Y), False, FProgressRect, '');
          Upsample(Width shr 1, Height shr 1, Width, YCbCrData[1]);
          Progress(Self, psRunning, MulDiv(Y, 100, Y), False, FProgressRect, '');
          Upsample(Width shr 1, Height shr 1, Width, YCbCrData[2]);

          Progress(Self, psEnding, 0, False, FProgressRect, '');

          Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
          // transfer luminance and chrominance channels
          YY := YCbCrData[0];
          C1 := YCbCrData[1];
          C2 := YCbCrData[2];

          // For the rotated mode where we need to turn the image by 90°. We can speed up loading
          // the image by factor 2 by using a local copy of the Scanline pointers.
          if Rotate in [1, 3] then
          begin
            Self.Width := Height;
            Self.Height := Width;
            FProgressRect.Right := Height;
            
            SetLength(ScanLines, Width);
            for Y := 0 to Width - 1 do
              ScanLines[Y] := ScanLine[Y];
            GetMem(LineBuffer, 3 * Width);
          end
          else
          begin
            ScanLines := nil;
            Self.Width := Width;
            Self.Height := Height;
            LineBuffer := nil;
          end;

          try
            case Rotate of
              1: // rotate -90° 
                begin
                  for Y := 0 to Height - 1 do
                  begin
                    ColorManager.ConvertRow([YY, C1, C2], LineBuffer, Width, $FF);
                    Inc(YY, Width);
                    Inc(C1, Width);
                    Inc(C2, Width);

                    Run := LineBuffer;
                    for X := 0 to Width - 1 do
                    begin
                      PChar(Line) := PChar(ScanLines[Width - X - 1]) + Y * 3;
                      Line^ := Run^;
                      Inc(Run);
                    end;

                    Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                    OffsetRect(FProgressRect, 0, 1);
                  end;
                end;
              3: // rotate 90°
                begin
                  for Y := 0 to Height - 1 do
                  begin
                    ColorManager.ConvertRow([YY, C1, C2], LineBuffer, Width, $FF);
                    Inc(YY, Width);
                    Inc(C1, Width);
                    Inc(C2, Width);

                    Run := LineBuffer;
                    for X := 0 to Width - 1 do
                    begin
                      PChar(Line) := PChar(ScanLines[X]) + (Height - Y - 1) * 3;
                      Line^ := Run^;
                      Inc(Run);
                    end;

                    Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                    OffsetRect(FProgressRect, 0, 1);
                  end;
                end;
            else
              for Y := 0 to Height - 1 do
              begin
                ColorManager.ConvertRow([YY, C1, C2], ScanLine[Y], Width, $FF);
                Inc(YY, Width);
                Inc(C1, Width);
                Inc(C2, Width);

                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            end;
            Progress(Self, psEnding, 0, False, FProgressRect, '');
          finally
            ScanLines := nil;
            if Assigned(LineBuffer) then
              FreeMem(LineBuffer);
          end;
        end;

      finally
        if Assigned(YCbCrData[2]) then
          FreeMem(YCbCrData[2]);
        if Assigned(YCbCrData[1]) then
          FreeMem(YCbCrData[1]);
        if Assigned(YCbCrData[0]) then
          FreeMem(YCbCrData[0]);
      end;
    end;
  end
  else
    GraphicExError(gesInvalidImage, ['PCD']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPCDGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Header: PChar;
  Temp: Cardinal;

begin
  if ImageIndex > 5 then
    ImageIndex := 5;
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex) and (Size > 3 * $800);

  if Result then
    with FImageProperties do
    begin
      Header := Memory;

      Overview := StrLComp(Header, 'PCD_OPA', 7) = 0;
      // determine if image is a PhotoCD image
      if Overview or (StrLComp(Header + $800, 'PCD', 3) = 0) then
      begin
        Rotate := Byte(Header[$0E02]) and 3;

        // image sizes are fixed, depending on the given image index
        if Overview then
          ImageIndex := 0;
        Width := 192 shl ImageIndex;
        Height := 128 shl ImageIndex;
        if (Rotate = 1) or (Rotate = 3) then
        begin
          Temp := Width;
          Width := Height;
          Height := Temp;
        end;
        ColorScheme := csPhotoYCC;
        BitsPerSample := 8;
        SamplesPerPixel := 3;
        BitsPerPixel := BitsPerSample * SamplesPerPixel;
        if ImageIndex > 2 then
          Compression := ctPCDHuffmann
        else
          Compression := ctNone;

        if Overview then
          ImageCount := (Byte(Header[10]) shl 8) or Byte(Header[11])
        else
          ImageCount := 5; // These are the always present image resolutions.

        Result := True;
      end
      else
        Result := False;
    end;
end;

{$endif PCDGraphic}

//----------------- TPPMGraphic ----------------------------------------------------------------------------------------

{$ifdef PortableMapGraphic}

class function TPPMGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := Size > 10;
  if Result then
  begin
    // These are weak criteria here, but there is nothing more to test for this image format.
    Result := (PChar(Memory)^ = 'P') and (PChar(Memory)[1] in ['1'..'6']);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPPMGraphic.GetChar: Char;

begin
  if FRemainingSize = 0 then
    GraphicExError(gesStreamReadError, ['PPM']);
  Result := FSource^;
  Inc(FSource);
  Dec(FRemainingSize);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPPMGraphic.GetNumber: Cardinal;

// reads the next number from the stream (and skips all characters which are not in 0..9)

var
  Ch: Char;

begin
  // skip all non-numbers
  repeat
    Ch := GetChar;
    // skip comments
    if Ch = '#' then
    begin
      ReadLine;
      Ch := GetChar;
    end;
  until Ch in ['0'..'9'];

  // read the number characters and convert meanwhile
  Result := 0;
  repeat
    Result := 10 * Result + Ord(Ch) - $30;
    Ch := GetChar;
  until not (Ch in ['0'..'9']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPPMGraphic.ReadLine: string;

// reads one text line from stream and skips comments

var
  Ch: Char;
  I: Integer;

begin
  Result := '';
  repeat
    Ch := GetChar;
    if Ch in [#13, #10] then
      Break
    else
      Result := Result + Ch;
  until False;
  // eat #13#10 combination
  if (Ch = #13) and (FSource = #10) then
    GetChar;

  // delete comments
  I := Pos('#', Result);
  if I > 0 then
    Delete(Result, I, MaxInt);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPPMGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); 

var
  Line24: PBGR;
  Line8: PByte;
  X, Y: Integer;
  Pixel: Byte;
  MaxVal: Word;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    FSource := Memory;
    FRemainingSize := Size;
    with FImageProperties do
    begin
      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);

      if GetChar <> 'P' then
        GraphicExError(gesInvalidImage, ['PBM, PGM or PPM']);
      case StrToInt(GetChar) of
        1: // PBM ASCII format (black & white)
          begin
            PixelFormat := pf1Bit;
            Self.Width := GetNumber;
            Self.Height := GetNumber;
            ColorManager.TargetSamplesPerPixel := 1;
            ColorManager.TargetBitsPerSample := 1;
            Palette := ColorManager.CreateGrayScalePalette(True);

            // read image data
            for Y := 0 to Height - 1 do
            begin
              Line8 := ScanLine[Y];
              Pixel := 0;
              for X := 1 to Width do
              begin
                Pixel := (Pixel shl 1) or (GetNumber and 1);
                if (X mod 8) = 0 then
                begin
                  Line8^ := Pixel;
                  Inc(Line8);
                  Pixel := 0;
                end;
              end;
              if (Width mod 8) <> 0 then
                Line8^ := Pixel shl (8 - (Width mod 8));

              Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end;
          end;
        2: // PGM ASCII form (gray scale)
          begin
            PixelFormat := pf8Bit;
            Self.Width := GetNumber;
            Self.Height := GetNumber;
            // skip maximum color value
            GetNumber;
            ColorManager.TargetSamplesPerPixel := 1;
            ColorManager.TargetBitsPerSample := 8;
            Palette := ColorManager.CreateGrayScalePalette(False);

            // read image data
            for Y := 0 to Height - 1 do
            begin
              Line8 := ScanLine[Y];
              for X := 0 to Width - 1 do
              begin
                Line8^ := GetNumber;
                Inc(Line8);
              end;

              Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end;
          end;
        3: // PPM ASCII form (true color)
          begin
            PixelFormat := pf24Bit;
            Self.Width := GetNumber;
            Self.Height := GetNumber;
            // skip maximum color value
            GetNumber;

            for Y := 0 to Height - 1 do
            begin
              Line24 := ScanLine[Y];
              for X := 0 to Width - 1 do
              begin
                Line24.R := GetNumber;
                Line24.G := GetNumber;
                Line24.B := GetNumber;
                Inc(Line24);
              end;

              Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end;
          end;
        4: // PBM binary format (black & white)
          begin
            PixelFormat := pf1Bit;
            Self.Width := GetNumber;
            Self.Height := GetNumber;
            ColorManager.TargetSamplesPerPixel := 1;
            ColorManager.TargetBitsPerSample := 1;
            Palette := ColorManager.CreateGrayScalePalette(True);

            // read image data
            for Y := 0 to Height - 1 do
            begin
              Line8 := ScanLine[Y];
              for X := 0 to (Width div 8) - 1 do
              begin
                Line8^ := Byte(GetChar);
                Inc(Line8);
              end;
              if (Width mod 8) <> 0 then
                Line8^ := Byte(GetChar);

              Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end;
          end;
        5: // PGM binary form (gray scale)
          begin
            PixelFormat := pf8Bit;
            Self.Width := GetNumber;
            Self.Height := GetNumber;
            // skip maximum color value
            GetNumber;
            ColorManager.TargetSamplesPerPixel := 1;
            ColorManager.TargetBitsPerSample := 8;
            Palette := ColorManager.CreateGrayScalePalette(False);

            // read image data
            for Y := 0 to Height - 1 do
            begin
              Line8 := ScanLine[Y];
              for X := 0 to Width - 1 do
              begin
                Line8^ := Byte(GetChar);
                Inc(Line8);
              end;

              Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end;
          end;
        6: // PPM binary form (true color)
          begin
            PixelFormat := pf24Bit;
            Self.Width := GetNumber;
            Self.Height := GetNumber;
            MaxVal := GetNumber;

            // Pixel values are store linearly (but RGB instead BGR).
            // There's one allowed white space which will automatically be skipped by the first
            // GetChar call below
            // now read the pixels
            for Y := 0 to Height - 1 do
            begin
              Line24 := ScanLine[Y];
              if MaxVal = 255 then
                for X := 0 to Width - 1 do
                begin
                  Line24.R := Byte(GetChar);
                  Line24.G := Byte(GetChar);
                  Line24.B := Byte(GetChar);
                  Inc(Line24);
                end
              else if MaxVal < 255 then
                for X := 0 to Width - 1 do
                begin
                  // These floating point calculations are the same as the GIMP's PPM scaling.
                  // Precomputing 255/MaxVal or using MulDiv both give slightly different results
                  // due to precision differences.
                  // Paint Shop Pro's calculations for MaxVal < 255 are screwed up, and I couldn't
                  // figure out the exact algorithm they're using.
                  Line24.R := Trunc(Byte(GetChar) * 255 / MaxVal);
                  Line24.G := Trunc(Byte(GetChar) * 255 / MaxVal);
                  Line24.B := Trunc(Byte(GetChar) * 255 / MaxVal);
                  Inc(Line24);
                end
              else
                GraphicExError(gesInvalidImage, ['PBM, PGM or PPM']);
                // TODO: PPM does support a MaxVal up to 65535, but I don't have any sample files to test
//                for X := 0 to Width - 1 do
//                begin
//                  Line24.R := Trunc(Byte(GetChar) shl 8 + Byte(GetChar), 255, MaxVal);
//                  Line24.G := Trunc(Byte(GetChar) shl 8 + Byte(GetChar), 255, MaxVal);
//                  Line24.B := Trunc(Byte(GetChar) shl 8 + Byte(GetChar), 255, MaxVal);
//                  Inc(Line24);
//                end;

              Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end;
          end;
        else
          GraphicExError(gesInvalidImage, ['PBM, PGM or PPM']);
      end;
      Progress(Self, psEnding, 0, False, FProgressRect, '');
    end;
  end
  else
    GraphicExError(gesInvalidImage, ['PBM, PGM or PPM']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPPMGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
    with FImageProperties do
    begin
      FSource := Memory;
      FRemainingSize := Size;

      Compression := ctNone;

      if GetChar = 'P' then
      begin
        case StrToInt(GetChar) of
          1: // PBM ASCII format (black & white)
            begin
              Width := GetNumber;
              Height := GetNumber;

              SamplesPerPixel := 1;
              BitsPerSample := 1;
              ColorScheme := csIndexed;
              BitsPerPixel := SamplesPerPixel * BitsPerSample;
            end;
          2: // PGM ASCII form (gray scale)
            begin
              Width := GetNumber;
              Height := GetNumber;
              // skip maximum color value
              GetNumber;

              SamplesPerPixel := 1;
              BitsPerSample := 8;
              ColorScheme := csIndexed;
              BitsPerPixel := SamplesPerPixel * BitsPerSample;
            end;
          3: // PPM ASCII form (true color)
            begin
              Width := GetNumber;
              Height := GetNumber;
              // skip maximum color value
              GetNumber;

              SamplesPerPixel := 3;
              BitsPerSample := 8;
              ColorScheme := csRGB;
              BitsPerPixel := SamplesPerPixel * BitsPerSample;
            end;
          4: // PBM binary format (black & white)
            begin
              Width := GetNumber;
              Height := GetNumber;

              SamplesPerPixel := 1;
              BitsPerSample := 1;
              ColorScheme := csIndexed;
              BitsPerPixel := SamplesPerPixel * BitsPerSample;
            end;
          5: // PGM binary form (gray scale)
            begin
              Width := GetNumber;
              Height := GetNumber;
              // skip maximum color value
              GetNumber;

              SamplesPerPixel := 1;
              BitsPerSample := 8;
              ColorScheme := csIndexed;
              BitsPerPixel := SamplesPerPixel * BitsPerSample;
            end;
          6: // PPM binary form (true color)
            begin
              Width := GetNumber;
              Height := GetNumber;
              // skip maximum color value
              GetNumber;

              SamplesPerPixel := 3;
              BitsPerSample := 8;
              ColorScheme := csRGB;
              BitsPerPixel := SamplesPerPixel * BitsPerSample;
            end;
        else
          Result := False;
        end;
      end
      else
        Result := False;
    end;
end;

{$endif PortableMapGraphic}

//----------------- TCUTGraphic ----------------------------------------------------------------------------------------

{$ifdef CUTGraphic}

class function TCUTGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

// Note: cut files cannot be determined from stream because the only information
//       is width and height of the image at stream/image start which is by no means
//       enough to identify a cut (or any other) image.

begin
  Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCUTGraphic.LoadFromFile(const FileName: string);

// Overridden to extract an implicit palette file name.

begin
  FPaletteFile := ChangeFileExt(FileName, '.pal');
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCUTGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  Source: PByte;
  Line: Pointer;
  Decoder: TCUTRLEDecoder;
  Y: Integer;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    with FImageProperties do
    begin
      Source := Pointer(PChar(Memory) + 6);

      FProgressRect := Rect(0, 0, Width, 0);
      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);

      PixelFormat := pf8Bit;
      Self.Width := Width;
      Self.Height := Height;
      LoadPalette;

      Decoder := TCUTRLEDecoder.Create;
      try
        for Y := 0 to Height - 1 do
        begin
          Line := ScanLine[Y];
          Decoder.Decode(Pointer(Source), Line, 0, Width);

          Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
          OffsetRect(FProgressRect, 0, 1);
        end;
      finally
        FreeAndNil(Decoder);
      end;

      Progress(Self, psEnding, 0, False, FProgressRect, '');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCUTGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Run: PWord;
  
begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
    with FImageProperties do
    begin
      PixelFormat := pf8Bit;
      Run := Memory;
      Width := Run^;
      Inc(Run);
      Height := Run^;

      ColorScheme := csIndexed;
      BitsPerSample := 8;
      SamplesPerPixel := 1;
      BitsPerPixel := BitsPerSample * SamplesPerPixel;

      Compression := ctRLE;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  // the palette file header is actually more complex than the
  // image file's header, funny...
  PHaloPaletteHeader = ^THaloPaletteHeader;
  THaloPaletteHeader = packed record
    ID: array[0..1] of Char;  // should be 'AH'
    Version,
    Size: Word;
    FileType,
    SubType: Byte;
    BrdID,
    GrMode: Word;
    MaxIndex,
    MaxRed,
    MaxGreen,
    MaxBlue: Word; // colors = MaxIndex + 1
    Signature: array[0..7] of Char; // 'Dr. Halo'
    Filler: array[0..11] of Byte;
  end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCUTGraphic.LoadPalette;

var
  Header: PHaloPaletteHeader;
  LogPalette: TMaxLogPalette;
  I: Integer;
  Buffer: array[0..511] of Byte;
  Run: PWord;

begin
  LogPalette.palVersion := $300;
  if FileExists(FPaletteFile) then
  begin
    with TFileStream.Create(FPaletteFile, fmOpenRead or fmShareDenyNone) do
    try
      // quite strange file organization here, we need always to load 512 bytes blocks
      // and skip occasionally some bytes
      ReadBuffer(Buffer, SizeOf(Buffer));
      Header := @Buffer;
      LogPalette.palNumEntries := Header.MaxIndex + 1;
      Run := @Buffer;
      Inc(PByte(Run), SizeOf(Header^));
      for I := 0 to LogPalette.palNumEntries - 1 do
      begin
        // load next 512 bytes buffer if necessary
        if (Integer(Run) - Integer(@Buffer)) > 506 then
        begin
          ReadBuffer(Buffer, SizeOf(Buffer));
          Run := @Buffer;
        end;
        LogPalette.palPalEntry[I].peRed := Byte(Run^);
        Inc(Run);
        LogPalette.palPalEntry[I].peGreen := Byte(Run^);
        Inc(Run);
        LogPalette.palPalEntry[I].peBlue := Byte(Run^);
        Inc(Run);
      end;
    finally
      Free;
    end;
  end
  else
  begin
    LogPalette.palNumEntries := 256;
    // no external palette so use gray scale
    for I := 0 to 255 do
    begin
      LogPalette.palPalEntry[I].peBlue := I;
      LogPalette.palPalEntry[I].peGreen := I;
      LogPalette.palPalEntry[I].peRed := I;
    end;
  end;

  // finally create palette
  Palette := CreatePalette(PLogPalette(@LogPalette)^);
end;

{$endif CUTGraphic}

//----------------- TGIFGraphic ----------------------------------------------------------------------------------------

{$ifdef GIFGraphic}

const
  // logical screen descriptor packed field masks
  GIF_GLOBALCOLORTABLE = $80;
  GIF_COLORRESOLUTION = $70;
  GIF_GLOBALCOLORTABLESORTED = $08; 
  GIF_COLORTABLESIZE = $07;

  // image flags
  GIF_LOCALCOLORTABLE = $80;
  GIF_INTERLACED = $40;
  GIF_LOCALCOLORTABLESORTED= $20;

  // block identifiers
  GIF_PLAINTEXT = $01;
  GIF_GRAPHICCONTROLEXTENSION = $F9;
  GIF_COMMENTEXTENSION = $FE;
  GIF_APPLICATIONEXTENSION = $FF;
  GIF_IMAGEDESCRIPTOR = Ord(',');
  GIF_EXTENSIONINTRODUCER = Ord('!');
  GIF_TRAILER = Ord(';');
  
type
  PGIFHeader = ^TGIFHeader;
  TGIFHeader = packed record
    Signature: array[0..2] of Char; // magic ID 'GIF'
    Version: array[0..2] of Char;   // '87a' or '89a' 
  end;

  TLogicalScreenDescriptor = packed record
    ScreenWidth: Word;
    ScreenHeight: Word;
    PackedFields,
    BackgroundColorIndex, // index into global color table
    AspectRatio: Byte;    // actual ratio = (AspectRatio + 15) / 64
  end;

  TImageDescriptor = packed record
    //Separator: Byte; // leave that out since we always read one bye ahead
    Left: Word;		 // X position of image with respect to logical screen
    Top: Word;		 // Y position
    Width: Word;
    Height: Word;
    PackedFields: Byte;
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TGIFGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := (Size > (SizeOf(TGIFHeader) + SizeOf(TLogicalScreenDescriptor) + SizeOf(TImageDescriptor))) and
    (StrLIComp(PChar(Memory), 'GIF', 3) = 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGIFGraphic.SkipExtensions: Byte;

// Skips all blocks until an image block has been found in the data stream.
// Result is the image block ID if an image block could be found.

var
  Increment: Byte;
  Content : array[0..255] of Char; // Gif comment sub-block has a maximum size of 255 bytes

begin
  FImageProperties.Comment := '';
  
  // Iterate through the blocks until first image is found.
  repeat
    Result := FSource^;
    Inc(FSource);
    if Result = GIF_EXTENSIONINTRODUCER then
    begin
      // Read the block control label and act accordingly.
      Result := FSource^;
      Inc(FSource);
      case Result of
        GIF_PLAINTEXT:
          begin
            // Block size of text grid data.
            Increment := FSource^;
            Inc(FSource, Increment + 1);
            // Skip variable lengthed text block.
            repeat
              // Block size.
              Increment := FSource^;
              Inc(FSource);
              if Increment = 0 then
                Break;
              Inc(FSource, Increment);
            until False;
          end;
        GIF_GRAPHICCONTROLEXTENSION:
          begin
            // Block size.
            Increment := FSource^;
            Inc(FSource);
            if Increment > 0 then
            begin
              // The graphic control extention includes the transparency flag.
              // Read this and the transparency color index.
              if (FSource^ and 1) <> 0 then
              begin
                // Image is transparent, read index.
                Transparent := True;
                FTransparentIndex := Byte((PChar(FSource) + 3)^);
              end;
              Inc(FSource, Increment);
            end;
            // Finally skip terminator.
            Inc(FSource);
          end;
        GIF_COMMENTEXTENSION:
          repeat
            // block size
            Increment := FSource^;
            Inc(FSource);
            if Increment = 0 then
              Break;
            Move(FSource^, Content, Increment);
            Content[Increment] := #0;
            FImageProperties.Comment := FImageProperties.Comment + Content;
          until False;
        GIF_APPLICATIONEXTENSION:
          begin
            // application id and authentication code plus potential application data
            repeat
              Increment := FSource^;
              Inc(FSource);
              if Increment = 0 then
                Break;
              Inc(FSource, Increment);
            until False;
          end;
      end;
    end;
  until (Result = GIF_IMAGEDESCRIPTOR) or (Result = GIF_TRAILER);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGIFGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  Header: TGIFHeader;
  ScreenDescriptor: TLogicalScreenDescriptor;
  ImageDescriptor: TImageDescriptor;
  LogPalette: TMaxLogPalette;
  I: Integer;
  BlockID: Byte;
  InitCodeSize: Byte;
  RawData,
  Run: PByte;
  TargetBuffer,
  TargetRun,
  Line: Pointer;
  Pass,
  Increment: Integer;
  Marker: Pointer;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    with FImageProperties do
    begin
      Transparent := False;

      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      FSource := Memory;
      Move(FSource^, Header, SizeOf(Header));
      Inc(FSource, SizeOf(Header));

      PixelFormat := pf8Bit;
      
      // Read general information.
      Move(FSource^, ScreenDescriptor, SizeOf(ScreenDescriptor));
      Inc(FSource, SizeOf(ScreenDescriptor));

      ZeroMemory(@LogPalette, SizeOf(LogPalette));
      LogPalette.palVersion := $300;
      // Read global color table if given.
      if (ScreenDescriptor.PackedFields and GIF_GLOBALCOLORTABLE) <> 0 then
      begin
        // The global color table immediately follows the screen descriptor.
        LogPalette.palNumEntries := 2 shl (ScreenDescriptor.PackedFields and GIF_COLORTABLESIZE);
        for I := 0 to LogPalette.palNumEntries - 1 do
        begin
          LogPalette.palPalEntry[I].peRed := FSource^;   Inc(FSource);
          LogPalette.palPalEntry[I].peGreen := FSource^; Inc(FSource);
          LogPalette.palPalEntry[I].peBlue := FSource^;  Inc(FSource);
        end;
        // Finally create the palette.
        Palette := CreatePalette(PLogPalette(@LogPalette)^);
      end;

      BlockID := SkipExtensions;

      // SkipExtensions might have set the transparent property.
      if Transparent then
        // If transparent color index is valid then get transparent color.
        if FTransparentIndex < LogPalette.palNumEntries then
          with LogPalette.palPalEntry[FTransparentIndex] do
          TransparentColor := RGB(peRed, peGreen, peBlue);

      Progress(Self, psEnding, 0, False, FProgressRect, '');

      // image found?
      if BlockID = GIF_IMAGEDESCRIPTOR then
      begin
        Progress(Self, psStarting, 0, False, FProgressRect, gesLoadingData);
        Move(FSource^, ImageDescriptor, SizeOf(TImageDescriptor));
        Inc(FSource, SizeOf(TImageDescriptor));
        Self.Width := Width;
        Self.Height := Height;

        // if there is a local color table then override the already set one
        if (ImageDescriptor.PackedFields and GIF_LOCALCOLORTABLE) <> 0 then
        begin
          // the global color table immediately follows the image descriptor
          LogPalette.palNumEntries := 2 shl (ImageDescriptor.PackedFields and GIF_COLORTABLESIZE);
          for I := 0 to LogPalette.palNumEntries - 1 do
          begin
            LogPalette.palPalEntry[I].peRed := FSource^;   Inc(FSource);
            LogPalette.palPalEntry[I].peGreen := FSource^; Inc(FSource);
            LogPalette.palPalEntry[I].peBlue := FSource^;  Inc(FSource);
          end;
          Palette := CreatePalette(PLogPalette(@LogPalette)^);
        end;

        InitCodeSize := FSource^;
        Inc(FSource);
        // decompress data in one step
        // 1) count data
        Marker := FSource;
        Pass := 0;
        repeat
          Increment := FSource^;
          Inc(FSource);
          Inc(Pass, Increment);
          Inc(FSource, Increment);
        until Increment = 0;

        // 2) allocate enough memory
        GetMem(RawData, Pass);
        // add one extra line of extra memory for badly coded images
        GetMem(TargetBuffer, Width * (Height + 1));

        try
          // 3) read and decode data
          FSource := Marker;
          Run := RawData;
          repeat
            Increment := FSource^;
            Inc(FSource);
            Move(FSource^, Run^, Increment);
            Inc(Run, Increment);
            Inc(FSource, Increment);
          until Increment = 0;

          Decoder := TGIFLZWDecoder.Create(InitCodeSize);
          try
            Run := RawData;
            Decoder.Decode(Pointer(Run), TargetBuffer, Pass, Width * Height);
          finally
            FreeAndNil(Decoder);
          end;
          Progress(Self, psEnding, 0, False, FProgressRect, '');

          // finally transfer image data
          Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
          if (ImageDescriptor.PackedFields and GIF_INTERLACED) = 0 then
          begin
            TargetRun := TargetBuffer;
            for I := 0 to Height - 1 do
            begin
              Line := Scanline[I];
              Move(TargetRun^, Line^, Width);
              Inc(PByte(TargetRun), Width);

              Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end;
          end
          else
          begin
            TargetRun := TargetBuffer;
            // interlaced image, need to move in four passes
            for Pass := 0 to 3 do
            begin
              // determine start line and increment of the pass
              case Pass of
                0:
                  begin
                    I := 0;
                    Increment := 8;
                  end;
                1:
                  begin
                    I := 4;
                    Increment := 8;
                  end;
                2:
                  begin
                    I := 2;
                    Increment := 4;
                  end;
              else
                I := 1;
                Increment := 2;
              end;

              while I < Height do
              begin
                Line := Scanline[I];
                Move(TargetRun^, Line^, Width);
                Inc(PByte(TargetRun), Width);
                Inc(I, Increment);

                if Pass = 3 then
                begin
                  // progress events only for last (and most expensive) run
                  Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
                  OffsetRect(FProgressRect, 0, 1);
                end;
              end;
            end;
          end;
          Progress(Self, psEnding, 0, False, FProgressRect, '');
        finally
          if Assigned(TargetBuffer) then
            FreeMem(TargetBuffer);
          if Assigned(RawData) then
            FreeMem(RawData);
        end;
      end;
    end;
  end
  else
    GraphicExError(gesInvalidImage, ['GIF']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGIFGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Header: TGIFHeader;
  ScreenDescriptor: TLogicalScreenDescriptor;
  ImageDescriptor: TImageDescriptor;
  BlockID: Integer;

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
    with FImageProperties do
    begin
      FSource := Memory;
      Move(FSource^, Header, SizeOf(Header));
      Inc(FSource, SizeOf(Header));
      if UpperCase(Header.Signature) = 'GIF' then
      begin
        Version := StrToInt(Copy(Header.Version, 1, 2));
        ColorScheme := csIndexed;
        SamplesPerPixel := 1;
        // might be overwritten
        BitsPerSample := 8;
        Compression := ctLZW;

        // general information
        Move(FSource^, ScreenDescriptor, SizeOf(ScreenDescriptor));
        Inc(FSource, SizeOf(ScreenDescriptor));

        // Skip global color table if given.
        if (ScreenDescriptor.PackedFields and GIF_GLOBALCOLORTABLE) <> 0 then
        begin
          BitsPerSample := (ScreenDescriptor.PackedFields and GIF_COLORTABLESIZE) + 1;
          // The global color table immediately follows the screen descriptor.
          Inc(FSource, 3 * (1 shl BitsPerSample));
        end;

        BlockID := SkipExtensions;

        // Image found?
        if BlockID = GIF_IMAGEDESCRIPTOR then
        begin
          Move(FSource^, ImageDescriptor, SizeOf(TImageDescriptor));

          Width := ImageDescriptor.Width;
          if Width = 0 then
            Width := ScreenDescriptor.ScreenWidth;
          Height := ImageDescriptor.Height;
          if Height = 0 then
            Height := ScreenDescriptor.ScreenHeight;

          // if there is a local color table then override the already set one
          LocalColorTable := (ImageDescriptor.PackedFields and GIF_LOCALCOLORTABLE) <> 0;
          if LocalColorTable then
            BitsPerSample := (ImageDescriptor.PackedFields and GIF_LOCALCOLORTABLE) + 1;
          Interlaced := (ImageDescriptor.PackedFields and GIF_INTERLACED) <> 0;
        end;

        BitsPerPixel := SamplesPerPixel * BitsPerSample;

        Result := True;
      end
      else
        Result := False;
    end;
end;

{$endif GIFGraphic}

//----------------- TRLAGraphic ----------------------------------------------------------------------------------------

{$ifdef RLAGraphic}

// This implementation is based on code from Dipl. Ing. Ingo Neumann (ingo@upstart.de, ingo_n@dialup.nacamar.de).

type
  TRLAWindow = packed record
    Left,
    Right,
    Bottom,
    Top: SmallInt;
  end;

  PRLAHeader = ^TRLAHeader;
  TRLAHeader = packed record
    Window,                            // overall image size
    Active_window: TRLAWindow;         // size of non-zero portion of image (we use this as actual image size)
    Frame,                             // frame number if part of a sequence
    Storage_type,                      // type of image channels (0 - integer data, 1 - float data)
    Num_chan,                          // samples per pixel (usually 3: r, g, b)
    Num_matte,                         // number of matte channels (usually only 1)
    Num_aux,                           // number of auxiliary channels, usually 0
    Revision: SmallInt;                // always $FFFE
    Gamma: array[0..15] of Char;       // gamma single value used when writing the image
    Red_pri: array[0..23] of Char;     // used chromaticity for red channel (typical format: "%7.4f %7.4f")
    Green_pri: array[0..23] of Char;   // used chromaticity for green channel
    Blue_pri: array[0..23] of Char;    // used chromaticity for blue channel
    White_pt: array[0..23] of Char;    // used chromaticity for white point
    Job_num: Integer;                  // rendering speciifc
    Name: array[0..127] of Char;       // original file name
    Desc: array[0..127] of Char;       // a file description
    ProgramName: array[0..63] of Char; // name of program which created the image
    Machine: array[0..31] of Char;     // name of computer on which the image was rendered
    User: array[0..31] of Char;        // user who ran the creation program of the image
    Date: array[0..19] of Char;        // creation data of image (ex: Sep 30 12:29 1993)
    Aspect: array[0..23] of Char;      // aspect format of the file (external resource)
    Aspect_ratio: array[0..7] of Char; // float number Width /Height
    Chan: array[0..31] of Char;        // color space (can be: rgb, xyz, sampled or raw)
    Field: SmallInt;                   // 0 - non-field rendered data, 1 - field rendered data
    Time: array[0..11] of Char;        // time needed to create the image (used when rendering)
    Filter: array[0..31] of Char;      // filter name to post-process image data
    Chan_bits,                         // bits per sample
    Matte_type,                        // type of matte channel (see aux_type)
    Matte_bits,                        // precision of a pixel's matte channel (1..32)
    Aux_type,                          // type of aux channel (0 - integer data; 4 - single (float) data
    Aux_bits: SmallInt;                // bits precision of the pixel's aux channel (1..32 bits)
    Aux: array[0..31] of Char;         // auxiliary channel as either range or depth
    Space: array[0..35] of Char;       // unused
    Next: Integer;                     // offset for next header if multi-frame image
  end;
  
//----------------------------------------------------------------------------------------------------------------------

class function TRLAGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := Size > SizeOf(TRLAHeader);
  if Result then
    with PRLAHeader(Memory)^ do
      Result := (Word(Revision) = $FEFF) and ((StrLIComp(Chan, 'rgb', 3) = 0) or (StrLIComp(Chan, 'xyz', 3) = 0));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRLAGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); 

var
  Offsets: TCardinalArray;
  RLELength: Word;
  Line: Pointer;
  Y: Integer;

  // RLE buffers
  RawBuffer,
  RedBuffer,
  GreenBuffer,
  BlueBuffer,
  AlphaBuffer: Pointer;
  Decoder: TRLADecoder;

  Run: PByte;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    with FImageProperties do
    begin
      Run := Memory;
      
      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);

      with ColorManager do
      begin
        SourceSamplesPerPixel := SamplesPerPixel;
        TargetSamplesPerPixel := SamplesPerPixel;

        SourceBitsPerSample := BitsPerSample;
        if BitsPerSample > 8 then
          TargetBitsPerSample := 8
        else
          TargetBitsPerSample := BitsPerSample;
        SourceColorScheme := ColorScheme;
        if ColorScheme = csRGBA then
          TargetColorScheme := csBGRA
        else
          TargetColorScheme := csBGR;

        PixelFormat := TargetPixelFormat;

        if FileGamma <> 1 then
        begin
          SetGamma(FileGamma);
          TargetOptions := TargetOptions + [coApplyGamma];
          Include(Options, ioUseGamma);
        end;
      end;

      // dimension of image, top might be larger than bottom denoting a bottom up image
      Self.Width := Width;
      Self.Height := Height;

      // Each scanline is organized in RLE compressed strips whose location in the stream
      // is determined by the offsets table.
      SetLength(Offsets, Height);
      Move(Run^, Offsets[0], Height * SizeOf(Cardinal));
      Inc(Run, Height * SizeOf(Cardinal));
      SwapLong(Pointer(Offsets), Height);

      // Setup intermediate storage.
      Decoder := TRLADecoder.Create;
      RawBuffer := nil;
      RedBuffer := nil;
      GreenBuffer := nil;
      BlueBuffer := nil;
      AlphaBuffer := nil;
      try
        GetMem(RedBuffer, Width);
        GetMem(GreenBuffer, Width);
        GetMem(BlueBuffer, Width);
        GetMem(AlphaBuffer, Width);

        // no go for each scanline
        for Y := 0 to Height - 1 do
        begin
          Run := Pointer(PChar(Memory) + Offsets[Y]);
          if BottomUp then
            Line := ScanLine[Height - Y - 1]
          else
            Line := ScanLine[Y];
          // read channel data to decode
          // red
          Move(Run^, RLELength, SizeOf(RLELength));
          Inc(Run, SizeOf(RLELength));
          RLELength := Swap(RLELength);
          RawBuffer := Run;
          Inc(Run, RLELength);
          Decoder.Decode(RawBuffer, RedBuffer, RLELength, Width);
          // green
          Move(Run^, RLELength, SizeOf(RLELength));
          Inc(Run, SizeOf(RLELength));
          RLELength := Swap(RLELength);
          RawBuffer := Run;
          Inc(Run, RLELength);
          Decoder.Decode(RawBuffer, GreenBuffer, RLELength, Width);
          // blue
          Move(Run^, RLELength, SizeOf(RLELength));
          Inc(Run, SizeOf(RLELength));
          RLELength := Swap(RLELength);
          RawBuffer := Run;
          Inc(Run, RLELength);
          Decoder.Decode(RawBuffer, BlueBuffer, RLELength, Width);

          if ColorManager.TargetColorScheme = csBGR then
          begin
            ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer], Line, Width, $FF);
          end
          else
          begin
            // alpha
            Move(Run^, RLELength, SizeOf(RLELength));
            Inc(Run, SizeOf(RLELength));
            RLELength := Swap(RLELength);
            Decoder.Decode(Pointer(Run), AlphaBuffer, RLELength, Width);

            ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer], Line, Width, $FF);
          end;

          Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
          OffsetRect(FProgressRect, 0, 1);
        end;
      finally
        if Assigned(RedBuffer) then
          FreeMem(RedBuffer);
        if Assigned(GreenBuffer) then
          FreeMem(GreenBuffer);
        if Assigned(BlueBuffer) then
          FreeMem(BlueBuffer);
        if Assigned(AlphaBuffer) then
          FreeMem(AlphaBuffer);
        FreeAndNil(Decoder);
      end;
      Progress(Self, psEnding, 0, False, FProgressRect, '');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TRLAGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Header: TRLAHeader;
  Run: PByte;

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
    with FImageProperties do
    begin
      Run := Memory;
      Move(Run^, Header, SizeOf(Header));

      // data is always given in big endian order, so swap data which needs this
      SwapHeader(Header);
      Options := [ioBigEndian];

      SamplesPerPixel := Header.num_chan;
      if Header.num_matte = 1 then
        Inc(SamplesPerPixel);
      BitsPerSample := Header.Chan_bits;
      BitsPerPixel := SamplesPerPixel * BitsPerSample;

      if LowerCase(Header.Chan) = 'rgb' then
      begin
        if Header.num_matte > 0 then
          ColorScheme := csRGBA
        else
          ColorScheme := csRGB;
      end
      else
        // if LowerCase(Header.Chan) = 'xyz' then
        ColorScheme := csUnknown;

      FileGamma := StrToFloatDef(Header.Gamma, 1);

      Compression := ctRLE;

      // dimension of image, top might be larger than bottom denoting a bottom up image
      Width := Header.Active_window.Right - Header.Active_window.Left + 1;
      Height := Abs(Header.Active_window.Bottom - Header.Active_window.Top) + 1;
      BottomUp := (Header.Active_window.Bottom - Header.Active_window.Top) < 0;

      Result := True;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRLAGraphic.SwapHeader(var Header);

// Separate swap method to ease reading the main flow of the LoadFromMemory method.

begin
  with TRLAHeader(Header) do
  begin
    SwapShort(@Window, 4);
    SwapShort(@Active_window, 4);
    Frame := Swap(Frame);
    Storage_type := Swap(Storage_type);
    Num_chan := Swap(Num_chan);
    Num_matte := Swap(Num_matte);
    Num_aux := Swap(Num_aux);
    Revision := Swap(Revision);
    Job_num  := SwapLong(Job_num);
    Field := Swap(Field);
    Chan_bits := Swap(Chan_bits);
    Matte_type := Swap(Matte_type);
    Matte_bits := Swap(Matte_bits);
    Aux_type := Swap(Aux_type);
    Aux_bits := Swap(Aux_bits);
    Next := SwapLong(Next);
  end;
end;

{$endif RLAGraphic}

//----------------- TPSDGraphic ----------------------------------------------------------------------------------------

{$ifdef PhotoshopGraphic}

const
  // color modes
  PSD_BITMAP = 0;
  PSD_GRAYSCALE = 1;
  PSD_INDEXED = 2;
  PSD_RGB = 3;
  PSD_CMYK = 4;
  PSD_MULTICHANNEL = 7;
  PSD_DUOTONE = 8;
  PSD_LAB = 9;

  PSD_COMPRESSION_NONE = 0;
  PSD_COMPRESSION_RLE = 1; // RLE compression (same as TIFF packed bits)

  PSDBlendModeMapping: array[TPSDLayerBlendMode] of PChar = (
    'norm', // lbmNormal
    'dark', // lbmDarken
    'lite', // lbmLighten
    'hue ', // lbmHue
    'sat ', // lbmSaturation
    'colr', // lbmColor
    'lum ', // lbmLuminosity
    'mul ', // lbmMultiply
    'scrn', // lbmScreen
    'diss', // lbmDissolve
    'over', // lbmOverlay
    'hLit', // lbmHardLight
    'sLit', // lbmSoftLight
    'diff', // lbmDifference
    'smud', // lbmExclusion
    'div ', // lbmColorDodge
    'idiv'  // lbmColorBur
  );

  // Resource IDs for records in the resource block.
  Obsolete1 = $03E8;                        // Obsolete—Photoshop 2.0 only. Contains five 2 byte values: number of
                                            // channels, rows, columns, depth, and mode.
  MacPrintManInfo = $03E9;                  // Optional. Macintosh print manager print info record.
  Obsolete2 = $03EB;                        // Obsolete—Photoshop 2.0 only. Contains the indexed color table.
  ResInfo = $03ED;                          // ResolutionInfo structure. See Appendix A in Photoshop SDK Guide.pdf.
  AlphaChannelNames = $03EE;                // Names of the alpha channels as a series of Pascal strings.
  DisplayInfo = $03EF;                      // DisplayInfo structure. See Appendix A in Photoshop SDK Guide.pdf.
  Caption = $03F0;                          // Optional. The caption as a Pascal string.
  Border = $03F1;                           // Border information. Contains a fixed-number for the border width, and 2
                                            // bytes for border units (1 = inches, 2 = cm, 3 = points, 4 = picas, 5 = columns).
  BackgroundColor = $03F2;                  // Background color. See the Colors additional file information.
  PrintFlags = $03F3;                       // Print flags. A series of one byte boolean values (see Page Setup dialog):
                                            // labels, crop marks, color bars, registration marks, negative, flip,
                                            // interpolate, caption.
  GrayMultichannelHalftoningInfo = $03F4;   // Grayscale and multichannel halftoning information.
  ColorHalftoningInfo = $03F5;              // Color halftoning information.
  DuotoneHalftoningInfo = $03F6;            // Duotone halftoning information.
  GrayMultiChannelTransferInfo = $03F7;     // Grayscale and multichannel transfer function.
  ColorTransferInfo = $03F8;                // Color transfer functions.
  DuotoneTransferInfo = $03F9;              // Duotone transfer functions.
  DuotoneImageInfo = $03FA;                 // Duotone image information.
  DotRange = $03FB;                         // Two bytes for the effective black and white values for the dot range.
  Obsolete3 = $03FC;                        // Obsolete.
  EPSOptions = $03FD;                       // EPS options.
  QuickMaskInfo = $03FE;                    // Quick Mask information. 2 bytes containing Quick Mask channel ID, 1 byte
                                            // boolean indicating whether the mask was initially empty.
  Obsolete4 = $03FF;                        // Obsolete.
  LayerStateInfo = $0400;                   // Layer state information. 2 bytes containing the index of target layer.
                                            // 0 = bottom layer.
  WorkingPath = $0401;                      // Working path (not saved). See path resource in Photoshop File Formats Spec.
  LayersGroupInfo = $0402;                  // Layers group information. 2 bytes per layer containing a group ID for the
                                            // dragging groups. Layers in a group have the same group ID.
  Obsolete5 = $0403;                        // Obsolete.
  IPTC_NAARecord = $0404;                   // IPTC-NAA record. This contains the File Info... information. See the
                                            // IIMV4.pdf document.
  RawImageMode = $0405;                     // Image mode for raw format files.
  JPEGQuality = $0406;                      // JPEG quality. Private.
  GridAndGuides = $0408;                    // Grid and guides information. 
  ThumbnailResource = $0409;                // Thumbnail resource. See thumbnail resource in Photoshop File Formats Spec.
  CopyrightFlg = $040A;                     // Copyright flag. Boolean indicating whether image is copyrighted. Can be
                                            // set via Property suite or by user in File Info...
  URL = $040B;                              // URL. Handle of a text string with uniform resource locator. Can be set
                                            // via Property suite or by user in File Info...
  ThumbnailResource2 = $040C;               // Thumbnail resource. See thumbnail resource in Photoshop File Formats Spec.
  GlobalAngle = $040D;                      // Global Angle. 4 bytes that contain an integer between 0..359 which is the
                                            // global lighting angle for effects layer. If not present assumes 30.
  ColorSamplersResource = $040E;            // Color samplers resource. 
  ICCProfile = $040F;                       // ICC Profile. The raw bytes of an ICC format profile, see the ICC34.pdf
                                            // and ICC34.h files from the Internation Color Consortium located in the
                                            // documentation section.
  Watermark = $0410;                        // One byte for Watermark.
  ICCUntagged = $0411;                      // ICC Untagged. 1 byte that disables any assumed profile handling when
                                            // opening the file. 1 = intentionally untagged.
  EffectsVisible = $0412;                   // Effects visible. 1 byte global flag to show/hide all the effects layer.
                                            // Only present when they are hidden.
  SpotHalftone = $0413;                     // Spot Halftone. 4 bytes for version, 4 bytes for length, and the variable
                                            // length data.
  DocumentSpecificIDs = $0414;              // Document specific IDs, layer IDs will be generated starting at this base
                                            // value or a greater value if we find existing IDs to already exceed it.
                                            // It’s purpose is to avoid the case where we add layers, flatten, save,
                                            // open, and then add more layers that end up with the same IDs as the first
                                            // set. 4 bytes.
  AlphaNames = $0415;                       // Unicode Alpha Names. 4 bytes for length and the string as a unicode string.
  ColorTableCount = $0416;                  // Indexed Color Table Count. 2 bytes for the number of colors in table that
                                            // are actually defined
  TransparentIndex = $0417;                 // New since version 6.0 of Adobe Photoshop: Tansparent Index. 2 bytes for
                                            // the index of transparent color, if any.
  GlobalAltitude = $0419;                   // New since version 6.0 of Adobe Photoshop: Global Altitude. 4 byte entry
                                            // for altitude
  Slices = $041A;                           // New since version 6.0 of Adobe Photoshop: Slices. See description later
                                            // in this chapter
  WorkflowURL = $041B;                      // New since version 6.0 of Adobe Photoshop: Workflow URL. Unicode string,
                                            // 4 bytes of length followed by unicode string.
  XPEP = $041C;                             // New since version 6.0 of Adobe Photoshop: Jump To XPEP. 2 bytes major
                                            // version, 2 bytes minor version, 4 bytes count. Following is repeated for
                                            // count: 4 bytes block size, 4 bytes key, if key = 'jtDd' then next is a
                                            // Boolean for the dirty flag otherwise it’s a 4 byte entry for the mod date.
  AlphaIdentifiers = $041D;                 // New since version 6.0 of Adobe Photoshop: Alpha Identifiers. 4 bytes of
                                            // length, followed by 4 bytes each for every alpha identifier.
  URLList = $041E;                          // New since version 6.0 of Adobe Photoshop: URL List. 4 byte count of URLs,
                                            // followed by 4 byte long, 4 byte ID, and unicode string for each count.
  VersionInfo = $0421;                      // New since version 6.0 of Adobe Photoshop: Version Info. 4 byte version,
                                            // 1 byte HasRealMergedData, unicode string of writer name, unicode string
                                            // of reader name, 4 bytes of file version.
  // $07D0 - $0BB6 Path Information (saved paths). See path resource format in Photoshop File Formats Spec.
  ClippingPathName = $0BB7;                 // Name of clipping path. See path resource format later in this chapter.
  PrintFlagsInfo = $2710;                   // Print flags information. 2 bytes version (= 1), 1 byte center crop marks,
                                            // 1 byte (= 0), 4 bytes bleed width value, 2 bytes bleed width scale.

type
  PPSDHeader = ^TPSDHeader;
  TPSDHeader = packed record
    Signature: array[0..3] of Char; // always '8BPS'
    Version: Word;                  // always 1
    Reserved: array[0..5] of Byte;  // reserved, always 0
    Channels: Word;                 // 1..24, number of channels in the image (including alpha)
    Rows,
    Columns: Cardinal;              // 1..30000, size of image
    Depth: Word;                    // 1, 8, 16 bits per channel
    Mode: Word;                     // color mode (see constants above)
  end;

  // Description of a channel in a layer.
  TPSDChannelLengthInfo = packed record
    ChannelID: SmallInt;            // 0 = red, 1 = green etc. -1 = transparency mask, -2 = user supplied mask
    Size: Cardinal;                 // Size of channel data.
  end;

//----------------- TPhotoshopLayer ------------------------------------------------------------------------------------

constructor TPhotoshopLayer.Create(Graphic: TPSDGraphic);

begin
  FGraphic := Graphic;
  FImage := TBitmap.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TPhotoshopLayer.Destroy;

  //---------------------------------------------------------------------------

  procedure FreeItem(var Data: TPSDItemData);

  // Releases any dynamically allocated memory in the item.
  // Works also recursive if necessary.

  var
    I: Integer;
    
  begin
    with Data do
    begin
      // List.
      if Assigned(List) then
      begin
        for I := 0 to High(List^) do
          FreeItem(List^[I]);
        Dispose(List);
      end;

      // Descriptor or global object.
      if Assigned(Descriptor) then
      begin
        for I := 0 to High(Descriptor.Items) do
          FreeItem(Descriptor.Items[I].Data);
        Descriptor.Items := nil;
        Dispose(Descriptor);
      end;

      // Raw data.
      if Assigned(Data) then
        FreeMem(Data);
    end;
  end;

  //---------------------------------------------------------------------------

var
  I: Integer;

begin
  FImage.Free;

  with FTypeToolInfo do
  begin
    with TextDescriptor do
    begin
      for I := 0 to High(Items) do
        FreeItem(Items[I].Data);
      Items := nil;
    end;
    with WarpDescriptor do
    begin
      for I := 0 to High(Items) do
        FreeItem(Items[I].Data);
      Items := nil;
    end;
  end;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPhotoshopLayer.SetImage(const Value: TBitmap);

begin
  FImage.Assign(Value);
end;

//----------------- TPhotoshopLayers -----------------------------------------------------------------------------------

constructor TPhotoshopLayers.Create(Graphic: TPSDGraphic);

begin
  FGraphic := Graphic;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPhotoshopLayers.Get(Index: Integer): TPhotoshopLayer;

begin
  Result := TPhotoshopLayer(inherited Get(Index));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPhotoshopLayers.Notify(Ptr: Pointer; Action: TListNotification);

begin
  if (Action = lnDeleted) and Assigned(Ptr) then
    TPhotoShopLayer(Ptr).Free;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPhotoshopLayers.Put(Index: Integer; Layer: TPhotoshopLayer);

begin
  inherited Put(Index, Layer);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPhotoshopLayers.Add(Layer: TPhotoshopLayer): Integer;

begin
  Result := inherited Add(Layer);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPhotoshopLayers.AddNewLayer: TPhotoshopLayer;

begin
  Result := TPhotoshopLayer.Create(FGraphic);
  inherited Add(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPhotoshopLayers.Extract(Layer: TPhotoshopLayer): TPhotoshopLayer;

begin
  Result := inherited Extract(Layer);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPhotoshopLayers.First: TPhotoshopLayer;

begin
  Result := TPhotoshopLayer(inherited First);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPhotoshopLayers.IndexOf(Layer: TPhotoshopLayer): Integer;

begin
  Result := inherited IndexOf(Layer);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPhotoshopLayers.Insert(Index: Integer; Layer: TPhotoshopLayer);

begin
  inherited Insert(Index, Layer);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPhotoshopLayers.Last: TPhotoshopLayer;

begin
  Result := TPhotoshopLayer(inherited Last);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPhotoshopLayers.Remove(Layer: TPhotoshopLayer): Integer;

begin
  Result := inherited Remove(Layer);
  Layer.Free;
end;

//----------------- TPSDGraphic ----------------------------------------------------------------------------------------

constructor TPSDGraphic.Create;

begin
  inherited;
  FLayers := TPhotoshopLayers.Create(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TPSDGraphic.Destroy;

begin
  FLayers.Free;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSDGraphic.CombineChannels(Layer: TPhotoshopLayer);

// Combines all separate channels of the given layer into the layer bitmap.
// Previously allocated memory is freed here too.

  //---------------------------------------------------------------------------

  function GetChannel(ID: Integer): Pointer;

  // Returns the reference of the channel with the given ID where ID means:
  // -2 = user data mask
  // -1 = alpha channel
  //  0 = red (or gray, or cyan etc.)
  //  1 = green (or magenta etc.)
  //  2 = blue (or yellow etc.)
  //  3 = black (for CMYK images)

  var
    I: Integer;

  begin
    Result := nil;
    I := 0;
    while I < Length(Layer.FChannels) do
    begin
      if Layer.FChannels[I].ChannelID = ID then
      begin
        Result := Layer.FChannels[I].Data;
        Break;
      end;
      Inc(I);
    end;
  end;

  //---------------------------------------------------------------------------

var
  RunR, RunG, RunB, RunA: PByte;
  Y: Integer;
  ChannelSize: Integer;

begin
  with Layer.FImage do
  begin
    ChannelSize := Width * Height;
    case PixelFormat of
      pf8Bit: // Alpha channel for gray scale images is currently ignored.
        begin
          RunR := GetChannel(0);
          for Y := 0 to Height - 1 do
          begin
            Move(RunR^, ScanLine[Y]^, Width);
            Inc(RunR, Width);
          end;
        end;
      pf24Bit: // RGB or Lab
        begin
          if FMode = PSD_CMYK then
          begin
            // Photoshop CMYK values are given with 0 for maximum values, but the
            // (general) CMYK conversion works with 255 as maxium value. Hence we must reverse
            // all entries in the buffer.
            RunR := GetChannel(0);
            RunG := GetChannel(1);
            RunB := GetChannel(2);
            RunA := GetChannel(3);
            for Y := 1 to ChannelSize do
            begin
              RunR^ := 255 - RunR^;
              Inc(RunR);
              RunG^ := 255 - RunG^;
              Inc(RunG);
              RunB^ := 255 - RunB^;
              Inc(RunB);
              RunA^ := 255 - RunA^;
              Inc(RunA);
            end;
            RunR := GetChannel(0);
            RunG := GetChannel(1);
            RunB := GetChannel(2);
            RunA := GetChannel(3);
          end
          else
          begin
            RunR := GetChannel(0);
            RunG := GetChannel(1);
            RunB := GetChannel(2);
            RunA := nil;
          end;
          for Y := 0 to Height - 1 do
          begin
            ColorManager.ConvertRow([RunR, RunG, RunB, RunA], ScanLine[Y], Width, $FF);
            Inc(RunR, Width);
            Inc(RunG, Width);
            Inc(RunB, Width);
          end;
        end;
      pf32Bit:
        begin
          if FMode = PSD_CMYK then
          begin
            // Photoshop CMYK values are given with 0 for maximum values, but the
            // (general) CMYK conversion works with 255 as maxium value. Hence we must reverse
            // all entries in the buffer.
            RunR := GetChannel(0);
            RunG := GetChannel(1);
            RunB := GetChannel(2);
            RunA := GetChannel(3);
            for Y := 1 to ChannelSize do
            begin
              RunR^ := 255 - RunR^;
              Inc(RunR);
              RunG^ := 255 - RunG^;
              Inc(RunG);
              RunB^ := 255 - RunB^;
              Inc(RunB);
              RunA^ := 255 - RunA^;
              Inc(RunA);
            end;
            RunR := GetChannel(0);
            RunG := GetChannel(1);
            RunB := GetChannel(2);
            RunA := GetChannel(3);
          end
          else
          begin
            // Either RGBA or Lab with alpha.
            RunR := GetChannel(0);
            RunG := GetChannel(1);
            RunB := GetChannel(2);
            RunA := GetChannel(-1);
          end;
          for Y := 0 to Height - 1 do
          begin
            ColorManager.ConvertRow([RunR, RunG, RunB, RunA], ScanLine[Y], Width, $FF);
            Inc(RunR, Width);
            Inc(RunG, Width);
            Inc(RunB, Width);
            Inc(RunA, Width);
          end;
        end;
    end;

    // Finally free all channel data.
    for Y := 0 to High(Layer.FChannels) do
      FreeMem(Layer.FChannels[Y].Data);
    Layer.FChannels := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPSDGraphic.ConvertCompression(Value: Word): TCompressionType;

begin
  case Value of
    0:
      Result := ctNone;
    1:
      Result := ctPackedBits;
    2: // not yet supported
      Result := ctPlainZIP;
    4: // not yet supported
      Result := ctPredictedZIP;
  else
    Result := ctUnknown;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPSDGraphic.DetermineColorScheme(ChannelCount: Integer): TColorScheme;

begin
  case FMode of
    PSD_DUOTONE, // duo tone should be handled as grayscale
    PSD_GRAYSCALE:
      case ChannelCount of
        1:
          Result := csG;
        2:
          Result := csGA;
      else
        Result := csUnknown;
      end;
    PSD_BITMAP:  // B&W
        Result := csG;
    PSD_INDEXED: // 8 bits only are assumed because 16 bit wouldn't make sense here
      case ChannelCount of
        1:
          Result := csIndexed;
        2:
          Result := csIndexedA;
      else
        Result := csUnknown;
      end;
    PSD_MULTICHANNEL,
    PSD_RGB:
      case ChannelCount of
        3:
          Result := csRGB;
        4:
          Result := csRGBA;
      else
        Result := csUnknown;
      end;
    PSD_CMYK:
      if ChannelCount >= 4 then
        Result := csCMYK
      else
        Result := csUnknown;
    PSD_LAB:
      if ChannelCount = 3 then
        Result := csCIELab
      else
        Result := csUnknown;
  else
    Result := csUnknown;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSDGraphic.LoadAdjustmentLayer(var Run: PChar; Layer: TPhotoshopLayer);

// Reads an adjustment layer whose identification is given by the first 4 bytes pointed to by Run.
// An adjustment layer is kind of a sub layer for the current layer.

const
  KeyCount = 36;
  AdjustmentKey: array[0..KeyCount - 1] of PChar = (
    'levl', //  0, Levels
    'curv', //  1, Curves
    'brit', //  2, Brightness/Contrast
    'blnc', //  3, Color balance
    'hue ', //  4, Old hue/saturation, Photoshop 4.0
    'hue2', //  5, New hue/saturation, Photoshop 5.0
    'selc', //  6, Selective color
    'thrs', //  7, Threshold
    'nvrt', //  8, Invert
    'post', //  9, Posterize
    'lrFX', // 10, Effects layer
    'tySh', // 11, Type tool info
    'luni', // 12, Unicode layer name
    'lyid', // 13, LayerID
    'lfx2', // 14, Object based effects layer info
    'Patt', // 15, Patterns
    'Anno', // 16, Annotations
    'clbl', // 17, Blend clipping elements
    'infx', // 18, Blend interior elements
    'knko', // 19, Knockout settings
    'lspf', // 20, Protected setting
    'lclr', // 21, Sheet color setting
    'fxrp', // 22, Reference point
    'grdm', // 23, Gradient settings
    'lsct', // 24, Section divider setting
    'brst', // 25, Channel blending restriction setting
    'SoCo', // 26, Solid color sheet setting
    'PtFl', // 27, Pattern fill setting
    'GdFl', // 28, Gradient fill setting
    'vmsk', // 29, Vector mask setting
    'TySh', // 30, Type tool object setting
    'ffxi', // 31, Foreign effect ID
    'lnsr', // 32, Layer name source setting
    'shpa', // 33, Pattern data
    'shmd', // 34, Meta data setting
    'Layr'  // 35, Layer data 
  );

  // Signatures used in an effects adjustment layer.
  EffectSignature: array[0..5] of PChar = (
    'cmnS', // 0, common state
    'dsdw', // 1, drop shadow
    'isdw', // 2, inner shadow
    'oglw', // 3, outer glow
    'iglw', // 4, inner glow
    'bevl'  // 5, bevel
  );

var
  I: Integer;
  Size: Cardinal;
  Temp: PChar;
  
begin
  // Find out which data there is.
  I := 0;
  while I < KeyCount do
  begin
    if StrLComp(Run, AdjustmentKey[I], 4) = 0 then
      Break;
    Inc(I);
  end;
  Inc(Run, 4);

  // Prepare read address after the adjustment layer, regardless whether we read the data or not.
  Size := ReadBigEndianCardinal(Run);
  Temp := Run + Size;
  // What type is it?
  case I of
    12: // Unicode layer name.
      Layer.FName := ReadBigEndianString(Run);
    30: // Type tool object settings (text layer).
      begin
        Layer.FType := ltText;
        // Skip version number (1 = Photoshop 6).
        Inc(Run, SizeOf(Word));
        with Layer.FTypeToolInfo do
        begin
          Transform.XX := ReadBigEndianDouble(Run);
          Transform.XY := ReadBigEndianDouble(Run);
          Transform.YX := ReadBigEndianDouble(Run);
          Transform.YY := ReadBigEndianDouble(Run);
          Transform.TX := ReadBigEndianDouble(Run);
          Transform.TY := ReadBigEndianDouble(Run);
          
          // Skip text descriptor version (= 50 for PS 6) and descriptor version (= 16 for PS 6) fields.
          Inc(Run, 6);
          // Read text descriptor.
          ReadDescriptor(Run, TextDescriptor);
          // Skip warp descriptor version (= 1 for PS 6) and descriptor version (= 16 for PS 6) fields.
          Inc(Run, 6);
          // Read warp descriptor.
          ReadDescriptor(Run, WarpDescriptor);

          // Finally read the warp rectangle. It is supposed to be four double values but
          // often is some other size. We can compensate for this by the overall size, but the
          // values might be wrong then.
          WarpRectangle.Left := ReadBigEndianDouble(Run);
          WarpRectangle.Top := ReadBigEndianDouble(Run);
          WarpRectangle.Right := ReadBigEndianDouble(Run);
          WarpRectangle.Bottom := ReadBigEndianDouble(Run);
        end;
      end;
  end;
  Run := Temp;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSDGraphic.ReadChannelData(var Run: PChar; var Channel: TPSDChannel; Width, Height: Integer;
  IsIrrelevant: Boolean);

// Reads and optionally decompresses image data for one channel.

var
  Y: Integer;
  Count: Integer;
  RLELength: array of Word;
  Compression: TCompressionType;
  Target: PByte;
  RemainingSize: Integer;

begin
  RemainingSize := Channel.Size;
  if RemainingSize > 0 then
  begin
    Compression := ConvertCompression(ReadBigEndianWord(Run));
    Dec(RemainingSize, 2);
    if (RemainingSize > 0) and not IsIrrelevant then
    begin
      // Allocate temporary storage for the channel data. This memory is freed in CombineChannels.
      // A channel is always 8 bit per pixel.
      GetMem(Channel.Data, Width * Height);

      case Compression of
        ctNone: // Simple case, just move the data to our storage.
          Move(Run^, Channel.Data^, RemainingSize);
        ctPackedBits:
          begin
            Decoder := TPackbitsRLEDecoder.Create;
            try
              SetLength(RLELength, Height);
              Count := 2 * Height;
              Move(Run^, Pointer(RLELength)^, Count); // RLE lengths are word values.
              SwapShort(Pointer(RLELength), Height);
              Dec(RemainingSize, Count);
              // Advance the running pointer to after the RLE lenghts.
              Inc(Run, Count);

              Target := Channel.Data;
              for Y := 0 to Height - 1 do
              begin
                Decoder.Decode(Pointer(Run), Pointer(Target), RLELength[Y], Width);
                Inc(Run, RLELength[Y]);
                Inc(Target, Width);
                Dec(RemainingSize, RLELength[Y]);
                if RemainingSize <= 0 then
                  Break;
              end;
            finally
              FreeAndNil(Decoder);
            end;
          end;
      else
        FreeMem(Channel.Data);
        GraphicExError(gesUnsupportedFeature, [gesCompressionScheme, 'PSD/PDD']);
      end;
    end;
    Inc(Run, RemainingSize);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSDGraphic.ReadDescriptor(var Run: PChar; var Descriptor: TPSDDescriptor);

const
  // Identifiers used in the descriptor structures.
  KeyCount = 20;
  OSTypeKey: array[0..KeyCount - 1] of PChar = (
    'obj ', // Reference
    'Objc', // Descriptor
    'VlLs', // List
    'doub', // Double
    'UntF', // Unit float
    'TEXT', // String
    'Enmr', // Enumerated
    'long', // Integer
    'bool', // Boolean
    'GlbO', // GlobalObject same as Descriptor
    'Clss', // Class
    'GlbC', // Gobal class
    'alis', // Alias

    // Additional keys for reference type.
    'prop', // Property
    'rele', // Offset
    'Idnt', // Identifier
    'indx', // Index
    'name', // Name

    // Other keys not mentioned in the docs.
    'enum', // enumeration
    'tdta'  // raw data
  );

  //--------------- local functions -------------------------------------------

  function ReadUnicodeID: WideString;

  // Reads an ID which is either a Unicode string or a 4 byte ANSI string.

  var
    I: Cardinal;

  begin
    I := ReadBigEndianCardinal(Run);
    if I = 0 then
    begin
      SetString(Result, Run, 4);
      Inc(Run, 4);
    end
    else
      Result := ReadBigEndianString(Run, I);
  end;

  //---------------------------------------------------------------------------

  function ReadANSIID: string;

  // Reads an ID which is an ANSI string.

  var
    I: Cardinal;

  begin
    I := ReadBigEndianCardinal(Run);
    if I = 0 then
      I := 4;
    SetString(Result, Run, I);
    Inc(Run, I);
  end;

  //---------------------------------------------------------------------------

  function ReadOSTypeKey: Cardinal;

  begin
    Result := 0;
    while (Result < KeyCount) and (StrLComp(Run, OSTypeKey[Result], 4) <> 0) do
      Inc(Result);
    Inc(Run, 4);
  end;

  //---------------------------------------------------------------------------

  procedure ReadItem(var Data: TPSDItemData);

  // Reads a single descriptor item. Since an item might be a list which again contains
  // descriptor items this code might be called recursively.
  // Note: There are a few types whose structure are not documented. They are loaded by
  //       guessing what could be there.

  var
    RefItemCount: Cardinal;
    I: Integer;

  begin
    with Data do
    begin
      case ItemType of
        0, 2: // Reference or List
          begin
            RefItemCount := ReadBigEndianCardinal(Run);
            New(List);
            SetLength(List^, RefItemCount);
            for I := 0 to RefItemCount - 1 do
            begin
              List^[I].ItemType := ReadOSTypeKey;
              // Recurse down and read a new PSD data item.
              ReadItem(List^[I]);
            end;
          end;
        1, 9: // Descriptor or global object
          begin
            New(Descriptor);
            ReadDescriptor(Run, Descriptor^);
          end;
        3: // Double
          Value := ReadBigEndianDouble(Run);
        4: // Unit float
          begin
            Units := ReadBigEndianCardinal(Run);
            Value := ReadBigEndianDouble(Run);
          end;
        5: // String
          KeyID := ReadBigEndianString(Run);
        6: // Enumerated
          begin
            ClassID := ReadUnicodeID;
            KeyID := ReadANSIID;
            TypeID := ReadANSIID;
            EnumValue := ReadANSIID;
          end;
        7: // Integer (undocumented)
          IntValue := Integer(ReadBigEndianCardinal(Run));
        8: // Boolean
          begin
            BoolValue := Run^ <> #0;
            Inc(Run);
          end;
        10, 11: // Class or global class (undocumented)
          begin
            ClassID := ReadUnicodeID;
            // Skip ANSI form of the string.
            ReadANSIID;
          end;
        12: // Alias
          begin
            // Alias data is OS specific. It could contain Macintosh FSSpec data, which cannot be
            // handled on Windows.
            // On Windows it might contain a handle to a string of the full path, whatever that means.
            // There is no further information available, so we skip that data.
            I := ReadBigEndianCardinal(Run);
            Inc(Run, I);
          end;
        13: // Property
          begin
            ClassID := ReadUnicodeID;
            // Skip ANSI form of the string.
            ReadANSIID;
            KeyID := ReadANSIID;
          end;
        14: // Offset
          begin
            ClassID := ReadUnicodeID;
            // Skip ANSI form of the string.
            ReadANSIID;
            Offset := ReadBigEndianCardinal(Run);
          end;
        15, 17: // Identifier (undocumented) or Name (undocumented)
          Name := ReadBigEndianString(Run);
        16: // Index (undocumented)
          IntValue := Integer(ReadBigEndianCardinal(Run));
        18: // 'enum' (undocumented)
          begin
            TypeID := ReadANSIID;
            EnumValue := ReadANSIID;
          end;
        19: // 'tdta' (undocumented)
          begin
            DataSize := ReadBigEndianCardinal(Run);
            GetMem(Data, DataSize);
            Move(Run^, Data^, DataSize);
            Inc(Run, DataSize);
          end;
      else
        // There is no error recovery in the file for the case the item key is unknown.
        // So we cannot skip it and have to throw an exception here to stop processing and to avoid
        // an exception showing up in a totally wrong place.
        GraphicExError(gesInvalidPSDLayerData);
      end;
    end;
  end;
  
  //--------------- end local functions ---------------------------------------

var
  I: Cardinal;
  ItemCount: Cardinal;

begin
  with Descriptor do
  begin
    // Class ID as Unicode string.
    ClassID := ReadBigEndianString(Run);
    ClassID2 := ReadANSIID;

    // Now read the items in the descriptor.
    ItemCount := ReadBigEndianCardinal(Run);
    SetLength(Descriptor.Items, ItemCount);
    for I := 0 to ItemCount - 1 do
    begin
      Descriptor.Items[I].Key := ReadANSIID;
      Descriptor.Items[I].Data.ItemType := ReadOSTypeKey;
      ReadItem(Descriptor.Items[I].Data);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSDGraphic.ReadMergedImage(var Source: PChar; Layer: TPhotoshopLayer; Compression: TCompressionType;
  Channels: Byte);

// Reads the image data of the composite image (if Layer = nil) or the given layer.

var
  Count: Cardinal;
  RLELength: array of Word;

  Y: Integer;
  BPS: Integer;         // bytes per sample either 1 or 2 for 8 bits per channel and 16 bits per channel respectively
  ChannelSize: Integer; // size of one channel (taking BPS into account)
  Increment: Integer;   // pointer increment from one line to next

  // RLE buffers
  Line,
  Buffer: Pointer;     // all iamge data uncompressed
  Run1,                // running pointer in Buffer 1
  Run2,                // etc.
  Run3,
  Run4: PByte;

  W, H: Integer;       // Width and height of the layer or composite image.

begin
  if Layer = nil then
  begin
    W := Width;
    H := Height;
  end
  else
  begin
    W := Layer.Image.Width;
    H := Layer.Image.Height;
  end;

  // Reading the merged image takes up the rest of the entire loading process.
  StartProgressSection(0, gesTransfering);
  
  Decoder := nil;
  case Compression of
    ctNone: ;
    ctPackedBits:
      begin
        Decoder := TPackbitsRLEDecoder.Create;
        Count := H * Channels;
        // We have to swap the byte order but must not modify the data pointed to by Run (might be a file mapping).
        // Hence we have to make a copy of the RLE lengths.
        SetLength(RLELength, Count);
        Move(Source^, Pointer(RLELength)^, 2 * Count);
        SwapShort(Pointer(RLELength), Count);
        // Advance the running pointer to after the RLE lenghts.
        Inc(Source, 2 * Count);
      end;
  else
    GraphicExError(gesUnsupportedFeature, [gesCompressionScheme, 'PSD/PDD']);
  end;

  try
    case FImageProperties.ColorScheme of
      csG,
      csIndexed:
        begin
          // Very simple format here, we don't need the color conversion manager.
          if Assigned(Decoder) then
          begin
            for Y := 0 to H - 1 do
            begin
              Count := RLELength[Y];
              Line := ScanLine[Y];
              Decoder.Decode(Pointer(Source), Line, Count, W);
              Inc(Source, Count);

              AdvanceProgress(100 / H, 0, 1, True);
            end;
          end
          else // uncompressed data
            for Y := 0 to H - 1 do
            begin
              Move(Source^, ScanLine[Y]^, W);
              Inc(Source, W);

              AdvanceProgress(100 / H, 0, 1, True);
            end;
        end;
      csRGB,
      csRGBA,
      csCMYK,
      csCIELab:
        begin
          // Data is organized in planes. This means first all red rows, then
          // all green and finally all blue rows.
          BPS := FImageProperties.BitsPerSample div 8;
          ChannelSize := BPS * W * H;

          GetMem(Buffer, Channels * ChannelSize);
          try
            // first run: load image data and decompress it if necessary
            if Assigned(Decoder) then
            begin
              // determine whole compressed size
              Count := 0;
              for Y := 0 to High(RLELength) do
                Inc(Count, RLELength[Y]);
              Decoder.Decode(Pointer(Source), Buffer, Count * Cardinal(BPS), Channels * ChannelSize);
              Inc(Source, Count * Cardinal(BPS));
            end
            else
            begin
              Move(Source^, Buffer^, Channels * ChannelSize);
              Inc(Source, Channels * ChannelSize);
            end;

            Increment := BPS * W;
            // second run: put data into image (convert color space if necessary)
            case FImageProperties.ColorScheme of
              csRGB:
                begin
                  Run1 := Buffer;
                  Run2 := Run1; Inc(Run2, ChannelSize);
                  Run3 := Run2; Inc(Run3, ChannelSize);
                  for Y := 0 to H - 1 do
                  begin
                    ColorManager.ConvertRow([Run1, Run2, Run3], ScanLine[Y], W, $FF);
                    Inc(Run1, Increment);
                    Inc(Run2, Increment);
                    Inc(Run3, Increment);

                    AdvanceProgress(100 / H, 0, 1, True);
                  end;
                end;
              csRGBA:
                begin
                  Run1 := Buffer;
                  Run2 := Run1; Inc(Run2, ChannelSize);
                  Run3 := Run2; Inc(Run3, ChannelSize);
                  Run4 := Run3; Inc(Run4, ChannelSize);
                  for Y := 0 to H - 1 do
                  begin
                    ColorManager.ConvertRow([Run1, Run2, Run3, Run4], ScanLine[Y], W, $FF);
                    Inc(Run1, Increment);
                    Inc(Run2, Increment);
                    Inc(Run3, Increment);
                    Inc(Run4, Increment);

                    AdvanceProgress(100 / H, 0, 1, True);
                  end;
                end;
              csCMYK:
                begin
                  // Photoshop CMYK values are given with 0 for maximum values, but the
                  // (general) CMYK conversion works with 255 as maxium value. Hence we must reverse
                  // all entries in the buffer.
                  Run1 := Buffer;
                  for Y := 1 to 4 * ChannelSize do
                  begin
                    Run1^ := 255 - Run1^;
                    Inc(Run1);
                  end;

                  Run1 := Buffer;
                  Run2 := Run1; Inc(Run2, ChannelSize);
                  Run3 := Run2; Inc(Run3, ChannelSize);
                  Run4 := Run3; Inc(Run4, ChannelSize);
                  for Y := 0 to H - 1 do
                  begin
                    ColorManager.ConvertRow([Run1, Run2, Run3, Run4], ScanLine[Y], W, $FF);
                    Inc(Run1, Increment);
                    Inc(Run2, Increment);
                    Inc(Run3, Increment);
                    Inc(Run4, Increment);

                    AdvanceProgress(100 / H, 0, 1, True);
                  end;
                end;
              csCIELab:
                begin
                  Run1 := Buffer;
                  Run2 := Run1; Inc(Run2, ChannelSize);
                  Run3 := Run2; Inc(Run3, ChannelSize);
                  for Y := 0 to H - 1 do
                  begin
                    ColorManager.ConvertRow([Run1, Run2, Run3], ScanLine[Y], W, $FF);
                    Inc(Run1, Increment);
                    Inc(Run2, Increment);
                    Inc(Run3, Increment);

                    AdvanceProgress(100 / H, 0, 1, True);
                  end;
                end;
            end;
          finally
            if Assigned(Buffer) then
              FreeMem(Buffer);
          end;
        end;
    end;
    FinishProgressSection(False);
  finally
    FreeAndNil(Decoder);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSDGraphic.ReadLayers(Run: PChar);

// Recreates the layer structure given in the file. Run points to the layer section size.

  //--------------- local functions -------------------------------------------

  procedure ReadBlendRanges(var Data: TPSDCompositeGrayBlend);

  begin
    with Data do
    begin
      Black1 := Byte(Run^);
      Inc(Run);
      Black2 := Byte(Run^);
      Inc(Run);
      White1 := Byte(Run^);
      Inc(Run);
      White2 := Byte(Run^);
      Inc(Run);
    end;
  end;

  //--------------- end local functions ---------------------------------------

var
  LayerCount: SmallInt;
  Layer: TPhotoshopLayer;
  I, LayerIndex: Integer;
  R: TRect;
  Channels: Integer;
  BlendMode: TPSDLayerBlendMode;
  Dummy: Byte;
  BlockSize: Cardinal;
  S: string;
  BlockStart: PChar;

begin
  // Skip the layer section size. We are going to read the full section.
  Inc(Run, SizeOf(Cardinal));

  LayerCount := Swap(PSmallInt(Run)^);
  // If LayerCount is < 0 then it means the first alpha channel contains the transparency data for the
  // composite image (the merged result). I'm not sure what to do with that info.
  LayerCount := Abs(LayerCount);
  Inc(Run, SizeOf(SmallInt));

  // Once we know how many layers to load we can setup a progress section for them.
  // The size is the layer count relative to itself plus one image (the composite/merged image) less 1% we
  // already spent for preparation.
  if LayerCount > 0 then
  begin
    StartProgressSection(100 * LayerCount / (LayerCount + 1) - 1, gesLoadingData);

    // Now retrieve the actual layers.
    LayerIndex := 0;

    // Start an own progress section for loading the layer info and assign it 5% (difficult to say in advance
    // how much it really is, since this depends on layer sizes and amount of info).
    StartProgressSection(5, gesLoadingData);

    while LayerIndex < LayerCount do
    begin
      Layer := FLayers.AddNewLayer;

      // bounds rectangle
      R.Top := Integer(ReadBigEndianCardinal(Run));
      R.Left := Integer(ReadBigEndianCardinal(Run));
      R.Bottom := Integer(ReadBigEndianCardinal(Run));
      R.Right := Integer(ReadBigEndianCardinal(Run));
      Layer.Bounds := R;
      Channels := ReadBigEndianWord(Run);

      // Keep the channel data for later pixel data retrieval.
      SetLength(Layer.FChannels, Channels);
      for I := 0 to Channels - 1 do
      begin
        with Layer.Channels[I] do
        begin
          ChannelID := Swap(PSmallInt(Run)^);
          Inc(Run, SizeOf(Word));
          Size := ReadBigEndianCardinal(Run);
        end;
      end;

      // Next comes the blend mode signature which is always '8BIM'. We can use this for error checking.
      if StrLIComp(Run, '8BIM', 4) <> 0 then
        GraphicExError(gesInvalidPSDLayerData);
      Inc(Run, 4);
      // Determine the blend mode from the four character ID.
      for BlendMode := Low(TPSDLayerBlendMode) to High(TPSDLayerBlendMode) do
        if StrLIComp(Run, PSDBlendModeMapping[BlendMode], 4) = 0 then
        begin
          Layer.BlendMode := BlendMode;
          Break;
        end;
      Inc(Run, 4);
      Layer.Opacity := Byte(Run^);
      Inc(Run);
      if Byte(Run^) = 0 then
        Layer.Clipping := lcBase
      else
        Layer.Clipping := lcNonBase;
      Inc(Run);
      Dummy := Byte(Run^);
      Layer.Options := TPSDLayerOptions(Dummy and 3);
      if Dummy and $18 = $18 then
        Layer.Options := Layer.Options + [loIrrelevantData];
      // There is a filler byte after the flags/options.
      Inc(Run, 2);

      // Skip extra data size value.
      Inc(Run, SizeOf(Cardinal));

      // Read out mask data.
      // The size is either 36, 20, or 0.
      BlockSize := ReadBigEndianCardinal(Run);
      if BlockSize > 0 then
      begin
        with Layer.MaskData do
        begin
          with Bounds do
          begin
            Top := ReadBigEndianInteger(Run);
            Left := ReadBigEndianInteger(Run);
            Bottom := ReadBigEndianInteger(Run);
            Right := ReadBigEndianInteger(Run);
          end;
          DefaultColor := Byte(Run^);
          Inc(Run);
          Flags := TPSDLayerMaskFlags(Run^);
          Inc(Run);
          // If only 20 bytes mask data is present then we are finished here with it (except 2 padding bytes).
          // Otherwise read additional data.
          if BlockSize = 20 then
            Inc(Run, 2)
          else
          begin
            // Skip "real flags" field, which is just a duplication of the flags.
            Inc(Run);
            UserMaskBackground := Byte(Run^);
            // Advance after the mask background value and skip the copy of the enclosing rectangle too, which follows.
            Inc(Run, 1 + 4 * SizeOf(Cardinal));
          end;
        end;
      end;

      // Next are the layer blending ranges. In opposition to the docs the size seems not to depend on the number of
      // channels in the layer. It is always 10 range entries large, even for gray scale images.
      BlockSize := ReadBigEndianCardinal(Run);
      if BlockSize > 0 then
      begin
        BlockStart := Run;
        // Take the first two entries for gray blending.
        ReadBlendRanges(Layer.FCompositeGrayBlendSource);
        ReadBlendRanges(Layer.FCompositeGrayBlendDestination);
        // Read as many entries as there are channels, but not more than 8.
        for I := 0 to Min(High(Layer.Channels), 4) do
        begin
          ReadBlendRanges(Layer.Channels[I].BlendSourceRange);
          ReadBlendRanges(Layer.Channels[I].BlendTargetRange);
        end;
        // Skip whatever left over.
        Run := BlockStart + BlockSize;
      end;
      
      // Read the pascal style (ANSI) layer name. This might get overwritten by the Unicode name.
      I := Byte(Run^);
      SetString(S, Run + 1, I);
      Layer.Name := S;
      // The name is padded to a 4 byte boundary.
      Inc(Run, (I + 4) and not 3);

      // From Photoshop version 4 on there might be additional data here. This data is organized in blocks
      // all starting with '8BIM' as tag and is referred to as "adjustment layers" (e.g. Unicode name, effects etc.).
      while StrLIComp(Run, '8BIM', 4) = 0 do
      begin
        Inc(Run, 4);
        LoadAdjustmentLayer(Run, Layer);
      end;

      // Advance to next layer.
      Inc(LayerIndex);
    end;
    // Finish progress for layer info retrieval.
    FinishProgressSection(False);

    // Start progress section for the rest of the entire layer loading process.
    StartProgressSection(0, gesLoadingData);
    // Here we reached the image data. This block contains all channel data for all layers.
    for LayerIndex := 0 to FLayers.Count - 1 do
    begin
      StartProgressSection(100 / LayerCount, gesLoadingData);

      Layer := FLayers[LayerIndex];
      // Each channel might have an individual compression scheme.
      // If the layer contains irrelevant data then tell it the reader method so it skips the data accordingly.
      for I := 0 to High(Layer.FChannels) do
        with Layer.Bounds do
          ReadChannelData(Run, Layer.FChannels[I], Right - Left, Bottom - Top, loIrrelevantData in Layer.Options);

      if not (loIrrelevantData in Layer.Options) then
      begin
        // Extra layer channels always follow the actual image data so we can limit the maximum
        // number of channels to use to 4 without harm.
        Layer.Image.PixelFormat := SetupColorManager(Min(4, Length(Layer.FChannels)));
        with Layer, Bounds do
        begin
          Image.Width := Right - Left;
          Image.Height := Bottom - Top;
          Image.Palette := CopyPalette(Palette);
        end;
        CombineChannels(Layer);
      end;

      FinishProgressSection(True);
    end;
    // Finish the decompression and combining process.
    FinishProgressSection(False);
    // Finish the layer loading progress. The global layer mask info does not produce progress events.
    FinishProgressSection(False);
  end;

  // The last step after all layers have been read is to read the global layer mask info.
  BlockSize := ReadBigEndianCardinal(Run);
  if BlockSize > 0 then
  begin
    BlockStart := Run;

    FLayers.FOverlayColorSpace := ReadBigEndianWord(Run);
    for I := 0 to 3 do
      FLayers.FColorComponents[I] := ReadBigEndianWord(Run);
    FLayers.FLayerMaskOpacity := ReadBigEndianWord(Run);
    FLayers.FKind := Byte(Run^);

    Run := BlockStart + BlockSize;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSDGraphic.ReadResources(Run: PChar);

var
  ID: Word;
  I: Cardinal;
  Name: string;
  Size: Cardinal;

begin
  while StrLIComp(Run, '8BIM', 4) = 0 do
  begin
    // Skip signature.
    Inc(Run, 4);
    // Resource ID.
    ID := ReadBigEndianWord(Run);
    // Resource name (pascal short string style).
    I := Byte(Run^);
    Inc(Run);
    SetString(Name, Run, I);
    Inc(Run, I);
    Inc(Run, Integer(Run) and 1); // Padded to even size.

    // Resource size.
    Size := ReadBigEndianCardinal(Run);
    case ID of
      GridAndGuides:
        with FGridSettings do
        begin
          // Skip version number (= 1 for Photoshop 4.0).
          Inc(Run, 4);
          // Numbers here are in 16.16 fix point format.
          HorizontalCycle := ReadBigEndianCardinal(Run) / 32;
          VerticalCycle := ReadBigEndianCardinal(Run) / 32;
          // Number of guides.
          Size := ReadBigEndianCardinal(Run);
          if Size > 0 then
          begin
            SetLength(Guides, Size);
            for I := 0 to Size - 1 do
            begin
              Guides[I].Location := ReadBigEndianCardinal(Run) / 32;
              Guides[I].IsHorizontal := Boolean(Run^);
              Inc(Run);
            end;
          end;
        end;
    else
      // Simply skip any unknown entries.
      Inc(Run, Size);
    end;
    Inc(Run, Integer(Run) and 1); // Padded to even size.
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPSDGraphic.SetupColorManager(Channels: Integer): TPixelFormat;

// Determines source and target color schemes and initializes the color manager options for the
// following load of image data.
// There are only a couple of scenarios which are allowed in layers in a PSD file:
//   - only grayscale, RGB, CMYK and Lab
//   - only 8 bits per sample.
// Returns the necessary pixel format.

var
  CurrentColorScheme: TColorScheme;
  
begin
  with FImageProperties, ColorManager do
  begin
    SourceOptions := [coNeedByteSwap];
    SourceBitsPerSample := BitsPerSample;
    if BitsPerSample = 16 then
      TargetBitsPerSample := 8
    else
      TargetBitsPerSample := BitsPerSample;

    SourceSamplesPerPixel := Channels;
    TargetSamplesPerPixel := Channels;

    CurrentColorScheme := DetermineColorScheme(Channels);
    case CurrentColorScheme of
      csG,
      csGA,
      csIndexed:
        begin
          SourceColorScheme := CurrentColorScheme;
          TargetColorScheme := CurrentColorScheme;
          // For the time being only one channel can be handled.
          // An eventual alpha channel is ignored.
          SourceSamplesPerPixel := 1;
          TargetSamplesPerPixel := 1;
        end;
      csRGB:
        if Channels = 4 then
        begin
          SourceColorScheme := csRGBA;
          TargetColorScheme := csBGRA;
        end
        else
        begin
          SourceColorScheme := CurrentColorScheme;
          TargetColorScheme := csBGR;
        end;
      csRGBA:
        begin
          SourceColorScheme := CurrentColorScheme;
          TargetColorScheme := csBGRA;
        end;
      csCMYK:
        begin
          SourceColorScheme := CurrentColorScheme;
          TargetColorScheme := csBGR;
          TargetSamplesPerPixel := 3;
        end;
      csCIELab:
        begin
          SourceColorScheme := CurrentColorScheme;
          // PSD uses 0..255 for a and b so we need to convert them to -128..127
          SourceOptions := SourceOptions + [coLabByteRange, coLabChromaOffset];
          if Channels = 4 then
            TargetColorScheme := csBGRA
          else
            TargetColorScheme := csBGR;
        end;
    end;
    Result := TargetPixelFormat;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TPSDGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := Size > SizeOf(TPSDHeader);
  if Result then
    with PPSDHeader(Memory)^ do
      Result := (StrLIComp(Signature, '8BPS', 4) = 0) and (Swap(Version) = 1);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSDGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  Run: PChar;           // Pointer to the current position in the given memory.
  Count: Cardinal;

begin
  inherited;

  FLayers.Clear;
  Run := Memory;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    with FImageProperties do
    begin
      // Initialize outermost progress display.
      InitProgress(Width, 1);
      StartProgressSection(0, '');

      // Initialize sub section for image preparation. We give it a (guessed) value of 1%.
      StartProgressSection(1, gesPreparing);

      // Skip the header, image info is already read.
      Inc(Run, SizeOf(TPSDHeader));

      PixelFormat := SetupColorManager(SamplesPerPixel);
      Self.Width := Width;
      Self.Height := Height;

      // Read color mode data if present.
      // Size of palette.
      Count := ReadBigEndianCardinal(Run);
      // Setup the palette if necessary.
      case ColorScheme of
        csG,
        csGA:
          Palette := ColorManager.CreateGrayscalePalette(ioMinIsWhite in Options);
        csIndexed:
          Palette := ColorManager.CreateColorPalette([Run, Run + Count div 3, Run + 2 * Count div 3], pfPlane8Triple,
            Count, False);
      end;
      Inc(Run, Count);

      // The preparation part is finished. Finish also progress section (which will step the main progress).
      FinishProgressSection(False);

      // Read resource section.
      Count := ReadBigEndianCardinal(Run);
      if Count > 0 then        
        ReadResources(Run);
      Inc(Run, Count);

      // Read layers section.
      Count := ReadBigEndianCardinal(Run);
      if Count > 0 then
        ReadLayers(Run);

      // Use +2 in order to skip the following compression value (which we already know).
      Inc(Run, Count + 2);
      // Setup the color manager again. It might be changed by the layer loading stuff.
      if FLayers.Count > 0 then
        SetupColorManager(SamplesPerPixel);
      ReadMergedImage(Run, nil, Compression, FChannels);

      FinishProgressSection(False);
    end;
  end
  else
    GraphicExError(gesInvalidImage, ['PSD or PDD']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPSDGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Run: PByte;
  Header: TPSDHeader;
  Count: Cardinal;

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
    with FImageProperties do
    begin
      Run := Memory;
      Inc(Run, SizeOf(TPSDHeader));

      Move(Memory^, Header, SizeOf(TPSDHeader));
      if Header.Signature = '8BPS' then
      begin
        with Header do
        begin
          // PSD files are big endian only.
          Channels := Swap(Channels);
          Rows := SwapLong(Rows);
          Columns := SwapLong(Columns);
          Depth := Swap(Depth);
          Mode := Swap(Mode);
        end;

        Options := [ioBigEndian];
        // Initialize color manager.
        BitsPerSample := Header.Depth;
        FChannels := Header.Channels;
        // 1..24 channels are supported in PSD files, we can only use 4.
        // The documentation states that main image data (rgb(a), cmyk etc.) is always
        // written with the first channels in their component order.
        if FChannels > 4 then
          SamplesPerPixel := 4
        else
          SamplesPerPixel := FChannels;

        BitsPerPixel := SamplesPerPixel * BitsPerSample;

        // color space
        FMode := Header.Mode;
        ColorScheme := DetermineColorScheme(SamplesPerPixel);
        if FMode = PSD_BITMAP then
          Include(Options, ioMinIsWhite);

        Width := Header.Columns;
        Height := Header.Rows;

        // Read the size of the palette.
        Count := ReadBigEndianCardinal(PChar(Run));
        // Skip palette (count is always given, might be 0 however, e.g. for RGB).
        Inc(Run, Count);

        // Skip resource and layers section.
        Count := ReadBigEndianCardinal(PChar(Run));
        Inc(Run, Count);
        Count := ReadBigEndianCardinal(PChar(Run));
        Inc(Run, Count);

        Compression := ConvertCompression(ReadBigEndianWord(PChar(Run)));
        Result := True;
      end
      else
        Result := False;                                        
    end;
end;

{$endif PhotoshopGraphic}

//----------------- TPSPGraphic ----------------------------------------------------------------------------------------

{$ifdef PaintshopProGraphic}

const
  // block identifiers
  PSP_IMAGE_BLOCK = 0;                      // General Image Attributes Block (main)
  PSP_CREATOR_BLOCK = 1;                    // Creator Data Block (main)
  PSP_COLOR_BLOCK = 2;                      // Color Palette Block (main and sub)
  PSP_LAYER_START_BLOCK = 3;                // Layer Bank Block (main)
    PSP_LAYER_BLOCK = 4;                    // Layer Block (sub)
    PSP_CHANNEL_BLOCK = 5;                  // Channel Block (sub)
  PSP_SELECTION_BLOCK = 6;                  // Selection Block (main)
  PSP_ALPHA_BANK_BLOCK = 7;                 // Alpha Bank Block (main)
    PSP_ALPHA_CHANNEL_BLOCK = 8;            // Alpha Channel Block (sub)
  PSP_THUMBNAIL_BLOCK = 9;                  // Thumbnail Block (main)
  PSP_EXTENDED_DATA_BLOCK = 10;             // Extended Data Block (main)
  PSP_TUBE_BLOCK = 11;                      // Picture Tube Data Block (main)
    PSP_ADJUSTMENT_EXTENSION_BLOCK = 12;    // Adjustment Layer Extension Block (sub)
    PSP_VECTOR_EXTENSION_BLOCK = 13;        // Vector Layer Extension Block (sub)
    PSP_SHAPE_BLOCK = 14;                   // Vector Shape Block (sub)
    PSP_PAINTSTYLE_BLOCK = 15;              // Paint Style Block (sub)
  PSP_COMPOSITE_IMAGE_BANK_BLOCK = 16;      // Composite Image Bank (main)
    PSP_COMPOSITE_ATTRIBUTES_BLOCK = 17;    // Composite Image Attributes (sub)
    PSP_JPEG_BLOCK = 18;                    // JPEG Image Block (sub)

  // bitmap types
	PSP_DIB_IMAGE = 0;            // Layer color bitmap
	PSP_DIB_TRANS_MASK = 1;       // Layer transparency mask bitmap
	PSP_DIB_USER_MASK = 2;        // Layer user mask bitmap
	PSP_DIB_SELECTION= 3;         // Selection mask bitmap
	PSP_DIB_ALPHA_MASK = 4;       // Alpha channel mask bitmap
	PSP_DIB_THUMBNAIL = 5;        // Thumbnail bitmap
  PSP_DIB_THUMBNAIL_TRANS_MASK = 6; // Thumbnail transparency mask
  PSP_DIB_ADJUSTMENT_LAYER = 7; // Adjustment layer bitmap
  PSP_DIB_COMPOSITE = 8;        // Composite image bitmap
  PSP_DIB_COMPOSITE_TRANS_MASK = 9; // Composite image transparency

  // composite image type
  PSP_IMAGE_COMPOSITE = 0;      // Composite Image
  PSP_IMAGE_THUMBNAIL = 1;      // Thumbnail Image

  // graphic contents flags
  PSP_GC_RASTERLAYERS = 1;      // At least one raster layer
  PSP_GC_VectorLayers = 2;      // At least one vector layer
  PSP_GC_ADJUSTMENTLAYERS = 4;  // At least one adjustment layer
  // Additional attributes
  PSP_GC_THUMBNAIL = $01000000;              // Has a thumbnail
  PSP_GC_THUMBNAILTRANSPARENCY = $02000000;  // Thumbnail transp.
  PSP_GC_COMPOSITE = $04000000;              // Has a composite image
  PSP_GC_COMPOSITETRANSPARENCY = $08000000;  // Composite transp.
  PSP_GC_FLATIMAGE = $10000000;              // Just a background
  PSP_GC_SELECTION = $20000000;              // Has a selection
  PSP_GC_FLOATINGSELECTIONLAYER = $40000000; // Has float. selection
  PSP_GC_ALPHACHANNELS = $80000000;          // Has alpha channel(s)

  // character style flags
  PSP_STYLE_ITALIC = 1;         // Italic property bit
  PSP_STYLE_STRUCK = 2;         // Strike-out property bit
  PSP_STYLE_UNDERLINED = 4;     // Underlined property bit

  // layer flags
	PSP_LAYER_VISIBLEFLAG = 1;    // Layer is visible
	PSP_LAYER_MASKPRESENCEFLAG = 2; // Layer has a mask

  // Shape property flags
  PSP_SHAPE_ANTIALIASED = 1;    // Shape is anti-aliased
  PSP_SHAPE_Selected = 2;       // Shape is selected
  PSP_SHAPE_Visible = 4;        // Shape is visible

  // Polyline node type flags
  PSP_NODE_UNCONSTRAINED = 0;   // Default node type
  PSP_NODE_SMOOTH = 1;          // Node is smooth
  PSP_NODE_SYMMETRIC = 2;       // Node is symmetric
  PSP_NODE_ALIGNED = 4;         // Node is aligned
  PSP_NODE_ACTIVE = 8;          // Node is active
  PSP_NODE_LOCKED = 16;         // Node is locked (PSP doc says 0x16 here, but this seems to be a typo)
  PSP_NODE_SELECTED = 32;       // Node is selected (PSP doc says 0x32 here)
  PSP_NODE_VISIBLE = 64;        // Node is visible (PSP doc says 0x64 here)
  PSP_NODE_CLOSED = 128;        // Node is closed (PSP doc says 0x128 here)

  // Blend modes
	LAYER_BLEND_NORMAL = 0;
  LAYER_BLEND_DARKEN = 1;
  LAYER_BLEND_LIGHTEN = 2;
  LAYER_BLEND_HUE = 3;
  LAYER_BLEND_SATURATION = 4;
  LAYER_BLEND_COLOR = 5;
  LAYER_BLEND_LUMINOSITY = 6;
  LAYER_BLEND_MULTIPLY = 7;
  LAYER_BLEND_SCREEN = 8;
  LAYER_BLEND_DISSOLVE = 9;
  LAYER_BLEND_OVERLAY = 10;
  LAYER_BLEND_HARD_LIGHT = 11;
  LAYER_BLEND_SOFT_LIGHT = 12;
  LAYER_BLEND_DIFFERENCE = 13;
  LAYER_BLEND_DODGE = 14;
  LAYER_BLEND_BURN = 15;
  LAYER_BLEND_EXCLUSION = 16;
  LAYER_BLEND_ADJUST = 255;

  // Adjustment layer types
  PSP_ADJUSTMENT_NONE = 0;      // Undefined adjustment layer type
  PSP_ADJUSTMENT_LEVEL = 1;     // Level adjustment
  PSP_ADJUSTMENT_CURVE = 2;     // Curve adjustment
  PSP_ADJUSTMENT_BRIGHTCONTRAST = 3; // Brightness-contrast adjustment
  PSP_ADJUSTMENT_COLORBAL = 4;  // Color balance adjustment
  PSP_ADJUSTMENT_HSL = 5;       // HSL adjustment
  PSP_ADJUSTMENT_CHANNELMIXER = 6; // Channel mixer adjustment
  PSP_ADJUSTMENT_INVERT = 7;    // Invert adjustment
  PSP_ADJUSTMENT_THRESHOLD = 8; // Threshold adjustment
  PSP_ADJUSTMENT_POSTER = 9;    // Posterize adjustment

  // Vector shape types
  PSP_VST_Unknown = 0;          // Undefined vector type
  PSP_VST_TEXT = 1;             // Shape represents lines of text
  PSP_VST_POLYLINE = 2;         // Shape represents a multiple segment line
  PSP_VST_ELLIPSE = 3;          // Shape represents an ellipse (or circle)
  PSP_VST_POLYGON = 4;          // Shape represents a closed polygon

  // Text element types
  PSP_TET_UNKNOWN = 0;          // Undefined text element type
  PSP_TET_CHAR = 1;             // A single character code
  PSP_TET_CHARSTYLE = 2;        // A character style change
  PSP_TET_LINESTYLE = 3;        // A line style change

  // Text alignment types
  PSP_TAT_LEFT = 0;             // Left text alignment
  PSP_TAT_CENTER = 1;           // Center text alignment
  PSP_TAT_RIGHT = 2;            // Right text alignment

  // Paint style types
  PSP_STYLE_NONE = 0;           // Undefined paint style
  PSP_STYLE_COLOR = 1;          // Paint using color (RGB or palette index)
  PSP_STYLE_GRADIENT = 2;       // Paint using gradient

  // Channel types
	PSP_CHANNEL_COMPOSITE = 0;    // Channel of single channel bitmap
	PSP_CHANNEL_RED = 1;          // Red channel of 24 bit bitmap
	PSP_CHANNEL_GREEN = 2;        // Green channel of 24 bit bitmap
	PSP_CHANNEL_BLUE = 3;         // Blue channel of 24 bit bitmap

  // Resolution metrics
  PSP_METRIC_UNDEFINED = 0;	    // Metric unknown
  PSP_METRIC_INCH = 1;          // Resolution is in inches
  PSP_METRIC_CM = 2;            // Resolution is in centimeters

  // Compression types
	PSP_COMP_NONE = 0;            // No compression
	PSP_COMP_RLE = 1;             // RLE compression
	PSP_COMP_LZ77 = 2;            // LZ77 compression
  PSP_COMP_JPEG = 3;            // JPEG compression (only used by thumbnail and composite image)

  // Picture tube placement mode
	PSP_TPM_Random = 0;           // Place tube images in random intervals
	PSPS_TPM_Constant = 1;        // Place tube images in constant intervals

  // Tube selection mode
	PSP_TSM_RANDOM =0;            // Randomly select the next image in tube to display
	PSP_TSM_INCREMENTAL = 1;     // Select each tube image in turn
	PSP_TSM_ANGULAR = 2;          // Select image based on cursor direction
	PSP_TSM_PRESSURE = 3;         // Select image based on pressure (from pressure-sensitive pad)
	PSP_TSM_VELOCITY = 4;         // Select image based on cursor speed

  // Extended data field types
  PSP_XDATA_TRNS_INDEX = 0;     // Transparency index field

  // Creator field types
	PSP_CRTR_FLD_TITLE = 0;       // Image document title field
	PSP_CRTR_FLD_CRT_DATE = 1;    // Creation date field
	PSP_CRTR_FLD_MOD_DATE = 2;    // Modification date field
	PSP_CRTR_FLD_ARTIST = 3;      // Artist name field
	PSP_CRTR_FLD_CPYRGHT = 4;     // Copyright holder name field
	PSP_CRTR_FLD_DESC = 5;        // Image document description field
	PSP_CRTR_FLD_APP_ID = 6;      // Creating app id field
	PSP_CRTR_FLD_APP_VER = 7;     // Creating app version field

  // Creator application identifier
	PSP_CREATOR_APP_UNKNOWN = 0;  // Creator application unknown
	PSP_CREATOR_APP_PAINT_SHOP_PRO = 1; // Creator is Paint Shop Pro

  // Layer types (file version 3)
  PSP_LAYER_NORMAL = 0;         // Normal layer
  PSP_LAYER_FLOATING_SELECTION = 1; // Floating selection layer

  // Layer types (file version 4)
  PSP_LAYER_UNDEFINED = 0;      // Undefined layer type
  PSP_LAYER_RASTER = 1;         // Standard raster layer
  PSP_LAYER_FLOATINGRASTERSELECTION = 2; // Floating selection (raster layer)
  PSP_LAYER_Vector = 3;         // Vector layer
  PSP_LAYER_ADJUSTMENT = 4;     // Adjustment layer

  MagicID = 'Paint Shop Pro Image File';

type
  // These block header structures are here for informational purposes only because the data of those
  // headers is read member by member to generalize code for the different file versions
  TPSPBlockHeader3 = packed record          // block header file version 3
    HeaderIdentifier: array[0..3] of Char;  // i.e. "~BK" followed by a zero byte
    BlockIdentifier: Word;                  // one of the block identifiers
    InitialChunkLength,                     // length of the first sub chunk header or similar
    TotalBlockLength: Cardinal;             // length of this block excluding this header
  end;

  TPSPBlockHeader4 = packed record          // block header file version 4
    HeaderIdentifier: array[0..3] of Char;  // i.e. "~BK" followed by a zero byte
    BlockIdentifier: Word;                  // one of the block identifiers
    TotalBlockLength: Cardinal;             // length of this block excluding this header
  end;

  TPSPColorPaletteInfoChunk = packed record
    EntryCount: Cardinal;                   // number of entries in the palette
  end;

  TPSPColorPaletteChunk = array[0..255] of TRGBQuad; // might actually be shorter 

  TPSPChannelInfoChunk = packed record
    CompressedSize,
    UncompressedSize: Cardinal;
    BitmapType,                             // one of the bitmap types
    ChannelType: Word;                      // one of the channel types
  end;

  // PSP defines a channel content chunk which is just a bunch of bytes (size is CompressedSize).
  // There is no sense to define this record type here.

  PPSPFileHeader = ^TPSPFileHeader;
  TPSPFileHeader = packed record
    Signature: array[0..31] of Char;        // the string "Paint Shop Pro Image File\n\x1a", padded with zeroes
    MajorVersion,
    MinorVersion: Word;                
  end;

  TPSPImageAttributes = packed record
    Width,
    Height: Integer;
    Resolution: Double;                     // Number of pixels per metric
    ResolutionMetric: Byte;                 // Metric used for resolution (one of the metric constants)
    Compression,                            // compression type of image (not thumbnail, it has its own compression)
    BitDepth,                               // The bit depth of the color bitmap in each Layer of the image document
                                            // (must be 1, 4, 8 or 24).
    PlaneCount: Word;                       // Number of planes in each layer of the image document (usually 1)
    ColorCount: Cardinal;                   // number of colors in each layer (2^bit depth)
    GreyscaleFlag: Boolean;                 // Indicates whether the color bitmap in each layer of image document is a
                                            // greyscale (False = not greyscale, True = greyscale).
    TotalImageSize: Cardinal;               // Sum of the sizes of all layer color bitmaps.
    ActiveLayer: Integer;                   // Identifies the layer that was active when the image document was saved.
    LayerCount: Word;                       // Number of layers in the document.
    GraphicContents: Cardinal;              // A series of flags that helps define the image's graphic contents.
  end;

  TPSPLayerInfoChunk = packed record
    //LayerName: array[0..255] of Char;     // Name of layer (in ASCII text). Has been replaced in version 4
                                            // by a Delphi like short string (length word and variable length string)
    LayerType: Byte;                        // Type of layer.
    ImageRectangle,                         // Rectangle defining image border.
    SavedImageRectangle: TRect;             // Rectangle within image rectangle that contains "significant" data
                                            // (only the contents of this rectangle are saved to the file).
    LayerOpacity: Byte;                     // Overall layer opacity.
    BlendingMode: Byte;                     // Mode to use when blending layer.
    Visible: Boolean;                       // TRUE if layer was visible at time of save, FALSE otherwise.
    TransparencyProtected: Boolean;         // TRUE if transparency is protected.
    LinkGroupIdentifier: Byte;              // Identifies group to which this layer belongs.
    MaskRectangle,                          // Rectangle defining user mask border.
    SavedMaskRectangle: TRect;              // Rectangle within mask rectangle that contains "significant" data
                                            // (only the contents of this rectangle are saved to the file).
    MaskLinked: Boolean;                    // TRUE if mask linked to layer (i.e., mask moves relative to layer)
    MaskDisabled: Boolean;                  // TRUE if mask is disabled, FALSE otherwise.
    InvertMask: Boolean;                    // TRUE if mask should be inverted when the layer is merged, FALSE otherwise.
    BlendRangeCount: Word;                  // Number of valid source-destination field pairs to follow (note, there are
                                            // currently always 5 such pairs, but they are not necessarily all valid).
    SourceBlendRange1,                      // First source blend range value.
    DestinationBlendRange1,                 // First destination blend range value.
    SourceBlendRange2,
    DestinationBlendRange2,
    SourceBlendRange3,
    DestinationBlendRange3,
    SourceBlendRange4,
    DestinationBlendRange4,
    SourceBlendRange5,
    DestinationBlendRange5: array[0..3] of Byte;
    // These fields are obsolete since file version 4, because there's an own chunk for them.
    // BitmapCount: Word;                      // Number of bitmaps to follow.
    // ChannelCount: Word;                     // Number of channels to follow.
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TPSPGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  with PPSPFileHeader(Memory)^ do
    Result := (Size > SizeOf(TPSPFileHeader)) and (StrLIComp(Signature, MagicID, Length(MagicID)) = 0) and
      (MajorVersion >= 3);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSPGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  Header: TPSPFileHeader;
  Image: TPSPImageAttributes;
  // To use the code below for file 3 and 4 I read the parts of the block header
  // separately instead as a structure.
  HeaderIdentifier: array[0..3] of Char;  // i.e. "~BK" followed by a zero byte
  BlockIdentifier: Word;                  // one of the block identifiers
  InitialChunkLength,                     // length of the first sub chunk header or similar
  TotalBlockLength: Cardinal;             // length of this block excluding this header

  ChunkSize: Cardinal;
  LayerInfo: TPSPLayerInfoChunk;
  ChannelInfo: TPSPChannelInfoChunk;
  LayerName: string;
  NameLength: Word;

  // file version 4 specific data
  BitmapCount,
  ChannelCount: Word;

  // load and decoding of image data
  R, G, B, C: PByte;
  RedBuffer,
  GreenBuffer,
  BlueBuffer,
  CompBuffer: Pointer;
  X, Y,
  Index,
  RowSize: Integer; // size in bytes of one scanline 

  // other data
  RawPalette: array[0..4 * 256 - 1] of Byte;

  LastPosition,
  NextMainBlock,
  NextLayerPosition: PChar; // PChar, because then direct pointer arithmethic is accepted.
  Run: PByte;

  //--------------- local functions -------------------------------------------

  function ReadBlockHeader: Boolean;

  // Fills in the block header variables according to the file version.
  // Returns True if a block header could be read otherwise False (stream end).

  begin
    Result := (PChar(Run) - PChar(Memory)) < Size;
    if Result then
    begin
      Move(Run^, HeaderIdentifier, SizeOf(HeaderIdentifier));
      Inc(Run, SizeOf(HeaderIdentifier));

      Move(Run^, BlockIdentifier, SizeOf(BlockIdentifier));
      Inc(Run, SizeOf(BlockIdentifier));

      if Header.MajorVersion = 3 then
      begin
        Move(Run^, InitialChunkLength, SizeOf(InitialChunkLength));
        Inc(Run, SizeOf(InitialChunkLength));
      end;
      Move(Run^, TotalBlockLength, SizeOf(TotalBlockLength));
      Inc(Run, SizeOf(TotalBlockLength));
    end;
  end;

  //---------------------------------------------------------------------------

  procedure ReadAndDecompress(Target: Pointer);

  // reads a stream of data from file stream and decompresses it into Target

  var
    Source: Pointer;

  begin
    Decoder := nil;
    try
      Source := Run;
      case Image.Compression of
        PSP_COMP_RLE:
          begin
            Decoder := TPSPRLEDecoder.Create;
            Decoder.Decode(Source, Target, ChannelInfo.CompressedSize, ChannelInfo.UncompressedSize);
          end;
        PSP_COMP_LZ77:
          begin
            Decoder := TLZ77Decoder.Create(Z_FINISH, False);
            Decoder.DecodeInit;
            Decoder.Decode(Source, Target, ChannelInfo.CompressedSize, ChannelInfo.UncompressedSize);
          end;
        PSP_COMP_JPEG: // here just for completeness, used only in thumbnails and composite images
          ;
      end;
      Inc(Run, ChannelInfo.CompressedSize);
      Decoder.DecodeEnd;
    finally
      FreeAndNil(Decoder);
    end;
  end;

  //---------------------------------------------------------------------------

  procedure ReadChannelData;

  // Reads the actual data of one channel from the current stream position.
  // Decompression is done by the way.

  begin
    ReadBlockHeader;
    if Header.MajorVersion > 3 then
    begin
      Move(Run^, ChunkSize, SizeOf(ChunkSize));
      Inc(Run, SizeOf(ChunkSize));
    end;
    Move(Run^, ChannelInfo, SizeOf(ChannelInfo));
    Inc(Run, SizeOf(ChannelInfo));

    case ChannelInfo.ChannelType of
      PSP_CHANNEL_COMPOSITE: // Single channel bitmap (indexed or transparency mask).
        begin
          // Damaged files can have more than one composite channel. Make sure we do not
          // allocate the buffer more than once without freeing it.
          // Do not use Realloc here as it copies the memory block.
          if Assigned(CompBuffer) then
            FreeMem(CompBuffer);
          GetMem(CompBuffer, ChannelInfo.UncompressedSize);
          if Image.Compression <> PSP_COMP_NONE then
            ReadAndDecompress(CompBuffer)
          else
          begin
            Move(Run^, CompBuffer^, ChannelInfo.CompressedSize);
            Inc(Run, ChannelInfo.CompressedSize);
          end;
        end;
      PSP_CHANNEL_RED:  // Red channel of 24 bit bitmap.
        begin
          GetMem(RedBuffer, ChannelInfo.UncompressedSize);
          if Image.Compression <> PSP_COMP_NONE then
            ReadAndDecompress(RedBuffer)
          else
          begin
            Move(Run^, RedBuffer^, ChannelInfo.CompressedSize);
            Inc(Run, ChannelInfo.CompressedSize);
          end;
        end;
      PSP_CHANNEL_GREEN:
        begin
          GetMem(GreenBuffer, ChannelInfo.UncompressedSize);
          if Image.Compression <> PSP_COMP_NONE then
            ReadAndDecompress(GreenBuffer)
          else
          begin
            Move(Run^, GreenBuffer^, ChannelInfo.CompressedSize);
            Inc(Run, ChannelInfo.CompressedSize);
          end;
        end;
      PSP_CHANNEL_BLUE:
        begin
          GetMem(BlueBuffer, ChannelInfo.UncompressedSize);
          if Image.Compression <> PSP_COMP_NONE then
            ReadAndDecompress(BlueBuffer)
          else
          begin
            Move(Run^, BlueBuffer^, ChannelInfo.CompressedSize);
            Inc(Run, ChannelInfo.CompressedSize);
          end;
        end;
    end;
  end;

  //--------------- end local functions ---------------------------------------

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    Run := Memory;
    RedBuffer := nil;
    GreenBuffer := nil;
    BlueBuffer := nil;
    CompBuffer := nil;
    with FImageProperties do
    try
      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      Move(Run^, Header, SizeOf(Header));
      Inc(Run, SizeOf(Header));

      // Read general image attribute block.
      ReadBlockHeader;
      LastPosition := PChar(Run);
      if Version > 3 then
      begin
        Move(Run^, ChunkSize, SizeOf(ChunkSize));
        Inc(Run, SizeOf(ChunkSize));
      end;
      Move(Run^, Image, SizeOf(Image));
      Run := Pointer(LastPosition + TotalBlockLength);

      with ColorManager, Image do
      begin
        SourceOptions := [];
        SourceBitsPerSample := BitsPerSample;
        TargetBitsPerSample := BitsPerSample;
        SourceSamplesPerPixel := SamplesPerPixel;
        TargetSamplesPerPixel := SamplesPerPixel;
        SourceColorScheme := ColorScheme;
        if ColorScheme = csRGB then
          TargetColorScheme := csBGR
        else
          TargetColorScheme := ColorScheme;

        PixelFormat := TargetPixelFormat;
      end;

      // set bitmap properties
      RowSize := 0; // make compiler quiet
      case BitsPerSample of
        1:
          RowSize := (Image.Width + 7) div 8;
        4:
          RowSize := Image.Width div 2 + 1;
        8:
          RowSize := Image.Width;
      else
        GraphicExError(gesInvalidColorFormat, ['PSP']);
      end;

      Self.Width := Width;
      Self.Height := Height;
      Progress(Self, psEnding, 0, False, FProgressRect, '');

      // go through main blocks and read what is needed
      repeat
        if not ReadBlockHeader then
          Break;
        NextMainBlock := Pointer(PChar(Run) + TotalBlockLength);
        // no more blocks?
        if HeaderIdentifier[0] <> '~' then
          Break;

        case BlockIdentifier of
          {PSP_COMPOSITE_IMAGE_BANK_BLOCK:
            begin
              // composite image block, if present then it must appear before the layer start block
              // and represents a composition of several layers

              // do not need to read anything further
              Break;
            end;}
          PSP_LAYER_START_BLOCK:
            repeat
              if not ReadBlockHeader then
                Break;

              Progress(Self, psStarting, 0, False, FProgressRect, gesLoadingData);

              // calculate start of next (layer) block in case we need to skip this one
              NextLayerPosition := Pointer(PChar(Run) + TotalBlockLength);
              // if all layers have been considered the break loop to continue with other blocks if necessary
              if BlockIdentifier <> PSP_LAYER_BLOCK then
                Break;

              // layer information chunk
              if Version > 3 then
              begin
                LastPosition := PChar(Run);
                Move(Run^, ChunkSize, SizeOf(ChunkSize));
                Inc(Run, SizeOf(ChunkSize));

                Move(Run^, NameLength, SizeOf(NameLength));
                Inc(Run, SizeOf(NameLength));
                SetLength(LayerName, NameLength);
                if NameLength > 0 then
                begin
                  Move(Run^, LayerName[1], NameLength);
                  Inc(Run, NameLength);
                end;
                Move(Run^, LayerInfo, SizeOf(LayerInfo));
                Inc(Run, SizeOf(LayerInfo));
                Run := Pointer(LastPosition + ChunkSize);

                // continue only with undefined or raster chunks
                if not (LayerInfo.LayerType in [PSP_LAYER_UNDEFINED, PSP_LAYER_RASTER]) then
                begin
                  Run := Pointer(NextLayerPosition);
                  Continue;
                end;

                // in file version 4 there's also an additional bitmap chunk which replaces
                // two fields formerly located in the LayerInfo chunk
                LastPosition := PChar(Run);
                Move(Run^, ChunkSize, SizeOf(ChunkSize));
                Inc(Run, SizeOf(ChunkSize));
              end
              else
              begin
                SetLength(LayerName, 256);
                Move(Run^, LayerName[1], 256);
                Inc(Run, 256);

                Move(Run^, LayerInfo, SizeOf(LayerInfo));
                Inc(Run, SizeOf(LayerInfo));

                // continue only with normal (raster) chunks
                if LayerInfo.LayerType <> PSP_LAYER_NORMAL then
                begin
                  Run := Pointer(NextLayerPosition);
                  Continue;
                end;
              end;

              Move(Run^, BitmapCount, SizeOf(BitmapCount));
              Inc(Run, SizeOf(BitmapCount));

              Move(Run^, ChannelCount, SizeOf(ChannelCount));
              Inc(Run, SizeOf(ChannelCount));

              // But now we can reliably say whether we have an alpha channel or not.
              // This kind of information can only be read very late and causes us to
              // possibly reallocate the entire image (because it is copied by the VCL
              // when changing the pixel format).
              // I don't know another way (preferably before the size of the image is set).
              if ChannelCount > 3 then
              begin
                ColorManager.TargetColorScheme := csBGRA;
                PixelFormat := pf32Bit;
              end;

              if Version > 3 then
                Run := Pointer(LastPosition + ChunkSize);

              // allocate memory for all channels and read raw data
              for X := 0 to ChannelCount - 1 do
                ReadChannelData;
              Progress(Self, psEnding, 0, False, FProgressRect, '');

              Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
              R := RedBuffer;
              G := GreenBuffer;
              B := BlueBuffer;
              C := CompBuffer;
              with ColorManager do
              begin
                if TargetColorScheme in [csIndexed, csG] then
                begin
                  for Y := 0 to Height - 1 do
                  begin
                    ColorManager.ConvertRow([C], ScanLine[Y], Width, $FF);
                    Inc(C, RowSize);

                    Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                    OffsetRect(FProgressRect, 0, 1);
                  end;
                end
                else
                begin
                  for Y := 0 to Height - 1 do
                  begin
                    ColorManager.ConvertRow([R, G, B, C], ScanLine[Y], Width, $FF);
                    Inc(R, RowSize);
                    Inc(G, RowSize);
                    Inc(B, RowSize);
                    Inc(C, RowSize);

                    Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                    OffsetRect(FProgressRect, 0, 1);
                  end;
                end;
              end;
              Progress(Self, psEnding, 0, False, FProgressRect, '');
              // after the raster layer has been read there's no need to loop further
              Break;
            until False; // layer loop
          PSP_COLOR_BLOCK:  // color palette block (this is also present for gray scale and b&w images)
            begin
              if Version > 3 then
              begin
                Move(Run^, ChunkSize, SizeOf(ChunkSize));
                Inc(Run, SizeOf(ChunkSize));
              end;
              Move(Run^, Index, SizeOf(Index));
              Inc(Run, SizeOf(Index));

              Move(Run^, RawPalette, Index * SizeOf(TRGBQuad));
              Inc(Run, Index * SizeOf(TRGBQuad));
              Palette := ColorManager.CreateColorPalette([@RawPalette], pfInterlaced8Quad, Index, True);
            end;
        end;

        // explicitly set stream position to next main block as we might have read a block only partially
        Run := Pointer(NextMainBlock);
      until False; // main block loop
    finally
      if Assigned(RedBuffer) then
        FreeMem(RedBuffer);
      if Assigned(GreenBuffer) then
        FreeMem(GreenBuffer);
      if Assigned(BlueBuffer) then
        FreeMem(BlueBuffer);
      if Assigned(CompBuffer) then
        FreeMem(CompBuffer);
    end;
  end
  else
    GraphicExError(gesInvalidImage, ['PSP']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPSPGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Header: TPSPFileHeader;
  Image: TPSPImageAttributes;
  // to use the code below for file 3 and 4 I read the parts of the block header
  // separately instead as a structure
  HeaderIdentifier: array[0..3] of Char;  // i.e. "~BK" followed by a zero byte
  BlockIdentifier: Word;                  // one of the block identifiers
  InitialChunkLength,                     // length of the first sub chunk header or similar
  TotalBlockLength: Cardinal;             // length of this block excluding this header

  ChunkSize: Cardinal;

  LastPosition,
  Run: PByte;
  
  //--------------- local functions -------------------------------------------

  function ReadBlockHeader: Boolean;

  // Fills in the block header variables according to the file version.
  // Returns True if a block header could be read otherwise False (stream end).

  begin
    Result := (PChar(Run) - PChar(Memory)) < Size;
    if Result then
    begin
      Move(Run^, HeaderIdentifier, SizeOf(HeaderIdentifier));
      Inc(Run, SizeOf(HeaderIdentifier));

      Move(Run^, BlockIdentifier, SizeOf(BlockIdentifier));
      Inc(Run, SizeOf(BlockIdentifier));

      if Header.MajorVersion = 3 then
      begin
        Move(Run^, InitialChunkLength, SizeOf(InitialChunkLength));
        Inc(Run, SizeOf(InitialChunkLength));
      end;
      Move(Run^, TotalBlockLength, SizeOf(TotalBlockLength));
      Inc(Run, SizeOf(TotalBlockLength));
    end;
  end;

  //--------------- end local functions ---------------------------------------

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
    with FImageProperties do
    begin
      Run := Memory;
      Move(Run^, Header, SizeOf(Header));
      Inc(Run, SizeOf(Header));

      if (StrLIComp(Header.Signature, MagicID, Length(MagicID)) = 0) and
         (Header.MajorVersion >= 3) then
      begin
        Version := Header.MajorVersion;

        // read general image attribute block
        ReadBlockHeader;
        LastPosition := Run;
        if Header.MajorVersion > 3 then
        begin
          Move(Run^, ChunkSize, SizeOf(ChunkSize));
          Inc(Run, SizeOf(ChunkSize));
        end;
        Move(Run^, Image, SizeOf(Image));
        Run := Pointer(PChar(LastPosition) + TotalBlockLength);

        if Image.BitDepth = 24 then
        begin
          BitsPerSample := 8;
          SamplesPerPixel := 3;
          ColorScheme := csRGB; // an alpha channel might exist, this is determined by the layer's channel count 
        end
        else
        begin
          BitsPerSample := Image.BitDepth;
          SamplesPerPixel := 1;
          if Image.GreyscaleFlag then
            ColorScheme := csG
          else
            ColorScheme := csIndexed;
        end;
        BitsPerPixel := BitsPerSample * SamplesPerPixel;

        Width := Image.Width;
        Height := Image.Height;

        case Image.Compression of
          PSP_COMP_NONE:
            Compression := ctNone;
          PSP_COMP_RLE:
            Compression := ctRLE;
          PSP_COMP_LZ77:
            Compression := ctLZ77;
          PSP_COMP_JPEG:
            Compression := ctJPEG;
        else
          Compression := ctUnknown;
        end;
        XResolution := Image.Resolution;
        if Image.ResolutionMetric = PSP_METRIC_CM then
          XResolution := XResolution * 2.54;
        YResolution := XResolution;
        Result := True;
      end
      else
        Result := False;
    end;
end;

{$endif PaintshopProGraphic}

//----------------- TPNGGraphic ----------------------------------------------------------------------------------------

{$ifdef PortableNetworkGraphic}

const
  PNGMagic: PChar = #137'PNG'#13#10#26#10;

  // Recognized and handled chunk types.
  IHDR = 'IHDR';
  IDAT = 'IDAT';
  IEND = 'IEND';
  PLTE = 'PLTE';
  gAMA = 'gAMA';
  tRNS = 'tRNS';
  bKGD = 'bKGD';
  tEXt = 'tEXt';

  CHUNKMASK = $20; // used to check bit 5 in chunk types

type
  // The following chunks structures are those which appear in the data field of the general chunk structure
  // given above.

  // chunk type: 'IHDR'
  PIHDRChunk = ^TIHDRChunk;
  TIHDRChunk = packed record
    Width,
    Height: Cardinal;
    BitDepth,          // bits per sample (allowed are 1, 2, 4, 8 and 16)
    ColorType,         // combination of:
                       //   1 - palette used
                       //   2 - colors used
                       //   4 - alpha channel used
                       // allowed values are:
                       //   0 - gray scale (allowed bit depths are: 1, 2, 4, 8, 16)
                       //   2 - RGB (8, 16)
                       //   3 - palette (1, 2, 4, 8)
                       //   4 - gray scale with alpha (8, 16)
                       //   6 - RGB with alpha (8, 16)
    Compression,       // 0 - LZ77, others are not yet defined
    Filter,            // filter mode 0 is the only one currently defined
    Interlaced: Byte;  // 0 - not interlaced, 1 - Adam7 interlaced
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TPNGGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := (Size > SizeOf(PNGMagic) + SizeOf(TIHDRChunk)) and (StrLIComp(PChar(Memory), PNGMagic, 8) = 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPNGGraphic.IsChunk(ChunkType: TChunkType): Boolean;

// determines, independant of the cruxial 5ths bits in each "letter", whether the
// current chunk type in the header is the same as the given chunk type

const
  Mask = not $20202020;

begin
  Result := (Cardinal(FHeader.ChunkType) and Mask) = (Cardinal(ChunkType) and Mask);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPNGGraphic.LoadAndSwapHeader(var Source: PByte): Cardinal;

// read next chunk header and swap fields to little endian,
// returns the intial CRC value for following checks

begin
  Move(Source^, FHeader, SizeOf(FHeader));
  Inc(Source, SizeOf(FHeader));

  Result := CRC32(0, @FHeader.ChunkType, 4);
  FHeader.Length := SwapLong(FHeader.Length);
end;

//----------------------------------------------------------------------------------------------------------------------

function PaethPredictor(a, b, c: Byte): Byte;

var
  p, pa, pb, pc: Integer;

begin
  // a = left, b = above, c = upper left
  p := a + b - c;        // initial estimate
  pa := Abs(p - a);      // distances to a, b, c
  pb := Abs(p - b);
  pc := Abs(p - c);
  // return nearest of a, b, c, breaking ties in order a, b, c
  if (pa <= pb) and (pa <= pc) then
    Result := a
  else
    if pb <= pc then
      Result := b
    else
      Result := c;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.ApplyFilter(Filter: Byte; Line, PrevLine, Target: PByte; BPP, BytesPerRow: Integer);

// Applies the filter given in Filter to all bytes in Line (potentially using PrevLine).
// Note: The filter type is assumed to be of filter mode 0, as this is the only one currently
//       defined in PNG.
//       In opposition to the PNG documentation different identifiers are used here.
//       Raw refers to the current, not yet decoded value. Decoded refers to the current, already
//       decoded value (this one is called "raw" in the docs) and Prior is the current value in the
//       previous line. For the Paeth prediction scheme a fourth pointer is used (PriorDecoded) to describe
//       the value in the previous line but less the BPP value (Prior[x - BPP]).      

var
  I: Integer;
  Raw,
  Decoded,
  Prior,
  PriorDecoded,
  TargetRun: PByte;

begin
  case Filter of
    0: // no filter, just copy data
      Move(Line^, Target^, BytesPerRow);
    1: // subtraction filter
      begin
        Raw := Line;
        TargetRun := Target;
        // Transfer BPP bytes without filtering. This mimics the effect of bytes left to the
        // scanline being zero.
        Move(Raw^, TargetRun^, BPP);

        // now do rest of the line
        Decoded := TargetRun;
        Inc(Raw, BPP);
        Inc(TargetRun, BPP);
        Dec(BytesPerRow, BPP);
        while BytesPerRow > 0 do
        begin
          TargetRun^ := Byte(Raw^ + Decoded^);
          Inc(Raw);
          Inc(Decoded);
          Inc(TargetRun);
          Dec(BytesPerRow);
        end;
      end;
    2: // Up filter
      begin
        Raw := Line;
        Prior := PrevLine;
        TargetRun := Target;
        while BytesPerRow > 0 do
        begin
          TargetRun^ := Byte(Raw^ + Prior^);
          Inc(Raw);
          Inc(Prior);
          Inc(TargetRun);
          Dec(BytesPerRow);
        end;
      end;
    3: // average filter
      begin
        // first handle BPP virtual pixels to the left
        Raw := Line;
        Decoded := Line;
        Prior := PrevLine;
        TargetRun := Target;
        for I := 0 to BPP - 1 do
        begin
          TargetRun^ := Byte(Raw^ + Floor(Prior^ / 2));
          Inc(Raw);
          Inc(Prior);
          Inc(TargetRun);
        end;
        Dec(BytesPerRow, BPP);

        // now do rest of line
        while BytesPerRow > 0 do
        begin
          TargetRun^ := Byte(Raw^ + Floor((Decoded^ + Prior^) / 2));
          Inc(Raw);
          Inc(Decoded);
          Inc(Prior);
          Inc(TargetRun);
          Dec(BytesPerRow);
        end;
      end;
   4: // paeth prediction
     begin
       // again, start with first BPP pixel which would refer to non-existing pixels to the left
       Raw := Line;
       Decoded := Target;
       Prior := PrevLine;
       PriorDecoded := PrevLine;
       TargetRun := Target;
       for I := 0 to BPP - 1 do
       begin
         TargetRun^ := Byte(Raw^ + PaethPredictor(0, Prior^, 0));
         Inc(Raw);
         Inc(Prior);
         Inc(TargetRun);
       end;
       Dec(BytesPerRow, BPP);

       // finally do rest of line
       while BytesPerRow > 0 do
       begin
         TargetRun^ := Byte(Raw^ + PaethPredictor(Decoded^, Prior^, PriorDecoded^));
          Inc(Raw);
          Inc(Decoded);
          Inc(Prior);
          Inc(PriorDecoded);
          Inc(TargetRun);
          Dec(BytesPerRow);
       end;
     end;
   end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  Description: TIHDRChunk;
  Run: PByte;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    with FImageProperties do
    begin
      Run := Pointer(PChar(Memory) + 8); // skip magic

      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      FPalette := 0;
      FTransparency := nil;
      FBackgroundColor := clWhite;
      FTransparentColor := clNone;

      // First chunk must be an IHDR chunk.
      FCurrentCRC := LoadAndSwapHeader(Run);

      FRawBuffer := nil;
      ColorManager.SourceOptions := [coNeedByteSwap];
      try
        // read IHDR chunk
        ReadDataAndCheckCRC(Run);
        Move(FRawBuffer^, Description, SizeOf(Description));
        SwapLong(@Description, 2);

        // currently only one compression type is supported by PNG (LZ77)
        if Compression = ctLZ77 then
        begin
          Decoder := TLZ77Decoder.Create(Z_PARTIAL_FLUSH, False);
          Decoder.DecodeInit;
        end
        else
          GraphicExError(gesUnsupportedFeature, [gesCompressionScheme, 'PNG']);

        // setup is done, now go for the chunks
        repeat
          FCurrentCRC := LoadAndSwapHeader(Run);
          if IsChunk(IDAT) then
          begin
            Progress(Self, psEnding, 0, False, FProgressRect, '');
            LoadIDAT(Run, Description);
            // After reading the image data the next chunk header has already been loaded
            // so continue with code below instead trying to load a new chunk header.
          end
          else
            if IsChunk(PLTE) then
            begin
              // palette chunk
              if (FHeader.Length mod 3) <> 0 then
                GraphicExError(gesInvalidPalette, ['PNG']);
              ReadDataAndCheckCRC(Run);
              // load palette only if the image is indexed colors
              if Description.ColorType = 3 then
              begin
                // first setup pixel format before actually creating a palette
                FSourceBPP := SetupColorDepth(Description.ColorType, Description.BitDepth);
                FPalette := ColorManager.CreateColorPalette([FRawBuffer], pfInterlaced8Triple, FHeader.Length div 3, False);
              end;
              Continue;
            end
            else                             
              if IsChunk(gAMA) then
              begin
                ReadDataAndCheckCRC(Run);
                // The file gamma given here is a scaled cardinal (e.g. 0.45 is expressed as 45000).
                ColorManager.SetGamma(SwapLong(PCardinal(FRawBuffer)^) / 100000);
                ColorManager.TargetOptions := ColorManager.TargetOptions + [coApplyGamma];
                Include(Options, ioUseGamma);
                Continue;
              end
              else
                if IsChunk(bKGD) then
                begin
                  LoadBackgroundColor(Run, Description);
                  Continue;
                end
                else
                  if IsChunk(tRNS) then
                  begin
                    LoadTransparency(Run, Description);
                    Continue;
                  end;

          // Skip unknown or unsupported chunks (+4 because of always present CRC).
          // IEND will be skipped as well, but this chunk is empty, so the stream will correctly
          // end on the first byte after the IEND chunk.
          Inc(Run, FHeader.Length + 4);
          if IsChunk(IEND) then
            Break;

          // Note: According to the specs an unknown, but as critical marked chunk is a fatal error.
          if (Byte(FHeader.ChunkType[0]) and CHUNKMASK) = 0 then
            GraphicExError(gesUnknownCriticalChunk);
        until False;
      finally
        if Assigned(Decoder) then
          Decoder.DecodeEnd;
        if Assigned(FRawBuffer) then
          FreeMem(FRawBuffer);
        Progress(Self, psEnding, 0, False, FProgressRect, '');
      end;
    end;
  end
  else
    GraphicExError(gesInvalidImage, ['PNG']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPNGGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Magic: array[0..7] of Char;
  Description: TIHDRChunk;
  Run: PByte;

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
    with FImageProperties do
    begin
      Run := Memory;
      Move(Run^, Magic, 8);
      Inc(Run, 8);

      if StrLComp(Magic, PNGMagic, Length(Magic)) = 0 then
      begin
        // first chunk must be an IHDR chunk
        FCurrentCRC := LoadAndSwapHeader(Run);
        if IsChunk(IHDR) then
        begin
          Include(Options, ioBigEndian);
          // read IHDR chunk
          ReadDataAndCheckCRC(Run);
          Move(FRawBuffer^, Description, SizeOf(Description));
          SwapLong(@Description, 2);

          if (Description.Width = 0) or (Description.Height = 0) then
            Exit;

          Width := Description.Width;
          Height := Description.Height;

          if Description.Compression = 0 then
            Compression := ctLZ77
          else
            Compression := ctUnknown;

          BitsPerSample := Description.BitDepth;
          SamplesPerPixel := 1;
          case Description.ColorType of
            0:
              ColorScheme := csG;
            2:
              begin
                ColorScheme := csRGB;
                SamplesPerPixel := 3;
              end;
            3:
              ColorScheme := csIndexed;
            4:
              ColorScheme := csGA;
            6:
              begin
                ColorScheme := csRGBA;
                SamplesPerPixel := 4;
              end;
          else
            ColorScheme := csUnknown;
          end;

          BitsPerPixel := SamplesPerPixel * BitsPerSample;
          FilterMode := Description.Filter;
          Interlaced := Description.Interlaced <> 0;
          HasAlpha := ColorScheme in [csGA, csRGBA, csBGRA];

          // Find gamma and comment.
          repeat
            FCurrentCRC := LoadAndSwapHeader(Run);
            if IsChunk(gAMA) then
            begin
              ReadDataAndCheckCRC(Run);
              // The file gamma given here is a scaled cardinal (e.g. 0.45 is expressed as 45000).
              FileGamma := SwapLong(PCardinal(FRawBuffer)^) / 100000;
              Continue;
            end
            else
              if IsChunk(tEXt) then
              begin
                LoadText(Run);
                Continue;
              end;

            Inc(Run, FHeader.Length + 4);
            if IsChunk(IEND) then
              Break;
          until False;

          Freemem(FRawBuffer);
          Result := True;
        end;
      end
      else
        Result := False;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.LoadBackgroundColor(var Source: PByte; const Description);

// loads the data from the current chunk (must be a bKGD chunk) and fills the bitmpap with that color

var
  Run: PWord;
  R, G, B: Byte;

begin
  ReadDataAndCheckCRC(Source);
  with TIHDRChunk(Description) do
  begin
    case ColorType of
      0, 4: // G(A)
        begin
          case BitDepth of
            2:
              FBackgroundColor := MulDiv16(Swap(PWord(FRawBuffer)^), 15, 3);
            16:
              FBackgroundColor := MulDiv16(Swap(PWord(FRawBuffer)^), 255, 65535);
          else // 1, 4, 8 bits gray scale
            FBackgroundColor := Byte(Swap(PWord(FRawBuffer)^));
          end;
        end;
      2, 6:  // RGB(A)
        begin
          Run := FRawBuffer;
          if BitDepth = 16 then
          begin
            R := MulDiv16(Swap(Run^), 255, 65535); Inc(Run);
            G := MulDiv16(Swap(Run^), 255, 65535); Inc(Run);
            B := MulDiv16(Swap(Run^), 255, 65535); 
          end
          else
          begin
            R := Byte(Swap(Run^)); Inc(Run);
            G := Byte(Swap(Run^)); Inc(Run);
            B := Byte(Swap(Run^));
          end;
          FBackgroundColor := RGB(R, G, B);
        end;
    else // indexed color scheme (3)
      FBackgroundColor := PByte(FRawBuffer)^;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.LoadIDAT(var Source: PByte; const Description);

// loads image data from the current position of the stream

const
  // interlace start and offsets
  RowStart: array[0..6] of Integer = (0, 0, 4, 0, 2, 0, 1);
  ColumnStart: array[0..6] of Integer = (0, 4, 0, 2, 0, 1, 0);
  RowIncrement: array[0..6] of Integer = (8, 8, 8, 4, 4, 2, 2);
  ColumnIncrement: array[0..6] of Integer = (8, 8, 4, 4, 2, 2, 1);
  PassMask: array[0..6] of Byte = ($80, $08, $88, $22, $AA, $55, $FF);

var
  Row: Integer;
  TargetBPP: Integer;
  RowBuffer: array[Boolean] of PChar; // I use PChar here instead of simple pointer to ease pointer math below
  EvenRow: Boolean; // distincts between the two rows we need to hold for filtering
  Pass: Integer;
  BytesPerRow,
  InterlaceRowBytes,
  InterlaceWidth: Integer;

begin
  Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
  RowBuffer[False] := nil;
  RowBuffer[True] := nil;
  try
    // adjust pixel format etc. if not yet done
    if PixelFormat = pfDevice then
      FSourceBPP := SetupColorDepth(TIHDRChunk(Description).ColorType, TIHDRChunk(Description).BitDepth);

    if TIHDRChunk(Description).BitDepth = 16 then
      TargetBPP := FSourceBPP div 2
    else
      TargetBPP := FSourceBPP;

    if FPalette <> 0 then
      Palette := FPalette;
    // after setting the pixel format we can set the dimensions too without
    // initiating color conversions
    Width := TIHDRChunk(Description).Width;
    Height := TIHDRChunk(Description).Height;

    // set background and transparency color, these values must be set after the
    // bitmap is actually valid (although, not filled)
    Canvas.Lock;
    try
      Canvas.Brush.Color := FBackgroundColor;
      Canvas.FillRect(Rect(0, 0, Width, Height));
    finally
      Canvas.Unlock;
    end;
    if FTransparentColor <> clNone then
    begin
      TransparentColor := FTransparentColor;
      Transparent := True;
    end;

    // determine maximum number of bytes per row and consider there's one filter byte at the start of each row
    BytesPerRow := TargetBPP * ((Width * TIHDRChunk(Description).BitDepth + 7) div 8) + 1;

    RowBuffer[True] := AllocMem(BytesPerRow);
    RowBuffer[False] := AllocMem(BytesPerRow);

    // there can be more than one IDAT chunk in the file but then they must directly
    // follow each other (handled in ReadRow)
    EvenRow := True;

    // prepare interlaced images
    if TIHDRChunk(Description).Interlaced = 1 then
    begin
      for Pass := 0 to 6 do
      begin
        // prepare next interlace run
        if Width <= ColumnStart[Pass] then
          Continue;
        InterlaceWidth := (Width + ColumnIncrement[Pass] - 1 - ColumnStart[Pass]) div ColumnIncrement[Pass];
        InterlaceRowBytes := TargetBPP * ((InterlaceWidth * TIHDRChunk(Description).BitDepth + 7) div 8) + 1;

        Row := RowStart[Pass];
        while Row < Height do
        begin
          ReadRow(Source, RowBuffer[EvenRow], InterlaceRowBytes);
          ApplyFilter(Byte(RowBuffer[EvenRow]^),
                      Pointer(RowBuffer[EvenRow] + 1),
                      Pointer(RowBuffer[not EvenRow] + 1),
                      Pointer(RowBuffer[EvenRow] + 1),
                      FSourceBPP,
                      InterlaceRowBytes - 1);

          ColorManager.ConvertRow([Pointer(RowBuffer[EvenRow] + 1)], ScanLine[Row], Width, PassMask[Pass]);
          EvenRow := not EvenRow;
          // continue with next row in interlaced order
          Inc(Row, RowIncrement[Pass]);

          if Pass = 6 then
          begin
            // progress event only for last (and most expensive) pass
            Progress(Self, psRunning, MulDiv(Row, 100, Height), True, FProgressRect, '');
            OffsetRect(FProgressRect, 0, 1);
          end;
        end;
      end;
    end
    else
    begin
      for Row := 0 to Height - 1 do
      begin
        ReadRow(Source, RowBuffer[EvenRow], BytesPerRow);
        ApplyFilter(Byte(RowBuffer[EvenRow]^),
                    Pointer(RowBuffer[EvenRow] + 1),
                    Pointer(RowBuffer[not EvenRow] + 1),
                    Pointer(RowBuffer[EvenRow] + 1),
                    FSourceBPP,
                    BytesPerRow - 1);

        ColorManager.ConvertRow([Pointer(RowBuffer[EvenRow] + 1)], ScanLine[Row], Width, $FF);
        EvenRow := not EvenRow;

        Progress(Self, psRunning, MulDiv(Row, 100, Height), True, FProgressRect, '');
        OffsetRect(FProgressRect, 0, 1);
      end;
    end;

    // in order to improve safe failness we read all remaining but not read IDAT chunks here
    while IsChunk(IDAT) do
    begin
      ReadDataAndCheckCRC(Source);;
      FCurrentCRC := LoadAndSwapHeader(Source);
    end;
  finally
    if Assigned(RowBuffer[True]) then
      FreeMem(RowBuffer[True]);
    if Assigned(RowBuffer[False]) then
      FreeMem(RowBuffer[False]);
  end;
  // ending progress event is issued in main method
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.LoadText(var Source: PByte);

var
  Keyword: string;
  Offset: Cardinal;
  Contents: array of Char;
  
begin
  ReadDataAndCheckCRC(Source);
  with FImageProperties do
  begin
    Keyword := PChar(FRawBuffer); // Keyword is zero terminated in file
    if Keyword = 'Comment' then   // Only text chunks with the 'Comment' keyword are loaded
    begin
      Offset := Length(Keyword) + 1;
      SetLength(Contents, FHeader.Length - Offset + 1);
      StrLCopy(PChar(Contents), PChar(FRawBuffer) + Offset, FHeader.Length - Offset);
      Comment := Comment + PChar(Contents);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.LoadTransparency(var Source: PByte; const Description);

// reads the data of the current transparency chunk

var
  Run: PWord;
  R, G, B: Byte;

begin
  ReadDataAndCheckCRC(Source);
  with TIHDRChunk(Description) do
  begin
    case ColorType of
      0: // gray
        begin
          case BitDepth of
            2:
              R := MulDiv16(Swap(PWord(FRawBuffer)^), 15, 3);
            16:
              R := MulDiv16(Swap(PWord(FRawBuffer)^), 255, 65535);
          else // 1, 4, 8 bits gray scale
            R := Byte(Swap(PWord(FRawBuffer)^));
          end;
          FTransparentColor := RGB(R, R, R);
        end;
      2:  // RGB
        begin
          Run := FRawBuffer;
          if BitDepth = 16 then
          begin
            R := MulDiv16(Swap(Run^), 255, 65535); Inc(Run);
            G := MulDiv16(Swap(Run^), 255, 65535); Inc(Run);
            B := MulDiv16(Swap(Run^), 255, 65535); 
          end
          else
          begin
            R := Byte(Swap(Run^)); Inc(Run);
            G := Byte(Swap(Run^)); Inc(Run);
            B := Byte(Swap(Run^));
          end;
          FTransparentColor := RGB(R, G, B);
        end;
      4, 6:
        // Formats with full alpha channel, they shouldn't have a transparent color.
        ; 
    else
      // Indexed color scheme (3), with at most 256 alpha values (for each palette entry).
      SetLength(FTransparency, 255);
      // read the values (at most 256)...
      Move(FRawBuffer^,  FTransparency[0], Min(FHeader.Length, 256));
      // ...and set default values (255, fully opaque) for non-supplied values
      if FHeader.Length < 256 then
        FillChar(FTransparency[FHeader.Length], 256 - FHeader.Length, $FF);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.ReadDataAndCheckCRC(var Source: PByte);

// Allocates memory in FRawBuffer and reads the next Header.Length bytes from Stream.
// Furthermore, the CRC value following the data is read as well and compared with
// the CRC value which is calculated here.

var
  FileCRC: Cardinal;

begin
  ReallocMem(FRawBuffer, FHeader.Length);
  Move(Source^, FRawBuffer^, FHeader.Length);
  Inc(Source, FHeader.Length);

  Move(Source^, FileCRC, SizeOf(FileCRC));
  Inc(Source, SizeOf(FileCRC));
  FileCRC := SwapLong(FileCRC);
  // The type field of a chunk is included in the CRC, this serves as initial value
  // for the calculation here and is determined in LoadAndSwapHeader.
  FCurrentCRC := CRC32(FCurrentCRC, FRawBuffer, FHeader.Length);
  if FCurrentCRC <> FileCRC then
    GraphicExError(gesInvalidCRC, ['PNG']);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.ReadRow(var Source: PByte; RowBuffer: Pointer; BytesPerRow: Integer);

// reads and decodes one scanline

var
  LocalBuffer: Pointer;
  PendingOutput: Integer;

begin
  LocalBuffer := RowBuffer;
  PendingOutput := BytesPerRow;
  repeat
    // read pending chunk data if available input has dropped to zero
    if TLZ77Decoder(Decoder).AvailableInput = 0 then
    begin
      FIDATSize := 0;
      // read all following chunks until enough data is available or there is no further IDAT chunk
      while FIDATSize = 0 do
      begin
        // finish if the current chunk is not an IDAT chunk
        if not IsChunk(IDAT) then
          Exit;

        ReadDataAndCheckCRC(Source);
        FCurrentSource := FRawBuffer;
        FIDATSize := FHeader.Length;

        // prepare next chunk (plus CRC)
        FCurrentCRC := LoadAndSwapHeader(Source);
      end;
    end;

    // this decode call will advance Source and Target accordingly
    Decoder.Decode(FCurrentSource, LocalBuffer, FIDATSize - (Integer(FCurrentSource) - Integer(FRawBuffer)),
      PendingOutput);

    if TLZ77Decoder(Decoder).ZLibResult = Z_STREAM_END then
    begin
       if (TLZ77Decoder(Decoder).AvailableOutput <> 0) or (TLZ77Decoder(Decoder).AvailableInput <> 0) then
         GraphicExError(gesExtraCompressedData, ['PNG']);
      Break;
    end;

    if TLZ77Decoder(Decoder).ZLibResult <> Z_OK then
      GraphicExError(gesCompression, ['PNG']);

    PendingOutput := BytesPerRow - (Integer(LocalBuffer) - Integer(RowBuffer));
  until PendingOutput = 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPNGGraphic.SetupColorDepth(ColorType, BitDepth: Integer): Integer;

begin
  Result := 0;
  // determine color scheme and setup related stuff,
  // Note: The calculated BPP value is always at least 1 even for 1 bits per pixel etc. formats
  //       and used in filter calculation.
  case ColorType of
    0: // gray scale (allowed bit depths are: 1, 2, 4, 8, 16 bits)
      if BitDepth in [1, 2, 4, 8, 16] then
      with ColorManager do
      begin
        SourceColorScheme := csG;
        TargetColorScheme := csG;

        SourceSamplesPerPixel := 1;
        TargetSamplesPerPixel := 1;
        SourceBitsPerSample := BitDepth;
        // 2 bits values are converted to 4 bits values because DIBs don't know the former variant
        case BitDepth of
          2:
            TargetBitsPerSample := 4;
          16:
            TargetBitsPerSample := 8;
        else
          TargetBitsPerSample := BitDepth;
        end;

        PixelFormat := TargetPixelFormat;
        FPalette := CreateGrayscalePalette(False);
        Result := (BitDepth + 7) div 8;
      end
      else
        GraphicExError(gesInvalidColorFormat, ['PNG']);
    2: // RGB
      if BitDepth in [8, 16] then
      with ColorManager do
      begin
        SourceSamplesPerPixel := 3;
        TargetSamplesPerPixel := 3;
        SourceColorScheme := csRGB;
        TargetColorScheme := csBGR;
        SourceBitsPerSample := BitDepth;
        TargetBitsPerSample := 8;
        PixelFormat := pf24Bit;
        Result := BitDepth * 3 div 8;
      end
      else
        GraphicExError(gesInvalidColorFormat, ['PNG']);
    3: // palette
      if BitDepth in [1, 2, 4, 8] then
      with ColorManager do
      begin
        SourceColorScheme := csIndexed;
        TargetColorScheme := csIndexed;
        SourceSamplesPerPixel := 1;
        TargetSamplesPerPixel := 1;
        SourceBitsPerSample := BitDepth;
        // 2 bits values are converted to 4 bits values because DIBs don't know the former variant
        if BitDepth = 2 then
          TargetBitsPerSample := 4
        else
          TargetBitsPerSample := BitDepth;

        PixelFormat := TargetPixelFormat;
        Result := 1;
      end
      else
        GraphicExError(gesInvalidColorFormat, ['PNG']);
    4: // gray scale with alpha,
       // For the moment this format is handled without alpha, but might later be converted
       // to RGBA with gray pixels or use a totally different approach.
      if BitDepth in [8, 16] then
      with ColorManager do
      begin
        SourceSamplesPerPixel := 1;
        TargetSamplesPerPixel := 1;
        SourceBitsPerSample := BitDepth;
        TargetBitsPerSample := 8;
        SourceColorScheme := csGA; 
        TargetColorScheme := csIndexed;
        PixelFormat := pf8Bit;
        FPalette := CreateGrayScalePalette(False);
        Result := 2 * BitDepth div 8;
      end
      else
        GraphicExError(gesInvalidColorFormat, ['PNG']);
    6: // RGB with alpha (8, 16)
      if BitDepth in [8, 16] then
      with ColorManager do
      begin
        SourceSamplesPerPixel := 4;
        TargetSamplesPerPixel := 4;
        SourceColorScheme := csRGBA;
        TargetColorScheme := csBGRA;
        SourceBitsPerSample := BitDepth;
        TargetBitsPerSample := 8;
        PixelFormat := pf32Bit;

        Result := BitDepth * 4 div 8;
      end
      else
        GraphicExError(gesInvalidColorFormat, ['PNG']);
  else
    GraphicExError(gesInvalidColorFormat, ['PNG']);
  end;
end;

{$endif PortableNetworkGraphic}

{$ifdef ArtsAndLettersGraphic}

//----------------- TGEDGraphic ----------------------------------------------------------------------------------------

const
  GEDMagic = 'A&L-' + #0 + 'ARTS & LETTERS';
  GEDEditorVersion40c = 133;
  GEDVersionHeader = $1F;
  GEDDibThumbnail = $51;
  GEDFileDescription = $5F;

class function TGEDGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

var
  Run: PByte;

begin
  Result := (Size > Length(GEDMagic)) and (StrLIComp(PChar(Memory), PChar(GEDMagic), Length(GEDMagic)) = 0);
  if Result then
  begin
    Run := Memory;
    // Seek to the start of the tags and check the version number.
    Inc(Run, GEDVersionHeader);

    Result := Run^ >= GEDEditorVersion40c;
    if Result then
    begin
      Inc(Run);
      // The file description is always first.
      Result := Run^ = GEDFileDescription;
      if Result then
      begin
        // Skip the description tag
        Inc(Run, Run^);

        // Here we should now find a thumbnail tag.
        Result := Run^ = GEDDibThumbnail;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGEDGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  WholeThumbnail: Integer;
  BI: PBitmapInfo;
  TableSize: Integer;
  Bits: Pointer;
  Tag: Word;
  Bytes: Byte;
  Run: PByte;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    Run := Memory;
    
    // Skip Arts & Letters ID string.
    Inc(Run, Length(GEDMagic));
    // Seek to the start of the tags.
    Inc(Run, GEDVersionHeader);

    // Get the version number.
    Move(Run^, Tag, SizeOf(Tag));
    Inc(Run, SizeOf(Tag));

    // The file description is always first.
    Move(Run^, Tag, SizeOf(Tag));
    Inc(Run, SizeOf(Tag));

    // Skip the description tag.
    Bytes := Run^;
    Inc(Run, Bytes + 1);

    // Here we should now find a thumbnail tag.
    Move(Run^, Tag, SizeOf(Tag));
    Inc(Run, SizeOf(Tag));

    Move(Run^, WholeThumbnail, SizeOf(WholeThumbnail));
    Inc(Run, SizeOf(WholeThumbnail));

    // Read basic data of the image.
    BI := Pointer(Run);
    // Allocate bitmap now...
    Width := BI.bmiHeader.biWidth;
    Height := BI.bmiHeader.biHeight;

    // Calculate palette size. The image data directly follows the bitmap info.
    TableSize := (1 shl BI.bmiHeader.biBitCount) * SizeOf(TRGBQuad);
    Bits := PChar(BI) + SizeOf(TBitmapInfoHeader) + TableSize;
    // ... and place them into our bitmap.
    SetDIBitsToDevice(Canvas.Handle, 0, 0, BI.bmiHeader.biWidth, BI.bmiHeader.biHeight, 0, 0, 0,
      BI.bmiHeader.biHeight, Bits, BI^, DIB_RGB_COLORS);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGEDGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Tag: Word;
  ThumbSize: Cardinal;
  Bytes: Byte;
  BI: PBitmapInfoHeader;
  Run: PByte;

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
    with FImageProperties do
    begin
      Run := Memory;
    
      // Skip Arts & Letters ID string.
      Inc(Run, Length(GEDMagic));
      // Seek to the start of the tags.
      Inc(Run, GEDVersionHeader);

      // Get the version number.
      Move(Run^, Tag, SizeOf(Tag));
      Inc(Run, SizeOf(Tag));
      if Tag >= GEDEditorVersion40c then
      begin
        // The file description is always first.
        Move(Run^, Tag, SizeOf(Tag));
        Inc(Run, SizeOf(Tag));
        if Tag = GEDFileDescription then
        begin
          // Skip the description tag.
          Bytes := Run^;
          Inc(Run, Bytes + 1);

          // Here we should now find a thumbnail tag.
          Move(Run^, Tag, SizeOf(Tag));
          Inc(Run, SizeOf(Tag));
          if Tag = GEDDibThumbnail then
          begin
            // skip thumbnail size
            Move(Run^, ThumbSize, SizeOf(ThumbSize));
            Inc(Run, SizeOf(ThumbSize));

            BI := Pointer(Run);

            Options := [];
            Width := BI.biWidth;
            Height := BI.biHeight;
            BitsPerPixel := BI.biBitCount;
            if BitsPerPixel > 8 then
            begin
              BitsPerSample := BitsPerPixel div 8;
              SamplesPerPixel := BitsPerPixel mod 8;
              if SamplesPerPixel = 3 then
                ColorScheme := csBGR
              else
                ColorScheme := csBGRA;
            end
            else
            begin
              BitsPerSample := BitsPerPixel;
              SamplesPerPixel := 1;
              ColorScheme := csIndexed;
            end;

            if BI.biCompression in [BI_RLE8, BI_RLE4] then
              Compression := ctRLE
            else
              Compression := ctNone;

            Result := True;
          end;
        end;
      end
      else
        Result := False;
    end;
end;

{$endif ArtsAndLettersGraphic}

//----------------- TFileFormatList ------------------------------------------------------------------------------------

type
  PClassEntry = ^TClassEntry;
  TClassEntry = record
    GraphicClass: TGraphicClass;
    Description: string;
    Count: Cardinal;
  end;

  PExtensionEntry = ^TExtensionEntry;
  TExtensionEntry = record
    Extension,
    Description: string;
    FormatTypes: TFormatTypes;
    ClassReference: PClassEntry;
  end;

constructor TFileFormatList.Create;

begin
  FClassList := TList.Create;
  FExtensionList := TList.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TFileFormatList.Destroy;

begin
  Clear;
  FClassList.Free;
  FExtensionList.Free;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFileFormatList.Clear;

var
  I: Integer;                         

begin
  for I := 0 to FClassList.Count - 1 do
  begin
    TPicture.UnregisterGraphicClass(PClassEntry(FClassList[I]).GraphicClass);
    Dispose(PClassEntry(FClassList[I])); // need Dispose with type casting to free strings too
  end;
  FClassList.Clear;

  for I := 0 to FExtensionList.Count - 1 do
    Dispose(PExtensionEntry(FExtensionList[I])); 
  FExtensionList.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.FindExtension(const Extension: string): Integer;

// Returns the entry which belongs to the given extension string or -1 if there's nothing in the list for this ext.

var
  I: Integer;

begin
  Result := -1;
  if Extension <> '' then
    for I := 0 to FExtensionList.Count - 1 do
      if CompareText(PExtensionEntry(FExtensionList[I]).Extension, Extension) = 0 then
      begin
        Result := I;
        Break;
      end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.FindGraphicClass(GraphicClass: TGraphicClass): Integer;

// returns the entry index which belongs to the given graphic class or -1

var
  I: Integer;

begin
  Result := -1;
  for I := 0 to FClassList.Count - 1 do
    if PClassEntry(FClassList[I]).GraphicClass = GraphicClass then
    begin
      Result := I;
      Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.GetDescription(Graphic: TGraphicClass): string;

// returns the registered description string for the given class

var
  I: Integer;

begin
  Result := '';
  I := FindGraphicClass(Graphic);
  if I > -1 then
    Result := PClassEntry(FClassList[I]).Description;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFileFormatList.GetExtensionList(List: TStrings);

// returns a list of registered extensions (letters only, no *. part)

var
  I: Integer;
  ExtEntry: PExtensionEntry;

begin
  List.Clear;
  for I := 0 to FExtensionList.Count - 1 do
  begin
    ExtEntry := FExtensionList[I];
    List.Add(ExtEntry.Extension);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.GetGraphicFilter(Formats: TFormatTypes; SortType: TFilterSortType;
  Options: TFilterOptions; GraphicClass: TGraphicClass): string;

// Creates a string which can directly be used in an open or save dialog's filter property.
// Formats may be used to limit the number of formats to return.
// SortType determines how to sort the entries.
// Compact determines whether to group extensions (= True) or to put every extension on a separate line.
// AllImages finally determines whether to include the 'All image file' entry which includes all allowed extensions
// which qualify by the other properties.
// Usually all these options determine quite nicely which formats are well suited for a particular task
// but sometimes you may find it better to specify a graphic class to limit returned formats further.
// In this case set GraphicClass to the particular class otherwise set it nil.

var
  I, J: Integer;
  DL, EL, All: TStringList;
  ExtEntry: PExtensionEntry;
  ClassEntry: PClassEntry;
  S,
  DescriptionFormat: string;

begin
  Result := '';
  if Formats = [] then
    Formats := [ftAnimation..ftVector];
  DL := TStringList.Create;
  DL.Sorted := SortType in [fstDescription, fstBoth];
  EL := TStringList.Create;
  EL.Sorted := SortType in [fstExtension, fstBoth];

  // this string list is used to hold the (possibly sorted) list of all allowed extensions
  All := TStringList.Create;
  All.Sorted := SortType in [fstExtension, fstBoth];

  // using an adjusted format string makes the code below easier for different options
  DescriptionFormat := '%s';
  if foIncludeExtension in Options then
    DescriptionFormat := DescriptionFormat + '%s';

  if foCompact in Options then
  begin
    // all extension for a particular image class on one line
    for I := 0 to FClassList.Count - 1 do
    begin
      ClassEntry := FClassList[I];
      if (GraphicClass = nil) or (GraphicClass = ClassEntry.GraphicClass) then
      begin
        EL.Clear;
        // collect allowed extensions for the current graphic class,
        // this will automatically sort the entries if wanted
        for J := 0 to FExtensionList.Count - 1 do
        begin
          ExtEntry := FExtensionList[J];
          if (ExtEntry.ClassReference = ClassEntry) and ((ExtEntry.FormatTypes * Formats) <> []) then
            EL.Add(ExtEntry.Extension);
        end;

        // Build the extension list and a description entry.
        if foIncludeAll in Options then
          All.AddStrings(EL);
        S := '';
        for J := 0 to EL.Count - 1 do
          S := S + '*.' + EL[J] + '; ';
        // remove last semicolon and space
        SetLength(S, Length(S) - 2);
        if S <> '' then
          DL.AddObject(ClassEntry.Description, Pointer(StrNew(PChar(S))));
      end;
    end;
  end
  else
  begin
    // list each extension separately
    for I := 0 to FExtensionList.Count - 1 do
    begin
      ExtEntry := FExtensionList[I];
      if ((GraphicClass = nil) or (ExtEntry.ClassReference.GraphicClass = GraphicClass)) and
         ((ExtEntry.FormatTypes * Formats) <> []) then
      begin
        S := ExtEntry.Description;
        if S = '' then
          S := ExtEntry.ClassReference.Description;
        DL.AddObject(S, Pointer(StrNew(PChar('*.' + ExtEntry.Extension))));
        if foIncludeAll in Options then
          All.Add(ExtEntry.Extension);
      end;
    end;
  end;

  // Build final filter string out of the collected sub strings.
  if (foIncludeAll in Options) and (All.Count > 0) then
  begin
    // First include the general entry if wanted (this entry is never taken into sort order.
    S := '';
    for J := 0 to All.Count - 1 do
      S := S + '*.' + All[J] + ';';
    SetLength(S, Length(S) - 1);
    Result := gesAllImages + '|' + S + '|';
  end;

  for I := 0 to DL.Count - 1 do
  begin
    S := PChar(DL.Objects[I]);
    StrDispose(PChar(DL.Objects[I]));
    Result := Result + Format(DescriptionFormat, [DL[I], ' (' + S + ')']) + '|' + S + '|';
  end;
  // remove last separator in string
  if Length(Result) > 0 then
    SetLength(Result, Length(Result) - 1);
  All.Free;
  EL.Free;
  DL.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.GraphicFromExtension(S: string): TGraphicClass;

// Returns the class which belongs to the extension given in S or nil if there's non registered.
// S may contain a regular file name (also UNC is allowed), a string returned from ExtractFileExt (with period) or just
// an extension string.

var
  Index: Integer;

begin
  Result := nil;
  if Pos('.', S) > 0 then
    S := ExtractFileExt(S);
  if S <> '' then
  begin
    Index := Pos('.', S);
    if Index > 0 then
      Delete(S, 1, Index);
    Index := FindExtension(S);
    if Index > -1 then
      Result := PExtensionEntry(FExtensionList[Index]).ClassReference.GraphicClass;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.GraphicFromContent(const FileName: string): TGraphicExGraphicClass;

// Tries to determine the type of the image in the file. 

begin
  with TFileMapping.Create(FileName, fmmReadOnly) do
  try
    Result := GraphicFromContent(Memory, Size);
  finally
    Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.GraphicFromContent(const Memory: Pointer; Size: Int64): TGraphicExGraphicClass;

// Tries to determine the type of the image in the file. 

var
  I: Integer;
  T: TGraphicExGraphicClass;

begin
  Result := nil;
  for I := 0 to FClassList.Count - 1 do
  begin
    if PClassEntry(FClassList[I]).GraphicClass.InheritsFrom(TGraphicExGraphic) then
    begin
      T := TGraphicExGraphicClass(PClassEntry(FClassList[I]).GraphicClass);
      if T.CanLoad(Memory, Size) then
      begin
        Result := T;
        Break;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.GraphicFromContent(Stream: TStream): TGraphicExGraphicClass;

// Tries to determine the type of the image in the file. 

var
  LastPos: Int64;

begin
  LastPos := Stream.Position;

  if Stream is TCustomMemoryStream then
  begin
    // Simple case: memory streams already are in memory.
    with Stream as TCustomMemoryStream do
      Result := GraphicFromContent(Memory, Size);
  end
  else
    if (Stream is THandleStream) and (GetFileType(THandleStream(Stream).Handle) = FILE_TYPE_DISK) then
    begin
      // File streams can be mapped to access their content directly.
      with TFileMapping.Create(Stream as THandleStream) do
      try
        Result := GraphicFromContent(Memory, Size);
      finally
        Free;
      end;
    end
    else
    begin
      // Any other stream is converted into a memory stream first.
      with TMemoryStream.Create do
      try
        CopyFrom(Stream, 0);
        Position := 0;
        Result := GraphicFromContent(Memory, Size);
      finally
        Free;
      end;
    end;

  Stream.Position := LastPos;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFileFormatList.RegisterFileFormat(const Extension, Common, Individual: string; FormatTypes: TFormatTypes;
  Replace: Boolean; GraphicClass: TGraphicClass);

// Registers the given graphic class with the passed extension string. If there's already a class registered with this
// extension then either the registration of the older entry is replaced by the new one (Replace = True) or an exception
// is raised.
// This method takes also care to register the new extension with TPicture to make the default handling work too.
// Further parameters are:
// - Extension: the new extension to be registered (not necessarily with only 3 characters, but without a period).
// - Common: a description string for all extensions registered with the same class used when several extensions are
//   listed on one filter line. Pass '' to avoid changing a previously set value if there's one.
// - Individual: a description string used when each extension is listed separately.
// - FormatTypes: classifies the given file type as being a raster or vector file, with single or multiple images etc.
// - GraphicClass: the TGraphic descentant to be used to load and save the particular file.

var
  ExtIndex,
  ClassIndex: Integer;
  ExtEntry: PExtensionEntry;
  ClassEntry,
  OldReference: PClassEntry;

  //--------------- local functions -------------------------------------------

  procedure UpdateClassEntry;

  // updates a class entry (creates one if necessary)

  begin
    if ClassIndex = -1 then
    begin
      New(ClassEntry);
      ClassEntry.GraphicClass := GraphicClass;
      ClassEntry.Count := 0;
      FClassList.Add(ClassEntry);
    end
    else
      ClassEntry := FClassList[ClassIndex];

    if Common <> '' then
      ClassEntry.Description := Common;
    Inc(ClassEntry.Count);
    ExtEntry.ClassReference := ClassEntry;
  end;

  //--------------- end local functions ---------------------------------------

var
  S: string;

begin
  if Extension <> '' then
  begin
    ExtIndex := FindExtension(Extension);
    ClassIndex := FindGraphicClass(GraphicClass);
    if ExtIndex = -1 then
    begin
      // extension not yet registered
      New(ExtEntry);
      ExtEntry.Extension := Extension;
      ExtEntry.Description := Individual;
      ExtEntry.FormatTypes := FormatTypes;
      FExtensionList.Add(ExtEntry);
      UpdateClassEntry;
    end
    else
      if Replace then
      begin
        // replace current extension entry with new one
        ExtEntry := FExtensionList[ExtIndex];
        if ExtEntry.ClassReference.GraphicClass <> GraphicClass then
        begin
          // assign existing extension to new graphic class
          OldReference := ExtEntry.ClassReference;
          UpdateClassEntry;
          Dec(OldReference.Count);
          // remove the graphic class entry if no longer used
          if OldReference.Count = 0 then
            FClassList.Remove(OldReference);
        end;
          // otherwise do nothing
      end
      else
        GraphicExError(gesRegistration, [Extension]);

    // finally make TPicture work
    S := Individual;
    if S = '' then
      S := ClassEntry.Description;
    TPicture.RegisterFileFormat(Extension, S, GraphicClass);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFileFormatList.UnregisterFileFormat(const Extension: string; GraphicClass: TGraphicClass);

// Removes the entry for the given extension from the internal list.
// If Extension is '' then all associations for the given GraphicClass are removed otherwise the class is ignored and
// only the one particular extension is removed.
// Unregistration from TPicture is done here too, if necessary.

var
  ExtIndex,
  ClassIndex: Integer;
  ExtEntry: PExtensionEntry;
  ClassEntry: PClassEntry;

begin
  ExtIndex := FindExtension(Extension);
  // make sure we don't try to remove a non-registered extension
  if (Extension = '') or (ExtIndex > -1) then
  begin
    if ExtIndex > -1 then
    begin
      // there's an entry for the extension
      ExtEntry := FExtensionList[ExtIndex];
      Dec(ExtEntry.ClassReference.Count);
      // unregister graphic class too if necessary
      if ExtEntry.ClassReference.Count = 0 then
      begin
        TPicture.UnregisterGraphicClass(ExtEntry.ClassReference.GraphicClass);
        Dispose(ExtEntry.ClassReference);
        FClassList.Remove(ExtEntry.ClassReference);
      end;

      // finally delete extension entry
      Dispose(ExtEntry);
      FExtensionList.Delete(ExtIndex);
    end
    else
    begin
      // all entries for the given graphic class must be removed
      ClassIndex := FindGraphicClass(GraphicClass);
      ClassEntry := FClassList[ClassIndex];
      for ExtIndex := FExtensionList.Count - 1 downto 0 do
      begin
        if PExtensionEntry(FExtensionList[ExtIndex]).ClassReference.GraphicClass = GraphicClass then
        begin
          Dec(ClassEntry.Count);
          Dispose(PExtensionEntry(FExtensionList[ExtIndex]));
          FExtensionList.Delete(ExtIndex);
          // no need to run through further entries if all references are done
          if ClassEntry.Count = 0 then
            Break;
        end;
      end;
      Dispose(ClassEntry);
      FClassList.Delete(ClassIndex);
      TPicture.UnregisterGraphicClass(GraphicClass);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  FileFormatList := TFileFormatList.Create;
  with FileFormatList do
  begin
    // Since we are going to add these four image types below, we better unregister them first
    // in order to avoid double entries.
    // TODO: enable jpeg image
    //TPicture.UnregisterGraphicClass(TJPEGImage);
    TPicture.UnregisterGraphicClass(TBitmap);
    TPicture.UnregisterGraphicClass(TIcon);
    TPicture.UnregisterGraphicClass(TMetafile);

    RegisterFileFormat('bmp', gesBitmaps, '', [ftRaster], False, TBitmap);
    RegisterFileFormat('ico', gesIcons, '', [ftRaster], False, TIcon);
    RegisterFileFormat('wmf', gesMetaFiles, '', [ftVector], False, TMetafile);
    RegisterFileFormat('emf', gesMetaFiles, gesEnhancedMetaFiles, [ftVector], False, TMetafile);
    // TODO: enable jpeg image
    //RegisterFileFormat('jfif', gesJPGImages, gesJFIFImages, [ftRaster], False, TJPEGImage);
    //RegisterFileFormat('jpg', '', gesJPGImages, [ftRaster], False, TJPEGImage);
    //RegisterFileFormat('jpe', '', gesJPEImages, [ftRaster], False, TJPEGImage);
    //RegisterFileFormat('jpeg', '', gesJPEGImages, [ftRaster], False, TJPEGImage);

    // Paintshop pro *.msk files are just grayscale bitmaps.
    RegisterFileFormat('msk', '', '', [ftRaster], False, TBitmap);

    // register our own formats
    RegisterFileFormat('rle', gesBitmaps, gesRLEBitmaps, [ftRaster], False, TBitmap);
    RegisterFileFormat('dib', '', gesDIBs, [ftRaster], False, TBitmap);

    {$ifdef TargaGraphic}
      RegisterFileFormat('tga', gesTruevision, '', [ftRaster], False, TTargaGraphic);
      RegisterFileFormat('vst', '', '', [ftRaster], False, TTargaGraphic);
      RegisterFileFormat('vda', '', '', [ftRaster], False, TTargaGraphic);
      RegisterFileFormat('win', '', '', [ftRaster], False, TTargaGraphic);
      RegisterFileFormat('icb', '', '', [ftRaster], False, TTargaGraphic);
    {$endif TargaGraphic}

    {$ifdef TIFFGraphic}
      RegisterFileFormat('tif', gesTIFF, gesPCTIF, [ftRaster, ftMultiImage], False, TTIFFGraphic);
      RegisterFileFormat('tiff', '', gesMacTIFF, [ftRaster, ftMultiImage], False, TTIFFGraphic);
      RegisterFileFormat('fax', '', gesGFIFax, [ftRaster, ftMultiImage], False, TTIFFGraphic);
      {$ifdef EPSGraphic}
        RegisterFileFormat('eps', gesEPS, '', [ftRaster], False, TEPSGraphic);
      {$endif EPSGraphic}
    {$endif TIFFGraphic}

    {$ifdef PCXGraphic}
      RegisterFileFormat('pcx', gesZSoft, '', [ftRaster], False, TPCXGraphic);
      RegisterFileFormat('pcc', '', '', [ftRaster], False, TPCXGraphic);
      RegisterFileFormat('scr', '', gesZSoftWord, [ftRaster], False, TPCXGraphic);
    {$endif PCXGraphic}

    {$ifdef RLAGraphic}
      RegisterFileFormat('rpf', gesAliasWaveFront, '', [ftRaster], False, TRLAGraphic);
      RegisterFileFormat('rla', '', '', [ftRaster], False, TRLAGraphic);
    {$endif RLAGraphic}

    {$ifdef SGIGraphic}
      RegisterFileFormat('sgi', gesSGI, gesSGITrueColor, [ftRaster], False, TSGIGraphic);
      RegisterFileFormat('rgba', '', gesSGITrueColorAlpha, [ftRaster], False, TSGIGraphic);
      RegisterFileFormat('rgb', '', gesSGITrueColor, [ftRaster], False, TSGIGraphic);
      RegisterFileFormat('bw', '', gesSGIMono, [ftRaster], False, TSGIGraphic);
    {$endif SGIGraphic}

    {$ifdef PhotoshopGraphic}
      RegisterFileFormat('psd', gesPhotoshop, '', [ftRaster, ftLayered], False, TPSDGraphic);
      RegisterFileFormat('pdd', '', '', [ftRaster, ftLayered], False, TPSDGraphic);
    {$endif PhotoshopGraphic}

    {$ifdef PortableMapGraphic}
      RegisterFileFormat('ppm', gesPortable, gesPortablePixel, [ftRaster], False, TPPMGraphic);
      RegisterFileFormat('pgm', '', gesPortableGray, [ftRaster], False, TPPMGraphic);
      RegisterFileFormat('pbm', '', gesPortableMono, [ftRaster], False, TPPMGraphic);
    {$endif PortableMapGraphic}

    {$ifdef AutodeskGraphic}
      RegisterFileFormat('cel', gesAutodesk, '', [ftRaster], False, TAutodeskGraphic);
      RegisterFileFormat('pic', gesAutodesk, '', [ftRaster], False, TAutodeskGraphic);
    {$endif AutodeskGraphic}

    {$ifdef PCDGraphic}
      RegisterFileFormat('pcd', gesKodakPhotoCD, '', [ftRaster], False, TPCDGraphic);
    {$endif PCDGraphic}

    {$ifdef GIFGraphic}
      RegisterFileFormat('gif', gesCompuserve, '', [ftRaster, ftMultiImage, ftAnimation], False, TGIFGraphic);
    {$endif GIFGraphic}

    {$ifdef CUTGraphic}
      RegisterFileFormat('cut', gesHalo, '', [ftRaster], False, TCUTGraphic);
    {$endif CUTGraphic}

    {$ifdef PaintshopProGraphic}
      RegisterFileFormat('psp', gesPaintshopPro, '', [ftRaster, ftVector], False, TPSPGraphic);
      RegisterFileFormat('pfr', '', gesPaintshopProFrames, [ftRaster, ftVector], False, TPSPGraphic);
      RegisterFileFormat('tub', '', gesPaintshopProTubes, [ftRaster, ftVector], False, TPSPGraphic);
    {$endif PaintshopProGraphic}

    {$ifdef PortableNetworkGraphic}
      RegisterFileFormat('png', gesPortableNetworkGraphic, '', [ftRaster], False, TPNGGraphic);
    {$endif PortableNetworkGraphic}

    {$ifdef ArtsAndLettersGraphic}
      RegisterFileFormat('ged', gesArtsAndLettersGraphic, '', [ftRaster], False, TGEDGraphic);
    {$endif ArtsAndLettersGraphic}
  end;
finalization
  with FileFormatList do
  begin
    {$ifdef PaintshopProGraphic} UnregisterFileFormat('', TPSPGraphic); {$endif PaintshopProGraphic}
    {$ifdef PhotoshopGraphic} UnregisterFileFormat('', TPSDGraphic); {$endif PhotoshopGraphic}
    {$ifdef TargaGraphic} UnregisterFileFormat('', TTargaGraphic); {$endif TargaGraphic}
    {$ifdef TIFFGraphic} UnregisterFileFormat('', TTIFFGraphic); {$endif TIFFGraphic}
    {$ifdef SGIGraphic} UnregisterFileFormat('', TSGIGraphic); {$endif SGIGraphic}
    {$ifdef PCXGraphic} UnregisterFileFormat('', TPCXGraphic); {$endif PCXGraphic}
    {$ifdef AutodeskGraphic} UnregisterFileFormat('', TAutodeskGraphic); {$endif AutodeskGraphic}
    {$ifdef PCDGraphic} UnregisterFileFormat('', TPCDGraphic); {$endif PCDGraphic}
    {$ifdef PortableMapGraphic} UnregisterFileFormat('', TPPMGraphic); {$endif PortableMapGraphic}
    {$ifdef CUTGraphic} UnregisterFileFormat('', TCUTGraphic); {$endif CUTGraphic}
    {$ifdef GIFGraphic} UnregisterFileFormat('', TGIFGraphic); {$endif GIFGraphic}
    {$ifdef RLAGraphic} UnregisterFileFormat('', TRLAGraphic); {$endif RLAGraphic} 
    {$ifdef PortableNetworkGraphic} UnregisterFileFormat('', TPNGGraphic); {$endif PortableNetworkGraphic}
    {$ifdef ArtsAndLettersGraphic} UnregisterFileFormat('', TGEDGraphic); {$endif ArtsAndLettersGraphic}

    Free;
  end;
end.

