//******************************************************************************
// Basic Alias Maya IFF image reader, NOT an Amiga IFF image reader!
// reads 24 RGB, 32 RGBA Alias Maya IFF files, ignores Zbuffer and other Chunks
//------------------------------------------------------------------------------
// by Ingo Neumann, email: ingo@delphingo.com
//    This code is free for all kinds of usage
//
// based on C code of Luke Tokheim, <luke@stdout.org>
//                    http://www.stdout.org/~luke/iff2tga/
//
// Documentations:
// http://www.alias.com/eng/support/studiotools/documentation/Technical/FileFormats9.html
// http://caad.arch.ethz.ch/info/maya/manual/FileFormats/ImageFiles.fm.html#12318
//------------------------------------------------------------------------------
// History:
// 13.02.2005: + adapted to GraphicEx
// 12.02.2005: * up and running
// 06.02.2005: + Initial Creation
//******************************************************************************
unit GraphicExMayaIFF;
interface
uses Classes, SysUtils, Graphics, GraphicEx, GraphicColor;

type
     TByteArray = array of Byte;

     TMayaIffTag = array[0..3] of Char;

     TMayaIffChunk = record
        tag        :  TMayaIffTag;
        start      :  Cardinal;
        size       :  Cardinal;
        chunkType  :  TMayaIffTag;
     end;

     TMayaIffGraphic = class(TGraphicExGraphic)
     private
       FTiles : Word;
       FImagePropertiesLoaded : Boolean;
       FErrorCode : Cardinal;
       FChunkDepth : Integer;
       FChunkStack : array[0..32-1] of TMayaIffChunk;
       function BeginReadChunk( Stream : TStream ) : TMayaIffChunk;
       procedure EndReadChunk( Stream : TStream );
       function ReadShort( Stream : TStream ) : Word;
       function ReadLong( Stream : TStream ) : Cardinal;
       function ReadUncompressedTile( Stream : TStream; width, height, depth : Word ) : TByteArray;
       function ReadCompressedTile( Stream : TStream; size : Integer ) : TByteArray;
       function DecompressRLE( Stream : TStream; numBytes: Cardinal; compressedData : TByteArray;
                               compressedDataSize : Cardinal; var compressedIndex : Cardinal) : TByteArray;
       function DecompressRLEtile( Stream : TStream; width, height, depth : Word;
                compressedData : TByteArray; compressedDataSize :Cardinal) : TByteArray;
     public
       constructor Create; override;
       class function CanLoad(Stream: TStream): Boolean; override;
       procedure LoadFromStream(Stream: TStream); override;
       function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; override;
       property ErrorCode : Cardinal read FErrorCode;
     end;

// Error code definitions
const Maya_IFF_NO_ERROR     = 0;
const Maya_IFF_OPEN_FAILS   = 1;
const Maya_IFF_READ_FAILS   = 2;
const Maya_IFF_BAD_TAG      = 3;
const Maya_IFF_BAD_COMPRESS = 4;
const Maya_IFF_BAD_STACK    = 5;
const Maya_IFF_BAD_CHUNK    = 6;

//******************************************************************************
implementation

const RGB_FLAG     = 1;
const ALPHA_FLAG   = 2;
const ZBUFFER_FLAG = 4;

// define the IFF tags we are looking for in the file.
//                              <--- read right_to_left
const IFF_TAG_CIMG : TMayaIffTag = 'GMIC';
const IFF_TAG_FOR4 : TMayaIffTag = '4ROF';
const IFF_TAG_TBHD : TMayaIffTag = 'DHBT';
const IFF_TAG_TBMP : TMayaIffTag = 'PMBT';
const IFF_TAG_RGBA : TMayaIffTag = 'ABGR';
const IFF_TAG_CLPZ : TMayaIffTag = 'ZPLC';
const IFF_TAG_ESXY : TMayaIffTag = 'YXSE';
const IFF_TAG_ZBUF : TMayaIffTag = 'FUBZ';
const IFF_TAG_BLUR : TMayaIffTag = 'RULB';
const IFF_TAG_BLRT : TMayaIffTag = 'TRLB';
const IFF_TAG_HIST : TMayaIffTag = 'TSIH';

//------------------------------------------------------------------------------
// Fast 32bit BGRA <-> RGBA via versa Colorspace conversion
procedure BGRA_RGBA(P: Pointer; Count: Cardinal);
asm
    @@Loop:
      MOV ECX, [EAX]    // load BGRA
      BSWAP ECX         // -> ARGB
      ROR ECX, 8        // -> RGBA

      MOV [EAX], ECX    // save RGBA
      ADD EAX, 4        // next 32bit Pixel Adress
      DEC EDX           // dec counter
      JNZ @@Loop        // repeat loop until counter = 0
end;

//------------------------------------------------------------------------------
// Fast 24bit BGR <-> RGB via versa Colorspace conversion
procedure BGR_RGB(P: Pointer; Count: Cardinal);
var r, g, b : PByte;
    swap : byte;
begin
   asm
       DEC EDX           // dec counter first to prevent GPF on the very last pixel
       @@Loop:
         MOV ECX, [EAX]    // load BGRA
         BSWAP ECX         // -> ARGB
         ROR ECX, 8        // -> RGBA

         MOV [EAX], ECX    // save RGBA
         ADD EAX, 3        // next 24bit Pixel Adress
         DEC EDX           // dec counter
         JNZ @@Loop        // repeat loop until counter = 0
   end;

   // handle last pixel;
   r := Pointer(Integer(P)+((Count-1)*3));
   g := Pointer(Integer(r)+1);
   b := Pointer(Integer(r)+2);
   swap := r^;
   r^ := b^;
   b^ := swap;
end;

//------------------------------------------------------------------------------
constructor TMayaIffGraphic.Create;
begin
  inherited;
  FImagePropertiesLoaded := False;
  FErrorCode := Maya_IFF_NO_ERROR;
  FChunkDepth := -1;
end;

//------------------------------------------------------------------------------
function TMayaIffGraphic.ReadShort( Stream : TStream ) : Word;
var buf : array[0..1] of Byte;
    bytesRead : Integer;
begin
  Result := 0;

  bytesRead := Stream.Read(buf, sizeOf(buf));

  if bytesRead < sizeOf(buf) then begin
    if FErrorCode  = Maya_IFF_NO_ERROR then
       FErrorCode := Maya_IFF_READ_FAILS;
    raise Exception.Create('ReadShort');
  end;

  Result := ( ( buf[0] shl 8 ) + ( buf[1] ) );
end;

//------------------------------------------------------------------------------
function TMayaIffGraphic.ReadLong( Stream : TStream ) : Cardinal;
var buf : array[0..3] of Byte;
    bytesRead : Integer;
begin
  Result := 0;
  bytesRead := Stream.Read(buf, sizeOf(buf));

  if bytesRead < sizeOf(buf) then begin
    if FErrorCode  = Maya_IFF_NO_ERROR then
       FErrorCode := Maya_IFF_READ_FAILS;
    raise Exception.Create('ReadLong');
  end;

  Result :=( ( buf[0] shl 24 ) + ( buf[1] shl 16 )
	         + ( buf[2] shl 8 )  + ( buf[3] shl 0 ) );
end;


//------------------------------------------------------------------------------
function TMayaIffGraphic.ReadCompressedTile( Stream : TStream; size : Integer ) : TByteArray;
var buffer : TByteArray;
    bytesRead : Integer;
begin
  Result := NIL;
  SetLength(buffer, size);
  bytesRead := Stream.Read( buffer[0], size );

  if( bytesRead <= 0 ) then begin
    if FErrorCode =  Maya_IFF_NO_ERROR then
       FErrorCode := Maya_IFF_READ_FAILS;
    raise Exception.Create('ReadData');
  end;

  Result := buffer;
end;

//------------------------------------------------------------------------------
function TMayaIffGraphic.ReadUncompressedTile( Stream : TStream; width, height, depth : Word ) : TByteArray;
var data : TByteArray;
    pixel : array[0..3] of Byte;
    i, j, d, index : Integer;
begin
  SetLength(data, width * height * depth );

  for i := 0 to height-1 do begin
      index := i * width * depth;
      for j := 0 to width-1 do begin
	        Stream.Read(pixel, depth);
	        for d := ( depth - 1 ) downto 0 do begin
	            data[index] := pixel[d];
	            inc(index);
            end;
      end;
  end;

  Result := data;
end;

//------------------------------------------------------------------------------
function TMayaIffGraphic.BeginReadChunk(Stream: TStream): TMayaIffChunk;
begin
  result := FChunkStack[0];
  inc(FChunkDepth);
  if( (FChunkDepth >= High(FChunkStack)) or (FChunkDepth < 0) ) then begin
    if FErrorCode =  Maya_IFF_NO_ERROR then begin
       FErrorCode := Maya_IFF_BAD_STACK;
       raise Exception.Create('iff_bad_stack');
    end;
  end;

  FChunkStack[FChunkDepth].start := Stream.Position;
  FChunkStack[FChunkDepth].tag := TMayaIffTag(ReadLong( Stream ));
  FChunkStack[FChunkDepth].size := ReadLong( Stream );

  if( FChunkStack[FChunkDepth].tag =  IFF_TAG_FOR4 ) then begin
    // -- We have a form, so read the form type tag as well.
    FChunkStack[FChunkDepth].chunkType := TMayaIffTag(ReadLong( Stream ));
  end else begin
    FChunkStack[FChunkDepth].chunkType := #0#0#0#0;
  end;

  Result := FChunkStack[FChunkDepth];
end;

//------------------------------------------------------------------------------
procedure TMayaIffGraphic.EndReadChunk(Stream: TStream);
var fin : Cardinal;
    part : Integer;
begin
  fin := FChunkStack[FChunkDepth].start + FChunkStack[FChunkDepth].size + 8;

  if ( FChunkStack[FChunkDepth].chunkType[0] <> #0 ) then begin
    fin := fin + 4;
  end;

  // Add padding
  part := fin mod 4;
  if ( part <> 0 ) then begin
    fin := fin + 4 - part;
  end;

  Stream.Position := fin;

  dec(FChunkDepth);
end;

//------------------------------------------------------------------------------
class function TMayaIffGraphic.CanLoad(Stream: TStream): Boolean;
var chunkInfo : TMayaIffChunk;
    oldpos : Int64;
begin
  oldpos := Stream.Position;
  Stream.Read(chunkInfo, SizeOf(chunkInfo));
  Stream.Position := oldpos;

  // check here for tags only, in an ideal world we should also check
  // the colordepth and the existence of RGBA chunks
  Result := (chunkInfo.tag = 'FOR4') and (chunkInfo.chunkType = 'TBHD');
end;

//------------------------------------------------------------------------------
function TMayaIffGraphic.ReadImageProperties(Stream: TStream;  ImageIndex: Cardinal): Boolean;
var chunkInfo : TMayaIffChunk;
    flags, compress : Cardinal;
    tbmpfound : Boolean;
begin
  inherited ReadImageProperties(Stream, ImageIndex);

  // initialize the top of the chunk stack.
  FChunkDepth := -1;

  // file should begin with a FOR4 chunk of type CIMG
  chunkInfo := BeginReadChunk( Stream );
  if TMayaIffTag(chunkInfo.chunkType) <> IFF_TAG_CIMG then begin
    if FErrorCode = Maya_IFF_NO_ERROR then
       FErrorCode := Maya_IFF_BAD_TAG;
    Result := False;
    raise Exception.Create('iff_load:IFF_BAD_TAG');
  end;

  // Read the image header
  // OK, we have a FOR4 of type CIMG, look for the following tags
  //     TBHD	bitmap header, definition of size, etc.
  while True do with FImageProperties do begin

    chunkInfo := BeginReadChunk( Stream );

    // -- Right now, the only info we need about the image is in TBHD
    // -- so search this level until we find it.
    if( chunkInfo.tag =  IFF_TAG_TBHD ) then begin
      // -- Header chunk found, load parameters
      Width  := ReadLong( Stream );
      Height := ReadLong( Stream );
      ReadShort( Stream );                 // -- Don't support
      ReadShort( Stream );                 // -- Don't support
      flags  := ReadLong( Stream );
      ReadShort( Stream );                 // -- Don't support
      FTiles  := ReadShort( Stream );
      compress := ReadLong( Stream );

      EndReadChunk( Stream );

      // compression scheme
      case compress of
           0 : Compression := ctNone;
           1 : Compression := ctRLE;
      else
          Compression := ctUnknown;
      end;

      // Bpp ...
      SamplesPerPixel := 0;
      if( flags and RGB_FLAG ) > 0 then
        SamplesPerPixel := SamplesPerPixel + 3;
      if( flags and ALPHA_FLAG ) > 0 then
        SamplesPerPixel := SamplesPerPixel + 1;

      Options := [ioBigEndian];
      BitsPerSample := 8;
      case SamplesPerPixel of
        3: ColorScheme := csRGB;
        4: ColorScheme := csRGBA;
      end;
      BitsPerPixel := BitsPerSample * SamplesPerPixel;

      break;

    end else begin
      EndReadChunk( Stream );
    end;
  end; // END find TBHD while loop

  Result := True;
  FImagePropertiesLoaded := True;
end;

//------------------------------------------------------------------------------
procedure TMayaIffGraphic.LoadFromStream(Stream: TStream);
var
  chunkInfo : TMayaIffChunk;
  depth : Cardinal;
  flags, compress : Cardinal;

  tbmpfound : Boolean;

  tileX1, tileX2, tileY1, tileY2, tileWidth, tileHeight : Word;
  tileNumber : Cardinal;
  tileData : TByteArray;

  isCompressed, i : Cardinal;
  remainingDataSize : Cardinal;
  streamData : TByteArray;

begin
  if not FImagePropertiesLoaded then
  if not ReadImageProperties(Stream, 0) then
     exit;

  // check correct bpp
  depth := FImageProperties.SamplesPerPixel;
  if (depth <> 3) and (depth <> 4) then
    raise Exception.Create('iff_load: bpp <> 24 or 32');

  // setup bitmap
  Width := FImageProperties.Width;
  Height := FImageProperties.Height;
  case depth of
       3 : PixelFormat := pf24Bit;
       4 : PixelFormat := pf32Bit;
  end;


  // assume the next FOR4 of type TBMP
  tbmpfound := False;
  while not tbmpfound do begin

    chunkInfo := BeginReadChunk( Stream );

    // OK, we have a FOR4 of type TBMP, (embedded FOR4)
    // look for the following tag: RGB(A) color data, RLE compressed tiles of 24/32 bbp data
    if chunkInfo.chunkType =  IFF_TAG_TBMP then begin
       tbmpfound := True;

       // image data found
       tileNumber := 0;
       if depth = 0 then
           tileNumber := FTiles;

       // read tiles
       while tileNumber < FTiles do begin

         chunkInfo := BeginReadChunk( Stream );
         if (chunkInfo.tag <> IFF_TAG_RGBA) and (chunkInfo.tag <> IFF_TAG_ZBUF) then begin
             if( FErrorCode =  Maya_IFF_NO_ERROR ) then
                 FErrorCode := Maya_IFF_BAD_CHUNK;
             raise Exception.Create('iff_load:IFF_BAD_CHUNK');
         end;

         remainingDataSize := chunkInfo.size - 8;

         // Get tile size and location info
         tileX1     := ReadShort( Stream );
         tileY1     := ReadShort( Stream );
         tileX2     := ReadShort( Stream );
         tileY2     := ReadShort( Stream );
         tileWidth  := tileX2 - tileX1 + 1;
         tileHeight := tileY2 - tileY1 + 1;

         if( chunkInfo.size >= ( tileWidth * tileHeight * depth + 8 ) )
             then isCompressed := 0
             else isCompressed := 1;

         // -- OK, we found an RGB(A) chunk, eat it.
         if chunkInfo.tag =  IFF_TAG_RGBA then begin
            if depth =  0 then begin
               EndReadChunk( Stream );
            end else begin
               if isCompressed > 0 then begin
                 streamData := ReadCompressedTile( Stream, remainingDataSize );
                 tileData := DecompressRLEtile( Stream, tileWidth, tileHeight, depth, streamData, remainingDataSize );
                 streamData := NIL; // unallocate
               end else begin
                 tileData := ReadUncompressedTile( Stream, tileWidth, tileHeight, depth );
               end;

               if tileData <> NIL then begin
                  // dump uncompressed tile RGB(A) data into bitmap with vertical flip
                  for i := 0 to tileHeight-1 do
                      move( tiledata[tileWidth * depth * i],
                            Pointer( Integer( Scanline[Height-1 - (tileY1+i)] ) + (tileX1*depth) )^,
                            tileWidth * depth);
                  tileData := NIL; // unallocate
               end;

               EndReadChunk( Stream );
               inc(tileNumber);
           end;
         end // END RGBA chunk
         else begin
             // sikp unknown chunk
             EndReadChunk( Stream );
         end;
       end; // END while TBMP tiles
    end // END if TBMP
    else begin
      EndReadChunk( Stream );
    end;
  end;

  // RGB(A) -> BGR(A)
  for i := 0 to Height-1 do
      case depth of
           3 : BGR_RGB  (Scanline[i], width);
           4 : BGRA_RGBA(Scanline[i], width);
      end;
end;

//------------------------------------------------------------------------------
function TMayaIffGraphic.DecompressRLE( Stream : TStream; numBytes: Cardinal; compressedData : TByteArray;
         compressedDataSize : Cardinal; var compressedIndex : Cardinal) : TByteArray;

var data : TByteArray;
    nextChar, count : Byte;
    i : Integer;
    byteCount : Cardinal;

begin
  SetLength(data, numBytes);

  byteCount := 0;
  while byteCount < numBytes do begin

    if compressedIndex >= compressedDataSize then
       break;

    nextChar := compressedData[ compressedIndex ];
    inc(compressedIndex);

    count := (nextChar and $7F) + 1;
    if (byteCount + count) > numBytes then
       break;

    if (nextChar and $80) > 0 then begin

       // we have a duplication run
       nextChar := compressedData[ compressedIndex ];
       inc(compressedIndex);

       for i := 0 to count-1 do begin
           data[byteCount] := nextChar;
           inc(byteCount);
       end;

    end else begin
      // we have a verbatim run
      for i := 0 to count-1 do begin
	        data[byteCount] := compressedData[compressedIndex];
	        inc(compressedIndex);
	        inc(byteCount);
      end;
    end;

    if byteCount > numBytes then
       raise Exception.Create('iff_decompress_rle');
  end;

  Result := Data;
end;

//------------------------------------------------------------------------------
// Decompress the different RGB(A) channels
// MUST OPERATE ON RGB(A) !!!
function  TMayaIffGraphic.DecompressRLEtile( Stream : TStream; width, height, depth : Word;
          compressedData : TByteArray; compressedDataSize :Cardinal) : TByteArray;

var channels : array[0..3] of TByteArray;
  data : TByteArray;
  rw, offset, i, k, row, column : Integer;
  compressedIndex :Cardinal;

begin
  Result := NIL;
  compressedIndex := 0;

  for i := depth-1 downto 0 do
      channels[i] := DecompressRLE( Stream, width * height, compressedData,
                                         compressedDataSize, compressedIndex );

  // pack all of the channels from the decompression into an RGB(A) array.
  SetLength(data, width * height * depth);

  for row := 0 to height-1 do begin
    rw := row * width;
    for column := 0 to width-1 do begin
      for k := 0 to depth-1 do begin
          offset := rw + column;
	        data[depth*offset + k] := channels[k][offset];
      end;
    end;
  end;

  // free allocated memory
  for i := 0 to depth-1 do
      channels[i] := NIL;

  Result := data;
end;

//******************************************************************************
initialization
    FileFormatList.RegisterFileFormat('iff', 'Maya IFF images', '', [ftRaster], False, True, TMayaIffGraphic);
finalization
    FileFormatList.UnregisterFileFormat('iff', TMayaIffGraphic);
end.
