unit GXzLib;

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is GXzlib.pas, released November 1, 1999.
//
// The initial developer of the original code is Dipl. Ing. Mike Lischke (Pleiﬂa, Germany, www.delphi-gems.com),
//
// Portions created by Dipl. Ing. Mike Lischke are
// Copyright (C) 1999-2003 Dipl. Ing. Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of the image library GraphicEx.

interface

{$Include Compilers.inc}

{$Z2}      // enum size = word
// Align record structures to 4 byte boundaries.
{$ifdef COMPILER_7_UP}
  {$Align 4}
{$else}
  {$Align On}
{$endif COMPILER_7_UP}

{$ifdef COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
  // We switch off warnings about that fact. We know it and we accept it.
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CAST off}
  {$warn UNSAFE_CODE off}
{$endif COMPILER_7_UP}

const
  ZLIB_VERSION = '1.1.4';

  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1; // will be removed, use Z_SYNC_FLUSH instead
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;

  // Return codes for the compression/decompression functions.
  // Negative values are errors, positive values are used for special but normal events.
  Z_OK            = 0;
  Z_STREAM_END    = 1;
  Z_NEED_DICT     = 2;
  Z_ERRNO         = (-1);
  Z_STREAM_ERROR  = (-2);
  Z_DATA_ERROR    = (-3);
  Z_MEM_ERROR     = (-4);
  Z_BUF_ERROR     = (-5);
  Z_VERSION_ERROR = (-6);

  Z_NO_COMPRESSION       =   0;
  Z_BEST_SPEED           =   1;
  Z_BEST_COMPRESSION     =   9;
  Z_DEFAULT_COMPRESSION  = (-1);

  Z_FILTERED         = 1;
  Z_HUFFMAN_ONLY     = 2;
  Z_DEFAULT_STRATEGY = 0;

  Z_BINARY   = 0;
  Z_ASCII    = 1;
  Z_UNKNOWN  = 2;

  Z_DEFLATED = 8;

  Z_NULL = 0;  // For initializing zalloc, zfree, opaque.

type
  alloc_func = function (opaque: Pointer; Items, Size: Cardinal): Pointer;
  free_func = procedure (opaque, address: Pointer);

  z_streamp = ^z_stream;
  z_stream = record
    next_in: PChar;     // next input byte
    avail_in: Integer;  // number of bytes available at next_in
    total_in: Integer;  // total nb of input bytes read so far

    next_out: PChar;    // next output byte should be put here
    avail_out: Integer; // remaining free space at next_out
    total_out: Integer; // total nb of bytes output so far

    msg: PChar;         // last error message, NULL if no error
    state: Pointer;     // not visible by applications

    zalloc: alloc_func; // used to allocate the internal state
    zfree: free_func;   // used to free the internal state
    opaque: Pointer;    // private data object passed to zalloc and zfree

    data_type: Integer; //  best guess about the data type: ascii or binary
    adler: Integer;     // adler32 value of the uncompressed data
    reserved: Integer;  // reserved for future use
  end;

function adler32(adler: Cardinal; buf: Pointer; len: Integer): Cardinal;
function crc32(crc: Cardinal; buf: Pointer; len: Cardinal): Cardinal;

function deflateInit_(var strm: z_stream; level: Integer; version: PChar; recsize: Integer): Integer;
function deflateInit2_(var strm: z_stream; level, method, windowBits, memLevel, strategy: Integer; version: PChar;
  stream_size: Integer): Integer; 
function deflate(var strm: z_stream; flush: Integer): Integer;
function deflateEnd(var strm: z_stream): Integer;
function deflateParams(var strm: z_stream; level, strategy: Integer): Integer; 
function deflateReset(var strm: z_stream): Integer;

function InflateInit(var Z: z_stream): Integer;
function inflateInit_(var strm: z_stream; version: PChar; recsize: Integer): Integer;
function inflate(var strm: z_stream; flush: Integer): Integer;
function inflateEnd(var strm: z_stream): Integer;
function inflateReset(var strm: z_stream): Integer; 
function inflateSync(var strm: z_stream): Integer;         

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  LibStub;

{$L adler32.obj}
{$L compress.obj}
{$L crc32.obj}
{$L uncompr.obj}
{$L deflate.obj}
{$L trees.obj}
{$L zutil.obj}
{$L inflate.obj}
{$L infblock.obj}
{$L inftrees.obj}
{$L infcodes.obj}
{$L infutil.obj}
{$L inffast.obj}

//----------------------------------------------------------------------------------------------------------------------

function adler32(adler: Cardinal; buf: Pointer; len: Integer): Cardinal; external;
function crc32(crc: Cardinal; buf: Pointer; len: Cardinal): Cardinal; external;

function deflateInit_(var strm: z_stream; level: Integer; version: PChar; recsize: Integer): Integer; external;
function deflateInit2_(var strm: z_stream; level, method, windowBits, memLevel, strategy: Integer; version: PChar;
  stream_size: Integer): Integer; external;

function deflate(var strm: z_stream; flush: Integer): Integer; external;
function deflateEnd(var strm: z_stream): Integer; external;
function deflateParams(var strm: z_stream; level, strategy: Integer): Integer; external;
function deflateReset(var strm: z_stream): Integer; external;

function inflateInit_(var strm: z_stream; version: PChar; recsize: Integer): Integer; external;
function inflate(var strm: z_stream; flush: Integer): Integer; external;
function inflateEnd(var strm: z_stream): Integer; external;
function inflateReset(var strm: z_stream): Integer; external;
function inflateSync(var strm: z_stream): Integer; external;

//----------------------------------------------------------------------------------------------------------------------

function InflateInit(var Z: z_stream): Integer;

// Initializes the internal stream state for decompression. 
//
// InflateInit returns Z_OK if success, Z_MEM_ERROR if there was not enough memory, Z_VERSION_ERROR if the zlib library
// version is incompatible with the version assumed by the caller. Msg is reset if there is no
// error message. InflateInit does not perform any decompression: this will be done by Inflate.

begin
  Result := InflateInit_(Z, ZLIB_VERSION, SizeOf(z));
end;

//----------------------------------------------------------------------------------------------------------------------

end.
