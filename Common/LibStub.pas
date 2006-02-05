unit LibStub;

//----------------------------------------------------------------------------------------------------------------------
//
// This unit is released under the MIT license:
// Copyright (c) 1999-2005 Mike Lischke (support@soft-gems.net, www.soft-gems.net).
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
// documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the
// Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
// WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
// OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
// You are asked to give the author(s) the due credit. This means that you acknowledge the work of the author(s)
// in the product documentation, about box, help or wherever a prominent place is.
//
//----------------------------------------------------------------------------------------------------------------------
// The original code is LibStub.pas, released February 1, 2003.
//
// The initial developer of the original code is Dipl. Ing. Mike Lischke (Pleiﬂa, Germany, www.delphi-gems.com),
//
// Portions created by Dipl. Ing. Mike Lischke are
// Copyright (C) 1999-2003 Dipl. Ing. Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// LibStub is an utility unit for Borland C++ Builder RTL libraries bound to Delphi code via object files.
// It requires Delphi 6 or better to compile.
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$include Compilers.inc}

{$ifdef COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
  // We switch off warnings about that fact. We know it and we accept it.
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CAST off}
  {$warn UNSAFE_CODE off}
{$endif COMPILER_7_UP}

{$define Underlined}

uses
  Windows, Classes;

var
  __turboFloat: LongBool = False;
  // _streams is only a dummy. In the BCB RTL it refers to an array of FILE structures for io operations
  // containing the file structures for stdin, stdout and stderror.
  // With the declaration below we create nil pointers for these standard "files". We can test them in the
  // file functions and act accordingly.
  __streams: array[0..2] of Pointer;

type
  cmp_callback = function(P1, P2: Pointer): Integer; cdecl;

  // Greenwich Mean Time (GMT)
  ptm = ^tm;
  tm = record
    tm_sec: Integer;
    tm_min: Integer;
    tm_hour: Integer;
    tm_mday: Integer;
    tm_mon: Integer;
    tm_year: Integer;
    tm_wday: Integer;
    tm_yday: Integer;
    tm_isdst: Integer;
  end;

  {$ifdef COMPILER_5}
    PPChar = ^PChar;
  {$endif COMPILER_5}

  Ptime_t = ^time_t;
  time_t = Integer;
  
procedure __assert(__cond, __file: PChar; __line: Integer); cdecl;
procedure __cexit;
function __ftol: Integer; cdecl;
function __ltoupper(c: Integer): Integer; cdecl;
procedure _Abort;
function _atan2(Y, X: Double): Double; cdecl;
function _atan(X: Double): Double; cdecl;
function _atof(Value: PChar): Double; cdecl;
function _atol(Value: PChar): Integer; cdecl;
function _calloc(nitems, size: Cardinal): Pointer; cdecl;
function _ceil(Value: Double): Double; cdecl;
function _cos(Value: Double): Double; cdecl;
function _ctime(const clock: Ptime_t): PChar; cdecl;
procedure _exit; cdecl;
function _exp(Value: Double): Double; cdecl;
function _fabs(Value: Double): Double; cdecl;
function _fclose(Stream: TStream): Integer; cdecl;
function _fflush(Stream: TStream): Integer; cdecl;
function _fgetc(Stream: TStream): Integer; cdecl;
function _floor(Value: Double): Double; cdecl;
function _fopen(const filename, mode: PChar): TStream; cdecl;
function _fread(var buf; recsize, reccount: Integer; Stream: TStream): Integer; cdecl;
procedure _free(P: Pointer); cdecl;
function _fputc(c: Integer; Stream: TStream): Integer; cdecl;
function _frexp(x: Double; var Exponent: Integer): Double; cdecl;
function _fscanf(Stream: TStream; Format: PChar; Argument: array of Pointer): Integer; cdecl;
function _fseek(Stream: TStream; offset, origin: Integer): Integer; cdecl;
function _ftell(Stream: TStream): Integer; cdecl;
function _fwrite(const buf; recsize, reccount: Integer; Stream: TStream): Integer; cdecl;
function _gmtime(clock: PInteger): ptm; cdecl;
function _hypot(x, y: Double): Double; cdecl;
function _isalnum(c: Integer): Integer; cdecl;
function _isdigit(c: Integer): Integer; cdecl;
function _isprint(c: Integer): Integer; cdecl;
function _isxdigit(c: Integer): Integer; cdecl;
function _ldexp(x: Double; exp: Integer): Double; cdecl;
function _localtime(clock: PInteger): ptm; cdecl;
function _log(Value: Double): Double; cdecl;
function _malloc(size: Integer): Pointer; cdecl;
function _memcmp(s1, s2: Pointer; n: Integer): Integer; cdecl;
procedure _memcpy(dest, source: Pointer; count: Integer); cdecl;
function _memmove(dest, src: Pointer; n: Cardinal): Pointer; cdecl;
procedure _memset(P: Pointer; B: Integer; count: Integer); cdecl;
function _pow(x, y: Double): Double; cdecl;
function _rand: Integer; cdecl;
procedure _qsort(base: Pointer; nelem, width: Cardinal; fcmp: cmp_callback); cdecl;
function _sin(Value: Double): Double; cdecl;
procedure _sprintf(Buffer, Format: PChar; Arguments: va_list); cdecl;
function _sqrt(Value: Double): Double; cdecl;
function _sscanf(Buffer, Format: PChar; Argument: array of Pointer): Integer; cdecl;
function _strcat(dest, src: PChar): PChar; cdecl;
function _strchr(s: PChar; c: Integer): PChar; cdecl;
function _strcpy(dest, src: PChar): PChar; cdecl;
function _strdup(s: PChar): PChar; cdecl;
function _stricmp(s1, s2: PChar): Integer; cdecl;
function _strlen(s: PChar): Cardinal; cdecl;
function _strncmp(s1, s2: PChar; maxlen: Cardinal): Integer; cdecl;
function _strncpy(strDest, strSource: PChar; count: Cardinal): PChar; cdecl;
function _strstr(s1, s2: PChar): PChar; cdecl;
function _strtod(s: PChar; endptr: PPChar): Double; cdecl;
procedure _swab(__from, __to: PChar; __nbytes: Integer); cdecl;
function _tan(Value: Double): Double; cdecl;
function _time(__timer: Ptime_t): time_t; cdecl;
function _unlink(FileName: PChar): Integer; cdecl;
function _vfprintf(Stream: TStream; Format: PChar; Arguments: va_list): Integer; cdecl;
function _vprintf(Format: PChar; Arguments: va_list): Integer; cdecl;
procedure _vsprintf(Buffer, Format: PChar; Arguments: va_list); cdecl;
function _wcscpy(Destination, Source: PWideChar): PWideChar; cdecl;
function _wcstombs(mbstr: PChar; wcstr: PWideChar; count: Cardinal): Cardinal; cdecl;

// Note: these functions cannot be emulated by Delphi nor forwarded to external APIs.
// Using them will raise an exception.
procedure _fprintf; cdecl;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Scanf,
  Math, SysUtils, DateUtils;

{$ifndef COMPILER_6_UP}
const
  NaN = 0.0 / 0.0;
{$endif COMPILER_6_UP}

var
  _daylight: Boolean;
  _timezone: Integer;

//----------------------------------------------------------------------------------------------------------------------

function StrLenW(Str: PWideChar): Cardinal;

// Returns number of characters in a string, not counting the null terminator.

asm
       MOV     EDX, EDI
       MOV     EDI, EAX
       MOV     ECX, 0FFFFFFFFH
       XOR     AX, AX
       REPNE   SCASW
       MOV     EAX, 0FFFFFFFEH
       SUB     EAX, ECX
       MOV     EDI, EDX

end;

//----------------------------------------------------------------------------------------------------------------------

procedure __assert(__cond, __file: PChar; __line: Integer);

// Emulation of the BCB RTL runtime library function _assert.

begin
  raise Exception.CreateFmt('Assertion failed: %s, file %s, line %d', [__cond, __file, __line]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure __cexit;

begin
  Exit;
end;

//----------------------------------------------------------------------------------------------------------------------

function __ftol: Integer;

var
  F: Double;

begin
  asm
    LEA   EAX, F             //  BC++ passes floats on the FPU stack
    FSTP QWORD PTR [EAX]     //  Delphi passes floats on the CPU stack
  end;
  Result := Trunc(f);
end;

//----------------------------------------------------------------------------------------------------------------------

function __ltoupper(c: Integer): Integer;

// Note: this code does not consider changes made with setlocale.

begin
  Result := Integer(Upcase(Char(c)));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure _Abort;

begin
  Sysutils.Abort;
end;

//----------------------------------------------------------------------------------------------------------------------

function _atan2(Y, X: Double): Double;

begin
  Result := Arctan2(Y, X);
end;

//----------------------------------------------------------------------------------------------------------------------

function _atan(X: Double): Double; cdecl;

begin
  Result := Arctan2(X, 1);
end;

//----------------------------------------------------------------------------------------------------------------------

function _atof(Value: PChar): Double;

// Note: this code does not consider changes made with setlocale.

begin
  Result := StrToFloatDef(Value, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function _atol(Value: PChar): Integer;

// Note: this code does not consider changes made with setlocale.

begin
  Result := StrToIntDef(Value, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function _calloc(nitems, size: Cardinal): Pointer;

begin
  Result := AllocMem(nitems * size);
end;

//----------------------------------------------------------------------------------------------------------------------

function _ceil(Value: Double): Double;

begin
  Result := Math.Ceil(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

function _cos(Value: Double): Double;

begin
  Result := System.Cos(Value); // Different types but same name. Keep this stub!
end;

//----------------------------------------------------------------------------------------------------------------------

var
  // Static buffer for ctime to return a time in a string of the form "Mon Nov 21 11:31:54 1983\n\0".
  // It is overriden everytime ctime is called (just as the BCB equivalent).
  Staticctime: array[0..25] of Char;

function _ctime(const clock: Ptime_t): PChar;

var
  Time: TDateTime;
  Buffer: string;

begin
  if clock = nil then
    Result := nil
  else
  begin
    Time := UnixToDateTime(clock^);
    DateTimeToString(Buffer, 'ddd mmm dd hh:mm:ss yyyy'#13#0, Time);
    StrPCopy(Staticctime, Buffer);
    Result := Staticctime;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure _exit;

begin
  System.Exit;
end;

//----------------------------------------------------------------------------------------------------------------------

function _exp(Value: Double): Double;

begin
  Result := System.Exp(Value); // Different types but same name. Keep this stub!
end;

//----------------------------------------------------------------------------------------------------------------------

function _fabs(Value: Double): Double;

begin
  Result := Abs(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

function _fclose(Stream: TStream): Integer;

begin
  if Assigned(Stream) then
    Stream.Free;
  Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function _fflush(Stream: TStream): Integer;

begin
  Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function _fgetc(Stream: TStream): Integer;

var
  B: Byte;

begin
  if Stream.Position < Stream.Size then
  begin
    Stream.ReadBuffer(B, 1);
    Result := B;
  end
  else
    Result := $0020; // The EOF flag.
end;

//----------------------------------------------------------------------------------------------------------------------

function _floor(Value: Double): Double;

begin
  Result := Math.Floor(Value); // Different types but same name. Keep this stub!
end;

//----------------------------------------------------------------------------------------------------------------------

function _fopen(const filename, mode: PChar): TStream;

// Maps, together with the other file functions, all C file access to a file stream.

var
  FileMode: Word;
  Append: Boolean;

begin
  // Convert mode string into a mode parameter. Only the most commonly used flags are supported.
  Append := False;
  case mode^ of
    'r', 'R':
      if (StrLen(mode) > 1) and ((mode + 1)^ = '+') then
        FileMode := fmOpenReadWrite
      else
        FileMode := fmOpenRead;
    'w', 'W':
      FileMode := fmOpenWrite;
    'a', 'A': // Append.
      begin
        FileMode := fmOpenReadWrite;
        Append := True;
      end;
  else
    FileMode := $FFFF;
  end;

  if FileMode = $FFFF then
    Result := nil
  else
    try
      Result := TFileStream.Create(filename, FileMode or fmShareDenyWrite);
      if Append then
        Result.Position := Result.Size;
    except
      Result := nil;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function _fread(var buf; recsize, reccount: Integer; Stream: TStream): Integer;

begin
  if Assigned(Stream) then
    if Stream.Read(buf, recsize * reccount) > 0 then
      Result := reccount
    else
      Result := 0
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure _free(P: Pointer);

begin
  FreeMem(P);
end;

//----------------------------------------------------------------------------------------------------------------------

function _fputc(c: Integer; Stream: TStream): Integer;

begin
  Stream.Write(c, 1);
  Result := c;
end;

//----------------------------------------------------------------------------------------------------------------------

function _frexp(x: Double; var Exponent: Integer): Double;

var
  Mantissa: Extended;

begin
  Math.Frexp(X, Mantissa, Exponent);
  Result := Mantissa;
end;

//----------------------------------------------------------------------------------------------------------------------

function _fscanf(Stream: TStream; Format: PChar; Argument: array of Pointer): Integer;

// Note: the actual implementation was provided by Evgeni Sorokin.

begin
  Result := Scanf.fscanf(Stream, Format, Argument);
end;

//----------------------------------------------------------------------------------------------------------------------

function _fseek(Stream: TStream; offset, origin: Integer): Integer;

begin
  Stream.Seek(offset, origin);
  Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function _ftell(Stream: TStream): Integer;

begin
  Result := Stream.Position;
end;

//----------------------------------------------------------------------------------------------------------------------

function _fwrite(const buf; recsize, reccount: Integer; Stream: TStream): Integer;

begin
  Result := Stream.Write(buf, recsize * reccount)
end;

//----------------------------------------------------------------------------------------------------------------------

// The following two routines are support functions for the emulation of the C RTL function gmtime.

const
  Thursday = 4;
  April    = 4;
  October  = 10;

  _Days: array[0..11] of Byte = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
  _YDays: array[0..12] of Integer = (0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365);

//----------------------------------------------------------------------------------------------------------------------

function _isDST (hour, yday, month, year: Cardinal): Boolean;

// Determines whether daylight savings is in effect and returns non-zero if so for the given date.
//
// If month is 0, yday is the day of the year; otherwise yday is the day of the month, where month is 1 for Jan, 2 for
// Feb, etc. In either case, yday is zero based.  year is the number of years since 1970.

var
  temp,
  sunday: Cardinal;
  
begin
  Result := False;

  if month = 0 then // if only day of year given 
  begin
    temp := yday;
    if (yday >= 31 + 28) and (((year + 70) and 3) = 0) then
      Dec(temp);
    month := 0;
    while temp >= Cardinal(_YDays[month]) do
      Inc(month);
  end
  else  // if month+day of month given 
  begin
    Inc(yday, _YDays[month - 1]);
    if (month > 2) and (((year + 70) and 3) = 0) then  // leap year, Mar-Dec
      Inc(yday);
  end;

  // Weed out months other than April or October.
  if (month < April) or (month > October) then
    System.Exit;
  if (month > April) and (month < October) then
  begin
    Result := True;
    System.Exit;
  end;

  // Month is either April or October. Up through 1986, the starting * day for DST is the last Sunday in April; after
  // that it is * the first Sunday in April.  The last day of DST is the last Sunday in October.
  if (year > 16) and (month = April) then // if April and year > 1986
    sunday := _YDays[month - 1] + 7       // day = 7th day in month
  else
    sunday := _YDays[month];              // day = last day in month
  if Boolean((year + 70) and 3) then      // leap year ?
    Dec(sunday);                          // no --> adjust

  // Adjust the threshold day downward to the preceding Sunday.
  temp := sunday + ((year + 1) shr 2)     // add leap days since 1970
         + (year * 365)                   // add years since 1970
         + Thursday;                      // 01-01-70 was Thursday
  Dec(sunday, temp mod 7);                // back up to Sunday

  // Check if the day falls within the limit.
  if month = April then
  begin
    if yday > sunday then
      Result := True
    else
      if yday = sunday then
        Result := hour >= 2; // DST started at 2:00 AM
  end
  else
  begin
    if yday < sunday then
      Result := True
    else
      if yday = sunday then
        Result := hour <= 1; // DST ended at 2:00 AM
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

var
  tmX: tm; // Variable will be overwritten each time comtime is called. Just like the static struct in BCB gmtime.c.

function comtime(ltime: Integer; dst: Boolean): ptm;

var
  time: Cardinal;
  hpery: Integer;
  i: Integer;
  cumdays: Cardinal;

begin
  time := Cardinal(ltime);
  tmX.tm_sec := Integer(time mod 60);
  time := time div 60;                      // Time in minutes
  tmX.tm_min := Integer(time mod 60);
  time := time div 60;                      // Time in hours
  i := Cardinal(time div (1461 * Cardinal(24))); // Number of 4 year blocks.
  tmX.tm_year := i shl 2;
  Inc(tmX.tm_year, 70);
  cumdays := 1461 * i;
  time := time mod (1461 * Cardinal(24));   // Hours since end of last 4 year block.

  repeat
    hpery := 365 * 24;
    if (tmX.tm_year and 3) = 0 then
      Inc(hpery, 24);
    if time < Cardinal(hpery) then
       Break;
    Inc(cumdays, hpery div 24);
    Inc(tmX.tm_year);
    Dec(time, hpery);
  until False; // At end, time is number of hours into current year.

  if dst and _daylight and _isDST(time mod 24, time div 24, 0, tmX.tm_year - 70) then
  begin
    Inc(time);
    tmX.tm_isdst := 1;
  end
  else
    tmX.tm_isdst := 0;

  tmX.tm_hour := (time mod 24);
  time := time div 24;             // Time in days
  tmX.tm_yday := time;
  Inc(cumdays, time + 4);
  tmX.tm_wday := cumdays mod 7;
  Inc(time);

  Result := @tmX;
  if (tmX.tm_year and 3) = 0 then
  begin
    if time > 60 then
      Dec(time)
    else
      if time = 60 then
      begin
        tmX.tm_mon := 1;
        tmX.tm_mday := 29;
        System.Exit;
      end;
  end;

  tmX.tm_mon := 0;
  while _Days[tmX.tm_mon] < time do
  begin
    Inc(tmX.tm_mon);
    Dec(time, _Days[tmX.tm_mon]);
  end;

  tmX.tm_mday := time;
end;

//----------------------------------------------------------------------------------------------------------------------

function _gmtime(clock: PInteger): ptm;

// Converts date and time to Greenwich Mean Time.
// gmtime returns a pointer to a structure containing the broken-down time.

begin
  Result := comtime(clock^, False);
end;

//----------------------------------------------------------------------------------------------------------------------

function _hypot(x, y: Double): Double;

begin
  Result := Math.Hypot(X, Y); // Different types but same name. Keep this stub!
end;

//----------------------------------------------------------------------------------------------------------------------

function _isalnum(c: Integer): Integer;

// Note: this code does not consider changes made with setlocale. Currently only Latin characters are checked.

begin
  Result := Ord(Char(c) in ['a'..'z', 'A'..'Z', '0'..'9']);
end;

//----------------------------------------------------------------------------------------------------------------------

function _isdigit(c: Integer): Integer;

// Note: this code does not consider changes made with setlocale. Currently only Latin characters are checked.

begin
  Result := Ord(Char(c) in ['0'..'9']);
end;

//----------------------------------------------------------------------------------------------------------------------

function _isprint(c: Integer): Integer;

begin
  Result := Ord(Char(c) in [#$20..#$7E]);
end;

//----------------------------------------------------------------------------------------------------------------------

function _isxdigit(c: Integer): Integer;

// Note: this code does not consider changes made with setlocale. Currently only Latin characters are checked.

begin
  Result := Ord(Char(c) in ['a'..'f', 'A'..'F', '0'..'9']);
end;

//----------------------------------------------------------------------------------------------------------------------

function _ldexp(x: Double; exp: Integer): Double;

begin
  Result := Math.Ldexp(x, exp); // Different types but same name. Keep this stub!
end;

//----------------------------------------------------------------------------------------------------------------------

function _localtime(clock: PInteger): ptm;

begin
  if (clock^ < _timezone) then
    Result := nil
  else
    Result := comtime(clock^ - _timezone, True);
end;

//----------------------------------------------------------------------------------------------------------------------

function _log(Value: Double): Double;

// Returns natural logarithm of Value.

begin
  Result := LogN(2.7182818284590452353602874713527, Value);
end;

//----------------------------------------------------------------------------------------------------------------------

function _malloc(size: Integer): Pointer;

begin
  GetMem(Result, size);
end;

//----------------------------------------------------------------------------------------------------------------------

function _memcmp(s1, s2: Pointer; n: Integer): Integer;

begin
  Result := StrLComp(s1, s2, n);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure _memcpy(dest, source: Pointer; count: Integer);

begin
  Move(source^, dest^, count);
end;

//----------------------------------------------------------------------------------------------------------------------

function _memmove(dest, src: Pointer; n: Cardinal): Pointer;

begin
  Move(src^, dest^, n);
  Result := dest;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure _memset(P: Pointer; B: Integer; count: Integer);

begin
  FillChar(P^, count, B);
end;

//----------------------------------------------------------------------------------------------------------------------

function _pow(x, y: Double): Double;

begin
  Result := Power(x, y);
end;

//----------------------------------------------------------------------------------------------------------------------

const
  RAND_MAX = $7FFF; // Predefined constant in BCB.

function _rand: Integer;

begin
  Result := Random(RAND_MAX);
end;

//----------------------------------------------------------------------------------------------------------------------

// Quick sort support stuff.
// This is median-of-three quick sort variant.

threadvar
  Compare: cmp_callback;
  QWidth: Cardinal;

//----------------------------------------------------------------------------------------------------------------------

procedure Exchange(Left, Right: PChar);

var
  I: Cardinal;
  C: Char;

begin
  for I := 1 to qWidth do
  begin
    C := Right^;
    Right^ := Left^;
    Inc(Right);
    Left^ := C;
    Inc(Left);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure qSortHelp(pivotP: PChar; nElem: Cardinal);

label
  tailRecursion, qBreak;

var
  leftP,
  rightP,
  pivotEnd,
  pivotTemp,
  leftTemp: PChar;
  lNum: Cardinal;
  retval: Integer;

begin
  tailRecursion:

  if nElem <= 2 then
  begin
    if nElem = 2 then
    begin
      rightP := qWidth + pivotP;
      if Compare(pivotP, rightP) > 0 then
        Exchange(pivotP, rightP);
    end;
    System.Exit;
  end;

  rightP := (nElem - 1) * qWidth + pivotP;
  leftP  := (nElem shr 1) * qWidth + pivotP;

  // Sort the pivot, left, and right elements for "median of 3".
  if Compare(leftP, rightP) > 0 then
    Exchange (leftP, rightP);
  if Compare(leftP, pivotP) > 0 then
    Exchange (leftP, pivotP)
  else
    if Compare(pivotP, rightP) > 0 then
      Exchange (pivotP, rightP);

  if nElem = 3 then
  begin
    Exchange(pivotP, leftP);
    System.Exit;
  end;

  // Now for the classic Hoare algorithm.
  pivotEnd := pivotP + qWidth;
  leftP := pivotEnd;

  repeat
    while True do
    begin
      retval := Compare(leftP, pivotP);
      if retval > 0 then
        Break;
      if retval = 0 then
      begin
        Exchange(leftP, pivotEnd);
        Inc(pivotEnd, qWidth);
      end;
      if leftP < rightP then
        Inc(leftP, qWidth)
      else
        goto qBreak;
    end;  

    while leftP < rightP do
    begin
      retval := Compare(pivotP, rightP);
      if retval < 0 then
        Dec(rightP, qWidth)
      else
      begin
        Exchange(leftP, rightP);
        if retval <> 0 then
        begin
          Inc(leftP, qWidth);
          Dec(rightP, qWidth);
        end;
        Break;
      end;
    end;
  until leftP >= rightP;

  qBreak:

  if Compare(leftP, pivotP) <= 0 then
    leftP := leftP + qWidth;

  leftTemp := leftP - qWidth;

  pivotTemp := pivotP;

  while (pivotTemp < pivotEnd) and (leftTemp >= pivotEnd) do
  begin
    Exchange(pivotTemp, leftTemp);
    Inc(pivotTemp, qWidth);
    Dec(leftTemp, qWidth);
  end;
        
  lNum := Cardinal(leftP - pivotEnd) div qWidth;
  nElem := Cardinal((nElem * qWidth + pivotP) - leftP) div qWidth;

  // Sort smaller partition first to reduce stack usage.
  if nElem < lNum then
  begin
    qSortHelp(leftP, nElem);
    nElem := lNum;
  end
  else
  begin
    qSortHelp(pivotP, lNum);
    pivotP := leftP;
  end;
                
  goto tailRecursion;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure _qsort(base: Pointer; nelem, width: Cardinal; fcmp: cmp_callback);

begin
  if width > 0 then
  begin
    qWidth := width;
    Compare := fcmp;
    qSortHelp(base, nElem);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function _sin(Value: Double): Double;

begin
  Result := System.Sin(Value); // Different types but same name. Keep this stub!
end;

//----------------------------------------------------------------------------------------------------------------------

procedure _sprintf(Buffer, Format: PChar; Arguments: va_list);

// Optional parameters are passed in a va_list as the last parameter. 

begin
  wvsprintf(Buffer, Format, Arguments);
end;

//----------------------------------------------------------------------------------------------------------------------

function _sqrt(Value: Double): Double;

begin
  Result := System.Sqrt(Value); // Different types but same name. Keep this stub!
end;

//----------------------------------------------------------------------------------------------------------------------

function _sscanf(Buffer, Format: PChar; Argument: array of Pointer): Integer;

// Note: the actual implementation was provided by Evgeni Sorokin.

begin
  Result := Scanf.sscanf(Buffer, Format, Argument);
end;

//----------------------------------------------------------------------------------------------------------------------

function _strcat(dest, src: PChar): PChar;

begin
  Result := SysUtils.StrCat(dest, src);
end;

//----------------------------------------------------------------------------------------------------------------------

function _strchr(s: PChar; c: Integer): PChar;

begin
  Result := StrScan(s, Char(c));
end;

//----------------------------------------------------------------------------------------------------------------------

function _strcpy(dest, src: PChar): PChar;

begin
  Result := StrCopy(dest, src);
end;

//----------------------------------------------------------------------------------------------------------------------

function _strdup(s: PChar): PChar;

var
  L: Integer;

begin
  L := SysUtils.StrLen(s);
  GetMem(Result, L + 1);
  Move(s^, Result^, L);
  Result[L + 1] := #0;
end;

//----------------------------------------------------------------------------------------------------------------------

function _stricmp(s1, s2: PChar): Integer;

begin
  Result := SysUtils.StrIComp(s1, s2);
end;

//----------------------------------------------------------------------------------------------------------------------

function _strlen(s: PChar): Cardinal;

begin
  Result := SysUtils.StrLen(s);
end;

//----------------------------------------------------------------------------------------------------------------------

function _strncmp(s1, s2: PChar; maxlen: Cardinal): Integer;

begin
  Result := StrLComp(s1, s2, maxlen);
end;

//----------------------------------------------------------------------------------------------------------------------

function _strncpy(strDest, strSource: PChar; count: Cardinal): PChar;

var
  Len: Cardinal;

begin
  Len := StrLen(strSource);
  if count <= Len then
    StrLCopy(strDest, strSource, count)
  else
  begin
    StrLCopy(strDest, strSource, Len);
    FillChar((strDest + Len)^, count - Len, 0);
  end;
  Result := strDest;
end;

//----------------------------------------------------------------------------------------------------------------------

function _strstr(s1, s2: PChar): PChar;

begin
  Result := StrPos(s1, s2);
end;

//----------------------------------------------------------------------------------------------------------------------

function _strtod(s: PChar; endptr: PPChar): Double;

var
  Buffer: Extended;

begin
  // Note: this conversion is only precise if the string contains a valid number.
  //       No overflow or underflow is indicated (as documented by the BCB docs) nor
  //       will endptr ever contain the correct error position.
  if TextToFloat(s, Buffer, fvExtended) then
    Result := Buffer
  else
  begin
    Result := NAN;
    if Assigned(endptr) then
      endptr^ := s;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure _swab(__from, __to: PChar; __nbytes: Integer);

// Swaps a number of words in __from to the opposite byte order and stores it in __to.
// Areas may overlap, in which case the conversion is done in-place.
// __nbytes must be even.

begin
  while __nbytes > 0 do
  begin
    PWord(__to)^  := Swap(PWord(__from)^);
    Inc(__from, 2);
    Inc(__to, 2);
    Dec(__nbytes, 2);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function _tan(Value: Double): Double;

begin
  Result := Math.Tan(Value); // Different types but same name. Keep this stub!
end;

//----------------------------------------------------------------------------------------------------------------------

function _time(__timer: Ptime_t): time_t;

var
  DelphiTime: TDateTime;

begin
  DelphiTime := SysUtils.Time;
  Result := DateTimeToUnix(DelphiTime);
  if Assigned(__timer) then
    __timer^ := Result;
end;

//----------------------------------------------------------------------------------------------------------------------

function _unlink(FileName: PChar): Integer;

begin
  if DeleteFile(FileName) then
    Result := 0
  else
    Result := GetLastError;
end;

//----------------------------------------------------------------------------------------------------------------------

function _vfprintf(Stream: TStream; Format: PChar; Arguments: va_list): Integer;

var
  Buffer: array[0..10000] of Char;

begin
  wvsprintf(Buffer, Format, Arguments);
  Result := StrLen(Buffer);
  Stream.Write(Buffer, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function _vprintf(Format: PChar; Arguments: va_list): Integer;

// In the C RTL this method writes to stdout, which should not be used for Win GUI applications.
// Hence we write a record to the debug output.

var
  Buffer: array[0..10000] of Char;

begin
  wvsprintf(Buffer, Format, Arguments);
  OutputDebugString(Buffer);
  OutputDebugString(#13#10);
  Result := StrLen(Buffer);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure _vsprintf(Buffer, Format: PChar; Arguments: va_list);

begin
  wvsprintf(Buffer, Format, Arguments);
end;

//----------------------------------------------------------------------------------------------------------------------

function _wcscpy(Destination, Source: PWideChar): PWideChar;

begin
  Move(Source^, Destination^, 2 * StrLenW(Source) + 2);
  Result := Destination;
end;

//----------------------------------------------------------------------------------------------------------------------

function _wcstombs(mbstr: PChar; wcstr: PWideChar; count: Cardinal): Cardinal;

begin
  Result := WideCharToMultibyte(CP_ACP, 0, wcstr, -1, mbstr, -1, nil, nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure _fprintf;

begin
  raise Exception.Create('Fatal error: stub function fprintf cannot be executed.');
end;

//----------------------------------------------------------------------------------------------------------------------

var
  Info: TTimezoneInformation;

begin
  GetTimezoneInformation(Info);
  _daylight := Info.DaylightBias <> 0;
  _timezone := (Info.Bias + Info.StandardBias) * 60;
end.

