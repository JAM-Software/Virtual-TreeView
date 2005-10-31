unit StringContainers;

//----------------------------------------------------------------------------------------------------------------------
// StringContainers contains a collection of special classes for string storage and manipulation.
//
// The hash trie implementation is based on work of Andre N Belokon, SoftLab.
//
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
//
// The original code is StringContainers.pas, released 1. January 2004.
//
// The initial developer of the original code is:
//   Mike Lischke, Delphi Gems software solutions (support@delphi-gems.com, www.delphi-gems.com).
//
// October 2004
// - Initial implementation:
//
//----------------------------------------------------------------------------------------------------------------------
//
interface

{$I Compilers.inc}

{$ifdef COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CAST off}
  {$warn UNSAFE_CODE off}
{$endif COMPILER_7_UP}

uses
  Windows, Classes;

const
  BufferIncrement = 1024; // Size by which the buffer in the buffered string is incremented
                          // when the current space is exhausted.

type
  // Class to speed up constructing result strings from large content.
  TBufferedString = class
  private
    FStart,
    FPosition,
    FEnd: PWideChar;
    function GetAsString: WideString;
  public
    destructor Destroy; override;

    procedure Add(S: PWideChar; Count: Integer); overload;
    procedure Add(const S: WideString); overload;
    procedure AddNewLine;

    property AsString: WideString read GetAsString;
  end;

// Hash trie implementation.
const
  // LeafSize must be 256. No changes allowed.
  LeafSize = 256;
  // BucketSize determines max length of the list. Very big|small values decrease performance, while
  // the optimum value in range 4..16.
  BucketSize = 8;

type
  THashLinkedItem = class(TObject)
  private
    FValue: Cardinal;
    FData: Pointer;
    FNext: THashLinkedItem;
  public
    constructor Create(Value: Cardinal; Data: Pointer; Next: THashLinkedItem);
    destructor Destroy; override;
  end;

  THashTrie = class;

  TTraverseProc = procedure(UserData, UserProc: Pointer; Value: Cardinal; Data: Pointer; var Done: Boolean) of object;

  THashTreeItem = class(TObject)
  private
    FOwner: THashTrie;
    FLevel: Integer;
    FFilled: Integer;
    FItems: array of TObject; // This will be at most LeafSize entries.
  protected
    procedure AddDown(Value, Hash: Cardinal; const Data: Pointer);
    procedure Delete(Value, Hash: Cardinal);
    function Find(Value, Hash: Cardinal; var Data: Pointer): Boolean;
    function GetFilled: Integer;
    function Modify(Value, Hash: Cardinal; const Data: Pointer): Boolean;
    function ROR(Value: Cardinal): Cardinal;
    function RORN(Value: Cardinal; Level: Integer): Cardinal;
    function Traverse(UserData, UserProc: Pointer; TraverseProc: TTraverseProc): Boolean;
  public
    constructor Create(AOwner: THashTrie);
    destructor Destroy; override;

    procedure Clear;
  end;

  THashTrie = class(TObject)
  private
    FRoot: THashTreeItem;
    function GetCount: Integer;
  protected
    procedure AddDown(Value, Hash: Cardinal; const Data: Pointer);
    function CompareValue(Value1, Value2: Cardinal): Boolean; virtual; abstract;
    procedure Delete(Value, Hash: Cardinal);
    procedure DestroyItem(var Value: Cardinal; var Data: Pointer); virtual; abstract;
    function HashValue(Value: Cardinal): Cardinal; virtual; abstract;
    procedure Traverse(UserData, UserProc: Pointer; TraverseProc: TTraverseProc);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear;
    function Find(Value, Hash: Cardinal; var Data: Pointer): Boolean; overload;

    property Count: Integer read GetCount;
  end;

  TStringHashTrie = class;

  TStrHashTraverseProc = procedure(UserData: Pointer; const Value: string; Data: Pointer; var Done: Boolean);
  TStrHashTraverseMeth = procedure(UserData: Pointer; const Value: string; Data: Pointer; var Done: Boolean) of object;

  TSHTFreeItemEvent = procedure(Sender: TStringHashTrie; const S: string; const Data: Pointer) of object;

  TStringHashTrie = class(THashTrie)
  private
    FCaseSensitive: Boolean;
    FOnFreeItem: TSHTFreeItemEvent;
  protected
    function HashValue(Value: Cardinal): Cardinal; override;
    procedure DestroyItem(var Value: Cardinal; var Data: Pointer); override;
    function CompareValue(Value1, Value2: Cardinal): Boolean; override;
    function HashStr(const S: string): Cardinal;
    procedure TraverseProc(UserData, UserProc: Pointer; Value: Cardinal; Data: Pointer; var Done: Boolean);
    procedure TraverseMeth(UserData, UserProc: Pointer; Value: Cardinal; Data: Pointer; var Done: Boolean);
  public
    procedure Add(const S: string; const Data: Pointer);
    procedure Delete(const S: string);
    function Find(const S: string; var Data: Pointer): Boolean; overload;
    procedure Traverse(UserData: Pointer; UserProc: TStrHashTraverseProc); overload;
    procedure Traverse(UserData: Pointer; UserProc: TStrHashTraverseMeth); overload;

    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default False;

    property OnFreeItem: TSHTFreeItemEvent read FOnFreeItem write FOnFreeItem;
  end;

  // Support for trie statistics.
  TLengthStatistics = array[1..BucketSize] of Integer;

function CalcStrCRC32(const S: string): Cardinal;
function JHash(Key: Pointer; Length, InitVal: Cardinal): Cardinal;
procedure TrieStatistics(Trie: THashTrie; var MaxLevel, PeakCount, FillCount, EmptyCount: Integer;
  var LengthStatistics: TLengthStatistics);

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  {$ifdef Compiler_6_UP}
    RTLConsts,
  {$else}
    Consts,
  {$endif Compiler_6_UP}
  SysUtils, Math, Variants;

const
  CRC32_POLYNOMIAL = $EDB88320;

var
  // Dynamic crc32 table.
  CCITT32Table: array of Cardinal;

procedure BuildCRCTable;

var
  i, j: longint;
  value: Cardinal;
begin
  SetLength(CCITT32Table, 256);
  for i := 0 to 255 do begin
    value := i;
    for j := 8 downto 1 do
      if ((value and 1) <> 0) then
        value := (value shr 1) xor CRC32_POLYNOMIAL
      else
        value := value shr 1;
    Ccitt32Table[i] := value;
  end
end;

//----------------------------------------------------------------------------------------------------------------------

function CalcStrCRC32(const S: string): Cardinal;

var
  I: Integer;

begin
  // Create CRC table if not yet done.
  if CCITT32Table = nil then
    BuildCRCTable;

  Result := $FFFFFFFF;
  for I:=1 to Length(S) do
    Result:= (((Result shr 8) and $00FFFFFF) xor (CCITT32Table[(Result xor Byte(S[I])) and $FF]));
end;

//----------------------------------------------------------------------------------------------------------------------

// By Bob Jenkins, 1996.  bob_jenkins@burtleburtle.net
//
// If you are hashing n strings (ub1 **)k, do it like this:
//   for (i=0, h=0; i<n; ++i) h = jhash( k[i], len[i], h);

procedure Mix(var A, B, C: Cardinal);

begin
  Dec(A, B); Dec(A, C); A := A xor (C shr 13);
  Dec(B, C); Dec(B, A); B := A xor (A shl 8);
  Dec(C, A); Dec(C, B); C := C xor (B shr 13);
  Dec(A, B); Dec(A, C); A := A xor (C shr 12);
  Dec(B, C); Dec(B, A); B := B xor (A shl 16);
  Dec(C, A); Dec(C, B); C := C xor (B shr 5);
  Dec(A, B); Dec(A, C); A := A xor (C shr 3);
  Dec(B, C); Dec(B, A); B := B xor (A shl 10);
  Dec(C, A); Dec(C, B); C := C xor (B shr 15);
end;

//----------------------------------------------------------------------------------------------------------------------

function JHash(Key: Pointer; Length, InitVal: Cardinal): Cardinal;

// Length: the length of the key.
// InitVal: the previous hash, or an arbitrary value.

var
  A, B, C, Len: Cardinal;
  K: PByteArray;

begin
  // Set up the internal state.
  Len := Length;
  K := Key;
  A := $9E3779B9;  // The golden ratio; an arbitrary value.
  B := $9E3779B9;
  C := InitVal;    // The previous hash value.

  // Handle most of the key.
  while Len >= 12 do
  begin
    Inc(A, K[0] + (Cardinal(K[1]) shl 8) + (Cardinal(K[2]) shl 16) + (Cardinal(K[3]) shl 24));
    Inc(B, K[4] +(Cardinal(K[5]) shl 8) + (Cardinal(K[6]) shl 16) + (Cardinal(K[7]) shl 24));
    Inc(C, K[8] + (Cardinal(K[9]) shl 8) + (Cardinal(K[10]) shl 16) + (Cardinal(K[11]) shl 24));
    Mix(A, B, C);
    Inc(PByte(K), 12);
    Dec(Len, 12);
  end;

   // Handle the last 11 bytes.
  Inc(C, Length);
  if Len >= 11 then
    Inc(C, Cardinal(K[10]) shl 24);
  if Len >= 10 then
    Inc(C, Cardinal(K[9]) shl 16);
  if Len >= 9 then
    Inc(C, Cardinal(K[8]) shl 8);
  if Len >= 8 then
    Inc(B, Cardinal(K[7]) shl 24);
  if Len >= 7 then
    Inc(B, Cardinal(K[6]) shl 16);
  if Len >= 6 then
    Inc(B, Cardinal(K[5]) shl 8);
  if Len >= 5 then
    Inc(B, Cardinal(K[4]));
  if Len >= 4 then
    Inc(A, Cardinal(K[3]) shl 24);
  if Len >= 3 then
    Inc(A, Cardinal(K[2]) shl 16);
  if Len >= 2 then
    Inc(A, Cardinal(K[1]) shl 8);
  if Len >= 1 then
    Inc(A, Cardinal(K[0]));
  // Case 0: nothing left to add.

  Mix(A, B, C);
  Result := C;
end;

//----------------- TBufferedString --------------------------------------------------------------------------------

destructor TBufferedString.Destroy;

begin
  FreeMem(FStart);
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBufferedString.GetAsString: WideString;

begin
  SetString(Result, FStart, FPosition - FStart);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBufferedString.Add(S: PWideChar; Count: Integer);

var
  LastLen,
  LastOffset,
  AllocIncrement: Integer;

begin
  // Make room for the new string.
  if FEnd - FPosition <= Count then
  begin
    // Keep last offset to restore it correctly in the case that FStart gets a new memory block assigned.
    LastLen := FEnd - FStart;
    LastOffset := FPosition - FStart;
    AllocIncrement := Max(Count, BufferIncrement);
    ReallocMem(FStart, 2 * (FEnd - FStart + AllocIncrement));
    FPosition := FStart + LastOffset;
    FEnd := FStart + LastLen + AllocIncrement;
  end;                     
  Move(S^, FPosition^, 2 * Count);
  Inc(FPosition, Count);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBufferedString.Add(const S: WideString);

var
  LastLen,
  LastOffset,
  Len: Integer;
  AllocIncrement: Integer;

begin
  Len := Length(S);
  // Make room for the new string.
  if FEnd - FPosition <= Len then
  begin
    // Keep last offset to restore it correctly in the case that FStart gets a new memory block assigned.
    LastLen := FEnd - FStart;
    LastOffset := FPosition - FStart;
    AllocIncrement := Max(Len, BufferIncrement);
    ReallocMem(FStart, 2 * (FEnd - FStart + AllocIncrement));
    FPosition := FStart + LastOffset;
    FEnd := FStart + LastLen + AllocIncrement;
  end;                     
  Move(PWideChar(S)^, FPosition^, 2 * Len);
  Inc(FPosition, Len);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBufferedString.AddNewLine;

var
  LastLen,
  LastOffset: Integer;

begin
  // Make room for the CR/LF characters.
  if FEnd - FPosition <= 4 then
  begin
    // Keep last offset to restore it correctly in the case that FStart gets a new memory block assigned.
    LastLen := FEnd - FStart;
    LastOffset := FPosition - FStart;
    ReallocMem(FStart, 2 * (FEnd - FStart + BufferIncrement));
    FPosition := FStart + LastOffset;
    FEnd := FStart + LastLen + BufferIncrement;
  end;
  FPosition^ := #13;
  Inc(FPosition);
  FPosition^ := #10;
  Inc(FPosition);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TrieStatistics(Trie: THashTrie; var MaxLevel, PeakCount, FillCount, EmptyCount: Integer;
  var LengthStatistics: TLengthStatistics);

  //--------------- local function --------------------------------------------

  procedure TreeStat(Item: THashTreeItem);

  var
    I, J: Integer;
    LinkedItem: THashLinkedItem;
    
  begin
    Inc(PeakCount);
    if Item.FLevel + 1 > MaxLevel then
      MaxLevel := Item.FLevel + 1;

    for J := 0 to High(Item.FItems) do
      if Assigned(Item.FItems[J]) then
      begin
        Inc(FillCount);
        if Item.FItems[J] is THashTreeItem then
          TreeStat(THashTreeItem(Item.FItems[J]))
        else
        begin
          I := 0;
          LinkedItem := THashLinkedItem(Item.FItems[J]);
          while Assigned(LinkedItem) do
          begin
            Inc(I);
            LinkedItem := LinkedItem.FNext;
          end;
          Inc(LengthStatistics[I]);
        end;
      end
      else
        Inc(EmptyCount);
  end;

  //--------------- end local function ----------------------------------------

begin
  MaxLevel := 0;
  PeakCount := 0;
  FillCount := 0;
  EmptyCount := 0;
  
  if Assigned(Trie.FRoot) then
    TreeStat(Trie.FRoot);
end;

//----------------- THashTreeItem --------------------------------------------------------------------------------------

constructor THashTreeItem.Create(AOwner: THashTrie);

begin
  FOwner := AOwner;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor THashTreeItem.Destroy;

begin
  Clear;
  
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THashTreeItem.Clear;

var
  I: Integer;
  LinkedItem: THashLinkedItem;
  
begin
  for I := 0 to High(FItems) do
    if FItems[I] is THashTreeItem then
      THashTreeItem(FItems[I]).Free
    else
    begin
      LinkedItem := THashLinkedItem(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        FOwner.DestroyItem(LinkedItem.FValue, LinkedItem.FData);
        LinkedItem := LinkedItem.FNext;
      end;
      THashLinkedItem(FItems[I]).Free;
    end;
  FItems := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THashTreeItem.AddDown(Value, Hash: Cardinal; const Data: Pointer);

var
  I, J: Integer;
  TreeItem: THashTreeItem;
  LinkedItem: THashLinkedItem;

begin
  I := Hash and $FF;
  if High(FItems) < I then
    SetLength(FItems, I + 1);
  if FItems[I] = nil then
  begin
    FItems[I] := THashLinkedItem.Create(Value, Data, nil);
    Inc(FFilled);
  end
  else
    if FItems[I] is THashTreeItem then
      THashTreeItem(FItems[I]).AddDown(Value, ROR(Hash), Data)
    else
    begin
      J := 0;
      LinkedItem := THashLinkedItem(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        if FOwner.CompareValue(LinkedItem.FValue, Value) then
        begin
          // found
          LinkedItem.FData := Data;
          Exit;
        end;
        LinkedItem := LinkedItem.FNext;
        Inc(J)
      end;

      if J >= BucketSize then
      begin
        // full
        TreeItem := THashTreeItem.Create(FOwner);
        TreeItem.FLevel := FLevel + 1;
        LinkedItem := THashLinkedItem(FItems[I]);
        while Assigned(LinkedItem) do
        begin
          TreeItem.AddDown(LinkedItem.FValue, RORN(FOwner.HashValue(LinkedItem.FValue), FLevel + 1), LinkedItem.FData);
          LinkedItem := LinkedItem.FNext;
        end;
        TreeItem.AddDown(Value, ROR(Hash), Data);
        THashLinkedItem(FItems[I]).Free;
        FItems[I] := TreeItem;
      end
      else
        FItems[I] := THashLinkedItem.Create(Value, Data, THashLinkedItem(FItems[I]));
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THashTreeItem.Delete(Value, Hash: Cardinal);

var
  I: Integer;
  PrevLinkedItem,
  LinkedItem: THashLinkedItem;

begin
  I := Hash and $FF;
  if High(FItems) < I then
    SetLength(FItems, I + 1);
  if Assigned(FItems[I]) then
  begin
    if FItems[i] is THashTreeItem then
    begin
      THashTreeItem(FItems[I]).Delete(Value, ROR(Hash));
      if THashTreeItem(FItems[I]).FFilled = 0 then
      begin
        THashTreeItem(FItems[I]).Free;
        FItems[I] := nil;
      end;
    end
    else
    begin
      PrevLinkedItem := nil;
      LinkedItem := THashLinkedItem(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        if FOwner.CompareValue(LinkedItem.FValue, Value) then
        begin
          // found
          if PrevLinkedItem = nil then
          begin
            FItems[I] := LinkedItem.FNext;
            if FItems[I] = nil then
              Dec(FFilled);
          end
          else
            PrevLinkedItem.FNext := LinkedItem.FNext;
          LinkedItem.FNext := nil;
          FOwner.DestroyItem(LinkedItem.FValue, LinkedItem.FData);
          LinkedItem.Free;
          Exit;
        end;
        PrevLinkedItem := LinkedItem;
        LinkedItem := LinkedItem.FNext;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function THashTreeItem.Find(Value, Hash: Cardinal; var Data: Pointer): Boolean;

var
  I: Integer;
  LinkedItem: THashLinkedItem;

begin
  Result := False;
  I := Hash and $FF;
  if High(FItems) < I then
    SetLength(FItems, I + 1);
  if Assigned(FItems[I]) then
  begin
    if FItems[I] is THashTreeItem then
      Result := THashTreeItem(FItems[I]).Find(Value, ROR(Hash), Data)
    else
    begin
      LinkedItem := THashLinkedItem(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        if FOwner.CompareValue(LinkedItem.FValue, Value) then
        begin
          // found
          Data := LinkedItem.FData;
          Result := True;
          Exit;
        end;
        LinkedItem := LinkedItem.FNext;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function THashTreeItem.GetFilled: Integer;

var
  I: Integer;
  LinkedItem: THashLinkedItem;

begin
  Result := 0;
  for I := 0 to High(FItems) do
    if FItems[I] is THashTreeItem then
      Inc(Result, THashTreeItem(FItems[I]).GetFilled)
    else
    begin
      LinkedItem := THashLinkedItem(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        Inc(Result);
        LinkedItem := LinkedItem.FNext;
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function THashTreeItem.Modify(Value, Hash: Cardinal; const Data: Pointer): Boolean;

var
  I: Integer;
  LinkedItem: THashLinkedItem;

begin
  Result := False;
  I := Hash and $FF;
  if High(FItems) < I then
    SetLength(FItems, I + 1);
  if Assigned(FItems[I]) then
  begin
    if FItems[I] is THashTreeItem then
      Result := THashTreeItem(FItems[I]).Modify(Value, ROR(Hash), Data)
    else
    begin
      LinkedItem := THashLinkedItem(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        if FOwner.CompareValue(LinkedItem.FValue, Value) then
        begin
          // found
          LinkedItem.FData := Data;
          Result := True;
          Exit;
        end;
        LinkedItem := LinkedItem.FNext;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function THashTreeItem.ROR(Value: Cardinal): Cardinal;

begin
  Result := ((Value and $FF) shl 24) or ((Value shr 8) and $FFFFFF);
end;

//----------------------------------------------------------------------------------------------------------------------

function THashTreeItem.RORN(Value: Cardinal; Level: Integer): Cardinal;

begin
  Result := Value;
  while Level > 0 do
  begin
    Result := ROR(Result);
    Dec(Level);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function THashTreeItem.Traverse(UserData, UserProc: Pointer; TraverseProc: TTraverseProc): Boolean;

var
  I: Integer;
  LinkedItem: THashLinkedItem;

begin
  Result := False;
  for I := 0 to High(FItems) do
    if Assigned(FItems[I]) then
    begin
      if FItems[I] is THashTreeItem then
        Result := THashTreeItem(FItems[I]).Traverse(UserData, UserProc, TraverseProc)
      else
      begin
        LinkedItem := THashLinkedItem(FItems[I]);
        while Assigned(LinkedItem) do
        begin
          TraverseProc(UserData, UserProc, LinkedItem.FValue, LinkedItem.FData, Result);
          LinkedItem := LinkedItem.FNext;
        end;
      end;
      if Result then
        Break;
    end;
end;

//----------------- THashLinkedItem ------------------------------------------------------------------------------------

constructor THashLinkedItem.Create(Value: Cardinal; Data: Pointer; Next: THashLinkedItem);

begin
  FValue := Value;
  FData := Data;
  FNext := Next;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor THashLinkedItem.Destroy;

begin
  FNext.Free;
end;

//----------------- THashTrei ------------------------------------------------------------------------------------------

constructor THashTrie.Create;

begin
  inherited;

  FRoot := THashTreeItem.Create(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor THashTrie.Destroy;

begin
  FRoot.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THashTrie.Clear;

begin
  FRoot.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

function THashTrie.Find(Value, Hash: Cardinal; var Data: Pointer): Boolean;

begin
  Result := FRoot.Find(Value, Hash, Data);
end;

//----------------------------------------------------------------------------------------------------------------------

function THashTrie.GetCount: Integer;

begin
  Result := FRoot.GetFilled;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THashTrie.AddDown(Value, Hash: Cardinal; const Data: Pointer);

begin
  FRoot.AddDown(Value, Hash, Data);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THashTrie.Delete(Value, Hash: Cardinal);

begin
  FRoot.Delete(Value, Hash);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THashTrie.Traverse(UserData, UserProc: Pointer; TraverseProc: TTraverseProc);

begin
  FRoot.Traverse(UserData, UserProc, TraverseProc);
end;

//----------------- TStringHashTrie ------------------------------------------------------------------------------------

procedure TStringHashTrie.Add(const S: string; const Data: Pointer);

var
  Value: PChar;

begin
  Value := StrNew(PChar(S));
  AddDown(Cardinal(Value), HashStr(S), Data);
end;

//----------------------------------------------------------------------------------------------------------------------

function TStringHashTrie.CompareValue(Value1, Value2: Cardinal): Boolean;

begin
  if FCaseSensitive then
    Result := StrComp(PChar(Value1), PChar(Value2)) = 0
  else
    Result := StrIComp(PChar(Value1), PChar(Value2)) = 0
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStringHashTrie.Delete(const S: string);

begin
  inherited Delete(Cardinal(@S), HashStr(S));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStringHashTrie.DestroyItem(var Value: Cardinal; var Data: Pointer);

begin
  if Assigned(FOnFreeItem) then
    FOnFreeItem(Self, PChar(Value), Data);

  StrDispose(PChar(Value));

  Value := 0;
  Data := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function TStringHashTrie.Find(const S: string; var Data: Pointer): Boolean;

begin
  Result := Find(Cardinal(PChar(S)), HashStr(S), Data);
end;

//----------------------------------------------------------------------------------------------------------------------

function TStringHashTrie.HashStr(const S: string): Cardinal;
                                                                                                     
begin
  if FCaseSensitive then
    Result := CalcStrCRC32(S)
  else
    Result := CalcStrCRC32(ANSIUpperCase(S));
end;

//----------------------------------------------------------------------------------------------------------------------

function TStringHashTrie.HashValue(Value: Cardinal): Cardinal;

begin
  Result := HashStr(PChar(Value));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStringHashTrie.Traverse(UserData: Pointer; UserProc: TStrHashTraverseProc);

begin
  inherited Traverse(UserData, @UserProc, TraverseProc);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStringHashTrie.TraverseProc(UserData, UserProc: Pointer; Value: Cardinal; Data: Pointer; var Done: Boolean);

begin
  TStrHashTraverseProc(UserProc)(UserData, PChar(Value), Data, Done);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStringHashTrie.Traverse(UserData: Pointer; UserProc: TStrHashTraverseMeth);

begin
  inherited Traverse(UserData, @TMethod(UserProc), TraverseMeth);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStringHashTrie.TraverseMeth(UserData, UserProc: Pointer; Value: Cardinal; Data: Pointer; var Done: Boolean);

type
  PTStrHashTraverseMeth = ^TStrHashTraverseMeth;

begin
  PTStrHashTraverseMeth(UserProc)^(UserData, PChar(Value), Data, Done);
end;

//----------------------------------------------------------------------------------------------------------------------

end.

