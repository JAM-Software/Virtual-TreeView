unit UCECPPHighlighter;

//----------------------------------------------------------------------------------------------------------------------
//
// UniCodeEditor, a Unicode Source Code Editor for Delphi.
//
// UniCodeEditor is released under the MIT license:
// Copyright (c) 1999-2004 Mike Lischke (support@soft-gems.net, www.soft-gems.net).
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
// Description
//   Original version of a C/C++ highlighter contained in mwEdit with some minor changes to make it
//   compatible to TSyntaxEdit. Will be replaced by a Unicode capable highlighter class.
//
// Changes:
//   Version 2.0, 2003-08-17, Mike Zinner
//     Repackaged for D7
//   Version 1.0, 1999-03-10, Mike Lischke
//     Initial Version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses Windows, SysUtils, Messages, Classes, Controls, Graphics, Registry,
  UCEHighlighter;

const
  tkAsm = 257;
  tkComment = 258;
  tkDirective = 259;
  tkIdentifier = 260;
  tkKey = 261;
  tkNull = 262;
  tkNumber = 263;
  tkSpace = 264;
  tkString = 265;
  tkSymbol = 266;
  tkUnknown = 267;

type
  TxtkTokenKind = (
    xtkAdd, xtkAddAssign, xtkAnd, xtkAndAssign, xtkArrow, xtkAssign,
    xtkBitComplement, xtkBraceClose, xtkBraceOpen, xtkColon, xtkComma,
    xtkDecrement, xtkDivide, xtkDivideAssign, xtkEllipse, xtkGreaterThan,
    xtkGreaterThanEqual, xtkIncOr, xtkIncOrAssign, xtkIncrement, xtkLessThan,
    xtkLessThanEqual, xtkLogAnd, xtkLogComplement, xtkLogEqual, xtkLogOr,
    xtkMod, xtkModAssign, xtkMultiplyAssign, xtkNotEqual, xtkPoint, xtkQuestion,
    xtkRoundClose, xtkRoundOpen, xtkScopeResolution, xtkSemiColon, xtkShiftLeft,
    xtkShiftLeftAssign, xtkShiftRight, xtkShiftRightAssign, xtkSquareClose,
    xtkSquareOpen, xtkStar, xtkSubtract, xtkSubtractAssign, xtkXor,
    xtkXorAssign);

  TRangeState = (rsANil, rsAnsiC, rsAnsiCAsm, rsAnsiCAsmBlock, rsAsm, rsAsmBlock, rsUnKnown);

  TProcTableProc = procedure of object;
  TIdentFuncTableFunc = function: Integer of object;

  TUCECPPHighlighter = class(TUCEHighlighter)
  private
    FAsmStart: Boolean;
    FRange: TRangeState;
    FLine: PChar;
    FProcTable: array[Char] of TProcTableProc;
    Run: LongInt;
    FRoundCount: Integer;
    FSquareCount: Integer;
    FStringLen: Integer;
    FToIdent: PChar;
    FTokenPos: Integer;
    FTokenID: Integer;
    FExtTokenID: TxtkTokenKind;
    FEol: Boolean;
    FIdentFuncTable: array[0..206] of TIdentFuncTableFunc;

    FAsmAttributes: THighLightAttributes;
    FCommentAttributes: THighLightAttributes;
    FDirecAttributes: THighLightAttributes;
    FIdentifierAttributes: THighLightAttributes;
    FInvalidAttributes: THighLightAttributes;
    FKeyAttributes: THighLightAttributes;
    FNumberAttributes: THighLightAttributes;
    FSpaceAttributes: THighLightAttributes;
    FStringAttributes: THighLightAttributes;
    FSymbolAttributes: THighLightAttributes;

    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(aKey: string): Boolean;
    function Func17: Integer;
    function Func21: Integer;
    function Func32: Integer;
    function Func34: Integer;
    function Func36: Integer;
    function Func40: Integer;
    function Func42: Integer;
    function Func45: Integer;
    function Func46: Integer;
    function Func48: Integer;
    function Func52: Integer;
    function Func54: Integer;
    function Func57: Integer;
    function Func58: Integer;
    function Func59: Integer;
    function Func60: Integer;
    function Func61: Integer;
    function Func62: Integer;
    function Func64: Integer;
    function Func65: Integer;
    function Func66: Integer;
    function Func67: Integer;
    function Func68: Integer;
    function Func69: Integer;
    function Func71: Integer;
    function Func74: Integer;
    function Func75: Integer;
    function Func76: Integer;
    function Func78: Integer;
    function Func79: Integer;
    function Func81: Integer;
    function Func82: Integer;
    function Func85: Integer;
    function Func86: Integer;
    function Func88: Integer;
    function Func89: Integer;
    function Func92: Integer;
    function Func97: Integer;
    function Func98: Integer;
    function Func100: Integer;
    function Func101: Integer;
    function Func102: Integer;
    function Func104: Integer;
    function Func105: Integer;
    function Func106: Integer;
    function Func107: Integer;
    function Func109: Integer;
    function Func110: Integer;
    function Func115: Integer;
    function Func116: Integer;
    function Func123: Integer;
    function Func125: Integer;
    function Func141: Integer;
    function Func206: Integer;

    procedure AnsiCProc;

    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure AtSymbolProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure DirectiveProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure QuestionProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StarProc;
    procedure StringProc;
    procedure TildeProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
    function AltFunc: Integer;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): Integer;
    procedure MakeMethodTables;
    procedure HighLightChange(Sender: TObject);
    procedure SetHighLightChange;
  protected
    function GetIdentChars: TIdentChars; override;
    function GetLanguageName: string; override;

    function GetAttributeCount: Integer; override;
    function GetAttribute(Index: Integer): THighLightAttributes; override;
    function GetCapabilities: THighlighterCapabilities; override;

    function GetExtTokenID: TxtkTokenKind;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EOL: Boolean; override;
    function GetRange: Integer; override;
    function GetTokenID: Integer;
    procedure SetLine(const NewValue: string); override;
    function GetToken: string; override;
    function GetTokenInfo: TTokenData; override;
    procedure Next; override;
    procedure SetRange(Value: Integer); override;
    procedure ResetRange; override;
    function UseUserSettings(Path: string): Boolean; override;
    procedure EnumUserSettings(Settings: TStrings); override;
    property IdentChars;
    property LanguageName;
    property AttributeCount;
    property Attribute;
    property Capabilities;

    property ExtTokenID: TxtkTokenKind read GetExtTokenID;
  published
    property AsmAttributes: THighLightAttributes read FAsmAttributes write FAsmAttributes;
    property CommentAttributes: THighLightAttributes read FCommentAttributes write FCommentAttributes;
    property DirecAttributes: THighLightAttributes read FDirecAttributes write FDirecAttributes;
    property IdentifierAttributes: THighLightAttributes read FIdentifierAttributes write FIdentifierAttributes;
    property InvalidAttributes: THighLightAttributes read FInvalidAttributes write FInvalidAttributes;
    property KeyAttributes: THighLightAttributes read FKeyAttributes write FKeyAttributes;
    property NumberAttributes: THighLightAttributes read FNumberAttributes write FNumberAttributes;
    property SpaceAttributes: THighLightAttributes read FSpaceAttributes write FSpaceAttributes;
    property StringAttributes: THighLightAttributes read FStringAttributes write FStringAttributes;
    property SymbolAttributes: THighLightAttributes read FSymbolAttributes write FSymbolAttributes;
  end;

//--------------------------------------------------------------------------------

implementation

var
  Identifiers: array[Char] of ByteBool;
  HashTable: array[Char] of Integer;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.InitIdent;

var
  I: Integer;

begin
  for I := 0 to 206 do
    case I of
      17: FIdentFuncTable[I] := Func17;
      21: FIdentFuncTable[I] := Func21;
      32: FIdentFuncTable[I] := Func32;
      34: FIdentFuncTable[I] := Func34;
      36: FIdentFuncTable[I] := Func36;
      40: FIdentFuncTable[I] := Func40;
      42: FIdentFuncTable[I] := Func42;
      45: FIdentFuncTable[I] := Func45;
      46: FIdentFuncTable[I] := Func46;
      48: FIdentFuncTable[I] := Func48;
      52: FIdentFuncTable[I] := Func52;
      54: FIdentFuncTable[I] := Func54;
      57: FIdentFuncTable[I] := Func57;
      58: FIdentFuncTable[I] := Func58;
      59: FIdentFuncTable[I] := Func59;
      60: FIdentFuncTable[I] := Func60;
      61: FIdentFuncTable[I] := Func61;
      62: FIdentFuncTable[I] := Func62;
      64: FIdentFuncTable[I] := Func64;
      65: FIdentFuncTable[I] := Func65;
      66: FIdentFuncTable[I] := Func66;
      67: FIdentFuncTable[I] := Func67;
      68: FIdentFuncTable[I] := Func68;
      69: FIdentFuncTable[I] := Func69;
      71: FIdentFuncTable[I] := Func71;
      74: FIdentFuncTable[I] := Func74;
      75: FIdentFuncTable[I] := Func75;
      76: FIdentFuncTable[I] := Func76;
      78: FIdentFuncTable[I] := Func78;
      79: FIdentFuncTable[I] := Func79;
      81: FIdentFuncTable[I] := Func81;
      82: FIdentFuncTable[I] := Func82;
      85: FIdentFuncTable[I] := Func85;
      86: FIdentFuncTable[I] := Func86;
      88: FIdentFuncTable[I] := Func88;
      89: FIdentFuncTable[I] := Func89;
      92: FIdentFuncTable[I] := Func92;
      97: FIdentFuncTable[I] := Func97;
      98: FIdentFuncTable[I] := Func98;
      100: FIdentFuncTable[I] := Func100;
      101: FIdentFuncTable[I] := Func101;
      102: FIdentFuncTable[I] := Func102;
      104: FIdentFuncTable[I] := Func104;
      105: FIdentFuncTable[I] := Func105;
      106: FIdentFuncTable[I] := Func106;
      107: FIdentFuncTable[I] := Func107;
      109: FIdentFuncTable[I] := Func109;
      110: FIdentFuncTable[I] := Func110;
      115: FIdentFuncTable[I] := Func115;
      116: FIdentFuncTable[I] := Func116;
      123: FIdentFuncTable[I] := Func123;
      125: FIdentFuncTable[I] := Func125;
      141: FIdentFuncTable[I] := Func141;
      206: FIdentFuncTable[I] := Func206;
    else
      FIdentFuncTable[I] := AltFunc;
    end;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.KeyHash(ToHash: PChar): Integer;

begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    Inc(Result, HashTable[ToHash^]);
    Inc(ToHash);
  end;
  FStringLen := ToHash - FToIdent;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.KeyComp(aKey: string): Boolean;

var
  I: Integer;
  Temp: PChar;

begin
  Temp := FToIdent;
  if Length(aKey) = FStringLen then
  begin
    Result := True;
    for I := 1 to FStringLen do
    begin
      if Temp^ <> aKey[i] then
      begin
        Result := False;
        Break;
      end;
      Inc(Temp);
    end;
  end
  else
    Result := False;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func17: Integer;

begin
  if KeyComp('if') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func21: Integer;

begin
  if KeyComp('do') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func32: Integer;

begin
  if KeyComp('cdecl') then
    Result := tkKey
  else
    if KeyComp('case') then
      Result := tkKey
    else
      if KeyComp('_cdecl') then
        Result := tkKey
      else
        if KeyComp('__cdecl') then
          Result := tkKey
        else
          Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func34: Integer;

begin
  if KeyComp('char') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func36: Integer;

begin
  if KeyComp('asm') then
  begin
    Result := tkKey;
    FRange := rsAsm;
    FAsmStart := True;
  end
  else
    if KeyComp('_asm') then
    begin
      Result := tkKey;
      FRange := rsAsm;
      FAsmStart := True;
    end
    else
      if KeyComp('__asm') then
      begin
        Result := tkKey;
        FRange := rsAsm;
        FAsmStart := True;
      end
      else
        Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func40: Integer;

begin
  if KeyComp('catch') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func42: Integer;

begin
  if KeyComp('for') then
    Result := tkKey
  else
    if KeyComp('Break') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func45: Integer;

begin
  if KeyComp('else') then
    Result := tkKey
  else
    if KeyComp('new') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func46: Integer;

begin
  if KeyComp('__int8') then
    Result := tkKey
  else
    if KeyComp('__int16') then
      Result := tkKey
    else
      if KeyComp('int') then
        Result := tkKey
      else
        if KeyComp('__int32') then
          Result := tkKey
        else
          if KeyComp('__int64') then
            Result := tkKey
          else
            Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func48: Integer;

begin
  if KeyComp('False') then
    Result := tkKey
  else
    if KeyComp('bool') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func52: Integer;

begin
  if KeyComp('long') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func54: Integer;

begin
  if KeyComp('void') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func57: Integer;

begin
  if KeyComp('enum') then
    Result := tkKey
  else
    if KeyComp('delete') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func58: Integer;

begin
  if KeyComp('_pascal') then
    Result := tkKey
  else
    if KeyComp('__pascal') then
      Result := tkKey
    else
      if KeyComp('pascal') then
        Result := tkKey
      else
        Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func59: Integer;

begin
  if KeyComp('class') then
    Result := tkKey
  else
    if KeyComp('float') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func60: Integer;

begin
  if KeyComp('this') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func61: Integer;

begin
  if KeyComp('goto') then
    Result := tkKey
  else
    if KeyComp('auto') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func62: Integer;

begin
  if KeyComp('__thread') then
    Result := tkKey
  else
    if KeyComp('while') then
      Result := tkKey
    else
      if KeyComp('friend') then
        Result := tkKey
      else
        Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func64: Integer;

begin
  if KeyComp('signed') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func65: Integer;

begin
  if KeyComp('double') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func66: Integer;

begin
  if KeyComp('__try') then
    Result := tkKey
  else
    if KeyComp('try') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func67: Integer;

begin
  if KeyComp('__dispid') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func68: Integer;

begin
  if KeyComp('True') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func69: Integer;

begin
  if KeyComp('public') then
    Result := tkKey
  else
    if KeyComp('inline') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func71: Integer;

begin
  if KeyComp('__rtti') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func74: Integer;

begin
  if KeyComp('__classid') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func75: Integer;

begin
  if KeyComp('__declspec') then
    Result := tkKey
  else
    if KeyComp('using') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func76: Integer;

begin
  if KeyComp('const') then
    Result := tkKey
  else
    if KeyComp('default') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func78: Integer;

begin
  if KeyComp('_stdcall') then
    Result := tkKey
  else
    if KeyComp('union') then
      Result := tkKey
    else
      if KeyComp('__stdcall') then
        Result := tkKey
      else
        if KeyComp('static') then
          Result := tkKey
        else
          Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func79: Integer;

begin
  if KeyComp('__except') then
    Result := tkKey
  else
    if KeyComp('wchar_t') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func81: Integer;

begin
  if KeyComp('mutable') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func82: Integer;

begin
  if KeyComp('_fastcall') then
    Result := tkKey
  else
    if KeyComp('__fastcall') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func85: Integer;

begin
  if KeyComp('short') then
    Result := tkKey
  else
    if KeyComp('typeid') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func86: Integer;

begin
  if KeyComp('sizeof') then
    Result := tkKey
  else
    if KeyComp('__finally') then
      Result := tkKey
    else
      if KeyComp('namespace') then
        Result := tkKey
      else
        Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func88: Integer;

begin
  if KeyComp('switch') then
    Result := tkKey
  else
    if KeyComp('typedef') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func89: Integer;

begin
  if KeyComp('throw') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func92: Integer;

begin
  if KeyComp('extern') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func97: Integer;

begin
  if KeyComp('__import') then
    Result := tkKey
  else
    if KeyComp('_import') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func98: Integer;

begin
  if KeyComp('private') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func100: Integer;

begin
  if KeyComp('template') then
    Result := tkKey
  else
    if KeyComp('__closure') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func101: Integer;

begin
  if KeyComp('unsigned') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func102: Integer;

begin
  if KeyComp('return') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func104: Integer;

begin
  if KeyComp('volatile') then
    Result := tkKey
  else
    if KeyComp('_export') then
      Result := tkKey
    else
      if KeyComp('__export') then
        Result := tkKey
      else
        Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func105: Integer;

begin
  if KeyComp('__published') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func106: Integer;

begin
  if KeyComp('explicit') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func107: Integer;

begin
  if KeyComp('typename') then
    Result := tkKey
  else
    if KeyComp('struct') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func109: Integer;

begin
  if KeyComp('register') then
    Result := tkKey
  else
    if KeyComp('continue') then
      Result := tkKey
    else
      if KeyComp('__automated') then
        Result := tkKey
      else
        Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func110: Integer;

begin
  if KeyComp('virtual') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func115: Integer;

begin
  if KeyComp('protected') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func116: Integer;

begin
  if KeyComp('operator') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func123: Integer;

begin
  if KeyComp('dynamic_cast') then
    Result := tkKey
  else
    if KeyComp('const_cast') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func125: Integer;

begin
  if KeyComp('static_cast') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func141: Integer;

begin
  if KeyComp('__property') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.Func206: Integer;

begin
  if KeyComp('reinterpret_cast') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.AltFunc: Integer;

begin
  Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.IdentKind(MayBe: PChar): Integer;

var
  HashKey: Integer;

begin
  FToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 207 then
    Result := FIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.MakeMethodTables;

var
  I: Char;

begin
  for I := #0 to #255 do
    case I of
      '&': FProcTable[I] := AndSymbolProc;
      #39: FProcTable[I] := AsciiCharProc;
      '@': FProcTable[I] := AtSymbolProc;
      '}': FProcTable[I] := BraceCloseProc;
      '{': FProcTable[I] := BraceOpenProc;
      #13: FProcTable[I] := CRProc;
      ':': FProcTable[I] := ColonProc;
      ',': FProcTable[I] := CommaProc;
      '#': FProcTable[I] := DirectiveProc;
      '=': FProcTable[I] := EqualProc;
      '>': FProcTable[I] := GreaterProc;
      '?': FProcTable[I] := QuestionProc;
      'A'..'Z', 'a'..'z', '_': FProcTable[I] := IdentProc;
      #10: FProcTable[I] := LFProc;
      '<': FProcTable[I] := LowerProc;
      '-': FProcTable[I] := MinusProc;
      '%': FProcTable[I] := ModSymbolProc;
      '!': FProcTable[I] := NotSymbolProc;
      #0: FProcTable[I] := NullProc;
      '0'..'9': FProcTable[I] := NumberProc;
      '|': FProcTable[I] := OrSymbolProc;
      '+': FProcTable[I] := PlusProc;
      '.': FProcTable[I] := PointProc;
      ')': FProcTable[I] := RoundCloseProc;
      '(': FProcTable[I] := RoundOpenProc;
      ';': FProcTable[I] := SemiColonProc;
      '/': FProcTable[I] := SlashProc;
      #1..#9, #11, #12, #14..#32: FProcTable[I] := SpaceProc;
      ']': FProcTable[I] := SquareCloseProc;
      '[': FProcTable[I] := SquareOpenProc;
      '*': FProcTable[I] := StarProc;
      #34: FProcTable[I] := StringProc;
      '~': FProcTable[I] := TildeProc;
      '^': FProcTable[I] := XOrSymbolProc;
    else
      FProcTable[I] := UnknownProc;
    end;
end;

//--------------------------------------------------------------------------------

constructor TUCECPPHighlighter.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  FAsmAttributes := THighLightAttributes.Create('assembler');
  FCommentAttributes := THighLightAttributes.Create('comment');
  FCommentAttributes.Style := [fsItalic];
  FIdentifierAttributes := THighLightAttributes.Create('identifier');
  FInvalidAttributes := THighLightAttributes.Create('illegal char');
  FKeyAttributes := THighLightAttributes.Create('reserved word');
  FKeyAttributes.Style := [fsBold];
  FNumberAttributes := THighLightAttributes.Create('number');
  FSpaceAttributes := THighLightAttributes.Create('space');
  FSpaceAttributes.Foreground := clWindow;
  FStringAttributes := THighLightAttributes.Create('String');
  FSymbolAttributes := THighLightAttributes.Create('symbol');
  FDirecAttributes := THighLightAttributes.Create('preprocessor');
  SetHighlightChange;
  InitIdent;
  MakeMethodTables;
  FRange := rsUnknown;
  FAsmStart := False;
  DefaultFilter := 'C++ files (*.c, *.cpp, *.h, *.hpp)|*.c;*.cpp;*.h;*.hpp';
end;

//--------------------------------------------------------------------------------

destructor TUCECPPHighlighter.Destroy;

begin
  FAsmAttributes.Free;
  FCommentAttributes.Free;
  FIdentifierAttributes.Free;
  FInvalidAttributes.Free;
  FKeyAttributes.Free;
  FNumberAttributes.Free;
  FSpaceAttributes.Free;
  FStringAttributes.Free;
  FSymbolAttributes.Free;
  FDirecAttributes.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.SetLine(const NewValue: string);

begin
  FLine := PChar(NewValue);
  Run := 0;
  FEol := False;
  Next;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.AnsiCProc;

begin
  FTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        exit;
      end;
    #10:
      begin
        LFProc;
        exit;
      end;
    #13:
      begin
        CRProc;
        exit;
      end;
  end;

  while FLine[Run] <> #0 do
    case FLine[Run] of
      '*':
        if FLine[Run + 1] = '/' then
        begin
          Inc(Run, 2);
          if FRange = rsAnsiCAsm then
            FRange := rsAsm
          else
          begin
            if FRange = rsAnsiCAsmBlock then
              FRange := rsAsmBlock
            else
              FRange := rsUnKnown;
          end;
          Break;
        end
        else
          Inc(Run);
      #10: Break;
      #13: Break;
    else
      Inc(Run);
    end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.AndSymbolProc;

begin
  case FLine[Run + 1] of
    '=': {and assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkAndAssign;
      end;
    '&': {logical and}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkLogAnd;
      end;
  else {and}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkAnd;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.AsciiCharProc;

begin
  FTokenID := tkString;
  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
      #92: // backslash
                                       // if we have an escaped single quote it doesn't count
        if FLine[Run + 1] = #39 then
          Inc(Run);
    end;
    Inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then
    Inc(Run);
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.AtSymbolProc;

begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.BraceCloseProc;

begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkBraceClose;
  if FRange = rsAsmBlock then
    FRange := rsUnknown;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.BraceOpenProc;

begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkBraceOpen;
  if FRange = rsAsm then
  begin
    FRange := rsAsmBlock;
    FAsmStart := True;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.CRProc;

begin
  FTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: Inc(Run, 2);
  else
    Inc(Run);
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.ColonProc;

begin
  case FLine[Run + 1] of
    ':': {scope resolution operator}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkScopeResolution;
      end;
  else {colon}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkColon;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.CommaProc;

begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkComma;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.DirectiveProc;

begin
  FTokenID := tkDirective;
  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
    end;
    Inc(Run);
  until FLine[Run] = #0;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.EqualProc;

begin
  case FLine[Run + 1] of
    '=': {logical equal}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkLogEqual;
      end;
  else {assign}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkAssign;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.GreaterProc;

begin
  case FLine[Run + 1] of
    '=': {greater than or equal to}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkGreaterThanEqual;
      end;
    '>':
      begin
        if FLine[Run + 2] = '=' then {shift right assign}
        begin
          Inc(Run, 3);
          FExtTokenID := xtkShiftRightAssign;
        end
        else {shift right}
        begin
          Inc(Run, 2);
          FExtTokenID := xtkShiftRight;
        end;
        FTokenID := tkSymbol;
      end;
  else {greater than}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkGreaterThan;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.QuestionProc;

begin
  FTokenID := tkSymbol; {conditional}
  FExtTokenID := xtkQuestion;
  Inc(Run);
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.IdentProc;

begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while Identifiers[FLine[Run]] do
    Inc(Run);
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.LFProc;

begin
  FTokenID := tkSpace;
  Inc(Run);
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.LowerProc;

begin
  case FLine[Run + 1] of
    '=': {less than or equal to}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkLessThanEqual;
      end;
    '<':
      begin
        if FLine[Run + 2] = '=' then {shift left assign}
        begin
          Inc(Run, 3);
          FExtTokenID := xtkShiftLeftAssign;
        end
        else {shift left}
        begin
          Inc(Run, 2);
          FExtTokenID := xtkShiftLeft;
        end;
        FTokenID := tkSymbol;
      end;
  else {less than}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkLessThan;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.MinusProc;

begin
  case FLine[Run + 1] of
    '=': {subtract assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkSubtractAssign;
      end;
    '-': {decrement}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkDecrement;
      end;
    '>': {arrow}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkArrow;
      end;
  else {subtract}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkSubtract;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.ModSymbolProc;

begin
  case FLine[Run + 1] of
    '=': {mod assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkModAssign;
      end;
  else {mod}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkMod;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.NotSymbolProc;

begin
  case FLine[Run + 1] of
    '=': {not equal}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkNotEqual;
      end;
  else {not}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkLogComplement;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.NullProc;

begin
  FTokenID := tkNull;
  FEol := True;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.NumberProc;

begin
  Inc(Run);
  FTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f', '.', 'u', 'U', 'l', 'L', 'x', 'X'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then
          Break;
    end;
    Inc(Run);
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.OrSymbolProc;

begin
  case FLine[Run + 1] of
    '=': {or assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkIncOrAssign;
      end;
    '|': {logical or}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkLogOr;
      end;
  else {or}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkIncOr;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.PlusProc;

begin
  case FLine[Run + 1] of
    '=': {add assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkAddAssign;
      end;
    '+': {increment}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkIncrement;
      end;
  else {add}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkAdd;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.PointProc;

begin
  if (FLine[Run + 1] = '.') and (FLine[Run + 2] = '.') then
  begin {ellipse}
    Inc(Run, 3);
    FTokenID := tkSymbol;
    FExtTokenID := xtkEllipse;
  end
  else {point}
  begin
    Inc(Run);
    FTokenID := tkSymbol;
    FExtTokenID := xtkPoint;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.RoundCloseProc;

begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundClose;
  dec(FRoundCount);
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.RoundOpenProc;

begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundOpen;
  Inc(FRoundCount);
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.SemiColonProc;

begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
  if FRange = rsAsm then
    FRange := rsUnknown;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.SlashProc;

begin
  case FLine[Run + 1] of
    '/': {c++ style comments}
      begin
        Inc(Run, 2);
        FTokenID := tkComment;
        while FLine[Run] <> #0 do
        begin
          case FLine[Run] of
            #10, #13: Break;
          end;
          Inc(Run);
        end;
      end;
    '*': {c style comments}
      begin
        FTokenID := tkComment;
        if FRange = rsAsm then
          FRange := rsAnsiCAsm
        else
        begin
          if FRange = rsAsmBlock then
            FRange := rsAnsiCAsmBlock
          else
            FRange := rsAnsiC;
        end;
        Inc(Run, 2);
        while FLine[Run] <> #0 do
          case FLine[Run] of
            '*':
              if FLine[Run + 1] = '/' then
              begin
                Inc(Run, 2);
                if FRange = rsAnsiCAsm then
                  FRange := rsAsm
                else
                begin
                  if FRange = rsAnsiCAsmBlock then
                    FRange := rsAsmBlock
                  else
                    FRange := rsUnKnown;
                end;
                Break;
              end
              else
                Inc(Run);
            #10: Break;
            #13: Break;
          else
            Inc(Run);
          end;
      end;
    '=': {divide assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkDivideAssign;
      end;
  else {divide}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkDivide;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.SpaceProc;

begin
  Inc(Run);
  FTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do
    Inc(Run);
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.SquareCloseProc;

begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareClose;
  dec(FSquareCount);
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.SquareOpenProc;

begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkSquareOpen;
  Inc(FSquareCount);
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.StarProc;

begin
  case FLine[Run + 1] of
    '=': {multiply assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkMultiplyAssign;
      end;
  else {star}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkStar;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.StringProc;

begin
  FTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then
    Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
      #92: {backslash}
        case FLine[Run + 1] of
          #10: Inc(Run); {line continuation character}
          #34: Inc(Run); {escaped quote doesn't count}
        end;
    end;
    Inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then
    Inc(Run);
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.TildeProc;

begin
  Inc(Run); {bitwise complement}
  FTokenID := tkSymbol;
  FExtTokenID := xtkBitComplement;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.XOrSymbolProc;

begin
  case FLine[Run + 1] of
    '=': {xor assign}
      begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
        FExtTokenID := xtkXorAssign;
      end;
  else {xor}
    begin
      Inc(Run);
      FTokenID := tkSymbol;
      FExtTokenID := xtkXor;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.UnknownProc;

begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.Next;

begin
  FAsmStart := False;
  FTokenPos := Run;
  case FRange of
    rsAnsiC: AnsiCProc;
    rsAnsiCAsm: AnsiCProc;
    rsAnsiCAsmBlock: AnsiCProc;
  else
    begin
      FRange := rsUnknown;
      FProcTable[FLine[Run]];
    end;
  end;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.EOL: Boolean;

begin
  Result := False;
  if FTokenID = tkNull then
    Result := True;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.GetRange: Integer;

begin
  Result := Ord(FRange);
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.GetToken: string;

var
  Len: LongInt;
begin
  Len := Run - FTokenPos;
  SetString(Result, (FLine + FTokenPos), Len);
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.GetTokenID: Integer;

begin
  if ((FRange = rsAsm) or (FRange = rsAsmBlock)) and (not FAsmStart) then
    case FTokenID of
      tkComment: Result := tkComment;
      tkNull: Result := tkNull;
      tkSpace: Result := tkSpace;
    else
      Result := tkAsm;
    end
  else
    Result := FTokenID;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.GetExtTokenID: TxtkTokenKind;

begin
  Result := FExtTokenID;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.GetTokenInfo: TTokenData;

// gibt für das aktuelle Token einen Zeiger in den momentanen Text zurück (also nicht freigeben)
// und außerdem die Länge des Tokenstrings

begin
  with Result do
  begin
    Token := FLine + FTokenPos;
    TokenType := GetTokenID;
    Position := FTokenPos;
    Length := Run - FTokenPos;

    case TokenType of
      tkAsm:
        begin
          Background := ColorToRGB(FAsmAttributes.Background);
          Foreground := ColorToRGB(FAsmAttributes.Foreground);
          Style := FAsmAttributes.Style;
        end;
      tkComment:
        begin
          Background := ColorToRGB(FCommentAttributes.Background);
          Foreground := ColorToRGB(FCommentAttributes.Foreground);
          Style := FCommentAttributes.Style;
        end;
      tkDirective:
        begin
          Background := ColorToRGB(FDirecAttributes.Background);
          Foreground := ColorToRGB(FDirecAttributes.Foreground);
          Style := FDirecAttributes.Style;
        end;
      tkIdentifier:
        begin
          Background := ColorToRGB(FIdentifierAttributes.Background);
          Foreground := ColorToRGB(FIdentifierAttributes.Foreground);
          Style := FIdentifierAttributes.Style;
        end;
      tkKey:
        begin
          Background := ColorToRGB(FKeyAttributes.Background);
          Foreground := ColorToRGB(FKeyAttributes.Foreground);
          Style := FKeyAttributes.Style;
        end;
      tkNumber:
        begin
          Background := ColorToRGB(FNumberAttributes.Background);
          Foreground := ColorToRGB(FNumberAttributes.Foreground);
          Style := FNumberAttributes.Style;
        end;
      tkSpace:
        begin
          Background := ColorToRGB(FSpaceAttributes.Background);
          Foreground := ColorToRGB(FSpaceAttributes.Foreground);
          Style := FSpaceAttributes.Style;
        end;
      tkString:
        begin
          Background := ColorToRGB(FStringAttributes.Background);
          Foreground := ColorToRGB(FStringAttributes.Foreground);
          Style := FStringAttributes.Style;
        end;
      tkSymbol:
        begin
          Background := ColorToRGB(FSymbolAttributes.Background);
          Foreground := ColorToRGB(FSymbolAttributes.Foreground);
          Style := FSymbolAttributes.Style;
        end;
      tkUnknown:
        begin
          Background := ColorToRGB(FInvalidAttributes.Background);
          Foreground := ColorToRGB(FInvalidAttributes.Foreground);
          Style := FInvalidAttributes.Style;
        end;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.ReSetRange;

begin
  FRange := rsUnknown;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.SetRange(Value: Integer);

begin
  FRange := TRangeState(Value);
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.EnumUserSettings(Settings: TStrings);

// Returns the user settings for C++ Builder from the Borland key in the registry.

begin
  with TRegistry.Create do
  begin
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('\Software\Borland\C++Builder') then
      begin
        try
          GetKeyNames(settings);
        finally
          CloseKey;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.UseUserSettings(Path: string): Boolean;

// Reads the settings from the C++ Builder installation that is given by Path. This value can be returned from EnumUserSettings.
// You can also simply set a fix value for it, e.g. "C++Builder/6.0".
// This path must be relative to HKLU/Software/Borland.
// Result is True if the settings could be read otherwise False.

var
  Version: Integer;

begin
  Result := Length(Path) > 0;

  if Result then
  begin
    // Version is 1..6 for BCB 1..6. BCB before 4 aren't supported, though.
    if Pos('C++Builder', Path) = 1 then
      Version := StrToInt(Copy(Path, Length('C++Builder\') + 1, 1))
    else
      Version := StrToInt(Path[1]);

    Result := Version > 3;
    if Result then
    begin
      Path := Path + '\Highlight';
      if not FAsmAttributes.LoadFromBorlandRegistry(Path + '\Assembler', Version) then
        Result := False;
      if not FCommentAttributes.LoadFromBorlandRegistry(Path + '\Comment', Version) then
        Result := False;
      if not FDirecAttributes.LoadFromBorlandRegistry(Path + '\Preprocessor', Version) then
        Result := False;
      if not FIdentifierAttributes.LoadFromBorlandRegistry(Path + '\Identifier', Version) then
        Result := False;
      if not FInvalidAttributes.LoadFromBorlandRegistry(Path + '\Illegal Char', Version) then
        Result := False;
      if not FKeyAttributes.LoadFromBorlandRegistry(Path + '\Reserved Word', Version) then
        Result := False;
      if not FNumberAttributes.LoadFromBorlandRegistry(Path + '\Integer', Version) then
        Result := False;
      if not FSpaceAttributes.LoadFromBorlandRegistry(Path + '\Whitespace', Version) then
        Result := False;
      if not FStringAttributes.LoadFromBorlandRegistry(Path + '\String', Version) then
        Result := False;
      if not FSymbolAttributes.LoadFromBorlandRegistry(Path + '\Symbol', Version) then
        Result := False;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.HighLightChange(Sender: TObject);

begin
  WindowList.Invalidate;
end;

//--------------------------------------------------------------------------------

procedure TUCECPPHighlighter.SetHighLightChange;

begin
  FAsmAttributes.Onchange := HighLightChange;
  FCommentAttributes.Onchange := HighLightChange;
  FDirecAttributes.Onchange := HighLightChange;
  FIdentifierAttributes.Onchange := HighLightChange;
  FInvalidAttributes.Onchange := HighLightChange;
  FKeyAttributes.Onchange := HighLightChange;
  FNumberAttributes.Onchange := HighLightChange;
  FSpaceAttributes.Onchange := HighLightChange;
  FStringAttributes.Onchange := HighLightChange;
  FSymbolAttributes.Onchange := HighLightChange;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.GetIdentChars: TIdentChars;

begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.GetAttributeCount: Integer;

begin
  Result := 10;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.GetAttribute(Index: Integer): THighLightAttributes;

begin
  case Index of
    0: Result := FAsmAttributes;
    1: Result := FCommentAttributes;
    2: Result := FDirecAttributes;
    3: Result := FIdentifierAttributes;
    4: Result := FInvalidAttributes;
    5: Result := FKeyAttributes;
    6: Result := FNumberAttributes;
    7: Result := FSpaceAttributes;
    8: Result := FStringAttributes;
    9: Result := FSymbolAttributes;
  else
    Result := nil;
  end;
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.GetLanguageName: string;

begin
  Result := 'C++';
end;

//--------------------------------------------------------------------------------

function TUCECPPHighlighter.GetCapabilities: THighlighterCapabilities;

begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

//--------------------------------------------------------------------------------

procedure MakeIdentTable;

var
  I: Char;

begin
  for I := #0 to #255 do
  begin
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else
      Identifiers[I] := False;
    end;

    case I in ['_', 'a'..'z', 'A'..'Z'] of
      True:
        begin
          if (I > #64) and (I < #91) then
            HashTable[I] := Ord(I) - 64
          else
            if (I > #96) then
              HashTable[I] := Ord(I) - 95;
        end;
    else
      HashTable[I] := 0;
    end;
  end;
end;

//--------------------------------------------------------------------------------

initialization
  MakeIdentTable;
end.

