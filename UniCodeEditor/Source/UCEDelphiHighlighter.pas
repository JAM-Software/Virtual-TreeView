unit UCEDelphiHighlighter;

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
//   Original version of a Delphi highlighter contained in mwEdit with some minor changes to make it
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

uses SysUtils, Windows, Messages, Classes, Controls, Graphics, Registry,
  UCEHighlighter;

const
  tkAsm = 257;
  tkComment = 258;
  tkIdentifier = 259;
  tkKey = 260;
  tkNull = 261;
  tkNumber = 262;
  tkSpace = 263;
  tkString = 264;
  tkSymbol = 265;
  tkUnknown = 266;

type
  TRangeState = (rsANil, rsAnsi, rsAnsiAsm, rsAsm, rsBor, rsBorAsm, rsProperty, rsUnKnown);

  // Do this in two parts with the type here and the array below
  // this is required because the BCB header creator is buggy.
  TProcTableProc = procedure of object;
  TIdentFuncTableFunc = function: Integer of object;

  TUCEDelphiHighlighter = class(TUCEHighlighter)
  private
    FAsmStart: Boolean;
    FRange: TRangeState;
    FLine: PChar;
    // Altered to use the above types
    // this is required because the BCB header creator is buggy.
    FProcTable: array[#0..#255] of TProcTableProc;
    FIdentFuncTable: array[0..191] of TIdentFuncTableFunc;
    Run: LongInt;
    FMarker: PChar;
    FRoundCount: Integer;
    FSquareCount: Integer;
    FStringLen: Integer;
    FToIdent: PChar;
    FTokenPos: Integer;
    FCurrentToken: Integer;
    FEol: Boolean;
    FStringAttributes,
    FNumberAttributes,
    FKeyAttributes,
    FSymbolAttributes,
    FAsmAttributes,
    FCommentAttributes,
    FIdentifierAttributes,
    FSpaceAttributes: THighlightAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(Key: string): Boolean;
    function Func15: Integer;
    function Func19: Integer;
    function Func20: Integer;
    function Func21: Integer;
    function Func23: Integer;
    function Func25: Integer;
    function Func27: Integer;
    function Func28: Integer;
    function Func32: Integer;
    function Func33: Integer;
    function Func35: Integer;
    function Func37: Integer;
    function Func38: Integer;
    function Func39: Integer;
    function Func40: Integer;
    function Func41: Integer;
    function Func44: Integer;
    function Func45: Integer;
    function Func47: Integer;
    function Func49: Integer;
    function Func52: Integer;
    function Func54: Integer;
    function Func55: Integer;
    function Func56: Integer;
    function Func57: Integer;
    function Func59: Integer;
    function Func60: Integer;
    function Func61: Integer;
    function Func63: Integer;
    function Func64: Integer;
    function Func65: Integer;
    function Func66: Integer;
    function Func69: Integer;
    function Func71: Integer;
    function Func73: Integer;
    function Func75: Integer;
    function Func76: Integer;
    function Func79: Integer;
    function Func81: Integer;
    function Func84: Integer;
    function Func85: Integer;
    function Func87: Integer;
    function Func88: Integer;
    function Func91: Integer;
    function Func92: Integer;
    function Func94: Integer;
    function Func95: Integer;
    function Func96: Integer;
    function Func97: Integer;
    function Func98: Integer;
    function Func99: Integer;
    function Func100: Integer;
    function Func101: Integer;
    function Func102: Integer;
    function Func103: Integer;
    function Func105: Integer;
    function Func106: Integer;
    function Func117: Integer;
    function Func126: Integer;
    function Func129: Integer;
    function Func132: Integer;
    function Func133: Integer;
    function Func136: Integer;
    function Func141: Integer;
    function Func143: Integer;
    function Func166: Integer;
    function Func168: Integer;
    function Func191: Integer;
    function AltFunc: Integer;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): Integer;
    procedure MakeMethodTables;
    procedure AddressOpProc;
    procedure AsciiCharProc;
    procedure AnsiProc;
    procedure BorProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure CRProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PlusProc;
    procedure PointerSymbolProc;
    procedure PointProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StarProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure HighlightChange(Sender: TObject);
    procedure SetAttribute(const Index: Integer; const Value: THighlightAttributes);
  protected
    function GetIdentChars: TIdentChars; override;
    function GetLanguageName: string; override;
    function GetAttributeCount: Integer; override;
    function GetAttribute(Index: Integer): THighlightAttributes; override;
    function GetCapabilities: THighlighterCapabilities; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function EOL: Boolean; override;
    function GetRange: Integer; override;
    function GetTokenID: Integer;
    procedure SetLine(const NewValue: string); override;
    function GetToken: string; override;
    function GetTokenInfo: TTokenData; override;
    function HasMoreTokens: Boolean; override;
    procedure Next; override;
    procedure SetRange(Value: Integer); override;
    procedure ResetRange; override;
    function UseUserSettings(Path: string): Boolean; override;
    procedure EnumUserSettings(Settings: TStrings); override;

    property Capabilities;
    property IdentChars;
  published
    property AsmAttributes: THighlightAttributes index 0 read FAsmAttributes write SetAttribute;
    property CommentAttributes: THighlightAttributes index 1 read FCommentAttributes write SetAttribute;
    property IdentifierAttributes: THighlightAttributes index 2 read FIdentifierAttributes write SetAttribute;
    property KeyAttributes: THighlightAttributes index 3 read FKeyAttributes write SetAttribute;
    property NumberAttributes: THighlightAttributes index 4 read FNumberAttributes write SetAttribute;
    property SpaceAttributes: THighlightAttributes index 5 read FSpaceAttributes write SetAttribute;
    property StringAttributes: THighlightAttributes index 6 read FStringAttributes write SetAttribute;
    property SymbolAttributes: THighlightAttributes index 7 read FSymbolAttributes write SetAttribute;
  end;

//--------------------------------------------------------------------------------

implementation

var
  Identifiers: array[Char] of ByteBool;
  HashTable: array[Char] of Integer;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.InitIdent;

var
  I: Integer;

begin
  for I := 0 to 191 do
    case I of
      15: FIdentFuncTable[I] := Func15;
      19: FIdentFuncTable[I] := Func19;
      20: FIdentFuncTable[I] := Func20;
      21: FIdentFuncTable[I] := Func21;
      23: FIdentFuncTable[I] := Func23;
      25: FIdentFuncTable[I] := Func25;
      27: FIdentFuncTable[I] := Func27;
      28: FIdentFuncTable[I] := Func28;
      32: FIdentFuncTable[I] := Func32;
      33: FIdentFuncTable[I] := Func33;
      35: FIdentFuncTable[I] := Func35;
      37: FIdentFuncTable[I] := Func37;
      38: FIdentFuncTable[I] := Func38;
      39: FIdentFuncTable[I] := Func39;
      40: FIdentFuncTable[I] := Func40;
      41: FIdentFuncTable[I] := Func41;
      44: FIdentFuncTable[I] := Func44;
      45: FIdentFuncTable[I] := Func45;
      47: FIdentFuncTable[I] := Func47;
      49: FIdentFuncTable[I] := Func49;
      52: FIdentFuncTable[I] := Func52;
      54: FIdentFuncTable[I] := Func54;
      55: FIdentFuncTable[I] := Func55;
      56: FIdentFuncTable[I] := Func56;
      57: FIdentFuncTable[I] := Func57;
      59: FIdentFuncTable[I] := Func59;
      60: FIdentFuncTable[I] := Func60;
      61: FIdentFuncTable[I] := Func61;
      63: FIdentFuncTable[I] := Func63;
      64: FIdentFuncTable[I] := Func64;
      65: FIdentFuncTable[I] := Func65;
      66: FIdentFuncTable[I] := Func66;
      69: FIdentFuncTable[I] := Func69;
      71: FIdentFuncTable[I] := Func71;
      73: FIdentFuncTable[I] := Func73;
      75: FIdentFuncTable[I] := Func75;
      76: FIdentFuncTable[I] := Func76;
      79: FIdentFuncTable[I] := Func79;
      81: FIdentFuncTable[I] := Func81;
      84: FIdentFuncTable[I] := Func84;
      85: FIdentFuncTable[I] := Func85;
      87: FIdentFuncTable[I] := Func87;
      88: FIdentFuncTable[I] := Func88;
      91: FIdentFuncTable[I] := Func91;
      92: FIdentFuncTable[I] := Func92;
      94: FIdentFuncTable[I] := Func94;
      95: FIdentFuncTable[I] := Func95;
      96: FIdentFuncTable[I] := Func96;
      97: FIdentFuncTable[I] := Func97;
      98: FIdentFuncTable[I] := Func98;
      99: FIdentFuncTable[I] := Func99;
      100: FIdentFuncTable[I] := Func100;
      101: FIdentFuncTable[I] := Func101;
      102: FIdentFuncTable[I] := Func102;
      103: FIdentFuncTable[I] := Func103;
      105: FIdentFuncTable[I] := Func105;
      106: FIdentFuncTable[I] := Func106;
      117: FIdentFuncTable[I] := Func117;
      126: FIdentFuncTable[I] := Func126;
      129: FIdentFuncTable[I] := Func129;
      132: FIdentFuncTable[I] := Func132;
      133: FIdentFuncTable[I] := Func133;
      136: FIdentFuncTable[I] := Func136;
      141: FIdentFuncTable[I] := Func141;
      143: FIdentFuncTable[I] := Func143;
      166: FIdentFuncTable[I] := Func166;
      168: FIdentFuncTable[I] := Func168;
      191: FIdentFuncTable[I] := Func191;
    else
      FIdentFuncTable[I] := AltFunc;
    end;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.KeyHash(ToHash: PChar): Integer;

begin
  Result := 0;
  while ToHash^ in ['a'..'z', 'A'..'Z'] do
  begin
    Inc(Result, HashTable[ToHash^]);
    Inc(ToHash);
  end;
  if ToHash^ in ['_', '0'..'9'] then
    Inc(ToHash);
  FStringLen := ToHash - FToIdent;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.KeyComp(Key: string): Boolean;

var
  I: Integer;

begin
  FMarker := FToIdent;
  if Length(Key) = FStringLen then
  begin
    Result := True;
    for I := 1 to FStringLen do
    begin
      if HashTable[FMarker^] <> HashTable[Key[I]] then
      begin
        Result := False;
        Break;
      end;
      Inc(FMarker);
    end;
  end
  else
    Result := False;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func15: Integer;

begin
  if KeyComp('If') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func19: Integer;

begin
  if KeyComp('Do') then
    Result := tkKey
  else
    if KeyComp('And') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func20: Integer;

begin
  if KeyComp('As') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func21: Integer;

begin
  if KeyComp('Of') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func23: Integer;

begin
  if KeyComp('End') then
  begin
    Result := tkKey;
    FRange := rsUnknown;
  end
  else
    if KeyComp('In') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func25: Integer;

begin
  if KeyComp('Far') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func27: Integer;

begin
  if KeyComp('Cdecl') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func28: Integer;

begin
  if KeyComp('Is') then
    Result := tkKey
  else
    if KeyComp('Read') then
    begin
      if FRange = rsProperty then
        Result := tkKey
      else
        Result := tkIdentifier;
    end
    else
      if KeyComp('case') then
        Result := tkKey
      else
        Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func32: Integer;

begin
  if KeyComp('Label') then
    Result := tkKey
  else
    if KeyComp('Mod') then
      Result := tkKey
    else
      if KeyComp('File') then
        Result := tkKey
      else
        Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func33: Integer;

begin
  if KeyComp('Or') then
    Result := tkKey
  else
    if KeyComp('Asm') then
    begin
      Result := tkKey;
      FRange := rsAsm;
      FAsmStart := True;
    end
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func35: Integer;

begin
  if KeyComp('Nil') then
    Result := tkKey
  else
    if KeyComp('To') then
      Result := tkKey
    else
      if KeyComp('Div') then
        Result := tkKey
      else
        Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func37: Integer;

begin
  if KeyComp('Begin') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func38: Integer;

begin
  if KeyComp('Near') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func39: Integer;

begin
  if KeyComp('For') then
    Result := tkKey
  else
    if KeyComp('Shl') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func40: Integer;

begin
  if KeyComp('Packed') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func41: Integer;

begin
  if KeyComp('Else') then
    Result := tkKey
  else
    if KeyComp('Var') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func44: Integer;

begin
  if KeyComp('Set') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func45: Integer;

begin
  if KeyComp('Shr') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func47: Integer;

begin
  if KeyComp('Then') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func49: Integer;

begin
  if KeyComp('Not') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func52: Integer;

begin
  if KeyComp('Pascal') then
    Result := tkKey
  else
    if KeyComp('Raise') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func54: Integer;

begin
  if KeyComp('Class') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func55: Integer;

begin
  if KeyComp('Object') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func56: Integer;

begin
  if KeyComp('Index') then
  begin
    if FRange = rsProperty then
      Result := tkKey
    else
      Result := tkIdentifier;
  end
  else
    if KeyComp('Out') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func57: Integer;

begin
  if KeyComp('Goto') then
    Result := tkKey
  else
    if KeyComp('While') then
      Result := tkKey
    else
      if KeyComp('Xor') then
        Result := tkKey
      else
        Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func59: Integer;

begin
  if KeyComp('Safecall') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func60: Integer;

begin
  if KeyComp('With') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func61: Integer;

begin
  if KeyComp('Dispid') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func63: Integer;

var
  FMarker: Integer;
begin
  if KeyComp('Public') then
  begin
    Result := tkKey;
    if Run = 0 then
      FRange := rsUnKnown
    else
    begin
      FMarker := Run;
      while Run > 0 do
      begin
        Dec(Run);
        if FLine[Run] in [#33..#255] then
          Break;
      end;
      if Run = 0 then
        FRange := rsUnKnown;
      Run := FMarker;
    end;
  end
  else
    if KeyComp('Record') then
      Result := tkKey
    else
      if KeyComp('Array') then
        Result := tkKey
      else
        if KeyComp('Try') then
          Result := tkKey
        else
          if KeyComp('Inline') then
            Result := tkKey
          else
            Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func64: Integer;

begin
  if KeyComp('Unit') then
    Result := tkKey
  else
    if KeyComp('Uses') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func65: Integer;

begin
  if KeyComp('Repeat') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func66: Integer;

begin
  if KeyComp('Type') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func69: Integer;

begin
  if KeyComp('Default') then
    Result := tkKey
  else
    if KeyComp('Dynamic') then
      Result := tkKey
    else
      if KeyComp('Message') then
        Result := tkKey
      else
        Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func71: Integer;

begin
  if KeyComp('Stdcall') then
    Result := tkKey
  else
    if KeyComp('Const') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func73: Integer;

begin
  if KeyComp('Except') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func75: Integer;

begin
  if KeyComp('Write') then
  begin
    if FRange = rsProperty then
      Result := tkKey
    else
      Result := tkIdentifier;
  end
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func76: Integer;

begin
  if KeyComp('Until') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func79: Integer;

begin
  if KeyComp('Finally') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func81: Integer;

begin
  if KeyComp('Stored') then
  begin
    if FRange = rsProperty then
      Result := tkKey
    else
      Result := tkIdentifier;
  end
  else
    if KeyComp('Interface') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func84: Integer;

begin
  if KeyComp('Abstract') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func85: Integer;

begin
  if KeyComp('Forward') then
    Result := tkKey
  else
    if KeyComp('Library') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func87: Integer;

begin
  if KeyComp('String') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func88: Integer;

begin
  if KeyComp('Program') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func91: Integer;

var
  FMarker: Integer;

begin
  if KeyComp('Downto') then
    Result := tkKey
  else
    if KeyComp('Private') then
    begin
      Result := tkKey;
      if Run = 0 then
        FRange := rsUnKnown
      else
      begin
        FMarker := Run;
        while Run > 0 do
        begin
          Dec(Run);
          if FLine[Run] in [#33..#255] then
            Break;
        end;
        if Run = 0 then
          FRange := rsUnKnown;
        Run := FMarker;
      end;
    end
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func92: Integer;

begin
  if KeyComp('overload') then
    Result := tkKey
  else
    if KeyComp('Inherited') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func94: Integer;

begin
  if KeyComp('Assembler') then
    Result := tkKey
  else
    if KeyComp('Readonly') then
    begin
      if FRange = rsProperty then
        Result := tkKey
      else
        Result := tkIdentifier;
    end
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func95: Integer;

begin
  if KeyComp('Absolute') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func96: Integer;

var
  FMarker: Integer;

begin
  if KeyComp('Published') then
  begin
    Result := tkKey;
    if Run = 0 then
      FRange := rsUnKnown
    else
    begin
      FMarker := Run;
      while Run > 0 do
      begin
        Dec(Run);
        if FLine[Run] in [#33..#255] then
          Break;
      end;
      if Run = 0 then
        FRange := rsUnKnown;
      Run := FMarker;
    end;
  end
  else
    if KeyComp('Override') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func97: Integer;

begin
  if KeyComp('Threadvar') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func98: Integer;

begin
  if KeyComp('Export') then
    Result := tkKey
  else
    if KeyComp('Nodefault') then
    begin
      if FRange = rsProperty then
        Result := tkKey
      else
        Result := tkIdentifier;
    end
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func99: Integer;

begin
  if KeyComp('External') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func100: Integer;

begin
  if KeyComp('Automated') then
  begin
    if FRange = rsProperty then
    begin
      Result := tkKey;
      FRange := rsUnKnown;
    end
    else
      Result := tkIdentifier;
  end
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func101: Integer;

begin
  if KeyComp('Register') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func102: Integer;

begin
  if KeyComp('Function') then
  begin
    Result := tkKey;
    FRange := rsUnKnown;
  end
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func103: Integer;

begin
  if KeyComp('Virtual') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func105: Integer;

begin
  if KeyComp('Procedure') then
  begin
    Result := tkKey;
    FRange := rsUnKnown;
  end
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func106: Integer;

var
  FMarker: Integer;

begin
  if KeyComp('Protected') then
  begin
    Result := tkKey;
    if Run = 0 then
      FRange := rsUnKnown
    else
    begin
      FMarker := Run;
      while Run > 0 do
      begin
        Dec(Run);
        if FLine[Run] in [#33..#255] then
          Break;
      end;
      if Run = 0 then
        FRange := rsUnKnown;
      Run := FMarker;
    end;
  end
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func117: Integer;

begin
  if KeyComp('Exports') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func126: Integer;

begin
  if KeyComp('Implements') then
  begin
    if FRange = rsProperty then
      Result := tkKey
    else
      Result := tkIdentifier;
  end
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func129: Integer;

begin
  if KeyComp('Dispinterface') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func132: Integer;

begin
  if KeyComp('Reintroduce') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func133: Integer;

begin
  if KeyComp('Property') then
  begin
    Result := tkKey;
    FRange := rsProperty;
  end
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func136: Integer;

begin
  if KeyComp('Finalization') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func141: Integer;

begin
  if KeyComp('Writeonly') then
  begin
    if FRange = rsProperty then
      Result := tkKey
    else
      Result := tkIdentifier;
  end
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func143: Integer;

begin
  if KeyComp('Destructor') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func166: Integer;

begin
  if KeyComp('Constructor') then
    Result := tkKey
  else
    if KeyComp('Implementation') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func168: Integer;

begin
  if KeyComp('Initialization') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.Func191: Integer;

begin
  if KeyComp('Resourcestring') then
    Result := tkKey
  else
    if KeyComp('Stringresource') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.AltFunc: Integer;

begin
  Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.IdentKind(MayBe: PChar): Integer;

var
  HashKey: Integer;

begin
  FToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 192 then
    Result := FIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.MakeMethodTables;

var
  I: Char;

begin
  for I := #0 to #255 do
    case I of
      #0: FProcTable[I] := NullProc;
      #10: FProcTable[I] := LFProc;
      #13: FProcTable[I] := CRProc;
      #1..#9,
        #11, #12,
        #14..#32: FProcTable[I] := SpaceProc;
      '#': FProcTable[I] := AsciiCharProc;
      '$': FProcTable[I] := IntegerProc;
      #39: FProcTable[I] := StringProc;
      '0'..'9': FProcTable[I] := NumberProc;
      'A'..'Z',
        'a'..'z',
        '_': FProcTable[I] := IdentProc;
      '{': FProcTable[I] := BraceOpenProc;
      '}': FProcTable[I] := BraceCloseProc;
      '!', '"', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
        begin
          case I of
            '(': FProcTable[I] := RoundOpenProc;
            ')': FProcTable[I] := RoundCloseProc;
            '*': FProcTable[I] := StarProc;
            '+': FProcTable[I] := PlusProc;
            ',': FProcTable[I] := CommaProc;
            '-': FProcTable[I] := MinusProc;
            '.': FProcTable[I] := PointProc;
            '/': FProcTable[I] := SlashProc;
            ':': FProcTable[I] := ColonProc;
            ';': FProcTable[I] := SemiColonProc;
            '<': FProcTable[I] := LowerProc;
            '=': FProcTable[I] := EqualProc;
            '>': FProcTable[I] := GreaterProc;
            '@': FProcTable[I] := AddressOpProc;
            '[': FProcTable[I] := SquareOpenProc;
            ']': FProcTable[I] := SquareCloseProc;
            '^': FProcTable[I] := PointerSymbolProc;
          else
            FProcTable[I] := SymbolProc;
          end;
        end;
    else
      FProcTable[I] := UnknownProc;
    end;
end;

//--------------------------------------------------------------------------------

constructor TUCEDelphiHighlighter.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);

  FAsmAttributes := THighlightAttributes.Create('assembler');
  FCommentAttributes := THighlightAttributes.Create('comment');
  FCommentAttributes.Style := [fsItalic];
  FIdentifierAttributes := THighlightAttributes.Create('identifier');
  FKeyAttributes := THighlightAttributes.Create('reserved word');
  FKeyAttributes.Style := [fsBold];
  FNumberAttributes := THighlightAttributes.Create('number');
  FSpaceAttributes := THighlightAttributes.Create('space');
  FStringAttributes := THighlightAttributes.Create('String');
  FSymbolAttributes := THighlightAttributes.Create('symbol');
  FAsmAttributes.Onchange := HighlightChange;
  FCommentAttributes.Onchange := HighlightChange;
  FIdentifierAttributes.Onchange := HighlightChange;
  FKeyAttributes.Onchange := HighlightChange;
  FNumberAttributes.Onchange := HighlightChange;
  FSpaceAttributes.Onchange := HighlightChange;
  FStringAttributes.Onchange := HighlightChange;
  FSymbolAttributes.Onchange := HighlightChange;
  InitIdent;
  MakeMethodTables;
  FRange := rsUnknown;
  FAsmStart := False;
  DefaultFilter := 'Delphi source files (*.pas,*.inc)|*.pas;*.inc';
end;

//--------------------------------------------------------------------------------

destructor TUCEDelphiHighlighter.Destroy;

begin
  FAsmAttributes.Free;
  FCommentAttributes.Free;
  FIdentifierAttributes.Free;
  FKeyAttributes.Free;
  FNumberAttributes.Free;
  FSpaceAttributes.Free;
  FStringAttributes.Free;
  FSymbolAttributes.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.SetLine(const NewValue: string);

begin
  FLine := PChar(NewValue);
  Run := 0;
  FEol := False;
  FCurrentToken := -1;
  Next;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.AddressOpProc;

begin
  case FLine[Run + 1] of
    '@':
      begin
        FCurrentToken := tkSymbol;
        Inc(Run, 2);
      end;
  else
    FCurrentToken := tkSymbol;
    Inc(Run);
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.AsciiCharProc;

begin
  FCurrentToken := tkString;
  Inc(Run);
  while FLine[Run] in ['0'..'9'] do
    Inc(Run);
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.BraceCloseProc;

begin
  Inc(Run);
  FCurrentToken := tkSymbol;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.BorProc;

begin
  FCurrentToken := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;

    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  while FLine[Run] <> #0 do
    case FLine[Run] of
      '}':
        begin
          if FRange = rsBorAsm then
            FRange := rsAsm
          else
            FRange := rsUnKnown;
          Inc(Run);
          Break;
        end;
      #10,
        #13: Break;
    else
      Inc(Run);
    end;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.BraceOpenProc;

begin
  FCurrentToken := tkComment;
  if FRange = rsAsm then
    FRange := rsBorAsm
  else
    FRange := rsBor;
  Inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      '}':
        begin
          if FRange = rsBorAsm then
            FRange := rsAsm
          else
            FRange := rsUnKnown;
          Inc(Run);
          Break;
        end;
      #10,
        #13: Break;
    else
      Inc(Run);
    end;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.ColonProc;

begin
  case FLine[Run + 1] of
    '=':
      begin
        Inc(Run, 2);
        FCurrentToken := tkSymbol;
      end;
  else
    begin
      Inc(Run);
      FCurrentToken := tkSymbol;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.CommaProc;

begin
  Inc(Run);
  FCurrentToken := tkSymbol;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.CRProc;

begin
  FCurrentToken := tkSpace;
  case FLine[Run + 1] of
    #10: Inc(Run, 2);
  else
    Inc(Run);
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.EqualProc;

begin
  Inc(Run);
  FCurrentToken := tkSymbol;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.GreaterProc;

begin
  case FLine[Run + 1] of
    '=':
      begin
        Inc(Run, 2);
        FCurrentToken := tkSymbol;
      end;
  else
    begin
      Inc(Run);
      FCurrentToken := tkSymbol;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.IdentProc;

begin
  FCurrentToken := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while Identifiers[FLine[Run]] do
    Inc(Run);
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.IntegerProc;

begin
  Inc(Run);
  FCurrentToken := tkNumber;
  while FLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do
    Inc(Run);
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.LFProc;

begin
  FCurrentToken := tkSpace;
  Inc(Run);
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.LowerProc;

begin
  case FLine[Run + 1] of
    '=':
      begin
        Inc(Run, 2);
        FCurrentToken := tkSymbol;
      end;
    '>':
      begin
        Inc(Run, 2);
        FCurrentToken := tkSymbol;
      end
  else
    begin
      Inc(Run);
      FCurrentToken := tkSymbol;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.MinusProc;

begin
  Inc(Run);
  FCurrentToken := tkSymbol;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.NullProc;

begin
  FCurrentToken := tkNull;
  FEol := True;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.NumberProc;

begin
  Inc(Run);
  FCurrentToken := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'e', 'E'] do
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

procedure TUCEDelphiHighlighter.PlusProc;

begin
  Inc(Run);
  FCurrentToken := tkSymbol;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.PointerSymbolProc;

begin
  Inc(Run);
  FCurrentToken := tkSymbol;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.PointProc;

begin
  case FLine[Run + 1] of
    '.':
      begin
        Inc(Run, 2);
        FCurrentToken := tkSymbol;
      end;
    ')':
      begin
        Inc(Run, 2);
        FCurrentToken := tkSymbol;
        Dec(FSquareCount);
      end;
  else
    begin
      Inc(Run);
      FCurrentToken := tkSymbol;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.RoundCloseProc;

begin
  Inc(Run);
  FCurrentToken := tkSymbol;
  Dec(FRoundCount);
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.AnsiProc;

begin
  FCurrentToken := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;

    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  while FLine[Run] <> #0 do
    case FLine[Run] of
      '*':
        if FLine[Run + 1] = ')' then
        begin
          if FRange = rsAnsiAsm then
            FRange := rsAsm
          else
            FRange := rsUnKnown;
          Inc(Run, 2);
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

procedure TUCEDelphiHighlighter.RoundOpenProc;

begin
  Inc(Run);
  case FLine[Run] of
    '*':
      begin
        FCurrentToken := tkComment;
        if FRange = rsAsm then
          FRange := rsAnsiAsm
        else
          FRange := rsAnsi;
        Inc(Run);
        while FLine[Run] <> #0 do
          case FLine[Run] of
            '*':
              if FLine[Run + 1] = ')' then
              begin
                if FRange = rsAnsiAsm then
                  FRange := rsAsm
                else
                  FRange := rsUnKnown;
                Inc(Run, 2);
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
    '.':
      begin
        Inc(Run);
        FCurrentToken := tkSymbol;
        Inc(FSquareCount);
      end;
  else
    begin
      FCurrentToken := tkSymbol;
      Inc(FRoundCount);
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.SemiColonProc;

begin
  Inc(Run);
  FCurrentToken := tkSymbol;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.SlashProc;

begin
  case FLine[Run + 1] of
    '/':
      begin
        Inc(Run, 2);
        FCurrentToken := tkComment;
        while FLine[Run] <> #0 do
        begin
          case FLine[Run] of
            #10, #13: Break;
          end;
          Inc(Run);
        end;
      end;
  else
    begin
      Inc(Run);
      FCurrentToken := tkSymbol;
    end;
  end;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.SpaceProc;

begin
  Inc(Run);
  FCurrentToken := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do
    Inc(Run);
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.SquareCloseProc;

begin
  Inc(Run);
  FCurrentToken := tkSymbol;
  Dec(FSquareCount);
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.SquareOpenProc;

begin
  Inc(Run);
  FCurrentToken := tkSymbol;
  Inc(FSquareCount);
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.StarProc;

begin
  Inc(Run);
  FCurrentToken := tkSymbol;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.StringProc;

begin
  FCurrentToken := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then
    Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
    end;
    Inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then
    Inc(Run);
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.SymbolProc;

begin
  Inc(Run);
  FCurrentToken := tkSymbol;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.UnknownProc;

begin
  Inc(Run);
  FCurrentToken := tkUnknown;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.Next;

begin
  FAsmStart := False;
  FTokenPos := Run;
  case FRange of
    rsAnsi: AnsiProc;
    rsAnsiAsm: AnsiProc;
    rsBor: BorProc;
    rsBorAsm: BorProc;
  else
    FProcTable[FLine[Run]];
  end;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.EOL: Boolean;

begin
  Result := False;
  if FCurrentToken = tkNull then
    Result := True;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.GetToken: string;

var
  Len: LongInt;

begin
  Len := Run - FTokenPos;
  SetString(Result, (FLine + FTokenPos), Len);
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.GetTokenInfo: TTokenData;

// gibt für das aktuelle Token einen Zeiger in den momentanen Text zurück (also nicht freigeben)
// und außerdem die Länge des Tokenstrings plus Format Informationen

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
          Background := ColorToRGB(FSymbolAttributes.Background);
          Foreground := ColorToRGB(FSymbolAttributes.Foreground);
          Style := FAsmAttributes.Style;
        end;
    end;
  end;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.GetTokenID: Integer;

begin
  if (FRange = rsAsm) and (not FAsmStart) then
  begin
    case FCurrentToken of
      tkComment: Result := tkComment;
      tkNull: Result := tkNull;
      tkSpace: Result := tkSpace;
    else
      Result := tkAsm;
    end
  end
  else
    Result := FCurrentToken;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.GetRange: Integer;

begin
  Result := Ord(FRange);
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.SetRange(Value: Integer);

begin
  FRange := TRangeState(Value);
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.ReSetRange;

begin
  FRange := rsUnknown;
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.EnumUserSettings(Settings: TStrings);

// Returns the user settings for Delphi and BDS from the Borland key in the registry.

var
  LocalList: TStringList;
  I: Integer;
  
begin
  with TRegistry.Create do
  begin
    LocalList := TStringList.Create;
    try
      RootKey := HKEY_CURRENT_USER;
      if OpenKeyReadOnly('\SOFTWARE\Borland\Delphi') then
      begin
        try
          GetKeyNames(LocalList);
          for I := 0 to LocalList.Count - 1 do
            Settings.Add('Delphi\' + LocalList[I]);
        finally
          CloseKey;
        end;
      end;
      if OpenKeyReadOnly('\SOFTWARE\Borland\BDS') then
      begin
        try
          GetKeyNames(LocalList);
          for I := 0 to LocalList.Count - 1 do
            Settings.Add('BDS\' + LocalList[I]);
        finally
          CloseKey;
        end;
      end;
    finally
      LocalList.Free;
      Free;
    end;
  end;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.UseUserSettings(Path: string): Boolean;

// Reads the settings from the Delphi installation that is given by Path. This value can be returned from EnumUserSettings.
// You can also simply set a fix value for it, e.g. "Delphi/7.0" for Delphi 7 or "BDS/1.0" for Borland Developer Studio.
// This path must be relative to HKLU/Software/Borland.
// Result is True if the settings could be read otherwise False.

var
  Version: Integer;

begin
  Result := Length(Path) > 0;

  if Result then
  begin
    // Version is 1..7 for Delphi 1..7 and 8+ for BDS 2.0+. AFAIK there was never a public BDS 1.0 Version.
    // Kylix is not supported here. It doesn't use the registry anyway.
    // Delphi versions 1..3 aren't supported either here as they use an old variant for the attributes. 
    if Pos('Delphi', Path) = 1 then
      Version := StrToInt(Copy(Path, Length('Delphi\') + 1, 1))
    else
      if Pos('BDS', Path) = 1 then
        Version := StrToInt(Copy(Path, Length('BDS\') + 1, 1)) + 6
      else
        Version := StrToInt(Path[1]);

    Result := Version > 3;
    if Result then
    begin
      Path := '\Software\Borland\' + Path + '\Editor\Highlight';
      if not FAsmAttributes.LoadFromBorlandRegistry(Path + '\Assembler', Version) then
        Result := False;
      if not FCommentAttributes.LoadFromBorlandRegistry(Path + '\Comment', Version) then
        Result := False;
      if not FIdentifierAttributes.LoadFromBorlandRegistry(Path + '\Identifier', Version) then
        Result := False;
      if not FKeyAttributes.LoadFromBorlandRegistry(Path + '\Reserved word', Version) then
        Result := False;
      if not FNumberAttributes.LoadFromBorlandRegistry(Path + '\Number', Version) then
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

procedure TUCEDelphiHighlighter.HighlightChange(Sender: TObject);

begin
  WindowList.Invalidate;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.GetIdentChars: TIdentChars;

begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.GetAttributeCount: Integer;

begin
  Result := 8;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.GetAttribute(Index: Integer): THighlightAttributes;

begin
  case Index of
    0: Result := FAsmAttributes;
    1: Result := FCommentAttributes;
    2: Result := FIdentifierAttributes;
    3: Result := FNumberAttributes;
    4: Result := FKeyAttributes;
    5: Result := FSpaceAttributes;
    6: Result := FStringAttributes;
    7: Result := FSymbolAttributes;
  else
    Result := nil;
  end;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.GetLanguageName: string;

begin
  Result := 'Delphi';
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.GetCapabilities: THighlighterCapabilities;

begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

//--------------------------------------------------------------------------------

procedure TUCEDelphiHighlighter.SetAttribute(const Index: Integer; const Value: THighlightAttributes);

begin
  case Index of
    0: FAsmAttributes.Assign(Value);
    1: FCommentAttributes.Assign(Value);
    2: FIdentifierAttributes.Assign(Value);
    3: FNumberAttributes.Assign(Value);
    4: FKeyAttributes.Assign(Value);
    5: FSpaceAttributes.Assign(Value);
    6: FStringAttributes.Assign(Value);
    7: FSymbolAttributes.Assign(Value);
  end;
end;

//--------------------------------------------------------------------------------

procedure MakeIdentTable;

var
  I, J: Char;

begin
  for I := #0 to #255 do
  begin
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else
      Identifiers[I] := False;
    end;
    J := UpperCase(I)[1];
    case I of
      'a'..'z', 'A'..'Z', '_': HashTable[I] := Ord(J) - 64;
    else
      HashTable[Char(I)] := 0;
    end;
  end;
end;

//--------------------------------------------------------------------------------

function TUCEDelphiHighlighter.HasMoreTokens: Boolean;

begin
  Result := FCurrentToken <> tkNull;
end;

//--------------------------------------------------------------------------------

initialization
  MakeIdentTable;
end.

