unit LexicalTools;

//----------------------------------------------------------------------------------------------------------------------
// Version 1.0.0
//
// LexicalTools is a unit, which contains support classes and methods for tokenizing, parsing and manipulating text.
//
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
// The original code is LexicalTools.pas, released 1. July 2003.
//
// The initial developer of the original code is:
//   Dipl. Ing. Mike Lischke, Delphi Gems software solutions (public@delphi-gems.com, www.delphi-gems.com).
//
// Portions created by Delphi Gems are
//   (C) 1999-2003 Delphi Gems. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------

interface

{$Include Compilers.inc}

{$ifdef COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
  // We switch off warnings about that fact. We know it and we accept it.
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CAST off}
  {$warn UNSAFE_CODE off}
{$endif COMPILER_7_UP}

uses
  Windows, Classes;
                                   
const
  // Tokens returned used with the Delphi parser.
  toEOF = #0;
  toBOF = #1;     // Set if the parser has not yet read the first token.
  toSymbol = #2;
  toString = #3;
  toInteger = #4;
  toFloat = #5;

  ParseBufferSize = 4096;

  // Some character classes.
  IdentifierStart = ['_', 'A'..'Z', 'a'..'z'];
  Number = ['0'..'9'];
  IdentifierPart = IdentifierStart + Number;
  HexNumber = Number + ['A'..'F', 'a'..'f'];
  WhiteSpace = [' ', #9];

type
  // This helper class is used to tokenize Delphi source code.
  TDelphiParser = class;

  TNewLineEvent = procedure(Sender: TDelphiParser; Line: Cardinal) of object;
  TParseErrorEvent = procedure(Pos: TPoint; const Error: string) of object;

  TDelphiParser = class(TObject)
  private
    FStream: TStream;
    FOrigin: Integer;
    FBuffer: PChar;
    FBufPtr: PChar;
    FBufEnd: PChar;
    FSourcePtr: PChar;
    FSourceEnd: PChar;
    FTokenPtr: PChar;
    FLineStart: PChar;
    FSourceLine: Cardinal;
    FSaveChar: Char;
    FToken: Char;
    FFloatType: Char;
    FTokenString: string;

    FOnNewLine: TNewLineEvent;
    FOnParseError: TParseErrorEvent;
  protected
    procedure AdvanceSource(Amount: Cardinal = 1);
    procedure DoNewLine;
    function GetToken: Char;
    function NeedBytes(Count: Cardinal): Boolean;
    procedure ReadBuffer;
    function ScanIdentifier: string;
    procedure SkipBlanks;
    procedure SkipComments;
    procedure SkipLine;
    procedure SkipString;
    procedure SkipUntil(S: string);
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;

    procedure CheckToken(T: Char);
    procedure CheckTokenSymbol(const S: string);
    function NextToken: Char;
    procedure ParseError(const Message: string);
    procedure ShowToken(Token: Char);
    function SourcePos: Integer;
    function TokenComponentIdent: string;
    function TokenFloat: Extended;
    function TokenInt64: Int64;
    function TokenInteger: Integer;
    function TokenString: string;
    function TokenSymbolIs(const S: string): Boolean;

    property FloatType: Char read FFloatType;
    property SourceLine: Cardinal read FSourceLine;
    property Token: Char read GetToken;

    property OnNewLine: TNewLineEvent read FOnNewLine write FOnNewLine;
    property OnParseError: TParseErrorEvent read FOnParseError write FOnParseError;
  end;

function AddToUsesClause(Input, Output: TStream; UnitName, Addition: string; InterfaceSection: Boolean): Boolean;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  SysUtils, RTLConsts;

const
  // Do not modify the copyright in any way! Usage of this unit is prohibited without the copyright notice
  // in the compiled binary file.
  Copyright: string = 'Lexical tools © 1999-2003 Mike Lischke, Delphi Gems software solutions';

resourcestring
  SUnexpectedEOF = 'Unexpected end of file.';
  SInvalidSourceCode = 'Source code is invalid. Cannot modify uses clause.';

//----------------- Helper functions -----------------------------------------------------------------------------------

function StrLScan(Str: PChar; Chr: Char; StrLen: Cardinal): PChar;

// Returns a pointer to first occurrence of a specified character in a string
// or nil if not found.
//
// On enter EAX contains Str, EDX contains Chr and ECX StrLen.
// On exit EAX contains result pointer or nil.

asm
       TEST    EAX, EAX
       JZ      @@Exit
       JCXZ    @@Exit        // Nothing to do if the string is nil or StrLen is 0.

@@Loop:
       CMP     [EAX], DL     // This unrolled loop is actually faster on modern processors
       JE      @@Exit        // than REP SCASB.
       INC     EAX
       DEC     ECX
       JNZ     @@Loop
       XOR     EAX, EAX

@@Exit:
end;

//----------------- TDelphiParser --------------------------------------------------------------------------------

constructor TDelphiParser.Create(Stream: TStream);

begin
  FStream := Stream;
  GetMem(FBuffer, ParseBufferSize);
  FBuffer[0] := #0;
  FBufPtr := FBuffer;
  FBufEnd := FBuffer + ParseBufferSize;
  FSourcePtr := FBuffer;
  FSourceEnd := FBuffer;
  FTokenPtr := FBuffer;

  FLineStart := FSourcePtr;
  FSourceLine := 1;
  FToken := toBOF;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TDelphiParser.Destroy;

begin
  FStream.Seek(Integer(FTokenPtr) - Integer(FBufPtr), 1);
  FreeMem(FBuffer);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiParser.AdvanceSource(Amount: Cardinal = 1);

begin
  NeedBytes(Amount);
  Inc(FSourcePtr, Amount);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiParser.DoNewLine;

begin
  Inc(FSourceLine);
  FLineStart := FSourcePtr + 1;
  if Assigned(FOnNewLine) then
    FOnNewLine(Self, FSourceLine);
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiParser.GetToken: Char;

begin
  if FToken = toBOF then
    NextToken;
  Result := FToken;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiParser.NeedBytes(Count: Cardinal): Boolean;

// Ensure that at least Count bytes are available in the current input buffer if possible.
// If less input is available than Count bytes then Result is set to False.

begin
  Result := True;

  // check if we need to load new data
  if Count > Cardinal(FSourceEnd - FSourcePtr) then
  begin
    ReadBuffer;
    Result := Count <= Cardinal(FBufEnd - FSourcePtr);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiParser.ReadBuffer;

var
  Count: Integer;

begin
  Inc(FOrigin, FSourcePtr - FBuffer);
  FSourceEnd^ := FSaveChar;
  Count := FBufPtr - FSourcePtr;
  if Count <> 0 then
    Move(FSourcePtr^, FBuffer^, 2 * Count);
  FBufPtr := FBuffer + Count;

  Count := FStream.Read(FBuffer^, FBufEnd - FBufPtr);
  Inc(FBufPtr, Count);

  FSourcePtr := FBuffer;
  FSourceEnd := FBufPtr;
  if FSourceEnd = FBufEnd then
  begin
    FSourceEnd := LineStart(FBuffer, FSourceEnd - 1);
    if FSourceEnd = FBuffer then
      ParseError(SLineTooLong);
  end;
  FSaveChar := FSourceEnd^;
  FSourceEnd^ := #0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiParser.ScanIdentifier: string;

var
  P: PChar;

begin
  P := FSourcePtr;
  while P^ in IdentifierPart do
    Inc(P);
  SetString(Result, FSourcePtr, P - FSourcePtr);
  FSourcePtr := P;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiParser.SkipBlanks;

begin
  while True do
  begin
    case FSourcePtr^ of
      #0:
        begin
          ReadBuffer;
          if FSourcePtr^ = #0 then
            Break;
          Continue;
        end;
      #10:
        DoNewLine;
      #13:
        // Increase line counter only for Macintosh style text (CR only)
        // otherwise just ignore CR characters.
        if NeedBytes(2) and ((FSourcePtr + 1)^ <> #10) then
          DoNewLine;
    else
      if not (FSourcePtr^ in WhiteSpace) then
        Break;
    end;
    AdvanceSource;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiParser.SkipComments;

// Skips any Delphi style comment. Conditional defines are interpreted as comment too.

begin
  repeat
    SkipBlanks;
    case FSourcePtr^ of
      '{': // Multi line comment first type.
        begin
          SkipUntil('}');
          if FSourcePtr^ = #0 then
            ParseError(SUnexpectedEOF)
          else
            AdvanceSource;
        end;                                
      '/':
        begin
          if NeedBytes(2) and ((FSourcePtr + 1)^ = '/') then
          begin
            // Single line comment, skip to end of line (or input end).
            SkipUntil(#10#13);
          end
          else
            Break; // Some other construct but not a comment.
        end;
      '(':
        begin
          if NeedBytes(2) and ((FSourcePtr + 1)^ = '*') then
          begin
            // Multi line comment second type, skip to '*)' combination.
            repeat
              SkipUntil('*');
              if NeedBytes(2) and (FSourcePtr^ = '*') and ((FSourcePtr + 1)^ = ')') then
                Break
              else
                if FSourcePtr^ <> #0 then
                  AdvanceSource;
            until FSourcePtr^ = #0;
            if FSourcePtr^ = #0 then
              ParseError(SUnexpectedEOF)
            else
              AdvanceSource(2);
          end
          else
            Break;
        end;
    else
      Break; // no more comments, get out of here
    end;
  until False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiParser.SkipLine;

// Skips all characters until the end of line.

begin
  AdvanceSource;
  SkipUntil(#13#10);
  SkipBlanks;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiParser.SkipString;

// Skips a Delphi string.
// On enter FSourcePtr points to the initial ' character on exit it points to the first character after the terminating
// ' character.

begin
  repeat
    AdvanceSource;
    // find next apostrophe
    SkipUntil('''');
    // advance to position after apostrophe
    if FSourcePtr^ = '''' then
      AdvanceSource;
    // is there another apostrophe then continue looking for string end, otherwise get out of here
    if FSourcePtr^ <> '''' then
      Break;
  until False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiParser.SkipUntil(S: string);

// Skips all characters until one of the characters in S is found or the source is exhausted.

var
  L: Cardinal;
  Ch: Char;

begin
  L := Length(S);
  if L = 1 then
  begin
    // optimized version for single char
    Ch := S[1];
    while FSourcePtr^ <> Ch do
    begin
      case FSourcePtr^ of
        #0:
          begin
            ReadBuffer;
            if FSourcePtr^ = #0 then
              Exit;
            Continue;
          end;
        #10:
          DoNewLine;
        #13:
          // Increase line counter only for Macintosh style text (CR only)
          // otherwise just ignore CR characters.
          if NeedBytes(2) and ((FSourcePtr + 1)^ <> #10) then
            DoNewLine;
      end;
      AdvanceSource;
    end;
  end
  else
  begin
    // Check for all characters in S.
    while StrLScan(PChar(S), FSourcePtr^, L) = nil do
    begin
      case FSourcePtr^ of
        #0:
          begin
            ReadBuffer;
            if FSourcePtr^ = #0 then
              Exit;
          end;
        #10:
          DoNewLine;
        #13:
          // Increase line counter only for Macintosh style text (CR only)
          // otherwise just ignore CR characters.
          if NeedBytes(2) and ((FSourcePtr + 1)^ <> #10) then
            DoNewLine;
      end;
      AdvanceSource;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiParser.CheckToken(T: Char);

begin
  if Token <> T then
    case T of
      toSymbol:
        ParseError(SIdentifierExpected + ' but ''' + Token + ''' found');
      toString:
        ParseError(SStringExpected + ' but ''' + Token + ''' found');
      toInteger, toFloat:
        ParseError(SNumberExpected + ' but ''' + Token + ''' found');
    else
      ParseError('"' + T + '" expected but ' + Token + ' found');
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiParser.CheckTokenSymbol(const S: string);

begin
  if not TokenSymbolIs(S) then
    ParseError('"' + S + '" expected but "' + TokenString + '" found');
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiParser.NextToken: Char;

// Advances the parser to the next valid token.

var
  I, J: Integer;
  P, S: PChar;

begin
  Result := toEOF;

  // Loop until a valid token has been found.
  while True do
  begin
    // First remove heading comments (this will automatically skip blanks too).
    SkipComments;

    // At this point we have the start of a valid token.
    P := FSourcePtr;
    FTokenPtr := P;
    FTokenString := '';

    // Is there a symbol?
    if P^ in IdentifierStart then
    begin
      Inc(P);
      while P^ in identifierPart do
        Inc(P);
      Result := toSymbol;
    end
    else
    begin
      case P^ of
        '#', '''', '"': // string
          begin
            J := 0;
            S := P;
            while True do
              case P^ of
                '#':
                  begin
                    Inc(P);
                    I := 0;
                    while P^ in Number do
                    begin
                      I := I * 10 + (Ord(P^) - Ord('0'));
                      Inc(P);
                    end;
                    Inc(J);
                  end;
                '"': // Have to allow " " too. This is not valid Delphi syntax, but used in some VCL assembler code.
                  begin
                    Inc(P);
                    while not (P^ in [#0, #10, #13, '"']) do
                    begin
                      Inc(P);
                      Inc(J);
                    end;
                    if P^ <> '"' then
                      ParseError(SInvalidString)
                    else
                      Inc(P);
                  end;
                '''':
                  begin
                    Inc(P);
                    while True do
                    begin
                      case P^ of
                        #0, #10, #13:
                          ParseError(SInvalidString);
                        '''':
                          begin
                            Inc(P);
                            if P^ <> '''' then
                              Break;
                          end;
                      end;
                      Inc(J);
                      Inc(P);
                    end;
                  end;
              else
                Break;
              end;
            P := S;
            SetLength(FTokenString, J);
            J := 1;
            while True do
              case P^ of
                '#':
                  begin
                    Inc(P);
                    I := 0;
                    while P^ in Number do
                    begin
                      I := I * 10 + (Ord(P^) - Ord('0'));
                      Inc(P);
                    end;
                    FTokenString[J] := Char(Word(I));
                    Inc(J);
                  end;
                '"': // Have to allow " " too. This is not valid Delphi syntax, but used in some VCL assembler code.
                  begin
                    Inc(P);
                    while not (P^ in [#0, #10, #13, '"']) do
                    begin
                      FTokenString[J] := P^;
                      Inc(P);
                      Inc(J);
                    end;
                    if P^ <> '"' then
                      ParseError(SInvalidString)
                    else
                      Inc(P);
                  end;
                '''':
                  begin
                    Inc(P);
                    while True do
                    begin
                      case P^ of
                        #0, #10, #13:
                          ParseError(SInvalidString);
                        '''':
                          begin
                            Inc(P);
                            if P^ <> '''' then
                              Break;
                          end;
                      end;
                      FTokenString[J] := P^;
                      Inc(J);
                      Inc(P);
                    end;
                  end;
              else
                Break;
              end;
            Result := toString;
          end;
        '$': // hex number
          begin
            Inc(P);
            while P^ in HexNumber do
              Inc(P);
            Result := toInteger;
          end;
        '-', '0'..'9': // Integer, float number or Integer subrange of which the lower bound will be returned
          begin
            Inc(P);
            while P^ in Number do
              Inc(P);
            Result := toInteger;

            // if we have a subrange then we are done, otherwise scan further
            if not ((P^ = '.') and NeedBytes(2) and ((P + 1)^ = '.')) then
            begin
              if P^ in ['.', 'e', 'E'] then
              begin
                // it is actually a floating point value
                Result := toFloat;

                // skip irrelevant period character if directly followed by exponent character
                if (P^ = '.') and NeedBytes(2) and ((P + 1)^ in ['e', 'E']) then
                  Inc(P);
                // skip exponent letter if there is one followed by a plus or minus sign
                if (P^ in ['e', 'E']) and NeedBytes(2) and
                  ((P + 1)^ in ['+', '-']) then
                  Inc(P);

                // skip whatever left over, the period, the expontent symbol or the sign 
                Inc(P);
                while P^ in ['0'..'9'] do
                  Inc(P);

                if (P^ in ['c', 'C', 'd', 'D', 's', 'S']) then
                begin
                  Result := toFloat;
                  FFloatType := P^;
                  Inc(P);
                end
                else
                  FFloatType := #0;
              end;
            end;
          end;
      else
        // Any other symbol not consumed above.
        Result := P^;
        if Result <> toEOF then
          Inc(P);
      end;
    end;
    FSourcePtr := P;  
    FToken := Result;
    Break; // If we come here then we have a valid token and can return.
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiParser.ParseError(const Message: string);

// Shows an error message.

var
  S: string;

begin
  if Assigned(FOnParseError) then
    FOnParseError(Point(FSourcePtr - FLineStart + 1, FSourceLine), Message) // 1 based position
  else
  begin
    S := Message + ' on line ' + IntToStr(FSourceLine);
    MessageBox(0, PChar(S), 'Parse Error', MB_OK or MB_ICONERROR or MB_APPLMODAL);
  end;
  Abort;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiParser.ShowToken(Token: Char);

// Used for debugging purposes and shows a message box with a description string of the current token along
// with its value.

var
  TokenName: string;

begin
  case Token of
    toEOF:
      TokenName := '(end of file)';
    toBOF:
      TokenName := '(begin of file)';
    toSymbol:
      TokenName := '(symbol) = "' + TokenString + '"';
    toString:
      TokenName := '(string) = "' + TokenString + '"';
    toInteger:
      TokenName := '(Integer) = "' + TokenString + '"';
    toFloat:
      TokenName := '(float) = "' + TokenString + '"';
  else
    TokenName := Format('(other) = "%s" (hex: %X)', [Token, Ord(Token)]) ;
  end;
  MessageBox(0, PChar(TokenName), 'Current token', MB_OK or MB_APPLMODAL);
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiParser.SourcePos: Integer;

begin
  Result := FOrigin + (FTokenPtr - FBuffer);
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiParser.TokenComponentIdent: string;

var
  P: PChar;

begin
  CheckToken(toSymbol);
  P := FSourcePtr;
  while P^ = '.' do
  begin
    Inc(P);
    if not (P^ in IdentifierStart) then
      ParseError(SIdentifierExpected);
    repeat
      Inc(P)
    until not (P^ in identifierPart);
  end;
  FSourcePtr := P;
  Result := TokenString;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiParser.TokenFloat: Extended;

begin
  if FFloatType <> #0 then
    Dec(FSourcePtr);
  Result := StrToFloat(TokenString);
  if FFloatType <> #0 then
    AdvanceSource;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiParser.TokenInt64: Int64;

begin
  Result := StrToInt64(TokenString);
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiParser.TokenInteger: Integer;

begin
  Result := StrToInt(TokenString);
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiParser.TokenString: string;

begin
  if FToken = toString then
    Result := FTokenString
  else
    SetString(Result, FTokenPtr, FSourcePtr - FTokenPtr);
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiParser.TokenSymbolIs(const S: string): Boolean;

begin
  Result := (Token = toSymbol) and (StrIComp(PChar(S), PChar(TokenString)) = 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function AddToUsesClause(Input, Output: TStream; UnitName, Addition: string; InterfaceSection: Boolean): Boolean;

// This function reads text from Input (which must contain a Delphi source file, e.g. unit, program or library) and
// parses this text to find the included uses clause.
// If Input is a library or progam source code then the uses clause must directly follow the lib or app name sequence or
// it is considered as not being there. For units the uses clause must directly follow the interface or implementation
// keyword if present.
//
// Addition is added directly after the unit name and can be used to insert comments or to complete the
//   Unitname in 'UnitName.pas' {some comment}
// syntax, which is allowed in library and program source code (but not units).
//
// InterfaceSection can be used to specify (in units only) whether to add the new unit to either the interface section
// uses clause (if True) or the implementation uses clause (if False).
// After the unit name (and perhaps the uses keyword) has been included the entire input is written to output. 

const
  NullChar: Char = #0;

var
  Parser: TDelphiParser;
  Index: Integer;
  UsesList: TStringList;
  IsUnit: Boolean;
  
begin
  Result := False;

  // Remove extension if there is one.
  Index := Pos('.', UnitName);
  if Index > 0 then
    Delete(UnitName, Index, MaxInt);

  Input.Position := 0;
  Parser := TDelphiParser.Create(Input);
  try
    UsesList := TStringList.Create;
    try
      UsesList.Sorted := True;
      UsesList.Duplicates := dupError;

      // Find interface keyword.
      with Parser do
      begin
        NextToken;
        if not (TokenSymbolIs('program') or TokenSymbolIs('library') or TokenSymbolIs('unit')) then
          raise Exception.Create(SInvalidSourceCode);

        // Only units have an interface and implementation section. Additionally, in libraries and application sources
        // each unit is written on an own line, usually. We do this too.
        IsUnit := TokenSymbolIs('unit');
        if IsUnit then
        begin
          repeat
            NextToken;
          until TokenSymbolIs('interface');

          // If only the interface uses clause is to be modified then we can start scanning the uses clause here,
          // otherwise advance to the implementation section.
          if not InterfaceSection then
          begin
            repeat
              NextToken;
            until TokenSymbolIs('implementation');
          end;
        end
        else
        begin
          // Skip library/program name and semicolon.
          NextToken;
          NextToken;
        end;

        // Now find and parse the uses clause.
        // Note that there might not yet be a uses clause, but the Delphi language specification says if there is
        // a uses clause then it must follow directly after the interface/implementation keyword.
        NextToken;
        if not TokenSymbolIs('uses') then
        begin
          // No uses clause there yet. Simply add it and the new unit reference.
          Input.Position := 0;
          // Copy everything until the interface/implementation keyword.
          Output.CopyFrom(Input, SourcePos);
          Output.Write('uses'#13#10 + '  ', 8);
          Output.Write(PChar(UnitName + ';' + #13#10#13#10)^, Length(UnitName) + 5);
          // Copy rest of the input stream.
          Output.CopyFrom(Input, Input.Size - Input.Position);
          Output.Write(NullChar, 1);
          Result := True;
        end
        else
        begin
          // There is already a uses clause. Parse this and check if the unit name to be added is already there or not.
          repeat
            // Advance to next unit name.
            NextToken;
            if Token <> ';' then
            begin
              // Check the correct token type.
              CheckToken(toSymbol);
              UsesList.Add(TokenString);
              NextToken;
              // Check if the unit name is followed by "in 'Filename'".
              if TokenSymbolIs('in') then
              begin
                NextToken;
                CheckToken(toString);
                NextToken;
              end;
              if not (Token in [',', ';']) then
                raise Exception.Create(SInvalidSourceCode);
            end;
          until Token = ';';

          // If the unit is not already in the uses list then add it now.
          if not UsesList.Find(UnitName, Index) then
          begin
            Input.Position := 0;
            // Copy everything until the interface/implementation keyword plus the existing uses list.
            // Do not copy the semicolon yet.
            Output.CopyFrom(Input, SourcePos);
            if IsUnit then
              Output.Write(PChar(', ' + UnitName)^, Length(UnitName) + 2)
            else
            begin
              Output.Write(PChar(','#13#10 + '  ' + UnitName)^, Length(UnitName) + 5);
              Output.Write(PChar(Addition)^, Length(Addition));
            end;
            // Copy rest of the input stream.
            Output.CopyFrom(Input, Input.Size - Input.Position);
            Output.Write(NullChar, 1);

            Result := True;
          end;
        end;
      end;
    finally
      UsesList.Free;
    end;
  finally
    Parser.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
