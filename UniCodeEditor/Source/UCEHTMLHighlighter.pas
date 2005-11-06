unit UCEHTMLHighlighter;

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
//   This unit contains the implementation of a syntax highlighter for the Unicode Syntax Edit control (USE).
//   The code below has been created by DCG, the Delphi Compiler Generator.
//
// Changes:
//   Version 2.0, 2003-08-17, Mike Zinner
//     Repackaged for D7
//   Version 1.0, 1999-03-10, Mike Lischke
//     Initial Version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses Classes, Graphics, UCEHighlighter;

const
  CR = #13;
  LF = #10;

// user defined start states and tokens

const
  Normal = 2;
  UnknownAttrs = 4;
  AttrParm = 6;
  Comment = 8;
  CommentAlt = 10;
  Script = 12;

const
  DOCTYPE = 257;
  FLOATNUMBER = 258;
  IDENTIFIER = 259;
  INTEGERNUMBER = 260;
  KEYWORD = 261;
  MLCOMMENT = 262;
  NAMETOKEN = 263;
  SCRIPTPART = 264;
  STRINGCONSTANT = 265;
  SYMBOL = 266;
  UNKNOWN = 267;
  UNKNOWNEND = 268;
  UNKNOWNPARM = 269;
  UNKNOWNPSEUDO = 270;
  UNKNOWNPSEUDOEND = 271;
  URLSTRING = 272;
  WHITESPACE = 273;

type
  TUCEHTMLHighlighter = class(TUCEHighlighter)
  private
    FCurrentToken: Integer; // type of current token
    FInput: PChar; // input string to be tokenized
    FCurrentPos: Cardinal; // current lexer position in FInput
    FTokenPos: Cardinal; // current token position in FInput
    FLookahead: Char; // next char after current position
    FCurrentState,
      FStartState,
      FLineState: Cardinal; // lexer states
    FLastChar: Char; // last matched char (#0 if no match)
    FPositions: array of Cardinal; // for each rule the last marked position, zeroed when
                                       // rule has already been considered (allocated once)
    FMatchStack: array of Cardinal; // stack containing matched rules (growing dynamically)
    FMatchCount: Integer; // contains current number of matches
    FRejected: Boolean; // current match rejected?
    FDoStop: Boolean; // token or eof found?
    FEOL: Boolean;

    // !! adjust attributes !!
    FIdentifierAttributes,
      FNumberAttributes,
      FSpaceAttributes,
      FStringAttributes,
      FCommentAttributes,
      FSymbolAttributes,
      FKeyAttributes,
      FUnknownAttributes,
      FParamAttributes,
      FURLAttributes,
      FDocTypeAttributes,
      FNameAttributes,
      FScriptAttributes: THighlightAttributes;

    procedure RuleToToken(Rule: Integer);
    function GetNextChar: Char;
    function MatchRule(var Rule: Integer): Boolean;
    function DoDefault: Boolean;
    function GetCurrentChar: Char;
    procedure SetAttribute(const Index: Integer; const Value: THighlightAttributes);
    procedure HighlightChange(Sender: TObject);
  protected
    procedure Reset; virtual;
    procedure SetMatch(I: Integer);
    procedure SetState(Value: Cardinal); virtual;
    procedure SetToken(Token: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function EOL: Boolean; override;
    function GetToken: string; override;
    function GetTokenInfo: TTokenData; override;
    procedure Next; override;
    procedure Reject;
    function GetAttribute(Index: Integer): THighlightAttributes; override;
    function GetAttributeCount: Integer; override;
    function GetIdentChars: TIdentChars; override;
    function GetLanguageName: string; override;
    function GetRange: Integer; override;
    function IsKeyword: Boolean;
    procedure ResetRange; override;
    procedure SetRange(Value: Integer); override;
    procedure SetLine(const NewValue: string); override;

    property CurrentChar: Char read GetCurrentChar;
    property NextChar: Char read GetNextChar;
    property Lookahead: Char read FLookahead;
    property State: Cardinal read FCurrentState write SetState; // read to get current lexer state
                                                                // write to set start state for next run
    property TokenKind: Integer read FCurrentToken;
    property TokenPosition: Cardinal read FTokenPos;
  published
    // !! adjust attributes !!
    property IdentifierAttributes: THighlightAttributes index 0 read FIdentifierAttributes write SetAttribute;
    property NumberAttributes: THighlightAttributes index 1 read FNumberAttributes write SetAttribute;
    property SpaceAttributes: THighlightAttributes index 2 read FSpaceAttributes write SetAttribute;
    property StringAttributes: THighlightAttributes index 3 read FStringAttributes write SetAttribute;
    property CommentAttributes: THighlightAttributes index 4 read FCommentAttributes write SetAttribute;
    property SymbolAttributes: THighlightAttributes index 5 read FSymbolAttributes write SetAttribute;
    property KeyAttributes: THighlightAttributes index 6 read FKeyAttributes write SetAttribute;
    property UnknownAttributes: THighlightAttributes index 7 read FUnknownAttributes write SetAttribute;
    property ParamAttributes: THighlightAttributes index 8 read FParamAttributes write SetAttribute;
    property URLAttributes: THighlightAttributes index 9 read FURLAttributes write SetAttribute;
    property DocTypeAttributes: THighlightAttributes index 10 read FDocTypeAttributes write SetAttribute;
    property NameAttributes: THighlightAttributes index 11 read FNameAttributes write SetAttribute;
    property ScriptAttributes: THighlightAttributes index 12 read FScriptAttributes write SetAttribute;
  end;

//--------------------------------------------------------------------------------

implementation

uses SysUtils;

// DFA table
type
  TTransition = record
    CharClass: set of Char;
    NextState: Integer;
  end;

const
  MarkPositionCount = 142;
  MatchCount = 142;
  TransitionCount = 429;
  StateCount = 163;

  MarkPositionTable: array[0..MarkPositionCount - 1] of Integer = (
    36, 0, 11, 12, 15, 15, 13, 14, 15, 16, 17, 28, 28, 28, 28, 28, 30, 30, 31, 32,
    33, 35, 35, 2, 7, 7, 5, 12, 15, 12, 17, 21, 28, 17, 28, 21, 28, 28, 27, 27,
    27, 25, 27, 19, 28, 25, 28, 30, 35, 1, 2, 7, 4, 7, 9, 12, 15, 21, 28, 21, 18,
    27, 27, 27, 27, 26, 26, 28, 30, 35, 1, 7, 8, 7, 6, 9, 9, 12, 21, 28, 18, 22,
    27, 22, 27, 27, 27, 20, 27, 27, 28, 30, 35, 7, 8, 9, 12, 21, 27, 22, 27, 28,
    30, 35, 7, 8, 10, 21, 27, 28, 30, 35, 3, 7, 27, 28, 35, 24, 27, 23, 23, 28,
    29, 30, 35, 24, 27, 34, 35, 28, 28, 30, 27, 27, 27, 27, 27, 28, 28, 27, 28,
    27);

  MatchTable: array[0..MatchCount - 1] of Integer = (
    36, 0, 11, 12, 15, 15, 13, 14, 15, 16, 17, 28, 28, 28, 28, 28, 30, 30, 31, 32,
    33, 35, 35, 2, 7, 7, 5, 12, 15, 12, 17, 21, 28, 17, 28, 21, 28, 28, 27, 27,
    27, 25, 27, 19, 28, 25, 28, 30, 35, 1, 2, 7, 4, 7, 9, 12, 15, 21, 28, 21, 18,
    27, 27, 27, 27, 26, 26, 28, 30, 35, 1, 7, 8, 7, 6, 9, 9, 12, 21, 28, 18, 22,
    27, 22, 27, 27, 27, 20, 27, 27, 28, 30, 35, 7, 8, 9, 12, 21, 27, 22, 27, 28,
    30, 35, 7, 8, 10, 21, 27, 28, 30, 35, 3, 7, 27, 28, 35, 24, 27, 23, 23, 28,
    29, 30, 35, 24, 27, 34, 35, 28, 28, 30, 27, 27, 27, 27, 27, 28, 28, 27, 28,
    27);

  TransitionTable: array[0..TransitionCount - 1] of TTransition = (
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..'%', ''''..';', '='..#255]; NextState: 15),
    (CharClass: ['&']; NextState: 16),
    (CharClass: ['<']; NextState: 17),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..'%', ''''..';', '='..#255]; NextState: 15),
    (CharClass: ['&']; NextState: 16),
    (CharClass: ['<']; NextState: 17),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!'..'<', '?'..'@', '[', ']'..'^', '`', '{'..#127, #166..#255]; NextState:
      23),
    (CharClass: [#9..#10, #13, ' ']; NextState: 18),
    (CharClass: ['=']; NextState: 21),
    (CharClass: ['>']; NextState: 22),
    (CharClass: ['A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 19),
    (CharClass: ['\']; NextState: 20),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!'..'<', '?'..'@', '[', ']'..'^', '`', '{'..#127, #166..#255]; NextState:
      23),
    (CharClass: [#9..#10, #13, ' ']; NextState: 18),
    (CharClass: ['=']; NextState: 21),
    (CharClass: ['>']; NextState: 22),
    (CharClass: ['A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 19),
    (CharClass: ['\']; NextState: 20),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '$'..'&', '('..'/', ':'..'=', '?'..'@', '['..'`', '{'..#255];
      NextState: 30),
    (CharClass: [#9..#10, #13, ' ']; NextState: 24),
    (CharClass: ['"']; NextState: 26),
    (CharClass: ['#']; NextState: 27),
    (CharClass: ['''']; NextState: 29),
    (CharClass: ['0'..'9']; NextState: 25),
    (CharClass: ['A'..'Z', 'a'..'z']; NextState: 28),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '$'..'&', '('..'/', ':'..'=', '?'..'@', '['..'`', '{'..#255];
      NextState: 30),
    (CharClass: [#9..#10, #13, ' ']; NextState: 24),
    (CharClass: ['"']; NextState: 26),
    (CharClass: ['#']; NextState: 27),
    (CharClass: ['''']; NextState: 29),
    (CharClass: ['0'..'9']; NextState: 25),
    (CharClass: ['A'..'Z', 'a'..'z']; NextState: 28),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..'=', '?'..'C', 'E'..'c', 'e'..#255]; NextState: 32),
    (CharClass: ['>']; NextState: 33),
    (CharClass: ['D', 'd']; NextState: 31),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..'=', '?'..'C', 'E'..'c', 'e'..#255]; NextState: 32),
    (CharClass: ['>']; NextState: 33),
    (CharClass: ['D', 'd']; NextState: 31),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..'=', '?'..#255]; NextState: 34),
    (CharClass: ['>']; NextState: 35),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..'=', '?'..#255]; NextState: 34),
    (CharClass: ['>']; NextState: 35),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..#9, #11..';', '='..#255]; NextState: 36),
    (CharClass: ['<']; NextState: 37),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..#9, #11..';', '='..#255]; NextState: 36),
    (CharClass: ['<']; NextState: 37),
    (CharClass: [#1..'%', ''''..';', '='..#255]; NextState: 15),
    (CharClass: ['#']; NextState: 38),
    (CharClass: ['A'..'Z', 'a'..'z']; NextState: 39),
    (CharClass: ['!']; NextState: 43),
    (CharClass: ['/']; NextState: 44),
    (CharClass: ['A'..'R', 'T'..'Z', '_', 'a'..'r', 't'..'z', #128..#165]; NextState: 41),
    (CharClass: ['S', 's']; NextState: 40),
    (CharClass: ['\']; NextState: 42),
    (CharClass: [#9..#10, #13, ' ']; NextState: 18),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!'..',', '.'..'/', ':'..'<', '?'..'@', '[', ']'..'^', '`', '{'..#127,
    #166..#255]; NextState: 23),
    (CharClass: ['-', '0'..'9', 'A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 19),
    (CharClass: ['\']; NextState: 20),
    (CharClass: [#1..#8, #11..#12, #14..#31, #127, #166..#255]; NextState: 23),
    (CharClass: [' ', '='..'>']; NextState: 46),
    (CharClass: ['!'..'/', ':'..'<', '?'..'@', 'G'..'`', 'g'..'~', #128..#165]; NextState: 19),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 45),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!'..'<', '?'..#255]; NextState: 23),
    (CharClass: [#9..#10, #13, ' ']; NextState: 24),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'$', '&'..',', '.'..'/', ':'..'=', '?'..'@', '[', ']'..'^',
    '`', '{'..#127, #166..#255]; NextState: 30),
    (CharClass: ['%']; NextState: 48),
    (CharClass: ['-', 'A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 49),
    (CharClass: ['0'..'9']; NextState: 47),
    (CharClass: ['\']; NextState: 50),
    (CharClass: [#1..'!', '$'..'/', ':'..'=', '?'..'@', '['..'`', '{'..#255]; NextState: 55),
    (CharClass: ['"']; NextState: 54),
    (CharClass: ['#']; NextState: 52),
    (CharClass: ['0'..'9']; NextState: 51),
    (CharClass: ['A'..'Z', 'a'..'z']; NextState: 53),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'/', ':'..'=', '?'..'@', 'G'..'`', 'g'..#255]; NextState: 30),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 56),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'=', '?'..'@', '['..'`', '{'..#255]; NextState: 30),
    (CharClass: ['A'..'Z', 'a'..'z']; NextState: 161),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'&', '('..'=', '?'..#255]; NextState: 151),
    (CharClass: [#9..#10, #13, ' ', '"']; NextState: 58),
    (CharClass: ['''']; NextState: 57),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'=', '?'..#255]; NextState: 30),
    (CharClass: [#1..'=', '?'..'N', 'P'..'n', 'p'..#255]; NextState: 32),
    (CharClass: ['O', 'o']; NextState: 59),
    (CharClass: [#1..'=', '?'..#255]; NextState: 32),
    (CharClass: [#1..'=', '?'..#255]; NextState: 34),
    (CharClass: [#1..#9, #11..';', '='..#255]; NextState: 36),
    (CharClass: ['<']; NextState: 37),
    (CharClass: [#1..#9, #11..'.', '0'..';', '='..#255]; NextState: 36),
    (CharClass: ['/']; NextState: 60),
    (CharClass: ['<']; NextState: 37),
    (CharClass: ['0'..'9']; NextState: 61),
    (CharClass: [';']; NextState: 62),
    (CharClass: ['A'..'Z', 'a'..'z']; NextState: 39),
    (CharClass: ['-', '0'..'9', 'A'..'B', 'D'..'Z', '_', 'a'..'b', 'd'..'z', #128..#165]; NextState: 41),
    (CharClass: [':']; NextState: 66),
    (CharClass: ['>']; NextState: 65),
    (CharClass: ['C', 'c']; NextState: 63),
    (CharClass: ['\']; NextState: 64),
    (CharClass: ['-', '0'..'9', 'A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 41),
    (CharClass: [':']; NextState: 66),
    (CharClass: ['>']; NextState: 65),
    (CharClass: ['\']; NextState: 64),
    (CharClass: [' '..'/', ':'..'@', 'G'..'`', 'g'..'~', #128..#165]; NextState: 41),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 67),
    (CharClass: ['-']; NextState: 68),
    (CharClass: ['A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 69),
    (CharClass: ['\']; NextState: 70),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!'..',', '.'..'/', ':'..'<', '?'..'@', '[', ']'..'^', '`', '{'..#127,
    #166..#255]; NextState: 23),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 19),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 71),
    (CharClass: ['\']; NextState: 20),
    (CharClass: ['-', '0'..'9', 'A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 46),
    (CharClass: ['\']; NextState: 72),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'$', '&'..',', '.'..'/', ':'..'=', '?'..'@', '[', ']'..'^',
    '`', '{'..#127, #166..#255]; NextState: 30),
    (CharClass: ['%']; NextState: 48),
    (CharClass: ['-', 'A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 49),
    (CharClass: ['0'..'9']; NextState: 47),
    (CharClass: ['\']; NextState: 50),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'=', '?'..#255]; NextState: 30),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..',', '.'..'/', ':'..'=', '?'..'@', '[', ']'..'^', '`',
    '{'..#127, #166..#255]; NextState: 30),
    (CharClass: ['-', '0'..'9', 'A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 49),
    (CharClass: ['\']; NextState: 50),
    (CharClass: [#1..#8, #11..#12, #14..#31, #127, #166..#255]; NextState: 30),
    (CharClass: [' ', '"', '>']; NextState: 74),
    (CharClass: ['!', '#'..'/', ':'..'=', '?'..'@', 'G'..'`', 'g'..'~', #128..#165]; NextState: 49),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 73),
    (CharClass: [#1..'!', '#'..'$', '&'..',', '.'..'/', ':'..'=', '?'..'@', '[', ']'..'^', '`', '{'..#127, #166..#255];
      NextState: 55),
    (CharClass: ['"']; NextState: 75),
    (CharClass: ['%']; NextState: 156),
    (CharClass: ['-', 'A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 76),
    (CharClass: ['0'..'9']; NextState: 153),
    (CharClass: ['\']; NextState: 77),
    (CharClass: [#1..'!', '#'..'/', ':'..'=', '?'..'@', 'G'..'`', 'g'..#255]; NextState: 55),
    (CharClass: ['"']; NextState: 78),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 154),
    (CharClass: [#1..'!', '#'..'=', '?'..'@', '['..'`', '{'..#255]; NextState: 55),
    (CharClass: ['"']; NextState: 78),
    (CharClass: ['A'..'Z', 'a'..'z']; NextState: 155),
    (CharClass: [#1..'!', '#'..'=', '?'..#255]; NextState: 55),
    (CharClass: ['"']; NextState: 78),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'/', ':'..'=', '?'..'@', 'G'..'`', 'g'..#255]; NextState: 30),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 56),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'=', '?'..#255]; NextState: 30),
    (CharClass: [#1..'&', '('..'=', '?'..#255]; NextState: 58),
    (CharClass: ['''']; NextState: 79),
    (CharClass: [#1..'=', '?'..'B', 'D'..'b', 'd'..#255]; NextState: 32),
    (CharClass: ['C', 'c']; NextState: 81),
    (CharClass: [#1..#9, #11..';', '='..'R', 'T'..'r', 't'..#255]; NextState: 36),
    (CharClass: ['<']; NextState: 37),
    (CharClass: ['S', 's']; NextState: 82),
    (CharClass: ['0'..'9']; NextState: 61),
    (CharClass: [';']; NextState: 83),
    (CharClass: ['-', '0'..'9', 'A'..'Q', 'S'..'Z', '_', 'a'..'q', 's'..'z', #128..#165]; NextState: 41),
    (CharClass: [':']; NextState: 66),
    (CharClass: ['>']; NextState: 65),
    (CharClass: ['R', 'r']; NextState: 84),
    (CharClass: ['\']; NextState: 64),
    (CharClass: [' '..'/', ':'..'@', 'G'..'`', 'g'..'~', #128..#165]; NextState: 41),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 67),
    (CharClass: ['A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 85),
    (CharClass: ['\']; NextState: 86),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 41),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 87),
    (CharClass: [':']; NextState: 66),
    (CharClass: ['>']; NextState: 65),
    (CharClass: ['\']; NextState: 64),
    (CharClass: ['-']; NextState: 88),
    (CharClass: ['-', '0'..'9', 'A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 69),
    (CharClass: [':']; NextState: 91),
    (CharClass: ['>']; NextState: 90),
    (CharClass: ['\']; NextState: 89),
    (CharClass: [' '..'/', ':'..'@', 'G'..'`', 'g'..'~', #128..#165]; NextState: 69),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 92),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!'..',', '.'..'/', ':'..'<', '?'..'@', '[', ']'..'^', '`', '{'..#127,
    #166..#255]; NextState: 23),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 19),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 19),
    (CharClass: ['\']; NextState: 20),
    (CharClass: [' '..'/', ':'..'@', 'G'..'`', 'g'..'~', #128..#165]; NextState: 46),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 93),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..',', '.'..'/', ':'..'=', '?'..'@', '[', ']'..'^', '`',
    '{'..#127, #166..#255]; NextState: 30),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 49),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 94),
    (CharClass: ['\']; NextState: 50),
    (CharClass: ['-', '0'..'9', 'A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 74),
    (CharClass: ['\']; NextState: 95),
    (CharClass: [#1..'!', '#'..',', '.'..'/', ':'..'=', '?'..'@', '[', ']'..'^', '`', '{'..#127, #166..#255];
      NextState: 55),
    (CharClass: ['"']; NextState: 97),
    (CharClass: ['-', '0'..'9', 'A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 76),
    (CharClass: ['\']; NextState: 77),
    (CharClass: [#1..#31, #127, #166..#255]; NextState: 55),
    (CharClass: [' '..'!', '#'..'/', ':'..'=', '?'..'@', 'G'..'`', 'g'..'~', #128..#165]; NextState: 76),
    (CharClass: ['"']; NextState: 100),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 98),
    (CharClass: ['>']; NextState: 99),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'=', '?'..#255]; NextState: 30),
    (CharClass: [#1..'=', '?'..'S', 'U'..'s', 'u'..#255]; NextState: 32),
    (CharClass: ['T', 't']; NextState: 104),
    (CharClass: [#1..#9, #11..';', '='..'B', 'D'..'b', 'd'..#255]; NextState: 36),
    (CharClass: ['<']; NextState: 37),
    (CharClass: ['C', 'c']; NextState: 105),
    (CharClass: ['-', '0'..'9', 'A'..'H', 'J'..'Z', '_', 'a'..'h', 'j'..'z', #128..#165]; NextState: 41),
    (CharClass: [':']; NextState: 66),
    (CharClass: ['>']; NextState: 65),
    (CharClass: ['I', 'i']; NextState: 106),
    (CharClass: ['\']; NextState: 64),
    (CharClass: ['-', '0'..'9', 'A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 85),
    (CharClass: ['\']; NextState: 107),
    (CharClass: [' '..'/', ':'..'@', 'G'..'`', 'g'..'~', #128..#165]; NextState: 85),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 108),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 41),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 41),
    (CharClass: [':']; NextState: 66),
    (CharClass: ['>']; NextState: 65),
    (CharClass: ['\']; NextState: 64),
    (CharClass: [' '..'/', ':'..'@', 'G'..'`', 'g'..'~', #128..#165]; NextState: 69),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 92),
    (CharClass: ['A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 109),
    (CharClass: ['\']; NextState: 110),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 69),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 111),
    (CharClass: [':']; NextState: 91),
    (CharClass: ['>']; NextState: 90),
    (CharClass: ['\']; NextState: 89),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 46),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 112),
    (CharClass: ['\']; NextState: 72),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..',', '.'..'/', ':'..'=', '?'..'@', '[', ']'..'^', '`',
    '{'..#127, #166..#255]; NextState: 30),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 49),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 49),
    (CharClass: ['\']; NextState: 50),
    (CharClass: [' '..'/', ':'..'@', 'G'..'`', 'g'..'~', #128..#165]; NextState: 74),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 113),
    (CharClass: [#1..'!', '#'..',', '.'..'/', ':'..'=', '?'..'@', '[', ']'..'^', '`', '{'..#127, #166..#255];
      NextState: 55),
    (CharClass: ['"']; NextState: 97),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 76),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 114),
    (CharClass: ['\']; NextState: 77),
    (CharClass: ['"']; NextState: 116),
    (CharClass: ['-', '0'..'9', 'A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 99),
    (CharClass: ['\']; NextState: 115),
    (CharClass: ['"']; NextState: 116),
    (CharClass: ['-', '0'..'9', 'A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 99),
    (CharClass: ['\']; NextState: 115),
    (CharClass: [#1..'!', '#'..'9', ';'..'=', '?'..'@', '['..'`', '{'..#255]; NextState: 55),
    (CharClass: ['"']; NextState: 78),
    (CharClass: [':']; NextState: 117),
    (CharClass: ['A'..'Z', 'a'..'z']; NextState: 162),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'9', ';'..'=', '?'..'@', '['..'`', '{'..#255]; NextState: 30),
    (CharClass: [':']; NextState: 118),
    (CharClass: ['A'..'Z', 'a'..'z']; NextState: 158),
    (CharClass: [#1..'=', '?'..'X', 'Z'..'x', 'z'..#255]; NextState: 32),
    (CharClass: ['Y', 'y']; NextState: 119),
    (CharClass: [#1..#9, #11..';', '='..'Q', 'S'..'q', 's'..#255]; NextState: 36),
    (CharClass: ['<']; NextState: 37),
    (CharClass: ['R', 'r']; NextState: 120),
    (CharClass: ['-', '0'..'9', 'A'..'O', 'Q'..'Z', '_', 'a'..'o', 'q'..'z', #128..#165]; NextState: 41),
    (CharClass: [':']; NextState: 66),
    (CharClass: ['>']; NextState: 65),
    (CharClass: ['P', 'p']; NextState: 121),
    (CharClass: ['\']; NextState: 64),
    (CharClass: [' '..'/', ':'..'@', 'G'..'`', 'g'..'~', #128..#165]; NextState: 85),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 108),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 85),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 122),
    (CharClass: ['\']; NextState: 107),
    (CharClass: ['-', '0'..'9', 'A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 109),
    (CharClass: ['>']; NextState: 124),
    (CharClass: ['\']; NextState: 123),
    (CharClass: [' '..'/', ':'..'@', 'G'..'`', 'g'..'~', #128..#165]; NextState: 109),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 125),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 69),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 69),
    (CharClass: [':']; NextState: 91),
    (CharClass: ['>']; NextState: 90),
    (CharClass: ['\']; NextState: 89),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 46),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 46),
    (CharClass: ['\']; NextState: 72),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 74),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 126),
    (CharClass: ['\']; NextState: 95),
    (CharClass: [#1..'!', '#'..',', '.'..'/', ':'..'=', '?'..'@', '[', ']'..'^', '`', '{'..#127, #166..#255];
      NextState: 55),
    (CharClass: ['"']; NextState: 97),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 76),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 76),
    (CharClass: ['\']; NextState: 77),
    (CharClass: [' '..'/', ':'..'@', 'G'..'`', 'g'..'~', #128..#165]; NextState: 99),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 127),
    (CharClass: [#1..'!', '#'..'.', '0'..'=', '?'..#255]; NextState: 55),
    (CharClass: ['"']; NextState: 78),
    (CharClass: ['/']; NextState: 128),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'.', '0'..'=', '?'..#255]; NextState: 30),
    (CharClass: ['/']; NextState: 159),
    (CharClass: [#1..'=', '?'..'O', 'Q'..'o', 'q'..#255]; NextState: 32),
    (CharClass: ['P', 'p']; NextState: 130),
    (CharClass: [#1..#9, #11..';', '='..'H', 'J'..'h', 'j'..#255]; NextState: 36),
    (CharClass: ['<']; NextState: 37),
    (CharClass: ['I', 'i']; NextState: 131),
    (CharClass: ['-', '0'..'9', 'A'..'S', 'U'..'Z', '_', 'a'..'s', 'u'..'z', #128..#165]; NextState: 41),
    (CharClass: [':']; NextState: 66),
    (CharClass: ['>']; NextState: 65),
    (CharClass: ['T', 't']; NextState: 132),
    (CharClass: ['\']; NextState: 64),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 85),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 85),
    (CharClass: ['\']; NextState: 107),
    (CharClass: [' '..'/', ':'..'@', 'G'..'`', 'g'..'~', #128..#165]; NextState: 109),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 133),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 109),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 134),
    (CharClass: ['>']; NextState: 124),
    (CharClass: ['\']; NextState: 123),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 74),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 74),
    (CharClass: ['\']; NextState: 95),
    (CharClass: ['"']; NextState: 116),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 99),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 135),
    (CharClass: ['\']; NextState: 115),
    (CharClass: [#1..'!', '#'..'.', '0'..'=', '?'..#255]; NextState: 55),
    (CharClass: ['"']; NextState: 78),
    (CharClass: ['/']; NextState: 160),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'9', ';'..'=', '?'..#255]; NextState: 30),
    (CharClass: [':']; NextState: 118),
    (CharClass: [#1..'=', '?'..'D', 'F'..'d', 'f'..#255]; NextState: 32),
    (CharClass: ['E', 'e']; NextState: 152),
    (CharClass: [#1..#9, #11..';', '='..'O', 'Q'..'o', 'q'..#255]; NextState: 36),
    (CharClass: ['<']; NextState: 37),
    (CharClass: ['P', 'p']; NextState: 138),
    (CharClass: ['-', '0'..'9', 'A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 41),
    (CharClass: [':']; NextState: 66),
    (CharClass: ['>']; NextState: 65),
    (CharClass: ['\']; NextState: 64),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 109),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 139),
    (CharClass: ['>']; NextState: 124),
    (CharClass: ['\']; NextState: 123),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 109),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 140),
    (CharClass: ['>']; NextState: 124),
    (CharClass: ['\']; NextState: 123),
    (CharClass: ['"']; NextState: 116),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 99),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 141),
    (CharClass: ['\']; NextState: 115),
    (CharClass: [#1..'!', '#'..'9', ';'..'=', '?'..#255]; NextState: 55),
    (CharClass: ['"']; NextState: 78),
    (CharClass: [':']; NextState: 117),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'=', '?'..#255]; NextState: 144),
    (CharClass: [#9..#10, #13, ' ']; NextState: 143),
    (CharClass: [#1..#9, #11..';', '='..'S', 'U'..'s', 'u'..#255]; NextState: 36),
    (CharClass: ['<']; NextState: 37),
    (CharClass: ['T', 't']; NextState: 146),
    (CharClass: ['-', 'G'..'Z', '_', 'g'..'z', #128..#165]; NextState: 109),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 147),
    (CharClass: ['>']; NextState: 124),
    (CharClass: ['\']; NextState: 123),
    (CharClass: ['-', '0'..'9', 'A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 109),
    (CharClass: ['>']; NextState: 124),
    (CharClass: ['\']; NextState: 123),
    (CharClass: ['"']; NextState: 116),
    (CharClass: ['-', '0'..'9', 'A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 99),
    (CharClass: ['\']; NextState: 115),
    (CharClass: [#1..'!', '#'..'=', '?'..#255]; NextState: 142),
    (CharClass: ['"']; NextState: 148),
    (CharClass: [#1..'!', '#'..'=', '?'..#255]; NextState: 143),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'=', '?'..#255]; NextState: 144),
    (CharClass: [#9..#10, #13, ' ']; NextState: 143),
    (CharClass: [#1..'=', '?'..#255]; NextState: 145),
    (CharClass: [#1..#9, #11..';', '=', '?'..#255]; NextState: 36),
    (CharClass: ['<']; NextState: 37),
    (CharClass: ['>']; NextState: 149),
    (CharClass: ['-', '0'..'9', 'A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 109),
    (CharClass: ['>']; NextState: 124),
    (CharClass: ['\']; NextState: 123),
    (CharClass: [#1..#9, #11..';', '='..#255]; NextState: 36),
    (CharClass: ['<']; NextState: 37),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'=', '?'..'@', '['..'`', '{'..#255]; NextState: 30),
    (CharClass: ['A'..'Z', 'a'..'z']; NextState: 103),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'&', '('..'=', '?'..#255]; NextState: 151),
    (CharClass: [#9..#10, #13, ' ', '"']; NextState: 58),
    (CharClass: ['''']; NextState: 80),
    (CharClass: [#1..'=', '?'..#255]; NextState: 145),
    (CharClass: [#1..'!', '#'..'$', '&'..',', '.'..'/', ':'..'=', '?'..'@', '[', ']'..'^', '`', '{'..#127, #166..#255];
      NextState: 55),
    (CharClass: ['"']; NextState: 96),
    (CharClass: ['%']; NextState: 156),
    (CharClass: ['-', 'A'..'Z', '_', 'a'..'z', #128..#165]; NextState: 76),
    (CharClass: ['0'..'9']; NextState: 153),
    (CharClass: ['\']; NextState: 77),
    (CharClass: [#1..'!', '#'..'/', ':'..'=', '?'..'@', 'G'..'`', 'g'..#255]; NextState: 55),
    (CharClass: ['"']; NextState: 101),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 154),
    (CharClass: [#1..'!', '#'..'=', '?'..'@', '['..'`', '{'..#255]; NextState: 55),
    (CharClass: ['"']; NextState: 78),
    (CharClass: ['A'..'Z', 'a'..'z']; NextState: 102),
    (CharClass: [#1..'!', '#'..'=', '?'..#255]; NextState: 55),
    (CharClass: ['"']; NextState: 75),
    (CharClass: [#1..'!', '#'..'9', ';'..'=', '?'..'@', '['..'`', '{'..#255]; NextState: 55),
    (CharClass: ['"']; NextState: 78),
    (CharClass: [':']; NextState: 117),
    (CharClass: ['A'..'Z', 'a'..'z']; NextState: 136),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'9', ';'..'=', '?'..'@', '['..'`', '{'..#255]; NextState: 30),
    (CharClass: [':']; NextState: 118),
    (CharClass: ['A'..'Z', 'a'..'z']; NextState: 129),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'.', '0'..'=', '?'..#255]; NextState: 30),
    (CharClass: ['/']; NextState: 137),
    (CharClass: [#1..'!', '#'..'=', '?'..#255]; NextState: 142),
    (CharClass: ['"']; NextState: 78),
    (CharClass: [#1..#8, #11..#12, #14..#31, '!', '#'..'=', '?'..'@', '['..'`', '{'..#255]; NextState: 30),
    (CharClass: ['A'..'Z', 'a'..'z']; NextState: 150),
    (CharClass: [#1..'!', '#'..'9', ';'..'=', '?'..'@', '['..'`', '{'..#255]; NextState: 55),
    (CharClass: ['"']; NextState: 78),
    (CharClass: [':']; NextState: 117),
    (CharClass: ['A'..'Z', 'a'..'z']; NextState: 157)
    );

  MarksLow: array[0..StateCount - 1] of Integer = (
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 2, 2, 3, 5, 6, 7, 8, 9, 10,
    12, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 23, 24, 25, 26, 26, 27,
    27, 29, 30, 33, 35, 37, 38, 39, 40, 41, 42, 43, 45, 47, 47, 48, 49, 50, 51,
    52, 52, 53, 53, 54, 54, 55, 55, 57, 57, 59, 60, 62, 63, 64, 65, 66, 68, 69,
    70, 71, 72, 73, 73, 74, 75, 75, 76, 76, 77, 78, 80, 80, 83, 85, 86, 86, 87,
    89, 90, 91, 92, 93, 94, 94, 95, 95, 95, 96, 97, 98, 99, 99, 100, 101, 102, 103,
    104, 105, 106, 106, 107, 107, 108, 108, 109, 110, 111, 112, 114, 114, 114, 114,
    115, 116, 117, 117, 117, 117, 119, 120, 122, 124, 125, 125, 127, 129, 130, 131,
    132, 133, 134, 135, 136, 137, 138, 139, 140, 141);

  MarksHigh: array[0..StateCount - 1] of Integer = (
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 1, 1, 2, 4, 5,
    6, 7, 8, 9, 11, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 22, 23, 24,
    25, 25, 26, 26, 28, 29, 32, 34, 36, 37, 38, 39, 40, 41, 42, 44, 46, 46, 47,
    48, 49, 50, 51, 51, 52, 52, 53, 53, 54, 54, 56, 56, 58, 59, 61, 62, 63, 64,
    65, 67, 68, 69, 70, 71, 72, 72, 73, 74, 74, 75, 75, 76, 77, 79, 79, 82, 84,
    85, 85, 86, 88, 89, 90, 91, 92, 93, 93, 94, 94, 94, 95, 96, 97, 98, 98, 99,
    100, 101, 102, 103, 104, 105, 105, 106, 106, 107, 107, 108, 109, 110, 111, 113,
    113, 113, 113, 114, 115, 116, 116, 116, 116, 118, 119, 121, 123, 124, 124, 126,
    128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141);

  MatchesLow: array[0..StateCount - 1] of Integer = (
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 2, 2, 3, 5, 6, 7, 8, 9, 10,
    12, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 23, 24, 25, 26, 26, 27,
    27, 29, 30, 33, 35, 37, 38, 39, 40, 41, 42, 43, 45, 47, 47, 48, 49, 50, 51,
    52, 52, 53, 53, 54, 54, 55, 55, 57, 57, 59, 60, 62, 63, 64, 65, 66, 68, 69,
    70, 71, 72, 73, 73, 74, 75, 75, 76, 76, 77, 78, 80, 80, 83, 85, 86, 86, 87,
    89, 90, 91, 92, 93, 94, 94, 95, 95, 95, 96, 97, 98, 99, 99, 100, 101, 102, 103,
    104, 105, 106, 106, 107, 107, 108, 108, 109, 110, 111, 112, 114, 114, 114, 114,
    115, 116, 117, 117, 117, 117, 119, 120, 122, 124, 125, 125, 127, 129, 130, 131,
    132, 133, 134, 135, 136, 137, 138, 139, 140, 141);

  MatchesHigh: array[0..StateCount - 1] of Integer = (
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 1, 1, 2, 4, 5,
    6, 7, 8, 9, 11, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 22, 23, 24,
    25, 25, 26, 26, 28, 29, 32, 34, 36, 37, 38, 39, 40, 41, 42, 44, 46, 46, 47,
    48, 49, 50, 51, 51, 52, 52, 53, 53, 54, 54, 56, 56, 58, 59, 61, 62, 63, 64,
    65, 67, 68, 69, 70, 71, 72, 72, 73, 74, 74, 75, 75, 76, 77, 79, 79, 82, 84,
    85, 85, 86, 88, 89, 90, 91, 92, 93, 93, 94, 94, 94, 95, 96, 97, 98, 98, 99,
    100, 101, 102, 103, 104, 105, 105, 106, 106, 107, 107, 108, 109, 110, 111, 113,
    113, 113, 113, 114, 115, 116, 116, 116, 116, 118, 119, 121, 123, 124, 124, 126,
    128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141);

  TransitionsLow: array[0..StateCount - 1] of Integer = (
    0, 1, 2, 6, 10, 17, 24, 32, 40, 44, 48, 51, 54, 57, 60, 60, 61, 63, 68, 69,
    72, 76, 76, 76, 77, 78, 83, 88, 90, 92, 95, 96, 98, 99, 99, 100, 100, 102, 105,
    106, 108, 113, 117, 119, 120, 122, 126, 128, 133, 134, 137, 141, 147, 150, 153,
    153, 155, 157, 158, 160, 162, 165, 167, 167, 172, 174, 174, 176, 181, 182, 186,
    188, 192, 194, 198, 200, 200, 204, 209, 209, 209, 210, 212, 215, 215, 220, 222,
    224, 229, 229, 231, 231, 233, 238, 241, 245, 247, 247, 247, 252, 255, 258, 258,
    262, 265, 267, 270, 275, 277, 280, 283, 285, 290, 293, 296, 301, 303, 303, 306,
    308, 310, 313, 318, 321, 323, 323, 327, 330, 334, 337, 339, 341, 344, 348, 352,
    356, 360, 363, 365, 368, 372, 375, 378, 380, 381, 383, 384, 387, 390, 390, 392,
    394, 397, 398, 404, 407, 410, 412, 416, 419, 421, 423, 425);

  TransitionsHigh: array[0..StateCount - 1] of Integer = (
    0, 1, 5, 9, 16, 23, 31, 39, 43, 47, 50, 53, 56, 59, 59, 60, 62, 67, 68, 71,
    75, 75, 75, 76, 77, 82, 87, 89, 91, 94, 95, 97, 98, 98, 99, 99, 101, 104, 105,
    107, 112, 116, 118, 119, 121, 125, 127, 132, 133, 136, 140, 146, 149, 152, 152,
    154, 156, 157, 159, 161, 164, 166, 166, 171, 173, 173, 175, 180, 181, 185, 187,
    191, 193, 197, 199, 199, 203, 208, 208, 208, 209, 211, 214, 214, 219, 221, 223,
    228, 228, 230, 230, 232, 237, 240, 244, 246, 246, 246, 251, 254, 257, 257, 261,
    264, 266, 269, 274, 276, 279, 282, 284, 289, 292, 295, 300, 302, 302, 305, 307,
    309, 312, 317, 320, 322, 322, 326, 329, 333, 336, 338, 340, 343, 347, 351, 355,
    359, 362, 364, 367, 371, 374, 377, 379, 380, 382, 383, 386, 389, 389, 391, 393,
    396, 397, 403, 406, 409, 411, 415, 418, 420, 422, 424, 428);

function GetKeywordToken(const S: string): Integer;

// internal search function for keywords,
// since no keywords are defined the result is always 0

begin
  Result := 0;
end;

//--------------------------------------------------------------------------------

constructor TUCEHTMLHighlighter.Create(AOwner: TComponent);

begin
  inherited;
  // allocate positions array (length is highest possible mark position)
  SetLength(FPositions, MarksHigh[StateCount - 1] + 1);

  DefaultFilter := 'HTML files (*.htm, *.html)|*.htm;*.html';

  // !! adjust attributes !!
  FIdentifierAttributes := THighlightAttributes.Create('identifier');
  FIdentifierAttributes.OnChange := HighlightChange;
  FNumberAttributes := THighlightAttributes.Create('number');
  FNumberAttributes.OnChange := HighlightChange;
  FSpaceAttributes := THighlightAttributes.Create('space');
  FSpaceAttributes.OnChange := HighlightChange;
  FStringAttributes := THighlightAttributes.Create('string');
  FStringAttributes.OnChange := HighlightChange;
  FCommentAttributes := THighlightAttributes.Create('comment');
  FCommentAttributes.OnChange := HighlightChange;
  FSymbolAttributes := THighlightAttributes.Create('symbol');
  FSymbolAttributes.OnChange := HighlightChange;
  FKeyAttributes := THighlightAttributes.Create('keyword');
  FKeyAttributes.OnChange := HighlightChange;
  FUnknownAttributes := THighlightAttributes.Create('unknown');
  FUnknownAttributes.OnChange := HighlightChange;
  FParamAttributes := THighlightAttributes.Create('parameter');
  FParamAttributes.OnChange := HighlightChange;
  FURLAttributes := THighlightAttributes.Create('URL');
  FURLAttributes.OnChange := HighlightChange;
  FDocTypeAttributes := THighlightAttributes.Create('doctype');
  FDocTypeAttributes.OnChange := HighlightChange;
  FNameAttributes := THighlightAttributes.Create('name');
  FNameAttributes.OnChange := HighlightChange;
  FScriptAttributes := THighlightAttributes.Create('script');
  FScriptAttributes.OnChange := HighlightChange;
end;

//-----------------------------------------------------------------------------

destructor TUCEHTMLHighlighter.Destroy;

begin
  FPositions := nil;
  FMatchStack := nil;

  // !! adjust attributes !!
  FIdentifierAttributes.Free;
  FNumberAttributes.Free;
  FSpaceAttributes.Free;
  FStringAttributes.Free;
  FCommentAttributes.Free;
  FSymbolAttributes.Free;
  FKeyAttributes.Free;
  FUnknownAttributes.Free;
  FParamAttributes.Free;
  FURLAttributes.Free;
  FDocTypeAttributes.Free;
  FNameAttributes.Free;
  FScriptAttributes.Free;

  inherited;
end;

//-----------------------------------------------------------------------------

function TUCEHTMLHighlighter.IsKeyword: Boolean;

// checks whether the current token is a keyword,
// if so then the current token type is set accordingly

var
  Word: string;

begin
  SetString(Word, FInput + FTokenPos, FCurrentPos - FTokenPos);
  FCurrentToken := GetKeywordToken(Word);
  if FCurrentToken > 0 then
  begin
    Result := True;
    FDoStop := True;
  end
  else
    Result := False;
end;

//-----------------------------------------------------------------------------

function TUCEHTMLHighlighter.EOL: Boolean;

begin
  Result := (FCurrentToken = -1) or FEOL;
end;

//-----------------------------------------------------------------------------

function TUCEHTMLHighlighter.GetToken: string;

var
  Len: Cardinal;

begin
  Len := FCurrentPos - FTokenPos;
  SetString(Result, FInput + FTokenPos, Len);
end;

//-----------------------------------------------------------------------------

procedure TUCEHTMLHighlighter.Reset;

begin
  FTokenPos := 0;
  FCurrentPos := 0;
  FCurrentToken := 0;
  FLineState := 1;
  FLastChar := #0;
  FMatchCount := 0;
  FEOL := False;
end;

//-----------------------------------------------------------------------------

procedure TUCEHTMLHighlighter.SetToken(Token: Integer);

begin
  FCurrentToken := Token;
  FDoStop := True;
end;

//-----------------------------------------------------------------------------

procedure TUCEHTMLHighlighter.RuleToToken(Rule: Integer);

// var S : String;

begin
  // SetString(S, FInput + FTokenPos, FCurrentPos - FTokenPos);
  // actions
  case Rule of
    // <Normal>[^<&]+
    0: SetToken(IDENTIFIER);
    // <Normal>&#[0-9]+;?
    1: SetToken(INTEGERNUMBER);
    // <Normal>&[A-Za-z]+;?
    2: SetToken(STRINGCONSTANT);
    // <Normal><[Ss][Cc][Rr][Ii][Pp][Tt]
    3:
      begin
        State := Script;
        SetToken(SCRIPTPART);
      end;
    // <Normal><¤ident¤>
    4: SetToken(KEYWORD); // SetToken(UNKNOWNEND);
    // <Normal><!
    5:
      begin
        State := Comment;
        SetToken(MLCOMMENT);
      end;
    // <Normal><!\-\-
    6:
      begin
        State := CommentAlt;
        SetToken(MLCOMMENT);
      end;
    // <Normal><¤ident¤
    7:
      begin
        State := UnknownAttrs;
        SetToken(KEYWORD); // SetToken(UNKNOWN);
      end;
    // <Normal><¤ident¤:¤ident¤
    8:
      begin
        State := UnknownAttrs;
        SetToken(KEYWORD); // SetToken(UNKNOWNPSEUDO);
      end;
    // <Normal><\/¤ident¤>?
    9:
      begin
        State := Normal;
        SetToken(KEYWORD); // SetToken(UNKNOWNEND);
      end;
    // <Normal><\/¤ident¤:¤ident¤>
    10:
      begin
        State := Normal;
        SetToken(KEYWORD); // SetToken(UNKNOWNPSEUDOEND);
      end;

    // <UnknownAttrs>[ \t\n\r]+
    11: SetToken(WHITESPACE);
    // <UnknownAttrs>¤ident¤
    12: SetToken(NAMETOKEN);
    // <UnknownAttrs>=
    13:
      begin
        SetToken(SYMBOL);
        State := AttrParm;
      end;
    // <UnknownAttrs>>
    14:
      begin
        State := Normal;
        SetToken(KEYWORD);
      end;
    // <UnknownAttrs>[^ \t\n\r=>]+
    15: SetToken(UNKNOWNPARM);

    // <AttrParm>[ \t\n\r]+
    16: SetToken(WHITESPACE);
    // <AttrParm>[0-9]+%?
    17,
    // <AttrParm>\"[0-9]+%?\"
    18,
    // <AttrParm>#[0-9a-fA-F]+
    19,
    // <AttrParm>\"#[0-9a-fA-F]+\"
    20,
    // <AttrParm>[0-9]+¤nmchar¤+
    21,
    // <AttrParm>\"[0-9]+¤nmchar¤+\"
    22:
      begin
        SetToken(INTEGERNUMBER);
        State := UnknownAttrs;
      end;
    // <AttrParm>[a-zA-Z]{4,6}:\/\/[^>"]+
    23,
    // <AttrParm>\"[a-zA-Z]{3,6}:\/\/[^>"]+\"?
    24:
      begin
        SetToken(URLSTRING);
        State := UnknownAttrs;
      end;
    // <AttrParm>(\"\")|('')
    25,
    // <AttrParm>'[^>']+'
    26,
    // <AttrParm>\"[^>"]+\"?
    27:
      begin
        State := UnknownAttrs;
        SetToken(STRINGCONSTANT);
      end;
    // <AttrParm>[^ ">\t\n\r]+
    28:
      begin
        State := UnknownAttrs;
        SetToken(NAMETOKEN);
      end;

    // <Comment>[Dd][Oo][Cc][Tt][Yy][Pp][Ee][^>]+
    29: SetToken(DOCTYPE);
    // <Comment>[^>]+
    30: SetToken(MLCOMMENT);
    // <Comment>>
    31:
      begin
        SetToken(MLCOMMENT);
        State := Normal;
      end;

    // <CommentAlt>[^>]+
    32: SetToken(MLCOMMENT);
    // <CommentAlt>>
    33:
      begin
        SetToken(MLCOMMENT);
        State := Normal;
      end;
    // <Script>.*<\/[Ss][Cc][Rr][Ii][Pp][Tt]>
    34:
      begin
        State := Normal;
        SetToken(SCRIPTPART);
      end;
    // <Script>.+
    35: SetToken(SCRIPTPART);
    // ¤eof¤
    36: SetToken(-1); // special token to mark input end

  end;
end;

//-----------------------------------------------------------------------------

function TUCEHTMLHighlighter.MatchRule(var Rule: Integer): Boolean;

// finds the last match and the corresponding marked position and adjusts
// the matched string accordingly;
// returns:
// - True if a rule has been matched, False otherwise
// - Rule: the number of the matched rule

begin
  FRejected := False;
  while (FMatchCount > 0) and (FPositions[FMatchStack[FMatchCount - 1]] = 0) do
    Dec(FMatchCount);

  if FMatchCount > 0 then
  begin
    Rule := FMatchStack[FMatchCount - 1];
    FCurrentPos := FTokenPos + FPositions[Rule];
    FPositions[Rule] := 0;
    if (FCurrentPos - FTokenPos) > 0 then
      FLastChar := FInput[FCurrentPos]
    else
      FLastChar := #0;
    Result := True;
  end
  else
  begin
    // return position to starting point
    FCurrentPos := FTokenPos;
    FLastChar := #0;
    Result := False;
  end
end;

//-----------------------------------------------------------------------------

function TUCEHTMLHighlighter.DoDefault: Boolean;

begin
  FRejected := False;
  if NextChar <> #0 then
    Result := True
  else
  begin
    FLineState := 1;
    Result := False;
  end;
  if not FEOL then
    FLastChar := FInput[FCurrentPos];
end;

//-----------------------------------------------------------------------------

procedure TUCEHTMLHighlighter.Next;

// scans for the next token and determines its kind (FCurrentToken and FTokenPos
// are set appropriately)

var
  I: Integer;
  Len: Cardinal;
  Matched: Boolean;

begin
  FDoStop := False;

  repeat
    // initialize run
    FTokenPos := FCurrentPos;

    if FLastChar <> #0 then
      if FLastChar = LF then
        FLineState := 1
      else
        FLineState := 0;
    FCurrentState := FStartState + FLineState;

    repeat // character scan loop
      // mark positions and matches
      Len := FCurrentPos - FTokenPos;
      for I := MarksLow[FCurrentState] to MarksHigh[FCurrentState] do
        FPositions[MarkPositionTable[I]] := Len;
      for I := MatchesHigh[FCurrentState] downto MatchesLow[FCurrentState] do
        SetMatch(MatchTable[I]);

      if TransitionsLow[FCurrentState] > TransitionsHigh[FCurrentState] then
        Break; // dead state, do action

      // determine action
      I := TransitionsLow[FCurrentState];
      while (I <= TransitionsHigh[FCurrentState]) and not (FInput[FCurrentPos] in TransitionTable[I].CharClass) do
        Inc(I);

      if I > TransitionsHigh[FCurrentState] then
        Break; // no transition on current char in this state

      // get next character
      NextChar;

      // switch to new state
      FCurrentState := TransitionTable[I].NextState;

    until False;

    repeat
      Matched := MatchRule(I);
      if Matched then
        RuleToToken(I)
      else
        Break;
      // FRejected can be set in RuleToToken by calling Reject
    until Matched and not FRejected;

    if not (Matched or DoDefault) then
    begin
      FStartState := Normal;
      FLineState := 1;
      FLastChar := #0;
      SetToken(0);
    end;
  until FDoStop;
end;

//-----------------------------------------------------------------------------

procedure TUCEHTMLHighlighter.SetLine(const NewValue: string);

begin
  FInput := PChar(NewValue);
  Reset;
  Next;
end;

//-----------------------------------------------------------------------------

function TUCEHTMLHighlighter.GetNextChar: Char;

begin
  if not FEOL and (FInput[FCurrentPos] = #0) then
    FEOL := True;
  Inc(FCurrentPos);
  if not FEOL then
  begin
    FLookahead := FInput[FCurrentPos + 1];
    Result := FInput[FCurrentPos];
  end
  else
    Result := #0;
end;

//-----------------------------------------------------------------------------

procedure TUCEHTMLHighlighter.SetState(Value: Cardinal);

begin
  FStartState := Value;
end;

//-----------------------------------------------------------------------------

procedure TUCEHTMLHighlighter.SetMatch(I: Integer);

begin
  if FMatchCount >= Length(FMatchStack) then
    SetLength(FMatchStack, FMatchCount + 1);
  FMatchStack[FMatchCount] := I;
  Inc(FMatchCount);
end;

//-----------------------------------------------------------------------------

procedure TUCEHTMLHighlighter.Reject;

begin
  FRejected := True;
end;

//-----------------------------------------------------------------------------

function TUCEHTMLHighlighter.GetTokenInfo: TTokenData;

// returns token string, token position relative to entire input string, and token length in one call

begin
  with Result do
  begin
    if FEOL then
    begin
      Token := StrEnd(FInput);
      Length := Token - FInput - Integer(FTokenPos);
    end
    else
    begin
      Token := FInput + FTokenPos;
      Length := FCurrentPos - FTokenPos;
    end;
    TokenType := FCurrentToken;
    Position := FTokenPos;

    // !! adjust attributes !!
    case FCurrentToken of
      IDENTIFIER:
        begin
          Background := ColorToRGB(FIdentifierAttributes.Background);
          Foreground := ColorToRGB(FIdentifierAttributes.Foreground);
          Style := FIdentifierAttributes.Style;
        end;
      INTEGERNUMBER,
        FLOATNUMBER:
        begin
          Background := ColorToRGB(FNumberAttributes.Background);
          Foreground := ColorToRGB(FNumberAttributes.Foreground);
          Style := FNumberAttributes.Style;
        end;
      WHITESPACE:
        begin
          Background := ColorToRGB(FSpaceAttributes.Background);
          Foreground := ColorToRGB(FSpaceAttributes.Foreground);
          Style := FSpaceAttributes.Style;
        end;
      STRINGCONSTANT:
        begin
          Background := ColorToRGB(FStringAttributes.Background);
          Foreground := ColorToRGB(FStringAttributes.Foreground);
          Style := FStringAttributes.Style;
        end;
      MLCOMMENT:
        begin
          Background := ColorToRGB(FCommentAttributes.Background);
          Foreground := ColorToRGB(FCommentAttributes.Foreground);
          Style := FCommentAttributes.Style;
        end;
      SYMBOL:
        begin
          Background := ColorToRGB(FSymbolAttributes.Background);
          Foreground := ColorToRGB(FSymbolAttributes.Foreground);
          Style := FSymbolAttributes.Style;
        end;
      KEYWORD:
        begin
          Background := ColorToRGB(FKeyAttributes.Background);
          Foreground := ColorToRGB(FKeyAttributes.Foreground);
          Style := FKeyAttributes.Style;
        end;
      UNKNOWN,
        UNKNOWNPSEUDO,
        UNKNOWNEND,
        UNKNOWNPSEUDOEND:
        begin
          Background := ColorToRGB(FUnknownAttributes.Background);
          Foreground := ColorToRGB(FUnknownAttributes.Foreground);
          Style := FUnknownAttributes.Style;
        end;
      UNKNOWNPARM:
        begin
          Background := ColorToRGB(FParamAttributes.Background);
          Foreground := ColorToRGB(FParamAttributes.Foreground);
          Style := FParamAttributes.Style;
        end;
      URLSTRING:
        begin
          Background := ColorToRGB(FURLAttributes.Background);
          Foreground := ColorToRGB(FURLAttributes.Foreground);
          Style := FURLAttributes.Style;
        end;
      DOCTYPE:
        begin
          Background := ColorToRGB(FDocTypeAttributes.Background);
          Foreground := ColorToRGB(FDocTypeAttributes.Foreground);
          Style := FDocTypeAttributes.Style;
        end;
      NAMETOKEN:
        begin
          Background := ColorToRGB(FNameAttributes.Background);
          Foreground := ColorToRGB(FNameAttributes.Foreground);
          Style := FNameAttributes.Style;
        end;
      SCRIPTPART:
        begin
          Background := ColorToRGB(FScriptAttributes.Background);
          Foreground := ColorToRGB(FScriptAttributes.Foreground);
          Style := FScriptAttributes.Style;
        end;
    end;
  end;
end;

//-----------------------------------------------------------------------------

function TUCEHTMLHighlighter.GetCurrentChar: Char;

begin
  Result := FInput[FCurrentPos];
end;

//-----------------------------------------------------------------------------

procedure TUCEHTMLHighlighter.HighlightChange(Sender: TObject);

begin
  WindowList.Invalidate;
end;

//--------------------------------------------------------------------------------

function TUCEHTMLHighlighter.GetIdentChars: TIdentChars;

begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

//--------------------------------------------------------------------------------

function TUCEHTMLHighlighter.GetAttributeCount: Integer;

begin
  Result := 13;
end;

//--------------------------------------------------------------------------------

function TUCEHTMLHighlighter.GetAttribute(Index: Integer): THighlightAttributes;

begin
  // !! adjust attributes !!
  case Index of
    0: Result := FIdentifierAttributes;
    1: Result := FNumberAttributes;
    2: Result := FSpaceAttributes;
    3: Result := FStringAttributes;
    4: Result := FCommentAttributes;
    5: Result := FSymbolAttributes;
    6: Result := FKeyAttributes;
    7: Result := FUnknownAttributes;
    8: Result := FParamAttributes;
    9: Result := FURLAttributes;
    10: Result := FDocTypeAttributes;
    11: Result := FNameAttributes;
    12: Result := FScriptAttributes;
  else
    Result := nil;
  end;
end;

//--------------------------------------------------------------------------------

function TUCEHTMLHighlighter.GetLanguageName: string;

begin
  Result := 'HTML';
end;

//--------------------------------------------------------------------------------

procedure TUCEHTMLHighlighter.SetAttribute(const Index: Integer; const Value: THighlightAttributes);

begin
  // !! adjust attributes !!
  case Index of
    0: FIdentifierAttributes.Assign(Value);
    1: FNumberAttributes.Assign(Value);
    2: FSpaceAttributes.Assign(Value);
    3: FStringAttributes.Assign(Value);
    4: FCommentAttributes.Assign(Value);
    5: FSymbolAttributes.Assign(Value);
    6: FKeyAttributes.Assign(Value);
    7: FUnknownAttributes.Assign(Value);
    8: FParamAttributes.Assign(Value);
    9: FURLAttributes.Assign(Value);
    10: FDocTypeAttributes.Assign(Value);
    11: FNameAttributes.Assign(Value);
    12: FScriptAttributes.Assign(Value);
  end;
end;

//--------------------------------------------------------------------------------

function TUCEHTMLHighlighter.GetRange: Integer;

begin
  Result := Ord(FStartState);
end;

//--------------------------------------------------------------------------------

procedure TUCEHTMLHighlighter.SetRange(Value: Integer);

begin
  FStartState := Integer(Value);
end;

//--------------------------------------------------------------------------------

procedure TUCEHTMLHighlighter.ResetRange;

begin
  FStartState := Normal;
  // in case someone will ask for immediately afterwards
  FCurrentState := Normal;
end;

//--------------------------------------------------------------------------------

end.

