unit UceDcgHighlighter;
 
// this unit contains the implementation of a lexical analyzer
// see documentation for an extensive description
 
interface
 
uses Classes, Graphics, UceHighlighter;
 
const CR = #13;
      LF = #10;
 
// user defined start states and tokens
 
const Normal = 2;
      StringState = 4;
      AssemblerState = 6;
      Comment = 8;
      Comment2 = 10;
      Comment3 = 12;
 
 
const kwFirstKeyword = 257;
      kwABSOLUTE = 257;
      kwABSTRACT = 258;
      kwAND = 259;
      kwARRAY = 260;
      kwAS = 261;
      kwASM = 262;
      kwASSEMBLER = 263;
      kwAT = 264;
      kwAUTOMATED = 265;
      kwBEGIN = 266;
      kwCASE = 267;
      kwCDECL = 268;
      kwCLASS = 269;
      kwCONST = 270;
      kwCONSTRUCTOR = 271;
      kwCONTAINS = 272;
      kwDEFAULT = 273;
      kwDESTRUCTOR = 274;
      kwDISPID = 275;
      kwDISPINTERFACE = 276;
      kwDIV = 277;
      kwDO = 278;
      kwDOWNTO = 279;
      kwDYNAMIC = 280;
      kwELSE = 281;
      kwEND = 282;
      kwEXCEPT = 283;
      kwEXPORT = 284;
      kwEXPORTS = 285;
      kwEXTERNAL = 286;
      kwFAR = 287;
      kwFILE = 288;
      kwFINALIZATION = 289;
      kwFINALLY = 290;
      kwFOR = 291;
      kwFORWARD = 292;
      kwFUNCTION = 293;
      kwGOTO = 294;
      kwIF = 295;
      kwIMPLEMENTATION = 296;
      kwIMPLEMENTS = 297;
      kwIN = 298;
      kwINDEX = 299;
      kwINHERITED = 300;
      kwINITIALIZATION = 301;
      kwINLINE = 302;
      kwINTERFACE = 303;
      kwIS = 304;
      kwLABEL = 305;
      kwLIBRARY = 306;
      kwMESSAGE = 307;
      kwMOD = 308;
      kwNAME = 309;
      kwNEAR = 310;
      kwNIL = 311;
      kwNODEFAULT = 312;
      kwNOT = 313;
      kwOBJECT = 314;
      kwOF = 315;
      kwON = 316;
      kwOR = 317;
      kwOUT = 318;
      kwOVERLOAD = 319;
      kwOVERRIDE = 320;
      kwPACKAGE = 321;
      kwPACKED = 322;
      kwPASCAL = 323;
      kwPRIVATE = 324;
      kwPROCEDURE = 325;
      kwPROGRAM = 326;
      kwPROPERTY = 327;
      kwPROTECTED = 328;
      kwPUBLIC = 329;
      kwPUBLISHED = 330;
      kwRAISE = 331;
      kwREAD = 332;
      kwREADONLY = 333;
      kwRECORD = 334;
      kwREGISTER = 335;
      kwREINTRODUCE = 336;
      kwREPEAT = 337;
      kwREQUIRES = 338;
      kwRESIDENT = 339;
      kwRESOURCESTRING = 340;
      kwSAFECALL = 341;
      kwSET = 342;
      kwSHL = 343;
      kwSHR = 344;
      kwSTDCALL = 345;
      kwSTORED = 346;
      kwSTRING = 347;
      kwTHEN = 348;
      kwTHREADVAR = 349;
      kwTO = 350;
      kwTRY = 351;
      kwTYPE = 352;
      kwUNIT = 353;
      kwUNTIL = 354;
      kwUSES = 355;
      kwVAR = 356;
      kwVIRTUAL = 357;
      kwWHILE = 358;
      kwWITH = 359;
      kwWRITE = 360;
      kwWRITEONLY = 361;
      kwLastKeyword = 361;

const ASMTOKEN = 362;
      DCGCOMMENT = 363;
      DCGKEYWORD = 364;
      DCGMACRO = 365;
      IDENTIFIER = 366;
      MLCOMMENT = 367;
      NUMBER = 368;
      SLCOMMENT = 369;
      STRINGCONSTANT = 370;
      SYMBOL = 371;
      UNKNOWN = 372;
      WHITESPACE = 373;


 
type
  TUceDcgHighlighter = class(TUCEHighlighter)
  private
    FCurrentToken: Integer;            // type of current token
    FInput: PChar;                     // input string to be tokenized
    FCurrentPos: Cardinal;             // current lexer position in FInput
    FTokenPos: Cardinal;               // current token position in FInput
    FLookahead: Char;                  // next char after current position
    FCurrentState,
    FStartState,
    FLineState: Cardinal;              // lexer states
    FLastChar: Char;                   // last matched char (#0 if no match)
    FPositions: array of Cardinal;     // for each rule the last marked position, zeroed when
                                       // rule has already been considered (allocated once)
    FMatchStack: array of Cardinal;    // stack containing matched rules (growing dynamically)
    FMatchCount: Integer;              // contains current number of matches
    FRejected: Boolean;                // current match rejected?
    FDoStop: Boolean;                  // token or eof found?
    FEOL: Boolean;
 
    // !! adjust attributes !!
    FDCGKeywordAttributes,
    FDCGMacroAttributes,
    FDCGCommentAttributes,
    FAsmAttributes,
    FIdentifierAttributes,
    FNumberAttributes,
    FSpaceAttributes,
    FStringAttributes,
    FCommentAttributes,
    FSymbolAttributes,
    FKeyAttributes,
    FUnknownAttributes: THighlightAttributes;
 
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
    function GetToken: String; override;
    function GetTokenInfo: TTokenData; override;
    procedure Next; override;
    procedure Reject;
    function GetAttribute(Index: Integer): THighlightAttributes; override;
    function GetAttributeCount: Integer; override;
    function GetIdentChars: TIdentChars; override;
    function GetLanguageName: String; override;
    function GetRange: Integer; override;
    function HasMoreTokens: Boolean; override;
    function IsKeyword: Boolean;
    procedure ResetRange; override;
    procedure SetRange(Value: Integer); override;
    procedure SetLine(const NewValue: String); override;
 
    property CurrentChar: Char read GetCurrentChar;
    property NextChar: Char read GetNextChar;
    property Lookahead: Char read FLookahead;
    property State: Cardinal read FCurrentState write SetState; // read to get current lexer state
                                                                // write to set start state for next run
    property TokenKind: Integer read FCurrentToken;
    property TokenPosition: Cardinal read FTokenPos;
  published
    property DCGKeywordAttributes: THighlightAttributes index 0 read FDCGKeywordAttributes write SetAttribute;
    property DCGMacroAttributes: THighlightAttributes index 0 read FDCGMacroAttributes write SetAttribute;
    property DCGCommentAttributes: THighlightAttributes index 0 read FDCGCommentAttributes write SetAttribute;
    property AsmAttributes: THighlightAttributes index 0 read FAsmAttributes write SetAttribute;
    property IdentifierAttributes: THighlightAttributes index 1 read FIdentifierAttributes write SetAttribute;
    property NumberAttributes: THighlightAttributes index 2 read FNumberAttributes write SetAttribute;
    property SpaceAttributes: THighlightAttributes index 3 read FSpaceAttributes write SetAttribute;
    property StringAttributes: THighlightAttributes index 4 read FStringAttributes write SetAttribute;
    property CommentAttributes: THighlightAttributes index 5 read FCommentAttributes write SetAttribute;
    property SymbolAttributes: THighlightAttributes index 6 read FSymbolAttributes write SetAttribute;
    property KeyAttributes: THighlightAttributes index 7 read FKeyAttributes write SetAttribute;
    property UnknownAttributes: THighlightAttributes index 8 read FUnknownAttributes write SetAttribute;
  end;
 
//------------------------------------------------------------------------------
 
implementation
 
uses SysUtils;
 
// DFA table

type TTransition = record
       CharClass : set of Char;
       NextState : Integer;
     end;

const
  MarkPositionCount = 70;
  MatchCount        = 70;
  TransitionCount   = 133;
  StateCount        = 69;

  MarkPositionTable : array[0..MarkPositionCount - 1] of Integer = (
    0, 0, 26, 24, 25, 2, 25, 24, 25, 4, 25, 5, 25, 24, 25, 24, 25, 10, 25, 25, 16, 
    25, 17, 25, 23, 25, 24, 25, 25, 24, 25, 24, 25, 22, 18, 22, 19, 22, 22, 11, 
    12, 20, 21, 8, 9, 2, 24, 24, 2, 3, 2, 6, 24, 7, 24, 16, 17, 23, 13, 24, 13, 
    24, 1, 1, 18, 3, 15, 14, 14, 24);

  MatchTable : array [0..MatchCount - 1] of Integer = (
    0, 0, 26, 24, 25, 2, 25, 24, 25, 4, 25, 5, 25, 24, 25, 24, 25, 10, 25, 25, 16, 
    25, 17, 25, 23, 25, 24, 25, 25, 24, 25, 24, 25, 22, 18, 22, 19, 22, 22, 11, 
    12, 20, 21, 8, 9, 2, 24, 24, 2, 3, 2, 6, 24, 7, 24, 16, 17, 23, 13, 24, 13, 
    24, 1, 1, 18, 3, 15, 14, 14, 24);

  TransitionTable : array [0..TransitionCount - 1] of TTransition = (
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..#8, #11..#31, '`', '}', #127..#163, #165..#175, #177..#186, #188..#255]; NextState: 28),
    (CharClass: [#9, ' ']; NextState: 26),
    (CharClass: ['!', '%'..'&', ')'..'.', ':'..'@', '['..'^', '|', '~', #176]; NextState: 27),
    (CharClass: ['"']; NextState: 18),
    (CharClass: ['#']; NextState: 15),
    (CharClass: ['$']; NextState: 17),
    (CharClass: ['''']; NextState: 19),
    (CharClass: ['(']; NextState: 21),
    (CharClass: ['/']; NextState: 20),
    (CharClass: ['0'..'9']; NextState: 16),
    (CharClass: ['A'..'Z', '_', 'a'..'z']; NextState: 25),
    (CharClass: ['{']; NextState: 22),
    (CharClass: [#164]; NextState: 23),
    (CharClass: [#187]; NextState: 24),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..#8, #11..#31, '`', '}', #127..#163, #165..#175, #177..#186, #188..#255]; NextState: 28),
    (CharClass: [#9, ' ']; NextState: 26),
    (CharClass: ['!', '&', ')'..'.', ':'..';', '='..'@', '['..'^', '|', '~', #176]; NextState: 27),
    (CharClass: ['"']; NextState: 18),
    (CharClass: ['#']; NextState: 15),
    (CharClass: ['$']; NextState: 17),
    (CharClass: ['%']; NextState: 29),
    (CharClass: ['''']; NextState: 19),
    (CharClass: ['(']; NextState: 21),
    (CharClass: ['/']; NextState: 20),
    (CharClass: ['0'..'9']; NextState: 16),
    (CharClass: ['<']; NextState: 30),
    (CharClass: ['A'..'Z', '_', 'a'..'z']; NextState: 25),
    (CharClass: ['{']; NextState: 22),
    (CharClass: [#164]; NextState: 23),
    (CharClass: [#187]; NextState: 24),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..#9, #11..'&', '('..'@', '['..'^', '`', '|'..#255]; NextState: 34),
    (CharClass: ['''']; NextState: 31),
    (CharClass: ['A'..'Z', '_', 'a'..'z']; NextState: 32),
    (CharClass: ['{']; NextState: 33),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..#9, #11..'&', '('..'@', '['..'^', '`', '|'..#255]; NextState: 34),
    (CharClass: ['''']; NextState: 31),
    (CharClass: ['A'..'Z', '_', 'a'..'z']; NextState: 32),
    (CharClass: ['{']; NextState: 33),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..'|', '~'..#255]; NextState: 35),
    (CharClass: ['}']; NextState: 36),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..'|', '~'..#255]; NextState: 35),
    (CharClass: ['}']; NextState: 36),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..'|', '~'..#255]; NextState: 37),
    (CharClass: ['}']; NextState: 38),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..'|', '~'..#255]; NextState: 37),
    (CharClass: ['}']; NextState: 38),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..')', '+'..#255]; NextState: 39),
    (CharClass: ['*']; NextState: 40),
    (CharClass: [#0]; NextState: 14),
    (CharClass: [#1..')', '+'..#255]; NextState: 39),
    (CharClass: ['*']; NextState: 40),
    (CharClass: ['!', '#', '%'..'&', '('..'/', ':'..'@', '['..'^', '|', '~', #176]; NextState: 43),
    (CharClass: ['$']; NextState: 42),
    (CharClass: ['0'..'9']; NextState: 41),
    (CharClass: ['.']; NextState: 45),
    (CharClass: ['0'..'9']; NextState: 44),
    (CharClass: ['E', 'e']; NextState: 46),
    (CharClass: ['!', '#'..'&', '('..'/', ':'..'@', '['..'^', '|', '~', #176]; NextState: 43),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 47),
    (CharClass: ['!', '#'..'&', '('..'.', ':'..'@', '['..'^', '|', '~', #176]; NextState: 43),
    (CharClass: ['/']; NextState: 48),
    (CharClass: ['!', '#'..'&', '('..')', '+'..'/', ':'..'@', '['..'^', '|', '~', #176]; NextState: 43),
    (CharClass: ['*']; NextState: 49),
    (CharClass: [#1..#163, #165..#255]; NextState: 50),
    (CharClass: [#1..#9, #11..#255]; NextState: 51),
    (CharClass: ['0'..'9', 'A'..'Z', '_', 'a'..'z']; NextState: 52),
    (CharClass: [#9, ' ']; NextState: 53),
    (CharClass: ['!', '#'..'&', '('..'/', ':'..'@', '['..'^', '|', '~', #176]; NextState: 43),
    (CharClass: ['!', '#'..'$', '&', '('..'/', ':'..'@', '['..'^', '|', '~', #176]; NextState: 43),
    (CharClass: ['%']; NextState: 54),
    (CharClass: ['A'..'Z', '_', 'a'..'z']; NextState: 55),
    (CharClass: [#1..' ', '"', '''', '0'..'9', 'A'..'Z', '_'..'{', '}', #127..#175, #177..#255]; NextState: 56),
    (CharClass: ['!', '#'..'&', '('..'/', ':'..'=', '?'..'@', '['..'^', '|', '~', #176]; NextState: 57),
    (CharClass: ['>']; NextState: 43),
    (CharClass: [#0, '''']; NextState: 59),
    (CharClass: [#1..#9, #11..'&', '('..#255]; NextState: 58),
    (CharClass: [#10]; NextState: 60),
    (CharClass: ['0'..'9', 'A'..'Z', '_', 'a'..'z']; NextState: 61),
    (CharClass: [#1..'|', '~'..#255]; NextState: 35),
    (CharClass: [#1..'|', '~'..#255]; NextState: 37),
    (CharClass: [#1..')', '+'..#255]; NextState: 39),
    (CharClass: ['0'..'9']; NextState: 41),
    (CharClass: ['!', '#'..'&', '('..'/', ':'..'@', '['..'^', '|', '~', #176]; NextState: 43),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 47),
    (CharClass: ['!', '#'..'&', '('..'/', ':'..'@', '['..'^', '|', '~', #176]; NextState: 43),
    (CharClass: ['.']; NextState: 45),
    (CharClass: ['0'..'9']; NextState: 44),
    (CharClass: ['E', 'e']; NextState: 46),
    (CharClass: ['0'..'9']; NextState: 45),
    (CharClass: ['E', 'e']; NextState: 62),
    (CharClass: ['+', '-']; NextState: 63),
    (CharClass: ['0'..'9']; NextState: 64),
    (CharClass: ['0'..'9', 'A'..'F', 'a'..'f']; NextState: 47),
    (CharClass: ['!', '#'..'&', '('..'/', ':'..'@', '['..'^', '|', '~', #176]; NextState: 43),
    (CharClass: ['!', '#'..'&', '('..'/', ':'..'@', '['..'^', '|', '~', #176]; NextState: 43),
    (CharClass: [#1..#163, #165..#255]; NextState: 50),
    (CharClass: [#164]; NextState: 65),
    (CharClass: [#1..#9, #11..#255]; NextState: 51),
    (CharClass: ['0'..'9', 'A'..'Z', '_', 'a'..'z']; NextState: 52),
    (CharClass: [#9, ' ']; NextState: 53),
    (CharClass: ['!', '#'..'&', '('..'/', ':'..'@', '['..'^', '|', '~', #176]; NextState: 43),
    (CharClass: ['0'..'9', 'A'..'Z', '_', 'a'..'z']; NextState: 55),
    (CharClass: [#1..'=', '?'..#255]; NextState: 56),
    (CharClass: ['>']; NextState: 66),
    (CharClass: [#1..' ', '"', '''', '0'..'9', 'A'..'Z', '_'..'{', '}', #127..#175, #177..#255]; NextState: 56),
    (CharClass: ['!', '#'..'&', '('..'/', ':'..'=', '?'..'@', '['..'^', '|', '~', #176]; NextState: 57),
    (CharClass: ['>']; NextState: 67),
    (CharClass: [#0, '''']; NextState: 59),
    (CharClass: [#1..#9, #11..'&', '('..#255]; NextState: 58),
    (CharClass: [#10]; NextState: 60),
    (CharClass: [#0, '''']; NextState: 59),
    (CharClass: [#1..#9, #11..'&', '('..#255]; NextState: 58),
    (CharClass: [#10]; NextState: 60),
    (CharClass: ['0'..'9', 'A'..'Z', '_', 'a'..'z']; NextState: 61),
    (CharClass: ['+', '-']; NextState: 68),
    (CharClass: ['0'..'9']; NextState: 64),
    (CharClass: ['0'..'9']; NextState: 64),
    (CharClass: ['0'..'9']; NextState: 64),
    (CharClass: ['!', '#'..'&', '('..'/', ':'..'@', '['..'^', '|', '~', #176]; NextState: 43),
    (CharClass: ['0'..'9']; NextState: 64)
    );

  MarksLow : array [0..StateCount-1] of Integer = (
    0, 0, 0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 5, 7, 9, 11, 13, 15, 17, 19, 
    20, 22, 24, 26, 28, 29, 31, 33, 34, 36, 38, 39, 40, 41, 42, 43, 44, 45, 46, 
    47, 48, 49, 50, 50, 51, 53, 55, 55, 56, 57, 58, 60, 61, 61, 62, 62, 63, 64, 
    65, 65, 65, 66, 67, 68, 70);

  MarksHigh : array [0..StateCount-1] of Integer = (
    -1, -1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 6, 8, 10, 12, 14, 16, 18, 
    19, 21, 23, 25, 27, 28, 30, 32, 33, 35, 37, 38, 39, 40, 41, 42, 43, 44, 45, 
    46, 47, 48, 49, 49, 50, 52, 54, 54, 55, 56, 57, 59, 60, 60, 61, 61, 62, 63, 
    64, 64, 64, 65, 66, 67, 69, 69);

  MatchesLow : array [0..StateCount-1] of Integer = (
    0, 0, 0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 5, 7, 9, 11, 13, 15, 17, 19, 
    20, 22, 24, 26, 28, 29, 31, 33, 34, 36, 38, 39, 40, 41, 42, 43, 44, 45, 46, 
    47, 48, 49, 50, 50, 51, 53, 55, 55, 56, 57, 58, 60, 61, 61, 62, 62, 63, 64, 
    65, 65, 65, 66, 67, 68, 70);

  MatchesHigh : array [0..StateCount-1] of Integer = (
    -1, -1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 6, 8, 10, 12, 14, 16, 18, 
    19, 21, 23, 25, 27, 28, 30, 32, 33, 35, 37, 38, 39, 40, 41, 42, 43, 44, 45, 
    46, 47, 48, 49, 49, 50, 52, 54, 54, 55, 56, 57, 59, 60, 60, 61, 61, 62, 63, 
    64, 64, 64, 65, 66, 67, 69, 69);

  TransitionsLow : array [0..StateCount-1] of Integer = (
    0, 1, 2, 17, 34, 35, 36, 41, 46, 49, 52, 55, 58, 61, 64, 64, 67, 70, 72, 72, 
    72, 74, 76, 76, 77, 78, 79, 80, 81, 81, 84, 87, 90, 91, 91, 91, 92, 92, 93, 
    93, 94, 94, 95, 97, 98, 101, 103, 105, 106, 107, 108, 110, 111, 112, 113, 114, 
    115, 117, 120, 123, 123, 126, 127, 129, 130, 131, 131, 131, 132);

  TransitionsHigh : array [0..StateCount-1] of Integer = (
    0, 1, 16, 33, 34, 35, 40, 45, 48, 51, 54, 57, 60, 63, 63, 66, 69, 71, 71, 71, 
    73, 75, 75, 76, 77, 78, 79, 80, 80, 83, 86, 89, 90, 90, 90, 91, 91, 92, 92, 
    93, 93, 94, 96, 97, 100, 102, 104, 105, 106, 107, 109, 110, 111, 112, 113, 114, 
    116, 119, 122, 122, 125, 126, 128, 129, 130, 130, 130, 131, 132);

type // keyword table support
     TKeyword = record
       Keyword: String;
       Token: Integer;
     end;

const KeywordCount = 105;
      Keywords : array[0..KeywordCount - 1] of TKeyword = (
        (Keyword: 'absolute'; Token: kwABSOLUTE), 
        (Keyword: 'abstract'; Token: kwABSTRACT), 
        (Keyword: 'and'; Token: kwAND), 
        (Keyword: 'array'; Token: kwARRAY), 
        (Keyword: 'as'; Token: kwAS), 
        (Keyword: 'asm'; Token: kwASM), 
        (Keyword: 'assembler'; Token: kwASSEMBLER), 
        (Keyword: 'at'; Token: kwAT), 
        (Keyword: 'automated'; Token: kwAUTOMATED), 
        (Keyword: 'begin'; Token: kwBEGIN), 
        (Keyword: 'case'; Token: kwCASE), 
        (Keyword: 'cdecl'; Token: kwCDECL), 
        (Keyword: 'class'; Token: kwCLASS), 
        (Keyword: 'const'; Token: kwCONST), 
        (Keyword: 'constructor'; Token: kwCONSTRUCTOR), 
        (Keyword: 'contains'; Token: kwCONTAINS), 
        (Keyword: 'default'; Token: kwDEFAULT), 
        (Keyword: 'destructor'; Token: kwDESTRUCTOR), 
        (Keyword: 'dispid'; Token: kwDISPID), 
        (Keyword: 'dispinterface'; Token: kwDISPINTERFACE), 
        (Keyword: 'div'; Token: kwDIV), 
        (Keyword: 'do'; Token: kwDO), 
        (Keyword: 'downto'; Token: kwDOWNTO), 
        (Keyword: 'dynamic'; Token: kwDYNAMIC), 
        (Keyword: 'else'; Token: kwELSE), 
        (Keyword: 'end'; Token: kwEND), 
        (Keyword: 'except'; Token: kwEXCEPT), 
        (Keyword: 'export'; Token: kwEXPORT), 
        (Keyword: 'exports'; Token: kwEXPORTS), 
        (Keyword: 'external'; Token: kwEXTERNAL), 
        (Keyword: 'far'; Token: kwFAR), 
        (Keyword: 'file'; Token: kwFILE), 
        (Keyword: 'finalization'; Token: kwFINALIZATION), 
        (Keyword: 'finally'; Token: kwFINALLY), 
        (Keyword: 'for'; Token: kwFOR), 
        (Keyword: 'forward'; Token: kwFORWARD), 
        (Keyword: 'function'; Token: kwFUNCTION), 
        (Keyword: 'goto'; Token: kwGOTO), 
        (Keyword: 'if'; Token: kwIF), 
        (Keyword: 'implementation'; Token: kwIMPLEMENTATION), 
        (Keyword: 'implements'; Token: kwIMPLEMENTS), 
        (Keyword: 'in'; Token: kwIN), 
        (Keyword: 'index'; Token: kwINDEX), 
        (Keyword: 'inherited'; Token: kwINHERITED), 
        (Keyword: 'initialization'; Token: kwINITIALIZATION), 
        (Keyword: 'inline'; Token: kwINLINE), 
        (Keyword: 'interface'; Token: kwINTERFACE), 
        (Keyword: 'is'; Token: kwIS), 
        (Keyword: 'label'; Token: kwLABEL), 
        (Keyword: 'library'; Token: kwLIBRARY), 
        (Keyword: 'message'; Token: kwMESSAGE), 
        (Keyword: 'mod'; Token: kwMOD), 
        (Keyword: 'name'; Token: kwNAME), 
        (Keyword: 'near'; Token: kwNEAR), 
        (Keyword: 'nil'; Token: kwNIL), 
        (Keyword: 'nodefault'; Token: kwNODEFAULT), 
        (Keyword: 'not'; Token: kwNOT), 
        (Keyword: 'object'; Token: kwOBJECT), 
        (Keyword: 'of'; Token: kwOF), 
        (Keyword: 'on'; Token: kwON), 
        (Keyword: 'or'; Token: kwOR), 
        (Keyword: 'out'; Token: kwOUT), 
        (Keyword: 'overload'; Token: kwOVERLOAD), 
        (Keyword: 'override'; Token: kwOVERRIDE), 
        (Keyword: 'package'; Token: kwPACKAGE), 
        (Keyword: 'packed'; Token: kwPACKED), 
        (Keyword: 'pascal'; Token: kwPASCAL), 
        (Keyword: 'private'; Token: kwPRIVATE), 
        (Keyword: 'procedure'; Token: kwPROCEDURE), 
        (Keyword: 'program'; Token: kwPROGRAM), 
        (Keyword: 'property'; Token: kwPROPERTY), 
        (Keyword: 'protected'; Token: kwPROTECTED), 
        (Keyword: 'public'; Token: kwPUBLIC), 
        (Keyword: 'published'; Token: kwPUBLISHED), 
        (Keyword: 'raise'; Token: kwRAISE), 
        (Keyword: 'read'; Token: kwREAD), 
        (Keyword: 'readonly'; Token: kwREADONLY), 
        (Keyword: 'record'; Token: kwRECORD), 
        (Keyword: 'register'; Token: kwREGISTER), 
        (Keyword: 'reintroduce'; Token: kwREINTRODUCE), 
        (Keyword: 'repeat'; Token: kwREPEAT), 
        (Keyword: 'requires'; Token: kwREQUIRES), 
        (Keyword: 'resident'; Token: kwRESIDENT), 
        (Keyword: 'resourcestring'; Token: kwRESOURCESTRING), 
        (Keyword: 'safecall'; Token: kwSAFECALL), 
        (Keyword: 'set'; Token: kwSET), 
        (Keyword: 'shl'; Token: kwSHL), 
        (Keyword: 'shr'; Token: kwSHR), 
        (Keyword: 'stdcall'; Token: kwSTDCALL), 
        (Keyword: 'stored'; Token: kwSTORED), 
        (Keyword: 'string'; Token: kwSTRING), 
        (Keyword: 'then'; Token: kwTHEN), 
        (Keyword: 'threadvar'; Token: kwTHREADVAR), 
        (Keyword: 'to'; Token: kwTO), 
        (Keyword: 'try'; Token: kwTRY), 
        (Keyword: 'type'; Token: kwTYPE), 
        (Keyword: 'unit'; Token: kwUNIT), 
        (Keyword: 'until'; Token: kwUNTIL), 
        (Keyword: 'uses'; Token: kwUSES), 
        (Keyword: 'var'; Token: kwVAR), 
        (Keyword: 'virtual'; Token: kwVIRTUAL), 
        (Keyword: 'while'; Token: kwWHILE), 
        (Keyword: 'with'; Token: kwWITH), 
        (Keyword: 'write'; Token: kwWRITE), 
        (Keyword: 'writeonly'; Token: kwWRITEONLY)
);

function GetKeywordToken(const S: String): Integer;

// internal search function for keywords

var L, R, K : Integer;
    Res     : Integer;

begin
  Result := 0;

  // binary search
  L := 0;
  R := KeywordCount - 1;
  while L <= R do
  begin
    K := L + (R - L) div 2;
    Res := CompareText(S, Keywords[K].Keyword);
    if Res = 0 then
    begin
      Result := Keywords[K].Token;
      Break;
    end
    else
      if Res > 0 then L := K + 1
                 else R := K - 1;
  end;
end;
       
 
//------------------------------------------------------------------------------
 
constructor TUceDcgHighlighter.Create(AOwner: TComponent);
 
begin
  inherited;
  // allocate positions array (length is highest possible mark position)
  SetLength(FPositions, MarksHigh[StateCount - 1] + 1);
 
  DefaultFilter := 'Lexical description (*.l)|*.l|Parser description (*.y)|*.y|DCG code template (*.cod)|*.cod|';
 
  // !! adjust attributes !!
  FDCGKeywordAttributes := THighlightAttributes.Create('DCG Keyword');
  FDCGKeywordAttributes.OnChange := HighlightChange;
  FDCGMacroAttributes := THighlightAttributes.Create('DCG Macro');
  FDCGMacroAttributes.OnChange := HighlightChange;
  FDCGCommentAttributes := THighlightAttributes.Create('DCG Comment');
  FDCGCommentAttributes.OnChange := HighlightChange;
 
  FAsmAttributes := THighlightAttributes.Create('Assembler');
  FAsmAttributes.OnChange := HighlightChange;
  FIdentifierAttributes := THighlightAttributes.Create('Identifier');
  FIdentifierAttributes.OnChange := HighlightChange;
  FNumberAttributes := THighlightAttributes.Create('Number');
  FNumberAttributes.OnChange := HighlightChange;
  FSpaceAttributes := THighlightAttributes.Create('White space');
  FSpaceAttributes.OnChange := HighlightChange;
  FStringAttributes := THighlightAttributes.Create('String');
  FStringAttributes.OnChange := HighlightChange;
  FCommentAttributes := THighlightAttributes.Create('Comment');
  FCommentAttributes.OnChange := HighlightChange;
  FSymbolAttributes := THighlightAttributes.Create('Symbol');
  FSymbolAttributes.OnChange := HighlightChange;
  FKeyAttributes := THighlightAttributes.Create('Keyword');
  FKeyAttributes.OnChange := HighlightChange;
  FUnknownAttributes := THighlightAttributes.Create('Unknown');
  FUnknownAttributes.OnChange := HighlightChange;
end;
 
//------------------------------------------------------------------------------
 
destructor TUceDcgHighlighter.Destroy;
 
begin
  FPositions := nil;
  FMatchStack := nil;
 
  // !! adjust attributes !!
  FDCGKeywordAttributes.Free;
  FDCGMacroAttributes.Free;
  FDCGCommentAttributes.Free;
 
  FAsmAttributes.Free;
  FIdentifierAttributes.Free;
  FNumberAttributes.Free;
  FSpaceAttributes.Free;
  FStringAttributes.Free;
  FCommentAttributes.Free;
  FSymbolAttributes.Free;
  FKeyAttributes.Free;
  FUnknownAttributes.Free;
 
  inherited;
end;
 
//------------------------------------------------------------------------------
 
function TUceDcgHighlighter.IsKeyword: Boolean;
 
// checks whether the current token is a keyword,
// if so then the current token type is set accordingly
 
var Word: String;
 
begin
  SetString(Word, FInput + FTokenPos, FCurrentPos - FTokenPos);
  FCurrentToken := GetKeywordToken(Word);
  if FCurrentToken > 0 then
  begin
    Result := True;
    FDoStop := True;
  end
  else Result := False;
end;
 
//------------------------------------------------------------------------------
 
function TUceDcgHighlighter.Eol: Boolean;
 
begin
  Result := (FCurrentToken = -1) or FEOL;
end;
 
//------------------------------------------------------------------------------
 
function TUceDcgHighlighter.GetToken: String;
 
var Len: Cardinal;
 
begin
  Len := FCurrentPos - FTokenPos;
  SetString(Result, FInput + FTokenPos, Len);
end;
 
//------------------------------------------------------------------------------
 
procedure TUceDcgHighlighter.Reset;
 
begin
  FTokenPos := 0;
  FCurrentPos := 0;
  FCurrentToken := 0;
  FLineState := 1;
  FLastChar := #0;
  FMatchCount := 0;
  FEOL := False;
end;
 
//------------------------------------------------------------------------------
 
procedure TUceDcgHighlighter.SetToken(Token: Integer);
 
begin
  FCurrentToken := Token;
  FDoStop := True;
end;
 
//------------------------------------------------------------------------------
 
procedure TUceDcgHighlighter.RuleToToken(Rule: Integer);
 
// var S : String;
 
begin
  // SetString(S, FInput + FTokenPos, FCurrentPos - FTokenPos);
  // actions
  case Rule of
    // <Normal>
      0,
    // <AssemblerState>'[^']*('|\n|\0)
      1: 
        SetToken(STRINGCONSTANT);
       
    // <Normal>#?¤integer¤
      2,
    // <Normal>¤realinteger¤
      3: 
        SetToken(NUMBER);
       
    // <Normal>\"
      4: 
        repeat
          case CurrentChar of
            '"',
            CR, #0:
              begin
                if CurrentChar = '"' then NextChar;
                SetToken(STRINGCONSTANT);
                Break;
              end;
            end;
            NextChar;
          until False;
       
    // <Normal>\'
      5: 
        repeat
          case CurrentChar of
            '''',
            CR, #0:
              begin
                if CurrentChar = '''' then NextChar;
                SetToken(STRINGCONSTANT);
                Break;
              end;
            end;
          NextChar;
        until False;
       
    // <Normal>"//"
      6: 
        begin
          while not (CurrentChar in [CR, #0]) do NextChar;
          SetToken(SLCOMMENT);
        end;
       
    // <Normal>"(*"
      7: 
        repeat
          case CurrentChar of
            '*':
              if Lookahead = ')' then
              begin
                SetToken(MLCOMMENT);
                // skip star and closing parenthesis
                NextChar;
                NextChar;
                Break;
              end;
            CR, #0:
              begin
                State := Comment3;
                SetToken(MLCOMMENT);
                Break;
              end;
          end;
          NextChar;
        until False;
       
    // <Comment3>[^*]+
      8: 
        SetToken(MLCOMMENT);
       
    // <Comment3>\*
      9: 
        begin
          if CurrentChar = ')' then
          begin
            NextChar;
            State := Normal;
          end;
          SetToken(MLCOMMENT);
        end;
       
    // <Normal>\{
      10: 
        repeat
          case CurrentChar of
            '}',
            CR, #0:
              begin
                if CurrentChar = '}' then NextChar
                                     else State := Comment;
                SetToken(MLCOMMENT);
              Break;
            end;
          end;
          NextChar;
        until False;
       
    // <Comment>[^}]+
      11: 
        SetToken(MLCOMMENT);
       
    // <Comment>\}
      12: 
        begin
          SetToken(MLComment);
          State := Normal;
        end;
       
    // <Normal>^%(%|¤identifier¤)
      13,
    // <Normal>^<[^>]+>
      14: 
        SetToken(DCGKEYWORD);
       
    // <Normal>\¤[^¤]+\¤
      15: 
        SetToken(DCGMACRO);
       
    // <Normal>"»".*
      16: 
        SetToken(DCGCOMMENT);
       
    // <Normal>¤identifier¤
      17: 
        if CompareText(GetToken, 'asm') = 0 then
        begin
          State := AssemblerState;
          IsKeyword;
        end
        else
          if not IsKeyword then SetToken(IDENTIFIER);
       
    // <AssemblerState>¤identifier¤
      18: 
        if CompareText(GetToken, 'end') = 0 then
        begin
          IsKeyword;
          State := Normal;
        end
        else SetToken(ASMTOKEN);
       
    // <AssemblerState>\{
      19: 
        repeat
          case CurrentChar of
            '}',
            CR, #0:
              begin
                if CurrentChar = '}' then NextChar
                                     else State := Comment2;
                SetToken(MLCOMMENT);
                Break;
              end;
          end;
          NextChar;
        until False;
       
    // <Comment2>[^}]+
      20: 
        SetToken(MLCOMMENT);
       
    // <Comment2>\}
      21: 
        begin
          SetToken(MLComment);
          State := AssemblerState;
        end;
       
    // <AssemblerState>.
      22: 
        SetToken(ASMTOKEN);
       
    // <Normal>¤white¤+
      23: 
        SetToken(WHITESPACE);
       
    // <Normal>¤symbol¤+
      24: 
        SetToken(SYMBOL);
       
    // <Normal>.
      25: 
        SetToken(UNKNOWN);
       
    // ¤eof¤
      26: 
        SetToken(-1); // special token to mark input end
  end;
end;
 
//------------------------------------------------------------------------------
 
function TUceDcgHighlighter.MatchRule(var Rule : Integer ) : Boolean;
 
// finds the last match and the corresponding marked position and adjusts
// the matched string accordingly;
// returns:
// - True if a rule has been matched, False otherwise
// - Rule: the number of the matched rule
 
begin
  FRejected := False;
  while (FMatchCount > 0) and (FPositions[FMatchStack[FMatchCount - 1]] = 0) do Dec(FMatchCount);
 
  if FMatchCount > 0 then
  begin
    Rule := FMatchStack[FMatchCount - 1];
    FCurrentPos := FTokenPos + FPositions[Rule];
    FPositions[Rule] := 0;
    if (FCurrentPos - FTokenPos) > 0 then FLastChar := FInput[FCurrentPos]
                                     else FLastChar := #0;
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
 
//------------------------------------------------------------------------------
 
function TUceDcgHighlighter.DoDefault : Boolean;
 
begin
  FRejected := False;
  if NextChar <> #0 then Result := True
                    else
  begin
    FLineState := 1;
    Result := False;
  end;
  if not FEOL then FLastChar := FInput[FCurrentPos];
end;
 
//------------------------------------------------------------------------------
 
procedure TUceDcgHighlighter.Next;
 
// scans for the next token and determines its kind (FCurrentToken and FTokenPos
// are set appropriately)
 
var I : Integer;
    Len : Cardinal;
    Matched : Boolean;
    TransLow,
    TransHigh: Integer;
 
begin
  FDoStop := False;
 
  repeat
    // initialize run
    FTokenPos := FCurrentPos;
 
    if FLastChar <> #0 then
      if FLastChar = LF then FLineState := 1
                        else FLineState := 0;
    FCurrentState := FStartState + FLineState;
 
    repeat // character scan loop
      // mark positions and matches
      // for I := MarksLow[FCurrentState] to MarksHigh[FCurrentState] do FPositions[MarkPositionTable[I]] := Len;
      // significant speed improvement by using assembler here:
      Len := FCurrentPos - FTokenPos;
      asm
         PUSH EDI
         PUSH ECX
         MOV EAX, Self
         MOV EDI, [EAX + FPositions]   // address of FPositon array
         MOV ECX, [EDI - 4]            // length of dynamic array (cannot call @DynArrayLength here)
         MOV EAX, [Len]
         REP STOSD
         POP ECX
         POP EDI
      end;
      for I := MatchesHigh[FCurrentState] downto MatchesLow[FCurrentState] do SetMatch(MatchTable[I]);
 
      TransLow := TransitionsLow[FCurrentState];
      TransHigh := TransitionsHigh[FCurrentState];
 
      // determine action
      while (TransLow <= TransHigh) and not (FInput[FCurrentPos] in TransitionTable[TransLow].CharClass) do Inc(TransLow);
 
      if TransLow > TransHigh then Break; // no transition on current char in this state
 
      // get next character
      NextChar;
 
      // switch to new state
      FCurrentState := TransitionTable[TransLow].NextState;
 
    until False;
 
    repeat
      Matched := MatchRule(I);
      if Matched then RuleToToken(I)
                 else Break;
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
 
//------------------------------------------------------------------------------
 
procedure TUceDcgHighlighter.SetLine(const NewValue: String);
 
begin
  FInput := PChar(NewValue);
  Reset;
  Next;
end;
 
//------------------------------------------------------------------------------
 
function TUceDcgHighlighter.GetNextChar: Char;
 
begin
  if not FEOL and (FInput[FCurrentPos] = #0) then FEOL := True;
  Inc(FCurrentPos);
  if not FEOL then
  begin
    FLookahead := FInput[FCurrentPos + 1];
    Result := FInput[FCurrentPos];
  end
  else Result := #0;
end;
 
//------------------------------------------------------------------------------
 
procedure TUceDcgHighlighter.SetState(Value: Cardinal);
 
begin
  FStartState := Value;
end;
 
//------------------------------------------------------------------------------
 
procedure TUceDcgHighlighter.SetMatch(I: Integer);
 
begin
  if FMatchCount >= Length(FMatchStack) then SetLength(FMatchStack, FMatchCount + 1);
  FMatchStack[FMatchCount] := I;
  Inc(FMatchCount);
end;
 
//------------------------------------------------------------------------------
 
procedure TUceDcgHighlighter.Reject;
 
begin
  FRejected := True;
end;
 
//------------------------------------------------------------------------------
 
function TUceDcgHighlighter.GetTokenInfo: TTokenData;
 
// returns all relevant data regarding the current token (length, font styles etc.)
 
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
 
    case FCurrentToken of
      DCGKEYWORD:
        begin
          Background := FDCGKeywordAttributes.Background;
          Foreground := FDCGKeywordAttributes.Foreground;
          Style := FDCGKeywordAttributes.Style;
        end;
      DCGMACRO:
        begin
          Background := FDCGMacroAttributes.Background;
          Foreground := FDCGMacroAttributes.Foreground;
          Style := FDCGMacroAttributes.Style;
        end;
      DCGCOMMENT:
        begin
          Background := FDCGCommentAttributes.Background;
          Foreground := FDCGCommentAttributes.Foreground;
          Style := FDCGCommentAttributes.Style;
        end;
      ASMTOKEN:
        begin
          Background := FAsmAttributes.Background;
          Foreground := FAsmAttributes.Foreground;
          Style := FAsmAttributes.Style;
        end;
      IDENTIFIER:
        begin
          Background := FIdentifierAttributes.Background;
          Foreground := FIdentifierAttributes.Foreground;
          Style := FIdentifierAttributes.Style;
        end;
      NUMBER:
        begin
          Background := FNumberAttributes.Background;
          Foreground := FNumberAttributes.Foreground;
          Style := FNumberAttributes.Style;
        end;
      WHITESPACE:
        begin
          Background := FSpaceAttributes.Background;
          Foreground := FSpaceAttributes.Foreground;
          Style := FSpaceAttributes.Style;
        end;
      STRINGCONSTANT:
        begin
          Background := FStringAttributes.Background;
          Foreground := FStringAttributes.Foreground;
          Style := FStringAttributes.Style;
        end;
        SLCOMMENT,
        MLCOMMENT:
        begin
          Background := FCommentAttributes.Background;
          Foreground := FCommentAttributes.Foreground;
          Style := FCommentAttributes.Style;
        end;
      SYMBOL:
        begin
          Background := FSymbolAttributes.Background;
          Foreground := FSymbolAttributes.Foreground;
          Style := FSymbolAttributes.Style;
        end;
      kwFirstKeyword..kwLastKeyword:
        begin
          Background := FKeyAttributes.Background;
          Foreground := FKeyAttributes.Foreground;
          Style := FKeyAttributes.Style;
        end;
    else // UNKNOWN
        begin
          Background := FUnknownAttributes.Background;
          Foreground := FUnknownAttributes.Foreground;
          Style := FUnknownAttributes.Style;
        end;
    end;
  end;
end;
 
//------------------------------------------------------------------------------
 
function TUceDcgHighlighter.GetCurrentChar: Char;
 
begin
  Result := FInput[FCurrentPos];
end;
 
//------------------------------------------------------------------------------
 
procedure TUceDcgHighlighter.HighlightChange(Sender:TObject);
 
begin
  WindowList.Invalidate;
end;
 
//------------------------------------------------------------------------------
 
function TUceDcgHighlighter.GetIdentChars: TIdentChars;
 
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;
 
//------------------------------------------------------------------------------
 
function TUceDcgHighlighter.GetAttributeCount: Integer;
 
begin
  Result := 12;
end;
 
//------------------------------------------------------------------------------
 
function TUceDcgHighlighter.GetAttribute(Index: Integer): THighlightAttributes;
 
begin
  // !! adjust attributes !!
  case Index of
    0:
      Result := FDCGKeywordAttributes;
    1:
      Result := FDCGMacroAttributes;
    2:
      Result := FDCGCommentAttributes;
    3:
      Result := FAsmAttributes;
    4:
      Result := FIdentifierAttributes;
    5:
      Result := FNumberAttributes;
    6:
      Result := FSpaceAttributes;
    7:
      Result := FStringAttributes;
    8:
      Result := FCommentAttributes;
    9:
      Result := FSymbolAttributes;
    10:
      Result := FKeyAttributes;
    11:
      Result := FUnknownAttributes;
    else Result := nil;
  end;
end;
 
//------------------------------------------------------------------------------
 
function TUceDcgHighlighter.GetLanguageName: String;
 
begin
  Result := 'DCG';
end;
 
//------------------------------------------------------------------------------
 
procedure TUceDcgHighlighter.SetAttribute(const Index: Integer; const Value: THighlightAttributes);
 
begin
  // !! adjust attributes !!
  case Index of
    0:
      FDCGKeywordAttributes.Assign(Value);
    1:
      FDCGMacroAttributes.Assign(Value);
    2:
      FDCGCommentAttributes.Assign(Value);
    3:
      FAsmAttributes.Assign(Value);
    4:
      FIdentifierAttributes.Assign(Value);
    5:
      FNumberAttributes.Assign(Value);
    6:
      FSpaceAttributes.Assign(Value);
    7:
      FStringAttributes.Assign(Value);
    8:
      FCommentAttributes.Assign(Value);
    9:
      FSymbolAttributes.Assign(Value);
    10:
      FKeyAttributes.Assign(Value);
    11:
      FUnknownAttributes.Assign(Value);
  end;
end;
 
//------------------------------------------------------------------------------
 
function TUceDcgHighlighter.GetRange: Integer;
 
begin
  Result := FStartState;
end;
 
//------------------------------------------------------------------------------
 
function TUceDcgHighlighter.HasMoreTokens: Boolean;
 
begin
  Result := FCurrentToken > -1;
end;
 
//--------------------------------------------------------------------------------
 
procedure TUceDcgHighlighter.SetRange(Value: Integer);
 
begin
  FStartState := Value;
end;
 
//------------------------------------------------------------------------------
 
procedure TUceDcgHighlighter.ResetRange;
 
begin
  FStartState := Normal;
  // in case someone will ask for it immediately afterwards
  FCurrentState := Normal;
end;
 
//------------------------------------------------------------------------------
 
end.
