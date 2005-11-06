unit UCESQLHighlighter;
 
// This file is machine-generated. Do not modify it manually.
 
interface
 
uses
  Classes, Graphics, UCEHighlighter;
 
const
  CR = #13;
  LF = #10;
 
// User defined start states and tokens.
 
 
const Normal = 2;
      MultilineComment = 4;
      EmbeddedCommand = 6;
 
 
const kwFirstKeyword = 257;
      kwADD = 257;
      kwALGORITHM = 258;
      kwALL = 259;
      kwALTER = 260;
      kwANALYZE = 261;
      kwAND = 262;
      kwAS = 263;
      kwASC = 264;
      kwASENSITIVE = 265;
      kwAUTO_INCREMENT = 266;
      kwBEFORE = 267;
      kwBEGIN = 268;
      kwBETWEEN = 269;
      kwBIGINT = 270;
      kwBINARY = 271;
      kwBLOB = 272;
      kwBOTH = 273;
      kwBY = 274;
      kwCALL = 275;
      kwCASCADE = 276;
      kwCASE = 277;
      kwCHANGE = 278;
      kwCHAR = 279;
      kwCHARACTER = 280;
      kwCHARSET = 281;
      kwCHECK = 282;
      kwCOLLATE = 283;
      kwCOLUMN = 284;
      kwCOLUMNS = 285;
      kwCOMMIT = 286;
      kwCONDITION = 287;
      kwCONNECTION = 288;
      kwCONSTRAINT = 289;
      kwCONTINUE = 290;
      kwCONVERT = 291;
      kwCREATE = 292;
      kwCROSS = 293;
      kwCURRENT_DATE = 294;
      kwCURRENT_TIME = 295;
      kwCURRENT_TIMESTAMP = 296;
      kwCURRENT_USER = 297;
      kwCURSOR = 298;
      kwDATABASE = 299;
      kwDATABASES = 300;
      kwDATE = 301;
      kwDATETIME = 302;
      kwDAY_HOUR = 303;
      kwDAY_MICROSECOND = 304;
      kwDAY_MINUTE = 305;
      kwDAY_SECOND = 306;
      kwDEC = 307;
      kwDECIMAL = 308;
      kwDECLARE = 309;
      kwDEFAULT = 310;
      kwDELAYED = 311;
      kwDELETE = 312;
      kwDELIMITER = 313;
      kwDESC = 314;
      kwDESCRIBE = 315;
      kwDETERMINISTIC = 316;
      kwDISTINCT = 317;
      kwDISTINCTROW = 318;
      kwDIV = 319;
      kwDOUBLE = 320;
      kwDROP = 321;
      kwDUAL = 322;
      kwEACH = 323;
      kwELSE = 324;
      kwELSEIF = 325;
      kwENCLOSED = 326;
      kwEND = 327;
      kwENGINE = 328;
      kwENUM = 329;
      kwEQUAL = 330;
      kwESCAPED = 331;
      kwEXISTS = 332;
      kwEXIT = 333;
      kwEXPLAIN = 334;
      kwFALSE = 335;
      kwFETCH = 336;
      kwFIELDS = 337;
      kwFLOAT = 338;
      kwFOR = 339;
      kwFORCE = 340;
      kwFOREIGN = 341;
      kwFOUND = 342;
      kwFROM = 343;
      kwFULLTEXT = 344;
      kwFUNCTION = 345;
      kwGOTO = 346;
      kwGRANT = 347;
      kwGREATER = 348;
      kwGROUP = 349;
      kwHASH = 350;
      kwHAVING = 351;
      kwHIGH_PRIORITY = 352;
      kwHOUR_MICROSECOND = 353;
      kwHOUR_MINUTE = 354;
      kwHOUR_SECOND = 355;
      kwIF = 356;
      kwIGNORE = 357;
      kwIN = 358;
      kwINDEX = 359;
      kwINFILE = 360;
      kwINNER = 361;
      kwINOUT = 362;
      kwINSENSITIVE = 363;
      kwINSERT = 364;
      kwINT = 365;
      kwINTEGER = 366;
      kwINTERVAL = 367;
      kwINTO = 368;
      kwIS = 369;
      kwITERATE = 370;
      kwJOIN = 371;
      kwKEY = 372;
      kwKEYS = 373;
      kwKILL = 374;
      kwLEADING = 375;
      kwLEAVE = 376;
      kwLEFT = 377;
      kwLESS = 378;
      kwLIKE = 379;
      kwLIMIT = 380;
      kwLINES = 381;
      kwLIST = 382;
      kwLOAD = 383;
      kwLOCALTIME = 384;
      kwLOCALTIMESTAMP = 385;
      kwLOCK = 386;
      kwLONG = 387;
      kwLONGBLOB = 388;
      kwLONGTEXT = 389;
      kwLOOP = 390;
      kwLOW_PRIORITY = 391;
      kwMATCH = 392;
      kwMEDIUMBLOB = 393;
      kwMEDIUMINT = 394;
      kwMEDIUMTEXT = 395;
      kwMIDDLEINT = 396;
      kwMINUTE_MICROSECOND = 397;
      kwMINUTE_SECOND = 398;
      kwMOD = 399;
      kwMODIFY = 400;
      kwNATURAL = 401;
      kwNOT = 402;
      kwNO_WRITE_TO_BINLOG = 403;
      kwNULL = 404;
      kwNUMERIC = 405;
      kwON = 406;
      kwOPTIMIZE = 407;
      kwOPTION = 408;
      kwOPTIONALLY = 409;
      kwOR = 410;
      kwORDER = 411;
      kwOUT = 412;
      kwOUTER = 413;
      kwOUTFILE = 414;
      kwPARTITION = 415;
      kwPARTITIONS = 416;
      kwPASSWORD = 417;
      kwPRECISION = 418;
      kwPRIMARY = 419;
      kwPRIVILEGES = 420;
      kwPROCEDURE = 421;
      kwPURGE = 422;
      kwRANGE = 423;
      kwREAD = 424;
      kwREAL = 425;
      kwREFERENCES = 426;
      kwREGEXP = 427;
      kwRENAME = 428;
      kwREPEAT = 429;
      kwREPLACE = 430;
      kwREQUIRE = 431;
      kwRESTRICT = 432;
      kwRETURN = 433;
      kwRETURNS = 434;
      kwREVOKE = 435;
      kwRIGHT = 436;
      kwRLIKE = 437;
      kwROLLBACK = 438;
      kwSCHEMA = 439;
      kwSCHEMAS = 440;
      kwSECOND_MICROSECOND = 441;
      kwSELECT = 442;
      kwSENSITIVE = 443;
      kwSEPARATOR = 444;
      kwSET = 445;
      kwSHOW = 446;
      kwSMALLINT = 447;
      kwSONAME = 448;
      kwSPATIAL = 449;
      kwSPECIFIC = 450;
      kwSQL = 451;
      kwSQLEXCEPTION = 452;
      kwSQLSTATE = 453;
      kwSQLWARNING = 454;
      kwSQL_BIG_RESULT = 455;
      kwSQL_CALC_FOUND_ROWS = 456;
      kwSQL_SMALL_RESULT = 457;
      kwSSL = 458;
      kwSTART = 459;
      kwSTARTING = 460;
      kwSTRAIGHT_JOIN = 461;
      kwTABLE = 462;
      kwTABLES = 463;
      kwTEMPORARY = 464;
      kwTERMINATED = 465;
      kwTHAN = 466;
      kwTHEN = 467;
      kwTIMESTAMP = 468;
      kwTINYBLOB = 469;
      kwTINYINT = 470;
      kwTINYTEXT = 471;
      kwTO = 472;
      kwTRAILING = 473;
      kwTRANSACTION = 474;
      kwTRIGGER = 475;
      kwTRUE = 476;
      kwUNDEFINED = 477;
      kwUNDO = 478;
      kwUNION = 479;
      kwUNIQUE = 480;
      kwUNLOCK = 481;
      kwUNSIGNED = 482;
      kwUPDATE = 483;
      kwUSAGE = 484;
      kwUSE = 485;
      kwUSING = 486;
      kwUTC_DATE = 487;
      kwUTC_TIME = 488;
      kwUTC_TIMESTAMP = 489;
      kwVALUE = 490;
      kwVALUES = 491;
      kwVARBINARY = 492;
      kwVARCHAR = 493;
      kwVARCHARACTER = 494;
      kwVARYING = 495;
      kwVIEW = 496;
      kwWHEN = 497;
      kwWHERE = 498;
      kwWHILE = 499;
      kwWITH = 500;
      kwWRITE = 501;
      kwXOR = 502;
      kwYEAR_MONTH = 503;
      kwZEROFILL = 504;
      kwLastKeyword = 504;

const COMMENT_WITH_COMMAND = 505;
      EMBEDDED_COMMAND = 506;
      FLOATNUMBER = 507;
      IDENTIFIER = 508;
      INTEGERNUMBER = 509;
      KEYWORD = 510;
      MLCOMMENT = 511;
      SLCOMMENT = 512;
      STRINGCONSTANT = 513;
      SYMBOL = 514;
      SYSTEM_VARIABLE = 515;
      UNKNOWN = 516;
      USER_VARIABLE = 517;
      WHITESPACE = 518;


 
type
  TUCESQLHighlighter = class(TUCEHighlighter)
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
 
    FSystemVariableAttributes,
    FUserVariableAttributes,
    FCommentWithCOmmandAttributes,
    FEmbeddedCommandAttributes,
    FStringAttributes,
    FNumberAttributes,
    FKeyAttributes,
    FSymbolAttributes,
    FCommentAttributes,
    FIdentifierAttributes,
    FSpaceAttributes: THighlightAttributes;
    procedure RuleToToken(Rule: Integer);
    function IsKeyword: Boolean;
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
    function HasMoreTokens: Boolean; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Integer); override;
    procedure SetLine(const NewValue: string); override;
 
    property CurrentChar: Char read GetCurrentChar;
    property NextChar: Char read GetNextChar;
    property Lookahead: Char read FLookahead;
    property State: Cardinal read FCurrentState write SetState; // read to get current lexer state
                                                                // write to set start state for next run
    property TokenPosition: Cardinal read FTokenPos;
  published
    property CommentAttributes: THighlightAttributes index 1 read FCommentAttributes write SetAttribute;
    property CommentWithCommandAttributes: THighlightAttributes index 8 read FCommentWithCommandAttributes write SetAttribute;
    property EmbeddedCommandAttributes: THighlightAttributes index 7 read FEmbeddedCommandAttributes write SetAttribute;
    property IdentifierAttributes: THighlightAttributes index 2 read FIdentifierAttributes write SetAttribute;
    property KeyAttributes: THighlightAttributes index 3 read FKeyAttributes write SetAttribute;
    property NumberAttributes: THighlightAttributes index 4 read FNumberAttributes write SetAttribute;
    property SpaceAttributes: THighlightAttributes index 5 read FSpaceAttributes write SetAttribute;
    property StringAttributes: THighlightAttributes index 6 read FStringAttributes write SetAttribute;
    property SymbolAttributes: THighlightAttributes index 7 read FSymbolAttributes write SetAttribute;
    property SystemVariableAttributes: THighlightAttributes index 9 read FSystemVariableAttributes write SetAttribute;
    property UserVariableAttributes: THighlightAttributes index 10 read FuserVariableAttributes write SetAttribute;
  end;
 
//----------------------------------------------------------------------------------------------------------------------
 
implementation
 
uses
  SysUtils;
 
// DFA table

type TTransition = record
       CharClass : set of Char;
       NextState : Integer;
     end;

const
  MarkPositionCount = 38;
  MatchCount        = 38;
  TransitionCount   = 92;
  StateCount        = 46;

  MarkPositionTable : array[0..MarkPositionCount - 1] of Integer = (
    14, 14, 17, 0, 16, 16, 2, 3, 16, 16, 16, 7, 16, 8, 16, 16, 10, 16, 16, 2, 16, 
    16, 12, 13, 14, 15, 0, 1, 3, 3, 5, 9, 11, 1, 3, 4, 6, 1);

  MatchTable : array [0..MatchCount - 1] of Integer = (
    14, 14, 17, 0, 16, 16, 2, 3, 16, 16, 16, 7, 16, 8, 16, 16, 10, 16, 16, 2, 16, 
    16, 12, 13, 14, 15, 0, 1, 3, 3, 5, 9, 11, 1, 3, 4, 6, 1);

  TransitionTable : array [0..TransitionCount - 1] of TTransition = (
    (CharClass: [#0]; NextState: 8),
    (CharClass: [#0]; NextState: 8),
    (CharClass: [#0]; NextState: 8),
    (CharClass: [#1..#9, #11..' ']; NextState: 20),
    (CharClass: [#10]; NextState: 11),
    (CharClass: ['!', '$'..'&', '('..'*', ',', '.', ':'..'?', '['..'^', '{'..#255]; NextState: 21),
    (CharClass: ['"']; NextState: 16),
    (CharClass: ['#']; NextState: 18),
    (CharClass: ['''']; NextState: 15),
    (CharClass: ['+']; NextState: 10),
    (CharClass: ['-']; NextState: 17),
    (CharClass: ['/']; NextState: 19),
    (CharClass: ['0'..'9']; NextState: 9),
    (CharClass: ['@']; NextState: 14),
    (CharClass: ['A'..'Z', '_', 'a'..'z']; NextState: 12),
    (CharClass: ['`']; NextState: 13),
    (CharClass: [#0]; NextState: 8),
    (CharClass: [#1..#9, #11..' ']; NextState: 20),
    (CharClass: [#10]; NextState: 11),
    (CharClass: ['!', '$'..'&', '('..'*', ',', '.', ':'..'?', '['..'^', '{'..#255]; NextState: 21),
    (CharClass: ['"']; NextState: 16),
    (CharClass: ['#']; NextState: 18),
    (CharClass: ['''']; NextState: 15),
    (CharClass: ['+']; NextState: 10),
    (CharClass: ['-']; NextState: 17),
    (CharClass: ['/']; NextState: 19),
    (CharClass: ['0'..'9']; NextState: 9),
    (CharClass: ['@']; NextState: 14),
    (CharClass: ['A'..'Z', '_', 'a'..'z']; NextState: 12),
    (CharClass: ['`']; NextState: 13),
    (CharClass: [#0]; NextState: 8),
    (CharClass: [#1..')', '+'..#255]; NextState: 22),
    (CharClass: ['*']; NextState: 23),
    (CharClass: [#0]; NextState: 8),
    (CharClass: [#1..')', '+'..#255]; NextState: 22),
    (CharClass: ['*']; NextState: 23),
    (CharClass: [#0]; NextState: 8),
    (CharClass: [#1..')', '+'..#255]; NextState: 24),
    (CharClass: ['*']; NextState: 25),
    (CharClass: [#0]; NextState: 8),
    (CharClass: [#1..')', '+'..#255]; NextState: 24),
    (CharClass: ['*']; NextState: 25),
    (CharClass: ['.']; NextState: 27),
    (CharClass: ['0'..'9']; NextState: 26),
    (CharClass: ['A'..'D', 'F'..'Z', '_', 'a'..'d', 'f'..'z']; NextState: 29),
    (CharClass: ['E', 'e']; NextState: 28),
    (CharClass: ['0'..'9']; NextState: 30),
    (CharClass: [#1..' ']; NextState: 11),
    (CharClass: ['0'..'9', 'A'..'Z', '_', 'a'..'z']; NextState: 29),
    (CharClass: ['0'..'9']; NextState: 32),
    (CharClass: ['A'..'Z', '_', 'a'..'z']; NextState: 31),
    (CharClass: ['0'..'9']; NextState: 34),
    (CharClass: ['@']; NextState: 35),
    (CharClass: ['A'..'Z', '_', 'a'..'z']; NextState: 33),
    (CharClass: ['-']; NextState: 36),
    (CharClass: ['0'..'9']; NextState: 30),
    (CharClass: ['*']; NextState: 37),
    (CharClass: [#1..' ']; NextState: 11),
    (CharClass: [#1..')', '+'..#255]; NextState: 22),
    (CharClass: [#1..')', '+'..#255]; NextState: 24),
    (CharClass: ['.']; NextState: 27),
    (CharClass: ['0'..'9']; NextState: 26),
    (CharClass: ['A'..'D', 'F'..'Z', '_', 'a'..'d', 'f'..'z']; NextState: 29),
    (CharClass: ['E', 'e']; NextState: 28),
    (CharClass: ['0'..'9']; NextState: 27),
    (CharClass: ['E', 'e']; NextState: 38),
    (CharClass: ['+', '-']; NextState: 39),
    (CharClass: ['0'..'9']; NextState: 40),
    (CharClass: ['A'..'Z', '_', 'a'..'z']; NextState: 29),
    (CharClass: ['0'..'9', 'A'..'Z', '_', 'a'..'z']; NextState: 29),
    (CharClass: ['.']; NextState: 27),
    (CharClass: ['0'..'9']; NextState: 30),
    (CharClass: ['E', 'e']; NextState: 41),
    (CharClass: ['0'..'9', 'A'..'Z', '_', 'a'..'z']; NextState: 31),
    (CharClass: ['`']; NextState: 42),
    (CharClass: ['0'..'9']; NextState: 32),
    (CharClass: ['A'..'Z', '_', 'a'..'z']; NextState: 31),
    (CharClass: ['0'..'9', 'A'..'Z', '_', 'a'..'z']; NextState: 33),
    (CharClass: ['0'..'9']; NextState: 34),
    (CharClass: ['A'..'Z', '_', 'a'..'z']; NextState: 33),
    (CharClass: ['0'..'9']; NextState: 35),
    (CharClass: ['A'..'Z', '_', 'a'..'z']; NextState: 43),
    (CharClass: ['+', '-']; NextState: 44),
    (CharClass: ['0'..'9']; NextState: 45),
    (CharClass: ['0'..'9']; NextState: 45),
    (CharClass: ['0'..'9']; NextState: 40),
    (CharClass: ['A'..'Z', '_', 'a'..'z']; NextState: 29),
    (CharClass: ['+', '-']; NextState: 39),
    (CharClass: ['0'..'9']; NextState: 45),
    (CharClass: ['0'..'9', 'A'..'Z', '_', 'a'..'z']; NextState: 43),
    (CharClass: ['0'..'9']; NextState: 45),
    (CharClass: ['0'..'9']; NextState: 45)
    );

  MarksLow : array [0..StateCount-1] of Integer = (
    0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 5, 6, 7, 9, 10, 11, 13, 15, 16, 18, 19, 21, 22, 
    23, 24, 25, 26, 27, 28, 29, 30, 30, 30, 30, 31, 31, 31, 32, 33, 33, 33, 35, 
    35, 36, 37, 37);

  MarksHigh : array [0..StateCount-1] of Integer = (
    -1, -1, -1, -1, -1, -1, 0, 1, 2, 4, 5, 6, 8, 9, 10, 12, 14, 15, 17, 18, 20, 
    21, 22, 23, 24, 25, 26, 27, 28, 29, 29, 29, 29, 30, 30, 30, 31, 32, 32, 32, 
    34, 34, 35, 36, 36, 37);

  MatchesLow : array [0..StateCount-1] of Integer = (
    0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 5, 6, 7, 9, 10, 11, 13, 15, 16, 18, 19, 21, 22, 
    23, 24, 25, 26, 27, 28, 29, 30, 30, 30, 30, 31, 31, 31, 32, 33, 33, 33, 35, 
    35, 36, 37, 37);

  MatchesHigh : array [0..StateCount-1] of Integer = (
    -1, -1, -1, -1, -1, -1, 0, 1, 2, 4, 5, 6, 8, 9, 10, 12, 14, 15, 17, 18, 20, 
    21, 22, 23, 24, 25, 26, 27, 28, 29, 29, 29, 29, 30, 30, 30, 31, 32, 32, 32, 
    34, 34, 35, 36, 36, 37);

  TransitionsLow : array [0..StateCount-1] of Integer = (
    0, 1, 2, 16, 30, 33, 36, 39, 42, 42, 46, 47, 48, 49, 51, 54, 54, 54, 56, 56, 
    57, 58, 58, 59, 59, 60, 60, 64, 66, 69, 70, 73, 75, 77, 78, 80, 82, 82, 82, 
    84, 85, 87, 89, 89, 90, 91);

  TransitionsHigh : array [0..StateCount-1] of Integer = (
    0, 1, 15, 29, 32, 35, 38, 41, 41, 45, 46, 47, 48, 50, 53, 53, 53, 55, 55, 56, 
    57, 57, 58, 58, 59, 59, 63, 65, 68, 69, 72, 74, 76, 77, 79, 81, 81, 81, 83, 
    84, 86, 88, 88, 89, 90, 91);

type // keyword table support
     TKeyword = record
       Keyword: String;
       Token: Integer;
     end;

const KeywordCount = 248;
      Keywords : array[0..KeywordCount - 1] of TKeyword = (
        (Keyword: 'ADD'; Token: kwADD), 
        (Keyword: 'ALGORITHM'; Token: kwALGORITHM), 
        (Keyword: 'ALL'; Token: kwALL), 
        (Keyword: 'ALTER'; Token: kwALTER), 
        (Keyword: 'ANALYZE'; Token: kwANALYZE), 
        (Keyword: 'AND'; Token: kwAND), 
        (Keyword: 'AS'; Token: kwAS), 
        (Keyword: 'ASC'; Token: kwASC), 
        (Keyword: 'ASENSITIVE'; Token: kwASENSITIVE), 
        (Keyword: 'AUTO_INCREMENT'; Token: kwAUTO_INCREMENT), 
        (Keyword: 'BEFORE'; Token: kwBEFORE), 
        (Keyword: 'BEGIN'; Token: kwBEGIN), 
        (Keyword: 'BETWEEN'; Token: kwBETWEEN), 
        (Keyword: 'BIGINT'; Token: kwBIGINT), 
        (Keyword: 'BINARY'; Token: kwBINARY), 
        (Keyword: 'BLOB'; Token: kwBLOB), 
        (Keyword: 'BOTH'; Token: kwBOTH), 
        (Keyword: 'BY'; Token: kwBY), 
        (Keyword: 'CALL'; Token: kwCALL), 
        (Keyword: 'CASCADE'; Token: kwCASCADE), 
        (Keyword: 'CASE'; Token: kwCASE), 
        (Keyword: 'CHANGE'; Token: kwCHANGE), 
        (Keyword: 'CHAR'; Token: kwCHAR), 
        (Keyword: 'CHARACTER'; Token: kwCHARACTER), 
        (Keyword: 'CHARSET'; Token: kwCHARSET), 
        (Keyword: 'CHECK'; Token: kwCHECK), 
        (Keyword: 'COLLATE'; Token: kwCOLLATE), 
        (Keyword: 'COLUMN'; Token: kwCOLUMN), 
        (Keyword: 'COLUMNS'; Token: kwCOLUMNS), 
        (Keyword: 'COMMIT'; Token: kwCOMMIT), 
        (Keyword: 'CONDITION'; Token: kwCONDITION), 
        (Keyword: 'CONNECTION'; Token: kwCONNECTION), 
        (Keyword: 'CONSTRAINT'; Token: kwCONSTRAINT), 
        (Keyword: 'CONTINUE'; Token: kwCONTINUE), 
        (Keyword: 'CONVERT'; Token: kwCONVERT), 
        (Keyword: 'CREATE'; Token: kwCREATE), 
        (Keyword: 'CROSS'; Token: kwCROSS), 
        (Keyword: 'CURRENT_DATE'; Token: kwCURRENT_DATE), 
        (Keyword: 'CURRENT_TIME'; Token: kwCURRENT_TIME), 
        (Keyword: 'CURRENT_TIMESTAMP'; Token: kwCURRENT_TIMESTAMP), 
        (Keyword: 'CURRENT_USER'; Token: kwCURRENT_USER), 
        (Keyword: 'CURSOR'; Token: kwCURSOR), 
        (Keyword: 'DATABASE'; Token: kwDATABASE), 
        (Keyword: 'DATABASES'; Token: kwDATABASES), 
        (Keyword: 'DATE'; Token: kwDATE), 
        (Keyword: 'DATETIME'; Token: kwDATETIME), 
        (Keyword: 'DAY_HOUR'; Token: kwDAY_HOUR), 
        (Keyword: 'DAY_MICROSECOND'; Token: kwDAY_MICROSECOND), 
        (Keyword: 'DAY_MINUTE'; Token: kwDAY_MINUTE), 
        (Keyword: 'DAY_SECOND'; Token: kwDAY_SECOND), 
        (Keyword: 'DEC'; Token: kwDEC), 
        (Keyword: 'DECIMAL'; Token: kwDECIMAL), 
        (Keyword: 'DECLARE'; Token: kwDECLARE), 
        (Keyword: 'DEFAULT'; Token: kwDEFAULT), 
        (Keyword: 'DELAYED'; Token: kwDELAYED), 
        (Keyword: 'DELETE'; Token: kwDELETE), 
        (Keyword: 'DELIMITER'; Token: kwDELIMITER), 
        (Keyword: 'DESC'; Token: kwDESC), 
        (Keyword: 'DESCRIBE'; Token: kwDESCRIBE), 
        (Keyword: 'DETERMINISTIC'; Token: kwDETERMINISTIC), 
        (Keyword: 'DISTINCT'; Token: kwDISTINCT), 
        (Keyword: 'DISTINCTROW'; Token: kwDISTINCTROW), 
        (Keyword: 'DIV'; Token: kwDIV), 
        (Keyword: 'DOUBLE'; Token: kwDOUBLE), 
        (Keyword: 'DROP'; Token: kwDROP), 
        (Keyword: 'DUAL'; Token: kwDUAL), 
        (Keyword: 'EACH'; Token: kwEACH), 
        (Keyword: 'ELSE'; Token: kwELSE), 
        (Keyword: 'ELSEIF'; Token: kwELSEIF), 
        (Keyword: 'ENCLOSED'; Token: kwENCLOSED), 
        (Keyword: 'END'; Token: kwEND), 
        (Keyword: 'ENGINE'; Token: kwENGINE), 
        (Keyword: 'ENUM'; Token: kwENUM), 
        (Keyword: 'EQUAL'; Token: kwEQUAL), 
        (Keyword: 'ESCAPED'; Token: kwESCAPED), 
        (Keyword: 'EXISTS'; Token: kwEXISTS), 
        (Keyword: 'EXIT'; Token: kwEXIT), 
        (Keyword: 'EXPLAIN'; Token: kwEXPLAIN), 
        (Keyword: 'FALSE'; Token: kwFALSE), 
        (Keyword: 'FETCH'; Token: kwFETCH), 
        (Keyword: 'FIELDS'; Token: kwFIELDS), 
        (Keyword: 'FLOAT'; Token: kwFLOAT), 
        (Keyword: 'FOR'; Token: kwFOR), 
        (Keyword: 'FORCE'; Token: kwFORCE), 
        (Keyword: 'FOREIGN'; Token: kwFOREIGN), 
        (Keyword: 'FOUND'; Token: kwFOUND), 
        (Keyword: 'FROM'; Token: kwFROM), 
        (Keyword: 'FULLTEXT'; Token: kwFULLTEXT), 
        (Keyword: 'FUNCTION'; Token: kwFUNCTION), 
        (Keyword: 'GOTO'; Token: kwGOTO), 
        (Keyword: 'GRANT'; Token: kwGRANT), 
        (Keyword: 'GREATER'; Token: kwGREATER), 
        (Keyword: 'GROUP'; Token: kwGROUP), 
        (Keyword: 'HASH'; Token: kwHASH), 
        (Keyword: 'HAVING'; Token: kwHAVING), 
        (Keyword: 'HIGH_PRIORITY'; Token: kwHIGH_PRIORITY), 
        (Keyword: 'HOUR_MICROSECOND'; Token: kwHOUR_MICROSECOND), 
        (Keyword: 'HOUR_MINUTE'; Token: kwHOUR_MINUTE), 
        (Keyword: 'HOUR_SECOND'; Token: kwHOUR_SECOND), 
        (Keyword: 'IF'; Token: kwIF), 
        (Keyword: 'IGNORE'; Token: kwIGNORE), 
        (Keyword: 'IN'; Token: kwIN), 
        (Keyword: 'INDEX'; Token: kwINDEX), 
        (Keyword: 'INFILE'; Token: kwINFILE), 
        (Keyword: 'INNER'; Token: kwINNER), 
        (Keyword: 'INOUT'; Token: kwINOUT), 
        (Keyword: 'INSENSITIVE'; Token: kwINSENSITIVE), 
        (Keyword: 'INSERT'; Token: kwINSERT), 
        (Keyword: 'INT'; Token: kwINT), 
        (Keyword: 'INTEGER'; Token: kwINTEGER), 
        (Keyword: 'INTERVAL'; Token: kwINTERVAL), 
        (Keyword: 'INTO'; Token: kwINTO), 
        (Keyword: 'IS'; Token: kwIS), 
        (Keyword: 'ITERATE'; Token: kwITERATE), 
        (Keyword: 'JOIN'; Token: kwJOIN), 
        (Keyword: 'KEY'; Token: kwKEY), 
        (Keyword: 'KEYS'; Token: kwKEYS), 
        (Keyword: 'KILL'; Token: kwKILL), 
        (Keyword: 'LEADING'; Token: kwLEADING), 
        (Keyword: 'LEAVE'; Token: kwLEAVE), 
        (Keyword: 'LEFT'; Token: kwLEFT), 
        (Keyword: 'LESS'; Token: kwLESS), 
        (Keyword: 'LIKE'; Token: kwLIKE), 
        (Keyword: 'LIMIT'; Token: kwLIMIT), 
        (Keyword: 'LINES'; Token: kwLINES), 
        (Keyword: 'LIST'; Token: kwLIST), 
        (Keyword: 'LOAD'; Token: kwLOAD), 
        (Keyword: 'LOCALTIME'; Token: kwLOCALTIME), 
        (Keyword: 'LOCALTIMESTAMP'; Token: kwLOCALTIMESTAMP), 
        (Keyword: 'LOCK'; Token: kwLOCK), 
        (Keyword: 'LONG'; Token: kwLONG), 
        (Keyword: 'LONGBLOB'; Token: kwLONGBLOB), 
        (Keyword: 'LONGTEXT'; Token: kwLONGTEXT), 
        (Keyword: 'LOOP'; Token: kwLOOP), 
        (Keyword: 'LOW_PRIORITY'; Token: kwLOW_PRIORITY), 
        (Keyword: 'MATCH'; Token: kwMATCH), 
        (Keyword: 'MEDIUMBLOB'; Token: kwMEDIUMBLOB), 
        (Keyword: 'MEDIUMINT'; Token: kwMEDIUMINT), 
        (Keyword: 'MEDIUMTEXT'; Token: kwMEDIUMTEXT), 
        (Keyword: 'MIDDLEINT'; Token: kwMIDDLEINT), 
        (Keyword: 'MINUTE_MICROSECOND'; Token: kwMINUTE_MICROSECOND), 
        (Keyword: 'MINUTE_SECOND'; Token: kwMINUTE_SECOND), 
        (Keyword: 'MOD'; Token: kwMOD), 
        (Keyword: 'MODIFY'; Token: kwMODIFY), 
        (Keyword: 'NATURAL'; Token: kwNATURAL), 
        (Keyword: 'NOT'; Token: kwNOT), 
        (Keyword: 'NO_WRITE_TO_BINLOG'; Token: kwNO_WRITE_TO_BINLOG), 
        (Keyword: 'NULL'; Token: kwNULL), 
        (Keyword: 'NUMERIC'; Token: kwNUMERIC), 
        (Keyword: 'ON'; Token: kwON), 
        (Keyword: 'OPTIMIZE'; Token: kwOPTIMIZE), 
        (Keyword: 'OPTION'; Token: kwOPTION), 
        (Keyword: 'OPTIONALLY'; Token: kwOPTIONALLY), 
        (Keyword: 'OR'; Token: kwOR), 
        (Keyword: 'ORDER'; Token: kwORDER), 
        (Keyword: 'OUT'; Token: kwOUT), 
        (Keyword: 'OUTER'; Token: kwOUTER), 
        (Keyword: 'OUTFILE'; Token: kwOUTFILE), 
        (Keyword: 'PARTITION'; Token: kwPARTITION), 
        (Keyword: 'PARTITIONS'; Token: kwPARTITIONS), 
        (Keyword: 'PASSWORD'; Token: kwPASSWORD), 
        (Keyword: 'PRECISION'; Token: kwPRECISION), 
        (Keyword: 'PRIMARY'; Token: kwPRIMARY), 
        (Keyword: 'PRIVILEGES'; Token: kwPRIVILEGES), 
        (Keyword: 'PROCEDURE'; Token: kwPROCEDURE), 
        (Keyword: 'PURGE'; Token: kwPURGE), 
        (Keyword: 'RANGE'; Token: kwRANGE), 
        (Keyword: 'READ'; Token: kwREAD), 
        (Keyword: 'REAL'; Token: kwREAL), 
        (Keyword: 'REFERENCES'; Token: kwREFERENCES), 
        (Keyword: 'REGEXP'; Token: kwREGEXP), 
        (Keyword: 'RENAME'; Token: kwRENAME), 
        (Keyword: 'REPEAT'; Token: kwREPEAT), 
        (Keyword: 'REPLACE'; Token: kwREPLACE), 
        (Keyword: 'REQUIRE'; Token: kwREQUIRE), 
        (Keyword: 'RESTRICT'; Token: kwRESTRICT), 
        (Keyword: 'RETURN'; Token: kwRETURN), 
        (Keyword: 'RETURNS'; Token: kwRETURNS), 
        (Keyword: 'REVOKE'; Token: kwREVOKE), 
        (Keyword: 'RIGHT'; Token: kwRIGHT), 
        (Keyword: 'RLIKE'; Token: kwRLIKE), 
        (Keyword: 'ROLLBACK'; Token: kwROLLBACK), 
        (Keyword: 'SCHEMA'; Token: kwSCHEMA), 
        (Keyword: 'SCHEMAS'; Token: kwSCHEMAS), 
        (Keyword: 'SECOND_MICROSECOND'; Token: kwSECOND_MICROSECOND), 
        (Keyword: 'SELECT'; Token: kwSELECT), 
        (Keyword: 'SENSITIVE'; Token: kwSENSITIVE), 
        (Keyword: 'SEPARATOR'; Token: kwSEPARATOR), 
        (Keyword: 'SET'; Token: kwSET), 
        (Keyword: 'SHOW'; Token: kwSHOW), 
        (Keyword: 'SMALLINT'; Token: kwSMALLINT), 
        (Keyword: 'SONAME'; Token: kwSONAME), 
        (Keyword: 'SPATIAL'; Token: kwSPATIAL), 
        (Keyword: 'SPECIFIC'; Token: kwSPECIFIC), 
        (Keyword: 'SQL'; Token: kwSQL), 
        (Keyword: 'SQLEXCEPTION'; Token: kwSQLEXCEPTION), 
        (Keyword: 'SQLSTATE'; Token: kwSQLSTATE), 
        (Keyword: 'SQLWARNING'; Token: kwSQLWARNING), 
        (Keyword: 'SQL_BIG_RESULT'; Token: kwSQL_BIG_RESULT), 
        (Keyword: 'SQL_CALC_FOUND_ROWS'; Token: kwSQL_CALC_FOUND_ROWS), 
        (Keyword: 'SQL_SMALL_RESULT'; Token: kwSQL_SMALL_RESULT), 
        (Keyword: 'SSL'; Token: kwSSL), 
        (Keyword: 'START'; Token: kwSTART), 
        (Keyword: 'STARTING'; Token: kwSTARTING), 
        (Keyword: 'STRAIGHT_JOIN'; Token: kwSTRAIGHT_JOIN), 
        (Keyword: 'TABLE'; Token: kwTABLE), 
        (Keyword: 'TABLES'; Token: kwTABLES), 
        (Keyword: 'TEMPORARY'; Token: kwTEMPORARY), 
        (Keyword: 'TERMINATED'; Token: kwTERMINATED), 
        (Keyword: 'THAN'; Token: kwTHAN), 
        (Keyword: 'THEN'; Token: kwTHEN), 
        (Keyword: 'TIMESTAMP'; Token: kwTIMESTAMP), 
        (Keyword: 'TINYBLOB'; Token: kwTINYBLOB), 
        (Keyword: 'TINYINT'; Token: kwTINYINT), 
        (Keyword: 'TINYTEXT'; Token: kwTINYTEXT), 
        (Keyword: 'TO'; Token: kwTO), 
        (Keyword: 'TRAILING'; Token: kwTRAILING), 
        (Keyword: 'TRANSACTION'; Token: kwTRANSACTION), 
        (Keyword: 'TRIGGER'; Token: kwTRIGGER), 
        (Keyword: 'TRUE'; Token: kwTRUE), 
        (Keyword: 'UNDEFINED'; Token: kwUNDEFINED), 
        (Keyword: 'UNDO'; Token: kwUNDO), 
        (Keyword: 'UNION'; Token: kwUNION), 
        (Keyword: 'UNIQUE'; Token: kwUNIQUE), 
        (Keyword: 'UNLOCK'; Token: kwUNLOCK), 
        (Keyword: 'UNSIGNED'; Token: kwUNSIGNED), 
        (Keyword: 'UPDATE'; Token: kwUPDATE), 
        (Keyword: 'USAGE'; Token: kwUSAGE), 
        (Keyword: 'USE'; Token: kwUSE), 
        (Keyword: 'USING'; Token: kwUSING), 
        (Keyword: 'UTC_DATE'; Token: kwUTC_DATE), 
        (Keyword: 'UTC_TIME'; Token: kwUTC_TIME), 
        (Keyword: 'UTC_TIMESTAMP'; Token: kwUTC_TIMESTAMP), 
        (Keyword: 'VALUE'; Token: kwVALUE), 
        (Keyword: 'VALUES'; Token: kwVALUES), 
        (Keyword: 'VARBINARY'; Token: kwVARBINARY), 
        (Keyword: 'VARCHAR'; Token: kwVARCHAR), 
        (Keyword: 'VARCHARACTER'; Token: kwVARCHARACTER), 
        (Keyword: 'VARYING'; Token: kwVARYING), 
        (Keyword: 'VIEW'; Token: kwVIEW), 
        (Keyword: 'WHEN'; Token: kwWHEN), 
        (Keyword: 'WHERE'; Token: kwWHERE), 
        (Keyword: 'WHILE'; Token: kwWHILE), 
        (Keyword: 'WITH'; Token: kwWITH), 
        (Keyword: 'WRITE'; Token: kwWRITE), 
        (Keyword: 'XOR'; Token: kwXOR), 
        (Keyword: 'YEAR_MONTH'; Token: kwYEAR_MONTH), 
        (Keyword: 'ZEROFILL'; Token: kwZEROFILL)
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
 
//----------------------------------------------------------------------------------------------------------------------
 
constructor TUCESQLHighlighter.Create(AOwner: TComponent);
 
begin
  inherited;
  // allocate positions array (length is highest possible mark position)
  SetLength(FPositions, MarksHigh[StateCount - 1] + 1);
 
  FCommentAttributes := THighlightAttributes.Create('comment');
  FCommentAttributes.Style := [fsItalic];
  FIdentifierAttributes := THighlightAttributes.Create('identifier');
  FKeyAttributes := THighlightAttributes.Create('reserved word');
  FKeyAttributes.Style := [fsBold];
  FNumberAttributes := THighlightAttributes.Create('number');
  FSpaceAttributes := THighlightAttributes.Create('space');
  FStringAttributes := THighlightAttributes.Create('String');
  FSymbolAttributes := THighlightAttributes.Create('symbol');
  FCommentWithCommandAttributes := THighlightAttributes.Create('comment with command');
  FEmbeddedCommandAttributes := THighlightAttributes.Create('embedded command');
  FSystemVariableAttributes := THighlightAttributes.Create('system variable');
  FUserVariableAttributes := THighlightAttributes.Create('user variable');
 
  FCommentAttributes.Onchange := HighlightChange;
  FIdentifierAttributes.Onchange := HighlightChange;
  FKeyAttributes.Onchange := HighlightChange;
  FNumberAttributes.Onchange := HighlightChange;
  FSpaceAttributes.Onchange := HighlightChange;
  FStringAttributes.Onchange := HighlightChange;
  FSymbolAttributes.Onchange := HighlightChange;
  FCommentWithCommandAttributes.Onchange := HighlightChange;
  FEmbeddedCommandAttributes.Onchange := HighlightChange;
  FSystemVariableAttributes.Onchange := HighlightChange;
  FUserVariableAttributes.Onchange := HighlightChange;
 
  DefaultFilter := 'SQL script files (*.sql)|*.sql';
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
destructor TUCESQLHighlighter.Destroy;
 
begin
  FPositions := nil;
  FMatchStack := nil;
  FCommentAttributes.Free;
  FIdentifierAttributes.Free;
  FKeyAttributes.Free;
  FNumberAttributes.Free;
  FSpaceAttributes.Free;
  FStringAttributes.Free;
  FSymbolAttributes.Free;
  FCommentWithCommandAttributes.Free;
  FEmbeddedCommandAttributes.Free;
  FSystemVariableAttributes.Free;
  FUserVariableAttributes.Free;
 
  inherited;
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
function TUCESQLHighlighter.IsKeyword: Boolean;
 
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
 
//----------------------------------------------------------------------------------------------------------------------
 
function TUCESQLHighlighter.EOL: Boolean;
 
begin
  Result := (FCurrentToken = -1) or FEOL;
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
function TUCESQLHighlighter.GetToken: string;
 
var
  Len: Cardinal;
 
begin
  Len := FCurrentPos - FTokenPos;
  SetString(Result, FInput + FTokenPos, Len);
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
procedure TUCESQLHighlighter.Reset;
 
begin
  FTokenPos := 0;
  FCurrentPos := 0;
  FCurrentToken := 0;
  FLineState := 1;
  FLastChar := #0;
  FMatchCount := 0;
  FEOL := False;
  ResetRange;
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
procedure TUCESQLHighlighter.SetToken(Token: Integer);
 
begin
  FCurrentToken := Token;
  FDoStop := True;
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
procedure TUCESQLHighlighter.RuleToToken(Rule: Integer);
 
begin
  case Rule of
    // <Normal>¤integer¤
      0: 
        SetToken(INTEGERNUMBER);
       
    // <Normal>¤realinteger¤
      1: 
        SetToken(FLOATNUMBER);
       
    // <Normal>¤white¤+
      2: 
        SetToken(WHITESPACE);
       
    // <Normal>¤identifier¤
      3: 
        if IsKeyword then
          SetToken(KEYWORD)
        else
          SetToken(IDENTIFIER);
       
    // <Normal>`¤identifier¤`
      4: 
        if IsKeyword then
          SetToken(KEYWORD)
        else
          SetToken(IDENTIFIER);
       
    // <Normal>@¤identifier¤
      5: 
        SetToken(USER_VARIABLE);
       
    // <Normal>@@¤identifier¤
      6: 
        SetToken(SYSTEM_VARIABLE);
       
    // <Normal>'
      7: 
        repeat
          case CurrentChar of
            '''',
            CR, #0 :
              begin
                if CurrentChar = '''' then
                  NextChar;
                SetToken(STRINGCONSTANT);
                Break;
              end;
            '\': // Escape character, skip this and the next one.
              NextChar;
          end;
          NextChar;
        until False;
       
    // <Normal>\"
      8: 
        repeat
          case CurrentChar of
            '"',
            CR, #0 :
              begin
                if CurrentChar = '"' then
                  NextChar;
                SetToken(STRINGCONSTANT);
                Break;
              end;
            '\': // Escape character, skip this and the next one.
              NextChar;
          end;
          NextChar;
        until False;
       
    // <Normal>"--"
      9: 
        if CurrentChar in [CR, #0, ' '] then
        begin
          SetToken(SLCOMMENT);
          if CurrentChar = ' ' then
            repeat
              case CurrentChar of
                CR, #0:
                  begin
                    SetToken(SLCOMMENT);
                    Break;
                  end;
              end;
              NextChar;
            until False;
        end
        else
          SetToken(SYMBOL);
       
    // <Normal>"#"
      10: 
        repeat
          case CurrentChar of
            CR, #0:
              begin
                SetToken(SLCOMMENT);
                Break;
              end;
            end;
          NextChar;
        until False;
       
    // <Normal>"/*"
      11: 
        repeat
          case CurrentChar of
            '*':
              if Lookahead = '/' then
              begin
                // skip lookahead and break loop
                NextChar;
                NextChar;
                SetToken(MLCOMMENT);
                Break;
              end;
            #0:
              begin
                State := MultilineComment;
                SetToken(MLCOMMENT);
                Break;
              end;
            '!': // Very special syntax for MySQL: command in comment.
              begin
                State := EmbeddedCommand;
                SetToken(COMMENT_WITH_COMMAND);
                Break;
              end;
          end;
          NextChar;
        until False;
       
    // <MultilineComment>[^\*]+
      12: 
        SetToken(MLCOMMENT);
       
    // <MultilineComment>\*
      13: 
        begin
          SetToken(MLCOMMENT);
          if CurrentChar = '/' then
          begin
            NextChar;
            State := Normal;
          end;
        end;
       
    // <EmbeddedCommand>[^\*]*
      14: 
        SetToken(EMBEDDED_COMMAND);
       
    // <EmbeddedCommand>\*
      15: 
        begin
          if CurrentChar = '/' then
          begin
            SetToken(COMMENT_WITH_COMMAND);
            NextChar;
            State := Normal;
          end
          else
            SetToken(EMBEDDED_COMMAND);
        end;
       
    // <Normal>.
      16: 
        SetToken(SYMBOL); // Any other char not catchd before.
       
    // ¤eof¤
      17: 
        SetToken(-1); // Special token to mark input end. Not really necessary since EOI is catched automatically.
  end;
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
function TUCESQLHighlighter.MatchRule(var Rule: Integer): Boolean;
 
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
 
//----------------------------------------------------------------------------------------------------------------------
 
function TUCESQLHighlighter.DoDefault: Boolean;
 
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
 
//----------------------------------------------------------------------------------------------------------------------
 
procedure TUCESQLHighlighter.Next;
 
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
 
//----------------------------------------------------------------------------------------------------------------------
 
procedure TUCESQLHighlighter.SetLine(const NewValue: string);
 
begin
  FInput := PChar(NewValue);
  Reset;
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
function TUCESQLHighlighter.GetNextChar: Char;
 
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
 
//----------------------------------------------------------------------------------------------------------------------
 
procedure TUCESQLHighlighter.SetState(Value: Cardinal);
 
begin
  FStartState := Value;
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
procedure TUCESQLHighlighter.SetMatch(I: Integer);
 
begin
  if FMatchCount >= Length(FMatchStack) then
    SetLength(FMatchStack, FMatchCount + 1);
  FMatchStack[FMatchCount] := I;
  Inc(FMatchCount);
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
procedure TUCESQLHighlighter.Reject;
 
begin
  FRejected := True;
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
function TUCESQLHighlighter.GetTokenInfo: TTokenData;
 
// Returns token string, token position relative to entire input string, and token length in one call.
 
begin
  FillChar(Result, SizeOf(Result), 0);
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
      SLCOMMENT,
      MLCOMMENT:
        begin
          Background := ColorToRGB(FCommentAttributes.Background);
          Foreground := ColorToRGB(FCommentAttributes.Foreground);
          Style := FCommentAttributes.Style;
        end;
      IDENTIFIER:
        begin
          Background := ColorToRGB(FIdentifierAttributes.Background);
          Foreground := ColorToRGB(FIdentifierAttributes.Foreground);
          Style := FIdentifierAttributes.Style;
        end;
      KEYWORD:
        begin
          Background := ColorToRGB(FKeyAttributes.Background);
          Foreground := ColorToRGB(FKeyAttributes.Foreground);
          Style := FKeyAttributes.Style;
        end;
      FLOATNUMBER,
      INTEGERNUMBER:
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
      SYMBOL:
        begin
          Background := ColorToRGB(FSymbolAttributes.Background);
          Foreground := ColorToRGB(FSymbolAttributes.Foreground);
          Style := FSymbolAttributes.Style;
        end;
      COMMENT_WITH_COMMAND:
        begin
          Background := ColorToRGB(FCommentWithCommandAttributes.Background);
          Foreground := ColorToRGB(FCommentWithCommandAttributes.Foreground);
          Style := FEmbeddedCommandAttributes.Style;
        end;
      EMBEDDED_COMMAND:
        begin
          Background := ColorToRGB(FEmbeddedCommandAttributes.Background);
          Foreground := ColorToRGB(FEmbeddedCommandAttributes.Foreground);
          Style := FEmbeddedCommandAttributes.Style;
        end;
      SYSTEM_VARIABLE:
        begin
          Background := ColorToRGB(FSystemVariableAttributes.Background);
          Foreground := ColorToRGB(FSystemVariableAttributes.Foreground);
          Style := FSystemVariableAttributes.Style;
        end;
      USER_VARIABLE:
        begin
          Background := ColorToRGB(FUserVariableAttributes.Background);
          Foreground := ColorToRGB(FUserVariableAttributes.Foreground);
          Style := FUserVariableAttributes.Style;
        end;
    end;
  end;
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
function TUCESQLHighlighter.GetCurrentChar: Char;
 
begin
  Result := FInput[FCurrentPos];
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
procedure TUCESQLHighlighter.HighlightChange(Sender: TObject);
 
begin
  WindowList.Invalidate;
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
function TUCESQLHighlighter.GetIdentChars: TIdentChars;
 
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
function TUCESQLHighlighter.GetAttributeCount: Integer;
 
begin
  Result := 7;
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
function TUCESQLHighlighter.GetAttribute(Index: Integer): THighlightAttributes;
 
begin
  case Index of
    0:
      Result := FCommentAttributes;
    1:
      Result := FIdentifierAttributes;
    2:
      Result := FNumberAttributes;
    3:
      Result := FKeyAttributes;
    4:
      Result := FSpaceAttributes;
    5:
      Result := FStringAttributes;
    6:
      Result := FSymbolAttributes;
    7:
      Result := FEmbeddedCommandAttributes;
    8:
      Result := FCommentWithCommandAttributes;
    9:
      Result := FSystemVariableAttributes;
    10:
      Result := FUserVariableAttributes;
  else
    Result := nil;
  end;
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
function TUCESQLHighlighter.GetLanguageName: string;
 
begin
  Result := 'SQL';
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
procedure TUCESQLHighlighter.SetAttribute(const Index: Integer; const Value: THighlightAttributes);
 
begin
  case Index of
    0:
      FCommentAttributes.Assign(Value);
    1:
      FIdentifierAttributes.Assign(Value);
    2:
      FNumberAttributes.Assign(Value);
    3:
      FKeyAttributes.Assign(Value);
    4:
      FSpaceAttributes.Assign(Value);
    5:
      FStringAttributes.Assign(Value);
    6:
      FSymbolAttributes.Assign(Value);
    7:
      FEmbeddedCommandAttributes.Assign(Value);
    8:
      FCommentWithCommandAttributes.Assign(Value);
    9:
      FSystemVariableAttributes.Assign(Value);
    10:
      FUserVariableAttributes.Assign(Value);
  end;
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
function TUCESQLHighlighter.GetRange: Integer;
 
begin
  Result := FStartState;
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
function TUCESQLHighlighter.HasMoreTokens: Boolean;
 
begin
  Result := FCurrentToken > -1;
end;
 
//--------------------------------------------------------------------------------
 
procedure TUCESQLHighlighter.SetRange(Value: Integer);
 
begin
  FStartState := Value;
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
procedure TUCESQLHighlighter.ResetRange;
 
begin
  FStartState := Normal;
  FCurrentState := Normal;
end;
 
//----------------------------------------------------------------------------------------------------------------------
 
end.
