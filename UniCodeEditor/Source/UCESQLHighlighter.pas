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
      kwACTION = 257;
      kwADD = 258;
      kwADDDATE = 259;
      kwAFTER = 260;
      kwAGAINST = 261;
      kwAGGREGATE = 262;
      kwALGORITHM = 263;
      kwALL = 264;
      kwALTER = 265;
      kwANALYZE = 266;
      kwAND = 267;
      kwANY = 268;
      kwAS = 269;
      kwASC = 270;
      kwASCII = 271;
      kwASENSITIVE = 272;
      kwATAN = 273;
      kwATAN2 = 274;
      kwAUTO_INCREMENT = 275;
      kwAVG = 276;
      kwAVG_ROW_LENGTH = 277;
      kwBACKUP = 278;
      kwBDB = 279;
      kwBEFORE = 280;
      kwBEGIN = 281;
      kwBENCHMARK = 282;
      kwBERKELEYDB = 283;
      kwBETWEEN = 284;
      kwBIGINT = 285;
      kwBINARY = 286;
      kwBINLOG = 287;
      kwBIT = 288;
      kwBIT_AND = 289;
      kwBIT_OR = 290;
      kwBIT_XOR = 291;
      kwBLOB = 292;
      kwBOOL = 293;
      kwBOOLEAN = 294;
      kwBOTH = 295;
      kwBTREE = 296;
      kwBY = 297;
      kwBYTE = 298;
      kwCACHE = 299;
      kwCALL = 300;
      kwCASCADE = 301;
      kwCASCADED = 302;
      kwCAST = 303;
      kwCHAIN = 304;
      kwCHANGE = 305;
      kwCHANGED = 306;
      kwCHAR = 307;
      kwCHARACTER = 308;
      kwCHARSET = 309;
      kwCHECK = 310;
      kwCHECKSUM = 311;
      kwCIPHER = 312;
      kwCLIENT = 313;
      kwCLOSE = 314;
      kwCOALESCE = 315;
      kwCOLLATE = 316;
      kwCOLLATION = 317;
      kwCOLUMN = 318;
      kwCOLUMNS = 319;
      kwCOMMENT = 320;
      kwCOMMIT = 321;
      kwCOMMITTED = 322;
      kwCOMPACT = 323;
      kwCOMPRESSED = 324;
      kwCONCAT = 325;
      kwCONCAT_WS = 326;
      kwCONCURRENT = 327;
      kwCONDITION = 328;
      kwCONNECTION = 329;
      kwCONSTRAINT = 330;
      kwCONTAINS = 331;
      kwCONTINUE = 332;
      kwCONVERT = 333;
      kwCONVERT_TZ = 334;
      kwCOUNT = 335;
      kwCREATE = 336;
      kwCROSS = 337;
      kwCUBE = 338;
      kwCURDATE = 339;
      kwCURRENT_DATE = 340;
      kwCURRENT_TIME = 341;
      kwCURRENT_TIMESTAMP = 342;
      kwCURRENT_USER = 343;
      kwCURSOR = 344;
      kwCURTIME = 345;
      kwDATA = 346;
      kwDATABASE = 347;
      kwDATABASES = 348;
      kwDATE = 349;
      kwDATETIME = 350;
      kwDATE_ADD = 351;
      kwDATE_SUB = 352;
      kwDAY = 353;
      kwDAY_HOUR = 354;
      kwDAY_MICROSECOND = 355;
      kwDAY_MINUTE = 356;
      kwDAY_SECOND = 357;
      kwDEALLOCATE = 358;
      kwDEC = 359;
      kwDECIMAL = 360;
      kwDECLARE = 361;
      kwDECODE = 362;
      kwDEFAULT = 363;
      kwDEFINER = 364;
      kwDELAYED = 365;
      kwDELAY_KEY_WRITE = 366;
      kwDELETE = 367;
      kwDELIMITER = 368;
      kwDESC = 369;
      kwDESCRIBE = 370;
      kwDES_DECRYPT = 371;
      kwDES_ENCRYPT = 372;
      kwDES_KEY_FILE = 373;
      kwDETERMINISTIC = 374;
      kwDIRECTORY = 375;
      kwDISABLE = 376;
      kwDISCARD = 377;
      kwDISTINCT = 378;
      kwDISTINCTROW = 379;
      kwDIV = 380;
      kwDO = 381;
      kwDOUBLE = 382;
      kwDROP = 383;
      kwDUAL = 384;
      kwDUMPFILE = 385;
      kwDUPLICATE = 386;
      kwDYNAMIC = 387;
      kwEACH = 388;
      kwELSE = 389;
      kwELSEIF = 390;
      kwELT = 391;
      kwENABLE = 392;
      kwENCLOSED = 393;
      kwENCODE = 394;
      kwENCRYPT = 395;
      kwEND = 396;
      kwENGINE = 397;
      kwENGINES = 398;
      kwENUM = 399;
      kwEQUAL = 400;
      kwERRORS = 401;
      kwESCAPE = 402;
      kwESCAPED = 403;
      kwEVENTS = 404;
      kwEXECUTE = 405;
      kwEXISTS = 406;
      kwEXIT = 407;
      kwEXPANSION = 408;
      kwEXPLAIN = 409;
      kwEXPORT_SET = 410;
      kwEXTENDED = 411;
      kwEXTRACT = 412;
      kwFALSE = 413;
      kwFAST = 414;
      kwFETCH = 415;
      kwFIELD = 416;
      kwFIELDS = 417;
      kwFILE = 418;
      kwFIRST = 419;
      kwFIXED = 420;
      kwFLOAT = 421;
      kwFLOAT4 = 422;
      kwFLOAT8 = 423;
      kwFLUSH = 424;
      kwFOR = 425;
      kwFORCE = 426;
      kwFOREIGN = 427;
      kwFORMAT = 428;
      kwFOUND = 429;
      kwFRAC_SECOND = 430;
      kwFROM = 431;
      kwFROM_UNIXTIME = 432;
      kwFULL = 433;
      kwFULLTEXT = 434;
      kwFUNCTION = 435;
      kwGEOMCOLLFROMTEXT = 436;
      kwGEOMCOLLFROMWKB = 437;
      kwGEOMETRY = 438;
      kwGEOMETRYCOLLECTION = 439;
      kwGEOMETRYCOLLECTIONFROMTEXT = 440;
      kwGEOMETRYCOLLECTIONFROMWKB = 441;
      kwGEOMETRYFROMTEXT = 442;
      kwGEOMETRYFROMWKB = 443;
      kwGEOMFROMTEXT = 444;
      kwGEOMFROMWKB = 445;
      kwGET_FORMAT = 446;
      kwGLOBAL = 447;
      kwGOTO = 448;
      kwGRANT = 449;
      kwGRANTS = 450;
      kwGREATER = 451;
      kwGREATEST = 452;
      kwGROUP = 453;
      kwGROUP_CONCAT = 454;
      kwGROUP_UNIQUE_USERS = 455;
      kwHANDLER = 456;
      kwHASH = 457;
      kwHAVING = 458;
      kwHELP = 459;
      kwHIGH_PRIORITY = 460;
      kwHOSTS = 461;
      kwHOUR = 462;
      kwHOUR_MICROSECOND = 463;
      kwHOUR_MINUTE = 464;
      kwHOUR_SECOND = 465;
      kwIDENTIFIED = 466;
      kwIF = 467;
      kwIGNORE = 468;
      kwIMPORT = 469;
      kwIN = 470;
      kwINDEX = 471;
      kwINDEXES = 472;
      kwINFILE = 473;
      kwINNER = 474;
      kwINNOBASE = 475;
      kwINNODB = 476;
      kwINOUT = 477;
      kwINSENSITIVE = 478;
      kwINSERT = 479;
      kwINSERT_METHOD = 480;
      kwINT = 481;
      kwINT1 = 482;
      kwINT2 = 483;
      kwINT3 = 484;
      kwINT4 = 485;
      kwINT8 = 486;
      kwINTEGER = 487;
      kwINTERVAL = 488;
      kwINTO = 489;
      kwINVOKER = 490;
      kwIO_THREAD = 491;
      kwIS = 492;
      kwISOLATION = 493;
      kwISSUER = 494;
      kwITERATE = 495;
      kwJOIN = 496;
      kwKEY = 497;
      kwKEYS = 498;
      kwKILL = 499;
      kwLABEL = 500;
      kwLANGUAGE = 501;
      kwLAST = 502;
      kwLAST_INSERT_ID = 503;
      kwLEADING = 504;
      kwLEAST = 505;
      kwLEAVE = 506;
      kwLEAVES = 507;
      kwLEFT = 508;
      kwLESS = 509;
      kwLEVEL = 510;
      kwLIKE = 511;
      kwLIMIT = 512;
      kwLINEFROMTEXT = 513;
      kwLINEFROMWKB = 514;
      kwLINES = 515;
      kwLINESTRING = 516;
      kwLINESTRINGFROMTEXT = 517;
      kwLINESTRINGFROMWKB = 518;
      kwLIST = 519;
      kwLOAD = 520;
      kwLOCAL = 521;
      kwLOCALTIME = 522;
      kwLOCALTIMESTAMP = 523;
      kwLOCATE = 524;
      kwLOCK = 525;
      kwLOCKS = 526;
      kwLOG = 527;
      kwLOGS = 528;
      kwLONG = 529;
      kwLONGBLOB = 530;
      kwLONGTEXT = 531;
      kwLOOP = 532;
      kwLOW_PRIORITY = 533;
      kwMAKE_SET = 534;
      kwMASTER = 535;
      kwMASTER_CONNECT_RETRY = 536;
      kwMASTER_HOST = 537;
      kwMASTER_LOG_FILE = 538;
      kwMASTER_LOG_POS = 539;
      kwMASTER_PASSWORD = 540;
      kwMASTER_PORT = 541;
      kwMASTER_POS_WAIT = 542;
      kwMASTER_SERVER_ID = 543;
      kwMASTER_SSL = 544;
      kwMASTER_SSL_CA = 545;
      kwMASTER_SSL_CAPATH = 546;
      kwMASTER_SSL_CERT = 547;
      kwMASTER_SSL_CIPHER = 548;
      kwMASTER_SSL_KEY = 549;
      kwMASTER_USER = 550;
      kwMATCH = 551;
      kwMAX = 552;
      kwMAX_CONNECTIONS_PER_HOUR = 553;
      kwMAX_QUERIES_PER_HOUR = 554;
      kwMAX_ROWS = 555;
      kwMAX_UPDATES_PER_HOUR = 556;
      kwMAX_USER_CONNECTIONS = 557;
      kwMEDIUM = 558;
      kwMEDIUMBLOB = 559;
      kwMEDIUMINT = 560;
      kwMEDIUMTEXT = 561;
      kwMERGE = 562;
      kwMICROSECOND = 563;
      kwMID = 564;
      kwMIDDLEINT = 565;
      kwMIGRATE = 566;
      kwMIN = 567;
      kwMINUTE = 568;
      kwMINUTE_MICROSECOND = 569;
      kwMINUTE_SECOND = 570;
      kwMIN_ROWS = 571;
      kwMLINEFROMTEXT = 572;
      kwMLINEFROMWKB = 573;
      kwMOD = 574;
      kwMODE = 575;
      kwMODIFIES = 576;
      kwMODIFY = 577;
      kwMONTH = 578;
      kwMPOINTFROMTEXT = 579;
      kwMPOINTFROMWKB = 580;
      kwMPOLYFROMTEXT = 581;
      kwMPOLYFROMWKB = 582;
      kwMULTILINESTRING = 583;
      kwMULTILINESTRINGFROMTEXT = 584;
      kwMULTILINESTRINGFROMWKB = 585;
      kwMULTIPOINT = 586;
      kwMULTIPOINTFROMTEXT = 587;
      kwMULTIPOINTFROMWKB = 588;
      kwMULTIPOLYGON = 589;
      kwMULTIPOLYGONFROMTEXT = 590;
      kwMULTIPOLYGONFROMWKB = 591;
      kwMUTEX = 592;
      kwNAME = 593;
      kwNAMES = 594;
      kwNATIONAL = 595;
      kwNATURAL = 596;
      kwNCHAR = 597;
      kwNDB = 598;
      kwNDBCLUSTER = 599;
      kwNEW = 600;
      kwNEXT = 601;
      kwNO = 602;
      kwNONE = 603;
      kwNOT = 604;
      kwNOW = 605;
      kwNO_WRITE_TO_BINLOG = 606;
      kwNULL = 607;
      kwNUMERIC = 608;
      kwNVARCHAR = 609;
      kwOFFSET = 610;
      kwOLD_PASSWORD = 611;
      kwON = 612;
      kwONE = 613;
      kwONE_SHOT = 614;
      kwOPEN = 615;
      kwOPTIMIZE = 616;
      kwOPTION = 617;
      kwOPTIONALLY = 618;
      kwOR = 619;
      kwORDER = 620;
      kwOUT = 621;
      kwOUTER = 622;
      kwOUTFILE = 623;
      kwPACK_KEYS = 624;
      kwPARTIAL = 625;
      kwPARTITION = 626;
      kwPARTITIONS = 627;
      kwPASSWORD = 628;
      kwPHASE = 629;
      kwPOINT = 630;
      kwPOINTFROMTEXT = 631;
      kwPOINTFROMWKB = 632;
      kwPOLYFROMTEXT = 633;
      kwPOLYFROMWKB = 634;
      kwPOLYGON = 635;
      kwPOLYGONFROMTEXT = 636;
      kwPOLYGONFROMWKB = 637;
      kwPOSITION = 638;
      kwPRECISION = 639;
      kwPREPARE = 640;
      kwPREV = 641;
      kwPRIMARY = 642;
      kwPRIVILEGES = 643;
      kwPROCEDURE = 644;
      kwPROCESS = 645;
      kwPROCESSLIST = 646;
      kwPURGE = 647;
      kwQUARTER = 648;
      kwQUERY = 649;
      kwQUICK = 650;
      kwRAID0 = 651;
      kwRAID_CHUNKS = 652;
      kwRAID_CHUNKSIZE = 653;
      kwRAID_TYPE = 654;
      kwRAND = 655;
      kwRANGE = 656;
      kwREAD = 657;
      kwREADS = 658;
      kwREAL = 659;
      kwRECOVER = 660;
      kwREDUNDANT = 661;
      kwREFERENCES = 662;
      kwREGEXP = 663;
      kwRELAY_LOG_FILE = 664;
      kwRELAY_LOG_POS = 665;
      kwRELAY_THREAD = 666;
      kwRELEASE = 667;
      kwRELOAD = 668;
      kwRENAME = 669;
      kwREPAIR = 670;
      kwREPEAT = 671;
      kwREPEATABLE = 672;
      kwREPLACE = 673;
      kwREPLICATION = 674;
      kwREQUIRE = 675;
      kwRESET = 676;
      kwRESTORE = 677;
      kwRESTRICT = 678;
      kwRESUME = 679;
      kwRETURN = 680;
      kwRETURNS = 681;
      kwREVOKE = 682;
      kwRIGHT = 683;
      kwRLIKE = 684;
      kwROLLBACK = 685;
      kwROLLUP = 686;
      kwROUND = 687;
      kwROUTINE = 688;
      kwROW = 689;
      kwROWS = 690;
      kwROW_COUNT = 691;
      kwROW_FORMAT = 692;
      kwRTREE = 693;
      kwSAVEPOINT = 694;
      kwSCHEMA = 695;
      kwSCHEMAS = 696;
      kwSECOND = 697;
      kwSECOND_MICROSECOND = 698;
      kwSECURITY = 699;
      kwSELECT = 700;
      kwSENSITIVE = 701;
      kwSEPARATOR = 702;
      kwSERIAL = 703;
      kwSERIALIZABLE = 704;
      kwSESSION = 705;
      kwSESSION_USER = 706;
      kwSET = 707;
      kwSHARE = 708;
      kwSHOW = 709;
      kwSHUTDOWN = 710;
      kwSIGNED = 711;
      kwSIMPLE = 712;
      kwSLAVE = 713;
      kwSMALLINT = 714;
      kwSNAPSHOT = 715;
      kwSOME = 716;
      kwSONAME = 717;
      kwSOUNDS = 718;
      kwSPATIAL = 719;
      kwSPECIFIC = 720;
      kwSQL = 721;
      kwSQLEXCEPTION = 722;
      kwSQLSTATE = 723;
      kwSQLWARNING = 724;
      kwSQL_BIG_RESULT = 725;
      kwSQL_BUFFER_RESULT = 726;
      kwSQL_CACHE = 727;
      kwSQL_CALC_FOUND_ROWS = 728;
      kwSQL_NO_CACHE = 729;
      kwSQL_SMALL_RESULT = 730;
      kwSQL_THREAD = 731;
      kwSQL_TSI_DAY = 732;
      kwSQL_TSI_FRAC_SECOND = 733;
      kwSQL_TSI_HOUR = 734;
      kwSQL_TSI_MINUTE = 735;
      kwSQL_TSI_MONTH = 736;
      kwSQL_TSI_QUARTER = 737;
      kwSQL_TSI_SECOND = 738;
      kwSQL_TSI_WEEK = 739;
      kwSQL_TSI_YEAR = 740;
      kwSSL = 741;
      kwSTART = 742;
      kwSTARTING = 743;
      kwSTATUS = 744;
      kwSTD = 745;
      kwSTDDEV = 746;
      kwSTDDEV_POP = 747;
      kwSTDDEV_SAMP = 748;
      kwSTOP = 749;
      kwSTORAGE = 750;
      kwSTRAIGHT_JOIN = 751;
      kwSTRING = 752;
      kwSTRIPED = 753;
      kwSUBDATE = 754;
      kwSUBJECT = 755;
      kwSUBSTR = 756;
      kwSUBSTRING = 757;
      kwSUBSTRING_INDEX = 758;
      kwSUM = 759;
      kwSUPER = 760;
      kwSUSPEND = 761;
      kwSYSDATE = 762;
      kwSYSTEM_USER = 763;
      kwTABLE = 764;
      kwTABLES = 765;
      kwTABLESPACE = 766;
      kwTEMPORARY = 767;
      kwTEMPTABLE = 768;
      kwTERMINATED = 769;
      kwTEXT = 770;
      kwTHAN = 771;
      kwTHEN = 772;
      kwTIME = 773;
      kwTIMESTAMP = 774;
      kwTIMESTAMPADD = 775;
      kwTIMESTAMPDIFF = 776;
      kwTINYBLOB = 777;
      kwTINYINT = 778;
      kwTINYTEXT = 779;
      kwTO = 780;
      kwTRAILING = 781;
      kwTRANSACTION = 782;
      kwTRIGGER = 783;
      kwTRIGGERS = 784;
      kwTRIM = 785;
      kwTRUE = 786;
      kwTRUNCATE = 787;
      kwTYPE = 788;
      kwTYPES = 789;
      kwUNCOMMITTED = 790;
      kwUNDEFINED = 791;
      kwUNDO = 792;
      kwUNICODE = 793;
      kwUNION = 794;
      kwUNIQUE = 795;
      kwUNIQUE_USERS = 796;
      kwUNIX_TIMESTAMP = 797;
      kwUNKNOWN = 798;
      kwUNLOCK = 799;
      kwUNSIGNED = 800;
      kwUNTIL = 801;
      kwUPDATE = 802;
      kwUSAGE = 803;
      kwUSE = 804;
      kwUSER = 805;
      kwUSER_RESOURCES = 806;
      kwUSE_FRM = 807;
      kwUSING = 808;
      kwUTC_DATE = 809;
      kwUTC_TIME = 810;
      kwUTC_TIMESTAMP = 811;
      kwVALUE = 812;
      kwVALUES = 813;
      kwVARBINARY = 814;
      kwVARCHAR = 815;
      kwVARCHARACTER = 816;
      kwVARIABLES = 817;
      kwVARIANCE = 818;
      kwVARYING = 819;
      kwVAR_POP = 820;
      kwVAR_SAMP = 821;
      kwVIEW = 822;
      kwWARNINGS = 823;
      kwWEEK = 824;
      kwWHEN = 825;
      kwWHERE = 826;
      kwWHILE = 827;
      kwWITH = 828;
      kwWORK = 829;
      kwWRITE = 830;
      kwX509 = 831;
      kwXA = 832;
      kwXOR = 833;
      kwYEAR = 834;
      kwYEARWEEK = 835;
      kwYEAR_MONTH = 836;
      kwZEROFILL = 837;
      kwLastKeyword = 837;

const COMMENT_WITH_COMMAND = 838;
      EMBEDDED_COMMAND = 839;
      FLOATNUMBER = 840;
      IDENTIFIER = 841;
      INTEGERNUMBER = 842;
      KEYWORD = 843;
      MLCOMMENT = 844;
      SLCOMMENT = 845;
      STRINGCONSTANT = 846;
      SYMBOL = 847;
      SYSTEM_VARIABLE = 848;
      UNKNOWN = 849;
      USER_VARIABLE = 850;
      WHITESPACE = 851;


 
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

const KeywordCount = 581;
      Keywords : array[0..KeywordCount - 1] of TKeyword = (
        (Keyword: 'ACTION'; Token: kwACTION), 
        (Keyword: 'ADD'; Token: kwADD), 
        (Keyword: 'ADDDATE'; Token: kwADDDATE), 
        (Keyword: 'AFTER'; Token: kwAFTER), 
        (Keyword: 'AGAINST'; Token: kwAGAINST), 
        (Keyword: 'AGGREGATE'; Token: kwAGGREGATE), 
        (Keyword: 'ALGORITHM'; Token: kwALGORITHM), 
        (Keyword: 'ALL'; Token: kwALL), 
        (Keyword: 'ALTER'; Token: kwALTER), 
        (Keyword: 'ANALYZE'; Token: kwANALYZE), 
        (Keyword: 'AND'; Token: kwAND), 
        (Keyword: 'ANY'; Token: kwANY), 
        (Keyword: 'AS'; Token: kwAS), 
        (Keyword: 'ASC'; Token: kwASC), 
        (Keyword: 'ASCII'; Token: kwASCII), 
        (Keyword: 'ASENSITIVE'; Token: kwASENSITIVE), 
        (Keyword: 'ATAN'; Token: kwATAN), 
        (Keyword: 'ATAN2'; Token: kwATAN2), 
        (Keyword: 'AUTO_INCREMENT'; Token: kwAUTO_INCREMENT), 
        (Keyword: 'AVG'; Token: kwAVG), 
        (Keyword: 'AVG_ROW_LENGTH'; Token: kwAVG_ROW_LENGTH), 
        (Keyword: 'BACKUP'; Token: kwBACKUP), 
        (Keyword: 'BDB'; Token: kwBDB), 
        (Keyword: 'BEFORE'; Token: kwBEFORE), 
        (Keyword: 'BEGIN'; Token: kwBEGIN), 
        (Keyword: 'BENCHMARK'; Token: kwBENCHMARK), 
        (Keyword: 'BERKELEYDB'; Token: kwBERKELEYDB), 
        (Keyword: 'BETWEEN'; Token: kwBETWEEN), 
        (Keyword: 'BIGINT'; Token: kwBIGINT), 
        (Keyword: 'BINARY'; Token: kwBINARY), 
        (Keyword: 'BINLOG'; Token: kwBINLOG), 
        (Keyword: 'BIT'; Token: kwBIT), 
        (Keyword: 'BIT_AND'; Token: kwBIT_AND), 
        (Keyword: 'BIT_OR'; Token: kwBIT_OR), 
        (Keyword: 'BIT_XOR'; Token: kwBIT_XOR), 
        (Keyword: 'BLOB'; Token: kwBLOB), 
        (Keyword: 'BOOL'; Token: kwBOOL), 
        (Keyword: 'BOOLEAN'; Token: kwBOOLEAN), 
        (Keyword: 'BOTH'; Token: kwBOTH), 
        (Keyword: 'BTREE'; Token: kwBTREE), 
        (Keyword: 'BY'; Token: kwBY), 
        (Keyword: 'BYTE'; Token: kwBYTE), 
        (Keyword: 'CACHE'; Token: kwCACHE), 
        (Keyword: 'CALL'; Token: kwCALL), 
        (Keyword: 'CASCADE'; Token: kwCASCADE), 
        (Keyword: 'CASCADED'; Token: kwCASCADED), 
        (Keyword: 'CAST'; Token: kwCAST), 
        (Keyword: 'CHAIN'; Token: kwCHAIN), 
        (Keyword: 'CHANGE'; Token: kwCHANGE), 
        (Keyword: 'CHANGED'; Token: kwCHANGED), 
        (Keyword: 'CHAR'; Token: kwCHAR), 
        (Keyword: 'CHARACTER'; Token: kwCHARACTER), 
        (Keyword: 'CHARSET'; Token: kwCHARSET), 
        (Keyword: 'CHECK'; Token: kwCHECK), 
        (Keyword: 'CHECKSUM'; Token: kwCHECKSUM), 
        (Keyword: 'CIPHER'; Token: kwCIPHER), 
        (Keyword: 'CLIENT'; Token: kwCLIENT), 
        (Keyword: 'CLOSE'; Token: kwCLOSE), 
        (Keyword: 'COALESCE'; Token: kwCOALESCE), 
        (Keyword: 'COLLATE'; Token: kwCOLLATE), 
        (Keyword: 'COLLATION'; Token: kwCOLLATION), 
        (Keyword: 'COLUMN'; Token: kwCOLUMN), 
        (Keyword: 'COLUMNS'; Token: kwCOLUMNS), 
        (Keyword: 'COMMENT'; Token: kwCOMMENT), 
        (Keyword: 'COMMIT'; Token: kwCOMMIT), 
        (Keyword: 'COMMITTED'; Token: kwCOMMITTED), 
        (Keyword: 'COMPACT'; Token: kwCOMPACT), 
        (Keyword: 'COMPRESSED'; Token: kwCOMPRESSED), 
        (Keyword: 'CONCAT'; Token: kwCONCAT), 
        (Keyword: 'CONCAT_WS'; Token: kwCONCAT_WS), 
        (Keyword: 'CONCURRENT'; Token: kwCONCURRENT), 
        (Keyword: 'CONDITION'; Token: kwCONDITION), 
        (Keyword: 'CONNECTION'; Token: kwCONNECTION), 
        (Keyword: 'CONSTRAINT'; Token: kwCONSTRAINT), 
        (Keyword: 'CONTAINS'; Token: kwCONTAINS), 
        (Keyword: 'CONTINUE'; Token: kwCONTINUE), 
        (Keyword: 'CONVERT'; Token: kwCONVERT), 
        (Keyword: 'CONVERT_TZ'; Token: kwCONVERT_TZ), 
        (Keyword: 'COUNT'; Token: kwCOUNT), 
        (Keyword: 'CREATE'; Token: kwCREATE), 
        (Keyword: 'CROSS'; Token: kwCROSS), 
        (Keyword: 'CUBE'; Token: kwCUBE), 
        (Keyword: 'CURDATE'; Token: kwCURDATE), 
        (Keyword: 'CURRENT_DATE'; Token: kwCURRENT_DATE), 
        (Keyword: 'CURRENT_TIME'; Token: kwCURRENT_TIME), 
        (Keyword: 'CURRENT_TIMESTAMP'; Token: kwCURRENT_TIMESTAMP), 
        (Keyword: 'CURRENT_USER'; Token: kwCURRENT_USER), 
        (Keyword: 'CURSOR'; Token: kwCURSOR), 
        (Keyword: 'CURTIME'; Token: kwCURTIME), 
        (Keyword: 'DATA'; Token: kwDATA), 
        (Keyword: 'DATABASE'; Token: kwDATABASE), 
        (Keyword: 'DATABASES'; Token: kwDATABASES), 
        (Keyword: 'DATE'; Token: kwDATE), 
        (Keyword: 'DATETIME'; Token: kwDATETIME), 
        (Keyword: 'DATE_ADD'; Token: kwDATE_ADD), 
        (Keyword: 'DATE_SUB'; Token: kwDATE_SUB), 
        (Keyword: 'DAY'; Token: kwDAY), 
        (Keyword: 'DAY_HOUR'; Token: kwDAY_HOUR), 
        (Keyword: 'DAY_MICROSECOND'; Token: kwDAY_MICROSECOND), 
        (Keyword: 'DAY_MINUTE'; Token: kwDAY_MINUTE), 
        (Keyword: 'DAY_SECOND'; Token: kwDAY_SECOND), 
        (Keyword: 'DEALLOCATE'; Token: kwDEALLOCATE), 
        (Keyword: 'DEC'; Token: kwDEC), 
        (Keyword: 'DECIMAL'; Token: kwDECIMAL), 
        (Keyword: 'DECLARE'; Token: kwDECLARE), 
        (Keyword: 'DECODE'; Token: kwDECODE), 
        (Keyword: 'DEFAULT'; Token: kwDEFAULT), 
        (Keyword: 'DEFINER'; Token: kwDEFINER), 
        (Keyword: 'DELAYED'; Token: kwDELAYED), 
        (Keyword: 'DELAY_KEY_WRITE'; Token: kwDELAY_KEY_WRITE), 
        (Keyword: 'DELETE'; Token: kwDELETE), 
        (Keyword: 'DELIMITER'; Token: kwDELIMITER), 
        (Keyword: 'DESC'; Token: kwDESC), 
        (Keyword: 'DESCRIBE'; Token: kwDESCRIBE), 
        (Keyword: 'DES_DECRYPT'; Token: kwDES_DECRYPT), 
        (Keyword: 'DES_ENCRYPT'; Token: kwDES_ENCRYPT), 
        (Keyword: 'DES_KEY_FILE'; Token: kwDES_KEY_FILE), 
        (Keyword: 'DETERMINISTIC'; Token: kwDETERMINISTIC), 
        (Keyword: 'DIRECTORY'; Token: kwDIRECTORY), 
        (Keyword: 'DISABLE'; Token: kwDISABLE), 
        (Keyword: 'DISCARD'; Token: kwDISCARD), 
        (Keyword: 'DISTINCT'; Token: kwDISTINCT), 
        (Keyword: 'DISTINCTROW'; Token: kwDISTINCTROW), 
        (Keyword: 'DIV'; Token: kwDIV), 
        (Keyword: 'DO'; Token: kwDO), 
        (Keyword: 'DOUBLE'; Token: kwDOUBLE), 
        (Keyword: 'DROP'; Token: kwDROP), 
        (Keyword: 'DUAL'; Token: kwDUAL), 
        (Keyword: 'DUMPFILE'; Token: kwDUMPFILE), 
        (Keyword: 'DUPLICATE'; Token: kwDUPLICATE), 
        (Keyword: 'DYNAMIC'; Token: kwDYNAMIC), 
        (Keyword: 'EACH'; Token: kwEACH), 
        (Keyword: 'ELSE'; Token: kwELSE), 
        (Keyword: 'ELSEIF'; Token: kwELSEIF), 
        (Keyword: 'ELT'; Token: kwELT), 
        (Keyword: 'ENABLE'; Token: kwENABLE), 
        (Keyword: 'ENCLOSED'; Token: kwENCLOSED), 
        (Keyword: 'ENCODE'; Token: kwENCODE), 
        (Keyword: 'ENCRYPT'; Token: kwENCRYPT), 
        (Keyword: 'END'; Token: kwEND), 
        (Keyword: 'ENGINE'; Token: kwENGINE), 
        (Keyword: 'ENGINES'; Token: kwENGINES), 
        (Keyword: 'ENUM'; Token: kwENUM), 
        (Keyword: 'EQUAL'; Token: kwEQUAL), 
        (Keyword: 'ERRORS'; Token: kwERRORS), 
        (Keyword: 'ESCAPE'; Token: kwESCAPE), 
        (Keyword: 'ESCAPED'; Token: kwESCAPED), 
        (Keyword: 'EVENTS'; Token: kwEVENTS), 
        (Keyword: 'EXECUTE'; Token: kwEXECUTE), 
        (Keyword: 'EXISTS'; Token: kwEXISTS), 
        (Keyword: 'EXIT'; Token: kwEXIT), 
        (Keyword: 'EXPANSION'; Token: kwEXPANSION), 
        (Keyword: 'EXPLAIN'; Token: kwEXPLAIN), 
        (Keyword: 'EXPORT_SET'; Token: kwEXPORT_SET), 
        (Keyword: 'EXTENDED'; Token: kwEXTENDED), 
        (Keyword: 'EXTRACT'; Token: kwEXTRACT), 
        (Keyword: 'FALSE'; Token: kwFALSE), 
        (Keyword: 'FAST'; Token: kwFAST), 
        (Keyword: 'FETCH'; Token: kwFETCH), 
        (Keyword: 'FIELD'; Token: kwFIELD), 
        (Keyword: 'FIELDS'; Token: kwFIELDS), 
        (Keyword: 'FILE'; Token: kwFILE), 
        (Keyword: 'FIRST'; Token: kwFIRST), 
        (Keyword: 'FIXED'; Token: kwFIXED), 
        (Keyword: 'FLOAT'; Token: kwFLOAT), 
        (Keyword: 'FLOAT4'; Token: kwFLOAT4), 
        (Keyword: 'FLOAT8'; Token: kwFLOAT8), 
        (Keyword: 'FLUSH'; Token: kwFLUSH), 
        (Keyword: 'FOR'; Token: kwFOR), 
        (Keyword: 'FORCE'; Token: kwFORCE), 
        (Keyword: 'FOREIGN'; Token: kwFOREIGN), 
        (Keyword: 'FORMAT'; Token: kwFORMAT), 
        (Keyword: 'FOUND'; Token: kwFOUND), 
        (Keyword: 'FRAC_SECOND'; Token: kwFRAC_SECOND), 
        (Keyword: 'FROM'; Token: kwFROM), 
        (Keyword: 'FROM_UNIXTIME'; Token: kwFROM_UNIXTIME), 
        (Keyword: 'FULL'; Token: kwFULL), 
        (Keyword: 'FULLTEXT'; Token: kwFULLTEXT), 
        (Keyword: 'FUNCTION'; Token: kwFUNCTION), 
        (Keyword: 'GEOMCOLLFROMTEXT'; Token: kwGEOMCOLLFROMTEXT), 
        (Keyword: 'GEOMCOLLFROMWKB'; Token: kwGEOMCOLLFROMWKB), 
        (Keyword: 'GEOMETRY'; Token: kwGEOMETRY), 
        (Keyword: 'GEOMETRYCOLLECTION'; Token: kwGEOMETRYCOLLECTION), 
        (Keyword: 'GEOMETRYCOLLECTIONFROMTEXT'; Token: kwGEOMETRYCOLLECTIONFROMTEXT), 
        (Keyword: 'GEOMETRYCOLLECTIONFROMWKB'; Token: kwGEOMETRYCOLLECTIONFROMWKB), 
        (Keyword: 'GEOMETRYFROMTEXT'; Token: kwGEOMETRYFROMTEXT), 
        (Keyword: 'GEOMETRYFROMWKB'; Token: kwGEOMETRYFROMWKB), 
        (Keyword: 'GEOMFROMTEXT'; Token: kwGEOMFROMTEXT), 
        (Keyword: 'GEOMFROMWKB'; Token: kwGEOMFROMWKB), 
        (Keyword: 'GET_FORMAT'; Token: kwGET_FORMAT), 
        (Keyword: 'GLOBAL'; Token: kwGLOBAL), 
        (Keyword: 'GOTO'; Token: kwGOTO), 
        (Keyword: 'GRANT'; Token: kwGRANT), 
        (Keyword: 'GRANTS'; Token: kwGRANTS), 
        (Keyword: 'GREATER'; Token: kwGREATER), 
        (Keyword: 'GREATEST'; Token: kwGREATEST), 
        (Keyword: 'GROUP'; Token: kwGROUP), 
        (Keyword: 'GROUP_CONCAT'; Token: kwGROUP_CONCAT), 
        (Keyword: 'GROUP_UNIQUE_USERS'; Token: kwGROUP_UNIQUE_USERS), 
        (Keyword: 'HANDLER'; Token: kwHANDLER), 
        (Keyword: 'HASH'; Token: kwHASH), 
        (Keyword: 'HAVING'; Token: kwHAVING), 
        (Keyword: 'HELP'; Token: kwHELP), 
        (Keyword: 'HIGH_PRIORITY'; Token: kwHIGH_PRIORITY), 
        (Keyword: 'HOSTS'; Token: kwHOSTS), 
        (Keyword: 'HOUR'; Token: kwHOUR), 
        (Keyword: 'HOUR_MICROSECOND'; Token: kwHOUR_MICROSECOND), 
        (Keyword: 'HOUR_MINUTE'; Token: kwHOUR_MINUTE), 
        (Keyword: 'HOUR_SECOND'; Token: kwHOUR_SECOND), 
        (Keyword: 'IDENTIFIED'; Token: kwIDENTIFIED), 
        (Keyword: 'IF'; Token: kwIF), 
        (Keyword: 'IGNORE'; Token: kwIGNORE), 
        (Keyword: 'IMPORT'; Token: kwIMPORT), 
        (Keyword: 'IN'; Token: kwIN), 
        (Keyword: 'INDEX'; Token: kwINDEX), 
        (Keyword: 'INDEXES'; Token: kwINDEXES), 
        (Keyword: 'INFILE'; Token: kwINFILE), 
        (Keyword: 'INNER'; Token: kwINNER), 
        (Keyword: 'INNOBASE'; Token: kwINNOBASE), 
        (Keyword: 'INNODB'; Token: kwINNODB), 
        (Keyword: 'INOUT'; Token: kwINOUT), 
        (Keyword: 'INSENSITIVE'; Token: kwINSENSITIVE), 
        (Keyword: 'INSERT'; Token: kwINSERT), 
        (Keyword: 'INSERT_METHOD'; Token: kwINSERT_METHOD), 
        (Keyword: 'INT'; Token: kwINT), 
        (Keyword: 'INT1'; Token: kwINT1), 
        (Keyword: 'INT2'; Token: kwINT2), 
        (Keyword: 'INT3'; Token: kwINT3), 
        (Keyword: 'INT4'; Token: kwINT4), 
        (Keyword: 'INT8'; Token: kwINT8), 
        (Keyword: 'INTEGER'; Token: kwINTEGER), 
        (Keyword: 'INTERVAL'; Token: kwINTERVAL), 
        (Keyword: 'INTO'; Token: kwINTO), 
        (Keyword: 'INVOKER'; Token: kwINVOKER), 
        (Keyword: 'IO_THREAD'; Token: kwIO_THREAD), 
        (Keyword: 'IS'; Token: kwIS), 
        (Keyword: 'ISOLATION'; Token: kwISOLATION), 
        (Keyword: 'ISSUER'; Token: kwISSUER), 
        (Keyword: 'ITERATE'; Token: kwITERATE), 
        (Keyword: 'JOIN'; Token: kwJOIN), 
        (Keyword: 'KEY'; Token: kwKEY), 
        (Keyword: 'KEYS'; Token: kwKEYS), 
        (Keyword: 'KILL'; Token: kwKILL), 
        (Keyword: 'LABEL'; Token: kwLABEL), 
        (Keyword: 'LANGUAGE'; Token: kwLANGUAGE), 
        (Keyword: 'LAST'; Token: kwLAST), 
        (Keyword: 'LAST_INSERT_ID'; Token: kwLAST_INSERT_ID), 
        (Keyword: 'LEADING'; Token: kwLEADING), 
        (Keyword: 'LEAST'; Token: kwLEAST), 
        (Keyword: 'LEAVE'; Token: kwLEAVE), 
        (Keyword: 'LEAVES'; Token: kwLEAVES), 
        (Keyword: 'LEFT'; Token: kwLEFT), 
        (Keyword: 'LESS'; Token: kwLESS), 
        (Keyword: 'LEVEL'; Token: kwLEVEL), 
        (Keyword: 'LIKE'; Token: kwLIKE), 
        (Keyword: 'LIMIT'; Token: kwLIMIT), 
        (Keyword: 'LINEFROMTEXT'; Token: kwLINEFROMTEXT), 
        (Keyword: 'LINEFROMWKB'; Token: kwLINEFROMWKB), 
        (Keyword: 'LINES'; Token: kwLINES), 
        (Keyword: 'LINESTRING'; Token: kwLINESTRING), 
        (Keyword: 'LINESTRINGFROMTEXT'; Token: kwLINESTRINGFROMTEXT), 
        (Keyword: 'LINESTRINGFROMWKB'; Token: kwLINESTRINGFROMWKB), 
        (Keyword: 'LIST'; Token: kwLIST), 
        (Keyword: 'LOAD'; Token: kwLOAD), 
        (Keyword: 'LOCAL'; Token: kwLOCAL), 
        (Keyword: 'LOCALTIME'; Token: kwLOCALTIME), 
        (Keyword: 'LOCALTIMESTAMP'; Token: kwLOCALTIMESTAMP), 
        (Keyword: 'LOCATE'; Token: kwLOCATE), 
        (Keyword: 'LOCK'; Token: kwLOCK), 
        (Keyword: 'LOCKS'; Token: kwLOCKS), 
        (Keyword: 'LOG'; Token: kwLOG), 
        (Keyword: 'LOGS'; Token: kwLOGS), 
        (Keyword: 'LONG'; Token: kwLONG), 
        (Keyword: 'LONGBLOB'; Token: kwLONGBLOB), 
        (Keyword: 'LONGTEXT'; Token: kwLONGTEXT), 
        (Keyword: 'LOOP'; Token: kwLOOP), 
        (Keyword: 'LOW_PRIORITY'; Token: kwLOW_PRIORITY), 
        (Keyword: 'MAKE_SET'; Token: kwMAKE_SET), 
        (Keyword: 'MASTER'; Token: kwMASTER), 
        (Keyword: 'MASTER_CONNECT_RETRY'; Token: kwMASTER_CONNECT_RETRY), 
        (Keyword: 'MASTER_HOST'; Token: kwMASTER_HOST), 
        (Keyword: 'MASTER_LOG_FILE'; Token: kwMASTER_LOG_FILE), 
        (Keyword: 'MASTER_LOG_POS'; Token: kwMASTER_LOG_POS), 
        (Keyword: 'MASTER_PASSWORD'; Token: kwMASTER_PASSWORD), 
        (Keyword: 'MASTER_PORT'; Token: kwMASTER_PORT), 
        (Keyword: 'MASTER_POS_WAIT'; Token: kwMASTER_POS_WAIT), 
        (Keyword: 'MASTER_SERVER_ID'; Token: kwMASTER_SERVER_ID), 
        (Keyword: 'MASTER_SSL'; Token: kwMASTER_SSL), 
        (Keyword: 'MASTER_SSL_CA'; Token: kwMASTER_SSL_CA), 
        (Keyword: 'MASTER_SSL_CAPATH'; Token: kwMASTER_SSL_CAPATH), 
        (Keyword: 'MASTER_SSL_CERT'; Token: kwMASTER_SSL_CERT), 
        (Keyword: 'MASTER_SSL_CIPHER'; Token: kwMASTER_SSL_CIPHER), 
        (Keyword: 'MASTER_SSL_KEY'; Token: kwMASTER_SSL_KEY), 
        (Keyword: 'MASTER_USER'; Token: kwMASTER_USER), 
        (Keyword: 'MATCH'; Token: kwMATCH), 
        (Keyword: 'MAX'; Token: kwMAX), 
        (Keyword: 'MAX_CONNECTIONS_PER_HOUR'; Token: kwMAX_CONNECTIONS_PER_HOUR), 
        (Keyword: 'MAX_QUERIES_PER_HOUR'; Token: kwMAX_QUERIES_PER_HOUR), 
        (Keyword: 'MAX_ROWS'; Token: kwMAX_ROWS), 
        (Keyword: 'MAX_UPDATES_PER_HOUR'; Token: kwMAX_UPDATES_PER_HOUR), 
        (Keyword: 'MAX_USER_CONNECTIONS'; Token: kwMAX_USER_CONNECTIONS), 
        (Keyword: 'MEDIUM'; Token: kwMEDIUM), 
        (Keyword: 'MEDIUMBLOB'; Token: kwMEDIUMBLOB), 
        (Keyword: 'MEDIUMINT'; Token: kwMEDIUMINT), 
        (Keyword: 'MEDIUMTEXT'; Token: kwMEDIUMTEXT), 
        (Keyword: 'MERGE'; Token: kwMERGE), 
        (Keyword: 'MICROSECOND'; Token: kwMICROSECOND), 
        (Keyword: 'MID'; Token: kwMID), 
        (Keyword: 'MIDDLEINT'; Token: kwMIDDLEINT), 
        (Keyword: 'MIGRATE'; Token: kwMIGRATE), 
        (Keyword: 'MIN'; Token: kwMIN), 
        (Keyword: 'MINUTE'; Token: kwMINUTE), 
        (Keyword: 'MINUTE_MICROSECOND'; Token: kwMINUTE_MICROSECOND), 
        (Keyword: 'MINUTE_SECOND'; Token: kwMINUTE_SECOND), 
        (Keyword: 'MIN_ROWS'; Token: kwMIN_ROWS), 
        (Keyword: 'MLINEFROMTEXT'; Token: kwMLINEFROMTEXT), 
        (Keyword: 'MLINEFROMWKB'; Token: kwMLINEFROMWKB), 
        (Keyword: 'MOD'; Token: kwMOD), 
        (Keyword: 'MODE'; Token: kwMODE), 
        (Keyword: 'MODIFIES'; Token: kwMODIFIES), 
        (Keyword: 'MODIFY'; Token: kwMODIFY), 
        (Keyword: 'MONTH'; Token: kwMONTH), 
        (Keyword: 'MPOINTFROMTEXT'; Token: kwMPOINTFROMTEXT), 
        (Keyword: 'MPOINTFROMWKB'; Token: kwMPOINTFROMWKB), 
        (Keyword: 'MPOLYFROMTEXT'; Token: kwMPOLYFROMTEXT), 
        (Keyword: 'MPOLYFROMWKB'; Token: kwMPOLYFROMWKB), 
        (Keyword: 'MULTILINESTRING'; Token: kwMULTILINESTRING), 
        (Keyword: 'MULTILINESTRINGFROMTEXT'; Token: kwMULTILINESTRINGFROMTEXT), 
        (Keyword: 'MULTILINESTRINGFROMWKB'; Token: kwMULTILINESTRINGFROMWKB), 
        (Keyword: 'MULTIPOINT'; Token: kwMULTIPOINT), 
        (Keyword: 'MULTIPOINTFROMTEXT'; Token: kwMULTIPOINTFROMTEXT), 
        (Keyword: 'MULTIPOINTFROMWKB'; Token: kwMULTIPOINTFROMWKB), 
        (Keyword: 'MULTIPOLYGON'; Token: kwMULTIPOLYGON), 
        (Keyword: 'MULTIPOLYGONFROMTEXT'; Token: kwMULTIPOLYGONFROMTEXT), 
        (Keyword: 'MULTIPOLYGONFROMWKB'; Token: kwMULTIPOLYGONFROMWKB), 
        (Keyword: 'MUTEX'; Token: kwMUTEX), 
        (Keyword: 'NAME'; Token: kwNAME), 
        (Keyword: 'NAMES'; Token: kwNAMES), 
        (Keyword: 'NATIONAL'; Token: kwNATIONAL), 
        (Keyword: 'NATURAL'; Token: kwNATURAL), 
        (Keyword: 'NCHAR'; Token: kwNCHAR), 
        (Keyword: 'NDB'; Token: kwNDB), 
        (Keyword: 'NDBCLUSTER'; Token: kwNDBCLUSTER), 
        (Keyword: 'NEW'; Token: kwNEW), 
        (Keyword: 'NEXT'; Token: kwNEXT), 
        (Keyword: 'NO'; Token: kwNO), 
        (Keyword: 'NONE'; Token: kwNONE), 
        (Keyword: 'NOT'; Token: kwNOT), 
        (Keyword: 'NOW'; Token: kwNOW), 
        (Keyword: 'NO_WRITE_TO_BINLOG'; Token: kwNO_WRITE_TO_BINLOG), 
        (Keyword: 'NULL'; Token: kwNULL), 
        (Keyword: 'NUMERIC'; Token: kwNUMERIC), 
        (Keyword: 'NVARCHAR'; Token: kwNVARCHAR), 
        (Keyword: 'OFFSET'; Token: kwOFFSET), 
        (Keyword: 'OLD_PASSWORD'; Token: kwOLD_PASSWORD), 
        (Keyword: 'ON'; Token: kwON), 
        (Keyword: 'ONE'; Token: kwONE), 
        (Keyword: 'ONE_SHOT'; Token: kwONE_SHOT), 
        (Keyword: 'OPEN'; Token: kwOPEN), 
        (Keyword: 'OPTIMIZE'; Token: kwOPTIMIZE), 
        (Keyword: 'OPTION'; Token: kwOPTION), 
        (Keyword: 'OPTIONALLY'; Token: kwOPTIONALLY), 
        (Keyword: 'OR'; Token: kwOR), 
        (Keyword: 'ORDER'; Token: kwORDER), 
        (Keyword: 'OUT'; Token: kwOUT), 
        (Keyword: 'OUTER'; Token: kwOUTER), 
        (Keyword: 'OUTFILE'; Token: kwOUTFILE), 
        (Keyword: 'PACK_KEYS'; Token: kwPACK_KEYS), 
        (Keyword: 'PARTIAL'; Token: kwPARTIAL), 
        (Keyword: 'PARTITION'; Token: kwPARTITION), 
        (Keyword: 'PARTITIONS'; Token: kwPARTITIONS), 
        (Keyword: 'PASSWORD'; Token: kwPASSWORD), 
        (Keyword: 'PHASE'; Token: kwPHASE), 
        (Keyword: 'POINT'; Token: kwPOINT), 
        (Keyword: 'POINTFROMTEXT'; Token: kwPOINTFROMTEXT), 
        (Keyword: 'POINTFROMWKB'; Token: kwPOINTFROMWKB), 
        (Keyword: 'POLYFROMTEXT'; Token: kwPOLYFROMTEXT), 
        (Keyword: 'POLYFROMWKB'; Token: kwPOLYFROMWKB), 
        (Keyword: 'POLYGON'; Token: kwPOLYGON), 
        (Keyword: 'POLYGONFROMTEXT'; Token: kwPOLYGONFROMTEXT), 
        (Keyword: 'POLYGONFROMWKB'; Token: kwPOLYGONFROMWKB), 
        (Keyword: 'POSITION'; Token: kwPOSITION), 
        (Keyword: 'PRECISION'; Token: kwPRECISION), 
        (Keyword: 'PREPARE'; Token: kwPREPARE), 
        (Keyword: 'PREV'; Token: kwPREV), 
        (Keyword: 'PRIMARY'; Token: kwPRIMARY), 
        (Keyword: 'PRIVILEGES'; Token: kwPRIVILEGES), 
        (Keyword: 'PROCEDURE'; Token: kwPROCEDURE), 
        (Keyword: 'PROCESS'; Token: kwPROCESS), 
        (Keyword: 'PROCESSLIST'; Token: kwPROCESSLIST), 
        (Keyword: 'PURGE'; Token: kwPURGE), 
        (Keyword: 'QUARTER'; Token: kwQUARTER), 
        (Keyword: 'QUERY'; Token: kwQUERY), 
        (Keyword: 'QUICK'; Token: kwQUICK), 
        (Keyword: 'RAID0'; Token: kwRAID0), 
        (Keyword: 'RAID_CHUNKS'; Token: kwRAID_CHUNKS), 
        (Keyword: 'RAID_CHUNKSIZE'; Token: kwRAID_CHUNKSIZE), 
        (Keyword: 'RAID_TYPE'; Token: kwRAID_TYPE), 
        (Keyword: 'RAND'; Token: kwRAND), 
        (Keyword: 'RANGE'; Token: kwRANGE), 
        (Keyword: 'READ'; Token: kwREAD), 
        (Keyword: 'READS'; Token: kwREADS), 
        (Keyword: 'REAL'; Token: kwREAL), 
        (Keyword: 'RECOVER'; Token: kwRECOVER), 
        (Keyword: 'REDUNDANT'; Token: kwREDUNDANT), 
        (Keyword: 'REFERENCES'; Token: kwREFERENCES), 
        (Keyword: 'REGEXP'; Token: kwREGEXP), 
        (Keyword: 'RELAY_LOG_FILE'; Token: kwRELAY_LOG_FILE), 
        (Keyword: 'RELAY_LOG_POS'; Token: kwRELAY_LOG_POS), 
        (Keyword: 'RELAY_THREAD'; Token: kwRELAY_THREAD), 
        (Keyword: 'RELEASE'; Token: kwRELEASE), 
        (Keyword: 'RELOAD'; Token: kwRELOAD), 
        (Keyword: 'RENAME'; Token: kwRENAME), 
        (Keyword: 'REPAIR'; Token: kwREPAIR), 
        (Keyword: 'REPEAT'; Token: kwREPEAT), 
        (Keyword: 'REPEATABLE'; Token: kwREPEATABLE), 
        (Keyword: 'REPLACE'; Token: kwREPLACE), 
        (Keyword: 'REPLICATION'; Token: kwREPLICATION), 
        (Keyword: 'REQUIRE'; Token: kwREQUIRE), 
        (Keyword: 'RESET'; Token: kwRESET), 
        (Keyword: 'RESTORE'; Token: kwRESTORE), 
        (Keyword: 'RESTRICT'; Token: kwRESTRICT), 
        (Keyword: 'RESUME'; Token: kwRESUME), 
        (Keyword: 'RETURN'; Token: kwRETURN), 
        (Keyword: 'RETURNS'; Token: kwRETURNS), 
        (Keyword: 'REVOKE'; Token: kwREVOKE), 
        (Keyword: 'RIGHT'; Token: kwRIGHT), 
        (Keyword: 'RLIKE'; Token: kwRLIKE), 
        (Keyword: 'ROLLBACK'; Token: kwROLLBACK), 
        (Keyword: 'ROLLUP'; Token: kwROLLUP), 
        (Keyword: 'ROUND'; Token: kwROUND), 
        (Keyword: 'ROUTINE'; Token: kwROUTINE), 
        (Keyword: 'ROW'; Token: kwROW), 
        (Keyword: 'ROWS'; Token: kwROWS), 
        (Keyword: 'ROW_COUNT'; Token: kwROW_COUNT), 
        (Keyword: 'ROW_FORMAT'; Token: kwROW_FORMAT), 
        (Keyword: 'RTREE'; Token: kwRTREE), 
        (Keyword: 'SAVEPOINT'; Token: kwSAVEPOINT), 
        (Keyword: 'SCHEMA'; Token: kwSCHEMA), 
        (Keyword: 'SCHEMAS'; Token: kwSCHEMAS), 
        (Keyword: 'SECOND'; Token: kwSECOND), 
        (Keyword: 'SECOND_MICROSECOND'; Token: kwSECOND_MICROSECOND), 
        (Keyword: 'SECURITY'; Token: kwSECURITY), 
        (Keyword: 'SELECT'; Token: kwSELECT), 
        (Keyword: 'SENSITIVE'; Token: kwSENSITIVE), 
        (Keyword: 'SEPARATOR'; Token: kwSEPARATOR), 
        (Keyword: 'SERIAL'; Token: kwSERIAL), 
        (Keyword: 'SERIALIZABLE'; Token: kwSERIALIZABLE), 
        (Keyword: 'SESSION'; Token: kwSESSION), 
        (Keyword: 'SESSION_USER'; Token: kwSESSION_USER), 
        (Keyword: 'SET'; Token: kwSET), 
        (Keyword: 'SHARE'; Token: kwSHARE), 
        (Keyword: 'SHOW'; Token: kwSHOW), 
        (Keyword: 'SHUTDOWN'; Token: kwSHUTDOWN), 
        (Keyword: 'SIGNED'; Token: kwSIGNED), 
        (Keyword: 'SIMPLE'; Token: kwSIMPLE), 
        (Keyword: 'SLAVE'; Token: kwSLAVE), 
        (Keyword: 'SMALLINT'; Token: kwSMALLINT), 
        (Keyword: 'SNAPSHOT'; Token: kwSNAPSHOT), 
        (Keyword: 'SOME'; Token: kwSOME), 
        (Keyword: 'SONAME'; Token: kwSONAME), 
        (Keyword: 'SOUNDS'; Token: kwSOUNDS), 
        (Keyword: 'SPATIAL'; Token: kwSPATIAL), 
        (Keyword: 'SPECIFIC'; Token: kwSPECIFIC), 
        (Keyword: 'SQL'; Token: kwSQL), 
        (Keyword: 'SQLEXCEPTION'; Token: kwSQLEXCEPTION), 
        (Keyword: 'SQLSTATE'; Token: kwSQLSTATE), 
        (Keyword: 'SQLWARNING'; Token: kwSQLWARNING), 
        (Keyword: 'SQL_BIG_RESULT'; Token: kwSQL_BIG_RESULT), 
        (Keyword: 'SQL_BUFFER_RESULT'; Token: kwSQL_BUFFER_RESULT), 
        (Keyword: 'SQL_CACHE'; Token: kwSQL_CACHE), 
        (Keyword: 'SQL_CALC_FOUND_ROWS'; Token: kwSQL_CALC_FOUND_ROWS), 
        (Keyword: 'SQL_NO_CACHE'; Token: kwSQL_NO_CACHE), 
        (Keyword: 'SQL_SMALL_RESULT'; Token: kwSQL_SMALL_RESULT), 
        (Keyword: 'SQL_THREAD'; Token: kwSQL_THREAD), 
        (Keyword: 'SQL_TSI_DAY'; Token: kwSQL_TSI_DAY), 
        (Keyword: 'SQL_TSI_FRAC_SECOND'; Token: kwSQL_TSI_FRAC_SECOND), 
        (Keyword: 'SQL_TSI_HOUR'; Token: kwSQL_TSI_HOUR), 
        (Keyword: 'SQL_TSI_MINUTE'; Token: kwSQL_TSI_MINUTE), 
        (Keyword: 'SQL_TSI_MONTH'; Token: kwSQL_TSI_MONTH), 
        (Keyword: 'SQL_TSI_QUARTER'; Token: kwSQL_TSI_QUARTER), 
        (Keyword: 'SQL_TSI_SECOND'; Token: kwSQL_TSI_SECOND), 
        (Keyword: 'SQL_TSI_WEEK'; Token: kwSQL_TSI_WEEK), 
        (Keyword: 'SQL_TSI_YEAR'; Token: kwSQL_TSI_YEAR), 
        (Keyword: 'SSL'; Token: kwSSL), 
        (Keyword: 'START'; Token: kwSTART), 
        (Keyword: 'STARTING'; Token: kwSTARTING), 
        (Keyword: 'STATUS'; Token: kwSTATUS), 
        (Keyword: 'STD'; Token: kwSTD), 
        (Keyword: 'STDDEV'; Token: kwSTDDEV), 
        (Keyword: 'STDDEV_POP'; Token: kwSTDDEV_POP), 
        (Keyword: 'STDDEV_SAMP'; Token: kwSTDDEV_SAMP), 
        (Keyword: 'STOP'; Token: kwSTOP), 
        (Keyword: 'STORAGE'; Token: kwSTORAGE), 
        (Keyword: 'STRAIGHT_JOIN'; Token: kwSTRAIGHT_JOIN), 
        (Keyword: 'STRING'; Token: kwSTRING), 
        (Keyword: 'STRIPED'; Token: kwSTRIPED), 
        (Keyword: 'SUBDATE'; Token: kwSUBDATE), 
        (Keyword: 'SUBJECT'; Token: kwSUBJECT), 
        (Keyword: 'SUBSTR'; Token: kwSUBSTR), 
        (Keyword: 'SUBSTRING'; Token: kwSUBSTRING), 
        (Keyword: 'SUBSTRING_INDEX'; Token: kwSUBSTRING_INDEX), 
        (Keyword: 'SUM'; Token: kwSUM), 
        (Keyword: 'SUPER'; Token: kwSUPER), 
        (Keyword: 'SUSPEND'; Token: kwSUSPEND), 
        (Keyword: 'SYSDATE'; Token: kwSYSDATE), 
        (Keyword: 'SYSTEM_USER'; Token: kwSYSTEM_USER), 
        (Keyword: 'TABLE'; Token: kwTABLE), 
        (Keyword: 'TABLES'; Token: kwTABLES), 
        (Keyword: 'TABLESPACE'; Token: kwTABLESPACE), 
        (Keyword: 'TEMPORARY'; Token: kwTEMPORARY), 
        (Keyword: 'TEMPTABLE'; Token: kwTEMPTABLE), 
        (Keyword: 'TERMINATED'; Token: kwTERMINATED), 
        (Keyword: 'TEXT'; Token: kwTEXT), 
        (Keyword: 'THAN'; Token: kwTHAN), 
        (Keyword: 'THEN'; Token: kwTHEN), 
        (Keyword: 'TIME'; Token: kwTIME), 
        (Keyword: 'TIMESTAMP'; Token: kwTIMESTAMP), 
        (Keyword: 'TIMESTAMPADD'; Token: kwTIMESTAMPADD), 
        (Keyword: 'TIMESTAMPDIFF'; Token: kwTIMESTAMPDIFF), 
        (Keyword: 'TINYBLOB'; Token: kwTINYBLOB), 
        (Keyword: 'TINYINT'; Token: kwTINYINT), 
        (Keyword: 'TINYTEXT'; Token: kwTINYTEXT), 
        (Keyword: 'TO'; Token: kwTO), 
        (Keyword: 'TRAILING'; Token: kwTRAILING), 
        (Keyword: 'TRANSACTION'; Token: kwTRANSACTION), 
        (Keyword: 'TRIGGER'; Token: kwTRIGGER), 
        (Keyword: 'TRIGGERS'; Token: kwTRIGGERS), 
        (Keyword: 'TRIM'; Token: kwTRIM), 
        (Keyword: 'TRUE'; Token: kwTRUE), 
        (Keyword: 'TRUNCATE'; Token: kwTRUNCATE), 
        (Keyword: 'TYPE'; Token: kwTYPE), 
        (Keyword: 'TYPES'; Token: kwTYPES), 
        (Keyword: 'UNCOMMITTED'; Token: kwUNCOMMITTED), 
        (Keyword: 'UNDEFINED'; Token: kwUNDEFINED), 
        (Keyword: 'UNDO'; Token: kwUNDO), 
        (Keyword: 'UNICODE'; Token: kwUNICODE), 
        (Keyword: 'UNION'; Token: kwUNION), 
        (Keyword: 'UNIQUE'; Token: kwUNIQUE), 
        (Keyword: 'UNIQUE_USERS'; Token: kwUNIQUE_USERS), 
        (Keyword: 'UNIX_TIMESTAMP'; Token: kwUNIX_TIMESTAMP), 
        (Keyword: 'UNKNOWN'; Token: kwUNKNOWN), 
        (Keyword: 'UNLOCK'; Token: kwUNLOCK), 
        (Keyword: 'UNSIGNED'; Token: kwUNSIGNED), 
        (Keyword: 'UNTIL'; Token: kwUNTIL), 
        (Keyword: 'UPDATE'; Token: kwUPDATE), 
        (Keyword: 'USAGE'; Token: kwUSAGE), 
        (Keyword: 'USE'; Token: kwUSE), 
        (Keyword: 'USER'; Token: kwUSER), 
        (Keyword: 'USER_RESOURCES'; Token: kwUSER_RESOURCES), 
        (Keyword: 'USE_FRM'; Token: kwUSE_FRM), 
        (Keyword: 'USING'; Token: kwUSING), 
        (Keyword: 'UTC_DATE'; Token: kwUTC_DATE), 
        (Keyword: 'UTC_TIME'; Token: kwUTC_TIME), 
        (Keyword: 'UTC_TIMESTAMP'; Token: kwUTC_TIMESTAMP), 
        (Keyword: 'VALUE'; Token: kwVALUE), 
        (Keyword: 'VALUES'; Token: kwVALUES), 
        (Keyword: 'VARBINARY'; Token: kwVARBINARY), 
        (Keyword: 'VARCHAR'; Token: kwVARCHAR), 
        (Keyword: 'VARCHARACTER'; Token: kwVARCHARACTER), 
        (Keyword: 'VARIABLES'; Token: kwVARIABLES), 
        (Keyword: 'VARIANCE'; Token: kwVARIANCE), 
        (Keyword: 'VARYING'; Token: kwVARYING), 
        (Keyword: 'VAR_POP'; Token: kwVAR_POP), 
        (Keyword: 'VAR_SAMP'; Token: kwVAR_SAMP), 
        (Keyword: 'VIEW'; Token: kwVIEW), 
        (Keyword: 'WARNINGS'; Token: kwWARNINGS), 
        (Keyword: 'WEEK'; Token: kwWEEK), 
        (Keyword: 'WHEN'; Token: kwWHEN), 
        (Keyword: 'WHERE'; Token: kwWHERE), 
        (Keyword: 'WHILE'; Token: kwWHILE), 
        (Keyword: 'WITH'; Token: kwWITH), 
        (Keyword: 'WORK'; Token: kwWORK), 
        (Keyword: 'WRITE'; Token: kwWRITE), 
        (Keyword: 'X509'; Token: kwX509), 
        (Keyword: 'XA'; Token: kwXA), 
        (Keyword: 'XOR'; Token: kwXOR), 
        (Keyword: 'YEAR'; Token: kwYEAR), 
        (Keyword: 'YEARWEEK'; Token: kwYEARWEEK), 
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
