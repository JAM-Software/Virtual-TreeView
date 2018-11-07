unit VirtualTrees.FMX;

{$SCOPEDENUMS ON}

interface
uses System.UITypes, System.Types, System.ImageList, FMX.ImgList, FMX.Graphics;

const
  clBtnFace = TAlphaColorRec.Gray;
  clBtnText = TAlphaColorRec.Black;
  clBtnHighlight = TAlphaColorRec.DkGray;
  clBtnShadow = TAlphaColorRec.Darkgray;
  clHighlight = TAlphaColorRec.Lightblue;
  clWindow = TAlphaColorRec.White;
  clWindowText = TAlphaColorRec.Black;
  clHighlightText = TAlphaColorRec.White;
  clWhite = TAlphaColorRec.White;
  clSilver = TAlphaColorRec.Silver;
  clGray = TAlphaColorRec.Gray;
  clBlack = TAlphaColorRec.Black;
  clGreen = TAlphaColorRec.Green;
  clBlue =  TAlphaColorRec.Blue;
  clGrayText =  TAlphaColorRec.DkGray;

const
  { 3D border styles }
  {$EXTERNALSYM BDR_RAISEDOUTER}
  BDR_RAISEDOUTER = 1;
  {$EXTERNALSYM BDR_SUNKENOUTER}
  BDR_SUNKENOUTER = 2;
  {$EXTERNALSYM BDR_RAISEDINNER}
  BDR_RAISEDINNER = 4;
  {$EXTERNALSYM BDR_SUNKENINNER}
  BDR_SUNKENINNER = 8;

  {$EXTERNALSYM BDR_OUTER}
  BDR_OUTER = 3;
  {$EXTERNALSYM BDR_INNER}
  BDR_INNER = 12;
  {$EXTERNALSYM BDR_RAISED}
  BDR_RAISED = 5;
  {$EXTERNALSYM BDR_SUNKEN}
  BDR_SUNKEN = 10;

  {$EXTERNALSYM EDGE_RAISED}
  EDGE_RAISED = (BDR_RAISEDOUTER or BDR_RAISEDINNER);
  {$EXTERNALSYM EDGE_SUNKEN}
  EDGE_SUNKEN = (BDR_SUNKENOUTER or BDR_SUNKENINNER);
  {$EXTERNALSYM EDGE_ETCHED}
  EDGE_ETCHED = (BDR_SUNKENOUTER or BDR_RAISEDINNER);
  {$EXTERNALSYM EDGE_BUMP}
  EDGE_BUMP = (BDR_RAISEDOUTER or BDR_SUNKENINNER);

  {$EXTERNALSYM ETO_OPAQUE}
  ETO_OPAQUE = 2;
  {$EXTERNALSYM ETO_CLIPPED}
  ETO_CLIPPED = 4;
  {$EXTERNALSYM ETO_RTLREADING}
  ETO_RTLREADING = $80;

  RTLFlag: array[Boolean] of Integer = (0, ETO_RTLREADING);

  { Border flags }
  {$EXTERNALSYM BF_LEFT}
  BF_LEFT = 1;
  {$EXTERNALSYM BF_TOP}
  BF_TOP = 2;
  {$EXTERNALSYM BF_RIGHT}
  BF_RIGHT = 4;
  {$EXTERNALSYM BF_BOTTOM}
  BF_BOTTOM = 8;

  {$EXTERNALSYM BF_TOPLEFT}
  BF_TOPLEFT = (BF_TOP or BF_LEFT);
  {$EXTERNALSYM BF_TOPRIGHT}
  BF_TOPRIGHT = (BF_TOP or BF_RIGHT);
  {$EXTERNALSYM BF_BOTTOMLEFT}
  BF_BOTTOMLEFT = (BF_BOTTOM or BF_LEFT);
  {$EXTERNALSYM BF_BOTTOMRIGHT}
  BF_BOTTOMRIGHT = (BF_BOTTOM or BF_RIGHT);
  {$EXTERNALSYM BF_RECT}
  BF_RECT = (BF_LEFT or BF_TOP or BF_RIGHT or BF_BOTTOM);

  {$EXTERNALSYM BF_MIDDLE}
  BF_MIDDLE = $800;   { Fill in the middle }
  {$EXTERNALSYM BF_SOFT}
  BF_SOFT = $1000;    { For softer buttons }
  {$EXTERNALSYM BF_ADJUST}
  BF_ADJUST = $2000;  { Calculate the space left over }
  {$EXTERNALSYM BF_FLAT}
  BF_FLAT = $4000;    { For flat rather than 3D borders }
  {$EXTERNALSYM BF_MONO}
  BF_MONO = $8000;    { For monochrome borders }

  { DrawText() Format Flags }
  DT_TOP = 0;
  {$EXTERNALSYM DT_TOP}
  DT_LEFT = 0;
  {$EXTERNALSYM DT_LEFT}
  DT_CENTER = 1;
  {$EXTERNALSYM DT_CENTER}
  DT_RIGHT = 2;
  {$EXTERNALSYM DT_RIGHT}
  DT_VCENTER = 4;
  {$EXTERNALSYM DT_VCENTER}
  DT_BOTTOM = 8;
  {$EXTERNALSYM DT_BOTTOM}
  DT_WORDBREAK = $10;
  {$EXTERNALSYM DT_WORDBREAK}
  DT_SINGLELINE = $20;
  {$EXTERNALSYM DT_SINGLELINE}
  DT_EXPANDTABS = $40;
  {$EXTERNALSYM DT_EXPANDTABS}
  DT_TABSTOP = $80;
  {$EXTERNALSYM DT_TABSTOP}
  DT_NOCLIP = $100;
  {$EXTERNALSYM DT_NOCLIP}
  DT_EXTERNALLEADING = $200;
  {$EXTERNALSYM DT_EXTERNALLEADING}
  DT_CALCRECT = $400;
  {$EXTERNALSYM DT_CALCRECT}
  DT_NOPREFIX = $800;
  {$EXTERNALSYM DT_NOPREFIX}
  DT_INTERNAL = $1000;
  {$EXTERNALSYM DT_INTERNAL}


  DT_EDITCONTROL = $2000;
  {$EXTERNALSYM DT_EDITCONTROL}
  DT_PATH_ELLIPSIS = $4000;
  {$EXTERNALSYM DT_PATH_ELLIPSIS}
  DT_END_ELLIPSIS = $8000;
  {$EXTERNALSYM DT_END_ELLIPSIS}
  DT_MODIFYSTRING = $10000;
  {$EXTERNALSYM DT_MODIFYSTRING}
  DT_RTLREADING = $20000;
  {$EXTERNALSYM DT_RTLREADING}
  DT_WORD_ELLIPSIS = $40000;
  {$EXTERNALSYM DT_WORD_ELLIPSIS}
  DT_NOFULLWIDTHCHARBREAK = $0080000;
  {$EXTERNALSYM DT_NOFULLWIDTHCHARBREAK}
  DT_HIDEPREFIX = $00100000;
  {$EXTERNALSYM DT_HIDEPREFIX}
  DT_PREFIXONLY = $00200000;
  {$EXTERNALSYM DT_PREFIXONLY}  
  
type
  TRect = System.Types.TRectF;
  PRect = System.Types.PRectF;
  TPoint = System.Types.TPointF;
  PPoint = System.Types.PPointF;
  PSize = System.Types.PSizeF;
  TSize = System.Types.TSizeF;
  TColor = System.UITypes.TAlphaColor;

  TBorderWidth = Single;
  TBevelCut = (bvNone, bvLowered, bvRaised, bvSpace);
  TBevelEdge = (beLeft, beTop, beRight, beBottom);
  TBevelEdges = set of TBevelEdge;
  TBevelKind = (bkNone, bkTile, bkSoft, bkFlat);
  TBevelWidth = 1..MaxInt;

  TFormBorderStyle = (bsNone, bsSingle, bsSizeable, bsDialog, bsToolWindow, bsSizeToolWin);
  TBorderStyle = TFormBorderStyle.bsNone..TFormBorderStyle.bsSingle;

  TChangeLink = class(TImageLink)
  private
    function GetSender: TCustomImageList; inline;
    procedure SetSender(const Value: TCustomImageList); inline;
  public
    constructor Create; override;
    property Sender: TCustomImageList read GetSender write SetSender;
  end;
  
  TTextMetric = record
    tmHeight: Single;  //The height (ascent + descent) of characters.
    tmAscent: Single;  //The ascent (units above the base line) of characters.
    tmDescent: Single; //The descent (units below the base line) of characters.
    tmInternalLeading: Single; //The amount of leading (space) inside the bounds set by the tmHeight member. Accent marks and other diacritical characters may occur in this area. The designer may set this member to zero
    tmExternalLeading: Single; //The amount of extra leading (space) that the application adds between rows. Since this area is outside the font, it contains no marks and is not altered by text output calls in either OPAQUE or TRANSPARENT mode. The designer may set this member to zero.
    tmAveCharWidth: Single; //The average width of characters in the font (generally defined as the width of the letter x ). This value does not include the overhang required for bold or italic characters.
    tmMaxCharWidth: Single; //The width of the widest character in the font.
    tmWeight: Single; //The weight of the font.
    tmOverhang: Single;
    tmDigitizedAspectX: Single; //The horizontal aspect of the device for which the font was designed.
    tmDigitizedAspectY: Single; //The vertical aspect of the device for which the font was designed. The ratio of the tmDigitizedAspectX and tmDigitizedAspectY members is the aspect ratio of the device for which the font was designed.
    tmFirstChar: WideChar; //The value of the first character defined in the font.
    tmLastChar: WideChar;  //The value of the last character defined in the font.
    tmDefaultChar: WideChar; //The value of the character to be substituted for characters not in the font.
    tmBreakChar: WideChar; //The value of the character that will be used to define word breaks for text justification.
    tmItalic: Byte; //Specifies an italic font if it is nonzero.
    tmUnderlined: Byte; //Specifies an underlined font if it is nonzero.
    tmStruckOut: Byte; //A strikeout font if it is nonzero.
    tmPitchAndFamily: Byte; //Specifies information about the pitch, the technology, and the family of a physical font.  TMPF_FIXED_PITCH, TMPF_VECTOR, TMPF_TRUETYPE, TMPF_DEVICE
    tmCharSet: Byte; //The character set of the font. The character set can be one of the following values. ANSI_CHARSET, GREEK_CHARSET....
  end;
  procedure GetTextMetrics(ACanvas: TCanvas; var TM: TTextMetric);
  function Rect(ALeft, ATop, ARight, ABottom: Single): TRect; overload; inline;
  function Rect(const ATopLeft, ABottomRight: TPoint): TRect; overload; inline;
  function Point(AX, AY: Single): TPoint; overload; inline;

  procedure Inc(Var V: Single; OIle: Single=1.0); overload;
  procedure Dec(Var V: Single; OIle: Single=1.0); overload;
  function MulDiv(const A, B, C: Single): Single; overload;
  procedure FillMemory(Destination: Pointer; Length: NativeUInt; Fill: Byte);
  procedure ZeroMemory(Destination: Pointer; Length: NativeUInt);
  procedure MoveMemory(Destination: Pointer; Source: Pointer; Length: NativeUInt);
  procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: NativeUInt);
  

implementation
uses FMX.TextLayout, System.SysUtils;

procedure GetTextMetrics(ACanvas: TCanvas; var TM: TTextMetric);
Var P: TPathData;
  tx: TTextLayout;
  R: TRectF;
begin
{
    tmHeight: Single;  //The height (ascent + descent) of characters.
    tmAscent: Single;  //The ascent (units above the base line) of characters.
    tmDescent: Single; //The descent (units below the base line) of characters.
    tmInternalLeading: Single; //The amount of leading (space) inside the bounds set by the tmHeight member. Accent marks and other diacritical characters may occur in this area. The designer may set this member to zero
    tmExternalLeading: Single; //The amount of extra leading (space) that the application adds between rows. Since this area is outside the font, it contains no marks and is not altered by text output calls in either OPAQUE or TRANSPARENT mode. The designer may set this member to zero.
    tmAveCharWidth: Single; //The average width of characters in the font (generally defined as the width of the letter x ). This value does not include the overhang required for bold or italic characters.
    tmMaxCharWidth: Single; //The width of the widest character in the font.
    tmWeight: Single; //The weight of the font.
    tmOverhang: Single;
    tmDigitizedAspectX: Single; //The horizontal aspect of the device for which the font was designed.
    tmDigitizedAspectY: Single; //The vertical aspect of the device for which the font was designed. The ratio of the tmDigitizedAspectX and tmDigitizedAspectY members is the aspect ratio of the device for which the font was designed.
    tmFirstChar: WideChar; //The value of the first character defined in the font.
    tmLastChar: WideChar;  //The value of the last character defined in the font.
    tmDefaultChar: WideChar; //The value of the character to be substituted for characters not in the font.
    tmBreakChar: WideChar; //The value of the character that will be used to define word breaks for text justification.
    tmItalic: Byte; //Specifies an italic font if it is nonzero.
    tmUnderlined: Byte; //Specifies an underlined font if it is nonzero.
    tmStruckOut: Byte; //A strikeout font if it is nonzero.
    tmPitchAndFamily: Byte; //Specifies information about the pitch, the technology, and the family of a physical font.  TMPF_FIXED_PITCH, TMPF_VECTOR, TMPF_TRUETYPE, TMPF_DEVICE
    tmCharSet: Byte; //The character set of the font. The character set can be one of the following values. ANSI_CHARSET, GREEK_CHARSET....
}
  TM.tmExternalLeading:= 0;
  TM.tmWeight:= 0; //boldness???
  TM.tmOverhang:= 0;
  TM.tmDigitizedAspectX:= 0;
  TM.tmDigitizedAspectY:= 0;
  TM.tmFirstChar:= 'a'; //???
  TM.tmLastChar:= 'z'; //???
  TM.tmDefaultChar:= ' ';
  TM.tmBreakChar:= ' ';
  TM.tmItalic:= 0;
  TM.tmUnderlined:= 0;
  TM.tmStruckOut:= 0;
  TM.tmPitchAndFamily:= 0;
  TM.tmCharSet:= 0;

  tx:= TTextLayoutManager.DefaultTextLayout.Create(ACanvas);
  P:= TPathData.Create;
  try
    tx.Text:= 'W';
    tx.ConvertToPath(p);
    R:= P.GetBounds();

    TM.tmHeight:= R.Height;
    TM.tmMaxCharWidth:= R.Width;

    //------------------------------------
    tx.Text:= 'Ó';
    p.Clear;
    tx.ConvertToPath(p);
    R:= P.GetBounds();
    TM.tmInternalLeading:= R.Height - TM.tmHeight;

    //------------------------------------
    tx.Text:= 'x';
    p.Clear;
    tx.ConvertToPath(p);
    R:= P.GetBounds();
    TM.tmAscent:= R.Height - TM.tmHeight;
    TM.tmAveCharWidth:= R.Width;

    //------------------------------------
    tx.Text:= 'y';
    p.Clear;
    tx.ConvertToPath(p);
    TM.tmDescent:= P.GetBounds().Height - R.Height;
    TM.tmHeight:= TM.tmHeight + TM.tmDescent;
  finally
    FreeAndNil(P);
    FreeAndNil(tx);
  end;
end;

function Rect(ALeft, ATop, ARight, ABottom: Single): TRect;
begin
  Result:= RectF(ALeft, ATop, ARight, ABottom);
end;

function Rect(const ATopLeft, ABottomRight: TPoint): TRect;
begin
  Result:= RectF(ATopLeft.X, ATopLeft.Y, ABottomRight.X, ABottomRight.Y);
end;

function Point(AX, AY: Single): TPoint;
begin
  Result.X:= AX;
  Result.Y:= AY;
end;

procedure Inc(Var V: Single; OIle: Single=1.0);
begin
  V:= V + OIle;
end;

procedure Dec(Var V: Single; OIle: Single=1.0);
begin
  V:= V - OIle;
end;

function MulDiv(const A, B, C: Single): Single;
begin
  Result:= (A * B) / C;
end;

procedure FillMemory(Destination: Pointer; Length: NativeUInt; Fill: Byte);
begin
  FillChar(Destination^, Length, Fill);
end;

procedure ZeroMemory(Destination: Pointer; Length: NativeUInt);
begin
  FillChar(Destination^, Length, 0);
end;

procedure MoveMemory(Destination: Pointer; Source: Pointer; Length: NativeUInt);
begin
  Move(Source^, Destination^, Length);
end;

procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: NativeUInt);
begin
  Move(Source^, Destination^, Length);
end;

{ TChangeLink }

constructor TChangeLink.Create;
begin
  inherited;
  IgnoreIndex := True;
  IgnoreImages := True;
end;

function TChangeLink.GetSender: TCustomImageList;
begin
  Result := TCustomImageList(Images);
end;

procedure TChangeLink.SetSender(const Value: TCustomImageList);
begin
  Images := TBaseImageList(Value);
end;



end.
