unit MFEdit;

// MF Editor components
// Version 1.0.2 Beta
// Copyright (c) 2001 Manfred Fuchs
// Manfred@Fuchsrudel.de
//
// This software is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied.
//
// Many ideas for the components based on the TEdites component from Erol S. Uzuner.
// TMFNumEdit is a enlarged pascal port from BMSpinEdit, originally written in C++.
// The developer of the famous BMSpinEdit is Boian Mitov.
//              Copyright (C) 1998, 2001 by Boian Mitov
//              <mitov@mitov.com>
//              <http://www.mitov.com>
//
//----------------------------------------------------------------------------------------------------------------------
//
// 10-MAY-2001:
//   - TMFNumEdit extensions
//   - bug fixes
//
// 08-MAY-2001:
//   - Alignment of TMFNumEdit now works correctly
//   - bug fixes
//
// 30-APR-2001:
//   - added TMFComboEdit
//   - bug fixes
//
// 27-APR-2001:
//   - first beta release
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  Windows, SysUtils, Forms, Classes, Messages, Graphics, Controls,
  StdCtrls, ExtCtrls, ComCtrls, CommCtrl, Menus, Buttons, Dialogs, Mask, Math;

const
  DEF_FOCUSED_COLOR = clWindow;
  DEF_REQUIRED_COLOR = clYellow;

  BORDER_WIDTH = 4;
  WM_NEEDS_UPDATE = WM_USER + $600;

  Glyph_Check = 'CHECKGLYPH';
  Glyph_Date = 'DATEGLYPH';
  Glyph_Directory = 'DIRECTORYGLYPH';
  Glyph_DropDown = 'DROPDOWNGLYPH';
  Glyph_DropLeft = 'DROPLEFTGLYPH';
  Glyph_Ellipsis = 'ELLIPSISGLYPH';
  Glyph_File = 'FILEGLYPH';
  Glyph_Help = 'HELPGLYPH';
  Glyph_Home = 'HOMEGLYPH';
  Glyph_Mail = 'MAILGLYPH';
  Glyph_Open = 'OPENGLYPH';
  Glyph_Save = 'SAVEGLYPH';
  Glyph_Search = 'SEARCHGLYPH';
  Glyph_Time = 'TIMEGLYPH';

type
  TEditStyle = (esUnderline, esCtl2D, esCtl3D);
  TGlyphStyle = (gsCustom, gsDefault,
    gsCheck, gsDate, gsDirectory, gsDropDown, gsEllipsis, gsFile,
    gsHelp, gsHome, gsMail, gsOpen, gsSave, gsSearch, gsTime);
  TSetItemIndex = procedure(Sender: TObject; var Value: Longint) of object;
  TKeySet = (ksReturn, ksEscape);
  TKeySets = set of TKeySet;

  TMFCustomEdit = class(TCustomMaskEdit)
  protected
    FAlignment: TAlignment;
    FBeforeFocusColor: TColor;
    FCanvas: TControlCanvas;
    FEditStyle: TEditStyle;
    FFirstCharUpperCase: Boolean;
    FFocusedColor: TColor;
    FKeys: TKeySets;
    FLineStyle: TBevelStyle;
    FRequired: Boolean;
    FRequiredColor: TColor;

    procedure CMEnter(var Message: TMessage); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FocusColorize(GotFocus: Boolean);
    procedure KeyPress(var Key: Char); override;
    procedure SetAlignment(Value: TAlignment);
    procedure SetEditStyle(Value: TEditStyle);
    procedure SetFirstCharUpper(Value: Boolean);
    procedure SetFRColor(Index: Integer; Value: TColor);
    procedure SetLineStyle(Value: TBevelStyle);
    procedure SetRequired(Value: Boolean);
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear; override;
    procedure RequiredColorize;

    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property EditStyle: TEditStyle read FEditStyle write SetEditStyle default esCtl3D;
    property FirstCharUpperCase: Boolean read FFirstCharUpperCase write setFirstCharUpper default False;
    property FocusedColor: TColor index 1 read FFocusedColor write SetFRColor default DEF_FOCUSED_COLOR;
    property Keys: TKeySets read FKeys write FKeys;
    property LineStyle: TBevelStyle read FLineStyle write SetLineStyle default bsRaised;
    property Required: Boolean read FRequired write SetRequired default False;
    property RequiredColor: TColor index 2 read FRequiredColor write SetFRColor default DEF_REQUIRED_COLOR;
  end;

  TMFEdit = class(TMFCustomEdit)
  public
    procedure DoPopupComplete(var Key: Char);

  published
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EditMask;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

    property Alignment;
    property EditStyle;
    property FirstCharUpperCase;
    property FocusedColor;
    property Keys;
    property LineStyle;
    property Required;
    property RequiredColor;
  end;

  TMFCustomButtonEdit = class(TMFCustomEdit)
  protected
    FButton: TSpeedButton;
    FButtonClick: TNotifyEvent;

    FGlyphStyle: TGlyphStyle;
    FDroppedDown: Boolean;

    procedure ButtonClick(Sender: TObject);
    procedure CMEnabledChanged(var Message: TWMNoParams); message CM_ENABLEDCHANGED;
    procedure CreateHandle; override;
    function GetOnClick: TNotifyEvent;
    procedure SetEditStyle(Value: TEditStyle);
    procedure SetGlyphStyle(Value: TGlyphStyle);
    procedure SetOnClick(Value: TNotifyEvent);
    procedure UpdateFormatRect;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

  public
    constructor Create(AOwner: TComponent); override;

    property DroppedDown: Boolean read FDroppedDown;
    property EditStyle: TEditStyle read FEditStyle write SetEditStyle default esCtl3D;
    property GlyphStyle: TGlyphStyle read FGlyphStyle write SetGlyphStyle default gsDefault;
    property OnButtonClick: TNotifyEvent read GetOnClick write SetOnClick;
  end;

  TMFButtonEdit = class(TMFCustomButtonEdit)
  published
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EditMask;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

    property Alignment;
    property EditStyle;
    property FirstCharUpperCase;
    property FocusedColor;
    property Keys;
    property LineStyle;
    property Required;
    property RequiredColor;

    property GlyphStyle;
    property OnButtonClick;
  end;

  TMFCustomComboEdit = class(TCustomComboBox)
  protected
    FAlignment: TAlignment;
    FAutoSelect: Boolean;
    FBeforeFocusColor: TColor;
    FCanvas: TControlCanvas;
    FEditStyle: TEditStyle;
    FFirstCharUpperCase: Boolean;
    FFocusedColor: TColor;
    FKeys: TKeySets;
    FLineStyle: TBevelStyle;
    FRequired: Boolean;
    FRequiredColor: TColor;

    procedure CMEnter(var Message: TMessage); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FocusColorize(GotFocus: Boolean);
    procedure KeyPress(var Key: Char); override;
    procedure SetAlignment(Value: TAlignment);
    procedure SetEditStyle(Value: TEditStyle);
    procedure SetFirstCharUpper(Value: Boolean);
    procedure SetFRColor(Index: Integer; Value: TColor);
    procedure SetLineStyle(Value: TBevelStyle);
    procedure SetRequired(Value: Boolean);
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear; override;
    procedure RequiredColorize;

    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSelect: Boolean read FAutoSelect write FAutoSelect default True;
    property EditStyle: TEditStyle read FEditStyle write SetEditStyle default esCtl3D;
    property FirstCharUpperCase: Boolean read FFirstCharUpperCase write setFirstCharUpper default False;
    property FocusedColor: TColor index 1 read FFocusedColor write SetFRColor default DEF_FOCUSED_COLOR;
    property Keys: TKeySets read FKeys write FKeys;
    property LineStyle: TBevelStyle read FLineStyle write SetLineStyle default bsRaised;
    property Required: Boolean read FRequired write SetRequired default False;
    property RequiredColor: TColor index 2 read FRequiredColor write SetFRColor default DEF_REQUIRED_COLOR;
  end;

  TMFComboEdit = class(TMFCustomComboEdit)
  published
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDock;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }

    property Alignment;
    property AutoSelect;
    property EditStyle;
    property FirstCharUpperCase;
    property FocusedColor;
    property Keys;
    property LineStyle;
    property Required;
    property RequiredColor;
  end;

  TNumFormat = (nfStandard, nfThousands); //, nfCurrency);

  TMFNumEditGauge = class;

  TMFCustomNumEdit = class(TMFCustomEdit)
  private
    Form: TForm;
    TrackBar: TTrackBar;
    ProgressBar: TMFNumEditGauge;
    FMinValue: Double;
    FMaxValue: Double;
    FGaugeMinValue: Double;
    FGaugeMaxValue: Double;
    FValueUnit: string;
    FIncrement: Double;
    FEditorEnabled: Boolean;
    InChanging: Boolean;
    FTrackBarOrientation: TTrackBarOrientation;
    FWrap: Boolean;
    FPrecision: Integer;
    FGaugeHeight: Integer;
    FTrackBarWidth: Integer;
    FTrackBarHeight: Integer;
    FUpDown: TUpDown;
    FSpeedButton: TSpeedButton;
    FTrackBarEnabled: Boolean;
    FPanel: TPanel;
    MoveFormToPoint: TPoint;

    FNumFormat: TNumFormat;
    FDroppedDown: Boolean;
    FLastValue: Double;
  private
    function ValueToText(const V: Double): string;
    function RawValueStr(const S: string): string;
    function TextToValue(S: string): Double;
    function GetMinHeight: Integer;
    procedure SetEditRect;
    function GetValue: Double;
    function CheckValue(NewV: Double): Double;
    procedure SetValue(NewV: Double);
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure SpeedButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SpeedButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SpeedButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure UpdateFormView;
    procedure SetMaxValue(V: Double);
    procedure SetMinValue(V: Double);
    procedure SetGaugeMaxValue(V: Double);
    procedure SetGaugeMinValue(V: Double);
    procedure SetValueUnit(S: string);
    procedure SetIncrement(V: Double);
    procedure SetGaugeAroundCenter(V: Double);
    function GetGaugeAroundCenter: Double;
    procedure SetIncrementGauge(V: Double);
    function GetIncrementGauge(): Double;
    procedure SetGaugeBeginColor(V: TColor);
    function GetGaugeBeginColor: TColor;
    procedure SetGaugeEndColor(V: TColor);
    function GetGaugeEndColor: TColor;
    procedure SetTrackBarWidth(V: Integer);
    procedure SetPrecision(V: Integer);
    procedure SetGaugeHeight(V: Integer);
    procedure SetTrackBarOrientation(Orientation: TTrackBarOrientation);
    procedure SetTrackBarEnabled(const Value: Boolean);
    procedure SetButtonHandle;
    procedure CMEnabledChanged(var Message: TWMNoParams); message CM_ENABLEDCHANGED;
    procedure SetNumFormat(V: TNumFormat);
    function GetTruncatedValue: Double;
    function GetText: string;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Change; override;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure CMEnter(var Message: TMessage); message CM_ENTER;
    procedure CMExit(var Message: TMessage); message CM_EXIT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMNeedsUpdate(var Message: TMessage); message WM_NEEDS_UPDATE;
  public
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled;
    property ValueUnit: string read FValueUnit write SetValueUnit;
    property Increment: Double read FIncrement write SetIncrement;
    property IncrementGauge: Double read GetIncrementGauge write SetIncrementGauge;
    property GaugeAroundCenter: Double read GetGaugeAroundCenter write SetGaugeAroundCenter;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property GaugeMaxValue: Double read FGaugeMaxValue write SetGaugeMaxValue;
    property GaugeMinValue: Double read FGaugeMinValue write SetGaugeMinValue;
    property Value: Double read GetTruncatedValue write SetValue;
    property GaugeBeginColor: TColor read GetGaugeBeginColor write SetGaugeBeginColor default clHighlight;
    property GaugeEndColor: TColor read GetGaugeEndColor write SetGaugeEndColor default clHighlight;
    property TrackBarWidth: Integer read FTrackBarWidth write SetTrackBarWidth default 125;
    property Precision: Integer read FPrecision write SetPrecision default 2;
    property TrackBarOrientation: TTrackBarOrientation read FTrackBarOrientation write SetTrackBarOrientation default
      trHorizontal;
    property TrackBarEnabled: Boolean read FTrackBarEnabled write SetTrackBarEnabled default True;
    property GaugeHeight: Integer read FGaugeHeight write SetGaugeHeight default 3;

    property Text: string read GetText;
    property RealValue: Double read GetValue;
    property NumFormat: TNumFormat read FNumFormat write SetNumFormat default nfStandard;
    property DroppedDown: Boolean read FDroppedDown;
  end;

  TMFNumEditGauge = class(TCustomControl)
  private
    FEdit: TMFCustomNumEdit;
    FGaugeBeginColor: TColor;
    FGaugeEndColor: TColor;
    FIncrement: Double;
    FGaugeAroundCenter: Double;
    procedure SetGaugeBeginColor(V: TColor);
    procedure SetGaugeEndColor(V: TColor);
    procedure PaintBackground(var AnImage: TBitmap);
    procedure PaintAsBar(var AnImage: TBitmap; const PaintRect: TRect);

  protected
    procedure Paint; override;
    procedure WMLBtnDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLBtnUp(var Message: TMessage); message WM_LBUTTONUP;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMEraseBkg(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMRBtnDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;

  public
    constructor Create(AOwner: TComponent); override;

    property Increment: Double read FIncrement write FIncrement;
    property GaugeAroundCenter: Double read FGaugeAroundCenter write FGaugeAroundCenter;
    property GaugeBeginColor: TColor read FGaugeBeginColor write SetGaugeBeginColor;
    property GaugeEndColor: TColor read FGaugeEndColor write SetGaugeEndColor;
  end;

  TMFNumEdit = class(TMFCustomNumEdit)
  published
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

    property Alignment;
    property EditStyle;
    property FocusedColor;
    property Keys;
    property LineStyle;
    property Required;
    property RequiredColor;

    property EditorEnabled;
    property ValueUnit;
    property Increment;
    property IncrementGauge;
    property GaugeAroundCenter;
    property MaxValue;
    property MinValue;
    property GaugeMaxValue;
    property GaugeMinValue;
    property Value;
    property GaugeBeginColor;
    property GaugeEndColor;
    property TrackBarWidth;
    property Precision;
    property TrackBarOrientation;
    property TrackBarEnabled;
    property GaugeHeight;

//    property NumFormat;
    property DroppedDown;
  end;

  TMFCustomDateTimeEdit = class(TDateTimePicker)
  protected
    FAlignment: TAlignment;
    FBeforeFocusColor: TColor;
    FCanvas: TControlCanvas;
//    FEditStyle          : TEditStyle;
    FFocusedColor: TColor;
    FKeys: TKeySets;
    FLineStyle: TBevelStyle;
    FRequired: Boolean;
    FRequiredColor: TColor;

    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FocusColorize(GotFocus: Boolean);
    procedure KeyPress(var Key: Char); override;
    procedure SetAlignment(Value: TAlignment);
//    procedure SetEditStyle(Value: TEditStyle);
    procedure SetFRColor(Index: Integer; Value: TColor);
    procedure SetLineStyle(Value: TBevelStyle);
    procedure SetRequired(Value: Boolean);
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RequiredColorize;

    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
//    property EditStyle          : TEditStyle     read FEditStyle          write SetEditStyle      default esCtl3D;
    property FocusedColor: TColor index 1 read FFocusedColor write SetFRColor default DEF_FOCUSED_COLOR;
    property Keys: TKeySets read FKeys write FKeys;
    property LineStyle: TBevelStyle read FLineStyle write SetLineStyle default bsRaised;
    property Required: Boolean read FRequired write SetRequired default False;
    property RequiredColor: TColor index 2 read FRequiredColor write SetFRColor default DEF_REQUIRED_COLOR;
  end;

  TMFDateTimeEdit = class(TMFCustomDateTimeEdit)
  published
    property Alignment;
//    property EditStyle;
    property FocusedColor;
    property Keys;
    property LineStyle;
    property Required;
    property RequiredColor;
  end;

// ****************************************************************************

procedure Register;

implementation

{$R *.res}

type
  TMFNumEditPopupForm = class(TForm)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd(); override;
  end;

// ****************************************************************************

constructor TMFCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAlignment := taLeftJustify;
  FBeforeFocusColor := Color;
  FCanvas := nil;
  FEditStyle := esCtl3D;
  FFirstCharUpperCase := False;
  FFocusedColor := DEF_FOCUSED_COLOR;
  FLineStyle := bsRaised;
  FRequired := False;
  FRequiredColor := DEF_REQUIRED_COLOR;

  Height := 21;
  Width := 101;
end;

destructor TMFCustomEdit.Destroy;
begin
  FCanvas.Free;

  inherited Destroy;
end;

procedure TMFCustomEdit.Clear;
begin
  inherited Clear;
  RequiredColorize;
end;

procedure TMFCustomEdit.CMEnter(var Message: TMessage);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

procedure TMFCustomEdit.CMExit(var Message: TCMExit);
begin
  inherited;
end;

procedure TMFCustomEdit.CMTextChanged(var Message: TMessage);
begin
  RequiredColorize;
end;

procedure TMFCustomEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  case Alignment of
    taLeftJustify:
      Params.Style := Params.Style or (ES_LEFT or ES_MULTILINE);
    taRightJustify:
      Params.Style := Params.Style or (ES_RIGHT or ES_MULTILINE);
    taCenter:
      Params.Style := Params.Style or (ES_CENTER or ES_MULTILINE);
  end;
end;

procedure TMFCustomEdit.FocusColorize(GotFocus: Boolean);
begin
  if GotFocus then
  begin
    if Color <> FRequiredColor then
      FBeforeFocusColor := Color;
    if Color <> FFocusedColor then
      Color := FFocusedColor;
  end
  else
  begin
    if FRequired and (Text = '') then
    begin
      if Color <> FRequiredColor then
        Color := FRequiredColor;
    end
    else
    begin
      Color := FBeforeFocusColor;
      if (FEditStyle = esUnderline) then
        ParentColor := True;
            {else if Color <> clWindow then Color :=  clWindow;}
    end;
  end;
end;

procedure TMFCustomEdit.KeyPress(var Key: Char);
var
  str: string;
begin
  if ksReturn in Keys then
  begin
    if Key = Chr(Vk_Return) then
    begin
      Key := #0;
      (Owner.Owner as TControl).Perform(WM_NEXTDLGCTL, 0, 0);
    end;
  end;
  if ksEscape in Keys then
  begin
    if Key = Chr(Vk_Escape) then
    begin
      Key := #0;
      (Owner.Owner as TControl).Perform(WM_NEXTDLGCTL, 1, 0);
    end;
  end;
  if Key = Chr(Vk_Return) then
    Key := #0;

  if FFirstCharUpperCase then
  begin
    if Length(Text) = 0 then
    begin
      str := '' + Key;
      Key := UpperCase(str)[1];
    end;
  end;

  inherited KeyPress(Key);
end;

procedure TMFCustomEdit.RequiredColorize;
begin
  if FRequired then
  begin
    if (Text = '') then
    begin
      if Color <> FRequiredColor then
        Color := FRequiredColor;
    end
    else
    begin
      Color := FBeforeFocusColor;
      if FEditStyle = esUnderline then
        ParentColor := True;
    end;
  end;
end;

procedure TMFCustomEdit.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TMFCustomEdit.SetEditStyle(Value: TEditStyle);
begin
  if Value <> FEditStyle then
  begin
    FEditStyle := Value;
    case Value of
      esUnderline:
        begin
          Ctl3D := False;
          BorderStyle := bsNone;
          ParentColor := True;
        end;
      esCtl2D:
        begin
          Ctl3D := False;
          BorderStyle := bsSingle;
          ParentColor := False;
          Color := FBeforeFocusColor;
        end;
      esCtl3D:
        begin
          Ctl3D := True;
          BorderStyle := bsSingle;
          ParentColor := False;
          Color := FBeforeFocusColor;
        end;
    end;
    Invalidate;
  end;
end;

procedure TMFCustomEdit.SetFirstCharUpper(Value: Boolean);
begin
  if (Value <> FFirstCharUpperCase) then
    FFirstCharUpperCase := Value;
end;

procedure TMFCustomEdit.SetFRColor(Index: Integer; Value: TColor);
begin
  case Index of
    1:
      if FFocusedColor <> Value then
      begin
        FFocusedColor := Value;
        Invalidate;
        if Focused then
          Color := Value;
      end;
    2:
      if FRequiredColor <> Value then
      begin
        FRequiredColor := Value;
        if (Text = '') and FRequired then
          Color := Value;
      end;
  end;
end;

procedure TMFCustomEdit.SetLineStyle(Value: TBevelStyle);
begin
  if FEditStyle = esUnderline then
  begin
    if Value <> FLineStyle then
    begin
      FLineStyle := Value;
      Invalidate;
    end;
  end;
end;

procedure TMFCustomEdit.SetRequired(Value: Boolean);
begin
  if FRequired <> Value then
  begin
    FRequired := Value;
    if Value and (Text = '') then
    begin
      if Color <> FRequiredColor then
        Color := FRequiredColor;
    end
    else
      if not Value then
      begin
        if ParentColor then
          Perform(CM_PARENTCOLORCHANGED, 0, 0)
        else
          if Color <> clWindow then
            Color := FBeforeFocusColor;
      end;
  end;
end;

procedure TMFCustomEdit.WMKeyUp(var Message: TWMKeyUp);
begin
  inherited;
end;

procedure TMFCustomEdit.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;
  FocusColorize(False);
end;

procedure TMFCustomEdit.WMPaint(var Message: TWMPaint);

procedure BevelLine(C: TColor; X1, Y1, X2, Y2: Integer);
  begin
    with FCanvas do
    begin
      Pen.Color := C;
      MoveTo(X1, Y1);
      LineTo(X2, Y2);
    end;
  end;

  procedure Underlined;
  var
    Color1,
      Color2: TColor;
  begin
    if FCanvas = nil then
    begin
      FCanvas := TControlCanvas.Create;
      FCanvas.Control := Self;
    end;

    with FCanvas do
    begin
      if FLineStyle = bsLowered then
      begin
        Color1 := clBtnShadow;
        Color2 := clBtnHighlight;
      end
      else
      begin
        Color1 := clBtnHighlight;
        Color2 := clBtnShadow;
      end;

      BevelLine(Color1, 0, Height - 2, Width, Height - 2);
      BevelLine(Color2, 0, Height - 1, Width, Height - 1);
    end;
  end;
begin
  inherited;
  if FEditStyle = esUnderline then
    Underlined;
end;

procedure TMFCustomEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  FocusColorize(True);
end;

// ****************************************************************************

procedure TMFEdit.DoPopupComplete(var Key: Char);
var
  str: string;
  i,
    k: Integer;
begin
  if Key > #31 then
  begin
    if PopupMenu <> nil then
    begin
            {Das ist der String + dem Zeichen }
      str := UpperCase(Copy(Text, 0, Length(Text) - SelLength) + Key);
      for i := 0 to PopupMenu.Items.Count - 1 do
      begin
        if Pos(str, UpperCase(PopupMenu.Items[i].Caption)) = 1 then
        begin
          k := Length(Text) - SelLength;
          Text := PopupMenu.Items[i].Caption;
          SelStart := k + 1;
          SelLength := Length(Text) - (k + 1);
          Key := #0;
          Exit;
        end;
      end;
      Key := #0; {wenn nichts übereinstimmt wurde eine Falsche Taste gedrückt}
    end;
  end;
end;

// ****************************************************************************

constructor TMFCustomButtonEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDroppedDown := False;
  FGlyphStyle := gsDefault;
  FButton := TSpeedButton.Create(Self);
  with FButton do
  begin
    Parent := Self;
    Align := alRight;
    Caption := '';
    Transparent := False;
    if EditStyle = esCtl3D then
      Flat := False
    else
      Flat := True;
  end;
end;

procedure TMFCustomButtonEdit.ButtonClick(Sender: TObject);
begin
  FDroppedDown := True;
  if Assigned(FButtonClick) then
    FButtonClick(Sender);
  FDroppedDown := False;
end;

procedure TMFCustomButtonEdit.CMEnabledChanged(var Message: TWMNoParams);
begin
  inherited;
  FButton.Enabled := Enabled;
end;

procedure TMFCustomButtonEdit.CreateHandle;
begin
  inherited CreateHandle;
  UpdateFormatRect;
end;

function TMFCustomButtonEdit.GetOnClick: TNotifyEvent;
begin
  Result := FButtonClick;
end;

procedure TMFCustomButtonEdit.SetEditStyle(Value: TEditStyle);
begin
  if Value <> FEditStyle then
  begin
    inherited SetEditStyle(Value);
    case Value of
      esUnderline,
        esCtl2D:
        FButton.Flat := True;
      esCtl3D:
        FButton.Flat := False;
    end;
  end;
end;

procedure TMFCustomButtonEdit.SetGlyphStyle(Value: TGlyphStyle);
{
    function CreateEllipsisGlyph: TBitmap;
    var
        W,
        G,
        I : Integer;
    begin
        Result := TBitmap.Create;
        with Result do
        try
            Monochrome := True;
            Width      := Max( 1, FButton.Width - 7 );
            Height     := 4;
            W          := 2;
            G          := ( Result.Width - 6 * W ) div 2;
            if G <= 0 then
                G := 1;
            if G > 3 then
                G := 3;
            I := ( Width - 3 * W - 2 * G ) div 2;
            PatBlt( Canvas.Handle, I,                 0, W, W, BLACKNESS );
            PatBlt( Canvas.Handle, I + G + W,         0, W, W, BLACKNESS );
            PatBlt( Canvas.Handle, I + 2 * G + 2 * W, 0, W, W, BLACKNESS );
        except
            Free;
            raise;
        end;
    end;
}
begin
  FGlyphStyle := Value;
  case Value of
    gsDefault:
      FButton.Glyph.Assign(nil);
    gsCheck:
      FButton.Glyph.Handle := LoadBitmap(hinstance, Glyph_Check);
    gsDate:
      FButton.Glyph.Handle := LoadBitmap(hinstance, Glyph_Date);
    gsDirectory:
      FButton.Glyph.Handle := LoadBitmap(hinstance, Glyph_Directory);
    gsDropDown:
      FButton.Glyph.Handle := LoadBitmap(0, PChar(32738));
    gsEllipsis:
      FButton.Glyph.Handle := LoadBitmap(hinstance, Glyph_Ellipsis);
    gsFile:
      FButton.Glyph.Handle := LoadBitmap(hinstance, Glyph_File);
    gsHelp:
      FButton.Glyph.Handle := LoadBitmap(hinstance, Glyph_Help);
    gsHome:
      FButton.Glyph.Handle := LoadBitmap(hinstance, Glyph_Home);
    gsMail:
      FButton.Glyph.Handle := LoadBitmap(hinstance, Glyph_Mail);
    gsOpen:
      FButton.Glyph.Handle := LoadBitmap(hinstance, Glyph_Open);
    gsSave:
      FButton.Glyph.Handle := LoadBitmap(hinstance, Glyph_Save);
    gsSearch:
      FButton.Glyph.Handle := LoadBitmap(hinstance, Glyph_Search);
    gsTime:
      FButton.Glyph.Handle := LoadBitmap(hinstance, Glyph_Time);
  end;
  FButton.NumGlyphs := 1;
end;

procedure TMFCustomButtonEdit.SetOnClick(Value: TNotifyEvent);
begin
  FButtonClick := Value;
  if Assigned(Value) then
    FButton.OnClick := ButtonClick
  else
    FButton.OnClick := nil;
end;

procedure TMFCustomButtonEdit.UpdateFormatRect;
var
  Rect: TRect;
begin
  Rect := ClientRect;
  Dec(Rect.Right, FButton.Width);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Rect));
end;

procedure TMFCustomButtonEdit.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  if (P.x >= ClientWidth - FButton.Width) then
//        Screen.Cursor := FButton.Cursor
    SetCursor(Screen.Cursors[crDefault])
  else
    inherited;
end;

procedure TMFCustomButtonEdit.WMSize(var Message: TWMSize);
begin
  inherited;
  FButton.Width := FButton.Height - 1;
  UpdateFormatRect;
end;

// ****************************************************************************

constructor TMFCustomComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAlignment := taLeftJustify;
  FAutoSelect := True;
  FBeforeFocusColor := Color;
  FCanvas := nil;
  FEditStyle := esCtl3D;
  FFirstCharUpperCase := False;
  FFocusedColor := DEF_FOCUSED_COLOR;
  FLineStyle := bsRaised;
  FRequired := False;
  FRequiredColor := DEF_REQUIRED_COLOR;

  Height := 21;
  Width := 101;
end;

destructor TMFCustomComboEdit.Destroy;
begin
  FCanvas.Free;

  inherited Destroy;
end;

procedure TMFCustomComboEdit.Clear;
begin
  inherited Clear;
  RequiredColorize;
end;

procedure TMFCustomComboEdit.CMEnter(var Message: TMessage);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

procedure TMFCustomComboEdit.CMExit(var Message: TCMExit);
begin
  inherited;
end;

procedure TMFCustomComboEdit.CMTextChanged(var Message: TMessage);
begin
  RequiredColorize;
end;

procedure TMFCustomComboEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  case Alignment of
    taLeftJustify:
      Params.Style := Params.Style or (ES_LEFT or ES_MULTILINE);
    taRightJustify:
      Params.Style := Params.Style or (ES_RIGHT or ES_MULTILINE);
    taCenter:
      Params.Style := Params.Style or (ES_CENTER or ES_MULTILINE);
  end;
end;

procedure TMFCustomComboEdit.FocusColorize(GotFocus: Boolean);
begin
  if GotFocus then
  begin
    if Color <> FRequiredColor then
      FBeforeFocusColor := Color;
    if Color <> FFocusedColor then
      Color := FFocusedColor;
  end
  else
  begin
    if FRequired and (Text = '') then
    begin
      if Color <> FRequiredColor then
        Color := FRequiredColor;
    end
    else
    begin
      Color := FBeforeFocusColor;
      if (FEditStyle = esUnderline) then
        ParentColor := True;
            {else if Color <> clWindow then Color :=  clWindow;}
    end;
  end;
end;

procedure TMFCustomComboEdit.KeyPress(var Key: Char);
var
  str: string;
begin
  if ksReturn in Keys then
  begin
    if Key = Chr(Vk_Return) then
    begin
      Key := #0;
      (Owner.Owner as TControl).Perform(WM_NEXTDLGCTL, 0, 0);
    end;
  end;
  if ksEscape in Keys then
  begin
    if Key = Chr(Vk_Escape) then
    begin
      Key := #0;
      (Owner.Owner as TControl).Perform(WM_NEXTDLGCTL, 1, 0);
    end;
  end;
  if (Key = Chr(Vk_Return)) and not DroppedDown then
    Key := #0;

  if FFirstCharUpperCase then
  begin
    if Length(Text) = 0 then
    begin
      str := '' + Key;
      Key := UpperCase(str)[1];
    end;
  end;

  inherited KeyPress(Key);
end;

procedure TMFCustomComboEdit.RequiredColorize;
begin
  if FRequired then
  begin
    if (Text = '') then
    begin
      if Color <> FRequiredColor then
        Color := FRequiredColor;
    end
    else
    begin
      Color := FBeforeFocusColor;
      if FEditStyle = esUnderline then
        ParentColor := True;
    end;
  end;
end;

procedure TMFCustomComboEdit.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TMFCustomComboEdit.SetEditStyle(Value: TEditStyle);
begin
  if Value <> FEditStyle then
  begin
    FEditStyle := Value;
    case Value of
      esUnderline:
        begin
          Ctl3D := False;
//                BorderStyle := bsNone;
          ParentColor := True;
        end;
      esCtl2D:
        begin
          Ctl3D := False;
//                BorderStyle := bsSingle;
          ParentColor := False;
          Color := FBeforeFocusColor;
        end;
      esCtl3D:
        begin
          Ctl3D := True;
//                BorderStyle := bsSingle;
          ParentColor := False;
          Color := FBeforeFocusColor;
        end;
    end;
    Invalidate;
  end;
end;

procedure TMFCustomComboEdit.SetFirstCharUpper(Value: Boolean);
begin
  if (Value <> FFirstCharUpperCase) then
    FFirstCharUpperCase := Value;
end;

procedure TMFCustomComboEdit.SetFRColor(Index: Integer; Value: TColor);
begin
  case Index of
    1:
      if FFocusedColor <> Value then
      begin
        FFocusedColor := Value;
        Invalidate;
        if Focused then
          Color := Value;
      end;
    2:
      if FRequiredColor <> Value then
      begin
        FRequiredColor := Value;
        if (Text = '') and FRequired then
          Color := Value;
      end;
  end;
end;

procedure TMFCustomComboEdit.SetLineStyle(Value: TBevelStyle);
begin
  if FEditStyle = esUnderline then
  begin
    if Value <> FLineStyle then
    begin
      FLineStyle := Value;
      Invalidate;
    end;
  end;
end;

procedure TMFCustomComboEdit.SetRequired(Value: Boolean);
begin
  if FRequired <> Value then
  begin
    FRequired := Value;
    if Value and (Text = '') then
    begin
      if Color <> FRequiredColor then
        Color := FRequiredColor;
    end
    else
      if not Value then
      begin
        if ParentColor then
          Perform(CM_PARENTCOLORCHANGED, 0, 0)
        else
          if Color <> clWindow then
            Color := FBeforeFocusColor;
      end;
  end;
end;

procedure TMFCustomComboEdit.WMKeyUp(var Message: TWMKeyUp);
begin
  inherited;
end;

procedure TMFCustomComboEdit.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;
  FocusColorize(False);
end;

procedure TMFCustomComboEdit.WMPaint(var Message: TWMPaint);

procedure BevelLine(C: TColor; X1, Y1, X2, Y2: Integer);
  begin
    with FCanvas do
    begin
      Pen.Color := C;
      MoveTo(X1, Y1);
      LineTo(X2, Y2);
    end;
  end;

  procedure Underlined;
  var
    Color1,
      Color2: TColor;
  begin
    if FCanvas = nil then
    begin
      FCanvas := TControlCanvas.Create;
      FCanvas.Control := Self;
    end;

    with FCanvas do
    begin
      if FLineStyle = bsLowered then
      begin
        Color1 := clBtnShadow;
        Color2 := clBtnHighlight;
      end
      else
      begin
        Color1 := clBtnHighlight;
        Color2 := clBtnShadow;
      end;

      BevelLine(Color1, 0, Height - 2, Width, Height - 2);
      BevelLine(Color2, 0, Height - 1, Width, Height - 1);
    end;
  end;
begin
  inherited;
  if FEditStyle = esUnderline then
    Underlined;
end;

procedure TMFCustomComboEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  FocusColorize(True);
end;

// ****************************************************************************

function TMFCustomNumEdit.ValueToText(const V: Double): string;
begin
  if FPrecision < 0 then
  begin
    Result := FloatToStrF(V, ffGeneral, 4, 4);
  end
  else
    case FNumFormat of
      nfStandard:
        Result := FloatToStrF(V, ffFixed, 15, FPrecision);
      nfThousands:
        Result := FloatToStrF(V, ffNumber, 15, FPrecision);
//            nfCurrency:
//                Result := FloatToStrF( V, ffCurrency, 15, FPrecision );
    end;

  if (FValueUnit <> '') then //and ( FNumFormat <> nfCurrency ) then
    Result := Result + ' ' + FValueUnit;
end;

function TMFCustomNumEdit.RawValueStr(const S: string): string;
var
  P: Integer;
begin
  Result := S;
  if FValueUnit <> '' then
  begin
    P := Pos(FValueUnit, S);
    if P > 2 then
      Result := Trim(Copy(S, 1, P - 1))
    else
    begin
      P := Pos(' ', S);
      if P > 1 then
        Result := Copy(S, 1, P - 1);
    end;
  end;
end;

function TMFCustomNumEdit.TextToValue(S: string): Double;
var
  P: Integer;
begin
  S := RawValueStr(S);
    // Try relaxed handling of decimalseparators.
    // Enable "." as well as "," as decimalseparator:
  if (S > '') and (S <> '-') then
  begin
    for P := Length(S) downto 1 do
      if S[P] = #32 then
        Delete(S, 1, 1)
      else
        if not (S[P] in ['0'..'9', '-', DecimalSeparator]) then
          S[P] := DecimalSeparator;
    if S[1] = DecimalSeparator then
      S := '0' + S;
    if S[Length(S)] = DecimalSeparator then
      S := S + '0';
    Result := StrToFloat(S);
  end
  else
    Result := 0;
end;

function TMFCustomNumEdit.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFONT;
  SysMetrics,
    Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Result := Metrics.tmHeight + Min(SysMetrics.tmHeight, Metrics.tmHeight) div 4 +
    GetSystemMetrics(SM_CYBORDER) * 4 + 2 + GaugeHeight + 1;
end;

procedure TMFCustomNumEdit.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, Longint(@Loc));
  Loc.Bottom := ClientHeight + 1; // +1 is workaround for windows paint bug
  Loc.Right := ClientWidth; //- FUpDown->Width - 2;
  Loc.Top := 0;
  Loc.Left := 0;
  SendMessage(Handle, EM_SETRECTNP, 0, Longint(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, Longint(@Loc)); // debug
end;

function TMFCustomNumEdit.GetValue: Double;

function SetValueBack: Double;
  var
    cpos: Integer;
  begin
    Result := CheckValue(FLastValue);
    cpos := SelStart;
    inherited Text := ValueToText(Result);
    if cpos > 0 then
      SelStart := cpos - 1
    else
      SelStart := cpos;
    ProgressBar.Repaint;
    if Form.Visible then
      UpdateFormView;
  end;
begin
  if inherited Text = '' then
  begin
    Result := CheckValue(0);
    inherited Text := ValueToText(Result);
    Exit;
  end;
  try
    Result := TextToValue(inherited Text);
    if Result <> CheckValue(Result) then
      Result := SetValueBack;
  except
//        Text   := ValueToText( MinValue );
//        Result := MinValue;
    Result := SetValueBack;
  end;
  FLastValue := Result;
end;

function TMFCustomNumEdit.CheckValue(NewV: Double): Double;
begin
  Result := NewV;
  if MaxValue <> MinValue then
    Result := Min(Max(NewV, MinValue), MaxValue);
end;

procedure TMFCustomNumEdit.SetValue(NewV: Double);
var
  OldValue: Double;
begin
  OldValue := RealValue;
  FLastValue := CheckValue(NewV);
  inherited Text := ValueToText(FLastValue);
    // Check NewV against OldValue and not Value because Value itself
    // could be already NewV:
  if NewV = OldValue then
    Exit;
  ProgressBar.Repaint;
  if Form.Visible then
    UpdateFormView;
  if not (csLoading in ComponentState) and Assigned(OnChange) then
    OnChange(Self);
end;

procedure TMFCustomNumEdit.UpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  SetFocus;
  if Button = btNext then
  begin
    Value := RealValue + Increment;
    if RealValue >= (MaxValue - Increment) then
    begin
      if FWrap then
        Value := MinValue
      else
        Value := MaxValue;
    end
  end
  else
  begin
    if RealValue <= (MinValue + Increment) then
    begin
      if FWrap then
        Value := MaxValue
      else
        Value := MinValue;
    end
    else
      Value := RealValue - Increment;
  end;
  PostMessage(Handle, WM_NEEDS_UPDATE, 0, 0);
  if (AutoSelect and not (csLButtonDown in ControlState)) then
    SelectAll;
  ProgressBar.Invalidate;
end;

procedure TMFCustomNumEdit.SpeedButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y:
  Integer);
var
  APoint,
    Pos: Tpoint;
begin
  if Button <> mbLeft then
    Exit;
  APoint.X := X;
  APoint.Y := Y;
  MoveFormToPoint := FSpeedButton.ClientToScreen(APoint);
  if FTrackBarOrientation = trVertical then
  begin
    APoint.X := FSpeedButton.Left + FSpeedButton.Width;
    APoint.Y := FSpeedButton.Top;
    Pos := FPanel.ClientToScreen(APoint);
  end
  else
  begin
    APoint.X := FSpeedButton.Left;
    APoint.Y := FSpeedButton.Top;
    Pos := FPanel.ClientToScreen(APoint);
  end;
  Form.Left := Pos.x;
  Form.Top := Pos.y + FSpeedButton.Height;

  FDroppedDown := True;
  UpdateFormView;
end;

procedure TMFCustomNumEdit.SpeedButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y:
  Integer);
begin
  if Button <> mbLeft then
    Exit;
  Form.Visible := False;
//    FSpeedButton.Enabled    := True;
  FSpeedButton.Down := False;
  FSpeedButton.GroupIndex := 0;
  FSpeedButton.Enabled := False;
  FSpeedButton.Enabled := True;

  FDroppedDown := False;

  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
end;

procedure TMFCustomNumEdit.SpeedButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  APoint,
    MousePoint,
    SliderLeftTop,
    SliderRightBottom: TPoint;
  Rect,
    Rect1: TRect;
  SliderSize,
    Position: Longint;
  TumbHeight,
    TumbWidth: Integer;
  V: Double;
begin
  if not Form.Visible then
    Exit;
  APoint.X := X;
  APoint.Y := Y;
  MousePoint := FSpeedButton.ClientToScreen(APoint);
  TrackBar.Perform(TBM_GETCHANNELRECT, 0, Integer(@Rect));
  TrackBar.Perform(TBM_GETTHUMBRECT, 0, Integer(@Rect1));
  if FTrackBarOrientation = trVertical then
  begin
    APoint.X := Rect.Top;
    APoint.Y := Rect.Left;
    SliderLeftTop := Form.ClientToScreen(APoint);
    APoint.X := Rect.Bottom;
    APoint.Y := Rect.Right;
    SliderRightBottom := Form.ClientToScreen(APoint);
    TumbHeight := Rect1.Bottom - Rect1.Top;
    SliderSize := SliderRightBottom.y - SliderLeftTop.y - TumbHeight;
    Position := SliderSize - (MousePoint.y - SliderLeftTop.y - TumbHeight);
  end
  else
  begin
    APoint.X := Rect.Left;
    APoint.Y := Rect.Top;
    SliderLeftTop := Form.ClientToScreen(APoint);
    APoint.X := Rect.Right;
    APoint.Y := Rect.Bottom;
    SliderRightBottom := Form.ClientToScreen(APoint);
    TumbWidth := Rect1.Right - Rect1.Left;
    SliderSize := SliderRightBottom.x - SliderLeftTop.x - TumbWidth;
    Position := MousePoint.x - SliderLeftTop.x - TumbWidth;
  end;
  if Position > SliderSize then
    Position := SliderSize;
  if Position < 0 then
    Position := 0;
  V := (Position * (FMaxValue - MinValue)) / SliderSize + MinValue;
  if IncrementGauge > 0 then
    V := IncrementGauge * Round(V / IncrementGauge);
  Value := V;
end;

procedure TMFCustomNumEdit.FormShow(Sender: TObject);
var
  Rect: TRect;
begin
  TrackBar.Perform(TBM_GETTHUMBRECT, 0, Integer(@Rect));
  if (FTrackBarOrientation = trVertical) then
    Form.Top := MoveFormToPoint.y - Rect.Top - ((Rect.Bottom - Rect.Top) div 2) - BORDER_WIDTH
  else
    Form.Left := MoveFormToPoint.x - Rect.Left - ((Rect.Right - Rect.Left) div 2) - BORDER_WIDTH;

//    FSpeedButton.Enabled    := False;
  FSpeedButton.GroupIndex := 333553;
  FSpeedButton.Down := True;

end;

procedure TMFCustomNumEdit.UpdateFormView;
var
  Diff: Double;
  FillSize: Longint;
begin
  Diff := MaxValue - MinValue;
  if Diff <= 0 then
    Diff := 100;
  FillSize := Min(Round((RealValue - MinValue) * 1000 / Diff), 1000);
  if FTrackBarOrientation = trVertical then
  begin
    TrackBar.Position := TrackBar.Max - FillSize;
    Form.Height := FTrackBarWidth;
  end
  else
  begin
    TrackBar.Position := FillSize;
    Form.Width := FTrackBarWidth;
  end;

  SetFocus;
  SelLength := 0;
  Form.Visible := True;
end;

procedure TMFCustomNumEdit.SetMaxValue(V: Double);
begin
  if FMaxValue <> V then
  begin
    FMaxValue := V;
    Value := Min(RealValue, FMaxValue);
    ProgressBar.Repaint;
  end;
end;

procedure TMFCustomNumEdit.SetMinValue(V: Double);
begin
  if MinValue <> V then
  begin
    FMinValue := V;
    Value := Max(RealValue, FMinValue);
    ProgressBar.Repaint;
  end;
end;

procedure TMFCustomNumEdit.SetGaugeMaxValue(V: Double);
begin
  if FGaugeMaxValue <> V then
  begin
    FGaugeMaxValue := V;
    ProgressBar.Repaint;
  end;
end;

procedure TMFCustomNumEdit.SetGaugeMinValue(V: Double);
begin
  if FGaugeMinValue <> V then
  begin
    FGaugeMinValue := V;
    ProgressBar.Repaint;
  end;
end;

procedure TMFCustomNumEdit.SetValueUnit(S: string);
begin
  FValueUnit := S;
  SetValue(RealValue);
end;

procedure TMFCustomNumEdit.SetIncrement(V: Double);
begin
  FIncrement := V;
end;

procedure TMFCustomNumEdit.SetIncrementGauge(V: Double);
begin
  ProgressBar.Increment := V;
end;

function TMFCustomNumEdit.GetIncrementGauge(): Double;
begin
  Result := ProgressBar.Increment;
end;

procedure TMFCustomNumEdit.SetGaugeAroundCenter(V: Double);
begin
  ProgressBar.GaugeAroundCenter := V;
  ProgressBar.Repaint;
end;

function TMFCustomNumEdit.GetGaugeAroundCenter: Double;
begin
  Result := ProgressBar.GaugeAroundCenter;
end;

procedure TMFCustomNumEdit.SetGaugeBeginColor(V: TColor);
begin
  ProgressBar.GaugeBeginColor := V;
end;

function TMFCustomNumEdit.GetGaugeBeginColor: TColor;
begin
  Result := ProgressBar.GaugeBeginColor;
end;

procedure TMFCustomNumEdit.SetGaugeEndColor(V: TColor);
begin
  ProgressBar.GaugeEndColor := V;
end;

function TMFCustomNumEdit.GetGaugeEndColor: TColor;
begin
  Result := ProgressBar.GaugeEndColor;
end;

procedure TMFCustomNumEdit.SetTrackBarWidth(V: Integer);
begin
  FTrackBarWidth := Min(Max(V, 50), 300);
end;

// Added by Boian Mitov >>>>

procedure TMFCustomNumEdit.SetGaugeHeight(V: Integer);
begin
  if FGaugeHeight <> V then
  begin
    FGaugeHeight := V;
    SetBounds(Left, Top, Width, Height);
  end;
end;
// <<<< Added by Boian Mitov

procedure TMFCustomNumEdit.SetPrecision(V: Integer);
begin
  V := Min(Max(V, -1), 6);
  if FPrecision <> V then
  begin
    FPrecision := V;
    SetValue(RealValue);
  end;
end;

procedure TMFCustomNumEdit.SetTrackBarOrientation(Orientation: TTrackBarOrientation);
begin
  if FTrackBarOrientation <> Orientation then
  begin
    FTrackBarOrientation := Orientation;
    SetButtonHandle;
  end;
end;

procedure TMFCustomNumEdit.SetTrackBarEnabled(const Value: Boolean);
begin
  if Value <> FTrackBarEnabled then
  begin
    FTrackBarEnabled := Value;
    FSpeedButton.Visible := Value;
    SetBounds(Left, Top, Width, Height);
    SetEditRect;
    FSpeedButton.Visible := Value;
    FSpeedButton.Refresh;
    Invalidate;
  end;
end;

procedure TMFCustomNumEdit.SetButtonHandle;
begin
  TrackBar.Orientation := TrackBarOrientation;
  if FTrackBarOrientation = trVertical then
  begin
    FSpeedButton.Glyph.Handle := LoadBitmap(HInstance, Glyph_DropLeft);
    Form.Width := FTrackBarHeight;
    Form.Height := FTrackBarWidth;
  end
  else
  begin
    FSpeedButton.Glyph.Handle := LoadBitmap(HInstance, Glyph_DropDown);
    Form.Height := FTrackBarHeight;
    Form.Width := FTrackBarWidth;
  end;
end;

procedure TMFCustomNumEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_UP) then
  begin
    FUpDown.OnClick(Self, btNext);
    Key := 0;
  end
  else
    if (Key = VK_DOWN) then
    begin
      FUpDown.OnClick(Self, btPrev);
      Key := 0;
    end;

  inherited KeyDown(Key, Shift);
end;

procedure TMFCustomNumEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

procedure TMFCustomNumEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TMFCustomNumEdit.Change;
var
  SelS: Integer;
  S: string;
begin
  if FValueUnit <> '' then
  begin
    S := RawValueStr(inherited Text);
    if (S > '') and (S[Length(S)] in ['0'..'9']) then
    begin
      SelS := SelStart;
      SetValue(RealValue);
      SelStart := SelS;
    end;
  end;
  ProgressBar.Repaint;
  if not (csLoading in ComponentState) and Assigned(OnChange) then
    OnChange(Self);
end;

procedure TMFCustomNumEdit.WMSize(var Message: TMessage);
var
  MinHeight: Integer;
begin
  MinHeight := GetMinHeight;
  if Height < MinHeight then
    Height := MinHeight
  else
    if Assigned(FUpDown) and Assigned(FSpeedButton) then
      SetEditRect;
  inherited;
end;

procedure TMFCustomNumEdit.CMEnter(var Message: TMessage);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

procedure TMFCustomNumEdit.CMExit(var Message: TMessage);
begin
  if CheckValue(RealValue) <> RealValue then
    SetValue(CheckValue(RealValue));
  inherited;
end;

procedure TMFCustomNumEdit.WMPaste(var Message: TMessage);
begin
  if not FEditorEnabled or ReadOnly then
    Exit;
  inherited;
end;

procedure TMFCustomNumEdit.WMCut(var Message: TMessage);
begin
  if not FEditorEnabled or ReadOnly then
    Exit;
  inherited;
end;

procedure TMFCustomNumEdit.WMNeedsUpdate(var Message: TMessage);
begin
  FUpDown.Position := 50;
  inherited;
end;

procedure TMFCustomNumEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  FSpW: Integer;
begin
  if Assigned(FPanel) then
  begin
    FSpW := Ord(FSpeedButton.Visible) * (FSpeedButton.Width);
    FPanel.SetBounds(AWidth - 4 - FUpDown.Width - FSpW - 3,
      0,
      FUpDown.Width + FSpW + 3,
      AHeight - 3 - GaugeHeight - 1);
    FUpDown.Height := FPanel.Height - 2;
    FSpeedButton.Height := FPanel.Height - 2;
  end;
  if Assigned(ProgressBar) then
    ProgressBar.SetBounds(0, AHeight - 4 - GaugeHeight - 1, AWidth - 4, GaugeHeight + 2);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

constructor TMFCustomNumEdit.Create(AOwner: TComponent);
var
  Panel: TPanel;
begin
  inherited Create(AOwner);
  FTrackBarEnabled := True;
  FTrackBarOrientation := trHorizontal;
  FTrackBarWidth := 125;
  FMaxValue := 100;
  FMinValue := 0;
  FPrecision := 2;
  FGaugeHeight := 3;
  FGaugeMinValue := 0.0;
  FGaugeMaxValue := 0.0;
  InChanging := False;
  FWrap := False;
  FValueUnit := '';

  FPanel := TPanel.Create(Self);
  with FPanel do
  begin
    Parent := Self;
    BevelOuter := bvNone;
    Color := clBtnFace;
  end;

  ProgressBar := TMFNumEditGauge.Create(Self);
  ProgressBar.Parent := Self;

  FUpDown := TUpDown.Create(FPanel);
  with FUpDown do
  begin
    Left := 1;
    Top := 1;
    Width := 11;
    Height := 17;
    Visible := True;
    Parent := FPanel;
//        Cursor    := crArrow;
    OnClick := UpDownClick;
    Wrap := True;
    Min := 0;
    Max := 100;
    Position := 1;
    Increment := 1;
  end;

  FSpeedButton := TSpeedButton.Create(FPanel);
  with FSpeedButton do
  begin
    Parent := FPanel;
    Top := 1;
    Left := 11 + 1 + 1;
    Visible := True;
    Width := 11;
    NumGlyphs := 1;
    Layout := blGlyphRight;
//        Cursor      := crArrow;
    Invalidate;
    OnMouseDown := SpeedButtonMouseDown;
    OnMouseUp := SpeedButtonMouseUp;
    OnMouseMove := SpeedButtonMouseMove;
  end;

  Form := TMFNumEditPopupForm.CreateNew(Self);
  Form.OnShow := FormShow;

  Panel := TPanel.Create(Form);
  with Panel do
  begin
    Parent := Form;
    BorderWidth := BORDER_WIDTH;
    Align := alClient;
  end;

  TrackBar := TTrackBar.Create(Panel);
  with TrackBar do
  begin
    Parent := Panel;
    Max := 1000;
    Min := 0;
    Frequency := 50;
    TabStop := False;
    FTrackBarHeight := Height - 10;
    Align := alClient;
    ThumbLength := 15;
    TickMarks := tmTopLeft;
  end;

//    Form.BorderStyle := bsNone;
  Form.FormStyle := fsStayOnTop;
  SetButtonHandle;
  Cursor := crDefault;
  Value := 0;
  Increment := 1;

  FNumFormat := nfStandard;
  FDroppedDown := False;
  FLastValue := 0;
end;

destructor TMFCustomNumEdit.Destroy;
begin
  TrackBar.Free;
  Form.Free;
  FSpeedButton.Free;
  FUpDown.Free;
  ProgressBar.Free;
  FPanel.Free;
  inherited Destroy;
end;

procedure TMFCustomNumEdit.CMEnabledChanged(var Message: TWMNoParams);
begin
  inherited;
  FUpDown.Enabled := Enabled;
  FSpeedButton.Enabled := Enabled;
end;

procedure TMFCustomNumEdit.SetNumFormat(V: TNumFormat);
begin
  if FNumFormat <> V then
  begin
    FNumFormat := V;
    SetValue(RealValue);
  end;
end;

function TMFCustomNumEdit.GetTruncatedValue: Double;
begin
  Result := TextToValue(ValueToText(RealValue));
end;

function TMFCustomNumEdit.GetText: string;
begin
  Result := ValueToText(RealValue);
end;

// ****************************************************************************

procedure TMFNumEditGauge.Paint;
var
  PaintTRect: TRect;
  TheImage,
    OverlayImage: TBitmap;
begin
  TheImage := TBitmap.Create;
  OverlayImage := TBitmap.Create;
  TheImage.Height := Height;
  TheImage.Width := Width;
  PaintBackground(TheImage);
  PaintTRect := ClientRect;
  OverlayImage.Width := TheImage.Width;
  OverlayImage.Height := TheImage.Height;
  OverlayImage.Canvas.Brush.Color := TColor(clWindowFrame);
  OverlayImage.Canvas.Brush.Style := bsSolid;
  OverlayImage.Canvas.FillRect(Rect(0, 0, Width, Height));
  PaintBackground(OverlayImage);
  PaintAsBar(OverlayImage, PaintTRect);
  OverlayImage.Canvas.Brush.Color := clBtnFace;
  OverlayImage.Canvas.FrameRect(Rect(0, 0, Width, Height));
  TheImage.Canvas.CopyMode := cmSrcInvert;
  TheImage.Canvas.Draw(0, 0, OverlayImage);
  TheImage.Canvas.CopyMode := cmSrcCopy;
  Canvas.CopyMode := cmSrcCopy;
  Canvas.Draw(0, 0, TheImage);
  OverlayImage.Free;
  TheImage.Free;
end;

procedure TMFNumEditGauge.SetGaugeBeginColor(V: TColor);
begin
  if FGaugeBeginColor <> V then
  begin
    FGaugeBeginColor := V;
    Invalidate;
  end;
end;

procedure TMFNumEditGauge.SetGaugeEndColor(V: TColor);
begin
  if FGaugeEndColor <> V then
  begin
    FGaugeEndColor := V;
    Invalidate;
  end;
end;

procedure TMFNumEditGauge.PaintBackground(var AnImage: TBitmap);
var
  ARect: TRect;
begin
  ARect := Rect(0, 0, Width, Height);
  with AnImage.Canvas do
  begin
    CopyMode := cmBlackness;
    CopyRect(ARect, AnImage.Canvas, ARect);
    CopyMode := cmSrcCopy;
  end;
end;

procedure TMFNumEditGauge.PaintAsBar(var AnImage: TBitmap; const PaintRect: TRect);
var
  CenterPos,
    FillSize: Longint;
  W,
    H: Integer;
  Diff: Double;
  BeginColor: TColor;
  EndColor: TColor;
  Red,
    Green,
    Blue: Byte;
  Correction: Integer;
  ResultColor: TColor;
  LocalPaintRect: TRect;
  EditValue,
    LocalMinValue,
    LocalMaxValue,
    LocalCenterValue: Double; // <- BM, 21.3.2001
  IsCentered,
    DoFillRect: Boolean;
begin
  with PaintRect do
  begin
    W := Right - Left + 1 - 2;
    H := Bottom - Top + 1;
  end;

  with AnImage.Canvas do
  begin
    Brush.Color := FEdit.Color;
    FillRect(PaintRect);
  end;

  LocalMinValue := FEdit.GaugeMinValue;
  LocalMaxValue := FEdit.GaugeMaxValue;
  LocalCenterValue := FEdit.GaugeAroundCenter;
  if (LocalMinValue = 0) and (LocalMaxValue = 0) then
  begin
    LocalMinValue := FEdit.MinValue;
    LocalMaxValue := FEdit.MaxValue;
  end;

  Diff := LocalMaxValue - LocalMinValue;
  if (Diff <= 0) then
    Diff := 100;

  EditValue := FEdit.RealValue;
  FillSize := Min(Round((EditValue - LocalMinValue) * W / Diff + 1.0), W);

  LocalPaintRect := PaintRect;
  LocalPaintRect.Bottom := H;
  if (GaugeAroundCenter > LocalMinValue) and (GaugeAroundCenter < LocalMaxValue) then
  begin
    CenterPos := Min(Round((GaugeAroundCenter - LocalMinValue) * W / Diff + 1.0), W);
    LocalPaintRect.Left := Min(CenterPos, FillSize);
    LocalPaintRect.Right := Max(CenterPos, FillSize);
    DoFillRect := EditValue <> GaugeAroundCenter;
    IsCentered := True;
  end
  else
  begin
    LocalPaintRect.Right := FillSize;
    DoFillRect := FillSize > 0;
    IsCentered := False;
    CenterPos := 0; // only to initialize it
  end;

  if (IsCentered and (LocalCenterValue > LocalMinValue) and (LocalCenterValue < LocalMaxValue)) then // <- BM, 21.3.2001
  begin // <- BM, 21.3.2001
    if (EditValue > LocalCenterValue) then // <- BM, 21.3.2001
      Correction := Round((EditValue - LocalCenterValue) * 256 / (LocalMaxValue - LocalCenterValue) + 1.0)
        // <- BM, 21.3.2001
    else // <- BM, 21.3.2001
      Correction := Round((LocalCenterValue - EditValue) * 256 / (LocalCenterValue - LocalMinValue) + 1.0);
        // <- BM, 21.3.2001
  end // <- BM, 21.3.2001
  else // <- BM, 21.3.2001
    Correction := Round((EditValue - LocalMinValue) * 256 / Diff + 1.0); // <- BM, 21.3.2001

  BeginColor := TColor(ColorToRGB(FGaugeBeginColor));
  EndColor := TColor(ColorToRGB(FGaugeEndColor));

  // ml: fixed byte range error
  Red := Byte(GetRValue(BeginColor) + ((GetRValue(EndColor) - GetRValue(BeginColor)) * Correction div 256));
  Green := Byte(GetGValue(BeginColor) + ((GetGValue(EndColor) - GetGValue(BeginColor)) * Correction div 256));
  Blue := Byte(GetBValue(BeginColor) + ((GetBValue(EndColor) - GetBValue(BeginColor)) * Correction div 256));

  ResultColor := TColor(RGB(Red, Green, Blue));

  with AnImage.Canvas do
  begin
    Pen.Color := ResultColor;
    Pen.Width := 1;
    Brush.Color := ResultColor;
  end;
  if DoFillRect then
    AnImage.Canvas.FillRect(LocalPaintRect);

  if IsCentered then
    with AnImage.Canvas do
    begin
      Pen.Color := clWindowFrame;
      Pen.Style := psSolid;
      MoveTo(CenterPos, LocalPaintRect.Top);
      LineTo(CenterPos, LocalPaintRect.Bottom);
    end;
end;

procedure TMFNumEditGauge.WMLBtnDown(var Message: TWMLButtonDown); // ml: fixed parameter

begin
  MouseCapture := True;
  // ml: type fix
  WMMouseMove(TWMMouseMove(Message));
  FEdit.SetFocus;
  FEdit.SelLength := 0;
  inherited;
end;

procedure TMFNumEditGauge.WMEraseBkg(var Message: TMessage);
begin
  Message.Result := 1; // ml: fix missing return value
end;

procedure TMFNumEditGauge.WMLBtnUp(var Message: TMessage);
begin
  //WMMouseMove(Message);
  MouseCapture := False;
  if FEdit.AutoSelect and not (csLButtonDown in FEdit.ControlState) then
    FEdit.SelectAll;
  inherited;
end;

procedure TMFNumEditGauge.WMMouseMove(var Message: TWMMouseMove);
var
  Pos: SmallInt;
  Diff,
  V: Double;
  LocalMinValue,
  LocalMaxValue: Double;
  
begin
  if (not MouseCapture) then
    Exit;
  Pos := Message.XPos + 1; // ml: bug fix
  LocalMinValue := FEdit.GaugeMinValue;
  LocalMaxValue := FEdit.GaugeMaxValue;
  if (LocalMinValue = 0) and (LocalMaxValue = 0) then
  begin
    LocalMinValue := FEdit.MinValue;
    LocalMaxValue := FEdit.MaxValue;
  end;
  Diff := LocalMaxValue - LocalMinValue;
  if Diff <= 0 then
    Exit;
  if Pos <= 1 then
  begin
    FEdit.Value := LocalMinValue;
    Exit;
  end;
  V := ((Pos * Diff) / (Width - 2)) + LocalMinValue;
  if FIncrement > 0 then
    V := FIncrement * Round(V / FIncrement);
  FEdit.Value := V;
  inherited;
end;

constructor TMFNumEditGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGaugeAroundCenter := 0;
  FGaugeBeginColor := clHighlight;
  FGaugeEndColor := clHighlight;
  FEdit := AOwner as TMFCustomNumEdit;
end;

// ****************************************************************************

procedure TMFNumEditPopupForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style and not (WS_POPUP or WS_CAPTION or WS_SIZEBOX);
  Params.Style := Params.Style or WS_CHILD;
  Params.ExStyle := Params.ExStyle or WS_EX_PALETTEWINDOW;
end;

procedure TMFNumEditPopupForm.CreateWnd;
begin
  inherited;
  Windows.SetParent(Handle, GetDesktopWindow);
end;

// ****************************************************************************

constructor TMFCustomDateTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAlignment := taLeftJustify;
  FBeforeFocusColor := Color;
  FCanvas := nil;
//    FEditStyle          := esCtl3D;
  FFocusedColor := DEF_FOCUSED_COLOR;
  FLineStyle := bsRaised;
  FRequired := False;
  FRequiredColor := DEF_REQUIRED_COLOR;

  Height := 21;
  Width := 101;
end;

destructor TMFCustomDateTimeEdit.Destroy;
begin
  FCanvas.Free;

  inherited Destroy;
end;

procedure TMFCustomDateTimeEdit.CMTextChanged(var Message: TMessage);
begin
  RequiredColorize;
end;

procedure TMFCustomDateTimeEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  case Alignment of
    taLeftJustify:
      Params.Style := Params.Style or (ES_LEFT); //OR ES_MULTILINE );
    taRightJustify:
      Params.Style := Params.Style or (ES_RIGHT); //OR ES_MULTILINE );
    taCenter:
      Params.Style := Params.Style or (ES_CENTER); //OR ES_MULTILINE );
  end;
end;

procedure TMFCustomDateTimeEdit.FocusColorize(GotFocus: Boolean);
begin
  if GotFocus then
  begin
    if Color <> FRequiredColor then
      FBeforeFocusColor := Color;
    if Color <> FFocusedColor then
      Color := FFocusedColor;
  end
  else
  begin
    if FRequired and (Text = '') then
    begin
      if Color <> FRequiredColor then
        Color := FRequiredColor;
    end
    else
    begin
      Color := FBeforeFocusColor;
//            if ( FEditStyle = esUnderline ) then
//                ParentColor := True;
            {else if Color <> clWindow then Color :=  clWindow;}
    end;
  end;
end;

procedure TMFCustomDateTimeEdit.KeyPress(var Key: Char);
begin
  if ksReturn in Keys then
  begin
    if Key = Chr(Vk_Return) then
    begin
      Key := Chr(0);
      (Owner.Owner as TControl).Perform(WM_NEXTDLGCTL, 0, 0);
    end;
  end;
  if ksEscape in Keys then
  begin
    if Key = Chr(Vk_Escape) then
    begin
      Key := Chr(0);
      (Owner.Owner as TControl).Perform(WM_NEXTDLGCTL, 1, 0);
    end;
  end;
  if Key = Chr(Vk_Return) then
    Key := Chr(0);

  inherited KeyPress(Key);
end;

procedure TMFCustomDateTimeEdit.RequiredColorize;
begin
  if FRequired then
  begin
    if (Text = '') then
    begin
      if Color <> FRequiredColor then
        Color := FRequiredColor;
    end
    else
    begin
      Color := FBeforeFocusColor;
//            if FEditStyle = esUnderline then
//                ParentColor := True;
    end;
  end;
end;

procedure TMFCustomDateTimeEdit.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

{
procedure TMFCustomDateTimeEdit.SetEditStyle(Value: TEditStyle);
begin
    if value <> FEditStyle then
    begin
        FEditStyle := Value;
        case Value of
            esUnderline:
            begin
                Ctl3D       := False;
//                BorderStyle := bsNone;
                ParentColor := True;
            end;
            esCtl2D:
            begin
                Ctl3D       := False;
//                BorderStyle := bsSingle;
                ParentColor := False;
                Color       := FBeforeFocusColor;
            end;
            esCtl3D:
            begin
                Ctl3D       := True;
//                BorderStyle := bsSingle;
                ParentColor := False;
                Color       := FBeforeFocusColor;
            end;
        end;
        Invalidate;
    end;
end;
}

procedure TMFCustomDateTimeEdit.SetFRColor(Index: Integer; Value: TColor);
begin
  case Index of
    1:
      if FFocusedColor <> Value then
      begin
        FFocusedColor := Value;
        Invalidate;
        if Focused then
          Color := Value;
      end;
    2:
      if FRequiredColor <> Value then
      begin
        FRequiredColor := Value;
        if (Text = '') and FRequired then
          Color := Value;
      end;
  end;
end;

procedure TMFCustomDateTimeEdit.SetLineStyle(Value: TBevelStyle);
begin
{
    if FEditStyle = esUnderline then
    begin
        if Value <> FLineStyle then
        begin
            FLineStyle := Value;
            Invalidate;
        end;
    end;
}
  Invalidate;
end;

procedure TMFCustomDateTimeEdit.SetRequired(Value: Boolean);
begin
  if FRequired <> Value then
  begin
    FRequired := Value;
    if Value and (Text = '') then
    begin
      if Color <> FRequiredColor then
        Color := FRequiredColor;
    end
    else
      if not Value then
      begin
        if ParentColor then
          Perform(CM_PARENTCOLORCHANGED, 0, 0)
        else
          if Color <> clWindow then
            Color := FBeforeFocusColor;
      end;
  end;
end;

procedure TMFCustomDateTimeEdit.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;
  FocusColorize(False);
end;

procedure TMFCustomDateTimeEdit.WMPaint(var Message: TWMPaint);

procedure BevelLine(C: TColor; X1, Y1, X2, Y2: Integer);
  begin
    with FCanvas do
    begin
      Pen.Color := C;
      MoveTo(X1, Y1);
      LineTo(X2, Y2);
    end;
  end;

  procedure Underlined;
  var
    Color1,
      Color2: TColor;
  begin
    if FCanvas = nil then
    begin
      FCanvas := TControlCanvas.Create;
      FCanvas.Control := Self;
    end;

    with FCanvas do
    begin
      if FLineStyle = bsLowered then
      begin
        Color1 := clBtnShadow;
        Color2 := clBtnHighlight;
      end
      else
      begin
        Color1 := clBtnHighlight;
        Color2 := clBtnShadow;
      end;

      BevelLine(Color1, 0, Height - 2, Width, Height - 2);
      BevelLine(Color2, 0, Height - 1, Width, Height - 1);
    end;
  end;
begin
  inherited;
//    if FEditStyle = esUnderline then
//        Underlined;
end;

procedure TMFCustomDateTimeEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  FocusColorize(True);
end;

// ****************************************************************************

procedure Register;
begin
  RegisterComponents('Misc', [TMFEdit,
    TMFButtonEdit,
      TMFComboEdit,
      TMFNumEdit,
      TMFDateTimeEdit]);
end;

procedure TMFNumEditGauge.WMRBtnDown(var Message: TWMRButtonDown);

// ml: added to cancel tracking.

begin
  MouseCapture := False;
  inherited;
end;

end.

