unit ColorPicker;

//----------------------------------------------------------------------------------------------------------------------
// Version 2.1.0
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
// The original code is ColorPickerButton.pas, released 16. June 1999.
//
// The initial developer of the original code is:
//   Dipl. Ing. Mike Lischke, Delphi Gems software solutions (public@delphi-gems.com, www.delphi-gems.com).
//
// Portions created by Delphi Gems are
//   (C) 1999-2003 Delphi Gems. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// April 2003:
//   - Color swatch widget to load Adobe Photoshop swatch files.
// March 2003:
//   - Complete rework of the entire design.
// June 1999 - August 2002:
//   - Initial release and minor adjustments.
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
  Windows, Sysutils, Messages, Classes, Controls, Graphics, Contnrs, Buttons, ImgList, GraphicTools, ComCtrls,
  {$ifndef COMPILER_7_UP}
    ThemeSrv, // Windows XP themes support for Delphi 5/6. Get these units from www.delphi-gems.com.
  {$else}
    Themes,
  {$endif COMPILE_7_UP}
  ColorTypes, ColorTools;

const
  // Widget notifications.
  WN_COLORPICKER = WM_USER + $1110;
  WN_COLORCHANGE = WN_COLORPICKER + 1;
  WN_HOTCOLORCHANGE = WN_COLORPICKER + 2;

type
  // These redeclarations are necessary to avoid undeclared identifiers when using ColorPicker.pas but not
  // ColorTools.pas.
  TColorComponents = ColorTypes.TColorComponents;
  TColorFormat = ColorTools.TColorFormat;

  TColorPickerOption = (
    cpoAutoSize
  );
  TColorPickerOptions = set of TColorPickerOption;

  TColorPickerBackground = (
    cpbStandard,         // Solid color or parent background, whatever is set by the VCL.
    cpbImage,            // Background image is used.
    cpbColorGradient,    // A four color gradient based on the control's color is used.
    cpbMetallic          // A metallic like gradient (grays) is used.
  );

const
  DefaultColorPickerOptions = [];

type
  TWidgetNotifyReason = (
    wnrHotColor,        // Hot color (the color over which the mouse hovers) has changed.
    wnrSelectedColor,   // A new color has been selected.
    wnrStructure        // Something changed, which requires the color picker layout to be recalculated.
  );

  TWidgetOption = (
    woDropShadow,            // Create a drop shadow underneath the widget content.
    woHotColorAsSelected,    // Use the hot color of another widget in the same color picker as selected color (if it
                             // changes). Requires woTackHotColor to be set.
    woTrackHotColor,         // Adjust the own hot color if the hot color of another widget in the same color picker changes.
                             // Be careful, this option requires frequent repaints with high CPU load.
    woTrackSelectedColor     // Same as woTrackHotColor but for the selected color.
  );
  TWidgetOptions = set of TWidgetOption;

const
  DefaultWidgetOptions = [woDropShadow, woTrackSelectedColor];

type
  // General purpose color info record. This is used with all notifications involving a color.
  PColorInfo = ^TColorInfo;
  TColorInfo = record
    Color: TColorComponents;                // The color components in the native color format.
    Format: TColorFormat;                   // The format used for the components.
    DisplayColor: TColorComponents;         // Color converted to screen format (usually RGB).
    XYZ: TXYZ;                              // Device independant storage of the color (used to convert between
                                            // different color formats).
    Name: WideString;                       // The name of the color, if any.
  end;

  TCustomColorPicker = class;

  TWidgetNotification = record
    Notification: Integer;
    Sender: TCustomColorPicker;
    Data: Pointer;
  end;

  // TBaseColorPickerWidget is a fundamental management unit to be used in a color picker. It does not allow to select
  // a color (this is possible with the next descentant TColorPickerWidget) but allows to create widget like descentants,
  // which are meaningfull to organize the color picker layout.
  TBaseColorPickerWidget = class(TPersistent)
  private
    FColorPicker: TCustomColorPicker;
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FOptions: TWidgetOptions;
    FDropShadow: TDropShadow;
    FBevelEdges: TBevelEdges;
    FBevelInner: TBevelCut;
    FBevelOuter: TBevelCut;
    FBevelKind: TBevelKind;
    FBevelWidth: TBevelWidth;
    FMargin: Integer;
    FCursor: TCursor;
    FTitle: WideString;
    procedure SetBevelCut(const Index: Integer; const Value: TBevelCut);
    procedure SetBevelEdges(const Value: TBevelEdges);
    procedure SetBevelKind(const Value: TBevelKind);
    procedure SetBevelWidth(const Value: TBevelWidth);
    procedure SetColorPicker(const Value: TCustomColorPicker);
    procedure SetDropShadow(const Value: TDropShadow);
    procedure SetHeight(const Value: Integer);
    procedure SetMargin(const Value: Integer);
    procedure SetOptions(const Value: TWidgetOptions);
    procedure SetTitle(const Value: WideString);
    procedure SetWidth(const Value: Integer);

    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetOwner: TPersistent; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; P: TPoint); virtual;
    procedure MouseMove(const P: TPoint); virtual;
    procedure ReadTitle(Reader: TReader);
    procedure SetPosition(X, Y: Integer);
    procedure WriteTitle(Writer: TWriter);

    property BevelEdges: TBevelEdges read FBevelEdges write SetBevelEdges default [];
    property BevelInner: TBevelCut index 0 read FBevelInner write SetBevelCut default bvRaised;
    property BevelKind: TBevelKind read FBevelKind write SetBevelKind default bkNone;
    property BevelOuter: TBevelCut index 1 read FBevelOuter write SetBevelCut default bvLowered;
    property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
    property DropShadow: TDropShadow read FDropShadow write SetDropShadow;
    property Height: Integer read FHeight write SetHeight default 100;
    property Left: Integer read FLeft;
    property Margin: Integer read FMargin write SetMargin default 5;
    property Options: TWidgetOptions read FOptions write SetOptions default DefaultWidgetOptions;
    property Title: WideString read FTitle write SetTitle stored False; // We use own storage methods.
    property Top: Integer read FTop;
    property Width: Integer read FWidth write SetWidth default 100;
  public
    constructor Create(Owner: TControl); virtual;
    destructor Destroy; override;

    function ClientToWidget(const P: TPoint): TPoint; overload;
    function ClientToWidget(const P: TSmallPoint): TPoint; overload;
    function ClientToWidget(const R: TRect): TRect; overload;
    class function GetDescription: string; virtual;
    function GetNamePath: string; override;
    class function GetSummary: string; virtual;
    procedure Invalidate;
    procedure Paint(const Canvas: TCanvas); virtual;
    procedure PaintBevel(const Canvas: TCanvas); virtual;
    procedure PaintShadowed(const Canvas: TCanvas); virtual;
    procedure Perform(Notification: Integer; Sender: TCustomColorPicker; const Parameter);
    class procedure RenderPreview(Canvas: TCanvas; const R: TRect); virtual;
    procedure StructureChanged; virtual;
    function WidgetToClient(const P: TPoint): TPoint;

    property ColorPicker: TCustomColorPicker read FColorPicker write SetColorPicker;
    property Cursor: TCursor read FCursor write FCursor default crDefault;
  end;

  TColorPickerWidgetClass = class of TBaseColorPickerWidget;

  TWidgetList = class(TObjectList)
  private
    FOwner: TCustomColorPicker;
    function GetWidget(Index: Integer): TBaseColorPickerWidget;
    procedure SetWidget(Index: Integer; const Value: TBaseColorPickerWidget);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(Owner: TCustomColorPicker); virtual;

    property Items[Index: Integer]: TBaseColorPickerWidget read GetWidget write SetWidget; default;
  end;

  TColorPickerState = (
    cpsImageDirty,           // Set when the color pickers screen image must be reconstructed.
    cpsInNotify              // The color picker currently handles a notification. Do not handle a second one.
  );
  TColorPickerStates = set of TColorPickerState;

  TCpColorChangeEvent = procedure(Sender: TCustomColorPicker; ScreenColor: COLORREF; const Info: TColorInfo) of object;

  // The custom color picker class is a container for one or more color picker widgets. It takes care for general
  // window handling and forwards important messages to the various widgets.
  TCustomColorPicker = class(TCustomControl)
  private
    FWidgets: TWidgetList;
    FRangeX,
    FRangeY: Integer;
    FOptions: TColorPickerOptions;
    FStates: TColorPickerStates;
    FMargin: Integer;
    FSpacing: Integer;
    FBackground: TColorPickerBackground;
    FUpdateRect: TRect;
    FBackgroundPicture: TPicture;
    FBackBuffer: TBitmap;                        // For flicker free painting.

    // Data regarding the current color.
    FCurrentColorInfo: TColorInfo;
    FScreenColor: COLORREF;

    FOnHotColorChange,
    FOnColorChange: TCpColorChangeEvent;
    FOnStructureChange: TNotifyEvent;
    procedure SetBackground(const Value: TColorPickerBackground);
    procedure SetBackgroundPicture(const Value: TPicture);
    procedure SetMargin(const Value: Integer);
    procedure SetOptions(const Value: TColorPickerOptions);
    procedure SetSelectedColor(const Value: TColorComponents);
    procedure SetWidgets(const Value: TWidgetList);

    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMInvalidate(var Message: TMessage); message CM_INVALIDATE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMPrintClient(var Message: TWMPrintClient); message WM_PRINTCLIENT;
    procedure WMSysColorChange(var Message: TMessage); message WM_SYSCOLORCHANGE;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure AutoAdjustSize; virtual;
    procedure CalculateLayout;
    procedure ColorChange(Widget: TBaseColorPickerWidget);
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoColorChange; virtual;
    procedure DoHotColorChange(Display: COLORREF; const Info: TColorInfo); virtual;
    procedure DoStructureChange; virtual;
    procedure DrawBackground; virtual;
    procedure HotColorChange(Widget: TBaseColorPickerWidget);
    procedure Loaded; override;
    procedure Paint; override;
    procedure ReadWidgets(Reader: TReader);
    procedure StructureChanged;
    procedure TileBackground(DC: HDC);
    procedure UpdateDesigner;
    procedure WriteWidgets(Writer: TWriter);
    procedure WndProc(var Message: TMessage); override;

    property Background: TColorPickerBackground read FBackground write SetBackground default cpbColorGradient;
    property BackgroundPicture: TPicture read FBackgroundPicture write SetBackgroundPicture;
    property ColorFormat: TColorFormat read FCurrentColorInfo.Format;
    property Margin: Integer read FMargin write SetMargin default 0;
    property Options: TColorPickerOptions read FOptions write SetOptions default DefaultColorPickerOptions;
    property SelectedColor: TColorComponents read FCurrentColorInfo.Color write SetSelectedColor;
    property SelectedColorName: WideString read FCurrentColorInfo.Name;

    property OnColorChange: TCpColorChangeEvent read FOnColorChange write FOnColorChange;
    property OnHotColorChange: TCpColorChangeEvent read FOnHotColorChange write FOnHotColorChange;
    property OnStructureChange: TNotifyEvent read FOnStructureChange write FOnStructureChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ColorToString(const Color: TColorComponents): string;
    function StringToColor(const S: string): TColorComponents;
    procedure Notify(Widget: TBaseColorPickerWidget; Reason: TWidgetNotifyReason); virtual;

    property ScreenColor: COLORREF read FScreenColor;
    property States: TColorPickerStates read FStates;
    property Widgets: TWidgetList read FWidgets write SetWidgets;
  end;

  TColorPicker = class(TCustomColorPicker)
  published
    property Align;
    property Anchors;
    property Background;
    property BackgroundPicture;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property Color;
    property ColorFormat;
    property Constraints;
    property Font;
    property Margin;
    property Options;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property SelectedColor;
    property SelectedColorName;
    property Visible;
    property Widgets;

    property OnCanResize;
    property OnClick;
    property OnColorChange;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnHotColorChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStructureChange;
  end;

  // State of the button.
  TCPBState = (
    bsNormal,
    bsHot,
    bsPressed,
    bsDroppedDown
  );

  TCPBIndicatorBorder = (
    ibNone,
    ibFlat,
    ibSunken,
    ibRaised
  );

  // Determines how the button of the drop-down color picker looks like.
  TDDCpButtonStyle = (
    bsCombobox,
    bsFlatButton,
    bsFlatCombobox,
    bsThickButton
  );

  TCPBOption = (
    boColorNameAsCaption,    // Use the name of the current color as button caption.
    boThemeAware,            // Control is Windows XP theme aware and draws itself using the current theme.
    boTransparent            // Control is transparent (background is not touched). Applies only to flat button style.
  );
  TCPBOptions = set of TCPBOption;

const
  DefaultButtonOptions = [boThemeAware];

type
  TColorPickerButton = class;

  TDropDownColorPicker = class(TCustomColorPicker)
  private
    FTransparentColor: Boolean;
    FTransparentColorValue: TColor;
    FAlphaBlend: Boolean;
    FAlphaBlendValue: Byte;
    FUseShadow: Boolean;

    FOnSelectionCancelled: TNotifyEvent;
    procedure SetAlphaBlend(const Value: Boolean);
    procedure SetAlphaBlendValue(const Value: Byte);
    procedure SetTransparentColor(const Value: Boolean);
    procedure SetTransparentColorValue(const Value: TColor);
    procedure SetUseShadow(const Value: Boolean);

    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DoSelectionCancelled; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetLayeredAttributes;
  public
    constructor Create(AOwner: TComponent); override;

    procedure FadeIn(Position: TPoint; Duration: Integer = 200);
    procedure FadeOut(Duration: Integer = 200);
  published
    property AlphaBlend: Boolean read FAlphaBlend write SetAlphaBlend default True;
    property AlphaBlendValue: Byte read FAlphaBlendValue write SetAlphaBlendValue;
    property Background;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property Color;
    property ColorFormat;
    property Font;
    property Left default -1000;
    property Margin;
    property Options;
    property SelectedColor;
    property SelectedColorName;
    property TransparentColor: Boolean read FTransparentColor write SetTransparentColor;
    property TransparentColorValue: TColor read FTransparentColorValue write SetTransparentColorValue;
    property UseShadow: Boolean read FUseShadow write SetUseShadow default True;
    property Widgets;

    property OnColorChange;
    property OnSelectionCancelled: TNotifyEvent read FOnSelectionCancelled write FOnSelectionCancelled;
  end;

  TCPBLayout = (
    blGlyphLeft,
    blGlyphRight,
    blGlyphTop,
    blGlyphBottom
  );

  TDropDownChangeEvent = procedure(Sender: TColorPickerButton; DroppedDown: Boolean) of object;
  TDropDownColorChangeEvent = procedure(Sender: TColorPickerButton; Display: COLORREF; Native: TColorComponents;
    Format: TColorFormat; Name: WideString) of object;

  TColorPickerButton = class(TCustomControl)
  private
    FText: WideString;
    FState: TCPBState;
    FAnimationDuration: Cardinal;                // Specifies how long an animation shall take (drop down).
    FDropDownWidth: Integer;
    FDropDownZone: Boolean;
    FIndicatorBorder: TCPBIndicatorBorder;
    FStyle: TDDCpButtonStyle;
    FOptions: TCPBOptions;
    FPicker: TDropDownColorPicker;

    // Layout properties.
    FMargin: Integer;
    FSpacing: Integer;
    FLayout: TCPBLayout;

    // Image (glyph) properties.
    FImages: TCustomImageList;                   // Images for normal, hot, down and disabled image.
    FImageChangeLink: TChangeLink;               // Connections to the image list to get notified about changes.
    FNormalIndex,
    FHotIndex,
    FDownIndex,
    FDisabledIndex: Integer;

    FOnChange: TNotifyEvent;
    FOnDropDownChange: TDropDownChangeEvent;
    FOnSelectionCancelled: TNotifyEvent;
    FOnColorChange: TDropDownColorChangeEvent;
    function GetDroppedDown: Boolean;
    procedure OnDropDownColorChange(Sender: TCustomColorPicker; Display: COLORREF; const Info: TColorInfo);
    procedure OnDropDownSelectionCancelled(Sender: TObject);
    procedure ReadText(Reader: TReader);
    procedure SetAnimationDuration(const Value: Cardinal);
    procedure SetColorPicker(const Value: TDropDownColorPicker);
    procedure SetDropDownWidth(const Value: Integer);
    procedure SetDroppedDown(Value: Boolean);
    procedure SetImageIndex(Index: Integer; ImageIndex: Integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetIndicatorBorder(const Value: TCPBIndicatorBorder);
    procedure SetLayout(const Value: TCPBLayout);
    procedure SetMargin(const Value: Integer);
    procedure SetOptions(const Value: TCPBOptions);
    procedure SetSpacing(const Value: Integer);
    procedure SetStyle(const Value: TDDCpButtonStyle);
    procedure SetText(const Value: WideString);
    procedure WriteText(Writer: TWriter);

    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CalcButtonLayout(const Client: TRect; const Offset: TPoint; var GlyphPos: TPoint; var TextBounds: TRect);
    procedure Changed; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoColorChange; virtual;
    procedure DoDropDownChange; virtual;
    procedure DoSelectionCancelled; virtual;
    procedure DrawButtonGlyph(const GlyphPos: TPoint);
    procedure DrawButtonText(TextBounds: TRect);
    function DrawInterior(const Client: TRect; const Offset: TPoint): TRect;
    procedure DrawTriangle(Canvas: TCanvas; Top, Left, Width: Integer);
    procedure GetXPThemeDetails(var MainDetails, ButtonDetails: TThemedElementDetails);
    procedure ImageListChange(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
    procedure ToggleDropDown; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;

    property DroppedDown: Boolean read GetDroppedDown write SetDroppedDown default False;
  published
    property Action;
    property Align;
    property Anchors;
    property AnimationDuration: Cardinal read FAnimationDuration write SetAnimationDuration default 200;
    property Color default clBtnFace;
    property Constraints;
    property Ctl3D;
    property Style: TDDCpButtonStyle read FStyle write SetStyle default bsThickButton;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownWidth: Integer read FDropDownWidth write SetDropDownWidth;
    property DropDownColorPicker: TDropDownColorPicker read FPicker write SetColorPicker;
    property Enabled;
    property Font;
    property ImageIndexDisabled: Integer index 0 read FDisabledIndex write SetImageIndex default -1;
    property ImageIndexDown: Integer index 1 read FDownIndex write SetImageIndex default -1;
    property ImageIndexHot: Integer index 2 read FHotIndex write SetImageIndex default -1;
    property ImageIndexNormal: Integer index 3 read FNormalIndex write SetImageIndex default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property IndicatorBorder: TCPBIndicatorBorder read FIndicatorBorder write SetIndicatorBorder default ibFlat;
    property Layout: TCPBLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property Options: TCPBOptions read FOptions write SetOptions default DefaultButtonOptions;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default -1;
    property TabOrder;
    property Text: WideString read FText write SetText stored False; // We use own storage methods.
    property Visible;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnColorChange: TDropDownColorChangeEvent read FOnColorChange write FOnColorChange;
    property OnDblClick;
    property OnDragOver;
    property OnDragDrop;
    property OnDropDownChange: TDropDownChangeEvent read FOnDropDownChange write FOnDropDownChange;
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
    property OnResize;
    property OnSelectionCancelled: TNotifyEvent  read FOnSelectionCancelled write FOnSelectionCancelled;
    property OnStartDock;
    property OnStartDrag;
  end;

  // The base exception class for the color picker.
  EColorPickerException = class(Exception);

procedure ColorPickerError(const S: string); overload;
procedure ColorPickerError(const S: string; Parameters: array of const); overload;
procedure RegisterColorPickerWidget(AClass: TColorPickerWidgetClass);

var
  RegisteredWidgets: TClassList;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Types, Forms, Math, ColorPickerStrings, ColorPickerTypes;

const
  // Do not modify the copyright in any way! Usage of this unit is prohibited without the copyright notice
  // in the compiled binary file.
  Copyright: string = 'Color Picker © 1999-2003 Mike Lischke, Delphi Gems software solutions';

var
  IsWinNT: Boolean; // True for Windows 2000 and up (not Windows NT 4.0 or below!).
  IsWinXP: Boolean; // True if we are on Windows XP or better.
  
//----------------- TBaseColorPickerWidget -----------------------------------------------------------------------------

constructor TBaseColorPickerWidget.Create(Owner: TControl);

begin
  FColorPicker := Owner as TCustomColorPicker;
  FWidth := 100;
  FHeight := 100;
  FOptions := DefaultWidgetOptions;

  FDropShadow := TDropShadow.Create;
  with FDropShadow do
  begin
    Size := 3;
    Offset := 3;
    Direction := -45;
    ShadowAlpha := 1;
    Color := clBtnShadow;
    SourceAlpha := 1;
  end;
  FBevelEdges := [];
  FBevelInner := bvRaised;
  FBevelOuter := bvLowered;
  FBevelWidth := 1;
  FMargin := 5;
  FCursor := crDefault;
  FTitle := '';
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TBaseColorPickerWidget.Destroy;

begin
  FDropShadow.Free;
  if Assigned(FColorPicker) then
    TCustomColorPicker(FColorPicker).Widgets.Remove(Self);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.SetBevelCut(const Index: Integer; const Value: TBevelCut);

begin
  case Index of
    0: // BevelInner
      if Value <> FBevelInner then
      begin
        FBevelInner := Value;
        Invalidate;
      end;
    1: // BevelOuter
      if Value <> FBevelOuter then
      begin
        FBevelOuter := Value;
        Invalidate;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.SetBevelEdges(const Value: TBevelEdges);

begin
  if FBevelEdges <> Value then
  begin
    FBevelEdges := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.SetBevelKind(const Value: TBevelKind);

begin
  if FBevelKind <> Value then
  begin
    FBevelKind := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.SetBevelWidth(const Value: TBevelWidth);

begin
  if FBevelWidth <> Value then
  begin
    FBevelWidth := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.SetColorPicker(const Value: TCustomColorPicker);

begin
  if FColorPicker <> Value then
  begin
    if Assigned(FColorPicker) then
    begin
      FColorPicker.Widgets.Remove(Self);
      FColorPicker.Notify(Self, wnrStructure);
    end;
    FColorPicker := Value;
    if Assigned(FColorPicker) then
    begin
      FColorPicker.Widgets.Add(Self);
      FColorPicker.Notify(Self, wnrStructure);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.SetDropShadow(const Value: TDropShadow);

begin
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.SetHeight(const Value: Integer);

begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    StructureChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.SetMargin(const Value: Integer);

begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.SetOptions(const Value: TWidgetOptions);

begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.SetTitle(const Value: WideString);

begin
  if FTitle <> Value then
  begin
    FTitle := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.SetWidth(const Value: Integer);

begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    StructureChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.WMLButtonDown(var Message: TWMLButtonDown);

var
  P: TPoint;

begin
  // Convert picker coordinates to widget coordinates and handle the message.
  P := ClientToWidget(Message.Pos);
  MouseDown(mbLeft, KeysToShiftState(Message.Keys), P);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.WMMouseMove(var Message: TWMMouseMove);

var
  P: TPoint;

begin
  // Convert picker coordinates to widget coordinates and handle the message.
  P := ClientToWidget(Message.Pos);
  MouseMove(P);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.WMSetCursor(var Message: TWMSetCursor);

begin
  // Apply own cursors only if there is no global cursor set.
  if Screen.Cursor = crDefault then
  begin
    Windows.SetCursor(Screen.Cursors[FCursor]);
    Message.Result := 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.DefineProperties(Filer: TFiler);

begin
  inherited;

  Filer.DefineProperty('WidgetTitle', ReadTitle, WriteTitle, True);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseColorPickerWidget.GetOwner: TPersistent;

begin
  Result := FColorPicker;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.MouseDown(Button: TMouseButton; Shift: TShiftState; P: TPoint);

begin
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.MouseMove(const P: TPoint);

begin
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.ReadTitle(Reader: TReader);

begin
  case Reader.NextValue of
    vaLString, vaString:
      FTitle := Reader.ReadString;
  else
    FTitle := Reader.ReadWideString;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.SetPosition(X, Y: Integer);

// This method is used internally and should not be used to manually place a widget.

begin
  FLeft := X;
  FTop := Y;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.WriteTitle(Writer: TWriter);

begin
  Writer.WriteWideString(FTitle);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseColorPickerWidget.ClientToWidget(const P: TPoint): TPoint;

// Converts client coordinates of the color picker control into widget local coordinates.

begin
  Result.X := P.X - FLeft;
  Result.Y := P.Y - FTop;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseColorPickerWidget.ClientToWidget(const P: TSmallPoint): TPoint;

// Converts client coordinates of the color picker control into widget local coordinates.

begin
  Result.X := P.X - FLeft;
  Result.Y := P.Y - FTop;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseColorPickerWidget.ClientToWidget(const R: TRect): TRect;

// Converts client coordinates of the color picker control into widget local coordinates.

begin
  Result.Left := R.Left - FLeft;
  Result.Top := R.Top - FTop;
  Result.Right := R.Right - FLeft;
  Result.Bottom := R.Bottom - FTop;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TBaseColorPickerWidget.GetDescription: string;

begin
  Result := '';
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseColorPickerWidget.GetNamePath: string;

// Returns a string describing the widget and its index in the owner color picker.
// Used for the object inspector in the IDE.

var
  I: Integer;

begin
  if Assigned(FColorPicker) then
  begin
    I := TCustomColorPicker(FColorPicker).Widgets.IndexOf(Self);
    Result := Format('%s.Widgets[%d]', [FColorPicker.Name, I]);
  end
  else
    Result := ClassName;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TBaseColorPickerWidget.GetSummary: string;

begin
  Result := '';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.Invalidate;

begin
  if Assigned(FColorPicker) then
    FColorPicker.Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.Paint(const Canvas: TCanvas);

begin
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.PaintBevel(const Canvas: TCanvas);

const
  InnerStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENINNER, BDR_RAISEDINNER, 0);
  OuterStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENOUTER, BDR_RAISEDOUTER, 0);
  EdgeStyles: array[TBevelKind] of Integer = (0, 0, BF_SOFT, BF_FLAT);

var
  EdgeSize: Integer;
  R: TRect;
  
begin
  if FBevelKind <> bkNone then
  begin
    EdgeSize := 0;
    if FBevelInner <> bvNone then
      Inc(EdgeSize, FBevelWidth);
    if FBevelOuter <> bvNone then
      Inc(EdgeSize, FBevelWidth);
    R := Rect(0, 0, FWidth, FHeight);
    with R do
    begin
      if beLeft in BevelEdges then
        Inc(Left, EdgeSize);
      if beTop in BevelEdges then
        Inc(Top, EdgeSize);
      if beRight in BevelEdges then
        Dec(Right, EdgeSize);
      if beBottom in BevelEdges then
        Dec(Bottom, EdgeSize);
    end;
    DrawEdge(Canvas.Handle, R, InnerStyles[BevelInner] or OuterStyles[BevelOuter], Byte(BevelEdges) or
      EdgeStyles[BevelKind] or BF_ADJUST);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.PaintShadowed(const Canvas: TCanvas);

var
  Buffer: TBitmap;
  R: TRect;

begin
  Buffer := TBitmap.Create;
  try
    with Buffer do
    begin
      PixelFormat := pf32Bit;
      Height := FHeight;
      Width := FWidth;
      R := Rect(0, 0, Width, Height);
    end;
    FDropShadow.InitializeBitmap(Buffer);
    //SetWindowOrgEx(Buffer.Canvas.Handle, FLeft, FTop, nil);
    Paint(Buffer.Canvas);
    //SetWindowOrgEx(Buffer.Canvas.Handle, 0, 0, nil);
    FDropShadow.Draw(Buffer, R, Canvas.Handle, Point(0, 0));
  finally
    Buffer.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.Perform(Notification: Integer; Sender: TCustomColorPicker; const Parameter);

// Triggers the appropriate notification handler.

var
  Data: TWidgetNotification;
  
begin
  Data.Notification := Notification;
  Data.Sender := Sender;
  Data.Data := @Parameter;
  Dispatch(Data);
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TBaseColorPickerWidget.RenderPreview(Canvas: TCanvas; const R: TRect);

begin
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseColorPickerWidget.StructureChanged;

begin
  if Assigned(FColorPicker) then
    TCustomColorPicker(FColorPicker).Notify(Self, wnrStructure);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseColorPickerWidget.WidgetToClient(const P: TPoint): TPoint;

// Converts coordinates from local widget space to color picker space.

begin
  Result.X := P.X + FLeft;
  Result.Y := P.Y + FTop;
end;

//----------------- TWidgetList ----------------------------------------------------------------------------------------

constructor TWidgetList.Create(Owner: TCustomColorPicker);

begin
  inherited;
  
  FOwner := Owner;
end;

//----------------------------------------------------------------------------------------------------------------------

function TWidgetList.GetWidget(Index: Integer): TBaseColorPickerWidget;

begin
  Result := TBaseColorPickerWidget(inherited Items[Index]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetList.SetWidget(Index: Integer; const Value: TBaseColorPickerWidget);

begin
  inherited Items[Index] := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetList.Notify(Ptr: Pointer; Action: TListNotification);

begin
  case Action of
    lnAdded:
      TBaseColorPickerWidget(Ptr).ColorPicker := FOwner;
    lnExtracted,
    lnDeleted:
      TBaseColorPickerWidget(Ptr).ColorPicker := nil;
  end;

  inherited Notify(Ptr, Action);
end;

//----------------- TCustomColorPicker ----------------------------------------------------------------------------------------

constructor TCustomColorPicker.Create(AOwner: TComponent);

begin
  inherited;

  FWidgets := TWidgetList.Create(Self);
  FBackground := cpbColorGradient;
  FSpacing := 4;
  FBackgroundPicture := TPicture.Create;
  FBackBuffer := TBitmap.Create;
  FBackBuffer.PixelFormat := pf32Bit;
  FStates := [cpsImageDirty];

  CalculateLayout;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TCustomColorPicker.Destroy;

begin
  // The widget collection owns all widgets and frees them implicitly.
  // Don't use FreeAndNil for the widgets. Internally first the reference is set to nil and then the
  // collection is freed. This causes an access violation when the widgets want to remove themselves
  // from the collection.
  FWidgets.Free;
  FWidgets := nil;
  FreeAndNil(FBackgroundPicture);
  FreeAndNil(FBackBuffer);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.SetBackground(const Value: TColorPickerBackground);

begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.SetBackgroundPicture(const Value: TPicture);

begin
  FBackgroundPicture.Assign(Value);
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.SetMargin(const Value: Integer);

begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    StructureChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.SetOptions(const Value: TColorPickerOptions);

var
  ToSet,
  ToClear: TColorPickerOptions;
  
begin
  if FOptions <> Value then
  begin
    ToSet := Value - FOptions;
    ToClear := FOptions - Value;

    FOptions := Value;
    if cpoAutoSize in ToSet then
      AutoAdjustSize;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.SetSelectedColor(const Value: TColorComponents);

begin
  
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.SetWidgets(const Value: TWidgetList);

begin
  FWidgets.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.CMHintShow(var Message: TMessage);

begin
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.CMInvalidate(var Message: TMessage);

begin
  inherited;

  Include(FStates, cpsImageDirty);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.WMEraseBkgnd(var Message: TWMEraseBkgnd);

begin
  Message.Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.WMKillFocus(var Message: TWMKillFocus);

begin
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.WMPaint(var Message: TWMPaint);

begin
  if csPaintCopy in ControlState then
    FUpdateRect := ClientRect
  else
    GetUpdateRect(Handle, FUpdateRect, False);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.WMPrintClient(var Message: TWMPrintClient);

begin
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.WMSysColorChange(var Message: TMessage);

var
  I: Integer;

begin
  for I := 0 to FWidgets.Count - 1 do
    if FWidgets[I] is TColorPickerWidget then
      TColorPickerWidget(FWidgets[I]).SysColorChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.AlignControls(AControl: TControl; var Rect: TRect);

begin
  inherited;

  // Adjust back bitmap to fill the full client area.
  // Usually method Resize would be natural for this task however it is not called
  // when the control is being loaded from the resource. AlignControls is called in any case.
  if HandleAllocated then
  begin
    Include(FStates, cpsImageDirty);

    // Reset one dimension to avoid copying the content of the bitmap.
    FBackBuffer.Width := 0;
    FBackBuffer.Height := ClientHeight;
    FBackBuffer.Width := ClientWidth;

    CalculateLayout;
    Update;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.AutoAdjustSize;

begin
  if FWidgets.Count > 0 then
    ;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.CalculateLayout;

var
  I,
  PreviousIndex,
  Bottom: Integer;
  X, Y: Integer;
  LineStack: TStack;

begin
  FRangeX := Width;
  if HandleAllocated and (FWidgets.Count > 0) then
  begin
    // Start with the left upper corner. Continue to the right and start a new line if the next widget
    // would not fit entirely into the remaining space (except it is the only widget on this row).
    // If a widget from a previous row reaches below the bottom line of the current widget when starting a
    // new row then start the row at that large widget, not x position 0.
    X := FMargin;
    Y := FMargin;
    // The line stack serves as lookup source for the current row.
    LineStack := TStack.Create;
    try
      // Initialize position calculation by setting the very first widget.
      FWidgets[0].SetPosition(X, Y);
      Inc(X, FWidgets[0].Width);
      FRangeY := FWidgets[0].Height;
      LineStack.Push(Pointer(0));
      for I := 1 to FWidgets.Count - 1 do
      begin
        // Check the current position together with the widget's width.
        // If it does not fit anymore on the current row then start a new line.
        if (X + FWidgets[I].Width) > (Width - FMargin) then
        begin
          // Start just below the previous widget and search backwards until the FMargin position is reached
          // or we find a previous widget, whose bottom line is below than that of the current widget.
          PreviousIndex := Integer(LineStack.Pop);
          repeat
            X := FWidgets[PreviousIndex].Left;
            Y := FWidgets[PreviousIndex].Top + FWidgets[PreviousIndex].Height;
            // If we reached the left border then no further steps are possible. Hence stop the loop.
            if X <= FMargin then
              Break;

            // See if the bottom line of the previous widget is equal or below the current one with the new
            // coordinates.
            PreviousIndex := Integer(LineStack.Peek);
            Bottom := FWidgets[PreviousIndex].Top + FWidgets[PreviousIndex].Height;
            if Bottom >= Y + FWidgets[I].Height then
              // If we found another widget, which is large enough to reach below the current widget
              // then use this as the new left border.
              Break;
            // This one before the previous widget is not large enough. Start over looking for the one before it.
            LineStack.Pop;
          until False;
        end;

        // Keep the index of the current widget for later lookup.
        LineStack.Push(Pointer(I));
        // Finally set the new position.
        FWidgets[I].SetPosition(X, Y);
        Inc(X, FWidgets[I].Width);
        if Y + FWidgets[I].Height > FRangeY then
          FRangeY := Y + FWidgets[I].Height;
      end;
    finally
      LineStack.Free;
    end;
  end
  else
  begin
    FRangeX := 0;
    FRangeY := 0;
  end;

  if cpoAutoSize in FOptions then
    AutoAdjustSize;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.ColorChange(Widget: TBaseColorPickerWidget);

var
  I: Integer;
  CpWidget: TColorPickerWidget;

begin
  Assert((Widget = nil) or (Widget is TColorPickerWidget), 'ColorChange: Invalid parameter.');

  if Assigned(Widget) then
  begin
    // If Widget is assigned then this is a notification from this particular widget and
    // we have to update our internal storage. Otherwise this info has already been set.
    CpWidget := Widget as TColorPickerWidget;
    FCurrentColorInfo := CpWidget.SelectedColorInfo;
    // Compute a color reference for quick display. It is assumed here that the display works with RGB.
    // I think we can safely assume for a whole while that computer screens will work with RGB.
    if Assigned(FCurrentColorInfo.DisplayColor) then
      FScreenColor := MakeColorRef(MakeRGB(FCurrentColorInfo.DisplayColor))
    else
      FScreenColor := ColorToRGB(clNone);
  end;
  for I := 0 to FWidgets.Count - 1 do
    if FWidgets[I] <> Widget then
      FWidgets[I].Perform(WN_COLORCHANGE, Self, FCurrentColorInfo);

  DoColorChange;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.DefineProperties(Filer: TFiler);

begin
  inherited;
  Filer.DefineProperty('WidgetCollection', ReadWidgets, WriteWidgets, FWidgets.Count > 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.DoColorChange;

begin
  if Assigned(FOnColorChange) then
    FOnColorChange(Self, FScreenColor, FCurrentColorInfo);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.DoHotColorChange(Display: COLORREF; const Info: TColorInfo);

begin
  if Assigned(FOnHotColorChange) then
    FOnHotColorChange(Self, Display, Info);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.DoStructureChange;

begin
  if Assigned(FOnStructureChange) then
    FOnStructureChange(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.DrawBackground;

var
  LUColor,
  RUColor,
  RLColor,
  LLColor: TRGB;
  HasImage: Boolean;

  //--------------- local function --------------------------------------------

  procedure DefaultBackground;

  begin
    if ThemeServices.ThemesEnabled and Assigned(Parent) and (csParentBackground in ControlStyle) then
      ThemeServices.DrawParentBackground(Handle, FBackBuffer.Canvas.Handle, nil, False)
    else
      with FBackBuffer.Canvas do
      begin
        Brush.Color := Self.Color;
        FillRect(Rect(0, 0, Width, Height));
      end;
  end;

  //--------------- end local fucntion ----------------------------------------

begin 
  if FBackBuffer.Height = 0 then
    Resize;
  case FBackground of
    cpbStandard: // Solid color or parent background.
      DefaultBackground;
    cpbImage:
      begin
        HasImage := Assigned(FBackgroundPicture.Graphic) and not FBackgroundPicture.Graphic.Empty;
        // If there is no background image then take standard path.
        if not HasImage or FBackgroundPicture.Graphic.Transparent then
          DefaultBackground;
        if HasImage then
          TileBackground(FBackBuffer.Canvas.Handle);
      end;
    cpbColorGradient:
      begin
        LUColor := MakeRGB(clWhite);
        RUColor := MakeRGB(Color);
        RLColor := DarkenColor(RUColor, 0.05);
        MakeSafeColor(RLColor);
        LLColor := BrightenColor(RUColor, 0.05);
        MakeSafeColor(LLColor);
        DrawGradientBox(FBackBuffer.Canvas.Handle, ClientRect, [LUColor, RUColor, RLColor, LLColor]);
      end;
    cpbMetallic:
      begin
        LUColor := MakeRGB(clWhite);
        RUColor := MakeRGB(clLtGray);
        RLColor := DarkenColor(RUColor, 0.1);
        LLColor := BrightenColor(RUColor, 0.1);
        DrawGradientBox(FBackBuffer.Canvas.Handle, ClientRect, [LUColor, RUColor, RLColor, LLColor]);
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.HotColorChange(Widget: TBaseColorPickerWidget);

var
  I: Integer;
  CpWidget: TColorPickerWidget;

begin
  Assert(Widget is TColorPickerWidget, 'HotColorChange: Invalid parameter.');

  CpWidget := Widget as TColorPickerWidget;
  for I := 0 to FWidgets.Count - 1 do
    if FWidgets[I] <> Widget then
      FWidgets[I].Perform(WN_HOTCOLORCHANGE, Self, CpWidget.HotColorInfo);

  DoHotColorChange(CpWidget.ConvertToColorRef(CpWidget.HotColorInfo.DisplayColor), CpWidget.HotColorInfo);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.Loaded;

begin
  inherited;

  CalculateLayout;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.Paint;

var
  I: Integer;
  R: TRect;
  ClipRegion: HRGN;

begin
  if cpsImageDirty in FStates then
  begin
    Exclude(FStates, cpsImageDirty);
    FBackBuffer.Canvas.Lock;
    if FWidgets.Count > 0 then
    begin
      // Background is drawn to the back buffer. For tabbed widgets this will become the background of a single
      // widget placed on its tabsheet. Otherwise it is the background of the entire control.
      DrawBackground;

      // The position and size for each widget has been set in CalculateLayout.
      for I := 0 to FWidgets.Count - 1 do
        with FWidgets[I] do
        begin
          R := Rect(Left, Top, Left + Width, Top + Height);
          if RectVisible(Canvas.Handle, R) then
          begin
            ClipRegion := CreateRectRgnIndirect(R);
            SelectClipRgn(FBackBuffer.Canvas.Handle, ClipRegion);
            DeleteObject(ClipRegion);
            SetWindowOrgEx(FBackBuffer.Canvas.Handle, -Left, -Top, nil);
            if woDropShadow in Options then
              PaintShadowed(FBackBuffer.Canvas)
            else
              Paint(FBackBuffer.Canvas);
            PaintBevel(FBackBuffer.Canvas);
          end;
        end;
      SetWindowOrgEx(FBackBuffer.Canvas.Handle, 0, 0, nil);
    end;
    FBackBuffer.Canvas.Unlock;
  end;

  Canvas.Draw(0, 0, FBackBuffer);
end;

//----------------------------------------------------------------------------------------------------------------------

type
  TReaderCast = class(TReader); // Enabled access to protected methods.

procedure TCustomColorPicker.ReadWidgets(Reader: TReader);

var
  I: Integer;
  S: string;
  Widget: TBaseColorPickerWidget;
  ClassFound: Boolean;

begin
  FWidgets.Clear;

  with TReaderCast(Reader) do
  begin
    // Skip vaCollection indicator.
    ReadValue;
    while not EndOfList do
    begin
      ReadListBegin;
      // Determine class to create. Skip 'WidgetClass' property name.
      ReadStr;
      S := ReadString;
      ClassFound := False;
      for I := 0 to RegisteredWidgets.Count - 1 do
        if RegisteredWidgets[I].ClassName = S then
        begin
          ClassFound := True;
          Widget := TColorPickerWidgetClass(RegisteredWidgets[I]).Create(Self);
          while not EndOfList do
            ReadProperty(Widget);
          FWidgets.Add(Widget);
          Break;
        end;

      if not ClassFound then
        ColorPickerError('Color picker widget class ''%s'' not found.'#13#10 +
          'Is the widget unit included in the uses clause?', [S]);
      ReadListEnd;
    end;
    ReadListEnd;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.StructureChanged;

begin
  if not (csReading in ComponentState) and not (csDestroying in ComponentState) then
  begin
    CalculateLayout;
    Invalidate;

    DoStructureChange;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.TileBackground(DC: HDC);

// Draws the given source graphic so that it tiles into the given rectangle which is relative to the target bitmap.
// The graphic is aligned so that it always starts at the upper left corner of the target canvas.
// R specifies
var
  TargetX,
  TargetY: Integer;
  Height,
  Width: Integer;

begin
  with TCanvas.Create do
  try
    Handle := DC;
    TargetY := 0;
    Height := ClientHeight;
    Width := ClientWidth;

    // Tile image vertically until target rectangle is filled.
    while TargetY < Height do
    begin
      TargetX := 0;

      // Tile the image horizontally.
      while TargetX < Width do
      begin
        Draw(TargetX, TargetY, FBackgroundPicture.Graphic);
        Inc(TargetX, FBackgroundPicture.Width);
      end;
      Inc(TargetY, FBackgroundPicture.Height);
    end;
  finally
    Handle := 0;
    Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.UpdateDesigner;

// Used to let the form designer know if something changed. This is using an interface so no design time code is
// included at runtime.

var
  ParentForm: TCustomForm;

begin
  if (csDesigning in ComponentState) and not (csUpdating in ComponentState) then
  begin
    ParentForm := GetParentForm(Self);
    if Assigned(ParentForm) and Assigned(ParentForm.Designer) then
      ParentForm.Designer.Modified;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  TWriterCast = class(TWriter); // Enable access to protected methods.

  // --- HACK WARNING!
  // This second type cast is a partial rewrite of the private section of TWriter. The purpose is to have access to
  // the FPropPath member, which is otherwise not accessible. The reason why this access is needed is that
  // with nested components this member contains unneeded property path information. These information prevent
  // successful load of the stored properties later.
  // In Classes.pas you can see that FPropPath is reset several times to '' to prevent this case for certain properies.
  // Unfortunately, there is no clean way for us here to do the same.
  {$Hints off}
  TWriterHack = class(TFiler)
  private
    FRootAncestor: TComponent;
    FPropPath: string;
  end;
  {$Hints on}

procedure TCustomColorPicker.WriteWidgets(Writer: TWriter);

var
  I: Integer;
  OldAncestor: TPersistent;
  LastPropPath: string;

begin
  with TWriterCast(Writer) do
  begin
    OldAncestor := Ancestor;
    Ancestor := nil;

    // Save last property path for restoration.
    LastPropPath := TWriterHack(Writer).FPropPath;
    // Reset property path to prevent output like
    //
    //    DropDownColorPicker.WidgetCollection = <
    //      item
    //        DropDownColorPicker.WidgetClass = 'TDelphiColorsCpWidget'
    //      end>
    //
    // instead of
    //
    //    DropDownColorPicker.WidgetCollection = <
    //      item
    //        WidgetClass = 'TDelphiColorsCpWidget'
    //      end>
    TWriterHack(Writer).FPropPath := '';
    
    try
      WriteValue(vaCollection);
      for I := 0 to FWidgets.Count - 1 do
      begin
        WriteListBegin;
        WritePropName('WidgetClass');
        WriteString(FWidgets[I].ClassName);
        WriteProperties(FWidgets[I]);
        WriteListEnd;
      end;
      WriteListEnd;
    finally
      // Finally restore old property path and ancestor.
      TWriterHack(Writer).FPropPath := LastPropPath;
      Ancestor := OldAncestor;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.WndProc(var Message: TMessage);

// Passes all incoming mouse messages to all widgets. A 1 in the message structure result indicates that a widget
// has handled the message. In this case no further dispatching is done.
// If the message is not handled by any widget then it is forwarded to the usual dispatch mechanism.

var
  P: TPoint;

  //--------------- local function ---------------------------------------------

  function CallWidget: Boolean;

  var
    I: Integer;

  begin
    Result := False;
    for I := 0 to FWidgets.Count - 1 do
    begin
      with FWidgets[I] do
      begin
        if PtInRect(Rect(Left, Top, Left + Width, Top + Height), P) then
        begin
          FWidgets[I].Dispatch(Message);
          Result := Message.Result <> 0;
        end;
      end;
      if Result then
        Break;
    end;
  end;

  //--------------- end local function -----------------------------------------

begin
  if not (csDesigning in ComponentState) then
  begin
    case Message.Msg of
      WM_MOUSEFIRST..WM_MOUSELAST:
        begin
          P := SmallPointToPoint(TWMMouse(Message).Pos);
          if not CallWidget then
            inherited;
        end;
      WM_SETCURSOR:
        begin
          GetCursorPos(P);
          P := ScreenToClient(P);
          if not CallWidget then
            inherited;
        end;
    else
      inherited;
    end;
  end
  else
    inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomColorPicker.ColorToString(const Color: TColorComponents): string;

// Converts the given color to a string, e.g. for debugging, design time support etc.
// The format is: "(%5.2f, %5.2f, ...)".

var
  I: Integer;
  OldSeparator: Char;

begin
  Result := '';
  if Length(Color) > 0 then
  begin
    OldSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    try
      for I := 0 to High(Color) do
        if I = High(Color) then
          Result := Format('%s%.2f', [Result, Color[I]])
        else
          Result := Format('%s%.2f, ', [Result, Color[I]]);
      Result := '(' + Result + ')';
    finally
      DecimalSeparator := OldSeparator;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomColorPicker.StringToColor(const S: string): TColorComponents;

// Converts the given string to a set of color components.
// The required format is: "(%f, %f, ...)".

var
  Entry: string;
  Head,
  Tail: PChar;
  OldSeparator: Char;

begin
  if S = '' then
    Result := nil
  else
  begin
    if (S[1] <> '(') or (S[Length(S)] <> ')') then
      ColorPickerError(SColor2StringInvalidFormat);

    OldSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    try
      Head := PChar(S) + 1;
      while Head^ <> ')' do
      begin
        // Start with the next entry.
        Tail := Head;
        // Look for its end, which is determined by a new entry (indicated by the comma) or the closing parenthesis.
        while not (Tail^ in [',', ')']) do
          Inc(Tail);

        if Tail - Head > 0 then
        begin
          SetString(Entry, Head, Tail - Head);
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := StrToFloat(Entry);
        end;
        // Skip comma and any following spaces.
        if Tail^ = ',' then
          Inc(Tail);
        while Tail^ = ' ' do
          Inc(Tail);
        Head := Tail;
      end;
    finally
      DecimalSeparator := OldSeparator;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomColorPicker.Notify(Widget: TBaseColorPickerWidget; Reason: TWidgetNotifyReason);

// Called whenever something changed in the given widget.

begin
  if not (cpsInNotify in FStates) and not (csLoading in ComponentState) then
  begin
    Include(FStates, cpsInNotify);
    try
      case Reason of
        wnrHotColor: // Hot color (the color over which the mouse hovers) has changed.
          HotColorChange(Widget as TColorPickerWidget);
        wnrSelectedColor: // A new color has been selected.
          ColorChange(Widget as TColorPickerWidget);
        wnrStructure: // Something changed, which requires the color picker structure to be updated.
          StructureChanged;
      end;
      UpdateDesigner;
    finally
      Exclude(FStates, cpsInNotify);
    end;
  end;
end;

//----------------- TColorPickerButton -------------------------------------------------------------------------------

constructor TColorPickerButton.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);

  ControlStyle := [csClickEvents, csSetCaption, csDoubleClicks, csReflector];
  Width := 150;
  Height := 25;
  TabStop := False;
  Color := clBtnFace;
  ParentColor := False;
  FAnimationDuration := 200;
  FDropDownWidth := GetSystemMetrics(SM_CXVSCROLL);
  FIndicatorBorder := ibFlat;
  FStyle := bsThickButton;
  FLayout := blGlyphLeft;
  FNormalIndex := -1;
  FHotIndex := -1;
  FDownIndex := -1;
  FDisabledIndex := -1;
  FOptions := DefaultButtonOptions;
  FMargin := -1;
  FSpacing := -1;

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FPicker := TDropDownColorPicker.Create(Self);
  with FPicker do
  begin
    ParentWindow := GetDesktopWindow;
    Visible := False; // Has no effect at design time.
    ShowWindow(Handle, SW_HIDE); // This one works.
    Left := -1000;
    Name := 'ColorPicker';
    Width := 100;
    Height := 200;
    OnSelectionCancelled := OnDropDownSelectionCancelled;
    OnColorChange := OnDropDownColorChange;
  end;
  Tabstop := False;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TColorPickerButton.Destroy;

begin
  FImageChangeLink.Free;
  
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorPickerButton.GetDroppedDown: Boolean;

begin
  Result := FState = bsDroppedDown;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.OnDropDownColorChange(Sender: TCustomColorPicker; Display: COLORREF;
  const Info: TColorInfo);

begin
  if boColorNameAsCaption in FOptions then
    FText := Info.Name;
  DroppedDown := False;
  Invalidate;
  DoColorChange;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.OnDropDownSelectionCancelled(Sender: TObject);

begin
  DroppedDown := False;
  DoSelectionCancelled;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.ReadText(Reader: TReader);

begin
  case Reader.NextValue of
    vaLString, vaString:
      FText := Reader.ReadString;
  else
    FText := Reader.ReadWideString;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.SetAnimationDuration(const Value: Cardinal);

begin
  FAnimationDuration := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.SetColorPicker(const Value: TDropDownColorPicker);

begin

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.SetDropDownWidth(const Value: Integer);

begin
  if FDropDownWidth <> Value then
  begin
    FDropDownWidth := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.SetDroppedDown(Value: Boolean);

begin
  if (FState = bsDroppedDown) <> Value then
  begin
    if Value then
      FState := bsDroppedDown
    else
      FState := bsNormal;
    ToggleDropDown;
    Invalidate;
    DoDropDownChange;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.SetImageIndex(Index, ImageIndex: Integer);

begin
  case Index of
    0:
      FDisabledIndex := ImageIndex;
    1:
      FDownIndex := ImageIndex;
    2:
      FHotIndex := ImageIndex;
    3:
      FNormalIndex := ImageIndex;
  end;
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.SetImages(const Value: TCustomImageList);

begin
  if FImages <> Value then
  begin
    if Assigned(FImages) then
    begin
      FImages.UnRegisterChanges(FImageChangeLink);
      FImages.RemoveFreeNotification(Self);
    end;
    FImages := Value;
    if Assigned(FImages) then
    begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(Self);
    end;
    if not (csLoading in ComponentState) then
      Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.SetIndicatorBorder(const Value: TCPBIndicatorBorder);

begin
  if FIndicatorBorder <> Value then
  begin
    FIndicatorBorder := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.SetLayout(const Value: TCPBLayout);

begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.SetMargin(const Value: Integer);

begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.SetOptions(const Value: TCPBOptions);

begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    if boTransparent in FOptions then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.SetSpacing(const Value: Integer);

begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.SetStyle(const Value: TDDCpButtonStyle);

begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.SetText(const Value: WideString);

begin
  if Value <> FText then
  begin
    FText := Value;
    Invalidate;
    Changed;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.WriteText(Writer: TWriter);

begin
  Writer.WriteWideString(FText);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.CMEnabledChanged(var Message: TMessage);

begin
  inherited;

  DroppedDown := False;
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.CMMouseEnter(var Message: TMessage);

begin
  if FState = bsNormal then
  begin
    FState := bsHot;
    Invalidate;
  end;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.CMMouseLeave(var Message: TMessage);

begin
  if FState = bsHot then
  begin
    FState := bsNormal;
    Invalidate;
  end;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.WMCancelMode(var Message: TWMCancelMode);

begin
  DroppedDown := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.WMEraseBkgnd(var Message: TWMEraseBkgnd);

begin
  Message.Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.CalcButtonLayout(const Client: TRect; const Offset: TPoint; var GlyphPos: TPoint;
  var TextBounds: TRect);

var
  ClientSize,
  GlyphSize,
  TextPos: TPoint;
  TextSize: TSize;
  TotalSize: TPoint;
  ALayout: TCPBLayout;
  DC: HDC;
  ASpacing,
  AMargin: Integer;

begin
  DC := Canvas.Handle;

  // These values might be changed during the computation so save them.
  ALayout := FLayout;
  ASpacing := FSpacing;
  AMargin := FMargin;
  
  if UseRightToLeftReading then
    if ALayout = blGlyphLeft then
      ALayout := blGlyphRight
    else
      if ALayout = blGlyphRight then
        ALayout := blGlyphLeft;

  // calculate the item sizes
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom - Client.Top);

  if Assigned(FImages) then
    GlyphSize := Point(FImages.Width, FImages.Height)
  else
    GlyphSize := Point(0, 0);

  if Length(FText) > 0 then
  begin
    GetTextExtentPoint32W(DC, PWideChar(FText), Length(FText), TextSize);
    TextBounds := Rect(0, 0, TextSize.cx, TextSize.cy);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize.cx := 0;
    TextSize.cy := 0;
  end;

  // If the layout has the glyph on the right or the left then both the
  // text and the glyph are centered vertically. If the glyph is on the top
  // or the bottom then both the text and the glyph are centered horizontally.
  if ALayout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.cy + 1) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.cx + 1) div 2;
  end;

  // If there is no text or no bitmap then spacing is irrelevant.
  if (TextSize.cx = 0) or (GlyphSize.X = 0) then
    ASpacing := 0;

  // Adjust margin and spacing.
  if AMargin = -1 then
  begin
    if ASpacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.cx, GlyphSize.Y + TextSize.cy);
      if ALayout in [blGlyphLeft, blGlyphRight] then
        AMargin := (ClientSize.X - TotalSize.X) div 3
      else
        AMargin := (ClientSize.Y - TotalSize.Y) div 3;
      ASpacing := AMargin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + ASpacing + TextSize.cx, GlyphSize.Y + ASpacing + TextSize.cy);
      if ALayout in [blGlyphLeft, blGlyphRight] then
        AMargin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        AMargin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if ASpacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (AMargin + GlyphSize.X), ClientSize.Y - (AMargin + GlyphSize.Y));
      if ALayout in [blGlyphLeft, blGlyphRight] then
        ASpacing := (TotalSize.X - TextSize.cx) div 2
      else
        ASpacing := (TotalSize.Y - TextSize.cy) div 2;
    end;
  end;

  case ALayout of
    blGlyphLeft:
      begin
        GlyphPos.X := AMargin;
        TextPos.X := GlyphPos.X + GlyphSize.X + ASpacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - AMargin - GlyphSize.X;
        TextPos.X := GlyphPos.X - ASpacing - TextSize.cx;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := AMargin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + ASpacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - AMargin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - ASpacing - TextSize.cy;
      end;
  end;

  // Fixup the result variables.
  with GlyphPos do
  begin
    Inc(X, Client.Left + Offset.X);
    Inc(Y, Client.Top + Offset.Y);
  end;
  OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X, TextPos.Y + Client.Top + Offset.Y);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.Changed;

begin
  inherited;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.CreateParams(var Params: TCreateParams);

begin
  inherited CreateParams(Params);

  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;

    WindowClass.Style := WindowClass.Style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.DefineProperties(Filer: TFiler);

begin
  inherited;

  Filer.DefineProperty('WideText', ReadText, WriteText, FText <> '');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.DoColorChange;

begin
  if Assigned(FOnColorChange) then
    FOnColorChange(Self, FPicker.ScreenColor, FPicker.SelectedColor, FPicker.ColorFormat, FPicker.SelectedColorName);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.DoDropDownChange;

begin
  if Assigned(FOnDropDownChange) then
    FOnDropDownChange(Self, FState = bsDroppedDown);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.DoSelectionCancelled;

begin
  if Assigned(FOnSelectionCancelled) then
    FOnSelectionCancelled(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.DrawButtonGlyph(const GlyphPos: TPoint);

var
  Index: Integer;

begin
  if Assigned(FImages) then
  begin
    if not Enabled then
      Index := FDisabledIndex
    else
      case FState of
        bsNormal:
          Index := FNormalIndex;
        bsHot:
          Index := FHotIndex;
        bsPressed,
        bsDroppedDown:
          Index := FDownIndex;
      else
        Index := -1;
      end;

    // Disabled images are not considered here. The implicit conversion to disabled state does not produce nice
    // looking results. So if you want a disabled image then construct one manually and add it to the image list.
    if Index > -1 then
      FImages.Draw(Canvas, GlyphPos.X, GlyphPos.Y, Index, True);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.DrawButtonText(TextBounds: TRect);

var
  X, Y: Integer;
  Options: Integer;
  MainDetails,
  ButtonDetails: TThemedElementDetails;

begin
  if ThemeServices.ThemesAvailable and (boThemeAware in FOptions) then
  begin
    // Draw with XP themes.
    GetXPThemeDetails(MainDetails, ButtonDetails);
    ThemeServices.DrawText(Canvas.Handle, MainDetails, FText, TextBounds, 0, 0);
  end
  else
    with Canvas do
    begin
      Brush.Style := bsClear;
      // Calculate center point for alignment.
      X := TextBounds.Left;
      Y := TextBounds.Top;
      Options := ETO_CLIPPED;
      if UseRightToLeftReading then
        Options := Options or ETO_RTLREADING;
      if not Enabled then
      begin
        Inc(X);
        Inc(Y);
        Font.Color := clBtnHighlight;
        ExtTextOutW(Handle, X, Y, Options, @TextBounds, PWideChar(FText), Length(FText), nil);
        Dec(X);
        Dec(Y);
        Font.Color := clBtnShadow;
      end;

      ExtTextOutW(Handle, X, Y, Options, @TextBounds, PWideChar(FText), Length(FText), nil);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorPickerButton.DrawInterior(const Client: TRect; const Offset: TPoint): TRect;

// Draws everything in the button. This includes text, color bar and glyph.

var
  GlyphPos: TPoint;
  R: TRect;

begin
  CalcButtonLayout(Client, Offset, GlyphPos, R);
  DrawButtonGlyph(GlyphPos);

  // Return a rectangle wherein the color indicator can be drawn.
  if FText = '' then
  begin
    Result := Client;
    InflateRect(Result, -Margin - 2, - Margin -2);

    // Consider glyph if no text is to be painted (else it is already taken into account).
    if Assigned(FImages) then
      case Layout of
        blGlyphLeft:
          Result.Left := GlyphPos.X + FImages.Width + 4;
        blGlyphRight:
          Result.Right := GlyphPos.X - 4;
        blGlyphTop:
          Result.Top := GlyphPos.Y + FImages.Height + 4;
        blGlyphBottom:
          Result.Bottom := GlyphPos.Y - 4;
      end;
  end
  else
  begin
    OffsetRect(R, 0, -4);
    DrawButtonText(R);

    // consider caption
    Result := Rect(R.Left, R.Bottom, R.Right, R.Bottom + 6);
    if (Result.Bottom + 2) > Client.Bottom then
      Result.Bottom := Client.Bottom - 2;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.DrawTriangle(Canvas: TCanvas; Top, Left, Width: Integer);

begin
  if Odd(Width) then
    Inc(Width);
  Canvas.Polygon([Point(Left, Top), Point(Left + Width, Top), Point(Left + Width div 2, Top + Width div 2)]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.GetXPThemeDetails(var MainDetails, ButtonDetails: TThemedElementDetails);

// For Windows XP theme support we have to determine what element to use for drawing depending on factors like the
// button style and enabled state.

begin
  case FStyle of
    bsFlatCombobox,
    bsCombobox:
      begin
        MainDetails := ThemeServices.GetElementDetails(tcComboBoxRoot);

        if not Enabled then
          ButtonDetails := ThemeServices.GetElementDetails(tcDropDownButtonDisabled)
        else
          case FState of
            bsHot:
              ButtonDetails := ThemeServices.GetElementDetails(tcDropDownButtonHot);
            bsPressed:
              ButtonDetails := ThemeServices.GetElementDetails(tcDropDownButtonPressed);
            bsDroppedDown,
            bsNormal:
              ButtonDetails := ThemeServices.GetElementDetails(tcDropDownButtonNormal);
          end;
      end;
    bsFlatButton:
      begin
        if not Enabled then
        begin
          MainDetails := ThemeServices.GetElementDetails(ttbSplitButtonDisabled);
          ButtonDetails := ThemeServices.GetElementDetails(ttbSplitButtonDropDownDisabled);
        end
        else
          if csDesigning in ComponentState then
          begin
            MainDetails := ThemeServices.GetElementDetails(ttbSplitButtonHot);
            ButtonDetails := ThemeServices.GetElementDetails(ttbSplitButtonDropDownHot);
          end
          else
            case FState of
              bsHot:
                begin
                  MainDetails := ThemeServices.GetElementDetails(ttbSplitButtonHot);
                  ButtonDetails := ThemeServices.GetElementDetails(ttbSplitButtonDropDownHot);
                end;
              bsPressed:
                begin
                  MainDetails := ThemeServices.GetElementDetails(ttbSplitButtonPressed);
                  ButtonDetails := ThemeServices.GetElementDetails(ttbSplitButtonDropDownPressed);
                end;
              bsDroppedDown:
                begin
                  MainDetails := ThemeServices.GetElementDetails(ttbSplitButtonHot);
                  ButtonDetails := ThemeServices.GetElementDetails(ttbSplitButtonDropDownPressed);
                end;
              bsNormal:
                begin
                  MainDetails := ThemeServices.GetElementDetails(ttbSplitButtonNormal);
                  ButtonDetails := ThemeServices.GetElementDetails(ttbSplitButtonDropDownNormal);
                end;
            end;
      end;
    bsThickButton:
      begin
        if not Enabled then
          MainDetails := ThemeServices.GetElementDetails(tbPushButtonDisabled)
        else
          case FState of
            bsHot:
              begin
                MainDetails := ThemeServices.GetElementDetails(tbPushButtonHot);
                ButtonDetails := ThemeServices.GetElementDetails(tbPushButtonHot);
              end;
            bsPressed:
              begin
                MainDetails := ThemeServices.GetElementDetails(tbPushButtonPressed);
                ButtonDetails := ThemeServices.GetElementDetails(tbPushButtonPressed);
              end;
            bsDroppedDown:
              begin
                MainDetails := ThemeServices.GetElementDetails(tbPushButtonHot);
                ButtonDetails := ThemeServices.GetElementDetails(tbPushButtonPressed);
              end;
            bsNormal:
              begin
                MainDetails := ThemeServices.GetElementDetails(tbPushButtonNormal);
                ButtonDetails := ThemeServices.GetElementDetails(tbPushButtonNormal);
              end;
          end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.ImageListChange(Sender: TObject);

begin
  if not (csDestroying in ComponentState) then
    Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  inherited;

  if Button = mbLeft then
  begin
    if DroppedDown then
    begin
      DroppedDown := False;
      FState := bsHot;
    end
    else
    begin
      FState := bsPressed;
      MouseCapture := True;
      if FDropDownZone or (FStyle in [bsCombobox, bsFlatCombobox]) then
        DroppedDown := True;
    end;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.MouseMove(Shift: TShiftState; X, Y: Integer);

begin
  inherited;

  FDropDownZone := X >= (Clientwidth - FDropDownWidth);
  if Enabled and (FState = bsNormal) then
  begin
    FState := bsHot;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  inherited;

  if Button = mbLeft then
  begin
    MouseCapture := False;
    if not FDropDownZone then
    begin
      FState := bsNormal;
      Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.Resize;

begin
  inherited;

  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.ToggleDropDown;

var
  DropPosition: TPoint;

begin
  // Animation is only possible if we are on Windows XP and neither alpha blending nor color key transparency is used.
  if FState = bsDroppedDown then
  begin
    // By default drop down direction is below the button. But when there is not enough space then use the room
    // above the button.
    DropPosition := Parent.ClientToScreen(Point(Left, Top));
    if DropPosition.Y + Height + FPicker.Height > GetSystemMetrics(SM_CYSCREEN) then
      // Not enough space below the button. Use upper direction.
      Dec(DropPosition.Y, FPicker.Height)
    else
      Inc(DropPosition.Y, Height);
    FPicker.FadeIn(DropPosition, FAnimationDuration);
    SetCapture(FPicker.Handle);
  end
  else
  begin
    ReleaseCapture;
    FPicker.FadeOut(FAnimationDuration);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerButton.Paint;

const
  TriangleWidth = 5;
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);

var
  FullRect,
  MainRect,
  ButtonRect,
  R: TRect;
  ButtonFlags: Integer;
  EdgeFlags: Integer;
  Offset: TPoint;
  LeftPos: Integer;

  Pressed: Boolean;
  Transparent: Boolean;
  DrawArrow: Boolean;

  Themed: Boolean;
  MainDetails,
  ButtonDetails: TThemedElementDetails;

begin
  DrawArrow := True;
  Themed := ThemeServices.ThemesAvailable and (boThemeAware in FOptions);
  with Canvas do
  begin
    Font := Self.Font;
    // Creates a rectangle that represent the button and the drop down button area,
    // determines also the position to draw the arrow.
    FullRect := ClientRect;
    MainRect := FullRect;
    Dec(MainRect.Right, FDropDownWidth);
    ButtonRect := FullRect;
    ButtonRect.Left := MainRect.Right;

    // Shortcut for certain states and options.
    Pressed := FState = bsPressed;
    Transparent := (boTransparent in FOptions) and (FStyle = bsFlatButton);

    if Pressed and not Themed then
    begin
      Offset.X := 1;
      Offset.Y := 1;
    end
    else
    begin
      Offset.X := 0;
      Offset.Y := 0;
    end;

    ButtonFlags := DFCS_BUTTONPUSH;

    if Pressed or DroppedDown then
      ButtonFlags := ButtonFlags or DFCS_PUSHED;

    if Themed then
    begin
      // Draw with XP themes.
      GetXPThemeDetails(MainDetails, ButtonDetails);
      ThemeServices.DrawParentBackground(Self.Handle, Handle, @MainDetails, True, nil);
      ThemeServices.DrawParentBackground(Self.Handle, Handle, @ButtonDetails, True, nil);
      if FStyle in [bsCombobox, bsFlatCombobox] then
      begin
        InflateRect(ButtonRect, -1, -1);
        ThemeServices.DrawElement(Handle, MainDetails, FullRect)
      end
      else
        ThemeServices.DrawElement(Handle, MainDetails, MainRect);

      ThemeServices.DrawElement(Handle, ButtonDetails, ButtonRect);

      // Except for the thick button style all theme elements include the down arrow already.
      DrawArrow := FStyle = bsThickButton;
    end
    else
      case FStyle of
        bsThickButton:
          begin
            if FState = bsDroppedDown then
            begin
              // Color zone depressed, drop down zone pressed.
              DrawFrameControl(Handle, FullRect, DFC_BUTTON, ButtonFlags and not DFCS_PUSHED);
              DrawFrameControl(Handle, ButtonRect, DFC_BUTTON, ButtonFlags);
            end
            else
            begin
              DrawFrameControl(Handle, FullRect, DFC_BUTTON, ButtonFlags);
              // Paint separator line.
              InflateRect(ButtonRect, 0, -2);
              OffsetRect(ButtonRect, Offset.X, 0);
              DrawEdge(Handle, ButtonRect, EDGE_ETCHED, BF_LEFT);
              Dec(ButtonRect.Left);
            end;
          end;
        bsFlatButton:
          begin
            if (FState <> bsNormal) or (csDesigning in ComponentState) then
            begin
              if FState = bsDroppedDown then
              begin
                DrawEdge(Handle, FullRect, DownStyles[False], FillStyles[Transparent] or BF_RECT);
                DrawEdge(Handle, ButtonRect, DownStyles[True], FillStyles[Transparent] or BF_RECT);
              end
              else
              begin
                DrawEdge(Handle, FullRect, DownStyles[Pressed], FillStyles[Transparent] or BF_RECT);
                // Paint separator line.
                OffsetRect(ButtonRect, Offset.X, 0);
                DrawEdge(Handle, ButtonRect, EDGE_ETCHED, BF_LEFT);
              end;
            end
            else
              if not Transparent then
              begin
                Brush.Style := bsSolid;
                Brush.Color := Color;
                FillRect(FullRect);
              end;
          end;
        bsCombobox,
        bsFlatCombobox:
          begin
            // Draw button like a list style combobox.
            EdgeFlags := BF_RECT or BF_ADJUST;
            if FStyle = bsFlatCombobox then
            begin
              ButtonFlags := ButtonFlags or DFCS_FLAT;
              EdgeFlags := EdgeFlags or BF_FLAT;
            end;
            Brush.Style := bsSolid;
            Brush.Color := Color;
            FillRect(FullRect);

            R := FullRect;
            DrawEdge(Handle, R, EDGE_SUNKEN, EdgeFlags);
            R.Left := ClientWidth - FDropDownWidth;
            DrawFrameControl(Handle, R, DFC_BUTTON, ButtonFlags);

            if DroppedDown then
              OffsetRect(ButtonRect, Offset.X - 1, Offset.Y)
            else
              OffsetRect(ButtonRect, Offset.X - 2, Offset.Y);
          end;
      end;

    R := ClientRect;
    Dec(R.Right, FDropDownWidth);
    // If themed drawing is enabled then reselect the font handle into our canvas. It was changed by the
    // theme draw code behind our back (the culprit is DrawParentBackground).
    if Themed then
      SelectObject(Canvas.Handle, Self.Font.Handle);
    // Draw interior of the control. This is independant of the button style.
    FullRect := DrawInterior(R, Offset);

    // Cause the current brush to recreate its handle as the themed drawing changes it behind our backs.
    if Themed then
      Brush.Handle := 0;
    Brush.Color := FPicker.ScreenColor;
    Pen.Color := clBtnShadow;

    case FIndicatorBorder of
      ibNone:
        FillRect(FullRect);
      ibFlat:
        Rectangle(FullRect);
    else
      if FIndicatorBorder = ibSunken then
        DrawEdge(Handle, FullRect, BDR_SUNKENOUTER, BF_RECT)
      else
        DrawEdge(Handle, FullRect, BDR_RAISEDINNER, BF_RECT);
      InflateRect(FullRect, -1, -1);
      FillRect(FullRect);
    end;

    // Draw the arrow in the correct state if not yet done (XP theming).
    if DrawArrow then
    begin
      if not Enabled then
      begin
        Pen.Color := clBtnShadow;
        Brush.Color := clBtnShadow;
      end
      else
      begin
        Pen.Color := clBlack;
        Brush.Color := clBlack;
      end;

      LeftPos := ButtonRect.Left + (FDropDownWidth - TriangleWidth) div 2;
      if FState = bsDroppedDown then
        DrawTriangle(Canvas, (Height div 2)  + 1, LeftPos, TriangleWidth)
      else
        DrawTriangle(Canvas, (Height div 2) + Offset.Y, LeftPos, TriangleWidth);
    end;

  end;
end;

//----------------- Common functions -----------------------------------------------------------------------------------

function CompareWidgets(Item1, Item2: Pointer): Integer;

begin
  Result := CompareText(TColorPickerWidgetClass(Item1).GetSummary, TColorPickerWidgetClass(Item2).GetSummary);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ColorPickerError(const S: string);

begin
  raise EColorPickerException.Create(S);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ColorPickerError(const S: string; Parameters: array of const);

begin
  raise EColorPickerException.CreateFmt(S, Parameters);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure RegisterColorPickerWidget(AClass: TColorPickerWidgetClass);

begin
  if RegisteredWidgets = nil then
      RegisteredWidgets := TClassList.Create;

  if RegisteredWidgets.IndexOf(AClass) = -1 then
  begin
    RegisteredWidgets.Add(AClass);
    RegisteredWidgets.Sort(CompareWidgets);
  end;
  RegisterClass(AClass);
end;

//----------------- TDropDownColorPicker -------------------------------------------------------------------------------

constructor TDropDownColorPicker.Create(AOwner: TComponent);

begin
  inherited;

  FUseShadow := True;
  Include(FComponentStyle, csSubComponent);
  ControlStyle := ControlStyle - [csCaptureMouse];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropDownColorPicker.SetAlphaBlend(const Value: Boolean);

begin
  if FAlphaBlend <> Value then
  begin
    FAlphaBlend := Value;
    SetLayeredAttributes;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropDownColorPicker.SetAlphaBlendValue(const Value: Byte);

begin
  if FAlphaBlendValue <> Value then
  begin
    FAlphaBlendValue := Value;
    SetLayeredAttributes;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropDownColorPicker.SetTransparentColor(const Value: Boolean);

begin
  if FTransparentColor <> Value then
  begin
    FTransparentColor := Value;
    SetLayeredAttributes;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropDownColorPicker.SetTransparentColorValue(const Value: TColor);

begin
  if FTransparentColorValue <> Value then
  begin
    FTransparentColorValue := Value;
    SetLayeredAttributes;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropDownColorPicker.SetUseShadow(const Value: Boolean);

begin
  if FUseShadow <> Value then
  begin
    FUseShadow := Value;
    if HandleAllocated then
      DestroyHandle;;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropDownColorPicker.WMCaptureChanged(var Message: TMessage);

begin
  DoSelectionCancelled;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropDownColorPicker.CreateParams(var Params: TCreateParams);

const
  CS_DROPSHADOW = $00020000;

begin
  inherited CreateParams(Params);

  with Params do
  begin
    Style := WS_POPUP;
    ExStyle := ExStyle or WS_EX_TOOLWINDOW or WS_EX_TOPMOST or WS_EX_NOPARENTNOTIFY or WS_EX_LAYERED;

    // Since the drop shadow style is a class style we have to ensure a new class is registered by TWinControl
    // when changing this style. The simplest way is to specify different window class names.
    if FUseShadow then
    begin
      // There is at most room for 64 characters (including terminating 0).
      StrPLCopy(WinClassName, ClassName + 'Shadow', 63);
      if IsWinXP then
        with WindowClass do
          Style := Style or CS_DROPSHADOW;
    end
    else
      StrPLCopy(WinClassName, ClassName + 'NoShadow', 63);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropDownColorPicker.CreateWindowHandle(const Params: TCreateParams);

begin
  inherited;

  SetLayeredAttributes;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropDownColorPicker.DoSelectionCancelled;

begin
  if Assigned(FOnSelectionCancelled) then
    FOnSelectionCancelled(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropDownColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  if PtInRect(ClientRect, Point(X, Y)) then
    inherited
  else
    DoSelectionCancelled;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropDownColorPicker.SetLayeredAttributes;

const
  cUseAlpha: array [Boolean] of Integer = (0, LWA_ALPHA);
  cUseColorKey: array [Boolean] of Integer = (0, LWA_COLORKEY);

var
  AStyle: Integer;

begin
  if Assigned(SetLayeredWindowAttributes) and HandleAllocated then
  begin
    AStyle := GetWindowLong(Handle, GWL_EXSTYLE);
    if FAlphaBlend or FTransparentColor then
    begin
      if (AStyle and WS_EX_LAYERED) = 0 then
        SetWindowLong(Handle, GWL_EXSTYLE, AStyle or WS_EX_LAYERED);
      SetLayeredWindowAttributes(Handle, ColorToRGB(FTransparentColorValue), FAlphaBlendValue, cUseAlpha[FAlphaBlend] or
        cUseColorKey[FTransparentColor]);
    end
    else
    begin
      SetWindowLong(Handle, GWL_EXSTYLE, AStyle and not WS_EX_LAYERED);
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_ALLCHILDREN);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropDownColorPicker.FadeIn(Position: TPoint; Duration: Integer);

// Shows the color picker window by fading it. Position gives the place of the upper left corner and Duration
// determines how long shold the animation take (in milliseconds). The default value is 200 ms.
// In case animation is not supported (or disabled by using alpha blending or transparency) the window is just
// shown unanimated.

var
  CanAnimate: Boolean;

begin
  // Animation is only possible if we are on Windows XP and neither alpha blending nor color key transparency is used.
  CanAnimate := IsWinXP and (Duration > 0) and not FAlphaBlend and not FTransparentColor;
  SetWindowPos(Handle, 0, Position.X, Position.Y, Width, Height, SWP_NOACTIVATE);
  if CanAnimate then
    AnimateWindow(Handle, Duration, AW_BLEND)
  else
    ShowWindow(Handle, SW_SHOWNOACTIVATE);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropDownColorPicker.FadeOut(Duration: Integer);

// Like FadeIn but it hides the window.

var
  CanAnimate: Boolean;

begin
  // Animation is only possible if we are on Windows XP and neither alpha blending nor color key transparency is used.
  CanAnimate := IsWinXP and (Duration > 0) and not FAlphaBlend and not FTransparentColor;
  if CanAnimate then
    AnimateWindow(Handle, Duration, AW_BLEND or AW_HIDE)
  else
    ShowWindow(Handle, SW_HIDE);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure InitializeGlobalData;

begin
  // Determine platform for certain features.
  // Fade animation is possible on Win2K and up.
  IsWinNT := (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5);
  // Shadows are possible with Windows XP and up.
  IsWinXP := IsWinNT and (Win32MinorVersion >= 1);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure FinalizeGlobalData;

begin
  RegisteredWidgets.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  InitializeGlobalData;
finalization
  FinalizeGlobalData;
end.


