unit VirtualTrees.Columns;

interface

uses
  System.Classes,
  System.Types,
  System.UITypes,
  WinApi.Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Menus,
  Vcl.Themes,
  Vcl.GraphUtil,
  VirtualTrees.Constants,
  VirtualTrees.Types,
  VirtualTrees.Options;

{$MINENUMSIZE 1, make enumerations as small as possible}


type
  // Options per column.
  TVTColumnOption = (
    coAllowClick,            // Column can be clicked (must be enabled too).
    coDraggable,             // Column can be dragged.
    coEnabled,               // Column is enabled.
    coParentBidiMode,        // Column uses the parent's bidi mode.
    coParentColor,           // Column uses the parent's background color.
    coResizable,             // Column can be resized.
    coShowDropMark,          // Column shows the drop mark if it is currently the drop target.
    coVisible,               // Column is shown.
    coAutoSpring,            // Column takes part in the auto spring feature of the header (must be resizable too).
    coFixed,                 // Column is fixed and can not be selected or scrolled etc.
    coSmartResize,           // Column is resized to its largest entry which is in view (instead of its largest
                             // visible entry).
    coAllowFocus,            // Column can be focused.
    coDisableAnimatedResize, // Column resizing is not animated.
    coWrapCaption,           // Caption could be wrapped across several header lines to fit columns width.
    coUseCaptionAlignment,   // Column's caption has its own aligment.
    coEditable,              // Column can be edited
    coStyleColor             // Prefer background color of VCL style over TVirtualTreeColumn.Color
    );
  TVTColumnOptions = set of TVTColumnOption;

  TVirtualTreeColumnStyle = (
    vsText,
    vsOwnerDraw
    );

  TVTHeaderColumnLayout = (
    blGlyphLeft,
    blGlyphRight,
    blGlyphTop,
    blGlyphBottom
    );

  TSortDirection = (
    sdAscending,
    sdDescending
    );

  TSortDirectionHelper = record helper for VirtualTrees.Columns.TSortDirection
  strict private
  const
    cSortDirectionToInt : Array [TSortDirection] of Integer = (1, - 1);
  public
    /// Returns +1 for ascending and -1 for descending sort order.
    function ToInt() : Integer; inline;
  end;

  TCheckType = (
    ctNone,
    ctTriStateCheckBox,
    ctCheckBox,
    ctRadioButton,
    ctButton
    );

  // The check states include both, transient and fluent (temporary) states. The only temporary state defined so
  // far is the pressed state.
  TCheckState = (
    csUncheckedNormal,   // unchecked and not pressed
    csUncheckedPressed,  // unchecked and pressed
    csCheckedNormal,     // checked and not pressed
    csCheckedPressed,    // checked and pressed
    csMixedNormal,       // 3-state check box and not pressed
    csMixedPressed,      // 3-state check box and pressed
    csUncheckedDisabled, // disabled checkbox, not checkable
    csCheckedDisabled,   // disabled checkbox, not uncheckable
    csMixedDisabled      // disabled 3-state checkbox
    );

  /// Adds some convenience methods to type TCheckState
  TCheckStateHelper = record helper for TCheckState
  strict private
  const
    // Lookup to quickly convert a specific check state into its pressed counterpart and vice versa.
    cPressedState : array [TCheckState] of TCheckState   = (
      csUncheckedPressed, csUncheckedPressed, csCheckedPressed, csCheckedPressed, csMixedPressed, csMixedPressed, csUncheckedDisabled, csCheckedDisabled, csMixedDisabled);
    cUnpressedState : array [TCheckState] of TCheckState = (
      csUncheckedNormal, csUncheckedNormal, csCheckedNormal, csCheckedNormal, csMixedNormal, csMixedNormal, csUncheckedDisabled, csCheckedDisabled, csMixedDisabled);
    cEnabledState : array [TCheckState] of TCheckState   = (
      csUncheckedNormal, csUncheckedPressed, csCheckedNormal, csCheckedPressed, csMixedNormal, csMixedPressed, csUncheckedNormal, csCheckedNormal, csMixedNormal);
    cToggledState : array [TCheckState] of TCheckState   = (
      csCheckedNormal, csCheckedPressed, csUncheckedNormal, csUncheckedPressed, csCheckedNormal, csCheckedPressed, csUncheckedDisabled, csCheckedDisabled, csMixedDisabled);
  public
    function GetPressed() : TCheckState; inline;
    function GetUnpressed() : TCheckState; inline;
    function GetEnabled() : TCheckState; inline;
    function GetToggled() : TCheckState; inline;
    function IsDisabled() : Boolean; inline;
    function IsChecked() : Boolean; inline;
    function IsUnChecked() : Boolean; inline;
    function IsMixed() : Boolean; inline;
  end;

// Used during owner draw of the header to indicate which drop mark for the column must be drawn.
  TVTDropMarkMode = (
    dmmNone,
    dmmLeft,
    dmmRight
    );

  // auto scroll directions
  TScrollDirections = set of TScrollDirection;
//    sdLeft,
//    sdUp,
//    sdRight,
//    sdDown
//  );

const
  DefaultColumnOptions = [coAllowClick, coDraggable, coEnabled, coParentColor, coParentBidiMode, coResizable,
    coShowDropMark, coVisible, coAllowFocus, coEditable, coStyleColor];

type
  TVirtualTreeColumn = class;

  // This structure carries all important information about header painting and is used in the advanced header painting.
  THeaderPaintInfo = record
    TargetCanvas : TCanvas;
    Column : TVirtualTreeColumn;
    PaintRectangle : TRect;
    TextRectangle : TRect;
    IsHoverIndex,
      IsDownIndex,
      IsEnabled,
      ShowHeaderGlyph,
      ShowSortGlyph,
      ShowRightBorder : Boolean;
    DropMark : TVTDropMarkMode;
    GlyphPos,
      SortGlyphPos : TPoint;
    SortGlyphSize : TSize;
    procedure DrawSortArrow(pDirection : TSortDirection);
    procedure DrawDropMark();
  end;

  TVirtualTreeColumns = class;

  TVirtualTreeColumn = class(TCollectionItem)
  private
    const
    cDefaultColumnSpacing = 3;
  private
    FText,
      FHint               : string;
    FWidth                : TDimension;
    FPosition             : TColumnPosition;
    FMinWidth             : TDimension;
    FMaxWidth             : TDimension;
    FStyle                : TVirtualTreeColumnStyle;
    FImageIndex           : TImageIndex;
    FBiDiMode             : TBiDiMode;
    FLayout               : TVTHeaderColumnLayout;
    FMargin,
      FSpacing            : TDimension;
    FOptions              : TVTColumnOptions;
    FEditOptions          : TVTEditOptions;
    FEditNextColumn       : TDimension;
    FTag                  : NativeInt;
    FAlignment            : TAlignment;
    FCaptionAlignment     : TAlignment; // Alignment of the caption.
    FLastWidth            : TDimension;
    FColor                : TColor;
    FBonusPixel           : Boolean;
    FSpringRest           : Single; // Accumulator for width adjustment when auto spring option is enabled.
    FCaptionText          : string;
    FCheckBox             : Boolean;
    FCheckType            : TCheckType;
    FCheckState           : TCheckState;
    FImageRect            : TRect;
    FHasImage             : Boolean;
    FDefaultSortDirection : TSortDirection;
    function GetCaptionAlignment : TAlignment;
    function GetCaptionWidth : TDimension;
    function GetLeft : TDimension;
    function IsBiDiModeStored : Boolean;
    function IsCaptionAlignmentStored : Boolean;
    function IsColorStored : Boolean;
    procedure SetAlignment(const Value : TAlignment);
    procedure SetBiDiMode(Value : TBiDiMode);
    procedure SetCaptionAlignment(const Value : TAlignment);
    procedure SetCheckBox(Value : Boolean);
    procedure SetCheckState(Value : TCheckState);
    procedure SetCheckType(Value : TCheckType);
    procedure SetColor(const Value : TColor);
    procedure SetImageIndex(Value : TImageIndex);
    procedure SetLayout(Value : TVTHeaderColumnLayout);
    procedure SetMargin(Value : TDimension);
    procedure SetMaxWidth(Value : TDimension);
    procedure SetMinWidth(Value : TDimension);
    procedure SetOptions(Value : TVTColumnOptions);
    procedure SetPosition(Value : TColumnPosition);
    procedure SetSpacing(Value : TDimension);
    procedure SetStyle(Value : TVirtualTreeColumnStyle);

  protected
    FLeft : TDimension;
    procedure ChangeScale(M, D : TDimension; isDpiChange : Boolean); virtual;
    procedure ComputeHeaderLayout(var PaintInfo : THeaderPaintInfo; DrawFormat : Cardinal; CalculateTextRect : Boolean = False);
    procedure DefineProperties(Filer : TFiler); override;
    procedure GetAbsoluteBounds(var Left, Right : TDimension);
    function GetDisplayName : string; override;
    function GetText : string; virtual;                   // [IPK]
    procedure SetText(const Value : string); virtual;     // [IPK] private to protected & virtual
    function GetOwner : TVirtualTreeColumns; reintroduce;
    procedure InternalSetWidth(const Value : TDimension); //bypass side effects in SetWidth
    procedure ReadHint(Reader : TReader);
    procedure ReadText(Reader : TReader);
    procedure SetCollection(Value : TCollection); override;
    procedure SetWidth(Value : TDimension);
  public
    constructor Create(Collection : TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source : TPersistent); override;
    function Equals(OtherColumnObj : TObject) : Boolean; override;
    function GetRect : TRect; virtual;
    property HasImage : Boolean read FHasImage;
    property ImageRect : TRect read FImageRect;
    procedure LoadFromStream(const Stream : TStream; Version : Integer);
    procedure ParentBiDiModeChanged;
    procedure ParentColorChanged;
    procedure RestoreLastWidth;
    function GetEffectiveColor() : TColor;
    procedure SaveToStream(const Stream : TStream);
    function UseRightToLeftReading : Boolean;

    property BonusPixel : Boolean read FBonusPixel write FBonusPixel;
    property CaptionText : string read FCaptionText;
    property Left : TDimension read GetLeft;
    property Owner : TVirtualTreeColumns read GetOwner;
    property SpringRest : Single read FSpringRest write FSpringRest;
  published
    property Alignment            : TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property BiDiMode             : TBiDiMode read FBiDiMode write SetBiDiMode stored IsBiDiModeStored;
    property CaptionAlignment     : TAlignment read GetCaptionAlignment write SetCaptionAlignment
      stored IsCaptionAlignmentStored default taLeftJustify;
    property CaptionWidth         : TDimension read GetCaptionWidth;
    property CheckType            : TCheckType read FCheckType write SetCheckType default ctCheckBox;
    property CheckState           : TCheckState read FCheckState write SetCheckState default csUncheckedNormal;
    property CheckBox             : Boolean read FCheckBox write SetCheckBox default False;
    property Color                : TColor read FColor write SetColor stored IsColorStored;
    property DefaultSortDirection : TSortDirection read FDefaultSortDirection write FDefaultSortDirection default sdAscending;
    property Hint                 : string read FHint write FHint;
    property ImageIndex           : TImageIndex read FImageIndex write SetImageIndex default - 1;
    property Layout               : TVTHeaderColumnLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin               : TDimension read FMargin write SetMargin default 4;
    property MaxWidth             : TDimension read FMaxWidth write SetMaxWidth default 10000;
    property MinWidth             : TDimension read FMinWidth write SetMinWidth default 10;
    property Options              : TVTColumnOptions read FOptions write SetOptions default DefaultColumnOptions;
    property EditOptions          : TVTEditOptions read FEditOptions write FEditOptions default toDefaultEdit;
    property EditNextColumn       : TDimension read FEditNextColumn write FEditNextColumn default - 1;
    property Position             : TColumnPosition read FPosition write SetPosition;
    property Spacing              : TDimension read FSpacing write SetSpacing default cDefaultColumnSpacing;
    property Style                : TVirtualTreeColumnStyle read FStyle write SetStyle default vsText;
    property Tag                  : NativeInt read FTag write FTag default 0;
    property Text                 : string read GetText write SetText;
    property Width                : TDimension read FWidth write SetWidth default 50;
  end;

  TVirtualTreeColumnClass = class of TVirtualTreeColumn;

  TColumnsArray = array of TVirtualTreeColumn;
  TCardinalArray = array of Cardinal;
  TIndexArray = array of TColumnIndex;

  TVirtualTreeColumns = class(TCollection)
  private
    FHeader           : TPersistent;
    FHeaderBitmap     : TBitmap;      // backbuffer for drawing
    FHoverIndex,                      // currently "hot" column
    FDownIndex,                       // Column on which a mouse button is held down.
    FTrackIndex       : TColumnIndex; // Index of column which is currently being resized.
    FClickIndex       : TColumnIndex; // Index of the last clicked column.
    FCheckBoxHit      : Boolean;      // True if the last click was on a header checkbox.
    FPositionToIndex  : TIndexArray;
    FDefaultWidth     : TDimension;   // the width columns are created with
    FNeedPositionsFix : Boolean;      // True if FixPositions must still be called after DFM loading or Bidi mode change.
    FClearing         : Boolean;      // True if columns are being deleted entirely.
    FColumnPopupMenu  : TPopupMenu;   // Member for storing the TVTHeaderPopupMenu

    function GetCount : TDimension;
    function GetItem(Index : TColumnIndex) : TVirtualTreeColumn;
    function GetNewIndex(P : TPoint; var OldIndex : TColumnIndex) : Boolean;
    procedure SetDefaultWidth(Value : TDimension);
    procedure SetItem(Index : TColumnIndex; Value : TVirtualTreeColumn);
  protected
    // drag support
    FDragIndex  : TColumnIndex; // index of column currently being dragged
    FDropTarget : TColumnIndex; // current target column (index) while dragging
    FDropBefore : Boolean;      // True if drop position is in the left half of a column, False for the right
                                          // side to drop the dragged column to

    procedure AdjustAutoSize(CurrentIndex : TColumnIndex; Force : Boolean = False);
    function AdjustDownColumn(P : TPoint) : TColumnIndex;
    function AdjustHoverColumn(P : TPoint) : Boolean;
    procedure AdjustPosition(Column : TVirtualTreeColumn; Position : Cardinal);
    function CanSplitterResize(P : TPoint; Column : TColumnIndex) : Boolean;
    procedure DoCanSplitterResize(P : TPoint; Column : TColumnIndex; var Allowed : Boolean); virtual;
    procedure DrawButtonText(DC : HDC; Caption : string; Bounds : TRect; Enabled, Hot : Boolean; DrawFormat : Cardinal;
      WrapCaption : Boolean);
    procedure FixPositions;
    function GetColumnAndBounds(P : TPoint; var ColumnLeft, ColumnRight : TDimension; Relative : Boolean = True) : Integer;
    function GetOwner : TPersistent; override;
    function HandleClick(P : TPoint; Button : TMouseButton; Force, DblClick : Boolean) : Boolean; virtual;
    procedure HeaderPopupMenuAddHeaderPopupItem(const Sender : TObject; const Column : TColumnIndex; var Cmd : TAddPopupItemType);
    procedure HeaderPopupMenuColumnChange(const Sender : TObject; const Column : TColumnIndex; Visible : Boolean);
    procedure IndexChanged(OldIndex, NewIndex : Integer);
    procedure InitializePositionArray;
    procedure Notify(Item : TCollectionItem; Action : System.Classes.TCollectionNotification); override;
    procedure ReorderColumns(RTL : Boolean);
    procedure SetHoverIndex(Index : TColumnIndex);
    procedure Update(Item : TCollectionItem); override;
    procedure UpdatePositions(Force : Boolean = False);

    property HeaderBitmap : TBitmap read FHeaderBitmap;
    property PositionToIndex : TIndexArray read FPositionToIndex;
    property HoverIndex : TColumnIndex read FHoverIndex write FHoverIndex;
    property DownIndex : TColumnIndex read FDownIndex write FDownIndex;
    property CheckBoxHit : Boolean read FCheckBoxHit write FCheckBoxHit;
    // Mitigator function to use the correct style service for this context (either the style assigned to the control for Delphi > 10.4 or the application style)
    function StyleServices(AControl : TControl = nil) : TCustomStyleServices;
  public
    constructor Create(AOwner : TPersistent); virtual;
    destructor Destroy; override;

    function Add : TVirtualTreeColumn; virtual;
    procedure AnimatedResize(Column : TColumnIndex; NewWidth : TDimension);
    procedure Assign(Source : TPersistent); override;
    procedure Clear; virtual;
    function ColumnFromPosition(P : TPoint; Relative : Boolean = True) : TColumnIndex; overload; virtual;
    function ColumnFromPosition(PositionIndex : TColumnPosition) : TColumnIndex; overload; virtual;
    function Equals(OtherColumnsObj : TObject) : Boolean; override;
    procedure GetColumnBounds(Column : TColumnIndex; var Left, Right : TDimension);
    function GetFirstVisibleColumn(ConsiderAllowFocus : Boolean = False) : TColumnIndex;
    function GetLastVisibleColumn(ConsiderAllowFocus : Boolean = False) : TColumnIndex;
    function GetFirstColumn : TColumnIndex;
    function GetNextColumn(Column : TColumnIndex) : TColumnIndex;
    function GetNextVisibleColumn(Column : TColumnIndex; ConsiderAllowFocus : Boolean = False) : TColumnIndex;
    function GetPreviousColumn(Column : TColumnIndex) : TColumnIndex;
    function GetPreviousVisibleColumn(Column : TColumnIndex; ConsiderAllowFocus : Boolean = False) : TColumnIndex;
    function GetScrollWidth : TDimension;
    function GetVisibleColumns : TColumnsArray;
    function GetVisibleFixedWidth : TDimension;
    function IsValidColumn(Column : TColumnIndex) : Boolean;
    procedure LoadFromStream(const Stream : TStream; Version : Integer);
    procedure PaintHeader(DC : HDC; R : TRect; HOffset : TDimension); overload; virtual;
    procedure PaintHeader(TargetCanvas : TCanvas; R : TRect; const Target : TPoint;
      RTLOffset : TDimension = 0); overload; virtual;
    procedure SaveToStream(const Stream : TStream);
    procedure EndUpdate(); override;
    function TotalWidth : TDimension;

    property Count : Integer read GetCount;
    property ClickIndex : TColumnIndex read FClickIndex write FClickIndex;
    property DefaultWidth : TDimension read FDefaultWidth write SetDefaultWidth;
    property DragIndex : TColumnIndex read FDragIndex write FDragIndex;
    property DropBefore : Boolean read FDropBefore write FDropBefore;
    property DropTarget : TColumnIndex read FDropTarget write FDropTarget;
    property Items[Index : TColumnIndex] : TVirtualTreeColumn read GetItem write SetItem; default;
    //property Header: TPersistent read FHeader;
    property TrackIndex : TColumnIndex read FTrackIndex write FTrackIndex;
  end;

  TVirtualTreeColumnsClass = class of TVirtualTreeColumns;

implementation

uses
  WinApi.ShlObj,
  WinApi.UxTheme,
  System.Math,
  System.SysUtils,
  Vcl.ImgList,
  VirtualTrees,
  VirtualTrees.Header,
  VirtualTrees.HeaderPopup,
  VirtualTrees.StyleHooks,
  VirtualTrees.Utils;

type
  TBaseVirtualTreeCracker = class(TBaseVirtualTree);
  TVTHeaderCracker = class(TVTHeader);

  TVirtualTreeColumnHelper = class helper for TVirtualTreeColumn
    function TreeView : TBaseVirtualTreeCracker;
    function Header : TVTHeaderCracker;
  end;

  TVirtualTreeColumnsHelper = class helper for TVirtualTreeColumns
    function Header : TVTHeaderCracker;
    function TreeView : TBaseVirtualTreeCracker;
  end;

{ TSortDirectionHelper }

function TSortDirectionHelper.ToInt() : Integer;
begin
  Result := cSortDirectionToInt[Self];
end;

{ TCheckStateHelper }

function TCheckStateHelper.IsDisabled : Boolean;
begin
  Result := Self >= TCheckState.csUncheckedDisabled;
end;

function TCheckStateHelper.IsChecked : Boolean;
begin
  Result := Self in [csCheckedNormal, csCheckedPressed, csCheckedDisabled];
end;

function TCheckStateHelper.IsUnChecked : Boolean;
begin
  Result := Self in [csUncheckedNormal, csUncheckedPressed, csUncheckedDisabled];
end;

function TCheckStateHelper.IsMixed : Boolean;
begin
  Result := Self in [csMixedNormal, csMixedPressed, csMixedDisabled];
end;

function TCheckStateHelper.GetEnabled : TCheckState;
begin
  Result := cEnabledState[Self];
end;

function TCheckStateHelper.GetPressed() : TCheckState;
begin
  Result := cPressedState[Self];
end;

function TCheckStateHelper.GetUnpressed() : TCheckState;
begin
  Result := cUnpressedState[Self];
end;

function TCheckStateHelper.GetToggled() : TCheckState;
begin
  Result := cToggledState[Self];
end;



//----------------- TVirtualTreeColumn ---------------------------------------------------------------------------------

constructor TVirtualTreeColumn.Create(Collection : TCollection);

begin
  FMinWidth := 10;
  FMaxWidth := 10000;
  FImageIndex := - 1;
  FMargin := 4;
  FSpacing := cDefaultColumnSpacing;
  FText := '';
  FOptions := DefaultColumnOptions;
  FAlignment := taLeftJustify;
  FBiDiMode := bdLeftToRight;
  FColor := clWindow;
  FLayout := blGlyphLeft;
  FBonusPixel := False;
  FCaptionAlignment := taLeftJustify;
  FCheckType := ctCheckBox;
  FCheckState := csUncheckedNormal;
  FCheckBox := False;
  FHasImage := False;
  FDefaultSortDirection := sdAscending;
  FEditNextColumn := - 1;

  inherited Create(Collection);

  if Assigned(Owner) then
  begin
    FWidth := Owner.DefaultWidth;
    FLastWidth := Owner.DefaultWidth;
    FPosition := Owner.Count - 1;
  end;
end;

procedure TVirtualTreeColumn.SetCollection(Value : TCollection);
begin
  inherited;
  // Read parent bidi mode and color values as default values.
  ParentBiDiModeChanged;
  ParentColorChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TVirtualTreeColumn.Destroy;

var
  I  : Integer;
  ai : TColumnIndex;
  sc : TColumnIndex;

  //--------------- local function ---------------------------------------------

  procedure AdjustColumnIndex(var ColumnIndex : TColumnIndex);

  begin
    if Index = ColumnIndex then
      ColumnIndex := NoColumn
    else
      if Index < ColumnIndex then
      Dec(ColumnIndex);
  end;

  //--------------- end local function -----------------------------------------

begin
  // Check if this column is somehow referenced by its collection parent or the header.
  with Owner do
  begin
    // If the columns collection object is currently deleting all columns
    // then we don't need to check the various cached indices individually.
    if not FClearing then
    begin
      TreeView.CancelEditNode;
      IndexChanged(Index, - 1);

      AdjustColumnIndex(FHoverIndex);
      AdjustColumnIndex(FDownIndex);
      AdjustColumnIndex(FTrackIndex);
      AdjustColumnIndex(FClickIndex);

      with TVTHeaderCracker(Header) do
      begin
        ai := AutoSizeIndex;
        AdjustColumnIndex(ai);
        InternalSetAutoSizeIndex(ai);
        if Index = MainColumn then
        begin
          // If the current main column is about to be destroyed then we have to find a new main column.
          InternalSetMainColumn(NoColumn); //SetColumn has side effects we want to avoid here.
          for I := 0 to Count - 1 do
            if I <> Index then
            begin
              InternalSetMainColumn(I);
              Break;
            end;
        end;
        sc := SortColumn;
        AdjustColumnIndex(sc);
        InternalSetSortColumn(sc);
      end;
    end;
  end;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.GetCaptionAlignment : TAlignment;

begin
  if coUseCaptionAlignment in FOptions then
    Result := FCaptionAlignment
  else
    Result := FAlignment;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.GetCaptionWidth : TDimension;
var
  Theme             : HTHEME;
  AdvancedOwnerDraw : Boolean;
  PaintInfo         : THeaderPaintInfo;
  RequestedElements : THeaderPaintElements;

  TextSize          : TSize;
  HeaderGlyphSize   : TPoint;
  UseText           : Boolean;
  R                 : TRect;
begin
  AdvancedOwnerDraw := (hoOwnerDraw in Header.Options) and Assigned(TreeView.OnAdvancedHeaderDraw) and Assigned(TreeView.OnHeaderDrawQueryElements) and
    not (csDesigning in TreeView.ComponentState);

  PaintInfo.Column := Self;
  PaintInfo.TargetCanvas := Owner.HeaderBitmap.Canvas;

  with PaintInfo, Column do
  begin
    ShowHeaderGlyph := (hoShowImages in Header.Options) and ((Assigned(Header.Images) and (FImageIndex > - 1)) or FCheckBox);
    ShowSortGlyph := ((Header.SortColumn > - 1) and (Self = Owner.Items[Header.SortColumn])) and (hoShowSortGlyphs in Header.Options);

      // This path for text columns or advanced owner draw.
      // See if the application wants to draw part of the header itself.
    RequestedElements := [];
    if AdvancedOwnerDraw then
    begin
      PaintInfo.Column := Self;
      TreeView.DoHeaderDrawQueryElements(PaintInfo, RequestedElements);
    end;
  end;

  UseText := Length(FText) > 0;
  // If nothing is to show then don't waste time with useless preparation.
  if not (UseText or PaintInfo.ShowHeaderGlyph or PaintInfo.ShowSortGlyph) then
    Exit(0);

  // Calculate sizes of the involved items.
  with Header do
  begin
    if PaintInfo.ShowHeaderGlyph then
      if not FCheckBox then
      begin
        if Assigned(Images) then
          HeaderGlyphSize := Point(Images.Width, Images.Height);
      end
      else
        with Self.TreeView do
        begin
          if Assigned(CheckImages) then
            HeaderGlyphSize := Point(CheckImages.Width, CheckImages.Height);
        end
    else
      HeaderGlyphSize := Point(0, 0);
    if PaintInfo.ShowSortGlyph then
    begin
      if tsUseExplorerTheme in Self.TreeView.TreeStates then
      begin
        R := Rect(0, 0, 100, 100);
        Theme := OpenThemeData(Self.TreeView.Handle, 'HEADER');
        GetThemePartSize(Theme, PaintInfo.TargetCanvas.Handle, HP_HEADERSORTARROW, HSAS_SORTEDUP, @R, TS_TRUE, PaintInfo.SortGlyphSize);
        CloseThemeData(Theme);
      end
      else
      begin
        PaintInfo.SortGlyphSize.cx := Self.TreeView.ScaledPixels(16);
        PaintInfo.SortGlyphSize.cy := Self.TreeView.ScaledPixels(4);
      end;
    end
    else
    begin
      PaintInfo.SortGlyphSize.cx := 0;
      PaintInfo.SortGlyphSize.cy := 0;
    end;
  end;

  if UseText then
  begin
    GetTextExtentPoint32W(PaintInfo.TargetCanvas.Handle, PWideChar(FText), Length(FText), TextSize);
    Inc(TextSize.cx, 2);
  end
  else
  begin
    TextSize.cx := 0;
    TextSize.cy := 0;
  end;

  // if CalculateTextRect then
  Result := TextSize.cx;
  if PaintInfo.ShowHeaderGlyph then
    if Layout in [blGlyphLeft, blGlyphRight] then
      Inc(Result, HeaderGlyphSize.X + FSpacing)
    else                                                    // if Layout in [ blGlyphTop, blGlyphBottom] then
      Result := Max(Result, HeaderGlyphSize.X);
  if PaintInfo.ShowSortGlyph then
    Inc(Result, PaintInfo.SortGlyphSize.cx + FSpacing + 2); // without this +2, there is a slight movement of the sort glyph when expanding the column

end;
//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.GetLeft : Integer;

begin
  Result := FLeft;
  if [coVisible, coFixed] * FOptions <> [coVisible, coFixed] then
    Dec(Result, TreeView.EffectiveOffsetX);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.IsBiDiModeStored : Boolean;

begin
  Result := not (coParentBidiMode in FOptions);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.IsCaptionAlignmentStored : Boolean;

begin
  Result := coUseCaptionAlignment in FOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.IsColorStored : Boolean;

begin
  Result := not (coParentColor in FOptions);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetAlignment(const Value : TAlignment);

begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
    // Setting the alignment affects also the tree, hence invalidate it too.
    TreeView.Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetBiDiMode(Value : TBiDiMode);

begin
  if Value <> FBiDiMode then
  begin
    FBiDiMode := Value;
    Exclude(FOptions, coParentBidiMode);
    Changed(False);
    // Setting the alignment affects also the tree, hence invalidate it too.
    TreeView.Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetCaptionAlignment(const Value : TAlignment);

begin
  if not (coUseCaptionAlignment in FOptions) or (FCaptionAlignment <> Value) then
  begin
    FCaptionAlignment := Value;
    Include(FOptions, coUseCaptionAlignment);
    // Setting the alignment affects also the tree, hence invalidate it too.
    Header.Invalidate(Self);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetColor(const Value : TColor);

begin
  if FColor <> Value then
  begin
    FColor := Value;
    Exclude(FOptions, coParentColor);
    Exclude(FOptions, coStyleColor); // Issue #919
    Changed(False);
    TreeView.Invalidate;
  end;
end;

function TVirtualTreeColumn.GetEffectiveColor() : TColor;
// Returns the color that should effectively be used as background color for this
// column considering all flags in the TVirtualTreeColumn.Options property
begin
  if (coParentColor in Options) or ((coStyleColor in Options) and TreeView.VclStyleEnabled) then
    Result := TreeView.Colors.BackGroundColor
  else
    Result := Self.Color;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetCheckBox(Value : Boolean);

begin
  if Value <> FCheckBox then
  begin
    FCheckBox := Value;
    if Value and (csDesigning in TreeView.ComponentState) then
      Header.Options := Header.Options + [hoShowImages];
    Changed(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetCheckState(Value : TCheckState);

begin
  if Value <> FCheckState then
  begin
    FCheckState := Value;
    Changed(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetCheckType(Value : TCheckType);

begin
  if Value <> FCheckType then
  begin
    FCheckType := Value;
    Changed(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetImageIndex(Value : TImageIndex);

begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetLayout(Value : TVTHeaderColumnLayout);

begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Changed(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetMargin(Value : Integer);

begin
  // Compatibility setting for -1.
  if Value < 0 then
    Value := 4;
  if FMargin <> Value then
  begin
    FMargin := Value;
    Changed(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetMaxWidth(Value : Integer);

begin
  if Value < FMinWidth then
    Value := FMinWidth;
  FMaxWidth := Value;
  SetWidth(FWidth);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetMinWidth(Value : Integer);

begin
  if Value < 0 then
    Value := 0;
  if Value > FMaxWidth then
    Value := FMaxWidth;
  FMinWidth := Value;
  SetWidth(FWidth);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetOptions(Value : TVTColumnOptions);

var
  ToBeSet,
    ToBeCleared     : TVTColumnOptions;
  VisibleChanged,
    lParentColorSet : Boolean;
begin
  if FOptions <> Value then
  begin
    ToBeCleared := FOptions - Value;
    ToBeSet := Value - FOptions;

    FOptions := Value;

    VisibleChanged := coVisible in (ToBeSet + ToBeCleared);
    lParentColorSet := coParentColor in ToBeSet;

    if coParentBidiMode in ToBeSet then
      ParentBiDiModeChanged;
    if lParentColorSet then
    begin
      Include(FOptions, coStyleColor); // Issue #919
      ParentColorChanged();
    end;

    if coAutoSpring in ToBeSet then
      FSpringRest := 0;

    if coVisible in ToBeCleared then
      Header.UpdateMainColumn(); // Fixes issue #946

    if ((coFixed in ToBeSet) or (coFixed in ToBeCleared)) and (coVisible in FOptions) then
      Header.RescaleHeader;

    Changed(False);
    // Need to repaint and adjust the owner tree too.
    if not (csLoading in TreeView.ComponentState) and (VisibleChanged or lParentColorSet) and (Owner.UpdateCount = 0) and TreeView.HandleAllocated then
    begin
      TreeView.Invalidate();
      if VisibleChanged then
      begin
        TreeView.DoColumnVisibilityChanged(Self.Index, coVisible in ToBeSet);
        TreeView.UpdateHorizontalScrollBar(False);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetPosition(Value : TColumnPosition);

var
  Temp : TColumnIndex;

begin
  if (csLoading in TreeView.ComponentState) or (Owner.UpdateCount > 0) then
    // Only cache the position for final fixup when loading from DFM.
    FPosition := Value
  else
  begin
    if Value >= TColumnPosition(Collection.Count) then
      Value := Collection.Count - 1;
    if FPosition <> Value then
    begin
      with Owner do
      begin
        InitializePositionArray;
        TreeView.CancelEditNode;
        AdjustPosition(Self, Value);
        Self.Changed(False);

        // Need to repaint.
        with Self.Header do
        begin
          if (UpdateCount = 0) and TreeView.HandleAllocated then
          begin
            Invalidate(Self);
            TreeView.Invalidate;
          end;
        end;
      end;

      // If the moved column is now within the fixed columns then we make it fixed as well. If it's not
      // we clear the fixed state (in case that fixed column is moved outside fixed area).
      if (coFixed in FOptions) and (FPosition > 0) then
        Temp := Owner.ColumnFromPosition(FPosition - 1)
      else
        Temp := Owner.ColumnFromPosition(FPosition + 1);

      if Temp <> NoColumn then
      begin
        if coFixed in Owner[Temp].Options then
          Options := Options + [coFixed]
        else
          Options := Options - [coFixed];
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetSpacing(Value : Integer);

begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Changed(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetStyle(Value : TVirtualTreeColumnStyle);

begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetText(const Value : string);

begin
  if FText <> Value then
  begin
    FText := Value;
    FCaptionText := '';
    Changed(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SetWidth(Value : Integer);

var
  EffectiveMaxWidth,
    EffectiveMinWidth,
    TotalFixedMaxWidth,
    TotalFixedMinWidth : Integer;
  I                    : TColumnIndex;

begin
  if not (hsScaling in Header.States) then
    if ([coVisible, coFixed] * FOptions = [coVisible, coFixed]) then
    begin
      with Header, FixedAreaConstraints, TreeView do
      begin
        TotalFixedMinWidth := 0;
        TotalFixedMaxWidth := 0;
        for I := 0 to Columns.Count - 1 do
          if ([coVisible, coFixed] * Columns[I].Options = [coVisible, coFixed]) then
          begin
            Inc(TotalFixedMaxWidth, Columns[I].MaxWidth);
            Inc(TotalFixedMinWidth, Columns[I].MinWidth);
          end;

        // The percentage values have precedence over the pixel values.
        If MaxWidthPercent > 0 then
          TotalFixedMinWidth := Min((ClientWidth * MaxWidthPercent) div 100, TotalFixedMinWidth);
        If MinWidthPercent > 0 then
          TotalFixedMaxWidth := Max((ClientWidth * MinWidthPercent) div 100, TotalFixedMaxWidth);

        EffectiveMaxWidth := Min(TotalFixedMaxWidth - (Columns.GetVisibleFixedWidth - Self.FWidth), FMaxWidth);
        EffectiveMinWidth := Max(TotalFixedMinWidth - (Columns.GetVisibleFixedWidth - Self.FWidth), FMinWidth);
        Value := Min(Max(Value, EffectiveMinWidth), EffectiveMaxWidth);

        if MinWidthPercent > 0 then
          Value := Max((ClientWidth * MinWidthPercent) div 100 - Columns.GetVisibleFixedWidth + Self.FWidth, Value);
        if MaxWidthPercent > 0 then
          Value := Min((ClientWidth * MaxWidthPercent) div 100 - Columns.GetVisibleFixedWidth + Self.FWidth, Value);
      end;
    end
    else
      Value := Min(Max(Value, FMinWidth), FMaxWidth);

  if FWidth <> Value then
  begin
    FLastWidth := FWidth;
    if not (hsResizing in Header.States) then
      FBonusPixel := False;
    if not (hoAutoResize in Header.Options) or (Index <> Header.AutoSizeIndex) then
    begin
      FWidth := Value;
      Owner.UpdatePositions;
    end;
    if not (csLoading in TreeView.ComponentState) and (Owner.UpdateCount = 0) then
    begin
      if hoAutoResize in Header.Options then
        Owner.AdjustAutoSize(Index);
      TreeView.DoColumnResize(Index);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.ChangeScale(M, D : TDimension; isDpiChange : Boolean);
begin
  FMinWidth := MulDiv(FMinWidth, M, D);
  FMaxWidth := MulDiv(FMaxWidth, M, D);
  FSpacing := MulDiv(FSpacing, M, D);
  Self.Width := MulDiv(Self.Width, M, D);
end;

procedure TVirtualTreeColumn.ComputeHeaderLayout(var PaintInfo : THeaderPaintInfo; DrawFormat : Cardinal; CalculateTextRect : Boolean = False);

// The layout of a column header is determined by a lot of factors. This method takes them all into account and
// determines all necessary positions and bounds:
// - for the header text
// - the header glyph
// - the sort glyph

var
  TextSize          : TSize;
  TextPos,
    ClientSize,
    HeaderGlyphSize : TPoint;
  CurrentAlignment  : TAlignment;
  MinLeft,
    MaxRight,
    TextSpacing     : Integer;
  UseText           : Boolean;
  R                 : TRect;
  Theme             : HTHEME;

begin
  UseText := Length(FText) > 0;
  // If nothing is to show then don't waste time with useless preparation.
  if not (UseText or PaintInfo.ShowHeaderGlyph or PaintInfo.ShowSortGlyph) then
    Exit;

  CurrentAlignment := CaptionAlignment;
  if FBiDiMode <> bdLeftToRight then
    ChangeBiDiModeAlignment(CurrentAlignment);

  // Calculate sizes of the involved items.
  ClientSize := Point(PaintInfo.PaintRectangle.Right - PaintInfo.PaintRectangle.Left, PaintInfo.PaintRectangle.Bottom - PaintInfo.PaintRectangle.Top);
  with Owner, Header do
  begin
    if PaintInfo.ShowHeaderGlyph then
      if not FCheckBox then
        HeaderGlyphSize := Point(Images.Width, Images.Height)
      else
        with Self.TreeView do
        begin
          if Assigned(CheckImages) then
            HeaderGlyphSize := Point(CheckImages.Width, CheckImages.Height);
        end
    else
      HeaderGlyphSize := Point(0, 0);
    if PaintInfo.ShowSortGlyph then
    begin
      if tsUseExplorerTheme in Self.TreeView.TreeStates then
      begin
        R := Rect(0, 0, 100, 100);
        Theme := OpenThemeData(TreeView.Handle, 'HEADER');
        GetThemePartSize(Theme, PaintInfo.TargetCanvas.Handle, HP_HEADERSORTARROW, HSAS_SORTEDUP, @R, TS_TRUE, PaintInfo.SortGlyphSize);
        CloseThemeData(Theme);
      end
      else
      begin
        PaintInfo.SortGlyphSize.cx := Self.TreeView.ScaledPixels(16);
        PaintInfo.SortGlyphSize.cy := Self.TreeView.ScaledPixels(4);
      end;

      // In any case, the sort glyph is vertically centered.
      PaintInfo.SortGlyphPos.Y := (ClientSize.Y - PaintInfo.SortGlyphSize.cy) div 2;
    end
    else
    begin
      PaintInfo.SortGlyphSize.cx := 0;
      PaintInfo.SortGlyphSize.cy := 0;
    end;
  end;

  if UseText then
  begin
    if not (coWrapCaption in FOptions) then
    begin
      FCaptionText := FText;
      GetTextExtentPoint32W(PaintInfo.TargetCanvas.Handle, PWideChar(FText), Length(FText), TextSize);
      Inc(TextSize.cx, 2);
      PaintInfo.TextRectangle := Rect(0, 0, TextSize.cx, TextSize.cy);
    end
    else
    begin
      R := PaintInfo.PaintRectangle;
      if FCaptionText = '' then
        FCaptionText := WrapString(PaintInfo.TargetCanvas.Handle, FText, R, DT_RTLREADING and DrawFormat <> 0, DrawFormat);

      GetStringDrawRect(PaintInfo.TargetCanvas.Handle, FCaptionText, R, DrawFormat);
      TextSize.cx := PaintInfo.PaintRectangle.Right - PaintInfo.PaintRectangle.Left;
      TextSize.cy := R.Bottom - R.Top;
      PaintInfo.TextRectangle := Rect(0, 0, TextSize.cx, TextSize.cy);
    end;
    TextSpacing := FSpacing;
  end
  else
  begin
    TextSpacing := 0;
    TextSize.cx := 0;
    TextSize.cy := 0;
  end;

  // Check first for the special case where nothing is shown except the sort glyph.
  if PaintInfo.ShowSortGlyph and not (UseText or PaintInfo.ShowHeaderGlyph) then
  begin
    // Center the sort glyph in the available area if nothing else is there.
    PaintInfo.SortGlyphPos := Point((ClientSize.X - PaintInfo.SortGlyphSize.cx) div 2, (ClientSize.Y - PaintInfo.SortGlyphSize.cy) div 2);
  end
  else
  begin
    // Determine extents of text and glyph and calculate positions which are clear from the layout.
    if (Layout in [blGlyphLeft, blGlyphRight]) or not PaintInfo.ShowHeaderGlyph then
    begin
      PaintInfo.GlyphPos.Y := (ClientSize.Y - HeaderGlyphSize.Y) div 2;
      // If the text is taller than the given height, perform no vertical centration as this
      // would make the text even less readable.
      //Using Max() fixes badly positioned text if Extra Large fonts have been activated in the Windows display options
      TextPos.Y := Max( - 5, (ClientSize.Y - TextSize.cy) div 2);
    end
    else
    begin
      if Layout = blGlyphTop then
      begin
        PaintInfo.GlyphPos.Y := (ClientSize.Y - HeaderGlyphSize.Y - TextSize.cy - TextSpacing) div 2;
        TextPos.Y := PaintInfo.GlyphPos.Y + HeaderGlyphSize.Y + TextSpacing;
      end
      else
      begin
        TextPos.Y := (ClientSize.Y - HeaderGlyphSize.Y - TextSize.cy - TextSpacing) div 2;
        PaintInfo.GlyphPos.Y := TextPos.Y + TextSize.cy + TextSpacing;
      end;
    end;

    // Each alignment needs special consideration.
    case CurrentAlignment of
      taLeftJustify :
        begin
          MinLeft := FMargin;
          if PaintInfo.ShowSortGlyph and (FBiDiMode <> bdLeftToRight) then
          begin
            // In RTL context is the sort glyph placed on the left hand side.
            PaintInfo.SortGlyphPos.X := MinLeft;
            Inc(MinLeft, PaintInfo.SortGlyphSize.cx + FSpacing);
          end;
          if Layout in [blGlyphTop, blGlyphBottom] then
          begin
            // Header glyph is above or below text, so both must be considered when calculating
            // the left positition of the sort glyph (if it is on the right hand side).
            TextPos.X := MinLeft;
            if PaintInfo.ShowHeaderGlyph then
            begin
              PaintInfo.GlyphPos.X := (ClientSize.X - HeaderGlyphSize.X) div 2;
              if PaintInfo.GlyphPos.X < MinLeft then
                PaintInfo.GlyphPos.X := MinLeft;
              MinLeft := Max(TextPos.X + TextSize.cx + TextSpacing, PaintInfo.GlyphPos.X + HeaderGlyphSize.X + FSpacing);
            end
            else
              MinLeft := TextPos.X + TextSize.cx + TextSpacing;
          end
          else
          begin
            // Everything is lined up. TextSpacing might be 0 if there is no text.
            // This simplifies the calculation because no extra tests are necessary.
            if PaintInfo.ShowHeaderGlyph and (Layout = blGlyphLeft) then
            begin
              PaintInfo.GlyphPos.X := MinLeft;
              Inc(MinLeft, HeaderGlyphSize.X + FSpacing);
            end;
            TextPos.X := MinLeft;
            Inc(MinLeft, TextSize.cx + TextSpacing);
            if PaintInfo.ShowHeaderGlyph and (Layout = blGlyphRight) then
            begin
              PaintInfo.GlyphPos.X := MinLeft;
              Inc(MinLeft, HeaderGlyphSize.X + FSpacing);
            end;
          end;
          if PaintInfo.ShowSortGlyph and (FBiDiMode = bdLeftToRight) then
            PaintInfo.SortGlyphPos.X := MinLeft;
        end;
      taCenter :
        begin
          if Layout in [blGlyphTop, blGlyphBottom] then
          begin
            PaintInfo.GlyphPos.X := (ClientSize.X - HeaderGlyphSize.X) div 2;
            TextPos.X := (ClientSize.X - TextSize.cx) div 2;
            if PaintInfo.ShowSortGlyph then
              Dec(TextPos.X, PaintInfo.SortGlyphSize.cx div 2);
          end
          else
          begin
            MinLeft := (ClientSize.X - HeaderGlyphSize.X - TextSpacing - TextSize.cx) div 2;
            if PaintInfo.ShowHeaderGlyph and (Layout = blGlyphLeft) then
            begin
              PaintInfo.GlyphPos.X := MinLeft;
              Inc(MinLeft, HeaderGlyphSize.X + TextSpacing);
            end;
            TextPos.X := MinLeft;
            Inc(MinLeft, TextSize.cx + TextSpacing);
            if PaintInfo.ShowHeaderGlyph and (Layout = blGlyphRight) then
              PaintInfo.GlyphPos.X := MinLeft;
          end;
          if PaintInfo.ShowHeaderGlyph then
          begin
            MinLeft := Min(PaintInfo.GlyphPos.X, TextPos.X);
            MaxRight := Max(PaintInfo.GlyphPos.X + HeaderGlyphSize.X, TextPos.X + TextSize.cx);
          end
          else
          begin
            MinLeft := TextPos.X;
            MaxRight := TextPos.X + TextSize.cx;
          end;
          // Place the sort glyph directly to the left or right of the larger item.
          if PaintInfo.ShowSortGlyph then
            if FBiDiMode = bdLeftToRight then
            begin
              // Sort glyph on the right hand side.
              PaintInfo.SortGlyphPos.X := MaxRight + FSpacing;
            end
            else
            begin
              // Sort glyph on the left hand side.
              PaintInfo.SortGlyphPos.X := MinLeft - FSpacing - PaintInfo.SortGlyphSize.cx;
            end;
        end;
    else
      // taRightJustify
      MaxRight := ClientSize.X - FMargin;
      if PaintInfo.ShowSortGlyph and (FBiDiMode = bdLeftToRight) then
      begin
        // In LTR context is the sort glyph placed on the right hand side.
        Dec(MaxRight, PaintInfo.SortGlyphSize.cx);
        PaintInfo.SortGlyphPos.X := MaxRight;
        Dec(MaxRight, FSpacing);
      end;
      if Layout in [blGlyphTop, blGlyphBottom] then
      begin
        TextPos.X := MaxRight - TextSize.cx;
        if PaintInfo.ShowHeaderGlyph then
        begin
          PaintInfo.GlyphPos.X := (ClientSize.X - HeaderGlyphSize.X) div 2;
          if PaintInfo.GlyphPos.X + HeaderGlyphSize.X + FSpacing > MaxRight then
            PaintInfo.GlyphPos.X := MaxRight - HeaderGlyphSize.X - FSpacing;
          MaxRight := Min(TextPos.X - TextSpacing, PaintInfo.GlyphPos.X - FSpacing);
        end
        else
          MaxRight := TextPos.X - TextSpacing;
      end
      else
      begin
        // Everything is lined up. TextSpacing might be 0 if there is no text.
        // This simplifies the calculation because no extra tests are necessary.
        if PaintInfo.ShowHeaderGlyph and (Layout = blGlyphRight) then
        begin
          PaintInfo.GlyphPos.X := MaxRight - HeaderGlyphSize.X;
          MaxRight := PaintInfo.GlyphPos.X - FSpacing;
        end;
        TextPos.X := MaxRight - TextSize.cx;
        MaxRight := TextPos.X - TextSpacing;
        if PaintInfo.ShowHeaderGlyph and (Layout = blGlyphLeft) then
        begin
          PaintInfo.GlyphPos.X := MaxRight - HeaderGlyphSize.X;
          MaxRight := PaintInfo.GlyphPos.X - FSpacing;
        end;
      end;
      if PaintInfo.ShowSortGlyph and (FBiDiMode <> bdLeftToRight) then
        PaintInfo.SortGlyphPos.X := MaxRight - PaintInfo.SortGlyphSize.cx;
    end;
  end;

  // Once the position of each element is determined there remains only one but important step.
  // The horizontal positions of every element must be adjusted so that it always fits into the
  // given header area. This is accomplished by shorten the text appropriately.

  // These are the maximum bounds. Nothing goes beyond them.
  MinLeft := FMargin;
  MaxRight := ClientSize.X - FMargin;
  if PaintInfo.ShowSortGlyph then
  begin
    if FBiDiMode = bdLeftToRight then
    begin
      // Sort glyph on the right hand side.
      if PaintInfo.SortGlyphPos.X + PaintInfo.SortGlyphSize.cx > MaxRight then
        PaintInfo.SortGlyphPos.X := MaxRight - PaintInfo.SortGlyphSize.cx;
      MaxRight := PaintInfo.SortGlyphPos.X - FSpacing;
    end;

    // Consider also the left side of the sort glyph regardless of the bidi mode.
    if PaintInfo.SortGlyphPos.X < MinLeft then
      PaintInfo.SortGlyphPos.X := MinLeft;
    // Left border needs only adjustment if the sort glyph marks the left border.
    if FBiDiMode <> bdLeftToRight then
      MinLeft := PaintInfo.SortGlyphPos.X + PaintInfo.SortGlyphSize.cx + FSpacing;

    // Finally transform sort glyph to its actual position.
    Inc(PaintInfo.SortGlyphPos.X, PaintInfo.PaintRectangle.Left);
    Inc(PaintInfo.SortGlyphPos.Y, PaintInfo.PaintRectangle.Top);
  end;
  if PaintInfo.ShowHeaderGlyph then
  begin
    if PaintInfo.GlyphPos.X + HeaderGlyphSize.X > MaxRight then
      PaintInfo.GlyphPos.X := MaxRight - HeaderGlyphSize.X;
    if Layout = blGlyphRight then
      MaxRight := PaintInfo.GlyphPos.X - FSpacing;
    if PaintInfo.GlyphPos.X < MinLeft then
      PaintInfo.GlyphPos.X := MinLeft;
    if Layout = blGlyphLeft then
      MinLeft := PaintInfo.GlyphPos.X + HeaderGlyphSize.X + FSpacing;
    if FCheckBox and (Header.MainColumn = Self.Index) then
      Dec(PaintInfo.GlyphPos.X, 2)
    else
      if Header.MainColumn <> Self.Index then
      Dec(PaintInfo.GlyphPos.X, 2);

    // Finally transform header glyph to its actual position.
    Inc(PaintInfo.GlyphPos.X, PaintInfo.PaintRectangle.Left);
    Inc(PaintInfo.GlyphPos.Y, PaintInfo.PaintRectangle.Top);
  end;
  if UseText then
  begin
    if TextPos.X < MinLeft then
      TextPos.X := MinLeft;
    OffsetRect(PaintInfo.TextRectangle, TextPos.X, TextPos.Y);
    if PaintInfo.TextRectangle.Right > MaxRight then
      PaintInfo.TextRectangle.Right := MaxRight;
    OffsetRect(PaintInfo.TextRectangle, PaintInfo.PaintRectangle.Left, PaintInfo.PaintRectangle.Top);

    if coWrapCaption in FOptions then
    begin
      // Wrap the column caption if necessary.
      R := PaintInfo.TextRectangle;
      FCaptionText := WrapString(PaintInfo.TargetCanvas.Handle, FText, R, DT_RTLREADING and DrawFormat <> 0, DrawFormat);
      GetStringDrawRect(PaintInfo.TargetCanvas.Handle, FCaptionText, R, DrawFormat);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.DefineProperties(Filer : TFiler);

begin
  inherited;

  // These properites are remains from non-Unicode Delphi versions, readers remain for backward compatibility.
  Filer.DefineProperty('WideText', ReadText, nil, False);
  Filer.DefineProperty('WideHint', ReadHint, nil, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.GetAbsoluteBounds(var Left, Right : Integer);

// Returns the column's left and right bounds in header coordinates, that is, independant of the scrolling position.

begin
  Left := FLeft;
  Right := FLeft + FWidth;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.GetDisplayName : string;

// Returns the column text if it only contains ANSI characters, otherwise the column id is returned because the IDE
// still cannot handle Unicode strings.

var
  I : Integer;

begin
  // Check if the text of the column contains characters > 255
  I := 1;
  while I <= Length(FText) do
  begin
    if Ord(FText[I]) > 255 then
      Break;
    Inc(I);
  end;

  if I > Length(FText) then
    Result := FText // implicit conversion
  else
    Result := Format('Column %d', [Index]);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.GetOwner : TVirtualTreeColumns;

begin
  Result := Collection as TVirtualTreeColumns;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.InternalSetWidth(const Value : TDimension);
begin
  FWidth := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.ReadText(Reader : TReader);

begin
  case Reader.NextValue of
    vaLString, vaString :
      SetText(Reader.ReadString);
  else
    SetText(Reader.ReadString);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.ReadHint(Reader : TReader);

begin
  case Reader.NextValue of
    vaLString, vaString :
      FHint := Reader.ReadString;
  else
    FHint := Reader.ReadString;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------


//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.Assign(Source : TPersistent);

var
  OldOptions : TVTColumnOptions;

begin
  if Source is TVirtualTreeColumn then
  begin
    OldOptions := FOptions;
    FOptions := [];

    BiDiMode := TVirtualTreeColumn(Source).BiDiMode;
    ImageIndex := TVirtualTreeColumn(Source).ImageIndex;
    Layout := TVirtualTreeColumn(Source).Layout;
    Margin := TVirtualTreeColumn(Source).Margin;
    MaxWidth := TVirtualTreeColumn(Source).MaxWidth;
    MinWidth := TVirtualTreeColumn(Source).MinWidth;
    Position := TVirtualTreeColumn(Source).Position;
    Spacing := TVirtualTreeColumn(Source).Spacing;
    Style := TVirtualTreeColumn(Source).Style;
    Text := TVirtualTreeColumn(Source).Text;
    Hint := TVirtualTreeColumn(Source).Hint;
    Width := TVirtualTreeColumn(Source).Width;
    Alignment := TVirtualTreeColumn(Source).Alignment;
    CaptionAlignment := TVirtualTreeColumn(Source).CaptionAlignment;
    Color := TVirtualTreeColumn(Source).Color;
    Tag := TVirtualTreeColumn(Source).Tag;
    EditOptions := TVirtualTreeColumn(Source).EditOptions;
    EditNextColumn := TVirtualTreeColumn(Source).EditNextColumn;

    // Order is important. Assign options last.
    FOptions := OldOptions;
    Options := TVirtualTreeColumn(Source).Options;

    Changed(False);
  end
  else
    inherited Assign(Source);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.Equals(OtherColumnObj : TObject) : Boolean;
var
  OtherColumn : TVirtualTreeColumn;
begin
  if OtherColumnObj is TVirtualTreeColumn then
  begin
    OtherColumn := TVirtualTreeColumn(OtherColumnObj);
    Result := (BiDiMode = OtherColumn.BiDiMode) and
      (ImageIndex = OtherColumn.ImageIndex) and
      (Layout = OtherColumn.Layout) and
      (Margin = OtherColumn.Margin) and
      (MaxWidth = OtherColumn.MaxWidth) and
      (MinWidth = OtherColumn.MinWidth) and
      (Position = OtherColumn.Position) and
      (Spacing = OtherColumn.Spacing) and
      (Style = OtherColumn.Style) and
      (Text = OtherColumn.Text) and
      (Hint = OtherColumn.Hint) and
      (Width = OtherColumn.Width) and
      (Alignment = OtherColumn.Alignment) and
      (CaptionAlignment = OtherColumn.CaptionAlignment) and
      (Color = OtherColumn.Color) and
      (Tag = OtherColumn.Tag) and
      (Options = OtherColumn.Options);
  end
  else
    Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.GetRect : TRect;

// Returns the rectangle this column occupies in the header (relative to (0, 0) of the non-client area).

begin
  with TVirtualTreeColumns(GetOwner).FHeader do
    Result := TreeView.HeaderRect;
  Inc(Result.Left, FLeft);
  Result.Right := Result.Left + FWidth;
end;

//----------------------------------------------------------------------------------------------------------------------

// [IPK]
function TVirtualTreeColumn.GetText : string;

begin
  Result := FText;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.LoadFromStream(const Stream : TStream; Version : Integer);
var
  Dummy : Integer;
  S     : string;

begin
  with Stream do
  begin
    ReadBuffer(Dummy, SizeOf(Dummy));
    SetLength(S, Dummy);
    ReadBuffer(PWideChar(S)^, 2 * Dummy);
    Text := S;
    ReadBuffer(Dummy, SizeOf(Dummy));
    SetLength(FHint, Dummy);
    ReadBuffer(PWideChar(FHint)^, 2 * Dummy);
    ReadBuffer(Dummy, SizeOf(Dummy));
    Width := Dummy;
    ReadBuffer(Dummy, SizeOf(Dummy));
    MinWidth := Dummy;
    ReadBuffer(Dummy, SizeOf(Dummy));
    MaxWidth := Dummy;
    ReadBuffer(Dummy, SizeOf(Dummy));
    Style := TVirtualTreeColumnStyle(Dummy);
    ReadBuffer(Dummy, SizeOf(Dummy));
    ImageIndex := Dummy;
    ReadBuffer(Dummy, SizeOf(Dummy));
    Layout := TVTHeaderColumnLayout(Dummy);
    ReadBuffer(Dummy, SizeOf(Dummy));
    Margin := Dummy;
    ReadBuffer(Dummy, SizeOf(Dummy));
    Spacing := Dummy;
    ReadBuffer(Dummy, SizeOf(Dummy));
    BiDiMode := TBiDiMode(Dummy);

    ReadBuffer(Dummy, SizeOf(Dummy));
    if Version >= 3 then
      Options := TVTColumnOptions(Dummy);

    if Version > 0 then
    begin
      // Parts which have been introduced/changed with header stream version 1+.
      ReadBuffer(Dummy, SizeOf(Dummy));
      Tag := Dummy;
      ReadBuffer(Dummy, SizeOf(Dummy));
      Alignment := TAlignment(Dummy);

      if Version > 1 then
      begin
        ReadBuffer(Dummy, SizeOf(Dummy));
        Color := TColor(Dummy);
      end;

      if Version > 5 then
      begin
        if coUseCaptionAlignment in FOptions then
        begin
          ReadBuffer(Dummy, SizeOf(Dummy));
          CaptionAlignment := TAlignment(Dummy);
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.ParentBiDiModeChanged;

var
  Columns : TVirtualTreeColumns;

begin
  if coParentBidiMode in FOptions then
  begin
    Columns := GetOwner as TVirtualTreeColumns;
    if Assigned(Columns) and (FBiDiMode <> TreeView.BiDiMode) then
    begin
      FBiDiMode := TreeView.BiDiMode;
      Changed(False);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.ParentColorChanged;

var
  Columns : TVirtualTreeColumns;

begin
  if coParentColor in FOptions then
  begin
    Columns := GetOwner as TVirtualTreeColumns;
    if Assigned(Columns) and (FColor <> TreeView.Color) then
    begin
      FColor := TreeView.Color;
      Changed(False);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.RestoreLastWidth;

begin
  TVirtualTreeColumns(GetOwner).AnimatedResize(Index, FLastWidth);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumn.SaveToStream(const Stream : TStream);

var
  Dummy : Integer;

begin
  with Stream do
  begin
    Dummy := Length(FText);
    WriteBuffer(Dummy, SizeOf(Dummy));
    WriteBuffer(PWideChar(FText)^, 2 * Dummy);
    Dummy := Length(FHint);
    WriteBuffer(Dummy, SizeOf(Dummy));
    WriteBuffer(PWideChar(FHint)^, 2 * Dummy);
    WriteBuffer(FWidth, SizeOf(FWidth));
    WriteBuffer(FMinWidth, SizeOf(FMinWidth));
    WriteBuffer(FMaxWidth, SizeOf(FMaxWidth));
    Dummy := Ord(FStyle);
    WriteBuffer(Dummy, SizeOf(Dummy));
    Dummy := FImageIndex;
    WriteBuffer(Dummy, SizeOf(Dummy));
    Dummy := Ord(FLayout);
    WriteBuffer(Dummy, SizeOf(Dummy));
    WriteBuffer(FMargin, SizeOf(FMargin));
    WriteBuffer(FSpacing, SizeOf(FSpacing));
    Dummy := Ord(FBiDiMode);
    WriteBuffer(Dummy, SizeOf(Dummy));
    Dummy := Integer(FOptions);
    WriteBuffer(Dummy, SizeOf(Dummy));

    // parts introduced with stream version 1
    WriteBuffer(FTag, SizeOf(Dummy));
    Dummy := Cardinal(FAlignment);
    WriteBuffer(Dummy, SizeOf(Dummy));

    // parts introduced with stream version 2
    Dummy := Integer(FColor);
    WriteBuffer(Dummy, SizeOf(Dummy));

    // parts introduced with stream version 6
    if coUseCaptionAlignment in FOptions then
    begin
      Dummy := Cardinal(FCaptionAlignment);
      WriteBuffer(Dummy, SizeOf(Dummy));
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumn.UseRightToLeftReading : Boolean;

begin
  Result := FBiDiMode <> bdLeftToRight;
end;

//----------------- TVirtualTreeColumns --------------------------------------------------------------------------------

constructor TVirtualTreeColumns.Create(AOwner : TPersistent);

var
  ColumnClass : TVirtualTreeColumnClass;

begin
  FHeader := AOwner;

  // Determine column class to be used in the header.
  ColumnClass := Self.TreeView.GetColumnClass;
  // The owner tree always returns the default tree column class if not changed by application/descendants.
  inherited Create(ColumnClass);

  FHeaderBitmap := TBitmap.Create;
  FHeaderBitmap.PixelFormat := pf32Bit;

  FHoverIndex := NoColumn;
  FDownIndex := NoColumn;
  FClickIndex := NoColumn;
  FDropTarget := NoColumn;
  FTrackIndex := NoColumn;
  FDefaultWidth := 50;
  Self.FColumnPopupMenu := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TVirtualTreeColumns.Destroy;

begin
  FreeAndNil(FColumnPopupMenu);
  FreeAndNil(FHeaderBitmap);
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetCount : Integer;

begin
  Result := inherited Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetItem(Index : TColumnIndex) : TVirtualTreeColumn;

begin
  Result := TVirtualTreeColumn(inherited GetItem(Index));
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetNewIndex(P : TPoint; var OldIndex : TColumnIndex) : Boolean;

var
  NewIndex : Integer;

begin
  Result := False;
  // convert to local coordinates
  Inc(P.Y, Header.Height);
  NewIndex := ColumnFromPosition(P);
  if NewIndex <> OldIndex then
  begin
    if OldIndex > NoColumn then
      Header.Invalidate(Items[OldIndex], False, True);
    OldIndex := NewIndex;
    if OldIndex > NoColumn then
      Header.Invalidate(Items[OldIndex], False, True);
    Result := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.SetDefaultWidth(Value : Integer);

begin
  FDefaultWidth := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.SetItem(Index : TColumnIndex; Value : TVirtualTreeColumn);

begin
  inherited SetItem(Index, Value);
end;

function TVirtualTreeColumns.StyleServices(AControl : TControl) : TCustomStyleServices;
begin
  if AControl = nil then
    AControl := TreeView;
  Result := VTStyleServices(AControl);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.AdjustAutoSize(CurrentIndex : TColumnIndex; Force : Boolean = False);

// Called only if the header is in auto-size mode which means a column needs to be so large
// that it fills all the horizontal space not occupied by the other columns.
// CurrentIndex (if not InvalidColumn) describes which column has just been resized.

var
  NewValue,
    AutoIndex,
    Index,
    RestWidth : Integer;
  WasUpdating : Boolean;
begin
  if Count > 0 then
  begin
    // Determine index to be used for auto resizing. This is usually given by the owner's AutoSizeIndex, but
    // could be different if the column whose resize caused the invokation here is either the auto column itself
    // or visually to the right of the auto size column.
    AutoIndex := Header.AutoSizeIndex;
    if (AutoIndex < 0) or (AutoIndex >= Count) then
      AutoIndex := Count - 1;

    if AutoIndex >= 0 then
    begin
      with TreeView do
      begin
        if HandleAllocated then
          RestWidth := ClientWidth
        else
          RestWidth := Width;
      end;

      // Go through all columns and calculate the rest space remaining.
      for Index := 0 to Count - 1 do
        if (Index <> AutoIndex) and (coVisible in Items[Index].Options) then
          Dec(RestWidth, Items[Index].Width);

      with Items[AutoIndex] do
      begin
        NewValue := Max(MinWidth, Min(MaxWidth, RestWidth));
        if Force or (FWidth <> NewValue) then
        begin
          FWidth := NewValue;
          UpdatePositions;
          WasUpdating := csUpdating in TreeView.ComponentState;
          if not WasUpdating then
            TreeView.Updating(); // Fixes #398
          try
            TreeView.DoColumnResize(AutoIndex);
          finally
            if not WasUpdating then
              TreeView.Updated();
          end;
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.AdjustDownColumn(P : TPoint) : TColumnIndex;

// Determines the column from the given position and returns it. If this column is allowed to be clicked then
// it is also kept for later use.

begin
  // Convert to local coordinates.
  Inc(P.Y, Header.Height);
  Result := ColumnFromPosition(P);
  if (Result > NoColumn) and (Result <> FDownIndex) and (coAllowClick in Items[Result].Options) and
    (coEnabled in Items[Result].Options) then
  begin
    if FDownIndex > NoColumn then
      Header.Invalidate(Items[FDownIndex]);
    FDownIndex := Result;
    FCheckBoxHit := Items[Result].HasImage and PtInRect(Items[Result].ImageRect, P) and Items[Result].CheckBox;
    Header.Invalidate(Items[FDownIndex]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.AdjustHoverColumn(P : TPoint) : Boolean;

// Determines the new hover column index and returns True if the index actually changed else False.

begin
  Result := GetNewIndex(P, FHoverIndex);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.AdjustPosition(Column : TVirtualTreeColumn; Position : Cardinal);

// Reorders the column position array so that the given column gets the given position.

var
  OldPosition : Cardinal;

begin
  OldPosition := Column.Position;
  if OldPosition <> Position then
  begin
    if OldPosition < Position then
    begin
      // column will be moved up so move down other entries
      Move(FPositionToIndex[OldPosition + 1], FPositionToIndex[OldPosition], (Position - OldPosition) * SizeOf(Cardinal));
    end
    else
    begin
      // column will be moved down so move up other entries
      Move(FPositionToIndex[Position], FPositionToIndex[Position + 1], (OldPosition - Position) * SizeOf(Cardinal));
    end;
    FPositionToIndex[Position] := Column.Index;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.CanSplitterResize(P : TPoint; Column : TColumnIndex) : Boolean;

begin
  Result := (Column > NoColumn) and ([coResizable, coVisible] * Items[Column].Options = [coResizable, coVisible]);
  DoCanSplitterResize(P, Column, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.DoCanSplitterResize(P : TPoint; Column : TColumnIndex; var Allowed : Boolean);

begin
  if Assigned(TreeView.OnCanSplitterResizeColumn) then
    TreeView.OnCanSplitterResizeColumn(Header, P, Column, Allowed);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.DrawButtonText(DC : HDC; Caption : string; Bounds : TRect; Enabled, Hot : Boolean;
  DrawFormat : Cardinal; WrapCaption : Boolean);

var
  TextSpace : Integer;
  Size      : TSize;

begin
  if not WrapCaption then
  begin
    // Do we need to shorten the caption due to limited space?
    GetTextExtentPoint32W(DC, PWideChar(Caption), Length(Caption), Size);
    TextSpace := Bounds.Right - Bounds.Left;
    if TextSpace < Size.cx then
      Caption := ShortenString(DC, Caption, TextSpace);
  end;

  SetBkMode(DC, TRANSPARENT);
  if not Enabled then
    if TreeView.VclStyleEnabled then
    begin
      SetTextColor(DC, ColorToRGB(TreeView.Colors.HeaderFontColor));
      WinApi.Windows.DrawTextW(DC, PWideChar(Caption), Length(Caption), Bounds, DrawFormat);
    end
    else
    begin
      OffsetRect(Bounds, 1, 1);
      SetTextColor(DC, ColorToRGB(clBtnHighlight));
      WinApi.Windows.DrawTextW(DC, PWideChar(Caption), Length(Caption), Bounds, DrawFormat);
      OffsetRect(Bounds, - 1, - 1);
      SetTextColor(DC, ColorToRGB(clBtnShadow));
      WinApi.Windows.DrawTextW(DC, PWideChar(Caption), Length(Caption), Bounds, DrawFormat);
    end
  else
  begin
    if Hot then
      SetTextColor(DC, ColorToRGB(TreeView.Colors.HeaderHotColor))
    else
      SetTextColor(DC, ColorToRGB(TreeView.Colors.HeaderFontColor));
    WinApi.Windows.DrawTextW(DC, PWideChar(Caption), Length(Caption), Bounds, DrawFormat);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.FixPositions;

// Fixes column positions after loading from DFM or Bidi mode change.

var
  I : Integer;

begin
  for I := 0 to Count - 1 do
    FPositionToIndex[Items[I].Position] := I;

  FNeedPositionsFix := False;
  UpdatePositions(True);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetColumnAndBounds(P : TPoint; var ColumnLeft, ColumnRight : Integer;
  Relative : Boolean = True) : Integer;

// Returns the column where the mouse is currently in as well as the left and right bound of
// this column (Left and Right are undetermined if no column is involved).

var
  I : Integer;

begin
  Result := InvalidColumn;
  if Relative and (P.X >= Header.Columns.GetVisibleFixedWidth) then
    ColumnLeft := - TreeView.EffectiveOffsetX
  else
    ColumnLeft := 0;

  if TreeView.UseRightToLeftAlignment then
    Inc(ColumnLeft, TreeView.ComputeRTLOffset(True));

  for I := 0 to Count - 1 do
    with Items[FPositionToIndex[I]] do
      if coVisible in FOptions then
      begin
        ColumnRight := ColumnLeft + FWidth;

        //fix: in right to left alignment, X can be in the
        //area on the left of first column which is OUT.
        if (P.X < ColumnLeft) and (I = 0) then
        begin
          Result := InvalidColumn;
          Exit;
        end;
        if P.X < ColumnRight then
        begin
          Result := FPositionToIndex[I];
          Exit;
        end;
        ColumnLeft := ColumnRight;
      end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetOwner : TPersistent;

begin
  Result := FHeader;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.HandleClick(P : TPoint; Button : TMouseButton; Force, DblClick : Boolean) : Boolean;

// Generates a click event if the mouse button has been released over the same column it was pressed first.
// Alternatively, Force might be set to True to indicate that the down index does not matter (right, middle and
// double click).
// Returns true if the click was handled, False otherwise.

var
  HitInfo       : TVTHeaderHitInfo;
  NewClickIndex : Integer;
  Menu          : TPopupMenu;
begin
  Result := False;
  if (csDesigning in TreeView.ComponentState) then
    Exit;
  // Convert vertical position to local coordinates.
  Inc(P.Y, Header.Height);
  NewClickIndex := ColumnFromPosition(P);
  with HitInfo do
  begin
    X := P.X;
    Y := P.Y;
    Shift := Header.GetShiftState;
    if DblClick then
      Shift := Shift + [ssDouble];
  end;
  HitInfo.Button := Button;

  if (NewClickIndex > NoColumn) and (coAllowClick in Items[NewClickIndex].Options) and
    ((NewClickIndex = FDownIndex) or Force) then
  begin
    FClickIndex := NewClickIndex;
    HitInfo.Column := NewClickIndex;
    HitInfo.HitPosition := [hhiOnColumn];

    if Items[NewClickIndex].HasImage and PtInRect(Items[NewClickIndex].ImageRect, P) then
    begin
      Include(HitInfo.HitPosition, hhiOnIcon);
      if Items[NewClickIndex].CheckBox then
      begin
        if Button = mbLeft then
          TreeView.UpdateColumnCheckState(Items[NewClickIndex]);
        Include(HitInfo.HitPosition, hhiOnCheckbox);
      end;
    end;
  end
  else
  begin
    FClickIndex := NoColumn;
    HitInfo.Column := NoColumn;
    HitInfo.HitPosition := [hhiNoWhere];
  end;

  if DblClick then
    TreeView.DoHeaderDblClick(HitInfo)
  else
  begin
    if (hoHeaderClickAutoSort in Header.Options) and (HitInfo.Button = mbLeft) and not (hhiOnCheckbox in HitInfo.HitPosition) and (HitInfo.Column >= 0) then
    begin
      // handle automatic setting of SortColumn and toggling of the sort order
      if HitInfo.Column <> Header.SortColumn then
      begin
        // set sort column
        Header.DoSetSortColumn(HitInfo.Column, Self[HitInfo.Column].DefaultSortDirection);
      end//if
      else
      begin
        // toggle sort direction
        if Header.SortDirection = sdDescending then
          Header.SortDirection := sdAscending
        else
          Header.SortDirection := sdDescending;
      end; //else
      Result := True;
    end;   //if

    if (Button = mbRight) then
    begin
      Dec(P.Y, Header.Height);      // popup menus at actual clicked point
      FreeAndNil(FColumnPopupMenu); // Attention: Do not free the TVTHeaderPopupMenu at the end of this method, otherwise the clikc events of the menu item will not be fired.
      Self.FDownIndex := NoColumn;
      Self.FTrackIndex := NoColumn;
      Self.FCheckBoxHit := False;
      Menu := Header.DoGetPopupMenu(Self.ColumnFromPosition(Point(P.X, P.Y + Integer(TreeView.Height))), P);
      if Assigned(Menu) then
      begin
        TreeView.StopTimer(ScrollTimer);
        TreeView.StopTimer(HeaderTimer);
        Header.Columns.SetHoverIndex(NoColumn);
        TreeView.DoStateChange([], [tsScrollPending, tsScrolling]);

        Menu.PopupComponent := TreeView;
        With TreeView.ClientToScreen(P) do
          Menu.Popup(X, Y);
        Result := True;
      end
      else if (hoAutoColumnPopupMenu in Header.Options) then
      begin
        FColumnPopupMenu := TVTHeaderPopupMenu.Create(TreeView);
        TVTHeaderPopupMenu(FColumnPopupMenu).OnAddHeaderPopupItem := HeaderPopupMenuAddHeaderPopupItem;
        TVTHeaderPopupMenu(FColumnPopupMenu).OnColumnChange := HeaderPopupMenuColumnChange;
        FColumnPopupMenu.PopupComponent := TreeView;
        if (hoDblClickResize in Header.Options) and ((TreeView.ChildCount[nil] > 0) or (hoAutoResizeInclCaption in Header.Options)) then
          TVTHeaderPopupMenu(FColumnPopupMenu).Options := TVTHeaderPopupMenu(FColumnPopupMenu).Options + [poResizeToFitItem]
        else
          TVTHeaderPopupMenu(FColumnPopupMenu).Options := TVTHeaderPopupMenu(FColumnPopupMenu).Options - [poResizeToFitItem];
        With TreeView.ClientToScreen(P) do
          FColumnPopupMenu.Popup(X, Y);
        Result := True;
      end; // if  hoAutoColumnPopupMenu
    end;   //if mbRight
    TreeView.DoHeaderClick(HitInfo);
  end;     //else (not DblClick)

  if not (hhiNoWhere in HitInfo.HitPosition) then
    Header.Invalidate(Items[NewClickIndex]);
  if (FClickIndex > NoColumn) and (FClickIndex <> NewClickIndex) then
    Header.Invalidate(Items[FClickIndex]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.HeaderPopupMenuAddHeaderPopupItem(const Sender : TObject; const Column : TColumnIndex; var Cmd : TAddPopupItemType);
begin
  TBaseVirtualTreeCracker(Sender).DoHeaderAddPopupItem(Column, Cmd);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.HeaderPopupMenuColumnChange(const Sender : TObject; const Column : TColumnIndex; Visible : Boolean);
begin
  TBaseVirtualTreeCracker(Sender).DoColumnVisibilityChanged(Column, Visible);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.IndexChanged(OldIndex, NewIndex : Integer);

// Called by a column when its index in the collection changes. If NewIndex is -1 then the column is
// about to be removed, otherwise it is moved to a new index.
// The method will then update the position array to reflect the change.

var
  I         : Integer;
  Increment : Integer;
  Lower,
    Upper   : Integer;

begin
  if NewIndex = - 1 then
  begin
    // Find position in the array with the old index.
    Upper := High(FPositionToIndex);
    for I := 0 to Upper do
    begin
      if FPositionToIndex[I] = OldIndex then
      begin
        // Index found. Move all higher entries one step down and remove the last entry.
        if I < Upper then
          Move(FPositionToIndex[I + 1], FPositionToIndex[I], (Upper - I) * SizeOf(TColumnIndex));
      end;
      // Decrease all indices, which are greater than the index to be deleted.
      if FPositionToIndex[I] > OldIndex then
        Dec(FPositionToIndex[I]);
    end;
    SetLength(FPositionToIndex, High(FPositionToIndex));
  end
  else
  begin
    if OldIndex < NewIndex then
      Increment := - 1
    else
      Increment := 1;

    Lower := Min(OldIndex, NewIndex);
    Upper := Max(OldIndex, NewIndex);
    for I := 0 to High(FPositionToIndex) do
    begin
      if (FPositionToIndex[I] >= Lower) and (FPositionToIndex[I] < Upper) then
        Inc(FPositionToIndex[I], Increment)
      else
        if FPositionToIndex[I] = OldIndex then
        FPositionToIndex[I] := NewIndex;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.InitializePositionArray;

// Ensures that the column position array contains as many entries as columns are defined.
// The array is resized and initialized with default values if needed.

var
  I, OldSize : Integer;
  Changed    : Boolean;

begin
  if Count <> Length(FPositionToIndex) then
  begin
    OldSize := Length(FPositionToIndex);
    SetLength(FPositionToIndex, Count);
    if Count > OldSize then
    begin
      // New items have been added, just set their position to the same as their index.
      for I := OldSize to Count - 1 do
        FPositionToIndex[I] := I;
    end
    else
    begin
      // Items have been deleted, so reindex remaining entries by decrementing values larger than the highest
      // possible index until no entry is higher than this limit.
      repeat
        Changed := False;
        for I := 0 to Count - 1 do
          if FPositionToIndex[I] >= Count then
          begin
            Dec(FPositionToIndex[I]);
            Changed := True;
          end;
      until not Changed;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.Notify(Item : TCollectionItem; Action : System.Classes.TCollectionNotification);
var
  I : Integer;
begin
  if Action in [cnExtracting, cnDeleting] then
  begin
    // Adjust all positions larger than the deleted column's position. Fixes #959
    for I := 0 to Count - 1 do
    begin
      if Items[I].Position > TVirtualTreeColumn(Item).Position then
        Items[I].Position := Items[I].Position - 1;
    end; //for I

    with TreeView do
      if not (csLoading in ComponentState) and (FocusedColumn = Item.Index) then
        InternalSetFocusedColumn(NoColumn); //bypass side effects in SetFocusedColumn
  end;                                      // if cnDeleting
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.ReorderColumns(RTL : Boolean);

var
  I : Integer;

begin
  if RTL then
  begin
    for I := 0 to Count - 1 do
      FPositionToIndex[I] := Count - I - 1;
  end
  else
  begin
    for I := 0 to Count - 1 do
      FPositionToIndex[I] := I;
  end;

  UpdatePositions(True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.SetHoverIndex(Index : TColumnIndex);
begin
  FHoverIndex := index;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.EndUpdate;
begin
  InitializePositionArray();
  FixPositions(); // Accept the cuurent order. See issue #753
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.Update(Item : TCollectionItem);

begin
  // This is the only place which gets notified when a new column has been added or removed
  // and we need this event to adjust the column position array.
  InitializePositionArray;
  if csLoading in TreeView.ComponentState then
    FNeedPositionsFix := True
  else
    UpdatePositions;

  // The first column which is created is by definition also the main column.
  if (Count > 0) and (Header.MainColumn < 0) then
    Header.MainColumn := 0;

  if not (csLoading in TreeView.ComponentState) and not (hsLoading in Header.States) then
  begin
    with Header do
    begin
      if hoAutoResize in Options then
        AdjustAutoSize(InvalidColumn);
      if Assigned(Item) then
        Invalidate(Item as TVirtualTreeColumn)
      else
        if Self.TreeView.HandleAllocated then
      begin
        Self.TreeView.UpdateHorizontalScrollBar(False);
        Invalidate(nil);
        TreeView.Invalidate;
      end;

      if not (Self.TreeView.IsUpdating) then
        // This is mainly to let the designer know when a change occurs at design time which
        // doesn't involve the object inspector (like column resizing with the mouse).
        // This does NOT include design time code as the communication is done via an interface.
        Self.TreeView.UpdateDesigner;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.UpdatePositions(Force : Boolean = False);

// Recalculates the left border of every column and updates their position property according to the
// PostionToIndex array which primarily determines where each column is placed visually.

var
  I, RunningPos : Integer;

begin
  if not (csDestroying in TreeView.ComponentState) and not FNeedPositionsFix and (Force or (UpdateCount = 0)) then
  begin
    RunningPos := 0;
    for I := 0 to High(FPositionToIndex) do
      with Items[FPositionToIndex[I]] do
      begin
        FPosition := I;
        FLeft := RunningPos;
        if coVisible in FOptions then
          Inc(RunningPos, FWidth);
      end;
    TreeView.UpdateHorizontalScrollBar(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.Add : TVirtualTreeColumn;

begin
  Assert(GetCurrentThreadId = MainThreadId, 'UI controls may only be changed in UI thread.');
  Result := TVirtualTreeColumn(inherited Add);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.AnimatedResize(Column : TColumnIndex; NewWidth : Integer);

// Resizes the given column animated by scrolling the window DC.

var
  OldWidth    : Integer;
  DC          : HDC;
  I,
    Steps,
    DX        : Integer;
  HeaderScrollRect,
    ScrollRect,
    R         : TRect;

  NewBrush,
    LastBrush : HBRUSH;

begin
  if not IsValidColumn(Column) then
    Exit; // Just in case.

  // Make sure the width constrains are considered.
  if NewWidth < Items[Column].MinWidth then
    NewWidth := Items[Column].MinWidth;
  if NewWidth > Items[Column].MaxWidth then
    NewWidth := Items[Column].MaxWidth;

  OldWidth := Items[Column].Width;
  // Nothing to do if the width is the same.
  if OldWidth <> NewWidth then
  begin
    if not ((hoDisableAnimatedResize in Header.Options) or
      (coDisableAnimatedResize in Items[Column].Options)) then
    begin
      DC := GetWindowDC(TreeView.Handle);
      with TreeView do
        try
          Steps := 32;
          DX := (NewWidth - OldWidth) div Steps;

        // Determination of the scroll rectangle is a bit complicated since we neither want
        // to scroll the scrollbars nor the border of the treeview window.
          HeaderScrollRect := HeaderRect;
          ScrollRect := HeaderScrollRect;
        // Exclude the header itself from scrolling.
          ScrollRect.Top := ScrollRect.Bottom;
          ScrollRect.Bottom := ScrollRect.Top + ClientHeight;
          ScrollRect.Right := ScrollRect.Left + ClientWidth;
          with Items[Column] do
            Inc(ScrollRect.Left, FLeft + FWidth);
          HeaderScrollRect.Left := ScrollRect.Left;
          HeaderScrollRect.Right := ScrollRect.Right;

        // When the new width is larger then avoid artefacts on the left hand side
        // by deleting a small stripe
          if NewWidth > OldWidth then
          begin
            R := ScrollRect;
            NewBrush := CreateSolidBrush(ColorToRGB(Color));
            LastBrush := SelectObject(DC, NewBrush);
            R.Right := R.Left + DX;
            FillRect(DC, R, NewBrush);
            SelectObject(DC, LastBrush);
            DeleteObject(NewBrush);
          end
          else
          begin
            Inc(HeaderScrollRect.Left, DX);
            Inc(ScrollRect.Left, DX);
          end;

          for I := 0 to Steps - 1 do
          begin
            ScrollDC(DC, DX, 0, HeaderScrollRect, HeaderScrollRect, 0, nil);
            Inc(HeaderScrollRect.Left, DX);
            ScrollDC(DC, DX, 0, ScrollRect, ScrollRect, 0, nil);
            Inc(ScrollRect.Left, DX);
            Sleep(1);
          end;
        finally
          ReleaseDC(Handle, DC);
        end;
    end;
    Items[Column].Width := NewWidth;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.Assign(Source : TPersistent);

begin
  // Let the collection class assign the items.
  inherited;

  if Source is TVirtualTreeColumns then
  begin
    // Copying the position array is the only needed task here.
    FPositionToIndex := Copy(TVirtualTreeColumns(Source).FPositionToIndex, 0, MaxInt);

    // Make sure the left edges are correct after assignment.
    FNeedPositionsFix := False;
    UpdatePositions(True);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.Clear;

begin
  FClearing := True;
  try
    TreeView.CancelEditNode;

    // Since we're freeing all columns, the following have to be true when we're done.
    FHoverIndex := NoColumn;
    FDownIndex := NoColumn;
    FTrackIndex := NoColumn;
    FClickIndex := NoColumn;
    FCheckBoxHit := False;

    with TVTHeaderCracker(Header) do
      if not (hsLoading in States) then
      begin
        InternalSetAutoSizeIndex(NoColumn); //bypass side effects in SetAutoSizeColumn
        MainColumn := NoColumn;
        InternalSetSortColumn(NoColumn);    //bypass side effects in SetSortColumn
      end;

    with TreeView do
      if not (csLoading in ComponentState) then
        InternalSetFocusedColumn(NoColumn); //bypass side effects in SetFocusedColumn

    inherited Clear;
  finally
    FClearing := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.ColumnFromPosition(P : TPoint; Relative : Boolean = True) : TColumnIndex;

// Determines the current column based on the position passed in P.

var
  I, Sum : Integer;

begin
  Result := InvalidColumn;

  // The position must be within the header area, but we extend the vertical bounds to the entire treeview area.
  if (P.X >= 0) and (P.Y >= 0) and (P.Y <= TreeView.Height) then
    with FHeader, TreeView do
    begin
      if Relative and (P.X >= GetVisibleFixedWidth) then
        Sum := - EffectiveOffsetX
      else
        Sum := 0;

      if UseRightToLeftAlignment then
        Inc(Sum, ComputeRTLOffset(True));

      for I := 0 to Count - 1 do
        if coVisible in Items[FPositionToIndex[I]].Options then
        begin
          Inc(Sum, Items[FPositionToIndex[I]].Width);
          if P.X < Sum then
          begin
            Result := FPositionToIndex[I];
            Break;
          end;
        end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.ColumnFromPosition(PositionIndex : TColumnPosition) : TColumnIndex;

// Returns the index of the column at the given position.

begin
  if Integer(PositionIndex) < Length(FPositionToIndex) then
    Result := FPositionToIndex[PositionIndex]
  else
    Result := NoColumn;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.Equals(OtherColumnsObj : TObject) : Boolean;

// Compares itself with the given set of columns and returns True if all published properties are the same
// (including column order), otherwise False is returned.

var
  I            : Integer;
  OtherColumns : TVirtualTreeColumns;

begin
  if not (OtherColumnsObj is TVirtualTreeColumns) then
  begin
    Result := False;
    Exit;
  end;

  OtherColumns := TVirtualTreeColumns(OtherColumnsObj);

  // Same number of columns?
  Result := OtherColumns.Count = Count;
  if Result then
  begin
    // Same order of columns?
    Result := CompareMem(Pointer(FPositionToIndex), Pointer(OtherColumns.FPositionToIndex),
      Length(FPositionToIndex) * SizeOf(TColumnIndex));
    if Result then
    begin
      for I := 0 to Count - 1 do
        if not Items[I].Equals(OtherColumns[I]) then
        begin
          Result := False;
          Break;
        end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.GetColumnBounds(Column : TColumnIndex; var Left, Right : Integer);

// Returns the left and right bound of the given column. If Column is NoColumn then the entire client width is returned.

begin
  if Column <= NoColumn then
  begin
    Left := 0;
    Right := TreeView.ClientWidth;
  end
  else
  begin
    Left := Items[Column].Left;
    Right := Left + Items[Column].Width;
    if TreeView.UseRightToLeftAlignment then
    begin
      Inc(Left, TreeView.ComputeRTLOffset(True));
      Inc(Right, TreeView.ComputeRTLOffset(True));
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetScrollWidth : Integer;

// Returns the average width of all visible, non-fixed columns. If there is no such column the indent is returned.

var
  I                 : Integer;
  ScrollColumnCount : Integer;

begin

  Result := 0;

  ScrollColumnCount := 0;
  for I := 0 to Header.Columns.Count - 1 do
  begin
    if ([coVisible, coFixed] * Header.Columns[I].Options = [coVisible]) then
    begin
      Inc(Result, Header.Columns[I].Width);
      Inc(ScrollColumnCount);
    end;
  end;

  if ScrollColumnCount > 0 then // use average width
    Result := Round(Result / ScrollColumnCount)
  else                          // use indent
    Result := Integer(TreeView.Indent);

end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetFirstVisibleColumn(ConsiderAllowFocus : Boolean = False) : TColumnIndex;

// Returns the index of the first visible column or "InvalidColumn" if either no columns are defined or
// all columns are hidden.
// If ConsiderAllowFocus is True then the column has not only to be visible but also focus has to be allowed.

var
  I : Integer;

begin
  Result := InvalidColumn;
  if (UpdateCount > 0) or (csLoading in TreeView.ComponentState) then
    Exit; // See issue #760
  for I := 0 to Count - 1 do
    if (coVisible in Items[FPositionToIndex[I]].Options) and
      ((not ConsiderAllowFocus) or
      (coAllowFocus in Items[FPositionToIndex[I]].Options)
      ) then
    begin
      Result := FPositionToIndex[I];
      Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetLastVisibleColumn(ConsiderAllowFocus : Boolean = False) : TColumnIndex;

// Returns the index of the last visible column or "InvalidColumn" if either no columns are defined or
// all columns are hidden.
// If ConsiderAllowFocus is True then the column has not only to be visible but also focus has to be allowed.

var
  I : Integer;

begin
  Result := InvalidColumn;
  if (UpdateCount > 0) or (csLoading in TreeView.ComponentState) then
    Exit; // See issue #760
  for I := Count - 1 downto 0 do
    if (coVisible in Items[FPositionToIndex[I]].Options) and
      ((not ConsiderAllowFocus) or
      (coAllowFocus in Items[FPositionToIndex[I]].Options)
      ) then
    begin
      Result := FPositionToIndex[I];
      Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetFirstColumn : TColumnIndex;

// Returns the first column in display order.

begin
  if Count = 0 then
    Result := InvalidColumn
  else
    Result := FPositionToIndex[0];
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetNextColumn(Column : TColumnIndex) : TColumnIndex;

// Returns the next column in display order. Column is the index of an item in the collection (a column).

var
  Position : Integer;

begin
  if Column < 0 then
    Result := InvalidColumn
  else
  begin
    Position := Items[Column].Position;
    if Position < Count - 1 then
      Result := FPositionToIndex[Position + 1]
    else
      Result := InvalidColumn;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetNextVisibleColumn(Column : TColumnIndex; ConsiderAllowFocus : Boolean = False) : TColumnIndex;

// Returns the next visible column in display order, Column is an index into the columns list.
// If ConsiderAllowFocus is True then the column has not only to be visible but also focus has to be allowed.

begin
  Result := Column;
  repeat
    Result := GetNextColumn(Result);
  until (Result = InvalidColumn) or
    ((coVisible in Items[Result].Options) and
    ((not ConsiderAllowFocus) or
    (coAllowFocus in Items[Result].Options)
    )
    );
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetPreviousColumn(Column : TColumnIndex) : TColumnIndex;

// Returns the previous column in display order, Column is an index into the columns list.

var
  Position : Integer;

begin
  if Column < 0 then
    Result := InvalidColumn
  else
  begin
    Position := Items[Column].Position;
    if Position > 0 then
      Result := FPositionToIndex[Position - 1]
    else
      Result := InvalidColumn;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetPreviousVisibleColumn(Column : TColumnIndex; ConsiderAllowFocus : Boolean = False) : TColumnIndex;

// Returns the previous visible column in display order, Column is an index into the columns list.
// If ConsiderAllowFocus is True then the column has not only to be visible but also focus has to be allowed.

begin
  Result := Column;
  repeat
    Result := GetPreviousColumn(Result);
  until (Result = InvalidColumn) or
    ((coVisible in Items[Result].Options) and
    ((not ConsiderAllowFocus) or
    (coAllowFocus in Items[Result].Options)
    )
    );
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetVisibleColumns : TColumnsArray;

// Returns a list of all currently visible columns in actual order.

var
  I, Counter : Integer;

begin
  SetLength(Result, Count);
  Counter := 0;

  for I := 0 to Count - 1 do
    if coVisible in Items[FPositionToIndex[I]].Options then
    begin
      Result[Counter] := Items[FPositionToIndex[I]];
      Inc(Counter);
    end;
  // Set result length to actual visible count.
  SetLength(Result, Counter);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.GetVisibleFixedWidth : Integer;

// Determines the horizontal space all visible and fixed columns occupy.

var
  I : Integer;

begin
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    if Items[I].Options * [coVisible, coFixed] = [coVisible, coFixed] then
      Inc(Result, Items[I].Width);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.IsValidColumn(Column : TColumnIndex) : Boolean;

// Determines whether the given column is valid or not, that is, whether it is one of the current columns.

begin
  Result := (Column > NoColumn) and (Column < Count);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.LoadFromStream(const Stream : TStream; Version : Integer);

var
  I,
    ItemCount : Integer;

begin
  Clear;
  Stream.ReadBuffer(ItemCount, SizeOf(ItemCount));
  // number of columns
  if ItemCount > 0 then
  begin
    BeginUpdate;
    try
      for I := 0 to ItemCount - 1 do
        Add.LoadFromStream(Stream, Version);
      SetLength(FPositionToIndex, ItemCount);
      Stream.ReadBuffer(FPositionToIndex[0], ItemCount * SizeOf(TColumnIndex));
      UpdatePositions(True);
    finally
      EndUpdate;
    end;
  end;

  // Data introduced with header stream version 5
  if Version > 4 then
    Stream.ReadBuffer(FDefaultWidth, SizeOf(FDefaultWidth));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.PaintHeader(DC : HDC; R : TRect; HOffset : Integer);

// Backward compatible header paint method. This method takes care of visually moving floating columns

var
  VisibleFixedWidth : Integer;
  RTLOffset         : Integer;

  procedure PaintFixedArea;

  begin
    if VisibleFixedWidth > 0 then
      PaintHeader(FHeaderBitmap.Canvas,
        Rect(0, 0, Min(R.Right, VisibleFixedWidth), R.Bottom - R.Top),
        Point(R.Left, R.Top), RTLOffset);
  end;

begin
  // Adjust size of the header bitmap
  with TWithSafeRect(TreeView.HeaderRect) do
  begin
    FHeaderBitmap.SetSize(Max(Right, R.Right - R.Left), Bottom);
  end;

  VisibleFixedWidth := GetVisibleFixedWidth;

  // Consider right-to-left directionality.
  if TreeView.UseRightToLeftAlignment then
    RTLOffset := TreeView.ComputeRTLOffset
  else
    RTLOffset := 0;

  if RTLOffset = 0 then
    PaintFixedArea;

  // Paint the floating part of the header.
  PaintHeader(FHeaderBitmap.Canvas,
    Rect(VisibleFixedWidth - HOffset, 0, R.Right + VisibleFixedWidth - HOffset, R.Bottom - R.Top),
    Point(R.Left + VisibleFixedWidth, R.Top), RTLOffset);

  // In case of right-to-left directionality we paint the fixed part last.
  if RTLOffset <> 0 then
    PaintFixedArea;

  // Blit the result to target.
  with TWithSafeRect(R) do
    BitBlt(DC, Left, Top, Right - Left, Bottom - Top, FHeaderBitmap.Canvas.Handle, Left, Top, SRCCOPY);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.PaintHeader(TargetCanvas : TCanvas; R : TRect; const Target : TPoint;
  RTLOffset : Integer = 0);

// Main paint method to draw the header.
// This procedure will paint the a slice (given in R) out of HeaderRect into TargetCanvas starting at position Target.
// This function does not offer the option to visually move floating columns due to scrolling. To accomplish this you
// need to call this method twice.

var
  Run : TColumnIndex;
  RightBorderFlag,
    NormalButtonStyle,
    NormalButtonFlags,
    PressedButtonStyle,
    PressedButtonFlags,
    RaisedButtonStyle,
    RaisedButtonFlags : Cardinal;
  Images              : TCustomImageList;
  OwnerDraw,
    AdvancedOwnerDraw : Boolean;
  PaintInfo           : THeaderPaintInfo;
  RequestedElements,
    ActualElements    : THeaderPaintElements;

  //--------------- local functions -------------------------------------------

  procedure PrepareButtonStyles;

  // Prepare the button styles and flags for later usage.

  begin
    RaisedButtonStyle := 0;
    RaisedButtonFlags := 0;
    case Header.Style of
      hsThickButtons :
        begin
          NormalButtonStyle := BDR_RAISEDINNER or BDR_RAISEDOUTER;
          NormalButtonFlags := BF_LEFT or BF_TOP or BF_BOTTOM or BF_MIDDLE or BF_SOFT or BF_ADJUST;
          PressedButtonStyle := BDR_RAISEDINNER or BDR_RAISEDOUTER;
          PressedButtonFlags := NormalButtonFlags or BF_RIGHT or BF_FLAT or BF_ADJUST;
        end;
      hsFlatButtons :
        begin
          NormalButtonStyle := BDR_RAISEDINNER;
          NormalButtonFlags := BF_LEFT or BF_TOP or BF_BOTTOM or BF_MIDDLE or BF_ADJUST;
          PressedButtonStyle := BDR_SUNKENOUTER;
          PressedButtonFlags := BF_RECT or BF_MIDDLE or BF_ADJUST;
        end;
    else
      // hsPlates or hsXPStyle, values are not used in the latter case
      begin
        NormalButtonStyle := BDR_RAISEDINNER;
        NormalButtonFlags := BF_RECT or BF_MIDDLE or BF_SOFT or BF_ADJUST;
        PressedButtonStyle := BDR_SUNKENOUTER;
        PressedButtonFlags := BF_RECT or BF_MIDDLE or BF_ADJUST;
        RaisedButtonStyle := BDR_RAISEDINNER;
        RaisedButtonFlags := BF_LEFT or BF_TOP or BF_BOTTOM or BF_MIDDLE or BF_ADJUST;
      end;
    end;
  end;

  //---------------------------------------------------------------------------

  procedure DrawBackground;

  // Draw the header background.

  var
    BackgroundRect : TRect;
    Details        : TThemedElementDetails;
    Theme          : HTHEME;
  begin
    BackgroundRect := Rect(Target.X, Target.Y, Target.X + R.Right - R.Left, Target.Y + Header.Height);

    with TargetCanvas do
    begin
      if hpeBackground in RequestedElements then
      begin
        PaintInfo.PaintRectangle := BackgroundRect;
        TreeView.DoAdvancedHeaderDraw(PaintInfo, [hpeBackground]);
      end
      else
      begin
        if (TreeView.VclStyleEnabled and (seClient in TreeView.StyleElements)) then
        begin
          Details := StyleServices.GetElementDetails(thHeaderItemRightNormal);
          StyleServices.DrawElement(Handle, Details, BackgroundRect, @BackgroundRect {$IF CompilerVersion  >= 34}, TreeView.FCurrentPPI{$IFEND});
        end
        else
          if tsUseThemes in TreeView.TreeStates then
        begin
          Theme := OpenThemeData(TreeView.Handle, 'HEADER');
          DrawThemeBackground(Theme, Handle, HP_HEADERITEM, HIS_NORMAL, BackgroundRect, nil);
          CloseThemeData(Theme);
        end
        else
        begin
          Brush.Color := Header.Background;
          FillRect(BackgroundRect);
        end;
      end;
    end;
  end;

  //---------------------------------------------------------------------------

  procedure PaintColumnHeader(AColumn : TColumnIndex; ATargetRect : TRect);

  // Draw a single column to TargetRect. The clipping rect needs to be set before
  // this procedure is called.

  var
    SavedDC        : Integer;
    ColCaptionText : string;
    ColImageInfo   : TVTImageInfo;
    Glyph          : TThemedHeader;
    Details        : TThemedElementDetails;
    WrapCaption    : Boolean;
    DrawFormat     : Cardinal;
    Pos            : TRect;
    DrawHot        : Boolean;
    ImageWidth     : Integer;
    Theme          : HTHEME;
    IdState        : Integer;
  begin
    ColImageInfo.Ghosted := False;
    PaintInfo.Column := Items[AColumn];
    with PaintInfo, Column do
    begin
      IsHoverIndex := (AColumn = FHoverIndex) and (hoHotTrack in Header.Options) and (coEnabled in Options);
      IsDownIndex := (AColumn = FDownIndex) and not FCheckBoxHit;

      if (coShowDropMark in FOptions) and (AColumn = FDropTarget) and (AColumn <> FDragIndex) then
      begin
        if FDropBefore then
          DropMark := dmmLeft
        else
          DropMark := dmmRight;
      end
      else
        DropMark := dmmNone;

      //Fix for issue 643
      //Do not show the left drop mark if the position to drop is just preceding the target which means
      //the dragged column will stay where it is
      if (DropMark = dmmLeft) and (Items[FDragIndex].Position = TColumnPosition(Max(Integer(Items[FDropTarget].Position) - 1, 0)))
      then
        DropMark := dmmNone
      else
      //Do not show the right drop mark if the position to drop is just following the target which means
      //the dragged column will stay where it is
        if (DropMark = dmmRight) and (Items[FDragIndex].Position = Items[FDropTarget].Position + 1)
        then
          DropMark := dmmNone;

      IsEnabled := (coEnabled in FOptions) and (TreeView.Enabled);
      ShowHeaderGlyph := (hoShowImages in Header.Options) and ((Assigned(Images) and (FImageIndex > - 1)) or FCheckBox);
      ShowSortGlyph := (AColumn = Header.SortColumn) and (hoShowSortGlyphs in Header.Options);
      WrapCaption := coWrapCaption in FOptions;

      PaintRectangle := ATargetRect;

      // This path for text columns or advanced owner draw.
      if (Style = vsText) or not OwnerDraw or AdvancedOwnerDraw then
      begin
        // See if the application wants to draw part of the header itself.
        RequestedElements := [];
        if AdvancedOwnerDraw then
        begin
          PaintInfo.Column := Items[AColumn];
          TreeView.DoHeaderDrawQueryElements(PaintInfo, RequestedElements);
        end;

        if ShowRightBorder or (AColumn < Count - 1) then
          RightBorderFlag := BF_RIGHT
        else
          RightBorderFlag := 0;

        if hpeBackground in RequestedElements then
          TreeView.DoAdvancedHeaderDraw(PaintInfo, [hpeBackground])
        else
        begin
          if TreeView.VclStyleEnabled and (seClient in TreeView.StyleElements) then
          begin
            if IsDownIndex then
              Details := StyleServices.GetElementDetails(thHeaderItemPressed)
            else
              if IsHoverIndex then
              Details := StyleServices.GetElementDetails(thHeaderItemHot)
            else
              Details := StyleServices.GetElementDetails(thHeaderItemNormal);
            StyleServices.DrawElement(TargetCanvas.Handle, Details, PaintRectangle, @PaintRectangle{$IF CompilerVersion >= 34}, TreeView.CurrentPPI{$IFEND});
          end
          else
          begin
            if tsUseThemes in TreeView.TreeStates then
            begin
              Theme := OpenThemeData(TreeView.Handle, 'HEADER');
              if IsDownIndex then
                IdState := HIS_PRESSED
              else
                if IsHoverIndex then
                IdState := HIS_HOT
              else
                IdState := HIS_NORMAL;
              DrawThemeBackground(Theme, TargetCanvas.Handle, HP_HEADERITEM, IdState, PaintRectangle, nil);
              CloseThemeData(Theme);
            end
            else
              if IsDownIndex then
              DrawEdge(TargetCanvas.Handle, PaintRectangle, PressedButtonStyle, PressedButtonFlags)
            else
                  // Plates have the special case of raising on mouse over.
              if (Header.Style = hsPlates) and IsHoverIndex and
                (coAllowClick in FOptions) and (coEnabled in FOptions) then
                DrawEdge(TargetCanvas.Handle, PaintRectangle, RaisedButtonStyle,
                  RaisedButtonFlags or RightBorderFlag)
              else
                DrawEdge(TargetCanvas.Handle, PaintRectangle, NormalButtonStyle,
                  NormalButtonFlags or RightBorderFlag);
          end;
        end;

        PaintRectangle := ATargetRect;

        // calculate text and glyph position
        InflateRect(PaintRectangle, - 2, - 2);
        DrawFormat := DT_TOP or DT_NOPREFIX;
        case CaptionAlignment of
          taLeftJustify :
            DrawFormat := DrawFormat or DT_LEFT;
          taRightJustify :
            DrawFormat := DrawFormat or DT_RIGHT;
          taCenter :
            DrawFormat := DrawFormat or DT_CENTER;
        end;
        if UseRightToLeftReading then
          DrawFormat := DrawFormat + DT_RTLREADING;
        ComputeHeaderLayout(PaintInfo, DrawFormat);

        // Move glyph and text one pixel to the right and down to simulate a pressed button.
        if IsDownIndex then
        begin
          OffsetRect(TextRectangle, 1, 1);
          Inc(GlyphPos.X);
          Inc(GlyphPos.Y);
          Inc(SortGlyphPos.X);
          Inc(SortGlyphPos.Y);
        end;

        // Advanced owner draw allows to paint elements, which would normally not be painted (because of space
        // limitations, empty captions etc.).
        ActualElements := RequestedElements * [hpeHeaderGlyph, hpeSortGlyph, hpeDropMark, hpeText, hpeOverlay];

        // main glyph
        FHasImage := False;
        if Assigned(Images) then
          ImageWidth := Images.Width
        else
          ImageWidth := 0;

        if not (hpeHeaderGlyph in ActualElements) and ShowHeaderGlyph and
          (not ShowSortGlyph or (FBiDiMode <> bdLeftToRight) or (GlyphPos.X + ImageWidth <= SortGlyphPos.X)) then
        begin
          if not FCheckBox then
          begin
            ColImageInfo.Images := Images;
            Images.Draw(TargetCanvas, GlyphPos.X, GlyphPos.Y, FImageIndex, IsEnabled);
          end
          else
          begin
            with TreeView do
            begin
              ColImageInfo.Images := CheckImages;
              ColImageInfo.Index := GetCheckImage(nil, FCheckType, FCheckState, IsEnabled);
              ColImageInfo.XPos := GlyphPos.X;
              ColImageInfo.YPos := GlyphPos.Y;
              PaintCheckImage(TargetCanvas, ColImageInfo, False);
            end;
          end;

          FHasImage := True;
          with TWithSafeRect(FImageRect) do
          begin
            Left := GlyphPos.X;
            Top := GlyphPos.Y;
            Right := Left + ColImageInfo.Images.Width;
            Bottom := Top + ColImageInfo.Images.Height;
          end;
        end;

        // caption
        if WrapCaption then
          ColCaptionText := FCaptionText
        else
          ColCaptionText := Text;
        if IsHoverIndex and TreeView.VclStyleEnabled then
          DrawHot := True
        else
          DrawHot := (IsHoverIndex and (hoHotTrack in Header.Options) and not (tsUseThemes in TreeView.TreeStates));
        if not (hpeText in ActualElements) and (Length(Text) > 0) then
          DrawButtonText(TargetCanvas.Handle, ColCaptionText, TextRectangle, IsEnabled, DrawHot, DrawFormat, WrapCaption);

        // sort glyph
        if not (hpeSortGlyph in ActualElements) and ShowSortGlyph then
        begin
          if tsUseExplorerTheme in TreeView.TreeStates then
          begin
            Pos.TopLeft := SortGlyphPos;
            Pos.Right := Pos.Left + SortGlyphSize.cx;
            Pos.Bottom := Pos.Top + SortGlyphSize.cy;
            if Header.SortDirection = sdAscending then
              Glyph := thHeaderSortArrowSortedUp
            else
              Glyph := thHeaderSortArrowSortedDown;
            Details := StyleServices.GetElementDetails(Glyph);
            if not StyleServices.DrawElement(TargetCanvas.Handle, Details, Pos, @Pos {$IF CompilerVersion  >= 34}, TreeView.CurrentPPI {$IFEND}) then
              PaintInfo.DrawSortArrow(Header.SortDirection);
          end
          else
          begin
            PaintInfo.DrawSortArrow(Header.SortDirection);
          end;
        end;

        // Show an indication if this column is the current drop target in a header drag operation.
        if not (hpeDropMark in ActualElements) and (DropMark <> dmmNone) then
        begin
          PaintInfo.DrawDropMark();
        end;

        if ActualElements <> [] then
        begin
          SavedDC := SaveDC(TargetCanvas.Handle);
          TreeView.DoAdvancedHeaderDraw(PaintInfo, ActualElements);
          RestoreDC(TargetCanvas.Handle, SavedDC);
        end;
      end
      else // Let application draw the header.
        TreeView.DoHeaderDraw(TargetCanvas, Items[AColumn], PaintRectangle, IsHoverIndex, IsDownIndex,
          DropMark);
    end;
  end;

  //--------------- end local functions ---------------------------------------

var
  TargetRect : TRect;
  MaxX       : Integer;

begin
  if IsRectEmpty(R) then
    Exit;

  // If both draw posibillities are specified then prefer the advanced way.
  AdvancedOwnerDraw := (hoOwnerDraw in Header.Options) and Assigned(TreeView.OnAdvancedHeaderDraw) and
    Assigned(TreeView.OnHeaderDrawQueryElements) and not (csDesigning in TreeView.ComponentState);
  OwnerDraw := (hoOwnerDraw in Header.Options) and Assigned(TreeView.OnHeaderDraw) and
    not (csDesigning in TreeView.ComponentState) and not AdvancedOwnerDraw;

  ZeroMemory(@PaintInfo, SizeOf(PaintInfo));
  PaintInfo.TargetCanvas := TargetCanvas;

  with PaintInfo, TargetCanvas do
  begin
    // Use shortcuts for the images and the font.
    Images := Header.Images;
    Font := Header.Font;

    PrepareButtonStyles;

    // At first, query the application which parts of the header it wants to draw on its own.
    RequestedElements := [];
    if AdvancedOwnerDraw then
    begin
      PaintRectangle := R;
      Column := nil;
      TreeView.DoHeaderDrawQueryElements(PaintInfo, RequestedElements);
    end;

    // Draw the background.
    DrawBackground;

    // Now that we have drawn the background, we apply the header's dimensions to R.
    R := Rect(Max(R.Left, 0), Max(R.Top, 0), Min(R.Right, TotalWidth), Min(R.Bottom, Header.Height));

    // Determine where to stop.
    MaxX := Target.X + R.Right - R.Left
    //Fixes issues #544, #427 -- MaxX should also shift on BidiMode bdRightToLeft
      + RTLOffset; //added for fix

    // Determine the start column.
    Run := ColumnFromPosition(Point(R.Left + RTLOffset, 0), False);
    if Run <= NoColumn then
      Exit;

    TargetRect.Top := Target.Y;
    TargetRect.Bottom := Target.Y + R.Bottom - R.Top;
    TargetRect.Left := Target.X - R.Left + Items[Run].Left + RTLOffset;
    // TargetRect.Right will be set in the loop

    ShowRightBorder := (Header.Style = hsThickButtons) or not (hoAutoResize in Header.Options) or (TreeView.BevelKind = bkNone);

    // Now go for each button.
    while (Run > NoColumn) and (TargetRect.Left < MaxX) do
    begin
      TargetRect.Right := TargetRect.Left + Items[Run].Width;

      // create a clipping rect to limit painting to button area
      ClipCanvas(TargetCanvas, Rect(Max(TargetRect.Left, Target.X), Target.Y + R.Top,
        Min(TargetRect.Right, MaxX), TargetRect.Bottom));

      PaintColumnHeader(Run, TargetRect);

      SelectClipRgn(Handle, 0);

      TargetRect.Left := TargetRect.Right;
      Run := GetNextVisibleColumn(Run);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeColumns.SaveToStream(const Stream : TStream);

var
  I : Integer;

begin
  I := Count;
  Stream.WriteBuffer(I, SizeOf(I));
  if I > 0 then
  begin
    for I := 0 to Count - 1 do
      TVirtualTreeColumn(Items[I]).SaveToStream(Stream);

    Stream.WriteBuffer(FPositionToIndex[0], Count * SizeOf(TColumnIndex));
  end;

  // Data introduced with header stream version 5.
  Stream.WriteBuffer(DefaultWidth, SizeOf(DefaultWidth));
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeColumns.TotalWidth : Integer;

var
  LastColumn : TColumnIndex;

begin
  Result := 0;
  if (Count > 0) and (Length(FPositionToIndex) > 0) then
  begin
    LastColumn := FPositionToIndex[Count - 1];
    if not (coVisible in Items[LastColumn].Options) then
      LastColumn := GetPreviousVisibleColumn(LastColumn);
    if LastColumn > NoColumn then
      with Items[LastColumn] do
        Result := FLeft + FWidth;
  end;
end;

{ THeaderPaintInfo }

procedure THeaderPaintInfo.DrawDropMark();
var
  Y           : Integer;
  lArrowWidth : Integer;
begin
  lArrowWidth := TBaseVirtualTreeCracker(Self.Column.TreeView).ScaledPixels(5);
  Y := (PaintRectangle.Top + PaintRectangle.Bottom - 3 * lArrowWidth) div 2;
  if DropMark = dmmLeft then
    DrawArrow(TargetCanvas, TScrollDirection.sdLeft, Point(PaintRectangle.Left, Y), lArrowWidth)
  else
    DrawArrow(TargetCanvas, TScrollDirection.sdRight, Point(PaintRectangle.Right - lArrowWidth - (lArrowWidth div 2) {spacing}, Y), lArrowWidth);
end;

procedure THeaderPaintInfo.DrawSortArrow(pDirection : TSortDirection);
const
  cDirection : array [TSortDirection] of TScrollDirection = (TScrollDirection.sdUp, TScrollDirection.sdDown);
var
  lOldColor : TColor;
begin
  lOldColor := TargetCanvas.Pen.Color;
  TargetCanvas.Pen.Color := clDkGray;
  DrawArrow(TargetCanvas, cDirection[pDirection], Point(SortGlyphPos.X, SortGlyphPos.Y), SortGlyphSize.cy);
  TargetCanvas.Pen.Color := lOldColor;
end;

{ TVirtualTreeColumnHelper }

function TVirtualTreeColumnHelper.Header : TVTHeaderCracker;
begin
  Result := TVTHeaderCracker(Owner.Header);
end;

function TVirtualTreeColumnHelper.TreeView : TBaseVirtualTreeCracker;
begin
  Result := TBaseVirtualTreeCracker(TVTHeaderCracker(Owner.Header).GetOwner);
end;

{ TVirtualTreeColumnsHelper }

function TVirtualTreeColumnsHelper.Header : TVTHeaderCracker;
begin
  Result := TVTHeaderCracker(FHeader);
end;

function TVirtualTreeColumnsHelper.TreeView : TBaseVirtualTreeCracker;
begin
  Result := TBaseVirtualTreeCracker(Header.GetOwner);
end;

end.
