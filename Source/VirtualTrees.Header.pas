unit VirtualTrees.Header;

interface

uses
  System.Classes,
  System.Types,
  WinApi.Windows,
  WinApi.Messages,
  Vcl.Graphics,
  Vcl.Menus,
  Vcl.ImgList,
  Vcl.Controls,
  VirtualTrees.Types,
  VirtualTrees.DragImage,
  VirtualTrees.Columns;

{$MINENUMSIZE 1, make enumerations as small as possible}


type
  //tree columns implementation
  TVTHeader = class;

  TVTConstraintPercent = 0 .. 100;

  TVTFixedAreaConstraints = class(TPersistent)
  private
    FHeader   : TVTHeader;
    FMaxHeightPercent, FMaxWidthPercent, FMinHeightPercent, FMinWidthPercent : TVTConstraintPercent;
    FOnChange : TNotifyEvent;
    procedure SetConstraints(Index : Integer; Value : TVTConstraintPercent);
  protected
    procedure Change;
    property Header : TVTHeader read FHeader;
  public
    constructor Create(AOwner : TVTHeader);

    procedure Assign(Source : TPersistent); override;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  published
    property MaxHeightPercent : TVTConstraintPercent index 0 read FMaxHeightPercent write SetConstraints default 0;
    property MaxWidthPercent  : TVTConstraintPercent index 1 read FMaxWidthPercent write SetConstraints default 95;
    property MinHeightPercent : TVTConstraintPercent index 2 read FMinHeightPercent write SetConstraints default 0;
    property MinWidthPercent  : TVTConstraintPercent index 3 read FMinWidthPercent write SetConstraints default 0;
  end;

  TVTHeaderStyle = (hsThickButtons, //TButton look and feel
    hsFlatButtons,                  //flatter look than hsThickButton, like an always raised flat TToolButton
    hsPlates                        //flat TToolButton look and feel (raise on hover etc.)
    );

  TVTHeaderOption = (hoAutoResize,  //Adjust a column so that the header never exceeds the client width of the owner control.
    hoColumnResize,                 //Resizing columns with the mouse is allowed.
    hoDblClickResize,               //Allows a column to resize itself to its largest entry.
    hoDrag,                         //Dragging columns is allowed.
    hoHotTrack,                     //Header captions are highlighted when mouse is over a particular column.
    hoOwnerDraw,                    //Header items with the owner draw style can be drawn by the application via event.
    hoRestrictDrag,                 //Header can only be dragged horizontally.
    hoShowHint,                     //Show application defined header hint.
    hoShowImages,                   //Show header images.
    hoShowSortGlyphs,               //Allow visible sort glyphs.
    hoVisible,                      //Header is visible.
    hoAutoSpring,                   //Distribute size changes of the header to all columns, which are sizable and have the coAutoSpring option enabled.
    hoFullRepaintOnResize,          //Fully invalidate the header (instead of subsequent columns only) when a column is resized.
    hoDisableAnimatedResize,        //Disable animated resize for all columns.
    hoHeightResize,                 //Allow resizing header height via mouse.
    hoHeightDblClickResize,         //Allow the header to resize itself to its default height.
    hoHeaderClickAutoSort,          //Clicks on the header will make the clicked column the SortColumn or toggle sort direction if it already was the sort column
    hoAutoColumnPopupMenu,          //Show a context menu for activating and deactivating columns on right click
    hoAutoResizeInclCaption         //Includes the header caption for the auto resizing
    );
  TVTHeaderOptions = set of TVTHeaderOption;

  THeaderState = (hsAutoSizing, //auto size chain is in progess, do not trigger again on WM_SIZE
    hsDragging,                 //header dragging is in progress (only if enabled)
    hsDragPending,              //left button is down, user might want to start dragging a column
    hsLoading,                  //The header currently loads from stream, so updates are not necessary.
    hsColumnWidthTracking,      //column resizing is in progress
    hsColumnWidthTrackPending,  //left button is down, user might want to start resize a column
    hsHeightTracking,           //height resizing is in progress
    hsHeightTrackPending,       //left button is down, user might want to start changing height
    hsResizing,                 //multi column resizing in progress
    hsScaling,                  //the header is scaled after a change of FixedAreaConstraints or client size
    hsNeedScaling               //the header needs to be scaled
    );
  THeaderStates = set of THeaderState;

  TVTHeader = class(TPersistent)
  private
    FOwner                       : TCustomControl;
    FColumns                     : TVirtualTreeColumns;
    FHeight                      : TDimension;
    FFont                        : TFont;
    FParentFont                  : Boolean;
    FOptions                     : TVTHeaderOptions;
    FStyle                       : TVTHeaderStyle; //button style
    FBackgroundColor             : TColor;
    FAutoSizeIndex               : TColumnIndex;
    FPopupMenu                   : TPopupMenu;
    FMainColumn                  : TColumnIndex; //the column which holds the tree
    FMaxHeight                   : TDimension;
    FMinHeight                   : TDimension;
    FDefaultHeight               : TDimension;
    FFixedAreaConstraints        : TVTFixedAreaConstraints; //Percentages for the fixed area (header, fixed columns).
    FImages                      : TCustomImageList;
    FImageChangeLink             : TChangeLink;             //connections to the image list to get notified about changes
    fSplitterHitTolerance        : TDimension;              //For property SplitterHitTolerance
    FSortColumn                  : TColumnIndex;
    FSortDirection               : TSortDirection;
    FDragImage                   : TVTDragImage;            //drag image management during header drag
    FLastWidth                   : TDimension;              //Used to adjust spring columns. This is the width of all visible columns, not the header rectangle.
    FRestoreSelectionColumnIndex : Integer;                 //The column that is used to implement the coRestoreSelection option
    function GetMainColumn : TColumnIndex;
    function GetUseColumns : Boolean;
    function IsFontStored : Boolean;
    procedure SetAutoSizeIndex(Value : TColumnIndex);
    procedure SetBackground(Value : TColor);
    procedure SetColumns(Value : TVirtualTreeColumns);
    procedure SetDefaultHeight(Value : TDimension);
    procedure SetFont(const Value : TFont);
    procedure SetHeight(Value : TDimension);
    procedure SetImages(const Value : TCustomImageList);
    procedure SetMainColumn(Value : TColumnIndex);
    procedure SetMaxHeight(Value : TDimension);
    procedure SetMinHeight(Value : TDimension);
    procedure SetOptions(Value : TVTHeaderOptions);
    procedure SetParentFont(Value : Boolean);
    procedure SetSortColumn(Value : TColumnIndex);
    procedure SetSortDirection(const Value : TSortDirection);
    procedure SetStyle(Value : TVTHeaderStyle);
    function GetRestoreSelectionColumnIndex : Integer;
  protected
    FStates              : THeaderStates; //Used to keep track of internal states the header can enter.
    FDragStart           : TPoint;        //initial mouse drag position
    FTrackStart          : TPoint;        //client coordinates of the tracking start point
    FTrackPoint          : TPoint;        //Client coordinate where the tracking started.
    FDoingAutoFitColumns : Boolean;       //Flag to avoid using the stored width for Main column

    procedure FontChanged(Sender : TObject); virtual;
    procedure AutoScale(isDpiChange: Boolean); virtual;
    function CanSplitterResize(P : TPoint) : Boolean;
    function CanWriteColumns : Boolean; virtual;
    procedure ChangeScale(M, D : TDimension; isDpiChange : Boolean); virtual;
    function DetermineSplitterIndex(P : TPoint) : Boolean; virtual;
    procedure DoAfterAutoFitColumn(Column : TColumnIndex); virtual;
    procedure DoAfterColumnWidthTracking(Column : TColumnIndex); virtual;
    procedure DoAfterHeightTracking; virtual;
    function DoBeforeAutoFitColumn(Column : TColumnIndex; SmartAutoFitType : TSmartAutoFitType) : Boolean; virtual;
    procedure DoBeforeColumnWidthTracking(Column : TColumnIndex; Shift : TShiftState); virtual;
    procedure DoBeforeHeightTracking(Shift : TShiftState); virtual;
    procedure DoCanSplitterResize(P : TPoint; var Allowed : Boolean); virtual;
    function DoColumnWidthDblClickResize(Column : TColumnIndex; P : TPoint; Shift : TShiftState) : Boolean; virtual;
    function DoColumnWidthTracking(Column : TColumnIndex; Shift : TShiftState; var TrackPoint : TPoint; P : TPoint) : Boolean; virtual;
    function DoGetPopupMenu(Column : TColumnIndex; Position : TPoint) : TPopupMenu; virtual;
    function DoHeightTracking(var P : TPoint; Shift : TShiftState) : Boolean; virtual;
    function DoHeightDblClickResize(var P : TPoint; Shift : TShiftState) : Boolean; virtual;
    procedure DoSetSortColumn(Value : TColumnIndex; pSortDirection : TSortDirection); virtual;
    procedure DragTo(P : TPoint); virtual;
    procedure FixedAreaConstraintsChanged(Sender : TObject);
    function GetColumnsClass : TVirtualTreeColumnsClass; virtual;
    function GetOwner : TPersistent; override;
    function GetShiftState : TShiftState;
    function HandleHeaderMouseMove(var Message : TWMMouseMove) : Boolean;
    function HandleMessage(var Message : TMessage) : Boolean; virtual;
    procedure ImageListChange(Sender : TObject);
    procedure PrepareDrag(P, Start : TPoint);
    procedure ReadColumns(Reader : TReader);
    procedure RecalculateHeader; virtual;
    procedure RescaleHeader;
    procedure UpdateMainColumn;
    procedure UpdateSpringColumns;
    procedure WriteColumns(Writer : TWriter);
    procedure InternalSetMainColumn(const Index : TColumnIndex);
    procedure InternalSetAutoSizeIndex(const Index : TColumnIndex);
    procedure InternalSetSortColumn(const Index : TColumnIndex);
  public
    constructor Create(AOwner : TCustomControl); virtual;
    destructor Destroy; override;

    function AllowFocus(ColumnIndex : TColumnIndex) : Boolean;
    procedure Assign(Source : TPersistent); override;
    procedure AutoFitColumns(Animated : Boolean = True; SmartAutoFitType : TSmartAutoFitType = smaUseColumnOption; RangeStartCol : Integer = NoColumn;
      RangeEndCol : Integer = NoColumn); virtual;
    function InHeader(P : TPoint) : Boolean; virtual;
    function InHeaderSplitterArea(P : TPoint) : Boolean; virtual;
    procedure Invalidate(Column : TVirtualTreeColumn; ExpandToBorder : Boolean = False; UpdateNowFlag : Boolean = False);
    procedure LoadFromStream(const Stream : TStream); virtual;
    function ResizeColumns(ChangeBy : TDimension; RangeStartCol : TColumnIndex; RangeEndCol : TColumnIndex; Options : TVTColumnOptions = [coVisible]) : TDimension;
    procedure RestoreColumns;
    procedure SaveToStream(const Stream : TStream); virtual;
    procedure StyleChanged(); virtual;

    property DragImage : TVTDragImage read FDragImage;
    property RestoreSelectionColumnIndex : Integer read GetRestoreSelectionColumnIndex write FRestoreSelectionColumnIndex default NoColumn;
    property States : THeaderStates read FStates;
    property Treeview : TCustomControl read FOwner;
    property UseColumns : Boolean read GetUseColumns;
    property doingAutoFitColumns : Boolean read FDoingAutoFitColumns;
  published
    property AutoSizeIndex        : TColumnIndex read FAutoSizeIndex write SetAutoSizeIndex;
    property Background           : TColor read FBackgroundColor write SetBackground default clBtnFace;
    property Columns              : TVirtualTreeColumns read FColumns write SetColumns stored False; //Stored by the owner tree to support VFI.
    property DefaultHeight        : Integer read FDefaultHeight write SetDefaultHeight default 19;
    property Font                 : TFont read FFont write SetFont stored IsFontStored;
    property FixedAreaConstraints : TVTFixedAreaConstraints read FFixedAreaConstraints write FFixedAreaConstraints;
    property Height               : Integer read FHeight write SetHeight default 19;
    property Images               : TCustomImageList read FImages write SetImages;
    property MainColumn           : TColumnIndex read GetMainColumn write SetMainColumn default 0;
    property MaxHeight            : Integer read FMaxHeight write SetMaxHeight default 10000;
    property MinHeight            : Integer read FMinHeight write SetMinHeight default 10;
    property Options              : TVTHeaderOptions read FOptions write SetOptions default [hoColumnResize, hoDrag, hoShowSortGlyphs];
    property ParentFont           : Boolean read FParentFont write SetParentFont default True;
    property PopupMenu            : TPopupMenu read FPopupMenu write FPopupMenu;
    property SortColumn           : TColumnIndex read FSortColumn write SetSortColumn default NoColumn;
    property SortDirection        : TSortDirection read FSortDirection write SetSortDirection default sdAscending;
    property SplitterHitTolerance : Integer read fSplitterHitTolerance write fSplitterHitTolerance default 8;
    //The area in pixels around a spliter which is sensitive for resizing
    property Style                : TVTHeaderStyle read FStyle write SetStyle default hsThickButtons;
  end;

  TVTHeaderClass = class of TVTHeader;

implementation

uses
  System.Math,
  System.UITypes,
  Vcl.Forms,
  VirtualTrees;

type
  TVirtualTreeColumnsCracker = class(TVirtualTreeColumns);
  TVirtualTreeColumnCracker = class(TVirtualTreeColumn);
  TBaseVirtualTreeCracker = class(TBaseVirtualTree);

  TVTHeaderHelper = class helper for TVTHeader
  public
    function Tree : TBaseVirtualTreeCracker;
  end;



  //----------------- TVTFixedAreaConstraints ----------------------------------------------------------------------------

constructor TVTFixedAreaConstraints.Create(AOwner : TVTHeader);

begin
  inherited Create;
  FMaxWidthPercent := 95;
  FHeader := AOwner;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTFixedAreaConstraints.SetConstraints(Index : Integer; Value : TVTConstraintPercent);

begin
  case Index of
    0 :
      if Value <> FMaxHeightPercent then
      begin
        FMaxHeightPercent := Value;
        if (Value > 0) and (Value < FMinHeightPercent) then
          FMinHeightPercent := Value;
        Change;
      end;
    1 :
      if Value <> FMaxWidthPercent then
      begin
        FMaxWidthPercent := Value;
        if (Value > 0) and (Value < FMinWidthPercent) then
          FMinWidthPercent := Value;
        Change;
      end;
    2 :
      if Value <> FMinHeightPercent then
      begin
        FMinHeightPercent := Value;
        if (FMaxHeightPercent > 0) and (Value > FMaxHeightPercent) then
          FMaxHeightPercent := Value;
        Change;
      end;
    3 :
      if Value <> FMinWidthPercent then
      begin
        FMinWidthPercent := Value;
        if (FMaxWidthPercent > 0) and (Value > FMaxWidthPercent) then
          FMaxWidthPercent := Value;
        Change;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTFixedAreaConstraints.Change;

begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTFixedAreaConstraints.Assign(Source : TPersistent);

begin
  if Source is TVTFixedAreaConstraints then
  begin
    FMaxHeightPercent := TVTFixedAreaConstraints(Source).FMaxHeightPercent;
    FMaxWidthPercent := TVTFixedAreaConstraints(Source).FMaxWidthPercent;
    FMinHeightPercent := TVTFixedAreaConstraints(Source).FMinHeightPercent;
    FMinWidthPercent := TVTFixedAreaConstraints(Source).FMinWidthPercent;
    Change;
  end
  else
    inherited;
end;

//----------------- TVTHeader -----------------------------------------------------------------------------------------

constructor TVTHeader.Create(AOwner : TCustomControl);

begin
  inherited Create;
  FOwner := AOwner;
  FColumns := GetColumnsClass.Create(Self);
  FHeight := 19;
  FDefaultHeight := FHeight;
  FMinHeight := 10;
  FMaxHeight := 10000;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FParentFont := True;
  FBackgroundColor := clBtnFace;
  FOptions := [hoColumnResize, hoDrag, hoShowSortGlyphs];

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;

  FSortColumn := NoColumn;
  FSortDirection := sdAscending;
  FMainColumn := NoColumn;

  FDragImage := TVTDragImage.Create(AOwner);
  with FDragImage do
  begin
    Fade := False;
    PreBlendBias := - 50;
    Transparency := 140;
  end;

  fSplitterHitTolerance := 8;
  FFixedAreaConstraints := TVTFixedAreaConstraints.Create(Self);
  FFixedAreaConstraints.OnChange := FixedAreaConstraintsChanged;

  FDoingAutoFitColumns := False;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TVTHeader.Destroy;

begin
  FDragImage.Free;
  FFixedAreaConstraints.Free;
  FImageChangeLink.Free;
  FFont.Free;
  FColumns.Clear; //TCollection's Clear method is not virtual, so we have to call our own Clear method manually.
  FColumns.Free;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.FontChanged(Sender : TObject);
begin
  inherited;
  {$IF CompilerVersion < 31}
  AutoScale(false);
  {$IFEND}
end;

procedure TVTHeader.AutoScale(isDpiChange: Boolean);
var
  I          : Integer;
  lMaxHeight : Integer;
begin
  if (toAutoChangeScale in TBaseVirtualTreeCracker(Tree).TreeOptions.AutoOptions) and not isDpiChange then
  begin
    //Ensure a minimum header size based on the font, so that all text is visible.
    //First find the largest Columns[].Spacing
    lMaxHeight := 0;
    for I := 0 to Self.Columns.Count - 1 do
      lMaxHeight := Max(lMaxHeight, Columns[I].Spacing);
    //Calculate the required height based on the font, this is important as the user might just have increased the size of the system icon font.
    with TBitmap.Create do
      try
        Canvas.Font.Assign(FFont);
        lMaxHeight := lMaxHeight { top spacing } + (lMaxHeight div 2) { minimum bottom spacing } + Canvas.TextHeight('Q');
      finally
        Free;
      end;
    //Get the maximum of the scaled original value and the minimum needed header height.
    lMaxHeight := Max(lMaxHeight, FHeight);
    //Set the calculated size
    Self.SetHeight(lMaxHeight);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.GetMainColumn : TColumnIndex;
begin
  if FColumns.Count > 0 then
    Result := FMainColumn
  else
    Result := NoColumn;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.GetUseColumns : Boolean;
begin
  Result := FColumns.Count > 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.IsFontStored : Boolean;
begin
  Result := not ParentFont;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetAutoSizeIndex(Value : TColumnIndex);
begin
  if FAutoSizeIndex <> Value then
  begin
    FAutoSizeIndex := Value;
    if hoAutoResize in FOptions then
      TVirtualTreeColumnsCracker(Columns).AdjustAutoSize(InvalidColumn);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetBackground(Value : TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    Invalidate(nil);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetColumns(Value : TVirtualTreeColumns);

begin
  FColumns.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetDefaultHeight(Value : Integer);
begin
  if Value < FMinHeight then
    Value := FMinHeight;
  if Value > FMaxHeight then
    Value := FMaxHeight;

  if FHeight = FDefaultHeight then
    SetHeight(Value);
  FDefaultHeight := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetFont(const Value : TFont);
begin
  FFont.Assign(Value);
  FParentFont := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetHeight(Value : Integer);
var
  RelativeMaxHeight, RelativeMinHeight, EffectiveMaxHeight, EffectiveMinHeight : Integer;
begin
  if not Tree.HandleAllocated then
  begin
    FHeight := Value;
    Include(FStates, hsNeedScaling);
  end
  else
  begin
    with FFixedAreaConstraints do
    begin
      RelativeMaxHeight := ((Tree.ClientHeight + FHeight) * FMaxHeightPercent) div 100;
      RelativeMinHeight := ((Tree.ClientHeight + FHeight) * FMinHeightPercent) div 100;

      EffectiveMinHeight := IfThen(FMaxHeightPercent > 0, Min(RelativeMaxHeight, FMinHeight), FMinHeight);
      EffectiveMaxHeight := IfThen(FMinHeightPercent > 0, Max(RelativeMinHeight, FMaxHeight), FMaxHeight);

      Value := Min(Max(Value, EffectiveMinHeight), EffectiveMaxHeight);
      if FMinHeightPercent > 0 then
        Value := Max(RelativeMinHeight, Value);
      if FMaxHeightPercent > 0 then
        Value := Min(RelativeMaxHeight, Value);
    end;

    if FHeight <> Value then
    begin
      FHeight := Value;
      if not (csLoading in Tree.ComponentState) and not (hsScaling in FStates) then
        RecalculateHeader;
      Tree.Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetImages(const Value : TCustomImageList);

begin
  if FImages <> Value then
  begin
    if Assigned(FImages) then
    begin
      FImages.UnRegisterChanges(FImageChangeLink);
      FImages.RemoveFreeNotification(FOwner);
    end;
    FImages := Value;
    if Assigned(FImages) then
    begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(FOwner);
    end;
    if not (csLoading in Tree.ComponentState) then
      Invalidate(nil);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetMainColumn(Value : TColumnIndex);

begin
  if csLoading in Tree.ComponentState then
    FMainColumn := Value
  else
  begin
    if Value < 0 then
      Value := 0;
    if Value > FColumns.Count - 1 then
      Value := FColumns.Count - 1;
    if Value <> FMainColumn then
    begin
      FMainColumn := Value;
      if not (csLoading in Tree.ComponentState) then
      begin
        Tree.MainColumnChanged;
        if not (toExtendedFocus in Tree.TreeOptions.SelectionOptions) then
          Tree.FocusedColumn := FMainColumn;
        Tree.Invalidate;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetMaxHeight(Value : Integer);

begin
  if Value < FMinHeight then
    Value := FMinHeight;
  FMaxHeight := Value;
  SetHeight(FHeight);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetMinHeight(Value : Integer);

begin
  if Value < 0 then
    Value := 0;
  if Value > FMaxHeight then
    Value := FMaxHeight;
  FMinHeight := Value;
  SetHeight(FHeight);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetOptions(Value : TVTHeaderOptions);

var
  ToBeSet, ToBeCleared : TVTHeaderOptions;

begin
  ToBeSet := Value - FOptions;
  ToBeCleared := FOptions - Value;
  FOptions := Value;

  if (hoAutoResize in (ToBeSet + ToBeCleared)) and (FColumns.Count > 0) then
  begin
    TVirtualTreeColumnsCracker(FColumns).AdjustAutoSize(InvalidColumn);
    if Tree.HandleAllocated then
    begin
      Tree.UpdateHorizontalScrollBar(False);
      if hoAutoResize in ToBeSet then
        Tree.Invalidate;
    end;
  end;

  if not (csLoading in Tree.ComponentState) and Tree.HandleAllocated then
  begin
    if hoVisible in (ToBeSet + ToBeCleared) then
      RecalculateHeader;
    Invalidate(nil);
    Tree.Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetParentFont(Value : Boolean);

begin
  if FParentFont <> Value then
  begin
    FParentFont := Value;
    if FParentFont then
      FFont.Assign(TBaseVirtualTree(FOwner).Font);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetSortColumn(Value : TColumnIndex);

begin
  if csLoading in Tree.ComponentState then
    FSortColumn := Value
  else
    DoSetSortColumn(Value, FSortDirection);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetSortDirection(const Value : TSortDirection);

begin
  if Value <> FSortDirection then
  begin
    FSortDirection := Value;
    Invalidate(nil);
    if ((toAutoSort in Tree.TreeOptions.AutoOptions) or (hoHeaderClickAutoSort in Options)) and (Tree.UpdateCount = 0) then
      Tree.SortTree(FSortColumn, FSortDirection, True);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.CanSplitterResize(P : TPoint) : Boolean;

begin
  Result := hoHeightResize in FOptions;
  DoCanSplitterResize(P, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SetStyle(Value : TVTHeaderStyle);

begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    if not (csLoading in Tree.ComponentState) then
      Invalidate(nil);
  end;
end;

procedure TVTHeader.StyleChanged();
begin
  {$IF CompilerVersion < 31}
  AutoScale(False); //Elements may have changed in size
  {$IFEND}
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.CanWriteColumns : Boolean;

//descendants may override this to optionally prevent column writing (e.g. if they are build dynamically).

begin
  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.ChangeScale(M, D : Integer; isDpiChange : Boolean);
var
  I : Integer;
begin
  //This method is only executed if toAutoChangeScale is set
  FMinHeight := MulDiv(FMinHeight, M, D);
  FMaxHeight := MulDiv(FMaxHeight, M, D);
  Self.Height := MulDiv(FHeight, M, D);
  if not ParentFont then
    Font.Height := MulDiv(Font.Height, M, D);
  //Scale the columns widths too
  for I := 0 to FColumns.Count - 1 do
    TVirtualTreeColumnCracker(Self.FColumns[I]).ChangeScale(M, D, isDpiChange);
  if not isDpiChange then
    AutoScale(isDpiChange);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.DetermineSplitterIndex(P : TPoint) : Boolean;

//Tries to find the index of that column whose right border corresponds to P.
//Result is True if column border was hit (with -3..+5 pixels tolerance).
//For continuous resizing the current track index and the column's left/right border are set.
//Note: The hit test is checking from right to left (or left to right in RTL mode) to make enlarging of zero-sized
//columns possible.

var
  VisibleFixedWidth : Integer;
  SplitPoint        : Integer;

  //--------------- local function --------------------------------------------

  function IsNearBy(IsFixedCol : Boolean; LeftTolerance, RightTolerance : Integer) : Boolean;

  begin
    if IsFixedCol then
      Result := (P.X < SplitPoint + Tree.EffectiveOffsetX + RightTolerance) and (P.X > SplitPoint + Tree.EffectiveOffsetX - LeftTolerance)
    else
      Result := (P.X > VisibleFixedWidth) and (P.X < SplitPoint + RightTolerance) and (P.X > SplitPoint - LeftTolerance);
  end;

//--------------- end local function ----------------------------------------

var
  I             : Integer;
  LeftTolerance : Integer; //The area left of the column divider which allows column resizing
begin
  Result := False;

  if FColumns.Count > 0 then
  begin
    FColumns.TrackIndex := NoColumn;
    VisibleFixedWidth := FColumns.GetVisibleFixedWidth;
    LeftTolerance := Round(SplitterHitTolerance * 0.6);
    if Tree.UseRightToLeftAlignment then
    begin
      SplitPoint := - Tree.EffectiveOffsetX;
      if FColumns.TotalWidth < Tree.ClientWidth then
        Inc(SplitPoint, Tree.ClientWidth - FColumns.TotalWidth);

      for I := 0 to FColumns.Count - 1 do
        with TVirtualTreeColumnsCracker(FColumns), Items[PositionToIndex[I]] do
          if coVisible in Options then
          begin
            if IsNearBy(coFixed in Options, LeftTolerance, SplitterHitTolerance - LeftTolerance) then
            begin
              if CanSplitterResize(P, PositionToIndex[I]) then
              begin
                Result := True;
                TrackIndex := PositionToIndex[I];

                //Keep the right border of this column. This and the current mouse position
                //directly determine the current column width.
                FTrackPoint.X := SplitPoint + IfThen(coFixed in Options, Tree.EffectiveOffsetX) + Width;
                FTrackPoint.Y := P.Y;
                Break;
              end;
            end;
            Inc(SplitPoint, Width);
          end;
    end
    else
    begin
      SplitPoint := - Tree.EffectiveOffsetX + FColumns.TotalWidth;

      for I := FColumns.Count - 1 downto 0 do
        with TVirtualTreeColumnsCracker(FColumns), Items[PositionToIndex[I]] do
          if coVisible in Options then
          begin
            if IsNearBy(coFixed in Options, SplitterHitTolerance - LeftTolerance, LeftTolerance) then
            begin
              if CanSplitterResize(P, PositionToIndex[I]) then
              begin
                Result := True;
                TrackIndex := PositionToIndex[I];

                //Keep the left border of this column. This and the current mouse position
                //directly determine the current column width.
                FTrackPoint.X := SplitPoint + IfThen(coFixed in Options, Tree.EffectiveOffsetX) - Width;
                FTrackPoint.Y := P.Y;
                Break;
              end;
            end;
            Dec(SplitPoint, Width);
          end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.DoAfterAutoFitColumn(Column : TColumnIndex);

begin
  if Assigned(Tree.OnAfterAutoFitColumn) then
    Tree.OnAfterAutoFitColumn(Self, Column);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.DoAfterColumnWidthTracking(Column : TColumnIndex);

//Tell the application that a column width tracking operation has been finished.

begin
  if Assigned(Tree.OnAfterColumnWidthTracking) then
    Tree.OnAfterColumnWidthTracking(Self, Column);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.DoAfterHeightTracking;

//Tell the application that a height tracking operation has been finished.

begin
  if Assigned(Tree.OnAfterHeaderHeightTracking) then
    Tree.OnAfterHeaderHeightTracking(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.DoBeforeAutoFitColumn(Column : TColumnIndex; SmartAutoFitType : TSmartAutoFitType) : Boolean;

//Query the application if we may autofit a column.

begin
  Result := True;
  if Assigned(Tree.OnBeforeAutoFitColumn) then
    Tree.OnBeforeAutoFitColumn(Self, Column, SmartAutoFitType, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.DoBeforeColumnWidthTracking(Column : TColumnIndex; Shift : TShiftState);

//Tell the a application that a column width tracking operation may begin.

begin
  if Assigned(Tree.OnBeforeColumnWidthTracking) then
    Tree.OnBeforeColumnWidthTracking(Self, Column, Shift);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.DoBeforeHeightTracking(Shift : TShiftState);

//Tell the application that a height tracking operation may begin.

begin
  if Assigned(Tree.OnBeforeHeaderHeightTracking) then
    Tree.OnBeforeHeaderHeightTracking(Self, Shift);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.DoCanSplitterResize(P : TPoint; var Allowed : Boolean);
begin
  if Assigned(Tree.OnCanSplitterResizeHeader) then
    Tree.OnCanSplitterResizeHeader(Self, P, Allowed);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.DoColumnWidthDblClickResize(Column : TColumnIndex; P : TPoint; Shift : TShiftState) : Boolean;

//Queries the application whether a double click on the column splitter should resize the column.

begin
  Result := True;
  if Assigned(Tree.OnColumnWidthDblClickResize) then
    Tree.OnColumnWidthDblClickResize(Self, Column, Shift, P, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.DoColumnWidthTracking(Column : TColumnIndex; Shift : TShiftState; var TrackPoint : TPoint; P : TPoint) : Boolean;

begin
  Result := True;
  if Assigned(Tree.OnColumnWidthTracking) then
    Tree.OnColumnWidthTracking(Self, Column, Shift, TrackPoint, P, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.DoGetPopupMenu(Column : TColumnIndex; Position : TPoint) : TPopupMenu;

//Queries the application whether there is a column specific header popup menu.

var
  AskParent : Boolean;

begin
  Result := PopupMenu;
  if Assigned(Tree.OnGetPopupMenu) then
    Tree.OnGetPopupMenu(TBaseVirtualTree(FOwner), nil, Column, Position, AskParent, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.DoHeightTracking(var P : TPoint; Shift : TShiftState) : Boolean;

begin
  Result := True;
  if Assigned(Tree.OnHeaderHeightTracking) then
    Tree.OnHeaderHeightTracking(Self, P, Shift, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.DoHeightDblClickResize(var P : TPoint; Shift : TShiftState) : Boolean;

begin
  Result := True;
  if Assigned(Tree.OnHeaderHeightDblClickResize) then
    Tree.OnHeaderHeightDblClickResize(Self, P, Shift, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.DoSetSortColumn(Value : TColumnIndex; pSortDirection : TSortDirection);

begin
  if Value < NoColumn then
    Value := NoColumn;
  if Value > Columns.Count - 1 then
    Value := Columns.Count - 1;
  if FSortColumn <> Value then
  begin
    if FSortColumn > NoColumn then
      Invalidate(Columns[FSortColumn]);
    FSortColumn := Value;
    FSortDirection := pSortDirection;
    if FSortColumn > NoColumn then
      Invalidate(Columns[FSortColumn]);
    if ((toAutoSort in Tree.TreeOptions.AutoOptions) or (hoHeaderClickAutoSort in Options)) and (Tree.UpdateCount = 0) then
      Tree.SortTree(FSortColumn, FSortDirection, True);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.DragTo(P : TPoint);

//Moves the drag image to a new position, which is determined from the passed point P and the previous
//mouse position.

var
  I, NewTarget : Integer;
  //optimized drag image move support
  ClientP      : TPoint;
  Left, Right  : Integer;
  NeedRepaint  : Boolean; //True if the screen needs an update (changed drop target or drop side)

begin
  //Determine new drop target and which side of it is prefered.
  ClientP := Tree.ScreenToClient(P);
  //Make coordinates relative to (0, 0) of the non-client area.
  Inc(ClientP.Y, FHeight);
  NewTarget := FColumns.ColumnFromPosition(ClientP);
  NeedRepaint := (NewTarget <> InvalidColumn) and (NewTarget <> FColumns.DropTarget);
  if NewTarget >= 0 then
  begin
    FColumns.GetColumnBounds(NewTarget, Left, Right);
    if (ClientP.X < ((Left + Right) div 2)) <> FColumns.DropBefore then
    begin
      NeedRepaint := True;
      FColumns.DropBefore := not FColumns.DropBefore;
    end;
  end;

  if NeedRepaint then
  begin
    //Invalidate columns which need a repaint.
    if FColumns.DropTarget > NoColumn then
    begin
      I := FColumns.DropTarget;
      FColumns.DropTarget := NoColumn;
      Invalidate(FColumns.Items[I]);
    end;
    if (NewTarget > NoColumn) and (NewTarget <> FColumns.DropTarget) then
    begin
      Invalidate(FColumns.Items[NewTarget]);
      FColumns.DropTarget := NewTarget;
    end;
  end;

  //Fix for various problems mentioned in issue 248.
  if NeedRepaint then
  begin
    UpdateWindow(FOwner.Handle);
    //The new routine recaptures the backup image after the updatewindow
    //Note: We could have called this unconditionally but when called
    //over the tree, doesn't capture the background image. Since our
    //problems are in painting of the header, we call it only when the
    //drag image is over the header.
    if
    //determine the case when the drag image is or was on the header area
      (InHeader(FOwner.ScreenToClient(FDragImage.LastPosition)) or InHeader(FOwner.ScreenToClient(FDragImage.ImagePosition))) then
    begin
      GDIFlush;
      TBaseVirtualTreeCracker(FOwner).UpdateWindowAndDragImage(TBaseVirtualTree(FOwner), TBaseVirtualTreeCracker(FOwner).HeaderRect, True, True);
    end;
    //since we took care of UpdateWindow above, there is no need to do an
    //update window again by sending NeedRepaint. So switch off the second parameter.
    NeedRepaint := False;
  end;

  FDragImage.DragTo(P, NeedRepaint);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.FixedAreaConstraintsChanged(Sender : TObject);

//This method gets called when FFixedAreaConstraints is changed.

begin
  if Tree.HandleAllocated then
    RescaleHeader
  else
    Include(FStates, hsNeedScaling);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.GetColumnsClass : TVirtualTreeColumnsClass;

//Returns the class to be used for the actual column implementation. descendants may optionally override this and
//return their own class.

begin
  Result := TVirtualTreeColumns;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.GetOwner : TPersistent;

begin
  Result := FOwner;
end;

function TVTHeader.GetRestoreSelectionColumnIndex : Integer;
begin
  if FRestoreSelectionColumnIndex >= 0 then
    Result := FRestoreSelectionColumnIndex
  else
    Result := MainColumn;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.GetShiftState : TShiftState;

begin
  Result := [];
  if GetKeyState(VK_SHIFT) < 0 then
    Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then
    Include(Result, ssCtrl);
  if GetKeyState(VK_MENU) < 0 then
    Include(Result, ssAlt);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.HandleHeaderMouseMove(var Message : TWMMouseMove) : Boolean;

var
  P             : TPoint;
  NextColumn, I : TColumnIndex;
  NewWidth      : Integer;

begin
  Result := False;
  with Message do
  begin
    P := Point(XPos, YPos);
    if hsColumnWidthTrackPending in FStates then
    begin
      Tree.StopTimer(HeaderTimer);
      FStates := FStates - [hsColumnWidthTrackPending] + [hsColumnWidthTracking];
      HandleHeaderMouseMove := True;
      Result := 0;
    end
    else if hsHeightTrackPending in FStates then
    begin
      Tree.StopTimer(HeaderTimer);
      FStates := FStates - [hsHeightTrackPending] + [hsHeightTracking];
      HandleHeaderMouseMove := True;
      Result := 0;
    end
    else if hsColumnWidthTracking in FStates then
    begin
      if DoColumnWidthTracking(FColumns.TrackIndex, GetShiftState, FTrackPoint, P) then
      begin
        if Tree.UseRightToLeftAlignment then
        begin
          NewWidth := FTrackPoint.X - XPos;
          NextColumn := FColumns.GetPreviousVisibleColumn(FColumns.TrackIndex);
        end
        else
        begin
          NewWidth := XPos - FTrackPoint.X;
          NextColumn := FColumns.GetNextVisibleColumn(FColumns.TrackIndex);
        end;

        //The autosized column cannot be resized using the mouse normally. Instead we resize the next
        //visible column, so it look as we directly resize the autosized column.
        if (hoAutoResize in FOptions) and (FColumns.TrackIndex = FAutoSizeIndex) and (NextColumn > NoColumn) and (coResizable in FColumns[NextColumn].Options) and
          (FColumns[FColumns.TrackIndex].MinWidth < NewWidth) and (FColumns[FColumns.TrackIndex].MaxWidth > NewWidth) then
          FColumns[NextColumn].Width := FColumns[NextColumn].Width - NewWidth + FColumns[FColumns.TrackIndex].Width
        else
          FColumns[FColumns.TrackIndex].Width := NewWidth; //1 EListError seen here (List index out of bounds (-1)) since 10/2013
      end;
      HandleHeaderMouseMove := True;
      Result := 0;
    end
    else if hsHeightTracking in FStates then
    begin
      if DoHeightTracking(P, GetShiftState) then
        SetHeight(Integer(FHeight) + P.Y);
      HandleHeaderMouseMove := True;
      Result := 0;
    end
    else
    begin
      if hsDragPending in FStates then
      begin
        P := Tree.ClientToScreen(P);
        //start actual dragging if allowed
        if (hoDrag in FOptions) and Tree.DoHeaderDragging(TVirtualTreeColumnsCracker(FColumns).DownIndex) then
        begin
          if ((Abs(FDragStart.X - P.X) > Mouse.DragThreshold) or (Abs(FDragStart.Y - P.Y) > Mouse.DragThreshold)) then
          begin
            Tree.StopTimer(HeaderTimer);
            with TVirtualTreeColumnsCracker(FColumns) do
            begin
              I := DownIndex;
              DownIndex := NoColumn;
              HoverIndex := NoColumn;
              if I > NoColumn then
                Invalidate(FColumns[I]);
            end;
            PrepareDrag(P, FDragStart);
            FStates := FStates - [hsDragPending] + [hsDragging];
            HandleHeaderMouseMove := True;
            Result := 0;
          end;
        end;
      end
      else if hsDragging in FStates then
      begin
        DragTo(Tree.ClientToScreen(Point(XPos, YPos)));
        HandleHeaderMouseMove := True;
        Result := 0;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.HandleMessage(var Message : TMessage) : Boolean;

//The header gets here the opportunity to handle certain messages before they reach the tree. This is important
//because the tree needs to handle various non-client area messages for the header as well as some dragging/tracking
//events.
//By returning True the message will not be handled further, otherwise the message is then dispatched
//to the proper message handlers.

var
  P                                          : TPoint;
  R                                          : TRect;
  I                                          : TColumnIndex;
  OldPosition                                : Integer;
  HitIndex                                   : TColumnIndex;
  NewCursor                                  : HCURSOR;
  Button                                     : TMouseButton;
  IsInHeader, IsHSplitterHit, IsVSplitterHit : Boolean;

  //--------------- local function --------------------------------------------

  function HSplitterHit : Boolean;

  var
    NextCol : TColumnIndex;

  begin
    Result := (hoColumnResize in FOptions) and DetermineSplitterIndex(P);
    if Result and not InHeader(P) then
    begin
      NextCol := FColumns.GetNextVisibleColumn(FColumns.TrackIndex);
      if not (coFixed in FColumns[FColumns.TrackIndex].Options) or (NextCol <= NoColumn) or (coFixed in FColumns[NextCol].Options) or (P.Y > Integer(Tree.RangeY)) then
        Result := False;
    end;
  end;

//--------------- end local function ----------------------------------------

begin
  Result := False;
  case Message.Msg of
    WM_SIZE :
      begin
        if not (tsWindowCreating in TBaseVirtualTreeCracker(FOwner).TreeStates) then
          if (hoAutoResize in FOptions) and not (hsAutoSizing in FStates) then
          begin
            TVirtualTreeColumnsCracker(FColumns).AdjustAutoSize(InvalidColumn);
            Invalidate(nil);
          end
          else if not (hsScaling in FStates) then
          begin
            RescaleHeader;
            Invalidate(nil);
          end;
      end;
    CM_PARENTFONTCHANGED :
      if FParentFont then
        FFont.Assign(TBaseVirtualTreeCracker(FOwner).Font);
    CM_BIDIMODECHANGED :
      for I := 0 to FColumns.Count - 1 do
        if coParentBiDiMode in FColumns[I].Options then
          FColumns[I].ParentBiDiModeChanged;
    WM_NCMBUTTONDOWN :
      begin
        with TWMNCMButtonDown(Message) do
          P := Tree.ScreenToClient(Point(XCursor, YCursor));
        if InHeader(P) then
          TBaseVirtualTreeCracker(FOwner).DoHeaderMouseDown(mbMiddle, GetShiftState, P.X, P.Y + Integer(FHeight));
      end;
    WM_NCMBUTTONUP :
      begin
        with TWMNCMButtonUp(Message) do
          P := FOwner.ScreenToClient(Point(XCursor, YCursor));
        if InHeader(P) then
        begin
          with TVirtualTreeColumnsCracker(FColumns) do
          begin
            HandleClick(P, mbMiddle, True, False);
            TBaseVirtualTreeCracker(FOwner).DoHeaderMouseUp(mbMiddle, GetShiftState, P.X, P.Y + Integer(Self.FHeight));
            DownIndex := NoColumn;
            CheckBoxHit := False;
          end;
        end;
      end;
    WM_LBUTTONDBLCLK, WM_NCLBUTTONDBLCLK, WM_NCMBUTTONDBLCLK, WM_NCRBUTTONDBLCLK :
      begin
        if Message.Msg <> WM_LBUTTONDBLCLK then
          with TWMNCLButtonDblClk(Message) do
            P := FOwner.ScreenToClient(Point(XCursor, YCursor))
        else
          with TWMLButtonDblClk(Message) do
            P := Point(XPos, YPos);

        if (hoHeightDblClickResize in FOptions) and InHeaderSplitterArea(P) and (FDefaultHeight > 0) then
        begin
          if DoHeightDblClickResize(P, GetShiftState) and (FDefaultHeight > 0) then
            SetHeight(FMinHeight);
          Result := True;
        end
        else if HSplitterHit and ((Message.Msg = WM_NCLBUTTONDBLCLK) or (Message.Msg = WM_LBUTTONDBLCLK)) and (hoDblClickResize in FOptions) and (FColumns.TrackIndex > NoColumn)
        then
        begin
          //If the click was on a splitter then resize column to smallest width.
          if DoColumnWidthDblClickResize(FColumns.TrackIndex, P, GetShiftState) then
            AutoFitColumns(True, smaUseColumnOption, FColumns[FColumns.TrackIndex].Position, FColumns[FColumns.TrackIndex].Position);
          Message.Result := 0;
          Result := True;
        end
        else if InHeader(P) and (Message.Msg <> WM_LBUTTONDBLCLK) then
        begin
          case Message.Msg of
            WM_NCMBUTTONDBLCLK :
              Button := mbMiddle;
            WM_NCRBUTTONDBLCLK :
              Button := mbRight;
          else
              //WM_NCLBUTTONDBLCLK
            Button := mbLeft;
          end;
          if Button = mbLeft then
            TVirtualTreeColumnsCracker(FColumns).AdjustDownColumn(P);
          TVirtualTreeColumnsCracker(FColumns).HandleClick(P, Button, True, True);
        end;
      end;
    //The "hot" area of the headers horizontal splitter is partly within the client area of the the tree, so we need
    //to handle WM_LBUTTONDOWN here, too.
    WM_LBUTTONDOWN, WM_NCLBUTTONDOWN :
      begin

        Application.CancelHint;

        if not (csDesigning in Tree.ComponentState) then
        begin
          with Tree do
          begin
            //make sure no auto scrolling is active...
            StopTimer(ScrollTimer);
            DoStateChange([], [tsScrollPending, tsScrolling]);
            //... pending editing is cancelled (actual editing remains active)
            StopTimer(EditTimer);
            DoStateChange([], [tsEditPending]);
          end;
        end;

        if Message.Msg = WM_LBUTTONDOWN then
          //Coordinates are already client area based.
          with TWMLButtonDown(Message) do
          begin
            P := Point(XPos, YPos);
            //#909
            FDragStart := Tree.ClientToScreen(P);
          end
        else
          with TWMNCLButtonDown(Message) do
          begin
            //want the drag start point in screen coordinates
            FDragStart := Point(XCursor, YCursor);
            P := Tree.ScreenToClient(FDragStart);
          end;

        IsInHeader := InHeader(P);
        //in design-time header columns are always resizable
        if (csDesigning in Tree.ComponentState) then
          IsVSplitterHit := InHeaderSplitterArea(P)
        else
          IsVSplitterHit := InHeaderSplitterArea(P) and CanSplitterResize(P);
        IsHSplitterHit := HSplitterHit;

        if IsVSplitterHit or IsHSplitterHit then
        begin
          FTrackStart := P;
          TVirtualTreeColumnsCracker(FColumns).HoverIndex := NoColumn;
          if IsVSplitterHit then
          begin
            if not (csDesigning in Tree.ComponentState) then
              DoBeforeHeightTracking(GetShiftState);
            Include(FStates, hsHeightTrackPending);
          end
          else
          begin
            if not (csDesigning in Tree.ComponentState) then
              DoBeforeColumnWidthTracking(FColumns.TrackIndex, GetShiftState);
            Include(FStates, hsColumnWidthTrackPending);
          end;

          SetCapture(Tree.Handle);
          Result := True;
          Message.Result := 0;
        end
        else if IsInHeader then
        begin
          HitIndex := TVirtualTreeColumnsCracker(FColumns).AdjustDownColumn(P);
          //in design-time header columns are always draggable
          if ((csDesigning in Tree.ComponentState) and (HitIndex > NoColumn)) or ((hoDrag in FOptions) and (HitIndex > NoColumn) and (coDraggable in FColumns[HitIndex].Options))
          then
          begin
            //Show potential drag operation.
            //Disabled columns do not start a drag operation because they can't be clicked.
            Include(FStates, hsDragPending);
            SetCapture(Tree.Handle);
            Result := True;
            Message.Result := 0;
          end;
        end;

        //This is a good opportunity to notify the application.
        if not (csDesigning in Tree.ComponentState) and IsInHeader then
          TBaseVirtualTreeCracker(FOwner).DoHeaderMouseDown(mbLeft, GetShiftState, P.X, P.Y + Integer(FHeight));
      end;
    WM_NCRBUTTONDOWN :
      begin
        with TWMNCRButtonDown(Message) do
          P := FOwner.ScreenToClient(Point(XCursor, YCursor));
        if InHeader(P) then
          TBaseVirtualTreeCracker(FOwner).DoHeaderMouseDown(mbRight, GetShiftState, P.X, P.Y + Integer(FHeight));
      end;
    WM_NCRBUTTONUP :
      if not (csDesigning in FOwner.ComponentState) then
        with TWMNCRButtonUp(Message) do
        begin
          Application.CancelHint;
          P := FOwner.ScreenToClient(Point(XCursor, YCursor));
          if InHeader(P) then
          begin
            HandleMessage := TVirtualTreeColumnsCracker(FColumns).HandleClick(P, mbRight, True, False);
            TBaseVirtualTreeCracker(FOwner).DoHeaderMouseUp(mbRight, GetShiftState, P.X, P.Y + Integer(FHeight));
          end;
        end;
    //When the tree window has an active mouse capture then we only get "client-area" messages.
    WM_LBUTTONUP, WM_NCLBUTTONUP :
      begin
        Application.CancelHint;

        if FStates <> [] then
        begin
          ReleaseCapture;
          if hsDragging in FStates then
          begin
            //successfull dragging moves columns
            with TWMLButtonUp(Message) do
              P := Tree.ClientToScreen(Point(XPos, YPos));
            GetWindowRect(Tree.Handle, R);
            with FColumns do
            begin
              FDragImage.EndDrag;

              //Problem fixed:
              //Column Header does not paint correctly after a drop in certain conditions
              // ** The conditions are, drag is across header, mouse is not moved after
              //the drop and the graphics hardware is slow in certain operations (encountered
              //on Windows 10).
              //Fix for the problem on certain systems where the dropped column header
              //does not appear in the new position if the mouse is not moved after
              //the drop. The reason is that the restore backup image operation (BitBlt)
              //in the above EndDrag is slower than the header repaint in the code below
              //and overlaps the new changed header with the older image.
              //This happens because BitBlt seems to operate in its own thread in the
              //graphics hardware and finishes later than the following code.
              //
              //To solve this problem, we introduce a small delay here so that the
              //changed header in the following code is correctly repainted after
              //the delayed BitBlt above has finished operation to restore the old
              //backup image.
              sleep(50);

              if (DropTarget > - 1) and (DropTarget <> DragIndex) and PtInRect(R, P) then
              begin
                OldPosition := FColumns[DragIndex].Position;
                if FColumns.DropBefore then
                begin
                  if FColumns[DragIndex].Position < FColumns[DropTarget].Position then
                    FColumns[DragIndex].Position := Max(0, FColumns[DropTarget].Position - 1)
                  else
                    FColumns[DragIndex].Position := FColumns[DropTarget].Position;
                end
                else
                begin
                  if FColumns[DragIndex].Position < FColumns[DropTarget].Position then
                    FColumns[DragIndex].Position := FColumns[DropTarget].Position
                  else
                    FColumns[DragIndex].Position := FColumns[DropTarget].Position + 1;
                end;
                Tree.DoHeaderDragged(DragIndex, OldPosition);
              end
              else
                Tree.DoHeaderDraggedOut(DragIndex, P);
              DropTarget := NoColumn;
            end;
            Invalidate(nil);
          end;
          Result := True;
          Message.Result := 0;
        end;

        case Message.Msg of
          WM_LBUTTONUP :
            with TWMLButtonUp(Message) do
            begin
              with TVirtualTreeColumnsCracker(FColumns) do
              begin
                if DownIndex > NoColumn then
                  HandleClick(Point(XPos, YPos), mbLeft, False, False);
              end;
              if FStates <> [] then
                TBaseVirtualTreeCracker(FOwner).DoHeaderMouseUp(mbLeft, KeysToShiftState(Keys), XPos, YPos);
            end;
          WM_NCLBUTTONUP :
            with TWMNCLButtonUp(Message) do
            begin
              P := FOwner.ScreenToClient(Point(XCursor, YCursor));
              TVirtualTreeColumnsCracker(FColumns).HandleClick(P, mbLeft, False, False);
              TBaseVirtualTreeCracker(FOwner).DoHeaderMouseUp(mbLeft, GetShiftState, P.X, P.Y + Integer(FHeight));
            end;
        end;

        if FColumns.TrackIndex > NoColumn then
        begin
          if hsColumnWidthTracking in FStates then
            DoAfterColumnWidthTracking(FColumns.TrackIndex);
          Invalidate(Columns[FColumns.TrackIndex]);
          FColumns.TrackIndex := NoColumn;
        end;
        with TVirtualTreeColumnsCracker(FColumns) do
        begin
          if DownIndex > NoColumn then
          begin
            Invalidate(FColumns[DownIndex]);
            DownIndex := NoColumn;
          end;
        end;
        if hsHeightTracking in FStates then
          DoAfterHeightTracking;

        FStates := FStates - [hsDragging, hsDragPending, hsColumnWidthTracking, hsColumnWidthTrackPending, hsHeightTracking, hsHeightTrackPending];
      end; //WM_NCLBUTTONUP
    //hovering, mouse leave detection
    WM_NCMOUSEMOVE :
      with TWMNCMouseMove(Message), TVirtualTreeColumnsCracker(FColumns) do
      begin
        P := Tree.ScreenToClient(Point(XCursor, YCursor));
        Tree.DoHeaderMouseMove(GetShiftState, P.X, P.Y + Integer(FHeight));
        if InHeader(P) and ((AdjustHoverColumn(P)) or ((DownIndex >= 0) and (HoverIndex <> DownIndex))) then
        begin
          //We need a mouse leave detection from here for the non client area.
          //TODO: The best solution available would be the TrackMouseEvent API.
          //With the drop of the support of Win95 totally and WinNT4 we should replace the timer.
          Tree.StopTimer(HeaderTimer);
          SetTimer(Tree.Handle, HeaderTimer, 50, nil);
          //use Delphi's internal hint handling for header hints too
          if hoShowHint in FOptions then
          begin
            //client coordinates!
            XCursor := P.X;
            YCursor := P.Y + Integer(FHeight);
            Application.HintMouseMessage(FOwner, Message);
          end;
        end;
      end;
    WM_TIMER :
      if TWMTimer(Message).TimerID = HeaderTimer then
      begin
        //determine current mouse position to check if it left the window
        GetCursorPos(P);
        P := Tree.ScreenToClient(P);
        with TVirtualTreeColumnsCracker(FColumns) do
        begin
          if not InHeader(P) or ((DownIndex > NoColumn) and (HoverIndex <> DownIndex)) then
          begin
            Tree.StopTimer(HeaderTimer);
            HoverIndex := NoColumn;
            ClickIndex := NoColumn;
            DownIndex := NoColumn;
            CheckBoxHit := False;
            Result := True;
            Message.Result := 0;
            Invalidate(nil);
          end;
        end;
      end;
    WM_MOUSEMOVE : //mouse capture and general message redirection
      Result := HandleHeaderMouseMove(TWMMouseMove(Message));
    WM_SETCURSOR :
      //Feature: design-time header
      if (FStates = []) then
      begin
        //Retrieve last cursor position (GetMessagePos does not work here, I don't know why).
        GetCursorPos(P);

        //Is the mouse in the header rectangle and near the splitters?
        P := Tree.ScreenToClient(P);
        IsHSplitterHit := HSplitterHit;
        //in design-time header columns are always resizable
        if (csDesigning in Tree.ComponentState) then
          IsVSplitterHit := InHeaderSplitterArea(P)
        else
          IsVSplitterHit := InHeaderSplitterArea(P) and CanSplitterResize(P);

        if IsVSplitterHit or IsHSplitterHit then
        begin
          NewCursor := Screen.Cursors[Tree.Cursor];
          if IsVSplitterHit and ((hoHeightResize in FOptions) or (csDesigning in Tree.ComponentState)) then
            NewCursor := Screen.Cursors[crVertSplit]
          else if IsHSplitterHit then
            NewCursor := Screen.Cursors[crHeaderSplit];

          if not (csDesigning in Tree.ComponentState) then
            Tree.DoGetHeaderCursor(NewCursor);
          Result := NewCursor <> Screen.Cursors[crDefault];
          if Result then
          begin
            WinApi.Windows.SetCursor(NewCursor);
            Message.Result := 1;
          end;
        end;
      end
      else
      begin
        Message.Result := 1;
        Result := True;
      end;
    WM_KEYDOWN, WM_KILLFOCUS :
      if (Message.Msg = WM_KILLFOCUS) or (TWMKeyDown(Message).CharCode = VK_ESCAPE) then
      begin
        if hsDragging in FStates then
        begin
          ReleaseCapture;
          FDragImage.EndDrag;
          Exclude(FStates, hsDragging);
          FColumns.DropTarget := NoColumn;
          Invalidate(nil);
          Result := True;
          Message.Result := 0;
        end
        else
        begin
          if [hsColumnWidthTracking, hsHeightTracking] * FStates <> [] then
          begin
            ReleaseCapture;
            if hsColumnWidthTracking in FStates then
              DoAfterColumnWidthTracking(FColumns.TrackIndex);
            if hsHeightTracking in FStates then
              DoAfterHeightTracking;
            Result := True;
            Message.Result := 0;
          end;

          FStates := FStates - [hsColumnWidthTracking, hsColumnWidthTrackPending, hsHeightTracking, hsHeightTrackPending];
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.ImageListChange(Sender : TObject);

begin
  if not (csDestroying in Tree.ComponentState) then
    Invalidate(nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.PrepareDrag(P, Start : TPoint);

//Initializes dragging of the header, P is the current mouse postion and Start the initial mouse position.

var
  Image      : TBitmap;
  ImagePos   : TPoint;
  DragColumn : TVirtualTreeColumn;
  RTLOffset  : Integer;

begin
  //Determine initial position of drag image (screen coordinates).
  FColumns.DropTarget := NoColumn;
  Start := Tree.ScreenToClient(Start);
  Inc(Start.Y, FHeight);
  FColumns.DragIndex := FColumns.ColumnFromPosition(Start);
  DragColumn := FColumns[FColumns.DragIndex];

  Image := TBitmap.Create;
  with Image do
    try
      PixelFormat := pf32Bit;
      SetSize(DragColumn.Width, FHeight);

      //Erase the entire image with the color key value, for the case not everything
      //in the image is covered by the header image.
      Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(Rect(0, 0, Width, Height));

      if Tree.UseRightToLeftAlignment then
        RTLOffset := Tree.ComputeRTLOffset
      else
        RTLOffset := 0;
      with DragColumn do
        FColumns.PaintHeader(Canvas, Rect(Left, 0, Left + Width, Height), Point( - RTLOffset, 0), RTLOffset);

      if Tree.UseRightToLeftAlignment then
        ImagePos := Tree.ClientToScreen(Point(DragColumn.Left + Tree.ComputeRTLOffset(True), 0))
      else
        ImagePos := Tree.ClientToScreen(Point(DragColumn.Left, 0));
      //Column rectangles are given in local window coordinates not client coordinates.
      Dec(ImagePos.Y, FHeight);

      if hoRestrictDrag in FOptions then
        FDragImage.MoveRestriction := dmrHorizontalOnly
      else
        FDragImage.MoveRestriction := dmrNone;
      FDragImage.PrepareDrag(Image, ImagePos, P, nil);
      FDragImage.ShowDragImage;
    finally
      Image.Free;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.ReadColumns(Reader : TReader);

begin
  Include(FStates, hsLoading);
  Columns.Clear;
  Reader.ReadValue;
  Reader.ReadCollection(Columns);
  Exclude(FStates, hsLoading);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.RecalculateHeader;

//Initiate a recalculation of the non-client area of the owner tree.

begin
  if Tree.HandleAllocated then
  begin
    Tree.UpdateHeaderRect;
    SetWindowPos(Tree.Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOSENDCHANGING or SWP_NOSIZE or SWP_NOZORDER);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.RescaleHeader;

//Rescale the fixed elements (fixed columns, header itself) to FixedAreaConstraints.

var
  FixedWidth, MaxFixedWidth, MinFixedWidth : Integer;

  //--------------- local function --------------------------------------------

  procedure ComputeConstraints;

  var
    I : TColumnIndex;

  begin
    with FColumns do
    begin
      I := GetFirstVisibleColumn;
      while I > NoColumn do
      begin
        if (coFixed in FColumns[I].Options) and (FColumns[I].Width < FColumns[I].MinWidth) then
          TVirtualTreeColumnCracker(FColumns[I]).InternalSetWidth(FColumns[I].MinWidth); //SetWidth has side effects and this bypasses them
        I := GetNextVisibleColumn(I);
      end;
      FixedWidth := GetVisibleFixedWidth;
    end;

    with FFixedAreaConstraints do
    begin
      MinFixedWidth := (Tree.ClientWidth * FMinWidthPercent) div 100;
      MaxFixedWidth := (Tree.ClientWidth * FMaxWidthPercent) div 100;
    end;
  end;

//----------- end local function --------------------------------------------

begin
  if ([csLoading, csReading, csWriting, csDestroying] * Tree.ComponentState = []) and not (hsLoading in FStates) and Tree.HandleAllocated then
  begin
    Include(FStates, hsScaling);

    SetHeight(FHeight);
    RecalculateHeader;

    with FFixedAreaConstraints do
      if (FMaxWidthPercent > 0) or (FMinWidthPercent > 0) or (FMinHeightPercent > 0) or (FMaxHeightPercent > 0) then
      begin
        ComputeConstraints;

        with FColumns do
          if (FMaxWidthPercent > 0) and (FixedWidth > MaxFixedWidth) then
            ResizeColumns(MaxFixedWidth - FixedWidth, 0, Count - 1, [coVisible, coFixed])
          else if (FMinWidthPercent > 0) and (FixedWidth < MinFixedWidth) then
            ResizeColumns(MinFixedWidth - FixedWidth, 0, Count - 1, [coVisible, coFixed]);

        TVirtualTreeColumnsCracker(FColumns).UpdatePositions;
      end;

    Exclude(FStates, hsScaling);
    Exclude(FStates, hsNeedScaling);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.UpdateMainColumn();

//Called once the load process of the owner tree is done.

begin
  if FMainColumn < 0 then
    MainColumn := 0;
  if FMainColumn > FColumns.Count - 1 then
    MainColumn := FColumns.Count - 1;
  if (FMainColumn >= 0) and not (coVisible in Self.Columns[FMainColumn].Options) then
  begin
    //Issue #946: Choose new MainColumn if current one ist not visible
    MainColumn := Self.Columns.GetFirstVisibleColumn();
  end
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.UpdateSpringColumns;

var
  I              : TColumnIndex;
  SpringCount    : Integer;
  Sign           : Integer;
  ChangeBy       : Single;
  Difference     : Single;
  NewAccumulator : Single;

begin
  with Tree do
    ChangeBy := HeaderRect.Right - HeaderRect.Left - FLastWidth;
  if (hoAutoSpring in FOptions) and (FLastWidth <> 0) and (ChangeBy <> 0) then
  begin
    //Stay positive if downsizing the control.
    if ChangeBy < 0 then
      Sign := - 1
    else
      Sign := 1;
    ChangeBy := Abs(ChangeBy);
    //Count how many columns have spring enabled.
    SpringCount := 0;
    for I := 0 to FColumns.Count - 1 do
      if [coVisible, coAutoSpring] * FColumns[I].Options = [coVisible, coAutoSpring] then
        Inc(SpringCount);
    if SpringCount > 0 then
    begin
      //Calculate the size to add/sub to each columns.
      Difference := ChangeBy / SpringCount;
      //Adjust the column's size accumulators and resize if the result is >= 1.
      for I := 0 to FColumns.Count - 1 do
        if [coVisible, coAutoSpring] * FColumns[I].Options = [coVisible, coAutoSpring] then
        begin
          //Sum up rest changes from previous runs and the amount from this one and store it in the
          //column. If there is at least one pixel difference then do a resize and reset the accumulator.
          NewAccumulator := FColumns[I].SpringRest + Difference;
          //Set new width if at least one pixel size difference is reached.
          if NewAccumulator >= 1 then
            TVirtualTreeColumnCracker(FColumns[I]).SetWidth(FColumns[I].Width + (Trunc(NewAccumulator) * Sign));
          FColumns[I].SpringRest := Frac(NewAccumulator);

          //Keep track of the size count.
          ChangeBy := ChangeBy - Difference;
          //Exit loop if resize count drops below freezing point.
          if ChangeBy < 0 then
            Break;
        end;
    end;
  end;
  with Tree do
    FLastWidth := HeaderRect.Right - HeaderRect.Left;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  //--- HACK WARNING!
  //This type cast is a partial rewrite of the private section of TWriter. The purpose is to have access to
  //the FPropPath member, which is otherwise not accessible. The reason why this access is needed is that
  //with nested components this member contains unneeded property path information. These information prevent
  //successful load of the stored properties later.
  //In System.Classes.pas you can see that FPropPath is reset several times to '' to prevent this case for certain properies.
  //Unfortunately, there is no clean way for us here to do the same.
{$HINTS off}
  TWriterHack = class(TFiler)
  private
    FRootAncestor : TComponent;
    FPropPath     : string;
  end;
{$HINTS on}


procedure TVTHeader.WriteColumns(Writer : TWriter);

//Write out the columns but take care for the case VT is a nested component.

var
  LastPropPath : string;

begin
  //Save last property path for restoration.
  LastPropPath := TWriterHack(Writer).FPropPath;
  try
    //If VT is a nested component then this path contains the name of the parent component at this time
    //(otherwise it is already empty). This path is then combined with the property name under which the tree
    //is defined in the parent component. Unfortunately, the load code in System.Classes.pas does not consider this case
    //is then unable to load this property.
    TWriterHack(Writer).FPropPath := '';
    Writer.WriteCollection(Columns);
  finally
    TWriterHack(Writer).FPropPath := LastPropPath;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.AllowFocus(ColumnIndex : TColumnIndex) : Boolean;
begin
  Result := False;
  if not FColumns.IsValidColumn(ColumnIndex) then
    Exit; //Just in case.

  Result := (coAllowFocus in FColumns[ColumnIndex].Options);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.Assign(Source : TPersistent);

begin
  if Source is TVTHeader then
  begin
    AutoSizeIndex := TVTHeader(Source).AutoSizeIndex;
    Background := TVTHeader(Source).Background;
    Columns := TVTHeader(Source).Columns;
    Font := TVTHeader(Source).Font;
    FixedAreaConstraints.Assign(TVTHeader(Source).FixedAreaConstraints);
    Height := TVTHeader(Source).Height;
    Images := TVTHeader(Source).Images;
    MainColumn := TVTHeader(Source).MainColumn;
    Options := TVTHeader(Source).Options;
    ParentFont := TVTHeader(Source).ParentFont;
    PopupMenu := TVTHeader(Source).PopupMenu;
    SortColumn := TVTHeader(Source).SortColumn;
    SortDirection := TVTHeader(Source).SortDirection;
    Style := TVTHeader(Source).Style;

    RescaleHeader;
  end
  else
    inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.AutoFitColumns(Animated : Boolean = True; SmartAutoFitType : TSmartAutoFitType = smaUseColumnOption; RangeStartCol : Integer = NoColumn;
  RangeEndCol : Integer = NoColumn);

//--------------- local functions -------------------------------------------

  function GetUseSmartColumnWidth(ColumnIndex : TColumnIndex) : Boolean;

  begin
    case SmartAutoFitType of
      smaAllColumns :
        Result := True;
      smaUseColumnOption :
        Result := coSmartResize in FColumns.Items[ColumnIndex].Options;
    else
      Result := False;
    end;
  end;

//----------------------------------------------------------------------------

  procedure DoAutoFitColumn(Column : TColumnIndex);

  begin
    with TVirtualTreeColumnsCracker(FColumns) do
      if ([coResizable, coVisible] * Items[PositionToIndex[Column]].Options = [coResizable, coVisible]) and DoBeforeAutoFitColumn(PositionToIndex[Column], SmartAutoFitType) and
        not Tree.OperationCanceled then
      begin
        if Animated then
          AnimatedResize(PositionToIndex[Column], Tree.GetMaxColumnWidth(PositionToIndex[Column], GetUseSmartColumnWidth(PositionToIndex[Column])))
        else
          FColumns[PositionToIndex[Column]].Width := Tree.GetMaxColumnWidth(PositionToIndex[Column], GetUseSmartColumnWidth(PositionToIndex[Column]));

        DoAfterAutoFitColumn(PositionToIndex[Column]);
      end;
  end;

//--------------- end local functions ----------------------------------------

var
  I                : Integer;
  StartCol, EndCol : Integer;

begin
  StartCol := Max(NoColumn + 1, RangeStartCol);

  if RangeEndCol <= NoColumn then
    EndCol := FColumns.Count - 1
  else
    EndCol := Min(RangeEndCol, FColumns.Count - 1);

  if StartCol > EndCol then
    Exit; //nothing to do

  Tree.StartOperation(okAutoFitColumns);
  FDoingAutoFitColumns := True;
  try
    if Assigned(Tree.OnBeforeAutoFitColumns) then
      Tree.OnBeforeAutoFitColumns(Self, SmartAutoFitType);

    for I := StartCol to EndCol do
      DoAutoFitColumn(I);

    if Assigned(Tree.OnAfterAutoFitColumns) then
      Tree.OnAfterAutoFitColumns(Self);

  finally
    Tree.EndOperation(okAutoFitColumns);
    Tree.Invalidate();
    FDoingAutoFitColumns := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.InHeader(P : TPoint) : Boolean;

//Determines whether the given point (client coordinates!) is within the header rectangle (non-client coordinates).

var
  R, RW : TRect;

begin
  R := Tree.HeaderRect;

  //Current position of the owner in screen coordinates.
  GetWindowRect(Tree.Handle, RW);

  //Convert to client coordinates.
  MapWindowPoints(0, Tree.Handle, RW, 2);

  //Consider the header within this rectangle.
  OffsetRect(R, RW.Left, RW.Top);
  Result := PtInRect(R, P);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.InHeaderSplitterArea(P : TPoint) : Boolean;

//Determines whether the given point (client coordinates!) hits the horizontal splitter area of the header.

var
  R, RW : TRect;

begin
  if (P.Y > 2) or (P.Y < - 2) or not (hoVisible in FOptions) then
    Result := False
  else
  begin
    R := Tree.HeaderRect;
    Inc(R.Bottom, 2);

    //Current position of the owner in screen coordinates.
    GetWindowRect(Tree.Handle, RW);

    //Convert to client coordinates.
    MapWindowPoints(0, Tree.Handle, RW, 2);

    //Consider the header within this rectangle.
    OffsetRect(R, RW.Left, RW.Top);
    Result := PtInRect(R, P);
  end;
end;

procedure TVTHeader.InternalSetAutoSizeIndex(const Index : TColumnIndex);
begin
  FAutoSizeIndex := index;
end;

procedure TVTHeader.InternalSetMainColumn(const Index : TColumnIndex);
begin
  FMainColumn := index;
end;

procedure TVTHeader.InternalSetSortColumn(const Index : TColumnIndex);
begin
  FSortColumn := index;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.Invalidate(Column : TVirtualTreeColumn; ExpandToBorder : Boolean = False; UpdateNowFlag : Boolean = False);

//Because the header is in the non-client area of the tree it needs some special handling in order to initiate its
//repainting.
//If ExpandToBorder is True then not only the given column but everything or (depending on hoFullRepaintOnResize) just
//everything to its right (or left, in RTL mode) will be invalidated (useful for resizing). This makes only sense when
//a column is given.

var
  R, RW : TRect;
  Flags : Cardinal;

begin
  if (hoVisible in FOptions) and Tree.HandleAllocated then
    with Tree do
    begin
      if Column = nil then
        R := HeaderRect
      else
      begin
        R := Column.GetRect;
        if not (coFixed in Column.Options) then
          OffsetRect(R, - EffectiveOffsetX, 0);
        if UseRightToLeftAlignment then
          OffsetRect(R, ComputeRTLOffset, 0);
        if ExpandToBorder then
        begin
          if (hoFullRepaintOnResize in Header.Options) then
          begin
            R.Left := HeaderRect.Left;
            R.Right := HeaderRect.Right;
          end
          else
          begin
            if UseRightToLeftAlignment then
              R.Left := HeaderRect.Left
            else
              R.Right := HeaderRect.Right;
          end;
        end;
      end;
      R.Bottom := Tree.ClientHeight; //We want to repaint the entire column to bottom, not just the header

      //Current position of the owner in screen coordinates.
      GetWindowRect(Handle, RW);

      //Consider the header within this rectangle.
      OffsetRect(R, RW.Left, RW.Top);

      //Expressed in client coordinates (because RedrawWindow wants them so, they will actually become negative).
      MapWindowPoints(0, Handle, R, 2);
      Flags := RDW_FRAME or RDW_INVALIDATE or RDW_VALIDATE or RDW_NOINTERNALPAINT or RDW_NOERASE or RDW_NOCHILDREN;
      if UpdateNowFlag then
        Flags := Flags or RDW_UPDATENOW;
      RedrawWindow(Handle, @R, 0, Flags);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.LoadFromStream(const Stream : TStream);

//restore the state of the header from the given stream

var
  Dummy, Version : Integer;
  S              : AnsiString;
  OldOptions     : TVTHeaderOptions;

begin
  Include(FStates, hsLoading);
  with Stream do
    try
      //Switch off all options which could influence loading the columns (they will be later set again).
      OldOptions := FOptions;
      FOptions := [];

      //Determine whether the stream contains data without a version number.
      ReadBuffer(Dummy, SizeOf(Dummy));
      if Dummy > - 1 then
      begin
        //Seek back to undo the read operation if this is an old stream format.
        Seek( - SizeOf(Dummy), soFromCurrent);
        Version := - 1;
      end
      else //Read version number if this is a "versionized" format.
        ReadBuffer(Version, SizeOf(Version));
      Columns.LoadFromStream(Stream, Version);

      ReadBuffer(Dummy, SizeOf(Dummy));
      AutoSizeIndex := Dummy;
      ReadBuffer(Dummy, SizeOf(Dummy));
      Background := Dummy;
      ReadBuffer(Dummy, SizeOf(Dummy));
      Height := Dummy;
      ReadBuffer(Dummy, SizeOf(Dummy));
      FOptions := OldOptions;
      Options := TVTHeaderOptions(Dummy);
      //PopupMenu is neither saved nor restored
      ReadBuffer(Dummy, SizeOf(Dummy));
      Style := TVTHeaderStyle(Dummy);
      //TFont has no own save routine so we do it manually
      with Font do
      begin
        ReadBuffer(Dummy, SizeOf(Dummy));
        Color := Dummy;
        ReadBuffer(Dummy, SizeOf(Dummy));
        Height := Dummy;
        ReadBuffer(Dummy, SizeOf(Dummy));
        SetLength(S, Dummy);
        ReadBuffer(PAnsiChar(S)^, Dummy);
        Name := UTF8ToString(S);
        ReadBuffer(Dummy, SizeOf(Dummy));
        Pitch := TFontPitch(Dummy);
        ReadBuffer(Dummy, SizeOf(Dummy));
        Style := TFontStyles(Byte(Dummy));
      end;

      //Read data introduced by stream version 1+.
      if Version > 0 then
      begin
        ReadBuffer(Dummy, SizeOf(Dummy));
        MainColumn := Dummy;
        ReadBuffer(Dummy, SizeOf(Dummy));
        SortColumn := Dummy;
        ReadBuffer(Dummy, SizeOf(Dummy));
        SortDirection := TSortDirection(Byte(Dummy));
      end;

      //Read data introduced by stream version 5+.
      if Version > 4 then
      begin
        ReadBuffer(Dummy, SizeOf(Dummy));
        ParentFont := Boolean(Dummy);
        ReadBuffer(Dummy, SizeOf(Dummy));
        FMaxHeight := Integer(Dummy);
        ReadBuffer(Dummy, SizeOf(Dummy));
        FMinHeight := Integer(Dummy);
        ReadBuffer(Dummy, SizeOf(Dummy));
        FDefaultHeight := Integer(Dummy);
        with FFixedAreaConstraints do
        begin
          ReadBuffer(Dummy, SizeOf(Dummy));
          FMaxHeightPercent := TVTConstraintPercent(Dummy);
          ReadBuffer(Dummy, SizeOf(Dummy));
          FMaxWidthPercent := TVTConstraintPercent(Dummy);
          ReadBuffer(Dummy, SizeOf(Dummy));
          FMinHeightPercent := TVTConstraintPercent(Dummy);
          ReadBuffer(Dummy, SizeOf(Dummy));
          FMinWidthPercent := TVTConstraintPercent(Dummy);
        end;
      end;
    finally
      Exclude(FStates, hsLoading);
      RecalculateHeader();
      Tree.DoColumnResize(NoColumn);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTHeader.ResizeColumns(ChangeBy : Integer; RangeStartCol : TColumnIndex; RangeEndCol : TColumnIndex; Options : TVTColumnOptions = [coVisible]) : Integer;

//Distribute the given width change to a range of columns. A 'fair' way is used to distribute ChangeBy to the columns,
//while ensuring that everything that can be distributed will be distributed.

var
  Start, I                                         : TColumnIndex;
  ColCount, ToGo, Sign, Rest, MaxDelta, Difference : Integer;
  Constraints, Widths                              : array of Integer;
  BonusPixel                                       : Boolean;

  //--------------- local functions -------------------------------------------

  function IsResizable(Column : TColumnIndex) : Boolean;

  begin
    if BonusPixel then
      Result := Widths[Column - RangeStartCol] < Constraints[Column - RangeStartCol]
    else
      Result := Widths[Column - RangeStartCol] > Constraints[Column - RangeStartCol];
  end;

//---------------------------------------------------------------------------

  procedure IncDelta(Column : TColumnIndex);

  begin
    if BonusPixel then
      Inc(MaxDelta, FColumns[Column].MaxWidth - Widths[Column - RangeStartCol])
    else
      Inc(MaxDelta, Widths[Column - RangeStartCol] - Constraints[Column - RangeStartCol]);
  end;

//---------------------------------------------------------------------------

  function ChangeWidth(Column : TColumnIndex; Delta : Integer) : Integer;

  begin
    if Delta > 0 then
      Delta := Min(Delta, Constraints[Column - RangeStartCol] - Widths[Column - RangeStartCol])
    else
      Delta := Max(Delta, Constraints[Column - RangeStartCol] - Widths[Column - RangeStartCol]);

    Inc(Widths[Column - RangeStartCol], Delta);
    Dec(ToGo, Abs(Delta));
    Result := Abs(Delta);
  end;

//---------------------------------------------------------------------------

  function ReduceConstraints : Boolean;

  var
    MaxWidth, MaxReserveCol, Column : TColumnIndex;

  begin
    Result := True;
    if not (hsScaling in FStates) or BonusPixel then
      Exit;

    MaxWidth := 0;
    MaxReserveCol := NoColumn;
    for Column := RangeStartCol to RangeEndCol do
      if (Options * FColumns[Column].Options = Options) and (FColumns[Column].Width > MaxWidth) then
      begin
        MaxWidth := Widths[Column - RangeStartCol];
        MaxReserveCol := Column;
      end;

    if (MaxReserveCol <= NoColumn) or (Constraints[MaxReserveCol - RangeStartCol] <= 10) then
      Result := False
    else
      Dec(Constraints[MaxReserveCol - RangeStartCol], Constraints[MaxReserveCol - RangeStartCol] div 10);
  end;

//----------- end local functions -------------------------------------------

begin
  Result := 0;
  if ChangeBy <> 0 then
  begin
    //Do some initialization here
    BonusPixel := ChangeBy > 0;
    Sign := IfThen(BonusPixel, 1, - 1);
    Start := IfThen(BonusPixel, RangeStartCol, RangeEndCol);
    ToGo := Abs(ChangeBy);
    SetLength(Widths, RangeEndCol - RangeStartCol + 1);
    SetLength(Constraints, RangeEndCol - RangeStartCol + 1);
    for I := RangeStartCol to RangeEndCol do
    begin
      Widths[I - RangeStartCol] := FColumns[I].Width;
      Constraints[I - RangeStartCol] := IfThen(BonusPixel, FColumns[I].MaxWidth, FColumns[I].MinWidth);
    end;

    repeat
      repeat
        MaxDelta := 0;
        ColCount := 0;
        for I := RangeStartCol to RangeEndCol do
          if (Options * FColumns[I].Options = Options) and IsResizable(I) then
          begin
            Inc(ColCount);
            IncDelta(I);
          end;
        if MaxDelta < Abs(ChangeBy) then
          if not ReduceConstraints then
            Break;
      until (MaxDelta >= Abs(ChangeBy)) or not (hsScaling in FStates);

      if ColCount = 0 then
        Break;

      ToGo := Min(ToGo, MaxDelta);
      Difference := ToGo div ColCount;
      Rest := ToGo mod ColCount;

      if Difference > 0 then
        for I := RangeStartCol to RangeEndCol do
          if (Options * FColumns[I].Options = Options) and IsResizable(I) then
            ChangeWidth(I, Difference * Sign);

      //Now distribute Rest.
      I := Start;
      while Rest > 0 do
      begin
        if (Options * FColumns[I].Options = Options) and IsResizable(I) then
          if FColumns[I].BonusPixel <> BonusPixel then
          begin
            Dec(Rest, ChangeWidth(I, Sign));
            FColumns[I].BonusPixel := BonusPixel;
          end;
        Inc(I, Sign);
        if (BonusPixel and (I > RangeEndCol)) or (not BonusPixel and (I < RangeStartCol)) then
        begin
          for I := RangeStartCol to RangeEndCol do
            if Options * FColumns[I].Options = Options then
              FColumns[I].BonusPixel := not FColumns[I].BonusPixel;
          I := Start;
        end;
      end;
    until ToGo <= 0;

    //Now set the computed widths. We also compute the result here.
    Include(FStates, hsResizing);
    for I := RangeStartCol to RangeEndCol do
      if (Options * FColumns[I].Options = Options) then
      begin
        Inc(Result, Widths[I - RangeStartCol] - FColumns[I].Width);
        TVirtualTreeColumnCracker(FColumns[I]).SetWidth(Widths[I - RangeStartCol]);
      end;
    Exclude(FStates, hsResizing);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.RestoreColumns;

//Restores all columns to their width which they had before they have been auto fitted.

var
  I : TColumnIndex;

begin
  with TVirtualTreeColumnsCracker(FColumns) do
    for I := Count - 1 downto 0 do
      if [coResizable, coVisible] * Items[PositionToIndex[I]].Options = [coResizable, coVisible] then
        Items[I].RestoreLastWidth;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeader.SaveToStream(const Stream : TStream);

//Saves the complete state of the header into the provided stream.

var
  Dummy : Integer;
  Tmp   : AnsiString;

begin
  with Stream do
  begin
    //In previous version of VT was no header stream version defined.
    //For feature enhancements it is necessary, however, to know which stream
    //format we are trying to load.
    //In order to distict from non-version streams an indicator is inserted.
    Dummy := - 1;
    WriteBuffer(Dummy, SizeOf(Dummy));
    //Write current stream version number, nothing more is required at the time being.
    Dummy := VTHeaderStreamVersion;
    WriteBuffer(Dummy, SizeOf(Dummy));

    //Save columns in case they depend on certain options (like auto size).
    Columns.SaveToStream(Stream);

    Dummy := FAutoSizeIndex;
    WriteBuffer(Dummy, SizeOf(Dummy));
    Dummy := FBackgroundColor;
    WriteBuffer(Dummy, SizeOf(Dummy));
    Dummy := FHeight;
    WriteBuffer(Dummy, SizeOf(Dummy));
    Dummy := Integer(FOptions);
    WriteBuffer(Dummy, SizeOf(Dummy));
    //PopupMenu is neither saved nor restored
    Dummy := Ord(FStyle);
    WriteBuffer(Dummy, SizeOf(Dummy));
    //TFont has no own save routine so we do it manually
    with Font do
    begin
      Dummy := Color;
      WriteBuffer(Dummy, SizeOf(Dummy));

      //Need only to write one: size or height, I decided to write height.
      Dummy := Height;
      WriteBuffer(Dummy, SizeOf(Dummy));
      Tmp := UTF8Encode(Name);
      Dummy := Length(Tmp);
      WriteBuffer(Dummy, SizeOf(Dummy));
      WriteBuffer(PAnsiChar(Tmp)^, Dummy);
      Dummy := Ord(Pitch);
      WriteBuffer(Dummy, SizeOf(Dummy));
      Dummy := Byte(Style);
      WriteBuffer(Dummy, SizeOf(Dummy));
    end;

    //Data introduced by stream version 1.
    Dummy := FMainColumn;
    WriteBuffer(Dummy, SizeOf(Dummy));
    Dummy := FSortColumn;
    WriteBuffer(Dummy, SizeOf(Dummy));
    Dummy := Byte(FSortDirection);
    WriteBuffer(Dummy, SizeOf(Dummy));

    //Data introduced by stream version 5.
    Dummy := Integer(ParentFont);
    WriteBuffer(Dummy, SizeOf(Dummy));
    Dummy := Integer(FMaxHeight);
    WriteBuffer(Dummy, SizeOf(Dummy));
    Dummy := Integer(FMinHeight);
    WriteBuffer(Dummy, SizeOf(Dummy));
    Dummy := Integer(FDefaultHeight);
    WriteBuffer(Dummy, SizeOf(Dummy));
    with FFixedAreaConstraints do
    begin
      Dummy := Integer(FMaxHeightPercent);
      WriteBuffer(Dummy, SizeOf(Dummy));
      Dummy := Integer(FMaxWidthPercent);
      WriteBuffer(Dummy, SizeOf(Dummy));
      Dummy := Integer(FMinHeightPercent);
      WriteBuffer(Dummy, SizeOf(Dummy));
      Dummy := Integer(FMinWidthPercent);
      WriteBuffer(Dummy, SizeOf(Dummy));
    end;
  end;
end;

{ TVTHeaderHelper }

function TVTHeaderHelper.Tree : TBaseVirtualTreeCracker;
begin
  Result := TBaseVirtualTreeCracker(Self.FOwner);
end;

end.
