unit VirtualTrees.MouseUtils;

interface

uses
  VirtualTrees, System.Types;

type

  /// <summary>
  /// Created to be used only for testing
  /// </summary>
  TCustomVirtualStringTreeMouseHelper = class helper for TCustomVirtualStringTree
  protected
  const
    KEYDOWN = Byte(1 shl 7);
  public
    function GetDisplayRectEx(ANode: PVirtualNode; AColumn: TColumnIndex): TPoint;

    procedure KeyedMouseClick(Key: Byte; ACursorPos: TPoint); overload;
    procedure KeyedMouseClick(Key: Byte; ANode: PVirtualNode; AColumn: TColumnIndex = 0); overload;

    procedure MouseClick(ACursorPos: TPoint); overload;
    procedure MouseClick(ANode: PVirtualNode; AColumn: TColumnIndex = 0); overload;

    procedure CtrlMouseClick(ACursorPos: TPoint); overload;
    procedure CtrlMouseClick(ANode: PVirtualNode; AColumn: TColumnIndex = 0); overload;

    procedure ShiftMouseClick(ANode: PVirtualNode; AColumn: TColumnIndex = 0); overload;
  end;

implementation

uses
  Winapi.Windows, Vcl.Controls, Winapi.Messages, VirtualTrees.Types, System.Math,
  System.SysUtils;

{ TCustomVirtualStringTreeMouseHelper }

function TCustomVirtualStringTreeMouseHelper.GetDisplayRectEx(
  ANode: PVirtualNode; AColumn: TColumnIndex): TPoint;
var
  R: TRect;
  LRight: TDimension;
begin
  if not Assigned(ANode) then
  begin
    Result := Point(0, 0);
    Exit;
  end;

  // Use the full-row rect to get a reliable Y coordinate for hit testing.
  R := GetDisplayRect(ANode, NoColumn, False, False, False);

  if R.IsEmpty then
  begin
    Exit(Point(0, 0));
  end;

  Result.Y := R.Top + (R.Bottom - R.Top) div 2;
  Header.Columns.GetColumnBounds(AColumn, Result.X, LRight);

  // If header is visible the client coordinates for hit testing are below the header.
  if hoVisible in Header.Options then
    Inc(Result.Y, Header.Height);
end;

procedure TCustomVirtualStringTreeMouseHelper.MouseClick(ACursorPos: TPoint);
var
  LKeyboardState: TKeyboardState;
  LTree: TCustomVirtualStringTree;
  LSavedCursorPos: TPoint;
  LWPARAM: WPARAM;
  LPos: LPARAM;
begin
  // Click a new cell on the tree...
  LTree := Self;
  LSavedCursorPos := Mouse.CursorPos;
  try
    Mouse.CursorPos := ACursorPos;
    LWPARAM := MK_LBUTTON;
    if GetKeyboardState(LKeyboardState) then
      begin
        if (LKeyboardState[VK_SHIFT] and KEYDOWN <> 0) or
           (LKeyboardState[VK_LSHIFT] and KEYDOWN <> 0) or
           (LKeyboardState[VK_RSHIFT] and KEYDOWN <> 0) then
          LWPARAM := LWPARAM or MK_SHIFT;
        if (LKeyboardState[VK_CONTROL] and KEYDOWN <> 0) or
           (LKeyboardState[VK_LCONTROL] and KEYDOWN <> 0) or
           (LKeyboardState[VK_RCONTROL] and KEYDOWN <> 0) then
          LWPARAM := LWPARAM or MK_CONTROL;
      end;
    LPos := MakeLParam(ACursorPos.X, ACursorPos.Y);
    LTree.Perform(WM_LBUTTONDOWN, LWPARAM, LPos);
    LTree.Perform(WM_LBUTTONUP, LWPARAM, LPos);
  finally
    Mouse.CursorPos := LSavedCursorPos;
  end;
end;

procedure TCustomVirtualStringTreeMouseHelper.KeyedMouseClick(
  Key: Byte; ACursorPos: TPoint);
var
  LOrigKBState, LNewKBState: TKeyboardState;
begin
  GetKeyboardState(LOrigKBState);
  LNewKBState := LOrigKBState;
  LNewKBState[Key] := LOrigKBState[Key] or KEYDOWN;
  SetKeyboardState(LNewKBState);
  try
    MouseClick(ACursorPos);
  finally
    SetKeyboardState(LOrigKBState);
  end;
end;

procedure TCustomVirtualStringTreeMouseHelper.KeyedMouseClick(
  Key: Byte; ANode: PVirtualNode; AColumn: TColumnIndex = 0);
var
  LOrigKBState, LNewKBState: TKeyboardState;
begin
  GetKeyboardState(LOrigKBState);
  LNewKBState := LOrigKBState;
  LNewKBState[Key] := LOrigKBState[Key] or KEYDOWN;
  SetKeyboardState(LNewKBState);
  try
    MouseClick(ANode, AColumn);
  finally
    SetKeyboardState(LOrigKBState);
  end;
end;

procedure TCustomVirtualStringTreeMouseHelper.MouseClick(ANode: PVirtualNode;
  AColumn: TColumnIndex = 0
);
var
  LTree: TCustomVirtualStringTree;
  LClientRect, LClientRect2: TRect;
  LHitInfo: THitInfo;
  LTopLeft: TPoint;
  LPasses, LCount: Integer;
begin
  LTree := Self;
  if not Assigned(ANode) then
    Exit;

  LClientRect := LTree.GetDisplayRect(ANode, AColumn, True, True, True);
  LTopLeft := LClientRect.TopLeft;
  if hoVisible in LTree.Header.Options then
    begin
      Inc(LTopLeft.Y, LTree.Header.Height);
    end;

  LPasses := 0;
  LCount := LTree.VisibleCount;
  repeat
    LTree.GetHitTestInfoAt(LTopLeft.X, LTopLeft.Y, True, LHitInfo, []);
    LClientRect2 := LTree.GetDisplayRect(LHitInfo.HitNode, AColumn, True);
    if LHitInfo.HitNode <> ANode then
      Inc(LTopLeft.Y, LHitInfo.HitNode.NodeHeight);
    Inc(LPasses); // Prevent forever loop
  until (LHitInfo.HitNode = ANode) or (LPasses > LCount);
  Assert((LHitInfo.HitNode = ANode) and (LHitInfo.HitColumn = AColumn));

  MouseClick(LTopLeft);
end;

procedure TCustomVirtualStringTreeMouseHelper.CtrlMouseClick(
  ANode: PVirtualNode; AColumn: TColumnIndex);
begin
  KeyedMouseClick(VK_CONTROL, ANode, AColumn);
end;

procedure TCustomVirtualStringTreeMouseHelper.CtrlMouseClick(
  ACursorPos: TPoint);
begin
  KeyedMouseClick(VK_CONTROL, ACursorPos);
end;

procedure TCustomVirtualStringTreeMouseHelper.ShiftMouseClick(
  ANode: PVirtualNode; AColumn: TColumnIndex = 0);
begin
  KeyedMouseClick(VK_SHIFT, ANode, AColumn);
end;

end.
