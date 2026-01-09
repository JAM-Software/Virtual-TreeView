unit VTCellSelectionTests;

// Virtual Treeview cell selection tests
// Written by CheeWee Chua.

interface

uses
  DUnitX.TestFramework, Vcl.Forms, VirtualTrees, System.Types;

type

  [TestFixture]
  TCellSelectionTests = class(TObject)
  strict private
    FTree: TVirtualStringTree;
    FForm: TForm;
    FNode1,
    FNode2,
    FNode3,
    FNode4,
    FNode5,
    FNode6,
    FNode7,
    FNode8: PVirtualNode;

    procedure MouseClick(ACursorPos: TPoint); overload;
    procedure MouseClick(ANode: PVirtualNode); overload;
    procedure ShiftMouseClick(ANode: PVirtualNode); overload;

    procedure FreeNodeEvent(Sender: TBaseVirtualTree; Node: PVirtualNode);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestSelectSingleCell;

    [Test]
    procedure TestShiftClickMultipleCells;

    [Test]
    procedure TestClearCellSelection;

    [Test]
    procedure TestSelectMultipleCellsFailWithoutMultiSelect;

    [Test]
    procedure TestSelectMultipleCellsFailWithoutExtendedFocus;

    [Test]
    procedure TestSelectCellsRectangular;

    [Test]
    procedure TestClickUnselectsSelectedCells;
  end;

implementation

uses
  System.SysUtils, Vcl.Controls, VirtualTrees.Types,
  Winapi.Messages, Winapi.Windows;

type
  TRowData = record
    table_schema: string;
    table_name: string;
    table_type: string;
    // 4th column has no data
  public
    constructor Create(const ASchema, AName, AType: string);
    procedure Clear;
    class operator Finalize(var Self: TRowData);
  end;

{ TRowData }

procedure TRowData.Clear;
begin
  table_schema := '';
  table_name   := '';
  table_type   := '';
end;

constructor TRowData.Create(const ASchema, AName, AType: string);
begin
  table_schema := ASchema;
  table_name   := AName;
  table_type   := AType;
end;

class operator TRowData.Finalize(var Self: TRowData);
begin
  Self.Clear;
end;

const
  colSchema = 0;
  colName   = 1;
  colType   = 2;
  // skip the 4th column

{ TCellSelectionTests }

procedure TCellSelectionTests.MouseClick(ACursorPos: TPoint);
const
  KEYDOWN = Byte(1 shl 7);
var
  LKeyboardState: TKeyboardState;
begin
  // Click a new cell on the tree...
  var LTree := FTree;
  var LSavedCursorPos := Mouse.CursorPos;
  try
    Mouse.CursorPos := ACursorPos;
    var LWPARAM: WPARAM := MK_LBUTTON;
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
    var LPos := MakeLParam(ACursorPos.X, ACursorPos.Y);
    LTree.Perform(WM_LBUTTONDOWN, LWPARAM, LPos);
    LTree.Perform(WM_LBUTTONUP, LWPARAM, LPos);
  finally
    Mouse.CursorPos := LSavedCursorPos;
  end;
end;

procedure TCellSelectionTests.FreeNodeEvent(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  var LRowData := Node.GetData<TRowData>;
  LRowData.Clear;
end;

procedure TCellSelectionTests.ShiftMouseClick(ANode: PVirtualNode);
var
  LOrigKBState, LNewKBState: TKeyboardState;
begin
  GetKeyboardState(LOrigKBState);
  LNewKBState := LOrigKBState;
  LNewKBState[VK_SHIFT] := LOrigKBState[VK_SHIFT] or (1 shl 7);
  SetKeyboardState(LNewKBState);
  try
    MouseClick(ANode);
  finally
    SetKeyboardState(LOrigKBState);
  end;
end;

procedure TCellSelectionTests.MouseClick(ANode: PVirtualNode);
begin
  var LTree := FTree;
  var LClientRect := LTree.GetDisplayRect(ANode, 0, True);
  MouseClick(LClientRect.TopLeft);
end;

procedure TCellSelectionTests.Setup;
begin
  FForm := TForm.Create(nil);
  FTree := TVirtualStringTree.Create(FForm);
  FTree.Parent := FForm;
  FTree.Align := alClient;
  FTree.OnFreeNode := FreeNodeEvent;

  var LTree := FTree;

  // Add three columns
  var LCol1 := LTree.Header.Columns.Add;
  var LCol2 := LTree.Header.Columns.Add;
  var LCol3 := LTree.Header.Columns.Add;
  var LCol4 := LTree.Header.Columns.Add;
  LCol1.Text := 'table_schema';
  LCol2.Text := 'table_name';
  LCol3.Text := 'table_type';
  LCol4.Text := 'table_version';

  LTree.NodeDataSize := SizeOf(TRowData);

  FNode1 := LTree.AddChild(LTree.RootNode);
  FNode2 := LTree.AddChild(LTree.RootNode);
  FNode3 := LTree.AddChild(LTree.RootNode);
  FNode4 := LTree.AddChild(LTree.RootNode);
  FNode5 := LTree.AddChild(LTree.RootNode);
  FNode6 := LTree.AddChild(LTree.RootNode);
  FNode7 := LTree.AddChild(LTree.RootNode);
  FNode8 := LTree.AddChild(LTree.RootNode);

  FNode1.SetData<TRowData>(TRowData.Create('pg_catalog', 'pg_user_info',  'VIEW'));
  FNode2.SetData<TRowData>(TRowData.Create('pg_catalog', 'pg_stastic',    'BASE TABLE'));
  FNode3.SetData<TRowData>(TRowData.Create('pg_catalog', 'pg_settings',   'VIEW'));
  FNode4.SetData<TRowData>(TRowData.Create('pg_catalog', 'pg_type',       'VIEW'));
  FNode5.SetData<TRowData>(TRowData.Create('pg_catalog', 'pg_attribute',  'BASE TABLE'));
  FNode6.SetData<TRowData>(TRowData.Create('pg_catalog', 'pg_class',      'BASE TABLE'));
  FNode7.SetData<TRowData>(TRowData.Create('pg_catalog', 'pg_tablespace', 'BASE TABLE'));
  FNode8.SetData<TRowData>(TRowData.Create('pg_catalog', 'pg_inherits',   'BASE TABLE'));
end;

procedure TCellSelectionTests.TearDown;
begin
  FreeAndNil(FForm);
end;

procedure TCellSelectionTests.TestClearCellSelection;
begin
  var LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  var n3 := FNode3;
  var n4 := FNode4;

  // Select rectangle from n3, col2 to n2, col3
  LTree.SelectCells(n3, 1, n4, 2, False);

  Assert.IsTrue(toExtendedFocus in LTree.TreeOptions.SelectionOptions);
  Assert.IsTrue(toMultiSelect in LTree.TreeOptions.SelectionOptions);

  // Ensure the selected cells above are selected
  Assert.IsTrue(LTree.IsCellSelected(n3, 1), 'n3, col1 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n3, 2), 'n3, col2 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n4, 1), 'n4, col1 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n4, 2), 'n4, col2 should be selected');

  var LSelectedCells := LTree.SelectedCells;
  Assert.IsTrue(Length(LSelectedCells) = 4, 'Length of selected cells is not 4!');

  LTree.ClearCellSelection;
  LSelectedCells := LTree.SelectedCells;
  Assert.IsTrue(Length(LSelectedCells) = 0, 'Length of selected cells is not 0!');
end;

procedure TCellSelectionTests.TestClickUnselectsSelectedCells;
begin
  var LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  var n3 := FNode3;
  var n4 := FNode4;

  // Select rectangle from n3, col2 to n2, col3
  LTree.SelectCells(n3, 1, n4, 2, False);

  Assert.IsTrue(toExtendedFocus in LTree.TreeOptions.SelectionOptions);
  Assert.IsTrue(toMultiSelect in LTree.TreeOptions.SelectionOptions);

  // Ensure the selected cells above are selected
  Assert.IsTrue(LTree.IsCellSelected(n3, 1), 'n3, col1 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n3, 2), 'n3, col2 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n4, 1), 'n4, col1 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n4, 2), 'n4, col2 should be selected');

  var LSelCel1 := LTree.SelectedCells;
  var n1 := FNode1;
  MouseClick(n1);
  var LSelCel2 := LTree.SelectedCells;

  // Ensures the above cells are no longer selected
  Assert.IsFalse(LTree.IsCellSelected(n3, 1), 'n3, col1 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n3, 2), 'n3, col2 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 1), 'n4, col1 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 2), 'n4, col2 should not be selected');

  Assert.IsTrue(LTree.IsCellSelected(n1, 0), 'n1, col0 should be selected');
end;

procedure TCellSelectionTests.TestSelectCellsRectangular;
begin

  var LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  var n3 := FNode3;
  var n4 := FNode4;

  // Select rectangle from n3, col2 to n2, col3
  LTree.SelectCells(n3, 1, n4, 2, False);

  Assert.IsTrue(toExtendedFocus in LTree.TreeOptions.SelectionOptions);
  Assert.IsTrue(toMultiSelect in LTree.TreeOptions.SelectionOptions);

  // Ensure the selected cells above are selected
  Assert.IsTrue(LTree.IsCellSelected(n3, 1), 'n3, col1 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n3, 2), 'n3, col2 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n4, 1), 'n4, col1 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n4, 2), 'n4, col2 should be selected');

  // Ensure the non-selected cells are not selected
  Assert.IsFalse(LTree.IsCellSelected(n3, 0), 'n3, col0 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n3, 3), 'n3, col3 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 0), 'n4, col0 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 3), 'n4, col3 should not be selected');

  var LNodes := [FNode1, FNode2, FNode5, FNode6, FNode7, FNode8];
  for var LNode in LNodes do
    for var LColumn := 0 to 3 do
      Assert.IsFalse(LTree.IsCellSelected(LNode, LColumn),
        Format('Row: $%p Column: %d should not be selected', [Pointer(LNode), LColumn]));
end;

procedure TCellSelectionTests.TestSelectMultipleCellsFailWithoutExtendedFocus;
begin
  var LTree := FTree;
  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions -
    [toExtendedFocus];

  var n3 := FNode3;
  var n4 := FNode4;

  // Select rectangle from n3, col2 to n2, col3
  LTree.SelectCells(n3, 1, n4, 2, False);

  // Ensure the selected cells above are not selected
  Assert.IsFalse(LTree.IsCellSelected(n3, 1), 'n3, col1 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n3, 2), 'n3, col2 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 1), 'n4, col1 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 2), 'n4, col2 should not be selected');
end;

procedure TCellSelectionTests.TestSelectMultipleCellsFailWithoutMultiSelect;
begin
  var LTree := FTree;
  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions -
    [toMultiSelect];

  var n3 := FNode3;
  var n4 := FNode4;

  // Select rectangle from n3, col2 to n2, col3
  LTree.SelectCells(n3, 1, n4, 2, False);

  // Ensure the selected cells above are not selected
  Assert.IsFalse(LTree.IsCellSelected(n3, 1), 'n3, col1 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n3, 2), 'n3, col2 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 1), 'n4, col1 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 2), 'n4, col2 should not be selected');
end;

procedure TCellSelectionTests.TestSelectSingleCell;
begin
  var LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect];

  var n3 := FNode3;
  LTree.SelectCells(n3, 1, n3, 1, False);

  Assert.IsTrue(LTree.IsCellSelected(n3, 1), 'n3, col1 should be selected');
  var LSelectedCells := LTree.SelectedCells;
  Assert.IsTrue(Length(LSelectedCells) = 1, 'Should only have 1 cell selected');
  Assert.IsTrue((LSelectedCells[0].Node = n3) and (LSelectedCells[0].Column = 1));

  var LNodes := [FNode1, FNode2, n3, FNode4, FNode5, FNode6, FNode7, FNode8];
  for var LNode in LNodes do
    for var LColumn := 0 to 3 do
    begin
      if (LNode = n3) and (LColumn = 1) then
        Continue;
      Assert.IsFalse(LTree.IsCellSelected(LNode, LColumn),
        Format('Row: $%p Column: %d should not be selected', [Pointer(LNode), LColumn]));
    end;
end;

procedure TCellSelectionTests.TestShiftClickMultipleCells;
begin
  var LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  var n3 := FNode3;
  var n4 := FNode4;

  MouseClick(n3);
  var LSelectedCells := LTree.SelectedCells;
  ShiftMouseClick(n4);
  var LNewSelectedCells := LTree.SelectedCells;
  Assert.IsTrue(Length(LNewSelectedCells) = 2);
  Assert.IsTrue((LNewSelectedCells[0].Node = n3) and (LNewSelectedCells[0].Column = 0));
  Assert.IsTrue((LNewSelectedCells[1].Node = n4) and (LNewSelectedCells[0].Column = 0));
end;

initialization
  TDUnitX.RegisterTestFixture(TCellSelectionTests);
end.
