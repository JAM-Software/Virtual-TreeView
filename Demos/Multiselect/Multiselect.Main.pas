unit Multiselect.Main;

// Virtual Treeview sample form demonstrating following feature:
//   - Multiple cell selection.
// Written by CheeWee Chua.

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL,
  VirtualTrees, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    VirtualStringTree1: TVirtualStringTree;
    Panel1: TPanel;
    btnSelect4CellsLeftToRight: TButton;
    btnSelect4CellsRightToLeft: TButton;
    btnClickRow2Col1: TButton;
    btnClickRow1Col1: TButton;
    btnSelectRow3Col1Row4Col2: TButton;
    btnSelectRow2_3_Copy: TButton;
    procedure FormCreate(Sender: TObject);
    procedure VirtualStringTree1FreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure btnSelect4CellsLeftToRightClick(Sender: TObject);
    procedure btnSelect4CellsRightToLeftClick(Sender: TObject);
    procedure btnClickRow1Col1Click(Sender: TObject);
    procedure btnClickRow2Col1Click(Sender: TObject);
    procedure btnSelectRow3Col1Row4Col2Click(Sender: TObject);
    procedure btnSelectRow2_3_CopyClick(Sender: TObject);
  private
    { Private declarations }

    procedure EnableMulticellSelection;
    procedure EnableFullRowSelection;

    // These functions mimic human interaction with the user interface
    procedure MouseClick(const ACursorPos: TPoint); overload;
    procedure MouseClick(ANode: PVirtualNode; AColumn: TColumnIndex = 0); overload;
    procedure ShiftMouseClick(ANode: PVirtualNode; AColumn: TColumnIndex = 0); overload;
    procedure ShiftMouseClick(const ACell: TVTCell); overload;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  VirtualTrees.Types, VirtualTrees.Clipboard;

{$R *.dfm}

type
  TRowData = record
    table_schema: string;
    table_name: string;
    table_type: string;
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

procedure TForm1.btnClickRow1Col1Click(Sender: TObject);
begin
  EnableMulticellSelection;
  var LTree := VirtualStringTree1;
  var LNode := LTree.GetFirstChild(LTree.RootNode);
  MouseClick(LNode);
end;

procedure TForm1.btnClickRow2Col1Click(Sender: TObject);
begin
  EnableMulticellSelection;
  var LTree := VirtualStringTree1;
  var LNode := LTree.GetFirstChild(LTree.RootNode);
  LNode := LTree.GetNext(LNode);
  MouseClick(LNode);
end;

procedure TForm1.btnSelect4CellsLeftToRightClick(Sender: TObject);
begin
  EnableMulticellSelection;
  var LTree := VirtualStringTree1;
  LTree.ClearCellSelection;

  var LNode := LTree.GetFirstChild(LTree.RootNode);
  for var I := 1 to 2 do
    LNode := LTree.GetNext(LNode);
  // We're on 3rd row now...
  var L3rdRow := LNode; // 3rd row
  var L4thRow := LTree.GetNext(L3rdRow); // 4th row

  // Select cells from left to right
  LTree.SelectCells(
    L3rdRow, 1, // Left aka Start
    L4thRow, 2, // Right aka End
    True);
end;

procedure TForm1.btnSelect4CellsRightToLeftClick(Sender: TObject);
begin
  EnableMulticellSelection;
  var LTree := VirtualStringTree1;

  LTree.ClearCellSelection;

  var LNode := LTree.GetFirstChild(LTree.RootNode);
  for var I := 1 to 2 do
    LNode := LTree.GetNext(LNode);
  // We're on 3rd row now...
  var L3rdRow := LNode;
  var L4thRow := LTree.GetNext(L3rdRow); // 4th row
  LTree.SelectCells(
    L4thRow, 2,  // Right aka Start
    L3rdRow, 1,  // Left aka End
    True);
end;

procedure TForm1.btnSelectRow2_3_CopyClick(Sender: TObject);
begin
  // RegisterVTClipboardFormat(CF_TEXT, TVirtualStringTree);
  EnableFullRowSelection;
  var LTree := VirtualStringTree1;

  var LNode1 := LTree.GetFirstVisible();
  var LNode2 := LTree.GetNextVisible(LNode1);
  var LNode3 := LTree.GetNextVisible(LNode2);
  LTree.Selected[LNode2] := True;
  LTree.Selected[LNode3] := True;

  LTree.CopyToClipboard;
end;

procedure TForm1.btnSelectRow3Col1Row4Col2Click(Sender: TObject);
begin
  EnableMulticellSelection;
  var LTree := VirtualStringTree1;

  var LNode := LTree.GetFirstChild(LTree.RootNode);
  for var I := 1 to 2 do
    LNode := LTree.GetNext(LNode);
  // We're on 3rd row now...
  var L3rdRow := LNode; // 3rd row
  var L4thRow := LTree.GetNext(L3rdRow); // 4th row

  // column 1 in code is column 2 in human eyes...
  MouseClick(L3rdRow, 1);

  // column 2 in code is column 3 in human eyes...
  ShiftMouseClick(TVTCell.Create(L4thRow, 2));

  LTree.CopyToClipboard;
end;

procedure TForm1.EnableFullRowSelection;
begin
  var LTree := VirtualStringTree1;
  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toFullRowSelect];
end;

procedure TForm1.EnableMulticellSelection;
begin
  var LTree := VirtualStringTree1;
  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toMultiSelect, toExtendedFocus] - [toFullRowSelect];
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  var LTree := VirtualStringTree1;

  LTree.NodeDataSize := SizeOf(TRowData);

  var LNode1 := LTree.AddChild(LTree.RootNode);
  var LNode2 := LTree.AddChild(LTree.RootNode);
  var LNode3 := LTree.AddChild(LTree.RootNode);
  var LNode4 := LTree.AddChild(LTree.RootNode);
  var LNode5 := LTree.AddChild(LTree.RootNode);
  var LNode6 := LTree.AddChild(LTree.RootNode);
  var LNode7 := LTree.AddChild(LTree.RootNode);
  var LNode8 := LTree.AddChild(LTree.RootNode);

  LNode1.SetData<TRowData>(TRowData.Create('pg_catalog', 'pg_user_info',  'VIEW'));
  LNode2.SetData<TRowData>(TRowData.Create('pg_catalog', 'pg_stastic',    'BASE TABLE'));
  LNode3.SetData<TRowData>(TRowData.Create('pg_catalog', 'pg_settings',   'VIEW1'));
  LNode4.SetData<TRowData>(TRowData.Create('pg_catalog', 'pg_type',       'VIEW2'));
  LNode5.SetData<TRowData>(TRowData.Create('pg_catalog', 'pg_attribute',  'BASE TABLE'));
  LNode6.SetData<TRowData>(TRowData.Create('pg_catalog', 'pg_class',      'BASE TABLE'));
  LNode7.SetData<TRowData>(TRowData.Create('pg_catalog', 'pg_tablespace', 'BASE TABLE'));
  LNode8.SetData<TRowData>(TRowData.Create('pg_catalog', 'pg_inherits',   'BASE TABLE'));

  LTree.ClipboardFormats.Add(GetVTClipboardFormatDescription(CF_TEXT));
  LTree.ClipboardFormats.Add(GetVTClipboardFormatDescription(CF_UNICODETEXT));
  LTree.ClipboardFormats.Add(GetVTClipboardFormatDescription(CF_VRTF));
  LTree.ClipboardFormats.Add(GetVTClipboardFormatDescription(CF_HTML));
end;

procedure TForm1.VirtualStringTree1FreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  var LData := Node.GetData<TRowData>;
  LData.Clear;
end;

procedure TForm1.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  if not Assigned(Node) then
    Exit;
  var LData := Node.GetData<TRowData>;
  case Column of
    colSchema: begin
      CellText := LData.table_schema;
    end;
    colName: begin
      CellText := LData.table_name;
    end;
    colType: begin
      CellText := LData.table_type;
    end;
    // column 4 is deliberately left empty
  end;
end;

procedure TForm1.MouseClick(const ACursorPos: TPoint);
const
  KEYDOWN = Byte(1 shl 7);
var
  LKeyboardState: TKeyboardState;
begin
  // Click a new cell on the tree...
  var LTree := VirtualStringTree1;
  var LCursorPos := Mouse.CursorPos;
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
    Mouse.CursorPos := LCursorPos;
  end;
end;

procedure TForm1.MouseClick(ANode: PVirtualNode; AColumn: TColumnIndex);
begin
  var LTree := VirtualStringTree1;
  var LClientRect := LTree.GetDisplayRect(ANode, AColumn, True);
  MouseClick(LClientRect.TopLeft);
end;

procedure TForm1.ShiftMouseClick(const ACell: TVTCell);
begin
  ShiftMouseClick(ACell.Node, ACell.Column);
end;

procedure TForm1.ShiftMouseClick(ANode: PVirtualNode; AColumn: TColumnIndex = 0);
const
  KEYDOWN = Byte(1 shl 7);
var
  LOrigKBState, LNewKBState: TKeyboardState;
begin
  GetKeyboardState(LOrigKBState);
  LNewKBState := LOrigKBState;
  LNewKBState[VK_SHIFT] := LOrigKBState[VK_SHIFT] or KEYDOWN;
  SetKeyboardState(LNewKBState);
  try
    MouseClick(ANode, AColumn);
  finally
    SetKeyboardState(LOrigKBState);
  end;
end;

end.
