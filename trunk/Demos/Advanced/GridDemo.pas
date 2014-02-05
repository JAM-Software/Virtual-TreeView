unit GridDemo;

// Virtual Treeview sample form demonstrating following features:
//   - TVirtualStringTree with enabled grid extensions and a fixed column.
//   - Owner draw column to simulate a fixed column.
//   - Extend focus, multiselection without selection rectangle.
//   - Various editors, specific to each column.
// Written by Mike Lischke.

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VirtualTrees, ImgList;

type
  TGridForm = class(TForm)
    VST5: TVirtualStringTree;
    GridLineCheckBox: TCheckBox;
    Label15: TLabel;
    TreeImages: TImageList;
    Label1: TLabel;
    procedure VST5BeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VST5BeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
      var Color: TColor; var EraseAction: TItemEraseAction);
    procedure VST5CreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VST5FocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn,
      NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure VST5GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: UnicodeString);
    procedure VST5InitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure VST5PaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure GridLineCheckBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure VST5AfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure VST5StateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);
    procedure VST5FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  end;

var
  GridForm: TGridForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Editors, States;

{$R *.DFM}

//----------------------------------------------------------------------------------------------------------------------

procedure TGridForm.FormCreate(Sender: TObject);

begin
  // We assign the OnGetText handler manually to keep the demo source code compatible
  // with older Delphi versions after using UnicodeString instead of WideString.
  VST5.OnGetText := VST5GetText;

  VST5.NodeDataSize := SizeOf(TGridData);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGridForm.VST5BeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  ItemRect: TRect; var Color: TColor; var EraseAction: TItemEraseAction);

// Some nodes get a different background color.

begin
  with Canvas do
  begin
    if Node.Index mod 6 = 0 then
      Color := $49DDEF // $70A33F // $436BFF
    else
      Color := VST5.Color;
    EraseAction := eaColor;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGridForm.VST5FocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn,
  NewColumn: TColumnIndex; var Allowed: Boolean);

begin
  // Do not allow focusing the indicator column (which is a fixed column).
  Allowed := NewColumn > 0;
end;

procedure TGridForm.VST5FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PGridData;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data.Value[1]);
  Finalize(Data.Value[2]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGridForm.VST5InitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  Data: PGridData;
  D: TDateTime;

begin
  Data := Sender.GetNodeData(Node);

  // These are the editor kinds used in the grid tree.
  Data.ValueType[0] := vtNumber;
  Data.ValueType[1] := vtPickString;
  Data.ValueType[2] := vtPickString;
  Data.ValueType[3] := vtDate;

  // fill some default values
  Data.Value[0] := Variant(Node.Index);
  Data.Value[1] := 'John';
  Data.Value[2] := 'Doe';
  // A date value slightly randomized around today. Need the way
  // using a local variable to tell the compiler we are not
  // using a float as variant, but a TDateTime.
  D := Date + Random(14) - 7;
  Data.Value[3] := D;

  if Sender.FocusedColumn < 1 then
    Sender.FocusedColumn := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGridForm.VST5GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: UnicodeString);

var
  Data: PGridData;

begin
  if Column > 0 then
  begin
    Data := Sender.GetNodeData(Node);
    CellText := Data.Value[Column - 1];
  end
  else
    CellText := '';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGridForm.VST5PaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);

var
  Data: PGridData;

begin
  Data := Sender.GetNodeData(Node);
  if Data.Changed then
    TargetCanvas.Font.Style := [fsBold];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGridForm.VST5BeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);

begin
  // Fill random cells with our own background, but don't touch the currently focused cell.
  if Assigned(Node) and ((Column <> Sender.FocusedColumn) or (Node <> Sender.FocusedNode)) and
    ((Column - 2) = (Integer(Node.Index) mod (VST5.Header.Columns.Count - 1))) then
  begin
    TargetCanvas.Brush.Color := $E0E0E0;
    TargetCanvas.FillRect(CellRect);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGridForm.VST5CreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  out EditLink: IVTEditLink);

// This is the callback of the tree control to ask for an application defined edit link. Providing one here allows
// us to control the editing process up to which actual control will be created.
// TGridEditLink implements an interface and hence benefits from reference counting. We don't need to keep a reference
// to free it. As soon as the tree finished editing the class will be destroyed automatically.

begin
  EditLink := TGridEditLink.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGridForm.GridLineCheckBoxClick(Sender: TObject);

begin
  if GridLineCheckBox.Checked then
    VST5.TreeOptions.PaintOptions := VST5.TreeOptions.PaintOptions + [toShowHorzGridLines, toShowVertGridLines]
  else
    VST5.TreeOptions.PaintOptions := VST5.TreeOptions.PaintOptions - [toShowHorzGridLines, toShowVertGridLines];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGridForm.VST5AfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);

begin
  if Column = 0 then
    with TargetCanvas do
    begin
      // Decorate the fixed indicator column by filling it with an edge similar to that of TCustomGrid.
      if toShowVertGridLines in VST5.TreeOptions.PaintOptions then
        Inc(CellRect.Right);
      if toShowHorzGridLines in VST5.TreeOptions.PaintOptions then
        Inc(CellRect.Bottom);
      DrawEdge(Handle, CellRect, BDR_RAISEDINNER, BF_RECT or BF_MIDDLE);
      if Node = Sender.FocusedNode then
        TreeImages.Draw(TargetCanvas, CellRect.Left + 4, CellRect.Top, 17);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGridForm.VST5StateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);

begin
  if not (csDestroying in ComponentState) then
    UpdateStateDisplay(Sender.TreeStates, Enter, Leave);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
