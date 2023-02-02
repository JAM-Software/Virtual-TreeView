unit MultilineDemo;

// Virtual Treeview sample form demonstrating following features:
//   - Multiline node captions.
// Written by Mike Lischke.

interface

uses
  Windows, SysUtils, Classes, Forms, Controls, Graphics, VirtualTrees,
  ExtCtrls, StdCtrls, ImgList, VirtualTrees.Types;
  
type
  TNodeForm = class(TForm)
    Panel1: TPanel;
    MLTree: TVirtualStringTree;
    Label8: TLabel;
    AutoAdjustCheckBox: TCheckBox;
    procedure MLTreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure FormCreate(Sender: TObject);
    procedure MLTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure MLTreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure MLTreeEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure MLTreeStateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);
    procedure MLTreeMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      var NodeHeight: Integer);
    procedure AutoAdjustCheckBoxClick(Sender: TObject);
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Main, States;

{$R *.dfm}

var
  DemoText: array[0..29] of UnicodeString;

//----------------------------------------------------------------------------------------------------------------------

procedure TNodeForm.MLTreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

begin
  Node.Align := 20; // Alignment of expand/collapse button nearly at the top of the node.
  if (Node.Index mod 3) = 0 then
  begin
    MLTree.NodeHeight[Node] := 40;
  end
  else
  begin
    MLTree.NodeHeight[Node] := 120;
    Include(InitialStates, ivsMultiline);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TNodeForm.FormCreate(Sender: TObject);

begin
  LoadUnicodeStrings('LoremIpsum', DemoText);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TNodeForm.MLTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);

// Returns the text for the given node. This text was loaded at form creation time from the application resource.

begin
  // The display is organized so that for every two main text nodes a node with empty main text is used. This
  // node however gets text in its second column (with column spanning enabled).
  // This creates the visual effect of a chapter heading and two indented paragraphs.
  // Another possibility to get this effect is to use child nodes and only one column.
  // Note: the main column is in the second position!
  case Column of
    0: // Main column.
      if (Node.Index mod 3) = 0 then
        CellText := ''
      else
        CellText := DemoText[Node.Index mod 30];
    1:
      if (Node.Index mod 3) = 0 then
        CellText := IntToStr(Node.Index div 3 + 1) + '. ' + DemoText[Node.Index mod 30] // Add a sequence numer before the heading
      else
        CellText := '';
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TNodeForm.MLTreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

begin
  if (Node.Index mod 3) = 0 then
  begin
    TargetCanvas.Font.Size := 12;
    TargetCanvas.Font.Style := [fsBold];
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TNodeForm.MLTreeEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);

begin
  Allowed := (Node.Index mod 3) <> 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TNodeForm.MLTreeStateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);

begin
  if not (csDestroying in ComponentState) then
    UpdateStateDisplay(Sender.TreeStates, Enter, Leave);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TNodeForm.MLTreeMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  var NodeHeight: Integer);

begin
  if Sender.MultiLine[Node] then
  begin
    TargetCanvas.Font := Sender.Font;
    NodeHeight := MLTree.ComputeNodeHeight(TargetCanvas, Node, 0) + 10;
  end;
  // ...else use what's set by default.
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TNodeForm.AutoAdjustCheckBoxClick(Sender: TObject);

begin
  if AutoAdjustCheckBox.Checked then
    MLTree.TreeOptions.MiscOptions := MLTree.TreeOptions.MiscOptions + [TVTMiscOption.toVariablenodeHeight]
  else
    MLTree.TreeOptions.MiscOptions := MLTree.TreeOptions.MiscOptions - [TVTMiscOption.toVariablenodeHeight];
end;

//----------------------------------------------------------------------------------------------------------------------

end.
