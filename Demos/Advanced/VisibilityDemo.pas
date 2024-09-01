unit VisibilityDemo;

// Virtual Treeview sample form demonstrating following features:
//   - Hiding nodes.
//   - Synchronization between 2 trees (expand, scroll, selection).
//   - Wheel scrolling and panning.
// Written by Mike Lischke.
{$WARN UNSAFE_CODE OFF} // Prevent warnins that are not applicable 

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VirtualTrees, ComCtrls, ExtCtrls, ImgList, VirtualTrees.Types;

type
  TVisibilityForm = class(TForm)
    Label17: TLabel;
    RadioGroup1: TRadioGroup;
    VST2: TVirtualStringTree;
    VST1: TVirtualStringTree;
    VST3: TVirtualStringTree;
    Splitter2: TSplitter;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure VST1InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure VST1InitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure VST2GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure VST3Scroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
    procedure VST2InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure VST2Scroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
    procedure VSTCollapsedExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VST2Change(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure Splitter2CanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure Splitter2Paint(Sender: TObject);
    procedure VST1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure VST3FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VST2FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VST1FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    FChanging: Boolean;
    procedure HideNodes(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
  end;

var
  VisibilityForm: TVisibilityForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses States;

{$R *.DFM}

type
  PLinkData = ^TLinkData;
  TLinkData = record
    Caption: UnicodeString;
    OtherNode: PVirtualNode;
  end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVisibilityForm.VST1InitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  Level: Integer;

begin
  Level := Sender.GetNodeLevel(Node);
  if Level < 4 then
    Include(InitialStates, ivsHasChildren);
  if Level > 0 then
    Node.CheckType := TCheckType(Level)
  else
    Node.CheckType := ctTriStateCheckBox;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVisibilityForm.VST1InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);

begin
  ChildCount := Random(5) + 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVisibilityForm.FormCreate(Sender: TObject);

var
  Run1, Run2: PVirtualNode;
  Data1, Data2: PLinkData;

begin
  Randomize;
  VST1.RootNodeCount := 5;

  // The base idea behind linking two (or more) trees together is that one has access to the nodes of the others.
  // This can be reached in several ways. I use here the simplest approach by validating both trees fully and creating
  // cross references for all nodes. Another one would be to create a common data base and link all trees to this.
  VST2.NodeDataSize := SizeOf(TLinkData);
  VST2.RootNodeCount := 5;
  VST3.NodeDataSize := SizeOf(TLinkData);
  VST3.RootNodeCount := 5;

  VST3.BackgroundOffsetX := Splitter2.Left + Splitter2.Width;

  // Create cross references. This will validate all nodes.
  Run1 := VST2.GetFirst;
  Run2 := VST3.GetFirst;
  while Assigned(Run1) do
  begin
    Data1 := VST2.GetNodeData(Run1);
    Data1.OtherNode := Run2;
    Data2 := VST3.GetNodeData(Run2);
    Data2.OtherNode := Run1;
    Run1 := VST2.GetNext(Run1);
    Run2 := VST3.GetNext(Run2);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVisibilityForm.HideNodes(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);

begin
  case Integer(Data) of
    0: // show all nodes
      Sender.IsVisible[Node] := True;
    1: // hide every second
      Sender.IsVisible[Node] := not Odd(Node.Index);
    2: // hide nodes with child nodes only
      Sender.IsVisible[Node] := not Sender.HasChildren[Node];
    3: // hide all
      Sender.IsVisible[Node] := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVisibilityForm.RadioGroup1Click(Sender: TObject);

begin
  with Sender as TRadioGroup do
  begin
    VST1.BeginUpdate;
    try
      VST1.IterateSubtree(nil, HideNodes, Pointer(ItemIndex), [], True);
    finally
      VST1.EndUpdate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVisibilityForm.VST2GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);

var
  Data: PLinkData;

begin
  Data := Sender.GetNodeData(Node);
  if Length(Data.Caption) = 0 then
    Data.Caption := 'Node ' + IntToStr(Sender.AbsoluteIndex(Node));

  CellText := Data.Caption;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVisibilityForm.VST3Scroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);

// Synchronizes scroll offsets of VST2 and VST3.

begin
  if not FChanging then
  begin
    FChanging := True;
    try
      VST3.Update;
      VST2.OffsetY := VST3.OffsetY;
      VST2.Update;
    finally
      FChanging := False;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVisibilityForm.VST2Scroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);

// Synchronizes scroll offsets of VST2 and VST3.

begin
  if not FChanging then
  begin
    FChanging := True;
    try
      VST2.Update;
      VST3.OffsetY := VST2.OffsetY;
      VST3.Update;
    finally
      FChanging := False;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVisibilityForm.VST2InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);

begin
  ChildCount := Sender.GetNodeLevel(Node) + 2;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVisibilityForm.VSTCollapsedExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);

// Collapse/Expand state synchronization.

var
  OtherTree: TBaseVirtualTree;
  Data: PLinkData;
  
begin
  // Avoid recursive calls.
  if not FChanging then
  begin
    FChanging := True;
    try
      if Sender = VST2 then
        OtherTree := VST3
      else
        OtherTree := VST2;

      Data := Sender.GetNodeData(Node);
      OtherTree.ToggleNode(Data.OtherNode);
    finally
      FChanging := False;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVisibilityForm.VST2Change(Sender: TBaseVirtualTree; Node: PVirtualNode);

// Keep selected nodes in sync.

var
  Data: PLinkData;
  OtherTree: TBaseVirtualTree;

begin
  if not FChanging and Assigned(Node) then
  begin
    FChanging := True;
    try
      Data := Sender.GetNodeData(Node);
      if Sender = VST2 then
        OtherTree := VST3
      else
        OtherTree := VST2;

      OtherTree.Selected[Data.OtherNode] := True;
      OtherTree.FocusedNode := Data.OtherNode;
    finally
      FChanging := False;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVisibilityForm.Splitter2CanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);

// This method is called just before resizing is done. This is a good opportunity to adjust the background image
// offset.

begin
  VST3.BackgroundOffsetX := NewSize + Splitter2.Width;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVisibilityForm.Splitter2Paint(Sender: TObject);

begin
  with Splitter2, Canvas do
  begin
    Brush.Color := clBtnFace;
    FillRect(Rect(0, 0, Width, VST2.Header.Height));
    Brush.Color := clWindow;
    FillRect(Rect(0, VST2.Header.Height, Width, Height));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVisibilityForm.VST1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: UnicodeString);

begin
  CellText := Format('Node Level %d, Index %d', [Sender.GetNodeLevel(Node), Node.Index]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVisibilityForm.FormShow(Sender: TObject);

begin
  StateForm.Hide;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVisibilityForm.FormHide(Sender: TObject);

begin
  StateForm.Show;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVisibilityForm.VST1FreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PLinkData;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVisibilityForm.VST2FreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PLinkData;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVisibilityForm.VST3FreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PLinkData;

begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;


end.
