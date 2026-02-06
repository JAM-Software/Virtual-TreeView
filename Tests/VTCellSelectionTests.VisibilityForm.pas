unit VTCellSelectionTests.VisibilityForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees.BaseAncestorVCL,
  VirtualTrees.BaseTree, VirtualTrees.AncestorVCL, VirtualTrees, Vcl.StdCtrls;

type
  TVisibilityForm = class(TForm)
    VST1: TVirtualStringTree;
    btnCheck: TButton;
    procedure btnCheckClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure VST1Change(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VST1FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VST1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
        TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VST1InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var
        ChildCount: Cardinal);
    procedure VST1InitNode(Sender: TBaseVirtualTree; ParentNode, Node:
        PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private
  type
    TVTChangeEventProc = reference to procedure(Sender: TBaseVirtualTree; Node: PVirtualNode);
  var
    FChangeEventProc: TVTChangeEventProc;
    { Private declarations }
    procedure AssignChange(
      const AChangeEventProc: TVTChangeEventProc;
      const ATree: TBaseVirtualTree = nil);
    procedure DoChangeEvent(Sender: TBaseVirtualTree; Node: PVirtualNode);
  public
    { Public declarations }
    procedure HideNodes(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure TestEmptyAreaOnChange2;
  end;

var
  VisibilityForm: TVisibilityForm;

implementation

uses
  VirtualTrees.Types, VirtualTrees.MouseUtils;

{$R *.dfm}

type
  PLinkData = ^TLinkData;
  TLinkData = record
    Caption: string;
    OtherNode: PVirtualNode;
  end;

  Assert = class
    class procedure IsTrue(ACondition: Boolean; const AMsg: string = ''); static;
  end;

class procedure Assert.IsTrue(ACondition: Boolean; const AMsg: string = '');
begin
  System.Assert(ACondition, AMsg);
end;

procedure TVisibilityForm.AssignChange(
  const AChangeEventProc: TVTChangeEventProc;
  const ATree: TBaseVirtualTree = nil);
var
  FTree: TVirtualStringTree;
begin
  FTree := VST1;
  FChangeEventProc := AChangeEventProc;
  if not Assigned(ATree) then
    FTree.OnChange := DoChangeEvent else
    TVirtualStringTree(ATree).OnChange := DoChangeEvent;
end;

procedure TVisibilityForm.btnCheckClick(Sender: TObject);
begin
  TestEmptyAreaOnChange2;
end;

procedure TVisibilityForm.DoChangeEvent(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if Assigned(FChangeEventProc) then
    FChangeEventProc(Sender, Node);
end;

procedure TVisibilityForm.FormCreate(Sender: TObject);
begin
  VST1.RootNodeCount := 5;
end;

procedure TVisibilityForm.HideNodes(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
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

procedure TVisibilityForm.VST1FreeNode(Sender: TBaseVirtualTree; Node:
    PVirtualNode);
var
  Data: PLinkData;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TVisibilityForm.VST1GetText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText:
    string);
begin
  CellText := Format('Node Level %d, Index %d', [Sender.GetNodeLevel(Node), Node.Index]);
end;

procedure TVisibilityForm.VST1InitChildren(Sender: TBaseVirtualTree; Node:
    PVirtualNode; var ChildCount: Cardinal);
begin
  ChildCount := Random(5) + 1;
end;

procedure TVisibilityForm.VST1InitNode(Sender: TBaseVirtualTree; ParentNode,
    Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
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

procedure TVisibilityForm.TestEmptyAreaOnChange2;
var
  LTree: TVirtualStringTree;
  n1: PVirtualNode;
  LOnChangeFired: LongBool;
  I, LColumnCount, LMaxWidth: Integer;
  Rects: TArray<TRect>;
  LargestRect: TRect;
  LTextOnly, LUnclipped, LApplyCellContentMargin: Boolean;
  LastNode, LHitNode: PVirtualNode;
  LEmptyArea: TPoint;
  LHitInfo: THitInfo;
  LTestForm: TVisibilityForm;
begin
  LTestForm := Self;

    LTree := LTestForm.VST1;

    n1 := LTree.GetLastVisible;

    // Calculate the largest client area for the VirtualTree and set it
    LColumnCount := LTree.Header.Columns.Count;
    LMaxWidth := 0;
    for I := 0 to LColumnCount-1 do
      begin
        if LTree.Header.Columns[I].Width > LMaxWidth then
          LMaxWidth := LTree.Header.Columns[I].Width;
      end;
    if LMaxWidth = 0 then
      LMaxWidth := 300;
    I := 0;
    LargestRect := TRect.Empty;
    for LTextOnly := False to True do
      for LUnclipped := False to True do
        for LApplyCellContentMargin := False to True do
        begin
          SetLength(Rects, I+1);
          Rects[I] := LTree.GetDisplayRect(n1, LColumnCount-1, LTextOnly, LUnclipped, LApplyCellContentMargin);
          LargestRect := TRect.Union(LargestRect, Rects[I]);
          Inc(I);
        end;

    LastNode := LTree.GetLastVisibleChild(LTree.RootNode);

    LTree.ClientHeight := LargestRect.BottomRight.Y + (LastNode.NodeHeight * 2);
    LTree.ClientWidth :=  LargestRect.BottomRight.X + LMaxWidth;

    // This should be an empty area, beyond any visible nodes
    LEmptyArea := Point(LargestRect.BottomRight.X + LMaxWidth, LargestRect.BottomRight.Y + LastNode.NodeHeight);

    // At this point, there should be no nodes selected
    Assert.IsTrue(LTree.SelectedCount = 0);
    LTree.MouseClick(n1, NoColumn);
    // At this point, a node should be selected
    Assert.IsTrue(LTree.SelectedCount = 1);

    LOnChangeFired := False;
    LHitNode := Pointer($FFFFFFFF);
    AssignChange(procedure (Sender: TBaseVirtualTree; ANode: PVirtualNode)
    begin
      LOnChangeFired := True;
      LHitNode := ANode;
    end, LTree);

    LTree.GetHitTestInfoAt(LEmptyArea.X, LEmptyArea.Y, True, LHitInfo);
    LTree.MouseClick(LEmptyArea);

    Assert.IsTrue(hiNowhere in LHitInfo.HitPositions, 'Mouse click is not in an unpopulated/empty area!');
    Assert.IsTrue(LOnChangeFired, 'OnChange event not fired!');
    Assert.IsTrue(LHitNode = nil, 'Node is not nil!');
end;

procedure TVisibilityForm.VST1Change(Sender: TBaseVirtualTree; Node:
    PVirtualNode);
begin
  OutputDebugString(PChar(Format('Node: %p', [Pointer(Node)])));
end;

end.
