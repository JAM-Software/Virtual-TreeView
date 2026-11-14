unit VTScaleNodeHeightsIssue1408Tests;

// Regression tests for issue #1408 "ScaleNodeHeights corrupts FRoot.TotalHeight
// for uninitialized children of collapsed nodes".
//
// ScaleNodeHeights' branch for uninitialized nodes (added for issue #1000) used
// to add every node's height difference directly to FRoot.TotalHeight. Since
// GetFirstNoInit/GetNextNoInit also visit the children of collapsed nodes -
// which do not contribute to the visible height - the vertical scroll range
// became too small after scaling down (the last rows could not be scrolled
// into view) and too large after scaling up.
//
// The fix scales every node through SetNodeHeight(), which propagates only the
// node's own height change via AdjustTotalHeight(). That stops at collapsed or
// invisible parents and does not initialize nodes.

interface

uses
  DUnitX.TestFramework,
  Classes,
  System.Types,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Graphics,
  VirtualTrees,
  VirtualTrees.Types;

type
  [TestFixture]
  TVTScaleNodeHeightsIssue1408Tests = class
  strict private
    const RootCount = 5;
    const ChildrenPerRoot = 50;
  var
    fForm: TForm;
    fTree: TVirtualStringTree;
    procedure OnInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    function SumVisibleNodeHeights: Integer;
    function VerticalScrollRange: Integer;
    function CountUninitializedNodes: Integer;
    procedure ScrollToBottom;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// After scaling, the vertical scroll range must equal the sum of the
    /// visible nodes' heights, although collapsed nodes have uninitialized children.
    [Test]
    procedure ScaleKeepsScrollRangeConsistent;

    /// Guards the contract of issue #1000: scaling must not initialize nodes.
    [Test]
    procedure ScaleDoesNotInitializeNodes;

    /// The user-visible symptom: after scaling down and expanding the last
    /// collapsed node, the last row must still be reachable by scrolling.
    [Test]
    procedure LastNodeReachableAfterScaleAndExpand;
  end;

implementation

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils;

type
  TTreeCracker = class(TBaseVirtualTree); // for reading the protected RangeY

procedure TVTScaleNodeHeightsIssue1408Tests.Setup;
begin
  fForm := TForm.Create(nil);
  fForm.SetBounds(0, 0, 320, 200);
  fTree := TVirtualStringTree.Create(fForm);
  fTree.BorderStyle := bsNone;
  fTree.DefaultNodeHeight := 18;
  // toAutoChangeScale would call ScaleNodeHeights itself on handle creation;
  // disable it so the tests control the only scaling that happens.
  fTree.TreeOptions.AutoOptions := fTree.TreeOptions.AutoOptions - [toAutoChangeScale];
  fTree.OnInitNode := OnInitNode;
  // The scenario of issue #1408: every root creates its children in OnInitNode
  // and stays collapsed, so the children exist but remain uninitialized.
  fTree.RootNodeCount := RootCount;
  fTree.Parent := fForm;
  fTree.Align := alClient;
  fForm.Show;
  Application.ProcessMessages; // first paint initializes the visible root nodes
  Assert.AreEqual(RootCount * ChildrenPerRoot, CountUninitializedNodes,
    'Sanity: all children of the collapsed roots must exist uninitialized.');
end;

procedure TVTScaleNodeHeightsIssue1408Tests.TearDown;
begin
  FreeAndNil(fForm);
end;

procedure TVTScaleNodeHeightsIssue1408Tests.OnInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if ParentNode = nil then
    Sender.ChildCount[Node] := ChildrenPerRoot;
end;

function TVTScaleNodeHeightsIssue1408Tests.SumVisibleNodeHeights: Integer;
var
  Node: PVirtualNode;
begin
  Result := 0;
  Node := fTree.GetFirstVisibleNoInit;
  while Assigned(Node) do
  begin
    Inc(Result, Node.NodeHeight);
    Node := fTree.GetNextVisibleNoInit(Node);
  end;
end;

function TVTScaleNodeHeightsIssue1408Tests.VerticalScrollRange: Integer;
begin
  // The scroll bar itself is hidden while the range fits the client area, so
  // read the tree's own range instead of GetScrollInfo().
  Result := TTreeCracker(fTree).RangeY;
end;

function TVTScaleNodeHeightsIssue1408Tests.CountUninitializedNodes: Integer;
var
  Node: PVirtualNode;
begin
  Result := 0;
  Node := fTree.GetFirstNoInit;
  while Assigned(Node) do
  begin
    if not (vsInitialized in Node.States) then
      Inc(Result);
    Node := fTree.GetNextNoInit(Node);
  end;
end;

procedure TVTScaleNodeHeightsIssue1408Tests.ScrollToBottom;
var
  I: Integer;
  LastOffsetY: TDimension;
begin
  LastOffsetY := 1; // never a valid offset, forces at least two iterations
  for I := 1 to 200 do
  begin
    fTree.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
    Application.ProcessMessages;
    if fTree.OffsetY = LastOffsetY then
      Break;
    LastOffsetY := fTree.OffsetY;
  end;
end;

procedure TVTScaleNodeHeightsIssue1408Tests.ScaleKeepsScrollRangeConsistent;
begin
  fTree.ScaleNodeHeights(2, 3); // e.g. moving to a monitor with a lower DPI
  Application.ProcessMessages;
  Assert.AreEqual(SumVisibleNodeHeights, VerticalScrollRange,
    'After scaling down, the vertical scroll range must equal the sum of the visible node heights (issue #1408).');

  fTree.ScaleNodeHeights(3, 2); // and back up again
  Application.ProcessMessages;
  Assert.AreEqual(SumVisibleNodeHeights, VerticalScrollRange,
    'After scaling back up, the vertical scroll range must equal the sum of the visible node heights (issue #1408).');
end;

procedure TVTScaleNodeHeightsIssue1408Tests.ScaleDoesNotInitializeNodes;
begin
  fTree.ScaleNodeHeights(2, 3);
  Assert.AreEqual(RootCount * ChildrenPerRoot, CountUninitializedNodes,
    'ScaleNodeHeights must not initialize nodes (issue #1000).');
end;

procedure TVTScaleNodeHeightsIssue1408Tests.LastNodeReachableAfterScaleAndExpand;
var
  R: TRect;
begin
  fTree.ScaleNodeHeights(2, 3);
  fTree.Expanded[fTree.GetLastChild(nil)] := True;
  Application.ProcessMessages;
  ScrollToBottom;
  R := fTree.GetDisplayRect(fTree.GetLast, NoColumn, False);
  Assert.IsTrue((R.Bottom > 0) and (R.Bottom <= fTree.ClientHeight),
    Format('After scaling down and expanding the last node, the last row (%d..%d) must be reachable inside the client area (height %d) - issue #1408.',
    [R.Top, R.Bottom, fTree.ClientHeight]));
end;

initialization
  TDUnitX.RegisterTestFixture(TVTScaleNodeHeightsIssue1408Tests);

end.
