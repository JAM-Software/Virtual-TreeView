unit VTToggleNodeIssue1415Tests;

// Regression tests for issue #1415 "Expanding a node with no children costs a scroll bar
// update inside BeginUpdate (major performance issue)".
//
// ToggleNode() set NeedUpdate before it knew whether the node changes at all. Expanding a
// node that has no children (and no vsHasChildren) therefore invalidated the position cache
// and repainted the node, and inside BeginUpdate it updated the scroll bars. FullExpand does
// that for every leaf, which took seconds for a large tree with a VCL style active.
//
// The fix sets NeedUpdate only when the node is changed and leaves the scroll bars to
// EndUpdate while the tree is updating. The tests check both, and that the cases which do
// change the node are still updated.

interface

uses
  DUnitX.TestFramework,
  Winapi.Messages,
  Vcl.Forms,
  VirtualTrees,
  VirtualTrees.StyleHooks;

type
  /// Counts the scroll bar updates: TBaseVirtualTree.UpdateScrollBars() always ends with
  /// Perform(CM_UPDATE_VCLSTYLE_SCROLLBARS), with or without a VCL style.
  TScrollBarCountingTree = class(TVirtualStringTree)
  strict private
    fScrollBarUpdates: Integer;
    procedure CMUpdateVclStyleScrollbars(var Message: TMessage); message CM_UPDATE_VCLSTYLE_SCROLLBARS;
  public
    property ScrollBarUpdates: Integer read fScrollBarUpdates write fScrollBarUpdates;
  end;

  [TestFixture]
  TVTToggleNodeIssue1415Tests = class
  strict private
    fForm: TForm;
    fTree: TScrollBarCountingTree;
    fExpandingCount: Integer;
    fChildCountOnExpanding: Cardinal;
    procedure TreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
    procedure TreeInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    function HasInvalidRegion: Boolean;
    function VertBarVisible: Boolean;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// Expanding a node without children changes nothing, so nothing must be repainted.
    [Test]
    procedure ExpandingLeafDoesNotRepaint;

    /// The reported scenario: FullExpand over many leaves updates the scroll bars once, in
    /// EndUpdate, and not for every leaf.
    [Test]
    procedure FullExpandUpdatesScrollBarsOnlyOnce;

    /// Expanding a node with children inside BeginUpdate keeps the range current but leaves
    /// the scroll bars to EndUpdate.
    [Test]
    procedure ExpandInsideUpdateLeavesScrollBarsToEndUpdate;

    /// A node with vsHasChildren whose OnInitChildren reports no children loses its button,
    /// so it still has to be repainted.
    [Test]
    procedure NodeLosingItsButtonIsRepainted;

    /// OnExpanding still fires for a node without children, and children added there are
    /// expanded.
    [Test]
    procedure ChildrenAddedInOnExpandingAreExpanded;
  end;

implementation

uses
  Winapi.Windows,
  System.Types,
  System.SysUtils,
  Vcl.Controls,
  VirtualTrees.Types;

{ TScrollBarCountingTree }

procedure TScrollBarCountingTree.CMUpdateVclStyleScrollbars(var Message: TMessage);
begin
  Inc(fScrollBarUpdates);
  inherited;
end;

{ TVTToggleNodeIssue1415Tests }

procedure TVTToggleNodeIssue1415Tests.Setup;
begin
  fExpandingCount := 0;
  fChildCountOnExpanding := 0;
  fForm := TForm.Create(nil);
  fForm.SetBounds(0, 0, 320, 200);
  fTree := TScrollBarCountingTree.Create(fForm);
  fTree.Parent := fForm;
  fTree.Align := alClient;
  fTree.DefaultNodeHeight := 18;
  fTree.OnExpanding := TreeExpanding;
  fTree.OnInitChildren := TreeInitChildren;
  // Five leaves fit into the client area, so there is no vertical scroll bar yet.
  fTree.RootNodeCount := 5;
  fForm.Show;
  Application.ProcessMessages;
end;

procedure TVTToggleNodeIssue1415Tests.TearDown;
begin
  FreeAndNil(fForm);
end;

procedure TVTToggleNodeIssue1415Tests.TreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var Allowed: Boolean);
begin
  Inc(fExpandingCount);
  if fChildCountOnExpanding > 0 then
    Sender.ChildCount[Node] := fChildCountOnExpanding;
end;

procedure TVTToggleNodeIssue1415Tests.TreeInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var ChildCount: Cardinal);
begin
  ChildCount := 0; // A node marked with HasChildren turns out to have none.
end;

function TVTToggleNodeIssue1415Tests.HasInvalidRegion: Boolean;
var
  R: TRect;
begin
  Result := GetUpdateRect(fTree.Handle, R, False);
end;

function TVTToggleNodeIssue1415Tests.VertBarVisible: Boolean;
begin
  Result := (GetWindowLong(fTree.Handle, GWL_STYLE) and WS_VSCROLL) <> 0;
end;

procedure TVTToggleNodeIssue1415Tests.ExpandingLeafDoesNotRepaint;
var
  Node: PVirtualNode;
begin
  Node := fTree.GetFirst;
  Assert.IsFalse(fTree.HasChildren[Node], 'Sanity: the node must not have children.');
  ValidateRect(fTree.Handle, nil);

  fTree.Expanded[Node] := True;

  Assert.AreEqual(1, fExpandingCount, 'OnExpanding must still be fired for a node without children.');
  Assert.IsFalse(fTree.Expanded[Node], 'A node without children cannot be expanded.');
  Assert.IsFalse(HasInvalidRegion,
    'Expanding a node without children changes nothing and must not repaint it (issue #1415).');
end;

procedure TVTToggleNodeIssue1415Tests.FullExpandUpdatesScrollBarsOnlyOnce;
const
  cLeafCount = 200;
begin
  fTree.RootNodeCount := cLeafCount;
  Application.ProcessMessages;
  fTree.ScrollBarUpdates := 0;

  fTree.FullExpand;

  Assert.AreEqual(cLeafCount, fExpandingCount, 'Sanity: FullExpand must try to expand every leaf.');
  Assert.AreEqual(1, fTree.ScrollBarUpdates,
    'FullExpand must update the scroll bars once in EndUpdate, not for every leaf (issue #1415).');
end;

procedure TVTToggleNodeIssue1415Tests.ExpandInsideUpdateLeavesScrollBarsToEndUpdate;
var
  Node: PVirtualNode;
begin
  Node := fTree.GetFirst;
  fTree.ChildCount[Node] := 50;
  Application.ProcessMessages;
  Assert.IsFalse(VertBarVisible, 'Sanity: with the node collapsed all nodes fit into the client area.');
  fTree.ScrollBarUpdates := 0;

  fTree.BeginUpdate;
  try
    fTree.Expanded[Node] := True;
    Assert.IsTrue(fTree.Expanded[Node], 'Sanity: the node must be expanded.');
    Assert.AreEqual(0, fTree.ScrollBarUpdates,
      'Inside BeginUpdate expanding must leave the scroll bars to EndUpdate (issue #1415).');
    Assert.IsTrue(fTree.RangeY > fTree.ClientHeight,
      'Inside BeginUpdate the vertical range must still be kept current.');
  finally
    fTree.EndUpdate;
  end;

  Assert.IsTrue(VertBarVisible, 'EndUpdate must show the vertical scroll bar for the expanded children.');
end;

procedure TVTToggleNodeIssue1415Tests.NodeLosingItsButtonIsRepainted;
var
  Node: PVirtualNode;
begin
  Node := fTree.GetFirst;
  fTree.HasChildren[Node] := True;
  Application.ProcessMessages;
  ValidateRect(fTree.Handle, nil);

  fTree.Expanded[Node] := True; // OnInitChildren reports no children.

  Assert.IsFalse(fTree.HasChildren[Node], 'Sanity: without children the node must lose vsHasChildren.');
  Assert.IsFalse(fTree.Expanded[Node], 'Sanity: a node without children cannot be expanded.');
  Assert.IsTrue(HasInvalidRegion,
    'The node lost its expand button and must be repainted (issue #1415).');
end;

procedure TVTToggleNodeIssue1415Tests.ChildrenAddedInOnExpandingAreExpanded;
var
  Node: PVirtualNode;
begin
  Node := fTree.GetFirst;
  fChildCountOnExpanding := 3;

  fTree.Expanded[Node] := True;

  Assert.AreEqual(1, fExpandingCount, 'Sanity: OnExpanding must have been fired once.');
  Assert.AreEqual(3, Integer(fTree.ChildCount[Node]), 'Sanity: OnExpanding must have added the children.');
  Assert.IsTrue(fTree.Expanded[Node], 'Children added in OnExpanding must be expanded (issue #1415).');
end;

initialization
  TDUnitX.RegisterTestFixture(TVTToggleNodeIssue1415Tests);

end.
