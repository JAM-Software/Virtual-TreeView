unit VTScrollRangeIssue983Tests;

// Regression test for issue #983 "Vertical Scroll Bar Cannot Scroll To Bottom In
// Select Circumstances".
//
// Without columns the horizontal scroll range is the width of the currently
// visible nodes. When the only over-wide node scrolls out of view, the range
// shrinks, the horizontal scroll bar disappears, the taller client area clamps
// the vertical offset back up - which scrolls the wide node into view again and
// brings the bar back. The tree oscillates between both states and the user can
// never reach the bottom.
//
// The fix makes the horizontal range grow-only while the update is caused by
// scrolling; every other trigger (resize, structure change) recomputes it from
// scratch as before. The tests assert both halves of that contract.

interface

uses
  DUnitX.TestFramework,
  Classes,
  System.Types,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Graphics,
  VirtualTrees;

type
  [TestFixture]
  TVTScrollRangeIssue983Tests = class
  strict private
    fForm: TForm;
    fTree: TVirtualStringTree;
    procedure OnGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    function HorzBarVisible: Boolean;
    procedure ScrollToBottom;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// The core symptom: line-scrolling down must reach the last node.
    [Test]
    procedure ScrollToBottomReachesLastNode;

    /// Releasing the scroll bar (SB_ENDSCROLL) must not pull the position back up.
    [Test]
    procedure EndScrollKeepsPosition;

    /// The other half of the contract: enlarging the window must still recompute
    /// the range from scratch and drop the horizontal scroll bar.
    [Test]
    procedure WideningTheWindowDropsHorizontalScrollBar;
  end;

implementation

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  VirtualTrees.Types;

procedure TVTScrollRangeIssue983Tests.Setup;
begin
  fForm := TForm.Create(nil);
  fForm.SetBounds(0, 0, 320, 160);
  fTree := TVirtualStringTree.Create(fForm);
  fTree.Parent := fForm;
  fTree.Align := alClient;
  fTree.BorderStyle := bsNone;
  fTree.DefaultNodeHeight := 18;
  fTree.OnGetText := OnGetText;
  // Node 0 is the only node wider than the client area; six short nodes below make
  // the tree just tall enough that node 0 can scroll completely out of view.
  fTree.RootNodeCount := 7;
  fForm.Show;
  Application.ProcessMessages;
end;

procedure TVTScrollRangeIssue983Tests.TearDown;
begin
  FreeAndNil(fForm);
end;

procedure TVTScrollRangeIssue983Tests.OnGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
begin
  if Node.Index = 0 then
    CellText := 'A long node name to fill the entire width of the tree window and then some'
  else
    CellText := Format('n%d', [Node.Index]);
end;

function TVTScrollRangeIssue983Tests.HorzBarVisible: Boolean;
begin
  Result := (GetWindowLong(fTree.Handle, GWL_STYLE) and WS_HSCROLL) <> 0;
end;

procedure TVTScrollRangeIssue983Tests.ScrollToBottom;
var
  I: Integer;
  LastOffsetY: TDimension;
begin
  LastOffsetY := 1; // never a valid offset, forces at least two iterations
  for I := 1 to 20 do
  begin
    fTree.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
    Application.ProcessMessages;
    if fTree.OffsetY = LastOffsetY then
      Break;
    LastOffsetY := fTree.OffsetY;
  end;
end;

procedure TVTScrollRangeIssue983Tests.ScrollToBottomReachesLastNode;
var
  R: TRect;
begin
  Assert.IsTrue(HorzBarVisible, 'Sanity: the over-wide node 0 must produce a horizontal scroll bar.');
  ScrollToBottom;
  R := fTree.GetDisplayRect(fTree.GetLast, NoColumn, False);
  Assert.IsTrue(R.Bottom <= fTree.ClientHeight,
    Format('After scrolling down the last node (%d..%d) must be fully inside the client area (height %d) - issue #983.',
    [R.Top, R.Bottom, fTree.ClientHeight]));
end;

procedure TVTScrollRangeIssue983Tests.EndScrollKeepsPosition;
var
  OffsetAtBottom: TDimension;
begin
  ScrollToBottom;
  OffsetAtBottom := fTree.OffsetY;
  fTree.Perform(WM_VSCROLL, SB_ENDSCROLL, 0);
  Application.ProcessMessages;
  Assert.AreEqual(Integer(OffsetAtBottom), Integer(fTree.OffsetY),
    'Releasing the scroll bar must not pull the scroll position back up (issue #983).');
end;

procedure TVTScrollRangeIssue983Tests.WideningTheWindowDropsHorizontalScrollBar;
begin
  Assert.IsTrue(HorzBarVisible, 'Sanity: the over-wide node 0 must produce a horizontal scroll bar.');
  fForm.Width := fForm.Width + 400;
  Application.ProcessMessages;
  Assert.IsFalse(HorzBarVisible,
    'After enlarging the window no node is over-wide anymore, the horizontal scroll bar must disappear.');
end;

initialization
  TDUnitX.RegisterTestFixture(TVTScrollRangeIssue983Tests);

end.
