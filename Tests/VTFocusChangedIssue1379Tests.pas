unit VTFocusChangedIssue1379Tests;

// Regression test for issue #1379 "FocusChanged is called at wrong place when the
// user is using keyboard".
//
// Keyboard navigation sets the focused node twice: WMKeyDown moves the focus (which
// fires OnFocusChanged), then AddToSelection() assigns the - by now unchanged -
// focused node again. SetFocusedNode() always ran DoFocusNode(), which starts by
// ending a node edit. So an edit started by the application inside OnFocusChanged
// was immediately ended again by that redundant assignment. With the mouse the
// order of events differs, which is why it worked there.
//
// Fix as suggested in the issue discussion: SetFocusedNode() exits early when the
// node is already focused.

interface

uses
  DUnitX.TestFramework,
  Vcl.Forms,
  VirtualTrees;

type
  [TestFixture]
  TVTFocusChangedIssue1379Tests = class
  strict private
    fForm: TForm;
    fTree: TVirtualStringTree;
    fFocusChangedCount: Integer;
    procedure TreeFocusChangedStartsEdit(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// The minimal contract: re-assigning the already focused node must not end editing.
    [Test]
    procedure RefocusingSameNodeKeepsEditing;

    /// The reported scenario: an edit started in OnFocusChanged survives keyboard navigation.
    [Test]
    procedure EditStartedInFocusChangedSurvivesKeyNavigation;
  end;

implementation

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  VirtualTrees.Types;

procedure TVTFocusChangedIssue1379Tests.Setup;
begin
  fForm := TForm.Create(nil);
  fTree := TVirtualStringTree.Create(fForm);
  fTree.Parent := fForm;
  fTree.TreeOptions.MiscOptions := fTree.TreeOptions.MiscOptions + [toEditable];
  fTree.Header.Columns.Add;
  fTree.AddChild(fTree.RootNode);
  fTree.AddChild(fTree.RootNode);
  fForm.Show;
end;

procedure TVTFocusChangedIssue1379Tests.TearDown;
begin
  FreeAndNil(fForm);
end;

procedure TVTFocusChangedIssue1379Tests.TreeFocusChangedStartsEdit(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  Inc(fFocusChangedCount);
  if Assigned(Node) then
    fTree.EditNode(Node, 0);
end;

procedure TVTFocusChangedIssue1379Tests.RefocusingSameNodeKeepsEditing;
begin
  fTree.FocusedNode := fTree.GetFirst;
  Assert.IsTrue(fTree.EditNode(fTree.FocusedNode, 0), 'Sanity: editing must start.');
  Assert.IsTrue(tsEditing in fTree.TreeStates, 'Sanity: tree must be in editing state.');

  fTree.FocusedNode := fTree.FocusedNode;

  Assert.IsTrue(tsEditing in fTree.TreeStates,
    'Assigning the already focused node must not end node editing (issue #1379).');
end;

procedure TVTFocusChangedIssue1379Tests.EditStartedInFocusChangedSurvivesKeyNavigation;
begin
  fTree.FocusedNode := fTree.GetFirst;
  fTree.Selected[fTree.GetFirst] := True;
  fFocusChangedCount := 0;
  fTree.OnFocusChanged := TreeFocusChangedStartsEdit;

  fTree.Perform(WM_KEYDOWN, VK_DOWN, 0);

  Assert.AreEqual(1, fFocusChangedCount, 'Sanity: the key press must have changed the focus once.');
  Assert.AreEqual(fTree.GetNextSibling(fTree.GetFirst), fTree.FocusedNode,
    'Sanity: the second node must be focused now.');
  Assert.IsTrue(tsEditing in fTree.TreeStates,
    'The edit started in OnFocusChanged must survive the rest of the key handling (issue #1379).');
end;

initialization
  TDUnitX.RegisterTestFixture(TVTFocusChangedIssue1379Tests);

end.
