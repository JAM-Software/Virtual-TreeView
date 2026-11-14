unit VTDefaultNodeHeightIssue1375Tests;

// Regression tests for issue #1375 "SetDefaultNodeHeight in loading a invisible
// virtual tree view visible".
//
// For issue #1341, SetDefaultNodeHeight() calls HandleNeeded() when
// toAutoChangeScale is set, so that a DefaultNodeHeight assigned at runtime is
// not overwritten by AutoScale() when the window handle is created later.
//
// TControl.ReadState() assigns the Parent before the properties are read from
// the DFM. Therefore the window handles of the tree and of all its parents were
// created in the middle of loading the DFM, whenever DefaultNodeHeight was
// stored in it. If the topmost parent is not a form, e.g. a frame that is
// created at runtime and gets its Parent only after construction, creating the
// handle even raised EInvalidOperation "Control '...' has no parent window".
// The same happened at runtime when DefaultNodeHeight was assigned to a tree
// whose parent did not have a parent itself.

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Forms,
  VirtualTrees,
  VirtualTrees.Types;

type
  [TestFixture]
  TVTDefaultNodeHeightIssue1375Tests = class
  strict private
    /// Reads the given DFM text into the existing root component, like
    /// InitInheritedComponent() does with the DFM resource of a form or frame.
    procedure LoadFromDfmText(aRoot: TComponent; const aDfmText: string);
    function FindTree(aRoot: TComponent): TVirtualStringTree;
  public
    /// Loading a tree with a stored DefaultNodeHeight from a DFM must not
    /// create its window handle while loading.
    [Test]
    procedure LoadingDoesNotCreateWindowHandle;

    /// Loading a frame that has no parent yet must not raise an exception,
    /// e.g. TMyFrame.Create(Self) before assigning the Parent of the frame.
    [Test]
    procedure LoadingFrameWithoutParentDoesNotRaise;

    /// Assigning DefaultNodeHeight at runtime to a tree whose parent has no
    /// parent itself must not raise an exception.
    [Test]
    procedure SetDefaultNodeHeightInParentlessContainerDoesNotRaise;

    /// Guards the behavior of issue #1341: a DefaultNodeHeight assigned at
    /// runtime before the window handle exists must be kept for the nodes,
    /// although toAutoChangeScale is set.
    [Test]
    procedure DefaultNodeHeightSetAtRuntimeIsKept;
  end;

implementation

uses
  System.SysUtils;

const
  cStoredNodeHeight = 30; // differs from cInitialDefaultNodeHeight, so it is stored in the DFM

  cFormDfm =
    'object Issue1375Form: TForm' + sLineBreak +
    '  object Tree: TVirtualStringTree' + sLineBreak +
    '    DefaultNodeHeight = 30' + sLineBreak +
    '  end' + sLineBreak +
    'end';

  cFrameDfm =
    'object Issue1375Frame: TFrame' + sLineBreak +
    '  object Tree: TVirtualStringTree' + sLineBreak +
    '    DefaultNodeHeight = 30' + sLineBreak +
    '  end' + sLineBreak +
    'end';

procedure TVTDefaultNodeHeightIssue1375Tests.LoadFromDfmText(aRoot: TComponent; const aDfmText: string);
var
  lTextStream: TStringStream;
  lBinaryStream: TMemoryStream;
begin
  lTextStream := TStringStream.Create(aDfmText);
  lBinaryStream := TMemoryStream.Create;
  try
    ObjectTextToBinary(lTextStream, lBinaryStream);
    lBinaryStream.Position := 0;
    lBinaryStream.ReadComponent(aRoot);
  finally
    lBinaryStream.Free;
    lTextStream.Free;
  end;
end;

function TVTDefaultNodeHeightIssue1375Tests.FindTree(aRoot: TComponent): TVirtualStringTree;
begin
  Result := aRoot.FindComponent('Tree') as TVirtualStringTree;
  Assert.IsNotNull(Result, 'Sanity: the tree must have been loaded from the DFM.');
end;

procedure TVTDefaultNodeHeightIssue1375Tests.LoadingDoesNotCreateWindowHandle;
var
  lForm: TForm;
  lTree: TVirtualStringTree;
begin
  lForm := TForm.CreateNew(nil);
  try
    LoadFromDfmText(lForm, cFormDfm);
    lTree := FindTree(lForm);
    Assert.IsTrue(toAutoChangeScale in lTree.TreeOptions.AutoOptions, 'Sanity: toAutoChangeScale must be set by default.');
    // Only the handle of the tree is checked: The VCL creates the handle of the form anyway while loading it.
    Assert.IsFalse(lTree.HandleAllocated, 'Loading a stored DefaultNodeHeight from a DFM must not create the window handle of the tree (issue #1375).');
    Assert.AreEqual<TDimension>(cStoredNodeHeight, lTree.DefaultNodeHeight, 'The DefaultNodeHeight from the DFM must have been applied.');
  finally
    lForm.Free;
  end;
end;

procedure TVTDefaultNodeHeightIssue1375Tests.LoadingFrameWithoutParentDoesNotRaise;
var
  lFrame: TFrame;
begin
  lFrame := TFrame.Create(nil);
  try
    Assert.WillNotRaiseAny(
      procedure
      begin
        LoadFromDfmText(lFrame, cFrameDfm);
      end,
      'Loading a frame without parent that contains a tree with a stored DefaultNodeHeight must not raise an exception (issue #1375).');
    Assert.AreEqual<TDimension>(cStoredNodeHeight, FindTree(lFrame).DefaultNodeHeight, 'The DefaultNodeHeight from the DFM must have been applied.');
  finally
    lFrame.Free;
  end;
end;

procedure TVTDefaultNodeHeightIssue1375Tests.SetDefaultNodeHeightInParentlessContainerDoesNotRaise;
var
  lPanel: TPanel;
  lTree: TVirtualStringTree;
begin
  lPanel := TPanel.Create(nil);
  try
    lTree := TVirtualStringTree.Create(lPanel);
    lTree.Parent := lPanel;
    Assert.WillNotRaiseAny(
      procedure
      begin
        lTree.DefaultNodeHeight := cStoredNodeHeight;
      end,
      'Setting DefaultNodeHeight of a tree whose parent has no parent must not raise an exception (issue #1375).');
    Assert.AreEqual<TDimension>(cStoredNodeHeight, lTree.DefaultNodeHeight);
  finally
    lPanel.Free;
  end;
end;

procedure TVTDefaultNodeHeightIssue1375Tests.DefaultNodeHeightSetAtRuntimeIsKept;
const
  cNodeHeight = 40;
var
  lForm: TForm;
  lTree: TVirtualStringTree;
  lNode: PVirtualNode;
begin
  lForm := TForm.CreateNew(nil);
  try
    lTree := TVirtualStringTree.Create(lForm);
    lTree.Parent := lForm;
    Assert.IsTrue(toAutoChangeScale in lTree.TreeOptions.AutoOptions, 'Sanity: toAutoChangeScale must be set by default.');
    lTree.DefaultNodeHeight := cNodeHeight;
    lTree.RootNodeCount := 3;
    lForm.Show;
    Application.ProcessMessages;
    Assert.AreEqual<TDimension>(cNodeHeight, lTree.DefaultNodeHeight, 'A DefaultNodeHeight set at runtime must be kept (issue #1341).');
    for lNode in lTree.Nodes do
      Assert.AreEqual<TDimension>(cNodeHeight, lNode.NodeHeight, 'The nodes must use the DefaultNodeHeight set at runtime (issue #1341).');
  finally
    lForm.Free;
  end;
end;

initialization
  // Needed to create the tree from the DFM text, as TForm and TFrame have no published field of this class.
  RegisterClass(TVirtualStringTree);
  TDUnitX.RegisterTestFixture(TVTDefaultNodeHeightIssue1375Tests);

end.
