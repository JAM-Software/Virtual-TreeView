unit VTOnEditCancelledTests;

interface

uses
  DUnitX.TestFramework,
  Vcl.Forms,
  VirtualTrees;

type

  [TestFixture]
  TVTOnEditCancelledTests = class
  strict private
    fTree: TVirtualStringTree;
    fForm: TForm;
    FEditCancelled: Boolean;
    procedure TreeEditCancelled(Sender: TBaseVirtualTree; Column: TColumnIndex);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestAddColumn;

    [Test]
    procedure TestEditNodeFail;

    [Test]
    procedure TestEditNode;

    [Test]
    procedure TestOnEditCancelled;
  end;

implementation

uses
  System.SysUtils, VirtualTrees.Types;

procedure TVTOnEditCancelledTests.Setup;
begin
  fForm := TForm.Create(nil);
  fTree := TVirtualStringTree.Create(fForm);
end;

procedure TVTOnEditCancelledTests.TearDown;
begin
  FreeAndNil(fForm);
end;

procedure TVTOnEditCancelledTests.TestAddColumn;
begin
  var LBeforeColumnCount := fTree.Header.Columns.Count;
  fTree.Header.Columns.Add;
  var LAfterColumnCount := fTree.Header.Columns.Count;
  Assert.AreEqual<Integer>(LAfterColumnCount - LBeforeColumnCount, 1);
end;

procedure TVTOnEditCancelledTests.TestEditNode;
begin
  fForm.Show;
  fTree.TreeOptions.MiscOptions := fTree.TreeOptions.MiscOptions + [toEditable];
  fTree.Parent := fForm;
  fTree.Header.Columns.Add;
  var LNode := fTree.AddChild(fTree.RootNode);
  var LBeforeStates := fTree.TreeStates;
  var LEditNodeResult := fTree.EditNode(LNode, 0);
  var LAfterStates := fTree.TreeStates;
  Assert.AreEqual<TVirtualTreeStates>(LAfterStates * [tsEditing], [tsEditing]);
  Assert.IsTrue(LEditNodeResult);
end;

procedure TVTOnEditCancelledTests.TestEditNodeFail;
begin
  fForm.Show;
  fTree.TreeOptions.MiscOptions := fTree.TreeOptions.MiscOptions - [toEditable];
  fTree.Parent := fForm;
  fTree.Header.Columns.Add;
  var LNode := fTree.AddChild(fTree.RootNode);
  var LBeforeStates := fTree.TreeStates;
  var LEditNodeResult := fTree.EditNode(LNode, 0);
  Assert.IsFalse(LEditNodeResult);
end;

procedure TVTOnEditCancelledTests.TestOnEditCancelled;
begin
  fForm.Show;
  FEditCancelled := False;
  fTree.OnEditCancelled := TreeEditCancelled;
  fTree.TreeOptions.MiscOptions := fTree.TreeOptions.MiscOptions + [toEditable];
  var LNode := fTree.AddChild(fTree.RootNode);
  fTree.Parent := fForm;
  fTree.Header.Columns.Add;
  fTree.EditNode(LNode, 0);
  fTree.CancelEditNode;
  Assert.IsTrue(FEditCancelled);
end;

procedure TVTOnEditCancelledTests.TreeEditCancelled(Sender: TBaseVirtualTree;
  Column: TColumnIndex);
begin
  FEditCancelled := True;
end;

initialization
  TDUnitX.RegisterTestFixture(TVTOnEditCancelledTests);
end.
