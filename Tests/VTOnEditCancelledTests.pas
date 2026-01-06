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
    procedure TestOnEditCancelled;
  end;

implementation

uses
  System.SysUtils;

procedure TVTOnEditCancelledTests.Setup;
begin
  fForm := TForm.Create(nil);
  fTree := TVirtualStringTree.Create(fForm);
end;

procedure TVTOnEditCancelledTests.TearDown;
begin
  FreeAndNil(fForm);
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
