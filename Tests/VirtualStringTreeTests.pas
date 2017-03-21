unit VirtualStringTreeTests;

interface

uses
  DUnitX.TestFramework,
  Vcl.Forms,
  VirtualTrees;

type

  [TestFixture]
  TVirtualStringTreeTests = class(TObject)
  strict private
    fTree: TVirtualStringTree;
    fForm: TForm;
    procedure FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    /// Test for helper function VirtualTrees.Utils.OrderRect()
    [Test]
    procedure TestNodeData;
  end;

implementation

uses
  Vcl.Controls;

type
  TMyObject = class
  public
    Value : String;
  end;

{ TVirtualStringTreeTests }

procedure TVirtualStringTreeTests.FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  Node.GetData<TMyObject>.Free();
end;

procedure TVirtualStringTreeTests.Setup;
begin
  fForm := TForm.Create(nil);
  fTree := TVirtualStringTree.Create(fForm);
  fTree.OnFreeNode := FreeNode;
  fTree.Parent := fForm;
  fTree.Align := alClient;
end;

procedure TVirtualStringTreeTests.TearDown;
begin
  fForm.Release();
  fForm := nil;
  Application.ProcessMessages;
end;

procedure TVirtualStringTreeTests.TestNodeData;
var
  pL_Node : PVirtualNode;
  lMyObj1,
  lMyObj2 : TMyObject;
begin
  Assert.IsNotNull(fTree, 'The Virtual TreeView controls was not created successfully');
  fTree.BeginUpdate;
  try
    lMyObj1 := TMyObject.Create();
    lMyObj1.Value := 'Hello World';

    pL_Node := fTree.AddChild(nil, lMyObj1);
  finally
    fTree.EndUpdate;
  end;

  lMyObj2 := TMyObject(pL_Node.GetData()^);
  Assert.AreEqual(lMyObj1, lMyObj2, 'The object that was set as node data should equal the object that was retrieved from the node using TVirtualNode.GetData()');
  Assert.AreEqual(lMyObj1.Value, lMyObj2.Value, 'The object''s vlaue which was set as node data should equal the object that was retrieved from the node using TVirtualNode.GetData()');
  lMyObj2 := TMyObject(fTree.GetNodeData(pL_Node)^);
  Assert.AreEqual(lMyObj1, lMyObj2, 'The object that was set as node data should equal the object that was retrieved from the node using TBaseVirtualTree.GetNodeData()');
  Assert.AreEqual(lMyObj1.Value, lMyObj2.Value, 'The object''s value which was set as node data should equal the object that was retrieved from the node using TBaseVirtualTree.GetNodeData()');
  lMyObj2 := pL_Node.GetData<TMyObject>();
  Assert.AreEqual(lMyObj1, lMyObj2, 'The object that was set as node data should equal the object that was retrieved from the node using TVirtualNode.GetData<T>()');
  Assert.AreEqual(lMyObj1.Value, lMyObj2.Value, 'The object''s value which was set as node data should equal the object that was retrieved from the node using TVirtualNode.GetData<T>()');
end;

initialization
  TDUnitX.RegisterTestFixture(TVirtualStringTreeTests);

end.
