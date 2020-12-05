unit VTWorkerThreadIssue1001Tests;

interface

uses
  DUnitX.TestFramework,
  Classes,
  Vcl.Forms,
  VirtualTrees;

type
  TTestBaseVirtualTree = class(TBaseVirtualTree)
  public
    property OnCompareNodes;
  end;

  [TestFixture]
  TVTWorkerThreadIssue1001Tests = class
  strict private
    fTree: TTestBaseVirtualTree;
    fForm: TForm;
    procedure TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// Test for CheckSynchronize when tree is destroyed
    /// repeated 50 times because AVs are not realiable
    [Test, RepeatTestAttribute(100)]
    procedure TestDestroyWhileWorkerThreadBusy;
  end;

implementation

uses
  VirtualTrees.WorkerThread,
  SysUtils;

procedure TVTWorkerThreadIssue1001Tests.Setup;
begin
  TThread.Synchronize(nil, procedure
    begin
      fForm := TForm.Create(nil);
      fTree := TTestBaseVirtualTree.Create(fForm);
      fTree.TreeOptions.AutoOptions:= fTree.TreeOptions.AutoOptions - [toAutoSort];
      fTree.OnCompareNodes:= TreeCompareNodes;
    end);
end;

procedure TVTWorkerThreadIssue1001Tests.TearDown;
begin
  TThread.Synchronize(nil, procedure
    begin
      FreeAndNil(fForm);
    end);
end;

procedure TVTWorkerThreadIssue1001Tests.TestDestroyWhileWorkerThreadBusy;
begin
  TThread.Synchronize(nil, procedure
    begin
      fTree.BeginUpdate;
      try
        fTree.SetChildCount(fTree.RootNode, 10000);
        Assert.AreEqual(fTree.RootNode.ChildCount + 1, fTree.RootNode.TotalCount, 'TotalCount <> ChildCount + 1');
        //fTree.SortTree(-1, sdAscending, false);
      finally
        fTree.EndUpdate;
      end;
      FreeAndNil(fTree);
      FreeAndNil(fForm);
    end);
end;

procedure TVTWorkerThreadIssue1001Tests.TreeCompareNodes(
  Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);
begin
  if Random(10) > 5 then
    Result:= 1 else
    Result:= -1;
end;

initialization
  Randomize;
  TDUnitX.RegisterTestFixture(TVTWorkerThreadIssue1001Tests);

end.
