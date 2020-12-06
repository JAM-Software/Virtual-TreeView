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
      fTree.SetChildCount(fTree.RootNode, 10000);
      Assert.AreEqual(fTree.RootNode.ChildCount + 1, fTree.RootNode.TotalCount, 'TotalCount <> ChildCount + 1');
      FreeAndNil(fTree);
      FreeAndNil(fForm);

      //Now that the tree is destroyed, we have to ensure that the code called
      //from WorkerThread.Execute via Synchronize is executed and causes the AV.
      //In a real-world GUI Application, CheckSynchronize is called often, but
      //here in a Console application we have to do this ourselves.
      //Unfortunately the AV will not make the test fail, since it is raised
      //in WorkerThread, which has FreeOnTerminate = True; therefore the AVs
      //can only be seen with the debugger.
      CheckSynchronize;
    end);
end;

initialization
  TDUnitX.RegisterTestFixture(TVTWorkerThreadIssue1001Tests);

end.
