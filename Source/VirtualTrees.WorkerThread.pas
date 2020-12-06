unit VirtualTrees.WorkerThread;

interface

uses
  System.Classes,
  VirtualTrees;

type
  // internal worker thread
  TWorkerThread = class(TThread)
  private
    FCurrentTree: TBaseVirtualTree;
    FWaiterList: TThreadList;
    FRefCount: Integer;
    class procedure EnsureCreated();
    class procedure Dispose(CanBlock: Boolean);
    procedure WaitForValidationTermination(Tree: TBaseVirtualTree);
  protected
    procedure Execute; override;
  public
    constructor Create();
    destructor Destroy; override;

    /// For lifeteime management of the TWorkerThread
    class procedure AddThreadReference;
    class procedure ReleaseThreadReference(ACanBlock: Boolean = False);

    class procedure AddTree(Tree: TBaseVirtualTree);
    class procedure RemoveTree(Tree: TBaseVirtualTree);
  end;





implementation

uses
  Winapi.Windows,
  System.Types,
  System.SysUtils;

type
  TBaseVirtualTreeCracker = class(TBaseVirtualTree)
  end;

var
  WorkerThread: TWorkerThread = nil;
  WorkEvent: THandle;
//----------------- TWorkerThread --------------------------------------------------------------------------------------

class procedure TWorkerThread.EnsureCreated();
begin
  if not Assigned(WorkerThread) then
  begin
    // Create an event used to trigger our worker thread when something is to do.
    WorkEvent := CreateEvent(nil, False, False, nil);
    if WorkEvent = 0 then
      RaiseLastOSError;

    // Create worker thread, initialize it and send it to its wait loop.
    WorkerThread := TWorkerThread.Create();
  end;
end;

class procedure TWorkerThread.Dispose;
var
  LRef: TThread;
begin
  WorkerThread.FreeOnTerminate := not CanBlock;
  WorkerThread.Terminate();
  SetEvent(WorkEvent);
  LRef := WorkerThread;
  WorkerThread := nil; //Will be freed usinf TThreaf.FreeOnTerminate
  CloseHandle(WorkEvent);
  if CanBlock then
    LRef.Free;
end;


class procedure TWorkerThread.AddThreadReference;
begin
  TWorkerThread.EnsureCreated();
  InterlockedIncrement(WorkerThread.FRefCount);
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TWorkerThread.ReleaseThreadReference(ACanBlock: Boolean);
begin
  if Assigned(WorkerThread) then
  begin
    if InterlockedDecrement(WorkerThread.FRefCount) = 0 then
    begin
      WorkerThread.Dispose(ACanBlock);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TWorkerThread.Create();

begin
  inherited Create(False);
  FreeOnTerminate := True;
  FWaiterList := TThreadList.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TWorkerThread.Destroy;

begin
  // First let the ancestor stop the thread before freeing our resources.
  inherited;
  FWaiterList.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWorkerThread.WaitForValidationTermination(Tree: TBaseVirtualTree);
begin
  // Wait for any references to this tree to be released.
  while FCurrentTree = Tree do
  begin
    Sleep(1);
    CheckSynchronize(); //since Execute uses Synchronize, it must have a chance to finish; #1000
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWorkerThread.Execute();

// Does some background tasks, like validating tree caches.

var
  EnterStates: TVirtualTreeStates;
begin
  TThread.NameThreadForDebugging('VirtualTrees.TWorkerThread');
  while not Terminated do
  begin
    WaitForSingleObject(WorkEvent, INFINITE);
    if Terminated then
      exit;

    // Get the next waiting tree.
    with FWaiterList.LockList do
    try
      if Count > 0 then
      begin
        FCurrentTree := Items[0];
        // Remove this tree from waiter list.
        Delete(0);
        // If there is yet another tree to work on then set the work event to keep looping.
        if Count > 0 then
          SetEvent(WorkEvent);
      end
      else
        FCurrentTree := nil;
    finally
      FWaiterList.UnlockList;
    end;

    // Something to do?
    if Assigned(FCurrentTree) then
    begin
      try
        TThread.Synchronize(nil, procedure
          begin
            TBaseVirtualTreeCracker(FCurrentTree).DoStateChange([tsValidating], [tsUseCache, tsValidationNeeded]);
          end);
        EnterStates := [];
        if not (tsStopValidation in FCurrentTree.TreeStates) and TBaseVirtualTreeCracker(FCurrentTree).DoValidateCache then
          EnterStates := [tsUseCache];

      finally
        TThread.Synchronize(nil, procedure
          begin
            TBaseVirtualTreeCracker(FCurrentTree).DoStateChange(EnterStates, [tsValidating, tsStopValidation]);
            TBaseVirtualTreeCracker(FCurrentTree).UpdateEditBounds;
          end);
        FCurrentTree := nil;
      end;
    end;
  end;//while
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TWorkerThread.AddTree(Tree: TBaseVirtualTree);

begin
  Assert(Assigned(Tree), 'Tree must not be nil.');
  TWorkerThread.EnsureCreated();

  // Remove validation stop flag, just in case it is still set.
  TBaseVirtualTreeCracker(Tree).DoStateChange([], [tsStopValidation]);
  with WorkerThread.FWaiterList.LockList do
  try
    if IndexOf(Tree) = -1 then
      Add(Tree);
  finally
    WorkerThread.FWaiterList.UnlockList;
  end;

  SetEvent(WorkEvent);
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TWorkerThread.RemoveTree(Tree: TBaseVirtualTree);

begin
  if not Assigned(WorkerThread) then
    exit;
  Assert(Assigned(Tree), 'Tree must not be nil.');

  with WorkerThread.FWaiterList.LockList do
  try
    Remove(Tree);
  finally
    WorkerThread.FWaiterList.UnlockList; // Seen several AVs in this line, was called from TWorkerThrea.Destroy. Joachim Marder.
  end;
  WorkerThread.WaitForValidationTermination(Tree);
end;


end.
