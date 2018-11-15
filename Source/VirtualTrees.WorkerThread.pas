unit VirtualTrees.WorkerThread;

interface
//{$DEFINE VT_FMX}
{$IFNDEF VT_FMX}
  {$DEFINE VT_VCL}
{$ENDIF}

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
    class procedure Dispose();
    procedure CancelValidation(Tree: TBaseVirtualTree);
  protected
    procedure Execute; override;
  public
    constructor Create();
    destructor Destroy; override;

    /// For lifeteime management of the TWorkerThread
    class procedure AddThreadReference;
    class procedure ReleaseThreadReference();

    class procedure AddTree(Tree: TBaseVirtualTree);
    class procedure RemoveTree(Tree: TBaseVirtualTree);

    property CurrentTree: TBaseVirtualTree read FCurrentTree;
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

class procedure TWorkerThread.Dispose();
begin
  WorkerThread.Terminate();
  SetEvent(WorkEvent);
  WorkerThread := nil; //Will be freed usinf TThreaf.FreeOnTerminate
  CloseHandle(WorkEvent);
end;


class procedure TWorkerThread.AddThreadReference;
begin
  TWorkerThread.EnsureCreated();
  InterlockedIncrement(WorkerThread.FRefCount);
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TWorkerThread.ReleaseThreadReference();
begin
  if Assigned(WorkerThread) then
  begin
    InterlockedDecrement(WorkerThread.FRefCount);

    if WorkerThread.FRefCount = 0 then
      WorkerThread.Dispose();
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

procedure TWorkerThread.CancelValidation(Tree: TBaseVirtualTree);

//var
//  Msg: TMsg;

begin
  // Wait for any references to this tree to be released.
  // Pump WM_CHANGESTATE messages so the thread doesn't block on SendMessage calls.
  while FCurrentTree = Tree do
  begin
//    if Tree.HandleAllocated and PeekMessage(Msg, Tree.Handle, WM_CHANGESTATE, WM_CHANGESTATE, PM_REMOVE) then
//    begin
//      TranslateMessage(Msg);
//      DispatchMessage(Msg);
//      Continue;
//    end;
    Yield();
    if (toVariableNodeHeight in TBaseVirtualTreeCracker(Tree).TreeOptions.MiscOptions) then
      CheckSynchronize(); // We need to call CheckSynchronize here because we are using TThread.Synchronize in TBaseVirtualTree.MeasureItemHeight()
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWorkerThread.Execute;

// Does some background tasks, like validating tree caches.

var
  EnterStates,
  LeaveStates: TChangeStates;
  lCurrentTree: TBaseVirtualTree;

begin
  TThread.NameThreadForDebugging('VirtualTrees.TWorkerThread');
  while not Terminated do
  begin
    WaitForSingleObject(WorkEvent, INFINITE);
    if not Terminated then
    begin
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
          TBaseVirtualTreeCracker(FCurrentTree).ChangeTreeStatesAsync([csValidating], [csUseCache, csValidationNeeded]);
          EnterStates := [];
          if not (tsStopValidation in FCurrentTree.TreeStates) and TBaseVirtualTreeCracker(FCurrentTree).DoValidateCache then
            EnterStates := [csUseCache];

        finally
          LeaveStates := [csValidating, csStopValidation];
          TBaseVirtualTreeCracker(FCurrentTree).ChangeTreeStatesAsync(EnterStates, LeaveStates);
          lCurrentTree := FCurrentTree; // Save reference in a local variable for later use
          FCurrentTree := nil; //Clear variable to prevent deadlock in CancelValidation. See #434
          Queue(TBaseVirtualTreeCracker(lCurrentTree).UpdateEditBounds);
        end;
      end;
    end;
  end;
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
  WorkerThread.CancelValidation(Tree);
end;


end.
