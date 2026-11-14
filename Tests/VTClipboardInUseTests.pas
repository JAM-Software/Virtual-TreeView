unit VTClipboardInUseTests;

// Regression tests for clipboard operations while another process has the clipboard open.
//
// The clipboard is shared by all processes. The Windows clipboard user service, for example,
// opens it shortly after every change, and while it is open, OleSetClipboard() and
// OleFlushClipboard() fail with CLIPBRD_E_CANT_OPEN. CopyToClipboard() then silently copied
// nothing. FlushClipboard(), and the destructor of a data object that never made it onto the
// clipboard, cancelled the pending copy although our data object was still on the clipboard,
// so that object rendered no nodes later. This made the TestCopy* tests in
// VTCellSelectionTests fail now and then.
//
// The tests simulate the other process with a thread that keeps the clipboard open.

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SyncObjs,
  Vcl.Forms,
  VirtualTrees;

type
  /// Keeps the clipboard open in a background thread, like another process that reads it.
  TClipboardBlocker = class(TThread)
  strict private
    fOpened: TEvent;
    fRelease: TEvent;
    fHoldTime: Cardinal;
    fOpenedClipboard: Boolean;
  protected
    procedure Execute; override;
  public
    /// Starts a thread that opens the clipboard and keeps it open for HoldTime milliseconds, or
    /// until the thread is freed if HoldTime is INFINITE.
    constructor Create(HoldTime: Cardinal);
    /// Closes the clipboard and waits for the thread to end.
    destructor Destroy; override;
    /// Waits until the thread has opened the clipboard. Returns False if it could not open it.
    function WaitUntilOpen: Boolean;
  end;

  [TestFixture]
  TVTClipboardInUseTests = class
  strict private
    fForm: TForm;
    fTree: TVirtualStringTree;
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    function BlockClipboard(HoldTime: Cardinal): TClipboardBlocker;
    function ReadClipboardText: string;
    function SelectNodes(const Indexes: array of Cardinal): string;
    procedure CopyAndCheck;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// CopyToClipboard() waits if another process has the clipboard open for a moment.
    [Test]
    procedure CopyWaitsForClipboard;

    /// FlushClipboard() waits if another process has the clipboard open for a moment.
    [Test]
    procedure FlushWaitsForClipboard;

    /// If the clipboard stays in use, FlushClipboard() gives up but keeps the pending copy, so
    /// our data object, which is still on the clipboard, renders the copied nodes later.
    [Test]
    procedure FailedFlushKeepsPendingCopy;

    /// If the clipboard stays in use, CopyToClipboard() gives up, but it must not cancel the
    /// previous copy, whose data object is still on the clipboard.
    [Test]
    procedure FailedCopyKeepsPreviousCopy;
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  Vcl.Controls,
  VirtualTrees.Types,
  VirtualTrees.Utils, // GetTickCount64() for RAD Studio versions older than 10.2, see issue #1410
  VirtualTrees.ClipBoard;

const
  // How long the helper functions try to open the clipboard before they give up.
  OpenClipboardTimeout = 5000;

{ TClipboardBlocker }

constructor TClipboardBlocker.Create(HoldTime: Cardinal);
begin
  fOpened := TEvent.Create(nil, True, False, '');
  fRelease := TEvent.Create(nil, True, False, '');
  fHoldTime := HoldTime;
  inherited Create(False);
end;

destructor TClipboardBlocker.Destroy;
begin
  fRelease.SetEvent;
  inherited; // Waits for the thread to end.
  fOpened.Free;
  fRelease.Free;
end;

procedure TClipboardBlocker.Execute;
var
  StartTime: UInt64;
begin
  // The clipboard may be in use by another process just now, so try for a while.
  StartTime := GetTickCount64;
  while not OpenClipboard(0) do
  begin
    if GetTickCount64 - StartTime > OpenClipboardTimeout then
    begin
      fOpened.SetEvent;
      Exit;
    end;
    Sleep(10);
  end;
  try
    fOpenedClipboard := True;
    fOpened.SetEvent;
    fRelease.WaitFor(fHoldTime);
  finally
    CloseClipboard;
  end;
end;

function TClipboardBlocker.WaitUntilOpen: Boolean;
var
  StartTime: UInt64;
begin
  // Another process that has the clipboard open may wait for this thread to render our
  // delayed clipboard data, so messages are processed while waiting.
  StartTime := GetTickCount64;
  while fOpened.WaitFor(10) <> wrSignaled do
  begin
    if GetTickCount64 - StartTime > 2 * OpenClipboardTimeout then
      Exit(False);
    Application.ProcessMessages;
  end;
  Result := fOpenedClipboard;
end;

{ TVTClipboardInUseTests }

procedure TVTClipboardInUseTests.Setup;
begin
  fForm := TForm.Create(nil);
  fTree := TVirtualStringTree.Create(fForm);
  fTree.Parent := fForm;
  fTree.Align := alClient;
  fTree.TreeOptions.SelectionOptions := fTree.TreeOptions.SelectionOptions + [toMultiSelect];
  fTree.OnGetText := TreeGetText;
  fTree.ClipboardFormats.Add(GetVTClipboardFormatDescription(CF_UNICODETEXT));
  fTree.RootNodeCount := 3;
  fForm.Show;
end;

procedure TVTClipboardInUseTests.TearDown;
begin
  // Freeing the tree takes our data object off the clipboard if a copy is still pending.
  FreeAndNil(fForm);
end;

procedure TVTClipboardInUseTests.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
begin
  CellText := 'Node ' + IntToStr(Node.Index);
end;

function TVTClipboardInUseTests.BlockClipboard(HoldTime: Cardinal): TClipboardBlocker;
begin
  Result := TClipboardBlocker.Create(HoldTime);
  if not Result.WaitUntilOpen then
  begin
    Result.Free;
    Assert.Fail('The blocking thread could not open the clipboard.');
  end;
end;

function TVTClipboardInUseTests.ReadClipboardText: string;
var
  StartTime: UInt64;
  Data: THandle;
begin
  Result := '';
  // Another process may have the clipboard open just now, and it may wait for this thread to
  // render our delayed clipboard data, so messages are processed while trying.
  StartTime := GetTickCount64;
  while not OpenClipboard(0) do
  begin
    if GetTickCount64 - StartTime > OpenClipboardTimeout then
      Assert.Fail('Could not open the clipboard to read it.');
    Application.ProcessMessages;
    Sleep(10);
  end;
  try
    // Asking for the data lets our data object render it if it is still on the clipboard.
    Data := GetClipboardData(CF_UNICODETEXT);
    if Data <> 0 then
    begin
      Result := PChar(GlobalLock(Data));
      GlobalUnlock(Data);
    end;
  finally
    CloseClipboard;
  end;
end;

function TVTClipboardInUseTests.SelectNodes(const Indexes: array of Cardinal): string;
var
  Node: PVirtualNode;
  Index: Cardinal;
begin
  fTree.ClearSelection;
  for Node in fTree.Nodes do
    for Index in Indexes do
      if Node.Index = Index then
        fTree.Selected[Node] := True;
  // This is what the clipboard has to contain after the selected nodes were copied.
  Result := fTree.ContentToText(tstSelected, #9);
  Assert.IsNotEmpty(Result, 'No text for the selected nodes.');
end;

procedure TVTClipboardInUseTests.CopyAndCheck;
begin
  fTree.CopyToClipboard;
  Assert.IsTrue(tsCopyPending in fTree.TreeStates, 'Precondition: CopyToClipboard() failed.');
end;

procedure TVTClipboardInUseTests.CopyWaitsForClipboard;
var
  Expected: string;
  Blocker: TClipboardBlocker;
begin
  Expected := SelectNodes([0, 1]);
  Blocker := BlockClipboard(50);
  try
    fTree.CopyToClipboard;
  finally
    Blocker.Free;
  end;
  Assert.IsTrue(tsCopyPending in fTree.TreeStates, 'CopyToClipboard() gave up while the clipboard was in use.');
  fTree.FlushClipboard;
  Assert.AreEqual(Expected, ReadClipboardText);
end;

procedure TVTClipboardInUseTests.FlushWaitsForClipboard;
var
  Expected: string;
  Blocker: TClipboardBlocker;
begin
  Expected := SelectNodes([0, 1]);
  CopyAndCheck;
  Blocker := BlockClipboard(50);
  try
    fTree.FlushClipboard;
  finally
    Blocker.Free;
  end;
  Assert.IsFalse(tsCopyPending in fTree.TreeStates, 'FlushClipboard() did not finish the copy.');
  Assert.AreEqual(Expected, ReadClipboardText);
end;

procedure TVTClipboardInUseTests.FailedFlushKeepsPendingCopy;
var
  Expected: string;
  Blocker: TClipboardBlocker;
begin
  Expected := SelectNodes([0, 1]);
  CopyAndCheck;
  Blocker := BlockClipboard(INFINITE);
  try
    fTree.FlushClipboard; // Gives up, as the clipboard stays in use.
    Assert.IsTrue(tsCopyPending in fTree.TreeStates,
      'FlushClipboard() cancelled the copy although it could not flush the clipboard.');
  finally
    Blocker.Free;
  end;
  Assert.AreEqual(Expected, ReadClipboardText, 'The data object on the clipboard did not render the copied nodes.');
end;

procedure TVTClipboardInUseTests.FailedCopyKeepsPreviousCopy;
var
  Expected: string;
  Blocker: TClipboardBlocker;
begin
  Expected := SelectNodes([0, 1]);
  CopyAndCheck;
  SelectNodes([2]);
  Blocker := BlockClipboard(INFINITE);
  try
    fTree.CopyToClipboard; // Gives up, as the clipboard stays in use.
    Assert.IsTrue(tsCopyPending in fTree.TreeStates, 'A failed CopyToClipboard() cancelled the previous copy.');
  finally
    Blocker.Free;
  end;
  Assert.AreEqual(Expected, ReadClipboardText, 'The data object on the clipboard did not render the copied nodes.');
end;

initialization
  TDUnitX.RegisterTestFixture(TVTClipboardInUseTests);
end.
