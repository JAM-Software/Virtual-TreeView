unit VTHeaderHintIssue728Tests;

// Regression tests for issue #728 "Header tooltip not always displaying".
//
// The header lives in the window's non-client area, so hovering it produces
// WM_NCMOUSEMOVE messages. While the application's hint window is the stock
// THintWindow, its IsHintMsg cancels the pending hint on every WM_NCMOUSEMOVE
// pulled from the message queue. The header only re-armed the hint pipeline
// (Application.HintMouseMessage) when the hover COLUMN changed, so any further
// mouse movement inside the same column killed the pending hint for good.
//
// Fix (local to TVTHeader, no application-global state):
//  1. Re-arm the hint pipeline on EVERY WM_NCMOUSEMOVE inside the header ...
//  2. ... except while the cursor is inside LastHintRect (a hint was already
//     accepted for this area). Re-entering the pipeline in that state bounces
//     off the LastHintRect short-circuit in CMHintShow, whose rejection makes
//     TApplication.ActivateHint cancel - and thereby hide - the visible hint.
//     This matters because showing the hint window itself posts a synthesized
//     WM_NCMOUSEMOVE at the unchanged cursor position.
//  3. The header's leave detection timer clears LastHintRect (the header band,
//     recognizable by Top < 0 in client coordinates), because the tree itself
//     only notices the departure via CM_MOUSELEAVE after the mouse visited its
//     client area.
//
// The tests below drive the header's message handler directly and observe the
// hint pipeline via CM_HINTSHOWPAUSE, which TApplication.HintMouseMessage sends
// to the hint control whenever it (re-)arms a hint. The full end-to-end
// behaviour (real cursor, real message pump, visible hint windows) is covered
// by the deterministic measurement harness build\HintProbe728V2.dpr.

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.Types,
  Winapi.Messages,
  Vcl.Forms,
  VirtualTrees;

type
  [TestFixture]
  TVTHeaderHintIssue728Tests = class
  strict private
    FForm: TForm;
    FTree: TVirtualStringTree;
    FOldTreeProc: TWndMethod;
    FHintShowPauseCount: Integer;
    procedure CountingTreeProc(var Message: TMessage);
    procedure SendHeaderNCMouseMove(OffsetX: Integer);
    function HeaderBandRect: TRect;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// A WM_NCMOUSEMOVE inside the header must re-arm the hint pipeline even if
    /// the hover column did not change. Arming only on column changes left the
    /// hint cancelled for good once the stock THintWindow.IsHintMsg had seen a
    /// WM_NCMOUSEMOVE - the core unreliability of issue #728.
    [Test]
    procedure RearmsHintPipelineOnEveryHeaderMouseMove;

    /// While the cursor is inside LastHintRect (a hint was already accepted for
    /// the header), the header must NOT re-enter the hint pipeline: the re-show
    /// would be rejected by the LastHintRect short-circuit in CMHintShow, and
    /// TApplication.ActivateHint hides the visible hint on rejection.
    [Test]
    procedure DoesNotRearmWhileCursorInsideLastHintRect;

    /// The header's leave detection must clear a header-band LastHintRect,
    /// otherwise no hint is shown on the next visit to the header (the tree
    /// only gets CM_MOUSELEAVE after the mouse visited its client area).
    [Test]
    procedure HeaderLeaveDetectionClearsLastHintRect;

    /// The leave detection must only clear the HEADER band (Top < 0). A hint
    /// rectangle inside the client area belongs to the node hint bookkeeping
    /// and is left alone.
    [Test]
    procedure HeaderLeaveDetectionKeepsClientAreaHintRect;
  end;

implementation

uses
  System.SysUtils,
  Winapi.Windows,
  Vcl.Controls,
  VirtualTrees.Types,
  VirtualTrees.BaseTree;

type
  TTreeCracker = class(TVirtualStringTree); // reach the protected LastHintRect

procedure TVTHeaderHintIssue728Tests.Setup;
var
  CursorPos: TPoint;
  FormLeft, FormTop: Integer;
begin
  // Place the form well away from the live mouse cursor so the real cursor can
  // neither sit in the header band (leave detection reads GetCursorPos) nor
  // interfere with the synthetic messages.
  GetCursorPos(CursorPos);
  if CursorPos.X < 560 then
    FormLeft := 620
  else
    FormLeft := 60;
  if CursorPos.Y < 460 then
    FormTop := 520
  else
    FormTop := 60;

  FForm := TForm.CreateNew(nil);
  FForm.SetBounds(FormLeft, FormTop, 420, 300);

  FTree := TVirtualStringTree.Create(FForm);
  FTree.Parent := FForm;
  FTree.SetBounds(20, 20, 360, 220);
  FTree.Header.Options := FTree.Header.Options + [hoVisible, hoShowHint];
  FTree.Header.Columns.Add;
  FTree.Header.Columns[0].Text := 'Column 0';
  FTree.Header.Columns[0].Hint := 'Header column hint';
  FTree.Header.Columns[0].Width := 340;
  FTree.ShowHint := True;

  // Show without activating and keep the window hittable for WindowFromPoint
  // (Application.HintMouseMessage resolves the hint control from the real
  // screen position carried by the message).
  FForm.Show;
  SetWindowPos(FForm.Handle, HWND_TOPMOST, 0, 0, 0, 0,
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);

  FHintShowPauseCount := 0;
  FOldTreeProc := FTree.WindowProc;
  FTree.WindowProc := CountingTreeProc;

  Application.CancelHint;
end;

procedure TVTHeaderHintIssue728Tests.TearDown;
begin
  Application.CancelHint; // do not leave an armed hint timer behind
  if Assigned(FTree) then
    FTree.WindowProc := FOldTreeProc;
  FreeAndNil(FForm);
  FTree := nil;
end;

procedure TVTHeaderHintIssue728Tests.CountingTreeProc(var Message: TMessage);
begin
  // TApplication.HintMouseMessage performs CM_HINTSHOWPAUSE on the hint control
  // whenever it (re-)arms a hint - the observable signal that the header entered
  // the hint pipeline.
  if Message.Msg = CM_HINTSHOWPAUSE then
    Inc(FHintShowPauseCount);
  FOldTreeProc(Message);
end;

procedure TVTHeaderHintIssue728Tests.SendHeaderNCMouseMove(OffsetX: Integer);
var
  P: TPoint;
begin
  // Center of the header band in screen coordinates; the client origin lies
  // below the header, so a negative Y addresses the header.
  P := FTree.ClientToScreen(
    Point(FTree.ClientWidth div 2 + OffsetX, -Integer(FTree.Header.Height) div 2));
  FTree.Perform(WM_NCMOUSEMOVE, HTBORDER, LPARAM(MakeLong(Word(P.X), Word(P.Y))));
end;

function TVTHeaderHintIssue728Tests.HeaderBandRect: TRect;
begin
  // The header band in client coordinates, as CMHintShow stores it in
  // LastHintRect when a header hint is accepted (Top < 0, small positive
  // Bottom for the splitter allowance).
  Result := Rect(0, -Integer(FTree.Header.Height), FTree.ClientWidth, 2);
end;

procedure TVTHeaderHintIssue728Tests.RearmsHintPipelineOnEveryHeaderMouseMove;
begin
  SendHeaderNCMouseMove(0);
  Assert.AreEqual(1, FHintShowPauseCount,
    'Sanity: entering the header must arm the hint pipeline. If this fails the ' +
    'tree window was not hittable at the synthetic screen position.');

  // The hint pipeline state is process-global; drop it so the second arming is
  // unconditional (this is what the stock THintWindow.IsHintMsg does to every
  // pending header hint anyway).
  Application.CancelHint;

  // Same hover column as before - no column change involved.
  SendHeaderNCMouseMove(4);
  Assert.AreEqual(2, FHintShowPauseCount,
    'A WM_NCMOUSEMOVE inside the same header column must re-arm the hint ' +
    'pipeline, otherwise a hint cancelled by the stock THintWindow.IsHintMsg ' +
    'is never re-armed and no header tooltip appears (issue #728).');
end;

procedure TVTHeaderHintIssue728Tests.DoesNotRearmWhileCursorInsideLastHintRect;
begin
  // A header hint was already accepted for the whole header band.
  TTreeCracker(FTree).LastHintRect := HeaderBandRect;

  SendHeaderNCMouseMove(0);
  Assert.AreEqual(0, FHintShowPauseCount,
    'While the cursor is inside LastHintRect the header must not re-enter the ' +
    'hint pipeline: the rejected re-show (LastHintRect short-circuit in ' +
    'CMHintShow) would make TApplication.ActivateHint hide the visible hint. ' +
    'The synthesized WM_NCMOUSEMOVE generated by showing the hint window would ' +
    'then kill every header hint within milliseconds (issue #728).');
end;

procedure TVTHeaderHintIssue728Tests.HeaderLeaveDetectionClearsLastHintRect;
begin
  TTreeCracker(FTree).LastHintRect := HeaderBandRect;

  // The real cursor is far away from the header (see Setup), so this tick takes
  // the leave branch of the header's mouse-leave detection.
  FTree.Perform(WM_TIMER, HeaderTimer, 0);

  Assert.IsTrue(IsRectEmpty(TTreeCracker(FTree).LastHintRect),
    'Leaving the header must clear a header-band LastHintRect so the next visit ' +
    'can show a hint again - the tree only receives CM_MOUSELEAVE after the ' +
    'mouse visited its client area (issue #728).');
end;

procedure TVTHeaderHintIssue728Tests.HeaderLeaveDetectionKeepsClientAreaHintRect;
var
  NodeRect: TRect;
begin
  NodeRect := Rect(10, 20, 200, 40); // a node hint rectangle, Top >= 0
  TTreeCracker(FTree).LastHintRect := NodeRect;

  FTree.Perform(WM_TIMER, HeaderTimer, 0);

  Assert.IsTrue(EqualRect(NodeRect, TTreeCracker(FTree).LastHintRect),
    'The header leave detection must only reset the header band (Top < 0); ' +
    'node hint bookkeeping in the client area is none of its business.');
end;

initialization
  TDUnitX.RegisterTestFixture(TVTHeaderHintIssue728Tests);

end.
