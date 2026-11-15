unit VTCheckImagesIssue1082Tests;

// Tests for issue #1082 "Reduce the number of GDI handles per tree".
//
// Every tree created an image list with the system check images in CreateWnd,
// even if toCheckSupport was not set and the tree never showed a check box. The
// image list costs several GDI handles per tree, which adds up in applications
// with many trees. Now the system check images are created on first use only.
//
// The tests make sure that
// - a tree without toCheckSupport does not create the check images and
//   therefore uses no GDI handles for them,
// - check boxes are still painted and hit-tested when toCheckSupport is set
//   from the start or switched on at runtime,
// - the public CheckImages property still returns the check images on demand,
//   e.g. for header check boxes, which do not need toCheckSupport,
// - the check images get the new size after a DPI change (issue #796).

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  Vcl.Forms,
  Vcl.Graphics,
  VirtualTrees,
  VirtualTrees.Types;

type
  [TestFixture]
  TVTCheckImagesIssue1082Tests = class
  strict private
    fForm: TForm;
    function CreateTree(aCheckSupport: Boolean): TVirtualStringTree;
    /// Switches on toCheckSupport and gives all nodes a check box.
    procedure EnableCheckBoxes(aTree: TVirtualStringTree);
    /// Renders the tree with PaintTree() and returns the raw pixels.
    function Render(aTree: TVirtualStringTree): TBytes;
    /// Returns True if the check box of the first node can be hit.
    function CheckBoxHit(aTree: TVirtualStringTree): Boolean;
    /// Asserts that the check box of the first node is painted, i.e. the
    /// rendering changes when the node gets checked.
    procedure AssertCheckBoxPainted(aTree: TVirtualStringTree);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// A tree without toCheckSupport must not create the check images when
    /// its window is created, painted or hit-tested.
    [Test]
    procedure NoCheckImagesWithoutCheckSupport;

    /// Trees without toCheckSupport must not use GDI handles for check images:
    /// the handles are only allocated once check support is switched on.
    [Test]
    procedure NoGdiHandlesForCheckImagesWithoutCheckSupport;

    /// Check boxes are painted and hit-tested when toCheckSupport is set before
    /// the window is created.
    [Test]
    procedure CheckBoxesWorkWithCheckSupport;

    /// Check boxes are painted and hit-tested when toCheckSupport is switched
    /// on after the tree was already painted without it.
    [Test]
    procedure CheckBoxesWorkWhenCheckSupportSwitchedOnLater;

    /// The public CheckImages property creates the system check images on
    /// demand, also without toCheckSupport, like it returned them before.
    [Test]
    procedure CheckImagesPropertyCreatesImagesOnDemand;

    /// After a DPI change the check images are created in the new size, but
    /// only for trees that use them.
    [Test]
    procedure CheckImagesAreScaledAfterDpiChange;
  end;

implementation

uses
  Winapi.Windows,
  System.Types,
  System.Rtti,
  Vcl.Controls,
  Vcl.Themes,
  VirtualTrees.BaseTree;

const
  cNodeCount = 3;

/// Returns the private field FCheckImages of the tree, without creating the
/// check images like the CheckImages property does.
function CheckImagesField(aTree: TBaseVirtualTree): TObject;
var
  lContext: TRttiContext;
  lField: TRttiField;
begin
  lField := lContext.GetType(TBaseVirtualTree).GetField('FCheckImages');
  Assert.IsNotNull(lField, 'Field TBaseVirtualTree.FCheckImages not found');
  Result := lField.GetValue(aTree).AsObject;
end;

function GdiObjectCount: Cardinal;
begin
  FreeMemoryContexts; // Release cached device contexts of bitmaps, they would distort the count
  Result := GetGuiResources(GetCurrentProcess, GR_GDIOBJECTS);
end;

procedure TVTCheckImagesIssue1082Tests.Setup;
begin
  fForm := TForm.Create(nil);
  fForm.SetBounds(0, 0, 600, 400);
  fForm.Show;
  Application.ProcessMessages;
end;

procedure TVTCheckImagesIssue1082Tests.TearDown;
begin
  FreeAndNil(fForm);
end;

function TVTCheckImagesIssue1082Tests.CreateTree(aCheckSupport: Boolean): TVirtualStringTree;
var
  I: Integer;
begin
  Result := TVirtualStringTree.Create(fForm);
  // Place the trees side by side, a tree covered by another one would not be painted by Repaint()
  Result.SetBounds(10 + (fForm.ControlCount mod 5) * 110, 10 + (fForm.ControlCount div 5) * 130, 100, 120);
  Result.TreeOptions.MiscOptions := Result.TreeOptions.MiscOptions - [toCheckSupport];
  Result.Parent := fForm;
  for I := 1 to cNodeCount do
    Result.AddChild(nil);
  if aCheckSupport then
    EnableCheckBoxes(Result);
  Result.HandleNeeded;
  Result.Repaint;
end;

procedure TVTCheckImagesIssue1082Tests.EnableCheckBoxes(aTree: TVirtualStringTree);
var
  lNode: PVirtualNode;
begin
  aTree.TreeOptions.MiscOptions := aTree.TreeOptions.MiscOptions + [toCheckSupport];
  for lNode in aTree.Nodes do
    aTree.CheckType[lNode] := ctCheckBox;
end;

function TVTCheckImagesIssue1082Tests.Render(aTree: TVirtualStringTree): TBytes;
var
  lBitmap: Vcl.Graphics.TBitmap;
  Y, lRowSize: Integer;
begin
  lBitmap := Vcl.Graphics.TBitmap.Create;
  try
    lBitmap.PixelFormat := pf32bit;
    lBitmap.SetSize(aTree.ClientWidth, aTree.ClientHeight);
    aTree.PaintTree(lBitmap.Canvas, Rect(0, 0, lBitmap.Width, lBitmap.Height), Point(0, 0), [poBackground, poColumnColor], pf32bit);
    lRowSize := lBitmap.Width * SizeOf(TRGBQuad);
    SetLength(Result, lRowSize * lBitmap.Height);
    for Y := 0 to lBitmap.Height - 1 do
      Move(lBitmap.ScanLine[Y]^, Result[Y * lRowSize], lRowSize);
  finally
    lBitmap.Free;
  end;
end;

function TVTCheckImagesIssue1082Tests.CheckBoxHit(aTree: TVirtualStringTree): Boolean;
var
  lRect: TRect;
  lHitInfo: THitInfo;
  X: Integer;
begin
  lRect := aTree.GetDisplayRect(aTree.GetFirst, NoColumn, False);
  for X := lRect.Left to lRect.Right - 1 do
  begin
    aTree.GetHitTestInfoAt(X, lRect.CenterPoint.Y, True, lHitInfo);
    if hiOnItemCheckbox in lHitInfo.HitPositions then
      Exit(True);
  end;
  Result := False;
end;

procedure TVTCheckImagesIssue1082Tests.AssertCheckBoxPainted(aTree: TVirtualStringTree);
var
  lUnchecked, lChecked: TBytes;
begin
  aTree.CheckState[aTree.GetFirst] := csUncheckedNormal;
  lUnchecked := Render(aTree);
  aTree.CheckState[aTree.GetFirst] := csCheckedNormal;
  lChecked := Render(aTree);
  Assert.IsTrue((Length(lChecked) > 0) and (Length(lChecked) = Length(lUnchecked)), 'Unexpected rendering size');
  Assert.IsFalse(CompareMem(@lChecked[0], @lUnchecked[0], Length(lChecked)),
    'Checking the node does not change the rendering, so no check box is painted');
end;

procedure TVTCheckImagesIssue1082Tests.NoCheckImagesWithoutCheckSupport;
var
  lTree: TVirtualStringTree;
begin
  lTree := CreateTree(False);
  Assert.IsTrue(lTree.HandleAllocated, 'The window handle of the tree must exist for this test');
  Render(lTree);
  Assert.IsFalse(CheckBoxHit(lTree), 'No check box must be hit without toCheckSupport');
  Assert.IsNull(CheckImagesField(lTree), 'The check images must not be created without toCheckSupport');
end;

procedure TVTCheckImagesIssue1082Tests.NoGdiHandlesForCheckImagesWithoutCheckSupport;
const
  cTreeCount = 10;
var
  lTrees: array[0..cTreeCount - 1] of TVirtualStringTree;
  lWithoutChecks, lWithChecks: Cardinal;
  I: Integer;
begin
  for I := 0 to cTreeCount - 1 do
    lTrees[I] := CreateTree(False);
  lWithoutChecks := GdiObjectCount;

  for I := 0 to cTreeCount - 1 do
  begin
    Assert.IsNull(CheckImagesField(lTrees[I]), 'The check images must not be created without toCheckSupport');
    EnableCheckBoxes(lTrees[I]);
    lTrees[I].Repaint;
    Assert.IsNotNull(CheckImagesField(lTrees[I]), 'The check images must be created when check boxes are painted');
  end;
  lWithChecks := GdiObjectCount;

  // Each image list with the system check images uses an image and a mask bitmap and their device contexts. If the
  // image lists were created with the window like before, they would not add any handles here.
  Assert.IsTrue(lWithChecks >= lWithoutChecks + 2 * cTreeCount,
    Format('Switching on check support for %d trees added only %d GDI handles, so the check images were not created on demand',
           [cTreeCount, Integer(lWithChecks) - Integer(lWithoutChecks)]));
end;

procedure TVTCheckImagesIssue1082Tests.CheckBoxesWorkWithCheckSupport;
var
  lTree: TVirtualStringTree;
begin
  lTree := CreateTree(True);
  Assert.IsNotNull(CheckImagesField(lTree), 'The check images must be created when check boxes are painted');
  Assert.IsTrue(CheckBoxHit(lTree), 'The check box of the node is not hit');
  AssertCheckBoxPainted(lTree);
end;

procedure TVTCheckImagesIssue1082Tests.CheckBoxesWorkWhenCheckSupportSwitchedOnLater;
var
  lTree: TVirtualStringTree;
  lWithoutCheckBoxes, lWithCheckBoxes: TBytes;
begin
  lTree := CreateTree(False);
  lWithoutCheckBoxes := Render(lTree);
  Assert.IsNull(CheckImagesField(lTree), 'The check images must not be created without toCheckSupport');

  EnableCheckBoxes(lTree);
  Assert.IsTrue(CheckBoxHit(lTree), 'The check box of the node is not hit after switching on toCheckSupport');
  Assert.IsNotNull(CheckImagesField(lTree), 'The check images must be created when check boxes are used');
  AssertCheckBoxPainted(lTree);
  lWithCheckBoxes := Render(lTree);
  Assert.IsFalse(CompareMem(@lWithoutCheckBoxes[0], @lWithCheckBoxes[0], Length(lWithoutCheckBoxes)),
    'The rendering did not change after switching on toCheckSupport');
end;

procedure TVTCheckImagesIssue1082Tests.CheckImagesPropertyCreatesImagesOnDemand;
var
  lTree: TVirtualStringTree;
begin
  // Without a window handle, the check images cannot be created, like before.
  lTree := TVirtualStringTree.Create(fForm);
  Assert.IsNull(lTree.CheckImages, 'CheckImages must be nil while there is no window handle');

  lTree.Free;
  lTree := CreateTree(False);
  Assert.IsNull(CheckImagesField(lTree), 'The check images must not be created without toCheckSupport');
  Assert.IsNotNull(lTree.CheckImages, 'CheckImages must create the system check images on demand');
  Assert.IsTrue(lTree.CheckImages.Width > 0, 'The check images must not be empty');
  Assert.IsTrue(lTree.CheckImages.Count > 0, 'The check images must not be empty');
end;

procedure TVTCheckImagesIssue1082Tests.CheckImagesAreScaledAfterDpiChange;
var
  lWithChecks, lWithoutChecks: TVirtualStringTree;
  lOldWidth: Integer;
begin
  lWithChecks := CreateTree(True);
  lWithoutChecks := CreateTree(False);
  lOldWidth := lWithChecks.CheckImages.Width;

  lWithChecks.ScaleForPPI(lWithChecks.CurrentPPI * 2);
  lWithoutChecks.ScaleForPPI(lWithoutChecks.CurrentPPI * 2);
  lWithChecks.Repaint;
  lWithoutChecks.Repaint;

  Assert.IsNull(CheckImagesField(lWithoutChecks), 'A DPI change must not create the check images without toCheckSupport');
  Assert.IsNotNull(CheckImagesField(lWithChecks), 'The check images must be created again after a DPI change');
  // The size of the system check images follows the PPI only if they are taken from the theme, see CreateSystemImageSet()
  if StyleServices(lWithChecks).Enabled and StyleServices(lWithChecks).IsSystemStyle then
    Assert.IsTrue(lWithChecks.CheckImages.Width > lOldWidth,
      Format('The check images were not scaled: width %d before and %d after the DPI change', [lOldWidth, lWithChecks.CheckImages.Width]));
  Assert.IsTrue(CheckBoxHit(lWithChecks), 'The check box of the node is not hit after the DPI change');
  AssertCheckBoxPainted(lWithChecks);
end;

initialization
  TDUnitX.RegisterTestFixture(TVTCheckImagesIssue1082Tests);

end.
