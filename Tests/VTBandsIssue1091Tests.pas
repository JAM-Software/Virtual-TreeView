unit VTBandsIssue1091Tests;

// Regression test for issue #1091 "Drawing issue when LineMode = lmBands".
//
// With the explorer theme active, DetermineLineImageAndSelectLevel() suppresses the
// tree line under the expand button by overwriting the LAST entry of the line image
// with ltNone. The band conversion in PaintTreeLines() however documents and relies
// on the invariant that ltNone never appears as the last entry: on ltNone it copies
// the style of the entry to the RIGHT, which for the last entry is an out-of-bounds
// read of the dynamic array. The garbage styles made the bands of every collapsed
// node with children disappear.
//
// The fix skips that suppression in band mode - bands are box edges, not lines
// pointing at a button. Hence the assertion: with buttons hidden, the band rendering
// must be pixel-identical whether the explorer theme state is set or not.

interface

uses
  DUnitX.TestFramework,
  Classes,
  System.Types,
  Vcl.Forms,
  Vcl.Graphics,
  VirtualTrees;

type
  [TestFixture]
  TVTBandsIssue1091Tests = class
  strict private
    fForm: TForm;
    fTree: TVirtualStringTree;
    /// Renders the tree offscreen and returns the band/grid pixels as count plus checksum.
    function RenderBands(out Checksum: Cardinal): Integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// Bands must render identically with and without the explorer theme state.
    [Test]
    procedure ExplorerThemeStateDoesNotChangeBands;
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  VirtualTrees.Types,
  VirtualTrees.BaseTree;

type
  TTreeCracker = class(TBaseVirtualTree); // for DoStateChange

const
  BandColor = clRed;

procedure TVTBandsIssue1091Tests.Setup;
var
  Root0, Child0, Root1: PVirtualNode;
begin
  fForm := TForm.Create(nil);
  fForm.SetBounds(0, 0, 420, 300);
  fTree := TVirtualStringTree.Create(fForm);
  fTree.Parent := fForm;
  fTree.SetBounds(10, 10, 380, 240);
  fTree.LineMode := lmBands;
  fTree.LineStyle := lsSolid;
  fTree.Colors.GridLineColor := BandColor;
  // Buttons off: the plus/minus vs. chevron glyphs would differ between the two
  // renderings; the line suppression under test does not depend on them.
  fTree.TreeOptions.PaintOptions := fTree.TreeOptions.PaintOptions
    + [toShowTreeLines, toShowRoot, toShowHorzGridLines, toShowVertGridLines]
    - [toShowButtons];

  // The broken cases are collapsed nodes WITH children (their last line image entry
  // got overwritten): one at level 1 inside an expanded root, one at level 0.
  Root0 := fTree.AddChild(nil);
  Child0 := fTree.AddChild(Root0);
  fTree.AddChild(Child0);          // makes Child0 a collapsed parent
  fTree.AddChild(Root0);
  Root1 := fTree.AddChild(nil);
  fTree.AddChild(Root1);           // makes Root1 a collapsed parent
  fTree.AddChild(nil);
  fTree.Expanded[Root0] := True;

  fForm.Show;
  Application.ProcessMessages;
end;

procedure TVTBandsIssue1091Tests.TearDown;
begin
  FreeAndNil(fForm);
end;

function TVTBandsIssue1091Tests.RenderBands(out Checksum: Cardinal): Integer;
var
  Bmp: Vcl.Graphics.TBitmap;
  X, Y: Integer;
begin
  Result := 0;
  Checksum := 0;
  Bmp := Vcl.Graphics.TBitmap.Create;
  try
    Bmp.PixelFormat := pf24bit;
    Bmp.SetSize(fTree.ClientWidth, fTree.ClientHeight);
    Bmp.Canvas.Brush.Color := clWhite;
    Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));

    fTree.PaintTree(Bmp.Canvas, Rect(0, 0, Bmp.Width, Bmp.Height), Point(0, 0),
      [poBackground, poColumnColor, poGridLines]);

    for Y := 0 to Bmp.Height - 1 do
      for X := 0 to Bmp.Width - 1 do
        if Bmp.Canvas.Pixels[X, Y] = BandColor then
        begin
          Inc(Result);
          // Position-sensitive checksum so lines moving elsewhere cannot cancel out.
          Checksum := Checksum xor Cardinal(Y * 4096 + X);
        end;
  finally
    Bmp.Free;
  end;
end;

procedure TVTBandsIssue1091Tests.ExplorerThemeStateDoesNotChangeBands;
var
  PlainCount, ThemedCount: Integer;
  PlainSum, ThemedSum: Cardinal;
begin
  PlainCount := RenderBands(PlainSum);
  Assert.IsTrue(PlainCount > 0, 'Sanity: band lines expected in the reference rendering.');

  TTreeCracker(fTree).DoStateChange([tsUseExplorerTheme]);
  try
    ThemedCount := RenderBands(ThemedSum);
  finally
    TTreeCracker(fTree).DoStateChange([], [tsUseExplorerTheme]);
  end;

  Assert.AreEqual(PlainCount, ThemedCount,
    'Explorer theme state must not change the number of band line pixels (issue #1091).');
  Assert.AreEqual(PlainSum, ThemedSum,
    'Explorer theme state must not move band lines (issue #1091).');
end;

initialization
  TDUnitX.RegisterTestFixture(TVTBandsIssue1091Tests);

end.
