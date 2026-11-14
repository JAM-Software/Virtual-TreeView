unit VTSortGlyphAboveCaptionIssue1242Tests;

// Tests for the header option hoSortGlyphAboveCaption (issue #1242).
//
// Newer versions of Windows Explorer draw the sort glyph horizontally centered above the column
// caption instead of next to it, so the caption can use the full column width. With the opt-in
// header option hoSortGlyphAboveCaption the tree does the same: the sort glyph is centered on
// the column at the top of the header, regardless of the caption alignment and the bidi mode,
// and the caption width no longer includes the sort glyph.

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.Types,
  Vcl.Forms,
  Vcl.Graphics,
  VirtualTrees;

type
  [TestFixture]
  TVTSortGlyphAboveCaptionTests = class
  strict private
    fForm: TForm;
    fTree: TVirtualStringTree;
    /// Renders the header offscreen and returns the bounding box of the classic sort arrow
    /// (painted in clDkGray) within the first column. Returns an empty rect if none was found.
    function SortGlyphBounds: TRect;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// The caption width must not include the sort glyph if it is drawn above the caption.
    [Test]
    procedure CaptionWidthExcludesSortGlyphAboveCaption;

    /// Above the caption, the sort glyph is horizontally centered on the column and placed at the
    /// top of the header, for every caption alignment and bidi mode.
    [Test]
    [TestCase('LeftLTR',   '0,False')]
    [TestCase('CenterLTR', '2,False')]
    [TestCase('RightLTR',  '1,False')]
    [TestCase('LeftRTL',   '0,True')]
    [TestCase('RightRTL',  '1,True')]
    procedure SortGlyphIsCenteredAboveCaption(AAlignment: Integer; ARightToLeft: Boolean);

    /// Without the option the sort glyph stays next to the caption and vertically centered.
    [Test]
    procedure DefaultPlacementIsBesideCaption;
  end;

implementation

uses
  System.SysUtils,
  System.Math,
  VirtualTrees.Types,
  VirtualTrees.BaseTree;

type
  TTreeCracker = class(TBaseVirtualTree); // for DoStateChange

const
  ColumnWidth = 200;
  CaptionText = 'Caption';

procedure TVTSortGlyphAboveCaptionTests.Setup;
var
  I: Integer;
begin
  fForm := TForm.Create(nil);
  fForm.SetBounds(0, 0, 520, 300);
  fTree := TVirtualStringTree.Create(fForm);
  fTree.Parent := fForm;
  fTree.SetBounds(10, 10, 480, 240);
  for I := 0 to 1 do
  begin
    fTree.Header.Columns.Add.Width := ColumnWidth;
    fTree.Header.Columns[I].Text := CaptionText;
  end;
  fTree.Header.Options := fTree.Header.Options + [hoVisible, hoShowSortGlyphs];
  // Red text cannot produce the clDkGray pixels of the sort arrow, not even by anti-aliasing.
  fTree.Header.Font.Color := clRed;
  fTree.Header.SortColumn := 0;
  fForm.Show;
  Application.ProcessMessages;

  // Force the classic (non-themed) paint path, which draws the sort arrow in clDkGray.
  TTreeCracker(fTree).DoStateChange([], [tsUseThemes, tsUseExplorerTheme]);
end;

procedure TVTSortGlyphAboveCaptionTests.TearDown;
begin
  FreeAndNil(fForm);
end;

function TVTSortGlyphAboveCaptionTests.SortGlyphBounds: TRect;
var
  Bmp: Vcl.Graphics.TBitmap;
  X, Y: Integer;
  Found: Boolean;
begin
  Result := Rect(0, 0, 0, 0);
  Found := False;
  Bmp := Vcl.Graphics.TBitmap.Create;
  try
    Bmp.PixelFormat := pf24bit;
    Bmp.SetSize(fTree.ClientWidth, fTree.Header.Height);
    fTree.Header.Columns.PaintHeader(Bmp.Canvas, Rect(0, 0, Bmp.Width, Bmp.Height), Point(0, 0));

    for Y := 0 to Bmp.Height - 1 do
      for X := 0 to ColumnWidth - 1 do
        if Bmp.Canvas.Pixels[X, Y] = ColorToRGB(clDkGray) then
        begin
          if not Found then
            Result := Rect(X, Y, X + 1, Y + 1)
          else
          begin
            Result.Left := Min(Result.Left, X);
            Result.Top := Min(Result.Top, Y);
            Result.Right := Max(Result.Right, X + 1);
            Result.Bottom := Max(Result.Bottom, Y + 1);
          end;
          Found := True;
        end;
  finally
    Bmp.Free;
  end;
end;

procedure TVTSortGlyphAboveCaptionTests.CaptionWidthExcludesSortGlyphAboveCaption;
var
  UnsortedWidth, BesideWidth, AboveWidth: TDimension;
begin
  // Column 1 has the same caption but is not the sort column.
  UnsortedWidth := fTree.Header.Columns[1].CaptionWidth;
  BesideWidth := fTree.Header.Columns[0].CaptionWidth;
  Assert.IsTrue(BesideWidth > UnsortedWidth, 'Sanity: next to the caption the sort glyph adds to the caption width.');

  fTree.Header.Options := fTree.Header.Options + [hoSortGlyphAboveCaption];
  AboveWidth := fTree.Header.Columns[0].CaptionWidth;
  Assert.AreEqual(UnsortedWidth, AboveWidth, 'Above the caption the sort glyph must not add to the caption width.');
end;

procedure TVTSortGlyphAboveCaptionTests.SortGlyphIsCenteredAboveCaption(AAlignment: Integer; ARightToLeft: Boolean);
var
  Bounds: TRect;
  GlyphCenter2: Integer;
begin
  fTree.Header.Options := fTree.Header.Options + [hoSortGlyphAboveCaption];
  fTree.Header.Columns[0].CaptionAlignment := TAlignment(AAlignment);
  if ARightToLeft then
    fTree.Header.Columns[0].BiDiMode := bdRightToLeft;

  Bounds := SortGlyphBounds;
  Assert.IsFalse(Bounds.IsEmpty, 'The sort glyph must be painted.');

  // Compare doubled coordinates to avoid rounding issues with odd widths.
  GlyphCenter2 := Bounds.Left + Bounds.Right;
  Assert.IsTrue(Abs(GlyphCenter2 - ColumnWidth) <= 2,
    Format('The sort glyph must be horizontally centered on the column (glyph %d..%d, column width %d).',
    [Bounds.Left, Bounds.Right, ColumnWidth]));
  Assert.IsTrue(Bounds.Top < fTree.Header.Height div 3,
    Format('The sort glyph must be at the top of the header (glyph top %d, header height %d).',
    [Bounds.Top, fTree.Header.Height]));
end;

procedure TVTSortGlyphAboveCaptionTests.DefaultPlacementIsBesideCaption;
var
  Bounds: TRect;
begin
  Bounds := SortGlyphBounds;
  Assert.IsFalse(Bounds.IsEmpty, 'The sort glyph must be painted.');
  // A short left aligned caption leaves the sort glyph in the left half of the column.
  Assert.IsTrue(Bounds.Right < ColumnWidth div 2,
    Format('Without hoSortGlyphAboveCaption the sort glyph must stay next to the caption (glyph %d..%d).',
    [Bounds.Left, Bounds.Right]));
  Assert.IsTrue(Abs((Bounds.Top + Bounds.Bottom) - fTree.Header.Height) <= 3,
    Format('Without hoSortGlyphAboveCaption the sort glyph must be vertically centered (glyph %d..%d, header height %d).',
    [Bounds.Top, Bounds.Bottom, fTree.Header.Height]));
end;

initialization
  TDUnitX.RegisterTestFixture(TVTSortGlyphAboveCaptionTests);

end.
