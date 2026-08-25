unit VTHeaderBackgroundTests;

// Regression test for the classic (non-themed) header paint path ignoring Header.Background
// inside the column cells.
//
// DrawBackground fills the area right of the last column with Header.Background, but
// PaintColumnHeader painted the cells themselves via DrawEdge with BF_MIDDLE, which always
// fills with clBtnFace - so a custom Header.Background only ever showed up in the filler
// area. The fix fills the cell interior explicitly with Header.Background before drawing
// the edges, which is pixel-identical for the default clBtnFace.

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
  TVTHeaderBackgroundTests = class
  strict private
    fForm: TForm;
    fTree: TVirtualStringTree;
    /// Renders the header offscreen and counts the pixels of the given color in the
    /// horizontal range [FromX, ToX) of the header band (edges excluded).
    function CountHeaderPixels(Color: TColor; FromX, ToX: Integer): Integer;
    function BandArea(FromX, ToX: Integer): Integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// In classic mode the column cells must be filled with Header.Background.
    [Test]
    procedure ClassicCellsUseHeaderBackground;
    /// With the default Header.Background the classic cells keep the clBtnFace look.
    [Test]
    procedure DefaultRenderingKeepsButtonFace;
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
  BackColor = clRed;
  ColumnWidth = 120;
  ColumnCount = 2;

procedure TVTHeaderBackgroundTests.Setup;
var
  I: Integer;
begin
  fForm := TForm.Create(nil);
  fForm.SetBounds(0, 0, 420, 300);
  fTree := TVirtualStringTree.Create(fForm);
  fTree.Parent := fForm;
  fTree.SetBounds(10, 10, 380, 240);
  for I := 1 to ColumnCount do
    fTree.Header.Columns.Add.Width := ColumnWidth; // no captions: no text pixels in the band
  fTree.Header.Options := fTree.Header.Options + [hoVisible];
  fForm.Show;
  Application.ProcessMessages;

  // Force the classic (non-themed) paint path regardless of the OS theme state.
  TTreeCracker(fTree).DoStateChange([], [tsUseThemes]);
end;

procedure TVTHeaderBackgroundTests.TearDown;
begin
  FreeAndNil(fForm);
end;

function TVTHeaderBackgroundTests.CountHeaderPixels(Color: TColor; FromX, ToX: Integer): Integer;
var
  Bmp: Vcl.Graphics.TBitmap;
  X, Y: Integer;
begin
  Result := 0;
  Bmp := Vcl.Graphics.TBitmap.Create;
  try
    Bmp.PixelFormat := pf24bit;
    Bmp.SetSize(fTree.ClientWidth, fTree.Header.Height);
    Bmp.Canvas.Brush.Color := clFuchsia; // neutral ground, clashes with nothing under test
    Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));

    fTree.Header.Columns.PaintHeader(Bmp.Canvas, Rect(0, 0, Bmp.Width, Bmp.Height), Point(0, 0));

    for Y := 2 to Bmp.Height - 3 do // skip the top/bottom bevel rows
      for X := FromX to ToX - 1 do
        if Bmp.Canvas.Pixels[X, Y] = ColorToRGB(Color) then
          Inc(Result);
  finally
    Bmp.Free;
  end;
end;

function TVTHeaderBackgroundTests.BandArea(FromX, ToX: Integer): Integer;
begin
  Result := (fTree.Header.Height - 4) * (ToX - FromX);
end;

procedure TVTHeaderBackgroundTests.ClassicCellsUseHeaderBackground;
var
  CellPixels, FillerPixels: Integer;
begin
  fTree.Header.Background := BackColor;

  // Sanity: the filler area right of the last column already honored Header.Background.
  FillerPixels := CountHeaderPixels(BackColor, ColumnCount * ColumnWidth + 2, fTree.ClientWidth - 2);
  Assert.IsTrue(FillerPixels > BandArea(ColumnCount * ColumnWidth + 2, fTree.ClientWidth - 2) div 2,
    'Sanity: the filler area right of the columns must use Header.Background.');

  // The actual regression: the cells themselves must be filled with it too.
  CellPixels := CountHeaderPixels(BackColor, 2, ColumnCount * ColumnWidth - 2);
  Assert.IsTrue(CellPixels > (BandArea(2, ColumnCount * ColumnWidth - 2) * 6) div 10,
    Format('Classic column cells must use Header.Background (%d of %d band pixels found).',
    [CellPixels, BandArea(2, ColumnCount * ColumnWidth - 2)]));
end;

procedure TVTHeaderBackgroundTests.DefaultRenderingKeepsButtonFace;
var
  CellPixels: Integer;
begin
  // Header.Background defaults to clBtnFace - the classic look must not change.
  CellPixels := CountHeaderPixels(clBtnFace, 2, ColumnCount * ColumnWidth - 2);
  Assert.IsTrue(CellPixels > (BandArea(2, ColumnCount * ColumnWidth - 2) * 6) div 10,
    Format('Default classic cells must keep the clBtnFace fill (%d band pixels found).',
    [CellPixels]));
end;

initialization
  TDUnitX.RegisterTestFixture(TVTHeaderBackgroundTests);

end.
