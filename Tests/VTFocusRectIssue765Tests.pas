unit VTFocusRectIssue765Tests;

// Regression test for issue #765 "FocusRect should extend to RowRect when
// toFullRowSelect, independent of tsUseExplorerTheme".
//
// Without the explorer theme the focus rect was drawn around InnerRect (or
// CellRect) only, although the selection covers the whole row. Additionally
// RowRect was only computed when the explorer theme was active.
//
// The test renders offscreen with toPopupMode (so no real window focus is
// needed), an empty cell text and no tree lines or buttons - every non-white
// pixel in the focused row is therefore part of the focus rectangle - and
// asserts that the dotted rectangle spans the whole row.

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
  TVTFocusRectIssue765Tests = class
  strict private
    fForm: TForm;
    fTree: TVirtualStringTree;
    procedure OnGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    /// Renders offscreen and returns count and horizontal span of the focus rect pixels
    /// in the focused (second) row.
    function RenderAndMeasure(out MinX, MaxX: Integer): Integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// Without columns the focus rect must span the whole client width.
    [Test]
    procedure FocusRectSpansRowWithoutColumns;

    /// With columns it must span all columns, not just the focused one.
    [Test]
    procedure FocusRectSpansRowAcrossColumns;
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  System.Math,
  VirtualTrees.Types;

const
  cNodeHeight = 18;

procedure TVTFocusRectIssue765Tests.Setup;
var
  I: Integer;
begin
  fForm := TForm.Create(nil);
  fForm.SetBounds(0, 0, 520, 420);
  fTree := TVirtualStringTree.Create(fForm);
  fTree.Parent := fForm;
  fTree.SetBounds(0, 0, 420, 320);
  fTree.BorderStyle := bsNone;
  fTree.DefaultNodeHeight := cNodeHeight;
  fTree.OnGetText := OnGetText;
  fTree.TreeOptions.SelectionOptions := fTree.TreeOptions.SelectionOptions + [toFullRowSelect];
  // toPopupMode draws the focus rect without real window focus; lines, buttons and
  // text are switched off so that only the focus rect produces non-white pixels.
  fTree.TreeOptions.PaintOptions := fTree.TreeOptions.PaintOptions + [toPopupMode]
    - [toShowTreeLines, toShowButtons, toShowRoot];
  for I := 1 to 3 do
    fTree.AddChild(nil);
  fTree.FocusedNode := fTree.GetNextSibling(fTree.GetFirst);
  fForm.Show;
  Application.ProcessMessages;
end;

procedure TVTFocusRectIssue765Tests.TearDown;
begin
  FreeAndNil(fForm);
end;

procedure TVTFocusRectIssue765Tests.OnGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
begin
  CellText := '';
end;

function TVTFocusRectIssue765Tests.RenderAndMeasure(out MinX, MaxX: Integer): Integer;
var
  Bmp: Vcl.Graphics.TBitmap;
  X, Y: Integer;
begin
  Result := 0;
  MinX := MaxInt;
  MaxX := -1;
  Bmp := Vcl.Graphics.TBitmap.Create;
  try
    Bmp.PixelFormat := pf24bit;
    Bmp.SetSize(fTree.ClientWidth, 120);
    Bmp.Canvas.Brush.Color := clWhite;
    Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));
    fTree.PaintTree(Bmp.Canvas, Rect(0, 0, Bmp.Width, Bmp.Height), Point(0, 0),
      [poBackground, poColumnColor, poDrawFocusRect, poDrawSelection]);

    for Y := cNodeHeight to 2 * cNodeHeight - 1 do
      for X := 0 to Bmp.Width - 1 do
        if Bmp.Canvas.Pixels[X, Y] <> clWhite then
        begin
          Inc(Result);
          MinX := Min(MinX, X);
          MaxX := Max(MaxX, X);
        end;
  finally
    Bmp.Free;
  end;
end;

procedure TVTFocusRectIssue765Tests.FocusRectSpansRowWithoutColumns;
var
  Count, MinX, MaxX: Integer;
begin
  Count := RenderAndMeasure(MinX, MaxX);
  Assert.IsTrue(Count > 0, 'Sanity: focus rect pixels expected in the focused row.');
  Assert.IsTrue(MinX <= 1, Format('Focus rect must start at the row''s left edge, starts at %d (issue #765).', [MinX]));
  Assert.IsTrue(MaxX >= fTree.ClientWidth - 2,
    Format('Focus rect must extend to the row''s right edge (>= %d), ends at %d (issue #765).',
    [fTree.ClientWidth - 2, MaxX]));
end;

procedure TVTFocusRectIssue765Tests.FocusRectSpansRowAcrossColumns;
var
  Count, MinX, MaxX, I: Integer;
begin
  for I := 1 to 3 do
    with fTree.Header.Columns.Add do
      Width := 120;
  fTree.Header.MainColumn := 0;
  fTree.FocusedColumn := 0;
  Application.ProcessMessages;

  Count := RenderAndMeasure(MinX, MaxX);
  Assert.IsTrue(Count > 0, 'Sanity: focus rect pixels expected in the focused row.');
  Assert.IsTrue(MinX <= 1, Format('Focus rect must start at the row''s left edge, starts at %d (issue #765).', [MinX]));
  Assert.IsTrue(MaxX >= 3 * 120 - 2,
    Format('Focus rect must span all three columns (>= %d), ends at %d (issue #765).', [3 * 120 - 2, MaxX]));
end;

initialization
  TDUnitX.RegisterTestFixture(TVTFocusRectIssue765Tests);

end.
