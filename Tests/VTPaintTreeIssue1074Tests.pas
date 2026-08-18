unit VTPaintTreeIssue1074Tests;

// Regression test for issue #1074 "problem with painting nodes PaintTree with
// poUnbuffered in options and setMapMode...".
//
// SetCanvasOrigin() transformed its shift through LPtoDP before passing it to
// SetWindowOrgEx. SetWindowOrgEx however expects logical units - the same units
// the tree calculates with - so on a canvas with a mapping mode the shift got
// scaled twice and every node was drawn at twice its offset. With the default
// MM_TEXT mapping the transformation was a no-op, which is why the ordinary
// paint paths never showed the problem.
//
// The test renders offscreen with a 2x MM_ANISOTROPIC mapping and compares the
// horizontal grid line positions: the unbuffered rendering must place them
// exactly like the buffered one, and exactly at twice the unmapped positions.

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
  TVTPaintTreeIssue1074Tests = class
  strict private
    fForm: TForm;
    fTree: TVirtualStringTree;
    /// Renders via PaintTree and returns the Y positions of the horizontal grid lines.
    function RenderHLines(Mapped, Unbuffered: Boolean): TArray<Integer>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// Under a mapping mode the unbuffered rendering must match the buffered one.
    [Test]
    procedure UnbufferedMatchesBufferedUnderMapMode;

    /// And both must be the unmapped rendering scaled by the mapping factor.
    [Test]
    procedure MappedRenderingIsScaledUnmappedRendering;
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  VirtualTrees.Types;

const
  LineColor = clRed;
  MapScale = 2;

procedure TVTPaintTreeIssue1074Tests.Setup;
var
  Root, Child: PVirtualNode;
begin
  fForm := TForm.Create(nil);
  fForm.SetBounds(0, 0, 420, 300);
  fTree := TVirtualStringTree.Create(fForm);
  fTree.Parent := fForm;
  fTree.SetBounds(10, 10, 380, 240);
  fTree.Colors.GridLineColor := LineColor;
  fTree.Colors.TreeLineColor := LineColor;
  fTree.TreeOptions.PaintOptions := fTree.TreeOptions.PaintOptions
    + [toShowRoot, toShowHorzGridLines, toShowVertGridLines];

  Root := fTree.AddChild(nil);
  Child := fTree.AddChild(Root);
  fTree.AddChild(Child);
  fTree.AddChild(Root);
  fTree.AddChild(nil);
  fTree.FullExpand;
  fForm.Show;
  Application.ProcessMessages;
end;

procedure TVTPaintTreeIssue1074Tests.TearDown;
begin
  FreeAndNil(fForm);
end;

function TVTPaintTreeIssue1074Tests.RenderHLines(Mapped, Unbuffered: Boolean): TArray<Integer>;
var
  Bmp: Vcl.Graphics.TBitmap;
  Options: TVTInternalPaintOptions;
  X, Y, RedCount: Integer;
  Pixel: TColor;
begin
  Result := nil;
  Options := [poBackground, poColumnColor, poGridLines];
  if Unbuffered then
    Include(Options, poUnbuffered);

  Bmp := Vcl.Graphics.TBitmap.Create;
  try
    Bmp.PixelFormat := pf24bit;
    Bmp.SetSize(500, 300);
    Bmp.Canvas.Brush.Color := clWhite;
    Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));

    if Mapped then
    begin
      SetMapMode(Bmp.Canvas.Handle, MM_ANISOTROPIC);
      SetWindowExtEx(Bmp.Canvas.Handle, 1, 1, nil);
      SetViewportExtEx(Bmp.Canvas.Handle, MapScale, MapScale, nil);
    end;
    try
      fTree.PaintTree(Bmp.Canvas, Rect(0, 0, Bmp.Width, 130), Point(0, 0), Options, pfDevice);
    finally
      if Mapped then
        SetMapMode(Bmp.Canvas.Handle, MM_TEXT);
    end;

    // A horizontal grid line is a row that is red almost across the whole width.
    for Y := 0 to Bmp.Height - 1 do
    begin
      RedCount := 0;
      for X := 0 to Bmp.Width - 1 do
      begin
        Pixel := Bmp.Canvas.Pixels[X, Y];
        if (GetRValue(Pixel) > 200) and (GetGValue(Pixel) < 80) and (GetBValue(Pixel) < 80) then
          Inc(RedCount);
      end;
      if RedCount > Bmp.Width div 2 then
        Result := Result + [Y];
    end;
  finally
    Bmp.Free;
  end;
end;

function RowList(const Rows: TArray<Integer>): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(Rows) do
  begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + IntToStr(Rows[I]);
  end;
end;

function ContainsRow(const Rows: TArray<Integer>; Row: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(Rows) do
    if Rows[I] = Row then
      Exit(True);
  Result := False;
end;

procedure TVTPaintTreeIssue1074Tests.UnbufferedMatchesBufferedUnderMapMode;
var
  Buffered, Unbuffered: TArray<Integer>;
begin
  Buffered := RenderHLines(True, False);
  Unbuffered := RenderHLines(True, True);
  Assert.IsTrue(Length(Buffered) > 0, 'Sanity: grid lines expected in the buffered rendering.');
  Assert.AreEqual(RowList(Buffered), RowList(Unbuffered),
    'Unbuffered painting must place the grid lines exactly like buffered painting under a mapping mode (issue #1074).');
end;

procedure TVTPaintTreeIssue1074Tests.MappedRenderingIsScaledUnmappedRendering;
var
  Plain, Mapped: TArray<Integer>;
  I: Integer;
begin
  Plain := RenderHLines(False, True);
  Mapped := RenderHLines(True, True);
  Assert.IsTrue(Length(Plain) > 0, 'Sanity: grid lines expected in the unmapped rendering.');
  for I := 0 to High(Plain) do
    Assert.IsTrue(ContainsRow(Mapped, Plain[I] * MapScale),
      Format('Grid line at %d must appear at %d under the %dx mapping (issue #1074), got [%s].',
      [Plain[I], Plain[I] * MapScale, MapScale, RowList(Mapped)]));
end;

initialization
  TDUnitX.RegisterTestFixture(TVTPaintTreeIssue1074Tests);

end.
