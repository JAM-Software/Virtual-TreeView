unit VTPaintToIssue632Tests;

// Regressionstest zu Issue #632 "Paint into device context via PrintTo or WM_PRINT".
//
// Der Header liegt im Nicht-Client-Bereich. Zwei Fehler machten das Rendern in einen
// fremden DC unbrauchbar:
//   1. WMPaint holte sich fuer den Header per GetDCEx einen Fenster-DC, statt den DC zu
//      benutzen, den die Nachricht mitbringt. Eine Kopie via TWinControl.PaintTo bekam
//      deshalb alles ausser dem Header - der landete stattdessen auf dem Bildschirm.
//   2. WMPrint zeichnete den Header ohne Ruecksicht auf die PRF_-Flags, also auch bei
//      einer reinen PRF_CLIENT-Anforderung. Dort gehoert er nicht hin.
//
// Gemessen wird offscreen ueber die Pixel einer auffaellig gefaerbten Kopfzeile, damit
// das Ergebnis nicht von Fenstersichtbarkeit oder Theme abhaengt.

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
  TVTPaintToIssue632Tests = class
  strict private
    fForm: TForm;
    fTree: TVirtualStringTree;
    /// Rendert in eine weisse Bitmap und liefert Anzahl und Bounding-Box der Header-Pixel.
    function RenderAndMeasure(UsePrint: Boolean; Flags: Cardinal; out Bounds: TRect): Integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// PaintTo muss den Header mitkopieren (das eigentliche Symptom des Issues).
    [Test]
    procedure PaintToIncludesHeader;

    /// und zwar an derselben Stelle wie WM_PRINT mit PRF_NONCLIENT.
    [Test]
    procedure PaintToPlacesHeaderLikeWmPrint;

    /// WM_PRINT ohne PRF_NONCLIENT darf den Header NICHT zeichnen.
    [Test]
    procedure WmPrintClientOnlyOmitsHeader;

    /// WM_PRINT mit PRF_NONCLIENT muss ihn zeichnen.
    [Test]
    procedure WmPrintNonClientIncludesHeader;
  end;

implementation

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  VirtualTrees.Types,
  VirtualTrees.Header;

const
  HeaderColor = clRed;

procedure TVTPaintToIssue632Tests.Setup;
begin
  fForm := TForm.Create(nil);
  fForm.SetBounds(0, 0, 420, 300);
  fTree := TVirtualStringTree.Create(fForm);
  fTree.Parent := fForm;
  fTree.SetBounds(10, 10, 380, 200);
  fTree.Header.Options := fTree.Header.Options + [hoVisible];
  fTree.Header.Background := HeaderColor;
  fTree.Header.Style := hsPlates; // ungethemed, damit die Farbe wirklich durchschlaegt
  if fTree.Header.Columns.Count = 0 then
    fTree.Header.Columns.Add.Width := 360;
  fTree.RootNodeCount := 5;
  fForm.Show;
  Application.ProcessMessages;
end;

procedure TVTPaintToIssue632Tests.TearDown;
begin
  FreeAndNil(fForm);
end;

function TVTPaintToIssue632Tests.RenderAndMeasure(UsePrint: Boolean; Flags: Cardinal;
  out Bounds: TRect): Integer;
var
  Bmp: Vcl.Graphics.TBitmap;
  X, Y: Integer;
begin
  Result := 0;
  Bounds := Rect(MaxInt, MaxInt, -1, -1);
  Bmp := Vcl.Graphics.TBitmap.Create;
  try
    Bmp.PixelFormat := pf24bit;
    Bmp.SetSize(fTree.Width, fTree.Height);
    Bmp.Canvas.Brush.Color := clWhite;
    Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));

    if UsePrint then
      fTree.Perform(WM_PRINT, WPARAM(Bmp.Canvas.Handle), LPARAM(Flags))
    else
      fTree.PaintTo(Bmp.Canvas.Handle, 0, 0);

    for Y := 0 to Bmp.Height - 1 do
      for X := 0 to Bmp.Width - 1 do
        if Bmp.Canvas.Pixels[X, Y] = HeaderColor then
        begin
          Inc(Result);
          if X < Bounds.Left then Bounds.Left := X;
          if Y < Bounds.Top then Bounds.Top := Y;
          if X > Bounds.Right then Bounds.Right := X;
          if Y > Bounds.Bottom then Bounds.Bottom := Y;
        end;
  finally
    Bmp.Free;
  end;
end;

procedure TVTPaintToIssue632Tests.PaintToIncludesHeader;
var
  Bounds: TRect;
begin
  Assert.IsTrue(RenderAndMeasure(False, 0, Bounds) > 0,
    'PaintTo hat den Header nicht mitkopiert');
end;

procedure TVTPaintToIssue632Tests.PaintToPlacesHeaderLikeWmPrint;
var
  PaintToBounds, PrintBounds: TRect;
  PaintToCount, PrintCount: Integer;
begin
  PaintToCount := RenderAndMeasure(False, 0, PaintToBounds);
  PrintCount := RenderAndMeasure(True, PRF_CLIENT or PRF_NONCLIENT, PrintBounds);

  Assert.AreEqual(PrintCount, PaintToCount, 'Anzahl der Header-Pixel weicht ab');
  Assert.AreEqual(PrintBounds.Left, PaintToBounds.Left, 'Header horizontal versetzt');
  Assert.AreEqual(PrintBounds.Top, PaintToBounds.Top, 'Header vertikal versetzt');
  Assert.AreEqual(PrintBounds.Right, PaintToBounds.Right, 'Header rechts beschnitten');
  Assert.AreEqual(PrintBounds.Bottom, PaintToBounds.Bottom, 'Header unten beschnitten');
end;

procedure TVTPaintToIssue632Tests.WmPrintClientOnlyOmitsHeader;
var
  Bounds: TRect;
begin
  Assert.AreEqual(0, RenderAndMeasure(True, PRF_CLIENT, Bounds),
    'PRF_CLIENT allein darf den Header des Nicht-Client-Bereichs nicht zeichnen');
end;

procedure TVTPaintToIssue632Tests.WmPrintNonClientIncludesHeader;
var
  Bounds: TRect;
begin
  Assert.IsTrue(RenderAndMeasure(True, PRF_CLIENT or PRF_NONCLIENT, Bounds) > 0,
    'PRF_NONCLIENT muss den Header zeichnen');
end;

initialization
  TDUnitX.RegisterTestFixture(TVTPaintToIssue632Tests);

end.
