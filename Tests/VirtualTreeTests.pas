unit VirtualTreeTests;

interface

uses
  DUnitX.TestFramework,
  WinApi.Windows,
  VirtualTrees,
  VirtualTrees.Utils, Vcl.Graphics;

type

  [TestFixture]
  TVirtualTreeUtilsTests = class(TObject)
  strict private
    fBitmap: TBitmap;
    function GetHDC(): HDC;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    /// Test for helper function VirtualTrees.Utils.OrderRect()
    [Test]
    procedure TestOrderRect;
    /// Test for helper function VirtualTrees.Utils.ShortenString()
    /// <param name="pLongString">The string that should be shortened.</param>
    /// <param name="pWidth">The width in pixels that is availbale, based on 96dpi == 100% dpi scaling.</param>
    /// <param name="pShortString">The expected shortened string.</param>
    [Test]
    [TestCase('Test1', 'Abc,20,A...')]
    [TestCase('Test2', 'Abc,100,Abc')]
    [TestCase('Test3', 'Abc,10,')]
    [TestCase('Test4', 'A,100,A')]
    [TestCase('Test5', 'ii,16,ii')]
    procedure TestShortenString(const pLongString : string; const pWidth: Integer; const pShortString: string);
  end;



implementation

uses
  System.Types,
  System.SysUtils;

type
  TRectHelper = record helper for TRect
    function ToString(): string;
  end;

function TRectHelper.ToString: string;
begin
  Result := Format('(%d,%d,%d,%d)', [Left, Top, Right, Bottom]);
end;


{ TVirtualTreeUtilsTests }

function TVirtualTreeUtilsTests.GetHDC: HDC;
begin
  Exit(fBitmap.Canvas.Handle);
end;

procedure TVirtualTreeUtilsTests.Setup;
begin
  fBitmap := TBitmap.Create;
  fBitmap.Canvas.Font.Name := 'Tahoma';
  fBitmap.Canvas.Font.Size := 8;
  Assert.AreEqual(fBitmap.Canvas.Font.PixelsPerInch, 96, 'PixelsPerInch of font does not have th expected value of 96 dpi')
end;

procedure TVirtualTreeUtilsTests.TearDown;
begin
  FreeAndNil(fBitmap);
end;

procedure TVirtualTreeUtilsTests.TestOrderRect;
var
  lRectUnordered: TRect;
  lRectOrderedExpected: TRect;
  lRectOrdered: TRect;
begin
  lRectUnordered := Rect(1,2,3,4);
  lRectOrderedExpected := lRectUnordered;
  lRectOrdered := OrderRect(lRectUnordered);
  Assert.AreEqual<TRect>(lRectOrderedExpected, lRectOrdered, lRectUnordered.ToString + ' should be ordered to ' + lRectOrderedExpected.ToString + ' but was ' + lRectOrdered.ToString);
  lRectUnordered := Rect(4,3,2,1);
  lRectOrderedExpected := Rect(2,1,4,3);
  lRectOrdered := OrderRect(lRectUnordered);
  Assert.AreEqual<TRect>(lRectOrderedExpected, lRectOrdered, lRectUnordered.ToString + ' should be ordered to ' + lRectOrderedExpected.ToString + ' but was ' + lRectOrdered.ToString);
end;

procedure TVirtualTreeUtilsTests.TestShortenString(const pLongString : string; const pWidth: Integer; const pShortString: string);
var
  lShortenedString: string;
  lShortenedWidth: Integer;
begin
  lShortenedString := ShortenString(GetHDC, pLongString, pWidth);
  lShortenedWidth := fBitmap.Canvas.TextWidth(lShortenedString);
  Assert.IsTrue(lShortenedWidth <= pWidth, Format('The shortened string "%s" has a width of %d and so does not fit into the requested %d pixles.', [lShortenedString, lShortenedWidth, pWidth]));
  Assert.AreEqual(lShortenedString, pShortString);
end;

initialization
  TDUnitX.RegisterTestFixture(TVirtualTreeUtilsTests);
end.
