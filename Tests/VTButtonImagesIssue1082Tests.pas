unit VTButtonImagesIssue1082Tests;

// Tests for issue #1082 "Reduce needed handles by putting expand / collapse images in an image list".
//
// Each tree used six TBitmaps for its expand / collapse buttons: plus, minus and their hot and
// selected hot variants. As every transparently painted TBitmap also creates a mask bitmap, the
// buttons needed up to 12 GDI handles per tree. Now the six images are copied side by side into
// one bitmap that is used for painting, which needs two GDI handles (bitmap and mask), and the six
// bitmaps release their GDI handles. They are kept for OnPrepareButtonBitmaps and the protected
// PlusBM etc. properties.
//
// The tests check that every button state paints its image out of the combined bitmap, that the
// six bitmaps hold no GDI handles but keep their image, and that painting the hot buttons does not
// allocate further GDI handles.

interface

uses
  DUnitX.TestFramework,
  System.Types,
  Vcl.Forms,
  Vcl.Graphics,
  VirtualTrees,
  VirtualTrees.Types;

type
  [TestFixture]
  TVTButtonImagesIssue1082Tests = class
  strict private
    fForm: TForm;
    fTree: TVirtualStringTree;
    procedure OnInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure OnInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure OnPrepareButtonBitmaps(Sender: TBaseVirtualTree; const APlusBM: TBitmap; const APlusHotBM: TBitmap;
      const APlusSelectedHotBM: TBitmap; const AMinusBM: TBitmap; const AMinusHotBM: TBitmap;
      const AMinusSelectedHotBM: TBitmap; var ASize: TSize);
    function ExpandedNode: PVirtualNode;
    function CollapsedNode: PVirtualNode;
    /// Sets up the selection and the hot state of the button of the given node.
    procedure SetButtonState(Node: PVirtualNode; Hot, Selected: Boolean);
    /// Paints the tree and returns the color in the center of the button of the given node.
    function PaintedButtonColor(Node: PVirtualNode): TColor;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// Every state of a button must paint its image out of the combined bitmap.
    [Test]
    [TestCase('Plus', 'False,False,False,Plus')]
    [TestCase('HotPlus', 'False,True,False,HotPlus')]
    [TestCase('SelectedHotPlus', 'False,True,True,SelectedHotPlus')]
    [TestCase('Minus', 'True,False,False,Minus')]
    [TestCase('HotMinus', 'True,True,False,HotMinus')]
    [TestCase('SelectedHotMinus', 'True,True,True,SelectedHotMinus')]
    procedure ButtonPaintsItsImage(Expanded, Hot, Selected: Boolean; const ImageName: string);

    /// The six button bitmaps must not hold GDI handles, but still provide their size and image.
    [Test]
    procedure ButtonBitmapsReleaseTheirGdiHandles;

    /// Painting the hot buttons must not allocate further GDI handles, e.g. masks for the hot bitmaps.
    [Test]
    procedure PaintingHotButtonsAllocatesNoGdiHandles;
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  System.Rtti;

type
  TBitmap = Vcl.Graphics.TBitmap; // Not the one of Winapi.Windows
  TVirtualStringTreeAccess = class(TVirtualStringTree);

const
  // Colors with which OnPrepareButtonBitmaps fills the six button bitmaps.
  cPlusColor = clRed;
  cHotPlusColor = clLime;
  cSelectedHotPlusColor = clBlue;
  cMinusColor = clYellow;
  cHotMinusColor = clAqua;
  cSelectedHotMinusColor = clMaroon;

function ImageColor(const ImageName: string): TColor;
begin
  if ImageName = 'Plus' then
    Result := cPlusColor
  else if ImageName = 'HotPlus' then
    Result := cHotPlusColor
  else if ImageName = 'SelectedHotPlus' then
    Result := cSelectedHotPlusColor
  else if ImageName = 'Minus' then
    Result := cMinusColor
  else if ImageName = 'HotMinus' then
    Result := cHotMinusColor
  else if ImageName = 'SelectedHotMinus' then
    Result := cSelectedHotMinusColor
  else
    raise EArgumentException.Create('Unknown button image: ' + ImageName);
end;

function GdiObjectCount: Cardinal;
begin
  FreeMemoryContexts(); // Releases the device contexts of all bitmap canvases, which are not kept
  Result := GetGuiResources(GetCurrentProcess, GR_GDIOBJECTS);
end;

procedure TVTButtonImagesIssue1082Tests.Setup;
begin
  fForm := TForm.CreateNew(nil);
  fForm.SetBounds(0, 0, 300, 300);
  fTree := TVirtualStringTree.Create(fForm);
  fTree.Parent := fForm;
  fTree.SetBounds(0, 0, 250, 250);
  fTree.OnInitNode := OnInitNode;
  fTree.OnInitChildren := OnInitChildren;
  fTree.RootNodeCount := 2;
  fForm.Show;
  Application.ProcessMessages;
  // Assigned after the window has been created, as it immediately prepares the button bitmaps.
  fTree.OnPrepareButtonBitmaps := OnPrepareButtonBitmaps;
end;

procedure TVTButtonImagesIssue1082Tests.TearDown;
begin
  FreeAndNil(fForm);
end;

procedure TVTButtonImagesIssue1082Tests.OnInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  if ParentNode = nil then
  begin
    Include(InitialStates, ivsHasChildren);
    if Node.Index = 0 then
      Include(InitialStates, ivsExpanded);
  end;
end;

procedure TVTButtonImagesIssue1082Tests.OnInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var ChildCount: Cardinal);
begin
  ChildCount := 2;
end;

procedure TVTButtonImagesIssue1082Tests.OnPrepareButtonBitmaps(Sender: TBaseVirtualTree; const APlusBM: TBitmap;
  const APlusHotBM: TBitmap; const APlusSelectedHotBM: TBitmap; const AMinusBM: TBitmap; const AMinusHotBM: TBitmap;
  const AMinusSelectedHotBM: TBitmap; var ASize: TSize);

  procedure Fill(Bitmap: TBitmap; Color: TColor);
  begin
    Bitmap.Canvas.Brush.Color := Color;
    Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));
  end;

begin
  Fill(APlusBM, cPlusColor);
  Fill(APlusHotBM, cHotPlusColor);
  Fill(APlusSelectedHotBM, cSelectedHotPlusColor);
  Fill(AMinusBM, cMinusColor);
  Fill(AMinusHotBM, cHotMinusColor);
  Fill(AMinusSelectedHotBM, cSelectedHotMinusColor);
end;

function TVTButtonImagesIssue1082Tests.ExpandedNode: PVirtualNode;
begin
  Result := fTree.GetFirst;
  Assert.IsTrue(fTree.Expanded[Result], 'Sanity: the first root node must be expanded.');
end;

function TVTButtonImagesIssue1082Tests.CollapsedNode: PVirtualNode;
begin
  Result := fTree.GetNextSibling(fTree.GetFirst);
  Assert.IsFalse(fTree.Expanded[Result], 'Sanity: the second root node must be collapsed.');
end;

procedure TVTButtonImagesIssue1082Tests.SetButtonState(Node: PVirtualNode; Hot, Selected: Boolean);
var
  Context: TRttiContext;
  Field: TRttiField;
begin
  fTree.ClearSelection;
  fTree.Selected[Node] := Selected;
  // HandleHotTrack() ignores mouse moves while the mouse cursor is hidden, e.g. on a build server, and there
  // is no public way to set whether the mouse is over the button of the hot node, so it is set via RTTI.
  Field := Context.GetType(TBaseVirtualTree).GetField('FHotNodeButtonHit');
  Assert.IsNotNull(Field, 'Sanity: TBaseVirtualTree.FHotNodeButtonHit not found, the test needs to be adapted.');
  Field.SetValue(fTree, Hot);
  if Hot then
    fTree.HotNode := Node
  else
    fTree.HotNode := nil;
end;

function TVTButtonImagesIssue1082Tests.PaintedButtonColor(Node: PVirtualNode): TColor;
var
  Bitmap: TBitmap;
  X, Y: Integer;
begin
  X := fTree.GetOffset(TVTElement.ofsToggleButton, Node) + TVirtualStringTreeAccess(fTree).PlusBM.Width div 2;
  Y := fTree.GetDisplayRect(Node, NoColumn, False).Top + Integer(fTree.NodeHeight[Node]) div 2;
  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf24bit;
    Bitmap.SetSize(fTree.ClientWidth, fTree.ClientHeight);
    fTree.PaintTree(Bitmap.Canvas, fTree.ClientRect, Point(0, 0), [poBackground, poColumnColor, poDrawSelection]);
    Result := Bitmap.Canvas.Pixels[X, Y];
  finally
    Bitmap.Free;
  end;
end;

procedure TVTButtonImagesIssue1082Tests.ButtonPaintsItsImage(Expanded, Hot, Selected: Boolean; const ImageName: string);
var
  Node: PVirtualNode;
begin
  if Expanded then
    Node := ExpandedNode
  else
    Node := CollapsedNode;
  SetButtonState(Node, Hot, Selected);
  Assert.AreEqual(IntToHex(ColorToRGB(ImageColor(ImageName)), 6), IntToHex(PaintedButtonColor(Node), 6),
    Format('The button must be painted with the %s image (issue #1082).', [ImageName]));
end;

procedure TVTButtonImagesIssue1082Tests.ButtonBitmapsReleaseTheirGdiHandles;
var
  Tree: TVirtualStringTreeAccess;

  procedure CheckBitmap(Bitmap: TBitmap; const Name: string; ExpectedColor: TColor);
  begin
    Assert.IsFalse(Bitmap.HandleAllocated, Name + ' must not hold a GDI handle (issue #1082).');
    Assert.IsTrue((Bitmap.Width > 0) and (Bitmap.Height > 0), Name + ' must keep its size.');
    Assert.AreEqual(IntToHex(ColorToRGB(ExpectedColor), 6), IntToHex(Bitmap.Canvas.Pixels[Bitmap.Width div 2, Bitmap.Height div 2], 6),
      Name + ' must keep its image.');
  end;

begin
  SetButtonState(CollapsedNode, True, False);
  PaintedButtonColor(CollapsedNode);
  Tree := TVirtualStringTreeAccess(fTree);
  CheckBitmap(Tree.PlusBM, 'PlusBM', cPlusColor);
  CheckBitmap(Tree.HotPlusBM, 'HotPlusBM', cHotPlusColor);
  CheckBitmap(Tree.MinusBM, 'MinusBM', cMinusColor);
  CheckBitmap(Tree.HotMinusBM, 'HotMinusBM', cHotMinusColor);
end;

procedure TVTButtonImagesIssue1082Tests.PaintingHotButtonsAllocatesNoGdiHandles;
var
  CountBefore: Cardinal;
begin
  // Paint the buttons in their normal state, including a selected node.
  SetButtonState(CollapsedNode, False, True);
  PaintedButtonColor(CollapsedNode);
  CountBefore := GdiObjectCount;

  SetButtonState(CollapsedNode, True, False);
  PaintedButtonColor(CollapsedNode);
  SetButtonState(CollapsedNode, True, True);
  PaintedButtonColor(CollapsedNode);
  SetButtonState(ExpandedNode, True, False);
  PaintedButtonColor(ExpandedNode);
  SetButtonState(ExpandedNode, True, True);
  PaintedButtonColor(ExpandedNode);

  Assert.AreEqual(CountBefore, GdiObjectCount, 'Painting the hot buttons must not allocate GDI handles (issue #1082).');
end;

initialization
  TDUnitX.RegisterTestFixture(TVTButtonImagesIssue1082Tests);

end.
