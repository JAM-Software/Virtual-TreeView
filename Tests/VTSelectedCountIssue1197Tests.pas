unit VTSelectedCountIssue1197Tests;

// Regressionstest zu Issue #1197 "SelectedCount is not always correct".
//
// Befund: ToggleSelection() (der Shift+Pfeil-Pfad) entfernt Knoten per
// InternalRemoveFromSelection aus der Auswahl. Diese Routine MARKIERT den Eintrag im
// Auswahl-Array nur (sie setzt das unterste Bit des Zeigers, siehe PackArray), feuert
// aber sofort DoRemoveFromSelection und Change. FSelectionCount wird erst NACH der
// Schleife durch PackArray korrigiert.
//
// Folge: In OnRemoveFromSelection / OnChange / OnStateChange meldet SelectedCount noch
// den alten, zu hohen Wert, waehrend das Zaehlen der Knoten mit vsSelected bereits
// stimmt - genau das beschreibt der Reporter.

interface

uses
  DUnitX.TestFramework,
  Classes,
  Vcl.Forms,
  VirtualTrees,
  VirtualTrees.Types,
  VirtualTrees.BaseTree;

type
  // Cracker, um an das protected ToggleSelection heranzukommen (der Tastaturpfad ruft es).
  TTestTree = class(TVirtualStringTree)
  public
    procedure PublicToggleSelection(StartNode, EndNode: PVirtualNode);
  end;

  [TestFixture]
  TVTSelectedCountIssue1197Tests = class
  strict private
    fForm: TForm;
    fTree: TTestTree;
    fCountInEvent: Integer;      // SelectedCount, wie es das Event sieht
    fActualInEvent: Integer;     // tatsaechlich selektierte Knoten zum selben Zeitpunkt
    fEventFired: Boolean;
    function CountSelectedNodes: Integer;
    procedure TreeRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    function NodeByIndex(Index: Integer): PVirtualNode;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// SelectedCount muss auch waehrend OnRemoveFromSelection zur tatsaechlichen
    /// Anzahl selektierter Knoten passen.
    [Test]
    procedure SelectedCountIsCorrectDuringRemoveFromSelection;

    /// Nach Abschluss der Operation muss der Wert in jedem Fall stimmen
    /// (das funktioniert schon vor dem Fix - Absicherung gegen Ueberkorrektur).
    [Test]
    procedure SelectedCountIsCorrectAfterToggleSelection;
  end;

implementation

uses
  SysUtils;

{ TTestTree }

procedure TTestTree.PublicToggleSelection(StartNode, EndNode: PVirtualNode);
begin
  ToggleSelection(StartNode, EndNode);
end;

{ TVTSelectedCountIssue1197Tests }

procedure TVTSelectedCountIssue1197Tests.Setup;
begin
  fForm := TForm.Create(nil);
  fTree := TTestTree.Create(fForm);
  fTree.Parent := fForm;
  fTree.TreeOptions.SelectionOptions := fTree.TreeOptions.SelectionOptions + [toMultiSelect];
  fTree.NodeDataSize := 0;
  fTree.RootNodeCount := 10;
  fTree.ValidateNode(nil, True);
  fEventFired := False;
  fCountInEvent := -1;
  fActualInEvent := -1;
end;

procedure TVTSelectedCountIssue1197Tests.TearDown;
begin
  FreeAndNil(fForm);
end;

function TVTSelectedCountIssue1197Tests.NodeByIndex(Index: Integer): PVirtualNode;
var
  I: Integer;
begin
  Result := fTree.GetFirst;
  for I := 1 to Index do
    Result := fTree.GetNext(Result);
end;

function TVTSelectedCountIssue1197Tests.CountSelectedNodes: Integer;
var
  Node: PVirtualNode;
begin
  Result := 0;
  Node := fTree.GetFirst;
  while Assigned(Node) do
  begin
    if vsSelected in Node.States then
      Inc(Result);
    Node := fTree.GetNext(Node);
  end;
end;

procedure TVTSelectedCountIssue1197Tests.TreeRemoveFromSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  // Nur den ersten Aufruf festhalten - dort ist die Abweichung am groessten.
  if fEventFired then
    Exit;
  fEventFired := True;
  fCountInEvent := fTree.SelectedCount;
  fActualInEvent := CountSelectedNodes;
end;

procedure TVTSelectedCountIssue1197Tests.SelectedCountIsCorrectDuringRemoveFromSelection;
var
  First, Fifth: PVirtualNode;
begin
  First := NodeByIndex(0);
  Fifth := NodeByIndex(4);

  // Knoten 0..4 auswaehlen, Anker auf den ersten setzen (wie bei Shift+Pfeil runter).
  fTree.FocusedNode := First;
  fTree.Selected[First] := True;
  fTree.SelectNodes(First, Fifth, False);
  Assert.AreEqual(5, fTree.SelectedCount, 'Vorbedingung: 5 Knoten ausgewaehlt');

  fTree.OnRemoveFromSelection := TreeRemoveFromSelection;

  // Auswahl verkleinern (Shift+Pfeil hoch): der Bereich 4..2 wird abgewaehlt.
  fTree.PublicToggleSelection(Fifth, NodeByIndex(2));

  Assert.IsTrue(fEventFired, 'OnRemoveFromSelection wurde nicht ausgeloest');
  Assert.AreEqual(fActualInEvent, fCountInEvent,
    Format('SelectedCount meldet im Event %d, tatsaechlich selektiert sind %d',
           [fCountInEvent, fActualInEvent]));
end;

procedure TVTSelectedCountIssue1197Tests.SelectedCountIsCorrectAfterToggleSelection;
var
  First, Fifth: PVirtualNode;
begin
  First := NodeByIndex(0);
  Fifth := NodeByIndex(4);

  fTree.FocusedNode := First;
  fTree.Selected[First] := True;
  fTree.SelectNodes(First, Fifth, False);

  fTree.PublicToggleSelection(Fifth, NodeByIndex(2));

  Assert.AreEqual(CountSelectedNodes, fTree.SelectedCount,
    'SelectedCount nach Abschluss der Operation');
end;

initialization
  TDUnitX.RegisterTestFixture(TVTSelectedCountIssue1197Tests);

end.
