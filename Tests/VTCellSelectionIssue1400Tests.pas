unit VTCellSelectionIssue1400Tests;

// Regression tests for issue #1400 "Cell range selection only works correctly
// when columns stay in original order".
//
// SelectCells/UnselectCells/ToggleCellSelection decided the iteration direction
// by comparing column INDICES, but iterate with GetNextVisibleColumn/
// GetPreviousVisibleColumn, which walk in visual order (POSITION). After
// reordering columns, the iteration ran past the target column to the edge of
// the header: cells of the selected range were missing and cells visually
// outside the range got selected.
//
// The fixture uses four columns A..D (indices 0..3) reordered visually to
// C B A D and two flat nodes; expectations are per visually contiguous range.

interface

uses
  DUnitX.TestFramework,
  Classes,
  System.Types,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Graphics,
  VirtualTrees,
  VirtualTrees.BaseTree,
  VirtualTrees.Types;

type
  TTreeCracker = class(TBaseVirtualTree); // for the protected ToggleCellSelection

  [TestFixture]
  TVTCellSelectionIssue1400Tests = class
  strict private
    fForm: TForm;
    fTree: TVirtualStringTree;
    fNode0, fNode1: PVirtualNode;
    function SelectedCellSet: string;
    procedure AddColumn(const Caption: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// Range from the visually leftmost to a middle column: all visually
    /// contiguous cells must be selected, in both directions.
    [Test]
    procedure SelectRangeHonorsColumnPositions;

    /// Unselecting a visual sub-range must remove exactly that sub-range.
    [Test]
    procedure UnselectRangeHonorsColumnPositions;

    /// Toggling a visual range must invert exactly that range.
    [Test]
    procedure ToggleRangeHonorsColumnPositions;
  end;

implementation

uses
  System.SysUtils;

procedure TVTCellSelectionIssue1400Tests.AddColumn(const Caption: string);
begin
  with fTree.Header.Columns.Add do
  begin
    Text := Caption;
    Width := 60;
  end;
end;

procedure TVTCellSelectionIssue1400Tests.Setup;
begin
  fForm := TForm.Create(nil);
  fForm.SetBounds(0, 0, 400, 160);
  fTree := TVirtualStringTree.Create(fForm);
  fTree.Parent := fForm;
  fTree.Align := alClient;
  fTree.BorderStyle := bsNone;
  fTree.TreeOptions.SelectionOptions := fTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect, toMultiCellSelect] - [toFullRowSelect];
  AddColumn('A'); // Index 0
  AddColumn('B'); // Index 1
  AddColumn('C'); // Index 2
  AddColumn('D'); // Index 3
  fTree.Header.MainColumn := 0;
  fNode0 := fTree.AddChild(nil);
  fNode1 := fTree.AddChild(nil);
  fForm.Show;
  Application.ProcessMessages;

  // Reorder the visual order to C B A D while the indices stay A=0 B=1 C=2 D=3.
  fTree.Header.Columns[2].Position := 0;
  fTree.Header.Columns[1].Position := 1;
  fTree.Header.Columns[0].Position := 2;
  fTree.Header.Columns[3].Position := 3;
end;

procedure TVTCellSelectionIssue1400Tests.TearDown;
begin
  FreeAndNil(fForm);
end;

function TVTCellSelectionIssue1400Tests.SelectedCellSet: string;
var
  Cells: TVTCellArray;
  Parts: TStringList;
  I: Integer;
begin
  Cells := fTree.SelectedCells;
  Parts := TStringList.Create;
  try
    Parts.Sorted := True;
    for I := 0 to High(Cells) do
      Parts.Add(Format('n%d:c%d', [Cells[I].Node.Index, Cells[I].Column]));
    Parts.Delimiter := ' ';
    Result := Parts.DelimitedText;
  finally
    Parts.Free;
  end;
end;

procedure TVTCellSelectionIssue1400Tests.SelectRangeHonorsColumnPositions;
begin
  // (n0, C) -> (n1, A): visually C..A, i.e. indices {2, 1, 0} on both nodes.
  fTree.SelectCells(fNode0, 2, fNode1, 0, False);
  Assert.AreEqual('n0:c0 n0:c1 n0:c2 n1:c0 n1:c1 n1:c2', SelectedCellSet,
    'Selecting from the visually leftmost to the visually third column must select the three contiguous columns (issue #1400).');

  // Same visual range anchored at the other end.
  fTree.SelectCells(fNode0, 0, fNode1, 2, False);
  Assert.AreEqual('n0:c0 n0:c1 n0:c2 n1:c0 n1:c1 n1:c2', SelectedCellSet,
    'The same range selected in the opposite direction must yield the same cells and no visually foreign ones (issue #1400).');
end;

procedure TVTCellSelectionIssue1400Tests.UnselectRangeHonorsColumnPositions;
begin
  fTree.SelectCells(fNode0, 2, fNode1, 3, False); // visually C..D = all four columns
  Assert.AreEqual('n0:c0 n0:c1 n0:c2 n0:c3 n1:c0 n1:c1 n1:c2 n1:c3', SelectedCellSet,
    'Sanity: selecting from the leftmost to the rightmost visual column must select all cells.');

  fTree.UnselectCells(fNode0, 1, fNode1, 0); // visually B..A
  Assert.AreEqual('n0:c2 n0:c3 n1:c2 n1:c3', SelectedCellSet,
    'Unselecting the visual range B..A must remove exactly those cells (issue #1400).');
end;

procedure TVTCellSelectionIssue1400Tests.ToggleRangeHonorsColumnPositions;
begin
  fTree.SelectCells(fNode0, 2, fNode1, 2, False); // only column C selected
  TTreeCracker(fTree).ToggleCellSelection(TVTCell.Create(fNode0, 2), TVTCell.Create(fNode1, 1)); // visually C..B
  Assert.AreEqual('n0:c1 n1:c1', SelectedCellSet,
    'Toggling the visual range C..B must deselect C and select B, nothing else (issue #1400).');
end;

initialization
  TDUnitX.RegisterTestFixture(TVTCellSelectionIssue1400Tests);

end.
