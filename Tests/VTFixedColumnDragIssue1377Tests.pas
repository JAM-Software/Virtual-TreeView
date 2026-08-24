unit VTFixedColumnDragIssue1377Tests;

// Regression test for issue #1377 "Normal columns can be dragged in front of fixed
// columns (!) and cannot be dragged back".
//
// Dropping a normal column inside the fixed area made it fixed (deliberate behavior
// of TVirtualTreeColumn.SetPosition) - and since issue #1314 fixed columns lose
// coDraggable, so the column was trapped there for good.
//
// The fix redirects the drop target in TVTHeader.DragTo(): when a non-fixed column
// is dragged over a fixed one, the target becomes the first non-fixed visible
// column, so the drop lands right after the fixed area. The deliberate programmatic
// behavior (assigning Position directly moves a column into the fixed area and makes
// it fixed) is unchanged and covered by a test as well.

interface

uses
  DUnitX.TestFramework,
  System.Types,
  Vcl.Forms,
  VirtualTrees;

type
  [TestFixture]
  TVTFixedColumnDragIssue1377Tests = class
  strict private
    fForm: TForm;
    fTree: TVirtualStringTree;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// Dragging a normal column over the fixed area must land it right after the
    /// fixed columns, keeping it normal and draggable.
    [Test]
    procedure DropOnFixedAreaLandsAfterFixedColumns;

    /// The deliberate programmatic behavior is unchanged: assigning Position
    /// directly still moves the column into the fixed area and makes it fixed.
    [Test]
    procedure DirectPositionAssignmentStillEntersFixedArea;
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  Vcl.Controls,
  VirtualTrees.Types,
  VirtualTrees.Header;

procedure TVTFixedColumnDragIssue1377Tests.Setup;
var
  I: Integer;
begin
  fForm := TForm.Create(nil);
  fForm.SetBounds(0, 0, 420, 300);
  fTree := TVirtualStringTree.Create(fForm);
  fTree.Parent := fForm;
  fTree.SetBounds(0, 0, 400, 260);
  for I := 0 to 2 do
    with fTree.Header.Columns.Add do
      Width := 80;
  fTree.Header.Columns[0].Options := fTree.Header.Columns[0].Options + [coFixed];
  fTree.Header.Options := fTree.Header.Options + [hoVisible, hoDrag];
  fForm.Show;
  Application.ProcessMessages;
end;

procedure TVTFixedColumnDragIssue1377Tests.TearDown;
begin
  FreeAndNil(fForm);
end;

procedure TVTFixedColumnDragIssue1377Tests.DropOnFixedAreaLandsAfterFixedColumns;
var
  P: TPoint;
begin
  Assert.IsFalse(coDraggable in fTree.Header.Columns[0].Options,
    'Sanity: the fixed column must not be draggable (issue #1314).');

  // Simulate dragging column 2 over the fixed column 0: point inside the header,
  // horizontally in the middle of column 0.
  fTree.Header.Columns.DragIndex := 2;
  P := fTree.ClientToScreen(Point(40, -fTree.Header.Height div 2));
  fTree.Header.DragTo(P);

  Assert.AreNotEqual<TColumnIndex>(0, fTree.Header.Columns.DropTarget,
    'The fixed column must not become the drop target (issue #1377).');
  Assert.AreEqual<TColumnIndex>(1, fTree.Header.Columns.DropTarget,
    'The drop target must be redirected to the first non-fixed column.');
  Assert.IsTrue(fTree.Header.Columns.DropBefore,
    'The drop must aim before the first non-fixed column.');

  fTree.Header.ColumnDropped(P);

  Assert.AreEqual(1, Integer(fTree.Header.Columns[2].Position),
    'The dropped column must land right after the fixed area (issue #1377).');
  Assert.AreEqual(0, Integer(fTree.Header.Columns[0].Position),
    'The fixed column must stay at position 0.');
  Assert.IsFalse(coFixed in fTree.Header.Columns[2].Options,
    'The dropped column must not become fixed (issue #1377).');
  Assert.IsTrue(coDraggable in fTree.Header.Columns[2].Options,
    'The dropped column must remain draggable (issue #1377).');
end;

procedure TVTFixedColumnDragIssue1377Tests.DirectPositionAssignmentStillEntersFixedArea;
begin
  fTree.Header.Columns[2].Position := 0;
  Assert.IsTrue(coFixed in fTree.Header.Columns[2].Options,
    'Programmatically moving a column into the fixed area must still make it fixed (deliberate behavior).');
end;

initialization
  TDUnitX.RegisterTestFixture(TVTFixedColumnDragIssue1377Tests);

end.
