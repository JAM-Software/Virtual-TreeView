program VisibilityTest;

uses
  Vcl.Forms,
  VTCellSelectionTests.VisibilityForm in 'VTCellSelectionTests.VisibilityForm.pas' {VisibilityForm},
  VirtualTrees.MouseUtils in 'VirtualTrees.MouseUtils.pas',
  VTCellSelectionTests.VTSelectionTestForm in 'VTCellSelectionTests.VTSelectionTestForm.pas' {SelectionTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSelectionTestForm, SelectionTestForm);
  Application.CreateForm(TVisibilityForm, VisibilityForm);
  Application.Run;
end.
