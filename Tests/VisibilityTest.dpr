program VisibilityTest;

uses
  Vcl.Forms,
  VTCellSelectionTests.VisibilityForm in 'VTCellSelectionTests.VisibilityForm.pas' {VisibilityForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TVisibilityForm, VisibilityForm);
  Application.Run;
end.
