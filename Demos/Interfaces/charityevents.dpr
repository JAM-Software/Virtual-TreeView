program charityevents;

uses
  Vcl.Forms,
  modelviewform in 'modelviewform.pas' {FormModelView},
  myeventdata in 'myeventdata.pas',
  myevents in 'myevents.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormModelView, FormModelView);
  Application.Run;
end.
