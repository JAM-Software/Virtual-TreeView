program OLE;

uses
  Forms,
  Main in 'Main.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'OLE Drag''n drop and cut''n paste demo';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
