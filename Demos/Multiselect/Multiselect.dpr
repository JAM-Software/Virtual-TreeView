program Multiselect;

uses
  Vcl.Forms,
  Multiselect.Main in 'Multiselect.Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
