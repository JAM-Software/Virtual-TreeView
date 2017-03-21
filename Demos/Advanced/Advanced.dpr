program Advanced;

{$R 'ExtraRes.res' 'ExtraRes.rc'}

uses
  Forms,
  SpeedDemo in 'SpeedDemo.pas' {SpeedForm},
  DrawTreeDemo in 'DrawTreeDemo.pas' {DrawTreeForm},
  GeneralAbilitiesDemo in 'GeneralAbilitiesDemo.pas' {frmGeneralAbilities},
  PropertiesDemo in 'PropertiesDemo.pas' {PropertiesForm},
  GridDemo in 'GridDemo.pas' {GridForm},
  Editors in 'Editors.pas',
  VisibilityDemo in 'VisibilityDemo.pas' {VisibilityForm},
  AlignDemo in 'AlignDemo.pas' {AlignForm},
  Main in 'Main.pas' {MainForm},
  WindowsXPStyleDemo in 'WindowsXPStyleDemo.pas' {WindowsXPForm},
  MultilineDemo in 'MultilineDemo.pas' {NodeForm},
  HeaderCustomDrawDemo in 'HeaderCustomDrawDemo.pas' {HeaderOwnerDrawForm},
  States in 'States.pas' {StateForm};

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TStateForm, StateForm);
  Application.Run;
end.
