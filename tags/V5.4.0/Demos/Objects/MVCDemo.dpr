program MVCDemo;

{ (c) 2000 Marian Aldenhövel
           Hainstraße 8
           53121 Bonn
           +49 228 6203366
           Fax: +49 228 624031
           marian@mba-software.de

  Free: You may use this code in every way you find it useful or fun. }

uses
  Forms,
  MVCDemoMain in 'MVCDemoMain.pas' {fmMVCDemo},
  MVCPanel in 'MVCPanel.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfmMVCDemo,fmMVCDemo);
  Application.Run;
end.
