program boxmanager;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, Forms, MainWindow, VMsystem, OptionsWindow, InfoWindow;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='BoxManager';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TInfoForm, InfoForm);
  Application.Run;
end.

