unit optionswindow;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, MaskEdit,
  ComCtrls, ExtCtrls, Buttons;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    ButtonOK: TButton;
    CheckGroup1: TCheckGroup;
    EditCustomVMsettings: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    SpeedButton1: TSpeedButton;
    TrackBarBackups: TTrackBar;
    TrackBarHDBackupSize: TTrackBar;
    procedure ButtonOKClick(Sender: TObject);
    procedure EditCustomVMsettingsChange(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure TrackBarBackupsChange(Sender: TObject);
    procedure TrackBarHDBackupSizeChange(Sender: TObject);
  private

  public
    exe: string;
    exe_dir: string;
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

{ TOptionsForm }

procedure TOptionsForm.ButtonOKClick(Sender: TObject);
begin
  Close;
end;

procedure TOptionsForm.EditCustomVMsettingsChange(Sender: TObject);
begin

end;

procedure TOptionsForm.Label6Click(Sender: TObject);
begin

end;

procedure TOptionsForm.SpeedButton1Click(Sender: TObject);
var
  OpenDialog : TOpenDialog;
begin
      OpenDialog := TOpenDialog.Create(nil);
      OpenDialog.Title := 'Please specify the full path of the 86Box binary';
      OpenDialog.Options := [ofFileMustExist,ofEnableSizing,ofViewDetail];
      OpenDialog.Execute;
      exe := OpenDialog.FileName;
      exe_dir := ExtractFilePath(exe);
      Label5.Caption:=ExtractFileName(exe);
end;

procedure TOptionsForm.TrackBarBackupsChange(Sender: TObject);
begin

end;

procedure TOptionsForm.TrackBarHDBackupSizeChange(Sender: TObject);
begin
  if TrackBarHDBackupSize.Position = 0 then begin
    Label3.Caption:= 'HD backup off';
    Label3.Visible:= True;
  end
  else if TrackBarHDBackupSize.Position = TrackBarHDBackupSize.Max then begin
    Label3.Caption:= 'HD always on';
    Label3.Visible:= True;
  end
  else begin
    Label3.Caption:= 'HD backup';
    Label3.Visible:= False;
  end;
end;

end.

