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
    CheckGroupSettings: TCheckGroup;
    EditCustomVMOtherSettings: TEdit;
    LabelNrBackups: TLabel;
    LabelHDBackupSize: TLabel;
    LabelHDBackupStatus: TLabel;
    Label86BoxBinary: TLabel;
    Label86BoxBinarySelected: TLabel;
    LabelOtherVMSettings: TLabel;
    SpeedButtonSelect86BoxBinary: TSpeedButton;
    TrackBarBackups: TTrackBar;
    TrackBarHDBackupSize: TTrackBar;
    procedure ButtonOKClick(Sender: TObject);
    procedure SpeedButtonSelect86BoxBinaryClick(Sender: TObject);
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


procedure TOptionsForm.SpeedButtonSelect86BoxBinaryClick(Sender: TObject);
var
  OpenDialog : TOpenDialog;
begin
      OpenDialog := TOpenDialog.Create(nil);
      OpenDialog.Title := 'Please specify the full path of the 86Box binary';
      OpenDialog.Options := [ofFileMustExist,ofEnableSizing,ofViewDetail];
      OpenDialog.Execute;
      exe := OpenDialog.FileName;
      exe_dir := ExtractFilePath(exe);
      Label86BoxBinarySelected.Caption:=ExtractFileName(exe);
end;


procedure TOptionsForm.TrackBarHDBackupSizeChange(Sender: TObject);
begin
  if TrackBarHDBackupSize.Position = 0 then begin
    LabelHDBackupStatus.Caption:= 'HD backup off';
    LabelHDBackupStatus.Visible:= True;
  end
  else if TrackBarHDBackupSize.Position = TrackBarHDBackupSize.Max then begin
    LabelHDBackupStatus.Caption:= 'HD always on';
    LabelHDBackupStatus.Visible:= True;
  end
  else begin
    LabelHDBackupStatus.Caption:= 'HD backup';
    LabelHDBackupStatus.Visible:= False;
  end;
end;

end.

