unit optionswindow;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, MaskEdit,
  ComCtrls, ExtCtrls;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    ButtonOK: TButton;
    CheckGroup1: TCheckGroup;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    TrackBarBackups: TTrackBar;
    TrackBarHDBackupSize: TTrackBar;
    procedure ButtonOKClick(Sender: TObject);
    procedure TrackBarBackupsChange(Sender: TObject);
    procedure TrackBarHDBackupSizeChange(Sender: TObject);
  private

  public

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

