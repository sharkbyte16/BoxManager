unit infowindow;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  VMsystem;

type

  { TInfoForm }

  TInfoForm = class(TForm)
    ButtonOK: TButton;
    LabelDisplay: TLabel;
    LabelSound: TLabel;
    LabelSoundCard: TLabel;
    LabelDisplayCard: TLabel;
    LabelDisks: TLabel;
    LabelHDMB: TLabel;
    LabelMB: TLabel;
    LabelOS: TLabel;
    LabelCPUinfo: TLabel;
    LabelMachineName: TLabel;
    LabelMem: TLabel;
    LabelApps: TLabel;
    LabelSystem: TLabel;
    LabelCPU: TLabel;
    MemoSysInfo: TMemo;
    MemoOS: TMemo;
    MemoApps: TMemo;
    procedure ButtonOKClick(Sender: TObject);
  private

  public
    constructor Create(AOwner : TComponent); override;
  end;

var
  InfoForm: TInfoForm;

implementation

{$R *.lfm}

{ TInfoForm }

procedure TInfoForm.ButtonOKClick(Sender: TObject);
begin
  Close;
end;

constructor TInfoForm.Create(AOwner: TComponent);
begin
  inherited;
  If not FileExists(Settings.sysinfo) then
    InfoForm.MemoSysInfo.Lines.SaveToFile(Settings.sysinfo)
  else
    InfoForm.MemoSysInfo.Lines.LoadFromFile(Settings.sysinfo);
end;

end.

