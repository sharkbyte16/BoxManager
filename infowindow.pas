unit infowindow;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, VMsystem;

type

  { TInfoForm }

  TInfoForm = class(TForm)
    ButtonOK: TButton;
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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

procedure TInfoForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

constructor TInfoForm.Create(AOwner: TComponent);
begin
  inherited;
  If not FileExists(Paths.sysinfo) then begin
    InfoForm.MemoSysInfo.Lines.SaveToFile(Paths.sysinfo)
  end
  else
    InfoForm.MemoSysInfo.Lines.LoadFromFile(Paths.sysinfo);
  end;

end.

