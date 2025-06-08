unit MainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, Buttons, Menus,
  ExtCtrls, IniFiles,
  // My units
  VMsystem, OptionsWindow, InfoWindow;


type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    ComboBoxDGA: TComboBox;
    ConfigListBox: TListBox;
    Image1: TImage;
    ImageList1: TImageList;
    LabelDGAmode: TLabel;
    MenuItemRun: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemDelete: TMenuItem;
    Separator1: TMenuItem;
    MenuItemFitWindow: TMenuItem;
    MenuItemRename: TMenuItem;
    PopupMenu1: TPopupMenu;
    Shape1: TShape;
    SpeedButtonLaunch: TSpeedButton;
    SpeedButtonSettings: TSpeedButton;
    SpeedButtonOptions: TSpeedButton;
    SpeedButtonAdd: TSpeedButton;
    SpeedButtonRename: TSpeedButton;
    SpeedButtonCopy: TSpeedButton;
    SpeedButtonDelete: TSpeedButton;
    SpeedButtonQuit: TSpeedButton;
    SpeedButtonInfo: TSpeedButton;
    procedure ButtonConfigureClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonLaunchClick(Sender: TObject);
    procedure ButtonNewVMClick(Sender: TObject);
    procedure ButtonRenameClick(Sender: TObject);
    procedure ButtonCopyClick(Sender: TObject);
    procedure ComboBoxDGAChange(Sender: TObject);
    procedure ConfigListBoxDblClick(Sender: TObject);
    procedure MenuItemCopyClick(Sender: TObject);
    procedure MenuItemDeleteClick(Sender: TObject);
    procedure MenuItemFitWindowClick(Sender: TObject);
    procedure ConfigListBoxSelectionChange(Sender: TObject);
    procedure MenuItemRenameClick(Sender: TObject);
    procedure MenuItemRunClick(Sender: TObject);
    procedure SpeedButtonInfoClick(Sender: TObject);
    procedure SpeedButtonOptionsClick(Sender: TObject);
    procedure SpeedButtonQuitClick(Sender: TObject);
  private
    function ConfigurationListItemWidth : integer;
    procedure UpdateGUI;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure UpdateList;
    function VMindex : integer;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

function TMainForm.ConfigurationListItemWidth : integer;
var
  MaxWidth, i, TextWidth: Integer;
begin
  MaxWidth := 0;
  for i := 0 to ConfigListBox.Items.Count - 1 do
  begin
    TextWidth := ConfigListBox.Canvas.TextWidth(ConfigListBox.Items[i]);
    if TextWidth > MaxWidth then
      MaxWidth := TextWidth;
  end;
  ConfigurationListItemWidth := MaxWidth;
end;


function TMainForm.VMindex : integer;
begin
  if ConfigListBox.Count > 0 then begin
    Result := VMs.IndexOfVM(ConfigListBox.Items[ConfigListBox.ItemIndex]);
  end
  else begin
    Result := -1;
  end;
end;

procedure TMainForm.ButtonLaunchClick(Sender: TObject);
begin
  VMs.LaunchVM(VMindex);
end;

procedure TMainForm.ButtonNewVMClick(Sender: TObject);
var
  NewName: String;
begin
  NewName := 'New VM';
  if InputQuery('Name for new VM','Enter name:', NewName) then begin
     VMs.NewVM(NewName);
     UpdateList;
  end;
  ConfigListBox.ItemIndex := ConfigListBox.Count-1;
end;

procedure TMainForm.ButtonDeleteClick(Sender: TObject);
begin
   VMs.DeleteVM(VMindex);
   UpdateList;
end;

procedure TMainForm.ButtonConfigureClick(Sender: TObject);
begin
  VMs.ConfigureVM(VMindex);
end;

procedure TMainForm.ButtonRenameClick(Sender: TObject);
var
  OldName, NewName: String;
begin
  OldName:= VMs.VMarr[VMindex].VMname;
  NewName := OldName;
  if InputQuery('New name','Enter name:', NewName) then begin
    if not (NewName = OldName) then begin
      VMs.RenameVM(VMindex, NewName);
      UpdateList;
    end;
  end;
end;

procedure TMainForm.ButtonCopyClick(Sender: TObject);
var
  OldName, NewName: String;
begin
  OldName:= VMs.VMarr[VMindex].VMname;
  NewName := OldName + ' (Copy)';
  if InputQuery('New name','Enter name:', NewName) then begin
    if not (NewName = OldName) then begin
      VMs.CopyVM(VMindex, NewName);
      UpdateList;
    end;
  end;
end;

procedure TMainForm.ComboBoxDGAChange(Sender: TObject);
var
  SelectedItemIndex : integer;
  VMINI : TIniFile;
begin
  SelectedItemIndex := VMs.IndexOfVM(ConfigListBox.Items[ConfigListBox.ItemIndex]);
  try
  VMINI := TIniFile.Create(VMs.VMarr[SelectedItemIndex].Cfg_path);
  if ComboBoxDGA.ItemIndex = 0 then begin
    // DGA Hercules mode
    VMINI.WriteString('Video', 'gfxcard', 'hercules');
  end
  else begin
    // DGA CGA mode
    VMINI.WriteString('Video', 'gfxcard', 'cga');
  end;
  finally
    VMINI.Free;
  end;
end;


procedure TMainForm.ConfigListBoxDblClick(Sender: TObject);
begin
  ButtonLaunchClick(Sender);
end;

procedure TMainForm.MenuItemCopyClick(Sender: TObject);
begin
  ButtonCopyClick(Sender);
end;

procedure TMainForm.MenuItemDeleteClick(Sender: TObject);
begin
  ButtonDeleteClick(Sender);
end;

procedure TMainForm.MenuItemFitWindowClick(Sender: TObject);
begin
  Width := ConfigurationListItemWidth + 92;
  ConfigListBox.ScrollWidth := ConfigListBox.Width;
end;

procedure TMainForm.ConfigListBoxSelectionChange(Sender: TObject);
begin
  UpdateGUI;
end;

procedure TMainForm.MenuItemRenameClick(Sender: TObject);
begin
  ButtonRenameClick(Sender);
end;

procedure TMainForm.MenuItemRunClick(Sender: TObject);
begin
  ButtonLaunchClick(Sender);
end;

procedure TMainForm.SpeedButtonInfoClick(Sender: TObject);
var
  S : string;
  Machine, CPU_family, CPU_speed, Mem_size, GFXcard, SndCard : String;
  Machine_name, CPU_name, CPU_MHz, GFXcard_name, SndCard_name : String;
  Mem, hd : Integer;
  SL : TStringList;
  CfgIni, SysInfoIni : TIniFile;
begin
  InfoForm.Caption := VMs.VMarr[VMindex].VMname;

  try
    CfgIni := TIniFile.Create(VMS.VMarr[VMindex].Cfg_path);
    Machine := CfgIni.ReadString('Machine', 'machine', 'No machine');
    CPU_family := CfgIni.ReadString('Machine', 'cpu_family', 'No CPU');
    CPU_speed := CfgIni.ReadString('Machine', 'cpu_speed', '0');
    Mem_size := CfgIni.ReadString('Machine', 'mem_size', '0');
    Mem := StrToInt(Mem_size);
    GFXcard := CfgIni.ReadString('Video', 'gfxcard', 'No graphics card');
    SndCard := CfgIni.ReadString('Sound', 'sndcard', 'No sound card');
  finally
    CfgIni.Free;
  end;

  try
    SysInfoIni := TIniFile.Create(Settings.sysinfo);
    Machine_name:= SysInfoIni.ReadString('Machine', Machine, 'Unknown machine');
    CPU_name := SysInfoIni.ReadString('CPU', CPU_family, 'Unknown CPU');
    CPU_MHz := IntToStr(StrToInt(CPU_speed)  div 1000000);
    GFXcard_name := SysInfoIni.ReadString('Video', GFXcard, 'Internal device');
    SndCard_name := SysInfoIni.ReadString('Sound', SndCard, 'Unknown  sound card');
  finally
    SysInfoIni.Free;
  end;

  InfoForm.LabelMachineName.Caption := Machine_name;
  InfoForm.LabelCPUinfo.Caption := CPU_name + ' @ ' + CPU_MHz + 'MHz';
  if Mem <= 2048 then
     InfoForm.LabelMB.Caption := IntToStr(Mem) + ' kB'
  else
     InfoForm.LabelMB.Caption := IntToStr(Mem div 1024) + ' MB';
  InfoForm.LabelDisplayCard.Caption := GFXcard_name;
  if SndCard = 'No sound card' then
    InfoForm.LabelSoundCard.Caption := SndCard
  else
    InfoForm.LabelSoundCard.Caption := SndCard_name;

  S := '';
  if Length(VMs.VMarr[VMindex].Storage) > 0 then begin
     for hd := 0 to Length(VMs.VMarr[VMindex].Storage)-1 do begin
       if VMs.VMarr[VMindex].Storage[hd].SizeMB <= 1024 then
          S := S + 'hd' + IntToStr(hd) + ': '+ FloatToStrF(VMs.VMarr[VMindex].Storage[hd].SizeMB, ffFixed, 15, 1) + ' MB' + Chr(10)
       else
          S := S + 'hd' + IntToStr(hd) + ': '+ FloatToStrF(VMs.VMarr[VMindex].Storage[hd].SizeMB/1024, ffFixed, 15, 2) + ' GB' + Chr(10)
     end;
     InfoForm.LabelHDMB.Caption := S;
  end
  else InfoForm.LabelHDMB.Caption := 'not present';
  S := ExtractFilePath(VMs.VMarr[VMindex].Cfg_path);
  if FileExists(S + 'os.txt') then InfoForm.MemoOS.Lines.LoadFromFile(S + 'os.txt');
  if FileExists(S + 'apps.txt') then InfoForm.MemoApps.Lines.LoadFromFile(S + 'apps.txt');
  SL := TStringList.Create;
  SL.Assign(InfoForm.MemoApps.Lines);
  Sl.Sort;
  InfoForm.MemoApps.Lines.Assign(SL);
  SL.Free;
  InfoForm.ShowModal;
  InfoForm.MemoOS.Lines.SaveToFile(S + 'os.txt');
  InfoForm.MemoOS.Lines.Clear;
  InfoForm.MemoApps.Lines.SaveToFile(S + 'apps.txt');
  InfoForm.MemoApps.Lines.Clear;
end;

procedure TMainForm.SpeedButtonOptionsClick(Sender: TObject);
begin
  OptionsForm.CheckGroupSettings.Checked[0] := (Settings.noconfirm=1);
  OptionsForm.CheckGroupSettings.Checked[1] := (Settings.fullscreen=1);
  OptionsForm.CheckGroupSettings.Checked[2] := (Settings.simulate_dga=1);
  OptionsForm.TrackBarBackups.Position := Settings.nrbackups;
  OptionsForm.TrackBarHDBackupSize.Position := Settings.hdbackupsize;
  OptionsForm.Label86BoxBinarySelected.Caption:=ExtractFileName(Settings.exe_86box);
  OptionsForm.exe:=Settings.exe_86box;
  OptionsForm.exe_dir:=Settings.exe_86box_dir;
  OptionsForm.EditCustomVMOtherSettings.Text := Settings.other_VM_settings;
  OptionsForm.ShowModal;
  if OptionsForm.CheckGroupSettings.Checked[0] then Settings.noconfirm:=1 else Settings.noconfirm:=0;
  if OptionsForm.CheckGroupSettings.Checked[1] then Settings.fullscreen:=1 else Settings.fullscreen:=0;
  if OptionsForm.CheckGroupSettings.Checked[2] then Settings.simulate_dga:=1 else Settings.simulate_dga:=0;
  Settings.nrbackups := OptionsForm.TrackBarBackups.Position;
  Settings.hdbackupsize := OptionsForm.TrackBarHDBackupSize.Position;
  Settings.exe_86box:=OptionsForm.exe;
  Settings.exe_86box_dir:=OptionsForm.exe_dir;
  Settings.other_VM_settings := OptionsForm.EditCustomVMOtherSettings.Text;
  Settings.SaveConfig;
end;

procedure TMainForm.SpeedButtonQuitClick(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.UpdateList;
var
  i: integer;
begin
  ConfigListBox.Items.Clear;
    for i := 0 to Length(VMs.VMarr)-1 do begin
      with VMs.VMarr[i] do begin
        ConfigListBox.Items.Add(VMname);
      end;
    end;
  if ConfigListBox.Items.Count > 0 then ConfigListBox.ItemIndex := 0;
  UpdateGUI;
end;

procedure TMainForm.UpdateGUI;
var
  ListNotEmpty : boolean;
  VMINI : TIniFile;
  gfxcard : string;
begin
  ListNotEmpty := ConfigListBox.Count > 0;
  SpeedButtonSettings.Enabled := ListNotEmpty;
  SpeedButtonRename.Enabled := ListNotEmpty;
  SpeedButtonCopy.Enabled := ListNotEmpty;
  SpeedButtonDelete.Enabled := ListNotEmpty;
  ComboBoxDGA.Visible := ListNotEmpty and (Settings.simulate_dga = 1) and (VMs.VMarr[VMindex].Tulip_DGA = 'present');
  LabelDGAmode.Visible := ListNotEmpty and (Settings.simulate_dga = 1) and (VMs.VMarr[VMindex].Tulip_DGA = 'present');
  if ListNotEmpty then begin
    try
      VMINI := TIniFile.Create(VMS.VMarr[VMindex].Cfg_path);
      gfxcard := VMINI.ReadString('Video', 'gfxcard', 'No gfxcard');
    finally
      VMINI.Free;
    end;
    if ComboBoxDGA.Visible and (gfxcard = 'hercules') then ComboBoxDGA.ItemIndex := 0 else ComboBoxDGA.ItemIndex := 1;
  end
  else ComboBoxDGA.Visible := False;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  Settings := TSettings.Create;
  VMs := TVMs.Create;
  UpdateList;
end;

destructor TMainForm.Destroy;
begin
  Settings.Free;
  VMs.Free;
  inherited;
end;



end.

