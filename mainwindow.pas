unit MainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, MaskEdit,
  Buttons, Menus, ExtCtrls, ComCtrls, VMsystem, OptionsWindow,
  InfoWindow, LazLogger;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    ConfigListBox: TListBox;
    Image1: TImage;
    ImageList1: TImageList;
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
  Result := VMs.IndexOfVM(ConfigListBox.Items[ConfigListBox.ItemIndex]);
end;

procedure TMainForm.ButtonLaunchClick(Sender: TObject);
begin
  DebugLn(['Launching ', VMindex, ':  ', VMs.VMarr[VMindex].VMname]);
  VMs.LaunchVM(VMindex);
end;

procedure TMainForm.ButtonNewVMClick(Sender: TObject);
var
  NewName: String;
  TargetEmulator: TEmulator;
begin
  NewName := 'New VM';
  TargetEmulator := vm_86box;
  if InputQuery('Name for new VM','Enter name:', NewName) then begin
     VMs.NewVM(NewName, TargetEmulator);
     UpdateList;
  end;
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
      DebugLn(['Rename VM index ' , VMindex, ': ', OldName, ' --> ', NewName]);
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
      DebugLn(['Copy VM index ' , VMindex, ': ', OldName, ' --> ', NewName]);
      VMs.CopyVM(VMindex, NewName);
      UpdateList;
    end;
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
var
  SelectedItemIndex : Integer;
begin
  SelectedItemIndex := VMs.IndexOfVM(ConfigListBox.Items[ConfigListBox.ItemIndex]);
  DebugLn([ConfigListBox.Items[ConfigListBox.ItemIndex], ' --> VMs.VMarr[', SelectedItemIndex, ']']);
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
  S, MHz : string;
  Mem, hd : Integer;
  SL : TStringList;
begin
  InfoForm.Caption := VMs.VMarr[VMindex].VMname;

  S := GetConfigSetting(VMS.VMarr[VMindex].Cfg_path, 'machine');
  S := GetConfigSetting(Paths.sysinfo, S);
  InfoForm.LabelMachineName.Caption := S;

  S := GetConfigSetting(VMS.VMarr[VMindex].Cfg_path, 'cpu_family');
  S := GetConfigSetting(Paths.sysinfo, S);
  MHz := IntToStr(StrToInt(GetConfigSetting(VMS.VMarr[VMindex].Cfg_path, 'cpu_speed'))  div 1000000);
  InfoForm.LabelCPUinfo.Caption := S + ' @ ' + MHz + 'MHz';

  Mem := StrToInt(GetConfigSetting(VMS.VMarr[VMindex].Cfg_path, 'mem_size'));
  if Mem <= 2048 then InfoForm.LabelMB.Caption := IntToStr(Mem) + ' kB'
     else InfoForm.LabelMB.Caption := IntToStr(Mem div 1024) + ' MB';

  S := GetConfigSetting(VMS.VMarr[VMindex].Cfg_path, 'gfxcard');
  S := GetConfigSetting(Paths.sysinfo, S);
  if S = '' then S := 'internal device';
  InfoForm.LabelDisplayCard.Caption := S;

  S := GetConfigSetting(VMS.VMarr[VMindex].Cfg_path, 'sndcard');
  S := GetConfigSetting(Paths.sysinfo, S);
  if S = '' then S := 'no sound card';
  InfoForm.LabelSoundCard.Caption := S;

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
  OptionsForm.CheckGroup1.Checked[0] := (noconfirm=1);
  OptionsForm.CheckGroup1.Checked[1] := (fullscreen=1);
  OptionsForm.TrackBarBackups.Position := nrbackups;
  OptionsForm.TrackBarHDBackupSize.Position := hdbackupsize;
  OptionsForm.ShowModal;
  if OptionsForm.CheckGroup1.Checked[0] then noconfirm:=1 else noconfirm:=0;
  if OptionsForm.CheckGroup1.Checked[1] then fullscreen:=1 else fullscreen:=0;
  nrbackups := OptionsForm.TrackBarBackups.Position;
  hdbackupsize := OptionsForm.TrackBarHDBackupSize.Position;
  SaveConfig;
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
begin
  ListNotEmpty := ConfigListBox.Count > 0;
  SpeedButtonSettings.Enabled := ListNotEmpty;
  SpeedButtonRename.Enabled := ListNotEmpty;
  SpeedButtonCopy.Enabled := ListNotEmpty;
  SpeedButtonDelete.Enabled := ListNotEmpty;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  Paths := TPaths.Create;
  VMs := TVMs.Create;
  UpdateList;
  PrintVMarr;
end;

destructor TMainForm.Destroy;
begin
  Paths.Free;
  VMs.Free;
  inherited;
end;



end.

