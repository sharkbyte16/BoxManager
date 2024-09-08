unit MainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, MaskEdit,
  Buttons, Menus, VMsystem, OptionsWindow, LazLogger;

type

  { TMainForm }

  TMainForm = class(TForm)
    ConfigListBox: TListBox;
    MenuItemRun: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemDelete: TMenuItem;
    Separator1: TMenuItem;
    MenuItemFitWindow: TMenuItem;
    MenuItemRename: TMenuItem;
    PopupMenu1: TPopupMenu;
    SpeedButtonLaunch: TSpeedButton;
    SpeedButtonSettings: TSpeedButton;
    SpeedButtonOptions: TSpeedButton;
    SpeedButtonAdd: TSpeedButton;
    SpeedButtonRename: TSpeedButton;
    SpeedButtonCopy: TSpeedButton;
    SpeedButtonDelete: TSpeedButton;
    SpeedButtonQuit: TSpeedButton;
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
  Result := VMs.IndexOfVM(ConfigListBox.Items[ConfigListBox.ItemIndex].Replace(' (86Box)', '').Replace(' (PCem)', ''));
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
  OK: boolean;
begin
  OK := True;
  NewName := 'New VM';
  case QuestionDlg('Target PC emulator',
                   'In which PC emulator do you want to create VM?',
                    mtInformation,
                    [100, 'PCem', 101, '86Box', 'IsDefault'], '') of
    100:  TargetEmulator := vm_pcem;
    101: TargetEmulator := vm_86box;
    mrCancel: OK := False;
  end;
  if OK then begin
    if InputQuery('Name for new VM','Enter name:', NewName) then begin
       VMs.NewVM(NewName, TargetEmulator);
       UpdateList;
    end;
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
  SelectedItemIndex := VMs.IndexOfVM(ConfigListBox.Items[ConfigListBox.ItemIndex].Replace(' (86Box)', '').Replace(' (PCem)', ''));
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
  Em, S : String;
begin
  ConfigListBox.Items.Clear;
    for i := 0 to Length(VMs.VMarr)-1 do begin
      with VMs.VMarr[i] do begin
        Em := EmulatorStr[Ord(Emulator)];
        S := VMname+' ('+Em+')';
        ConfigListBox.Items.Add(S);
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

