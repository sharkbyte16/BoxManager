unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLType, StdCtrls, ExtCtrls,
  Buttons, FileUtil, LazUTF8, Data86box, Options;

type

  { TMainForm }

  TMainForm = class(TForm)
    ConfigListBox: TListBox;
    SpeedButtonAdd: TSpeedButton;
    SpeedButtonRename: TSpeedButton;
    SpeedButtonCopy: TSpeedButton;
    SpeedButtonDelete: TSpeedButton;
    SpeedButtonQuit: TSpeedButton;
    SpeedButtonLaunch: TSpeedButton;
    SpeedButtonSettings: TSpeedButton;
    SpeedButtonOptions: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButtonAddClick(Sender: TObject);
    procedure SpeedButtonCopyClick(Sender: TObject);
    procedure SpeedButtonDeleteClick(Sender: TObject);
    procedure SpeedButtonLaunchClick(Sender: TObject);
    procedure SpeedButtonOptionsClick(Sender: TObject);
    procedure SpeedButtonQuitClick(Sender: TObject);
    procedure SpeedButtonRenameClick(Sender: TObject);
    procedure SpeedButtonSettingsClick(Sender: TObject);
  private
    PathData : TPathData;
    procedure UpdateConfigurationList;
    function ExtractFilenameOnlyWithoutExt(AFileName: string): string;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  PathData := TPathData.Create;
end;


destructor TMainForm.Destroy;
begin
  PathData.Free;
  inherited;
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  UpdateConfigurationList;
end;


function TMainForm.ExtractFilenameOnlyWithoutExt(AFileName: string): string;
begin
  Result := ChangeFileExt(ExtractFileName(AFileName), '');
end;


procedure TMainForm.UpdateConfigurationList;
var
  SearchRec: TSearchRec;
  Result: Integer;
begin
  ConfigListBox.Clear;
  Result := FindFirst(PathData.GetVMConfigsPath+'*', faDirectory, SearchRec);
  try
    while Result = 0 do
    begin
      if ((SearchRec.Name <> '.') and (SearchRec.Name <> '..')) then begin
        if FileExists(PathData.GetVMConfigsPath+SearchRec.Name+'/'+ExtractFilenameOnlyWithoutExt(SearchRec.Name)+'.cfg') then
          ConfigListBox.Items.Add(SearchRec.Name);
      end;
      Result := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
  if ConfigListBox.Items.Count > 0 then begin
    ConfigListBox.ItemIndex := 0;
    SpeedButtonRename.Enabled := True;
    SpeedButtonCopy.Enabled := True;
    SpeedButtonDelete.Enabled := True;
    SpeedButtonSettings.Enabled := True;
    SpeedButtonLaunch.Enabled := True;
  end;
end;


procedure TMainForm.SpeedButtonAddClick(Sender: TObject);
var
  NewName, NewDir, NewFileName, CmdLine : string;
  ListWasEmpty, OK : boolean;
begin
  ListWasEmpty := (ConfigListBox.Items.Count = 0);
  NewName := '';
  InputQuery('New config','Enter name:', NewName);
  NewDir := PathData.GetVMConfigsPath+NewName+'/';
  NewFileName := NewDir+NewName+VMEXT;
  OK := True;
  if DirectoryExists(NewDir) then begin
    ShowMessage(NewDir+' already exists');
    OK := False;
  end;
  if FileExists(NewFileName) then begin
    ShowMessage(NewName+' already exists');
    OK := False;
  end;
  if OK then OK := CreateDir(NewDir);
  if OK then begin
    CmdLine := '--settings "'+NewFileName+'"';
    ExecuteProcess(UTF8ToSys(PathData.GetBinaryPath), UTF8ToSys(CmdLine));
  end;
  if FileExists(NewFileName) then ConfigListBox.Items.Add(NewName)
    else RemoveDir(NewDir);
  if ListWasEmpty then begin
    ConfigListBox.ItemIndex := 0;
    SpeedButtonRename.Enabled := True;
    SpeedButtonCopy.Enabled := True;
    SpeedButtonDelete.Enabled := True;
    SpeedButtonSettings.Enabled := True;
    SpeedButtonLaunch.Enabled := True;
  end;
end;


procedure TMainForm.SpeedButtonRenameClick(Sender: TObject);
var
  Item, OldDirName, NewDirOldFile, NewName, NewDirName, NewFileName : string;
  ItemIndex : integer;
begin
  ItemIndex := ConfigListBox.ItemIndex;
  if ItemIndex >= 0 then begin
    Item := ConfigListBox.Items[ItemIndex];
    OldDirName := PathData.GetVMConfigsPath+Item+'/';
    NewName := Item;
    InputQuery('New name','Enter name:', NewName);
    NewDirName := PathData.GetVMConfigsPath+NewName+'/';
    NewDirOldFile :=  NewDirName+Item+VMEXT;
    NewFileName := NewDirName+NewName+VMEXT;
    if FileExists(NewFileName) then begin
      ShowMessage(NewName+' already exists')
    end
    else begin
      if RenameFile(OldDirName, NewDirName) then
        if RenameFile(NewDirOldFile, NewFileName) then ConfigListBox.Items[ItemIndex] := NewName
          else ShowMessage('Error renaming file '+NewDirOldFile)
      else
        ShowMessage('Error renaming directory '+OldDirName);
    end;
  end;
end;


procedure TMainForm.SpeedButtonCopyClick(Sender: TObject);
var
  Item, CfgFile, NewName, NewDir,NewFileName : string;
  ItemIndex : integer;
  OK : boolean;
begin
  ItemIndex := ConfigListBox.ItemIndex;
  if ItemIndex >= 0 then begin
    Item := ConfigListBox.Items[ItemIndex];
    CfgFile := PathData.GetVMConfigsPath+Item+'/'+Item+VMEXT;
    NewName := Item+' (Copy)';
    InputQuery('New name','Enter name:', NewName);
    NewDir := PathData.GetVMConfigsPath+NewName+'/';
    NewFileName := NewDir+NewName+VMEXT;
    OK := True;
    if DirectoryExists(NewDir) then begin
      ShowMessage(NewDir+' already exists');
      OK := False;
    end;
    if FileExists(NewFileName) then begin
      ShowMessage(NewName+' already exists');
      OK := False;
    end;
    if OK then OK := CreateDir(NewDir);
    if OK then begin
      if CopyFile(CfgFile, NewFileName) then
        ConfigListBox.Items.Add(NewName)
      else begin
        RemoveDir(NewDir);
        ShowMessage('Error copying file "'+CfgFile+'".');
      end;
    end;
  end;
end;


procedure TMainForm.SpeedButtonDeleteClick(Sender: TObject);
var
  Item, CfgFile, Dir : string;
  ItemIndex : integer;
begin
  ItemIndex := ConfigListBox.ItemIndex;
  if ItemIndex >= 0 then begin
    Item := ConfigListBox.Items[ItemIndex];
    Dir := PathData.GetVMConfigsPath+Item+'/';
    CfgFile := Dir+Item+VMEXT;
    if Application.MessageBox(Pchar('Do you want to delete "'+Item+'"?'),
                              Pchar('Delete confirmation'),
                              MB_ICONQUESTION + MB_YESNO) = IDYES then
    begin
      if not DeleteFile(CfgFile) then ShowMessage('Error deleting "'+CfgFile+'".')
        else ConfigListBox.Items.Delete(ItemIndex);
      if not RemoveDir(Dir) then ShowMessage('Error deleting "'+Dir+'".')
    end;
  end;
  if ConfigListBox.Items.Count = 0 then begin
    SpeedButtonRename.Enabled := False;
    SpeedButtonCopy.Enabled := False;
    SpeedButtonDelete.Enabled := False;
    SpeedButtonSettings.Enabled := False;
    SpeedButtonLaunch.Enabled := False;
  end;
end;


procedure TMainForm.SpeedButtonLaunchClick(Sender: TObject);
var
  Item, CfgFile, CmdLine, FullScr, NoConfrm : string;
  ItemIndex : integer;
begin
  ItemIndex := ConfigListBox.ItemIndex;
  if ItemIndex >= 0 then begin
    Item := ConfigListBox.Items[ItemIndex];
    CfgFile := PathData.GetVMConfigsPath+Item+'/'+Item+VMEXT;
    if FileExists(CfgFile) then begin
      if PathData.FullScreen then FullScr := '--fullscreen ' else FullScr := '';
      if PathData.FullScreen then NoConfrm := '--noconfirm ' else NoConfrm := '';
      CmdLine := FullScr+NoConfrm+'"'+CfgFile+'"';
      ExecuteProcess(UTF8ToSys(PathData.GetBinaryPath), UTF8ToSys(CmdLine));
    end
    else
      ShowMessage(CfgFile+' not found.');
  end;
end;


procedure TMainForm.SpeedButtonSettingsClick(Sender: TObject);
var
  Item, CfgFile, CmdLine : string;
  ItemIndex : integer;
begin
  ItemIndex := ConfigListBox.ItemIndex;
  if ItemIndex >= 0 then begin
    Item := ConfigListBox.Items[ItemIndex];
    CfgFile := PathData.GetVMConfigsPath+Item+'/'+Item+VMEXT;
    if FileExists(CfgFile) then begin
      CmdLine := '--settings "'+CfgFile+'"';
      ExecuteProcess(UTF8ToSys(PathData.GetBinaryPath), UTF8ToSys(CmdLine));
    end
    else
      ShowMessage(CfgFile+' not found.');
  end;
end;


procedure TMainForm.SpeedButtonOptionsClick(Sender: TObject);
begin
  OptionsForm.CheckBoxFullScreen.Checked := PathData.FullScreen;
  OptionsForm.CheckBoxNoConfirm.Checked := PathData.NoConfirm;
  OptionsForm.ShowModal;
  PathData.FullScreen := OptionsForm.CheckBoxFullScreen.Checked;
  PathData.NoConfirm := OptionsForm.CheckBoxNoConfirm.Checked;
  SaveConfig(PathData.GetSettingsPath+CFGFILE, PathData);
end;


procedure TMainForm.SpeedButtonQuitClick(Sender: TObject);
begin
  MainForm.Close;
end;


end.

