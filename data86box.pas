unit Data86box;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, StrUtils;

const
  VMEXT = '.cfg';

type
  { TPathData }
  TPathData = class
      FullScreen : boolean;
      NoConfirm : boolean;
    private
      SettingsDir : string;
      BinaryPath : string;
      VMConfigsDir : string;
    public
      constructor Create;
      function GetSettingsPath: string;
      function GetBinaryPath: string;
      function GetVMConfigsPath: string;
  end;

function GetConfig(ConfigFilename : string; SettingName : string) : string;
procedure SaveConfig(PathData : TPathData);

implementation

const
  {$IFDEF MSWINDOWS}
    HOMEDIR = '~/';
  {$ENDIF}
  {$IFDEF UNIX}
    HOMEDIR = 'C:\';
  {$ENDIF}
  APPDIR_DEF = HOMEDIR + '.boxmanager/';
  VMDIR_DEF = APPDIR_DEF + 'VMs/';
  CFGFILE_DEF = APPDIR_DEF + 'BoxManager.cfg';

var
  APPDIR : string = APPDIR_DEF;
  VMDIR : string  = VMDIR_DEF;
  CFGFILE : string  = CFGFILE_DEF;

constructor TPathData.Create;
var OK, binpathOK, pathfileOK : boolean;
    binpath : string;
    F : Text;
    OpenDialog : TOpenDialog;
begin
  APPDIR := GetAppConfigDir(False);
  VMDIR := APPDIR + 'VMs/';
  CFGFILE := APPDIR + 'BoxManager.cfg';
  SettingsDir := ExpandFileName(APPDIR);
  VMConfigsDir := ExpandFileName(VMDIR);
  // config file
  OK := DirectoryExists(SettingsDir);
  if not OK then OK := CreateDir(SettingsDir);
  if not OK then ShowMessage('Failed to create directory '+SettingsDir);
  // get or set binary 86Box path from config file
  pathfileOK := False;
  if OK then begin
    binpathOK := False;
    if FileExists(CFGFILE) then begin
      binpath := GetConfig(CFGFILE, 'executable');
      binpathOK := FileExists(binpath);
      pathfileOK := binpathOK;
    end;
    if not binpathOK then begin
      {$IFDEF MSWINDOWS}
        binpath := FileSearch('86Box.exe', '.:C:\:C:\Program Files:C:\Program Files (x86):') + '\86Box';
      {$ENDIF}
      {$IFDEF UNIX}
        binpath := FileSearch('86Box', '.:/bin/:/usr/bin:/usr/local/bin:') + '/86Box';
      {$ENDIF}
      binpathOK := FileExists(binpath);
    end;
    if not binpathOK then begin
      ShowMessage('Could not find the 86Box executable, please specify.');
      repeat
        OpenDialog := TOpenDialog.Create(nil);
        OpenDialog.Title := 'Please specify the full path of the 86Box binary';
        OpenDialog.Options := [ofFileMustExist,ofEnableSizing,ofViewDetail];
        OpenDialog.Execute;
        binpath := OpenDialog.FileName;
        binpathOK := FileExists(binpath);
      until binpathOK;
    end;
    if not pathfileOK then begin
      Assign(F, CFGFILE);
      Rewrite(F);
      Writeln(F, 'executable = '+binpath);
      Close(F);
    end;
  end;
  BinaryPath := binpath;
  // VM configs dir
  if OK then begin
    OK := DirectoryExists(VMConfigsDir);
    if not OK then OK := CreateDir(VMConfigsDir);
    if not OK then ShowMessage('Failed to create directory '+VMConfigsDir);
  end;
  FullScreen := LowerCase(GetConfig(CFGFILE, 'fullscreen')) = 'true';
  NoConfirm := LowerCase(GetConfig(CFGFILE, 'noconfirm')) = 'true';
end;

function TPathData.GetSettingsPath: string;
begin
  Result := SettingsDir;
end;

function TPathData.GetBinaryPath: string;
begin
  Result := BinaryPath;
end;

function TPathData.GetVMConfigsPath: string;
begin
  Result := VMConfigsDir;
end;


function GetConfig(ConfigFilename : string; SettingName : string) : string;
var
  F : Text;
  S, Value : string;
  Delims : TSysCharSet;
begin
  Delims := [' '];
  Assign(F, ConfigFilename);
  Reset(F);
  Value := '';
  while not EOF(F) do begin
    Readln(F, S);
    if ExtractWord(1, S, Delims) = SettingName then Value := ExtractWord(3, S, Delims);
  end;
  Close(F);
  Result := Value;
end;

function Bool2Str(bool : boolean) : string;
begin
  if bool then Result := 'True' else Result := 'False';
end;

procedure SaveConfig(PathData : TPathData);
var
  F : Text;
begin
  Assign(F, CFGFILE);
  Rewrite(F);
  Writeln(F, 'executable = '+PathData.GetBinaryPath);
  Writeln(F, 'fullscreen = '+Bool2Str(PathData.FullScreen));
  Writeln(F, 'noconfirm = '+Bool2Str(PathData.NoConfirm));
  Close(F);
end;

end.

