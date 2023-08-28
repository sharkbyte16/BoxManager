unit Data86box;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, StrUtils;

const
  APPDIR = '~/.boxmanager/';
  VMDIR = APPDIR + 'VMs/';
  VMEXT = '.cfg';
  CFGFILE = 'config.txt';


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
procedure SaveConfig(ConfigFilename : string; PathData : TPathData);

implementation

constructor TPathData.Create;
var OK, binpathOK, pathfileOK : boolean;
    binpath : string;
    F : Text;
begin
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
    if FileExists(SettingsDir+CFGFILE) then begin
      binpath := GetConfig(SettingsDir+CFGFILE, 'executable');
      binpathOK := FileExists(binpath);
      pathfileOK := binpathOK;
    end;
    if not binpathOK then begin
      binpath := FileSearch('86Box', '.:/bin/:/usr/bin:/usr/local/bin:') + '/86Box';
      binpathOK := FileExists(binpath);
    end;
    if not binpathOK then begin
      repeat
        InputQuery('86Box path','Please specify the full path of the 86Box binary', binpath);
        binpathOK := FileExists(binpath);
      until binpathOK;
    end;
    if not pathfileOK then begin
      Assign(F, SettingsDir+CFGFILE);
      Rewrite(F);
      Writeln(F, binpath);
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
  FullScreen := LowerCase(GetConfig(SettingsDir+CFGFILE, 'fullscreen')) = 'true';
  NoConfirm := LowerCase(GetConfig(SettingsDir+CFGFILE, 'noconfirm')) = 'true';
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

procedure SaveConfig(ConfigFilename : string; PathData : TPathData);
var
  F : Text;
  S, Value : string;
begin
  Assign(F, ConfigFilename);
  Rewrite(F);
  Writeln(F, 'executable = '+PathData.GetBinaryPath);

  Writeln(F, 'fullscreen = '+Bool2Str(PathData.FullScreen));
  Writeln(F, 'noconfirm = '+Bool2Str(PathData.NoConfirm));
  Close(F);
end;

end.

