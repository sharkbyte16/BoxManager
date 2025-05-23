unit VMsystem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Process, Dialogs,
  FileUtil, LazFileUtils, Controls, LazLoggerBase; //LazLoggerDummy;

const
  APPNAME='BoxManager2';
  {$IFDEF MSWINDOWS}
    SLASH = '\';
  {$ENDIF}
  {$IFDEF UNIX}
    SLASH = '/';
  {$ENDIF}
  DEFAULT_NRBAK = 5;
  DEFAULT_HDBAKSIZE = 100;
  NOHDBAKSIZE = 1001;

type
  TPaths = class
    cfg_boxmanager : string;             // full path string to BoxManager.cfg
    sysinfo : string;                    // full path string to sysinfo.txt
    exe_86box : string;                  // full path string 86box executable
    exe_86box_dir : string;              // directory path string 86box executable
    dir_vm : string;                     // directory path string location VM's
    dir_vm_86box : string;               // dir_vm/86box
    private
    public
      constructor Create;
  end;

  TEmulator = (vm_86box);

  TStorageEntry = (
      // harddisk entries in 86box machine config:
      hdd_01_fn, hdd_02_fn, hdd_03_fn, hdd_04_fn, hdd_05_fn, hdd_06_fn, hdd_07_fn, hdd_08_fn);

  TStorage = record
    StorageEntry: TStorageEntry;         // use WriteStr(S, StorageEntry) to get string
    Path: string;                        // full path string to image file
    SizeMB: double;                     // Size in MB
  end;

  TStorageArray = array of TStorage;

  TVMobject = class                      // all info for a virtual machine config
    Emulator: TEmulator;
    VMname: string;
    Cfg_path: string;
    Nvr_path: string;
    Info_path: string;
    Storage: TStorageArray;
    Tulip_DGA: string;
  private
  public
    procedure GetStorageFromCfg;
  end;

  TVMs = class
    VMarr: array of TVMobject;
  private
  public
    constructor Create;
    procedure LaunchVM(index: integer);
    procedure RenameVM(index: integer; NewName: string);
    procedure CopyVm(index: integer; NewName: string);
    procedure DeleteVm(index: integer);
    procedure ConfigureVM(index: integer);
    procedure BackupVM(index: integer);
    procedure NewVM(NewName: string; TargetEm: TEmulator);
    function IndexOfVM(VMname : string): integer;
  end;


function GetConfigSetting(ConfigFilename : string; Section : string; SettingName : string) : string;
procedure SetConfigSetting(ConfigFilename : string; Section : string; SettingName : string; NewValue : string);
function ConfigSettingSectionExists(ConfigFilename : string; Section : string) : boolean;
procedure ModifyConfigFilePath(const configFile, oldVMname, newVMname: string);
procedure PrintVMarr;
procedure SaveConfig;

var
  Paths : TPaths;
  VMs : TVMs;
  // settings
  nrbackups, hdbackupsize : integer;
  noconfirm, fullscreen, simulate_dga : integer;
  other_VM_settings : string;
  exe_86box_present : boolean;

implementation

constructor TPaths.Create;
var
  cfg_dir : String;
  OK, binpathOK : boolean;
  F : Text;
  OpenDialog : TOpenDialog;
begin
  DebugLnEnter('[Paths]');
  cfg_dir        := GetAppConfigDir(False);
  cfg_boxmanager := cfg_dir + 'BoxManager.cfg';
  sysinfo        := cfg_dir + 'sysinfo.txt';

  // open config file or create
  OK := DirectoryExists(cfg_dir);
  if not OK then OK := CreateDir(cfg_dir);
  if not OK then ShowMessage('Failed to create directory '+cfg_dir);
  DebugLn('cfg_dir = ' + cfg_dir);
  OK := FileExists(cfg_boxmanager);
  if not OK then begin
    try
      AssignFile(F, cfg_boxmanager);
      Rewrite(F);
      CloseFile(F);
    except
      on E: EInOutError do ShowMessage('Failed to create config file: '+cfg_boxmanager);
    end;
  end;
  DebugLn('cfg_boxmanager = ' + cfg_boxmanager);
  DebugLn('sysinfo = ' + sysinfo);

  // get or set binary 86Box path from config file
  binpathOK := False;
  exe_86box := GetConfigSetting(cfg_boxmanager, '', 'exe_86box');
  if exe_86box = 'not_present' then begin
    binpathOK := True;
    exe_86box_present := False;
  end
  else begin
    binpathOK := FileExists(exe_86box);
    exe_86box_present := True;
  end;
  if not binpathOK then begin
    ShowMessage('Could not find the 86Box executable, please specify.');
    repeat
      OpenDialog := TOpenDialog.Create(nil);
      OpenDialog.Title := 'Please specify the full path of the 86Box binary';
      OpenDialog.Options := [ofFileMustExist,ofEnableSizing,ofViewDetail];
      OpenDialog.Execute;
      exe_86box := OpenDialog.FileName;
      if FileExists(exe_86box) then begin
        binpathOK := True;
        exe_86box_present := True;
      end
      else begin
        binpathOK := (MessageDlg('Question', 'Do you wish to continue without 86Box present? BoxManager will have no value without it...',
                                    mtConfirmation, [mbYes, mbNo],0) = mrYes);
        exe_86box := 'not_present';
        exe_86box_present := False;
      end;
    until binpathOK;
  end;
  exe_86box_dir := ExtractFilePath(exe_86box);
  DebugLn(['exe_86box_present = ', exe_86box_present]);
  DebugLn('exe_86box_dir = ' + exe_86box_dir);
  DebugLn('exe_86box = ' + exe_86box);

  // get settings
  if not TryStrToInt(GetConfigSetting(cfg_boxmanager, '', 'nrbackups'), nrbackups) then
     nrbackups := DEFAULT_NRBAK;
  if nrbackups < 0 then nrbackups := 0;
  DebugLn(['nrbackups = ', nrbackups]);

  if not TryStrToInt(GetConfigSetting(cfg_boxmanager, '', 'hdbackupsize'), hdbackupsize) then
     hdbackupsize := DEFAULT_HDBAKSIZE;
  if hdbackupsize < 0 then hdbackupsize := 0;
  DebugLn(['hdbackupsize = ', hdbackupsize]);

  if not TryStrToInt(GetConfigSetting(cfg_boxmanager, '', 'noconfirm'), noconfirm) then
     noconfirm := 0;
  if noconfirm < 0 then noconfirm := 0;
  DebugLn(['noconfirm = ', noconfirm]);

  if not TryStrToInt(GetConfigSetting(cfg_boxmanager, '', 'fullscreen'), fullscreen) then
     fullscreen := 0;
  if fullscreen < 0 then fullscreen := 0;
  DebugLn(['fullscreen = ', fullscreen]);

  if not TryStrToInt(GetConfigSetting(cfg_boxmanager, '', 'simulate_dga'), simulate_dga) then
     simulate_dga := 0;
  if simulate_dga < 0 then simulate_dga := 0;
  DebugLn(['simulate_dga = ', simulate_dga]);

  other_VM_settings := GetConfigSetting(cfg_boxmanager, '', 'other_VM_settings');
  DebugLn(['other_VM_settings = ', other_VM_settings]);

  // refresh config file
  DebugLn(['Refreshing config file.']);
  AssignFile(F, cfg_boxmanager);
  Rewrite(F);
  Writeln(F, 'exe_86box = ' + exe_86box);
  Writeln(F, 'nrbackups = ', nrbackups);
  Writeln(F, 'hdbackupsize = ', hdbackupsize);
  Writeln(F, 'noconfirm = ', noconfirm);
  Writeln(F, 'fullscreen = ', fullscreen);
  Writeln(F, 'simulate_dga = ', simulate_dga);
  Writeln(F, 'other_VM_settings = ', other_VM_settings);
  Close(F);

  // VM configs dir
  dir_vm         := cfg_dir + 'VMs/';
  // override default div_vm path with command line argument
  if paramCount() > 0 then dir_vm := paramStr(1);
  OK := DirectoryExists(dir_vm);
  if not OK then OK := CreateDir(dir_vm);
  if not OK then ShowMessage('Failed to create directory '+dir_vm);
  DebugLn('dir_vm = ' + dir_vm);

  // 86Box VM configs dir
  dir_vm_86box   := dir_vm + '86box/';
  if OK then begin
    OK := DirectoryExists(dir_vm_86box);
    if not OK then OK := CreateDir(dir_vm_86box);
    if not OK then ShowMessage('Failed to create directory '+dir_vm_86box);
  end;
  DebugLn('dir_vm_86box = ' + dir_vm_86box);
  DebugLnExit;
end;

procedure SaveConfig;
var
  F : Text;
begin
  with Paths do begin
    DebugLn(['Saving config file.']);
    AssignFile(F, cfg_boxmanager);
    Rewrite(F);
    Writeln(F, 'exe_86box = ' + exe_86box);
    Writeln(F, 'nrbackups = ', nrbackups);
    Writeln(F, 'hdbackupsize = ', hdbackupsize);
    Writeln(F, 'noconfirm = ', noconfirm);
    Writeln(F, 'fullscreen = ', fullscreen);
    Writeln(F, 'simulate_dga = ', simulate_dga);
    Writeln(F, 'other_VM_settings = ', other_VM_settings);
    Close(F);
  end;
end;

procedure TVMobject.GetStorageFromCfg;
var
  St : TStorage;
  S, HDPath : String;
  hdd : TStorageEntry;
  F : file;
begin
  // ensure we start with an empty Storage array
  SetLength(Storage, 0);
  // then fill from cfg entries
  for hdd in TStorageEntry do begin
    St.StorageEntry := hdd;
    WriteStr(S, St.StorageEntry);
    St.Path := GetConfigSetting(Cfg_path, 'Hard disks', S);
    HDPath := ExtractFileDir(St.Path);
    if HDPath = '' then HDPath := ExtractFilePath(Cfg_path)+St.Path else HDPath := St.Path;
    St.SizeMB := 0;
    if St.Path <> '' then begin
      {$I-}
      Assign (F, HDPath);
      Reset (F);
      {$I+}
      if IOResult = 0 then begin
        St.SizeMB := (FileSize(F)*128) / 1048576; // FileSize default record size is 128 bytes
        Close (F);
      end;
      SetLength(Storage, Length(Storage)+1);
      Storage[Length(Storage)-1] := St;
    end;
  end;
end;

constructor TVMs.Create;
var
  SearchRec : TSearchRec;
  Result : Integer;
  PN, FN, S : String;
  VMobject : TVMobject;
  Machine, NVR, INFO : String;
  i : integer;
  F : Text;
  tulipdga : string;
begin
  // Look for 86Box VMs
  if exe_86box_present then begin
    Result := FindFirst(Paths.dir_vm_86box + '*', faDirectory, SearchRec);
    try
      while Result = 0 do
      begin
        if ((SearchRec.Name <> '.') and (SearchRec.Name <> '..')) then begin
          PN := Paths.dir_vm_86box + SearchRec.Name;
          FN := PN + '/'+ SearchRec.Name + '.cfg';
          INFO := PN + '/'+ SearchRec.Name + '.info';
          if FileExists(FN) then begin
            Machine := GetConfigSetting(FN, 'Machine', 'machine');
            NVR := PN + '/nvr/'+ Machine + '.nvr';
            VMobject := TVMobject.Create;
            with VMobject do begin
              Emulator := vm_86box;
              VMname := SearchRec.Name;
              Cfg_path := FN;
              Nvr_path := NVR;
              Info_path := INFO;
              GetStorageFromCfg;
            end;
            SetLength(VMarr, Length(VMarr)+1);
            VMarr[Length(VMarr)-1] := VMobject;
          end;
        end;
        Result := FindNext(SearchRec);
      end;
    finally
      FindClose(SearchRec);
    end;
  end; // exe_86box_present
  // Check for Tulip DGA card
  // possible values tulipdga = ['present', 'none']
  for i := 0 to Length(VMarr)-1 do begin
    if ConfigSettingSectionExists(VMarr[i].Cfg_path, 'Tulip DGA') then begin
      S := GetConfigSetting(VMarr[i].Cfg_path, 'Tulip DGA', 'tulipdga');
      // Current value consistency
      if ConfigSettingSectionExists(VMarr[i].Cfg_path, 'Hercules')
        and ConfigSettingSectionExists(VMarr[i].Cfg_path, 'IBM CGA')
        and (
             (GetConfigSetting(VMarr[i].Cfg_path, 'Video', 'gfxcard') = 'hercules')
             or
             (GetConfigSetting(VMarr[i].Cfg_path, 'Video', 'gfxcard') = 'cga'))
        then S := 'present' else S := 'none';
      SetConfigSetting(VMarr[i].Cfg_path, 'Tulip DGA', 'tulipdga', S);
      VMarr[i].Tulip_DGA := S;
    end
    else begin
      // Create [Tulip DGA] section
      if (ConfigSettingSectionExists(VMarr[i].Cfg_path, 'Hercules')
        and ConfigSettingSectionExists(VMarr[i].Cfg_path, 'IBM CGA'))
        then S := 'present' else S := 'none';
      tulipdga := 'tulipdga = '+S;
      {$i-}
      Assign (F,VMarr[i].Cfg_path);
      Append (F);
      {$I+}
      if (IoResult=0) then begin
        Writeln(F);
        Writeln(F, '[Tulip DGA]');
        Writeln(F, tulipdga);
      end;
      Close(F);
      VMarr[i].Tulip_DGA := tulipdga;
    end;
  end;
end;

procedure TVMs.LaunchVM(index: integer);
var
  Exe_path : string;
  proc: TProcess;
begin
  if not (index > Length(VMarr)-1) then begin
    with VMarr[index] do begin
      if Emulator = vm_86box then Exe_path := Paths.exe_86box;
      proc := TProcess.Create(nil);
      proc.Executable := Exe_path;
      proc.Parameters.Add('--config');
      proc.Parameters.Add(Cfg_path);
      if ((Emulator = vm_86box) and (noconfirm = 1)) then proc.Parameters.Add('--noconfirm');
      if (fullscreen = 1) then proc.Parameters.Add('--fullscreen');
      proc.Parameters.Add(other_VM_settings);
      proc.Options := proc.Options - [poWaitOnExit];
      proc.Execute;
      proc.Free;
    end;
  end
  else begin
    ShowMessage('VM index out of range.');
  end;
end;

procedure TVMs.RenameVM(index: integer; NewName: string);
var
  OldName : string;
  dir_vm : string;
  machine : string;
begin
  if (index > Length(VMarr)-1) then begin
    ShowMessage('VM index out of range.');
    Exit;
  end;
  with VMarr[index] do begin
    OldName := VMname;
    dir_vm := Paths.dir_vm_86box;
    if RenameFile(dir_vm + OldName, dir_vm + NewName) then begin
       if RenameFile(dir_vm + NewName + '/' + OldName + '.cfg', dir_vm + NewName + '/' + NewName + '.cfg') then begin
          Cfg_path := dir_vm + NewName + '/' + NewName + '.cfg';
          ModifyConfigFilePath(Cfg_path, OldName, NewName);
       end;
       machine := GetConfigSetting(Cfg_path, 'Machine', 'machine');
       if not (machine = '') then begin
         Nvr_path := dir_vm + NewName + '/nvr/' + machine + '.nvr';
       end;
       GetStorageFromCfg;
       VMname := NewName;
    end;
  end;
end;

procedure TVMs.CopyVm(index: integer; NewName: string);
var
  SourceName, SourceDir, SourceCfg, machine, dir_vm : string;
  NewDir, NewCfg, NewNvr : string;
  InputOK, CopyOK, CfgOK, NvrOK : boolean;
  VMobject : TVMobject;
  SourceEm: TEmulator;
begin
  if (index > Length(VMarr)-1) then begin
    ShowMessage('VM index out of range.');
    Exit;
  end;

  with VMarr[index] do begin
    // source info
    SourceName := VMname;
    SourceCfg := Cfg_path;
    SourceEm := Emulator;
    dir_vm := Paths.dir_vm_86box;
    machine := GetConfigSetting(SourceCfg, 'Machine', 'machine');
    SourceDir := dir_vm + SourceName + '/';
    DebugLn(['CopyVM: ', VMname, ' --> ', NewName]);

    // copy the vm
    InputOK := True;
    CopyOK := False;
    CfgOK := False;
    NvrOK := False;

    NewDir := dir_vm + NewName + '/';
    if DirectoryExists(ExcludeTrailingPathDelimiter(NewDir)) then begin
      ShowMessage(NewDir+' already exists');
      InputOK := False;
    end;
    DebugLn(['InputOK = ', InputOK]);

    if InputOK then begin
       CopyOK := CopyDirTree(SourceDir, NewDir);
    end;
    DebugLn(['CopyOK = ', CopyOK]);

    if CopyOK then begin
      CfgOk := RenameFile(NewDir+SourceName+'.cfg', NewDir+NewName+'.cfg');
      NewCfg := NewDir+NewName+'.cfg';
    end;
    DebugLn(['CfgOK = ', CfgOK]);

    if CfgOK then begin
      ModifyConfigFilePath(NewCfg, SourceName, NewName);
      NewNvr := NewDir + 'nvr/' + machine + '.nvr';
      NvrOK := True;
    end;
    DebugLn(['NvrOK = ', NvrOK]);

  end; // with VMarr[index]

  if NvrOK then begin
  // everything copied, now make VMobject
  VMobject := TVMobject.Create;
   with VMobject do begin
     Emulator := SourceEm;
     VMname := NewName;
     Cfg_path := NewCfg;
     Nvr_path := NewNvr;
     GetStorageFromCfg;
   end;
   SetLength(VMarr, Length(VMarr)+1);
   VMarr[Length(VMarr)-1] := VMobject;
   DebugLn(['Copy successful.']);
  end
  else begin
    // Copy failed, clean up and warn
    DeleteDirectory(NewDir, False);
    DeleteFile(NewNvr);
    ShowMessage('Creating the copy from '+SourceName+' failed.');
  end;
end;

procedure TVMs.DeleteVm(index: integer);
var
  vm_dir: string;
  vm_name: string;
begin
  if (index > Length(VMarr)-1) then begin
    ShowMessage('VM index out of range.');
    Exit;
  end;

  if MessageDlg('Question', 'Are you sure you want to delete the VM? All the VM files will be lost!',
                mtConfirmation, [mbYes, mbNo],0) = mrNo then Exit;
  vm_name := 'VM to delete';
  if InputQuery('Please confirm the name of the VM to delete','Enter name:', vm_name) then begin
     if vm_name <> VMarr[index].VMname then begin
        ShowMessage('No matching VM');
        Exit;
     end;
  end;
  with VMarr[index] do begin
    // first delete directory and nvr file
    vm_dir := Paths.dir_vm_86box + VMname + '/';
    DeleteDirectory(vm_dir, False);
    // now delete storage array
    Delete(Storage, 0, Length(Storage)-1);
  end; // with VMarr[index]

  // finally delte the VM from VMarr
  delete(VMarr, index, 1);
end;

procedure TVMs.ConfigureVM(index: integer);
var
  Exe_path : string;
  proc: TProcess;
  machine : string;
begin
  if not (index > Length(VMarr)-1) then begin
    with VMarr[index] do begin
      // first backup cfg
      BackupVM(index);
      // now (re)configure the VM
      machine := GetConfigSetting(Cfg_path, 'Machine', 'machine');
      Exe_path := Paths.exe_86box;
      proc := TProcess.Create(nil);
      proc.Executable := Exe_path;
      proc.Parameters.Add('--settings');
      proc.Parameters.Add(Cfg_path);
      proc.Options := proc.Options - [poWaitOnExit];
      proc.Execute;
      proc.Free;
      // update VMobject
      if not (GetConfigSetting(Cfg_path, 'Machine', 'machine') = machine) then begin
         DeleteFile(Nvr_path);
         Nvr_path := Paths.dir_vm_86box + VMname + '/nvr/' + machine + '.nvr';
         GetStorageFromCfg;

      end;
    end; // with VMarr[index]
  end // not out of range
  else begin
    ShowMessage('VM index out of range.');
  end;
end;

procedure TVMs.BackupVM(index: integer);
var
  TimeStamp, BAKdir, BackupDir : string;
  BakList : TStringList;
  SearchRec : TSearchRec;
  Result : Integer;
  i : integer;
  HD : TStorage;
  HDfile : file of byte;
  HDfilename : string;
  HDsizeMB : Integer;
begin
  // backup if we do backups and the cfg exists
  if (nrbackups > 0) and FileExistsUTF8(VMarr[index].Cfg_path) then begin
    // backup folder
    DateTimeToString(TimeStamp,'yyyymmdd_hhmmss',Now);
    BAKdir := ExtractFilePath(VMarr[index].Cfg_path) + 'bak/';
    if not DirectoryExists(BAKdir) then CreateDir(BAKdir);
    BackupDir := BAKdir + 'vmbackup_' + TimeStamp + '/';
    DebugLn('Creating '+BackupDir);
    if not DirectoryExists(BackupDir) then CreateDir(BackupDir);
    // cfg
    if not CopyFile(VMarr[index].Cfg_path, BackupDir + ExtractFileName(VMarr[index].Cfg_path)) then
       ShowMessage('Cannot backup cfg file. Please check directory access rights for '+ BackupDir);
    // nvr
    if FileExists(VMarr[index].Nvr_path) then begin
      if not CopyFile(VMarr[index].Nvr_path, BackupDir + ExtractFileName(VMarr[index].Nvr_path)) then
         ShowMessage('Cannot backup nvr file. Please check directory access rights for '+ BackupDir);
    end;
    // hard disk images
    for HD in VMarr[index].Storage do begin
      if ExtractFilePath(HD.Path) = '' then
        HDfilename := ExtractFilePath(VMarr[index].Cfg_path) + HD.Path
      else
        HDfilename := HD.Path;
      DebugLn(['Backing up ', HDfilename]);
      Assign(HDfile, HDfilename);
      Reset(HDfile);
      HDsizeMB := FileSize(HDfile) div 1048576;
      Close(HDfile);
      DebugLn(['HDsizeMB = ', HDsizeMB]);
      if hdbackupsize > 0 then begin
        if (HDsizeMB <= hdbackupsize) or (hdbackupsize = NOHDBAKSIZE) then begin
          if not CopyFile(HDfilename, BackupDir + ExtractFileName(HDfilename)) then
             ShowMessage('Cannot backup harddisk file. Please check directory access rights for '+ BackupDir);
        end;
      end;
    end;
    // info
    if FileExists(ExtractFilePath(VMarr[index].Cfg_path)+'os.txt') then begin
      if not CopyFile(ExtractFilePath(VMarr[index].Cfg_path)+'os.txt', BackupDir + 'os.txt') then
         ShowMessage('Cannot backup OS info file. Please check directory access rights for '+ BackupDir);
    end;
    if FileExists(ExtractFilePath(VMarr[index].Cfg_path)+'apps.txt') then begin
      if not CopyFile(ExtractFilePath(VMarr[index].Cfg_path)+'apps.txt', BackupDir + 'apps.txt') then
         ShowMessage('Cannot backup app info file. Please check directory access rights for '+ BackupDir);
    end;
  end;
  // cleanup number of backup folder if more than nrbackups, oldest first
  Result := FindFirst(BAKdir + '*', faDirectory, SearchRec);
  BakList := TStringList.Create;
  try
    while Result = 0 do
    begin
      if Copy(SearchRec.Name, 1, 9) = 'vmbackup_' then begin
         BakList.Add(BAKdir+SearchRec.Name);
      end;
      Result := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
  // Sort the list. With the time stamps in the name, the order will be old to new
  BakList.Sort;
  if BakList.Count > nrbackups then begin
    for i := 0 to BakList.Count - nrbackups - 1 do begin
      DeleteDirectory(BakList[i], false);
    end;
  end;
  BakList.Free
end;

procedure TVMs.NewVM(NewName: string; TargetEm: TEmulator);
var
  vm_base_dir, New_vm_dir : string;
  NameOK : boolean;
  VMobject : TVMobject;
begin
  // First create VM dir. If it fails, no need to create VMobject
  vm_base_dir := Paths.dir_vm_86box;
  New_vm_dir := vm_base_dir + NewName + '/';
  if DirectoryExists(ExcludeTrailingPathDelimiter(New_vm_dir)) then begin
    ShowMessage(New_vm_dir+' already exists');
    NameOK := False;
  end
  else begin
    CreateDir(New_vm_dir);
    NameOK := True;
  end;

  if NameOK then begin
    // create VMobject and add to VMarr
    VMobject := TVMobject.Create;
    with VMobject do begin
      Emulator := TargetEm;
      VMname := NewName;
      Cfg_path := New_vm_dir + NewName + '.cfg';
      // Nvr_path and Storage will be updated by Configure
    end;
    SetLength(VMarr, Length(VMarr)+1);
    VMarr[Length(VMarr)-1] := VMobject;

    // now run the Configure, this will create the .cfg and .nvr
    ConfigureVM(Length(VMarr)-1);
  end;
end;

function TVMs.IndexOfVM(VMname : string): integer;
var
  index, i : integer;
begin
  index := -1;
  for i := 0 to Length(VMarr)-1 do begin
    if VMarr[i].VMname = VMname then index := i;
  end;
  Result := index;
end;

function ConfigSettingSectionExists(ConfigFilename : string; Section : string) : boolean;
var
  F : Text;
  S, Sec : string;
  Exists : boolean;
begin
  Sec := '';
  Exists := False;
  {$i-}
  Assign (F,ConfigFilename);
  Reset (F);
  {$I+}
  if (IoResult=0) then begin
    while not EOF(F) do begin
      Readln(F, S);
      if Trim(S) <> '' then
         if Trim(S[1]) = '[' then begin
           Sec := Copy(Trim(S), 2, length(Trim(S)) - 2);
         end;
      Exists := (Sec = Section) or Exists;
    end;
    Close(F);
  end;
  Result := Exists;
end;

function GetConfigSetting(ConfigFilename : string; Section : string; SettingName : string) : string;
var
  F : Text;
  S, Sec, Value : string;
  Delims : TSysCharSet;
begin
  Value := '';
  Sec := '';
  Delims := ['='];
  {$i-}
  Assign (F,ConfigFilename);
  Reset (F);
  {$I+}
  if (IoResult=0) then begin
    while not EOF(F) do begin
      Readln(F, S);
      if Trim(S) <> '' then
         if Trim(S[1]) = '[' then begin
           Sec := Copy(Trim(S), 2, length(Trim(S)) - 2);
         end;
      if (Trim(ExtractDelimited(1, S, Delims)) = SettingName) and (Sec = Section) then
         Value := Trim(ExtractDelimited(2, S, Delims));
    end;
    Close(F);
  end;
  Result := Value;
end;

procedure SetConfigSetting(ConfigFilename : string; Section : string; SettingName : string; NewValue : string);
var
  Lines: TStringList;
  i: Integer;
  InSection: boolean;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(ConfigFilename);
    InSection := False;

    for i := 0 to Lines.Count - 1 do
    begin
      if Lines[i].Trim.StartsWith('[' + Section + ']') then
        InSection := True
      else if Lines[i].Trim.StartsWith('[') then
        InSection := False
      else if InSection and Lines[i].StartsWith(SettingName + ' = ') then
      begin
        Lines[i] := SettingName + ' = ' + NewValue;
        Break;
      end;
    end;
    Lines.SaveToFile(ConfigFilename);
  finally
    Lines.Free;
  end;
end;

procedure ModifyConfigFilePath(const configFile, oldVMname, newVMname: string);
var
  FileLines: TStringList;
  oldVMpath, newVMpath : string;
begin
  FileLines := TStringList.Create;
  oldVMpath := '/'+oldVMname+'/';
  newVMpath := '/'+newVMname+'/';
  try
    FileLines.LoadFromFile(configFile);
    FileLines.Text := StringReplace(FileLines.Text, oldVMpath, newVMpath, [rfReplaceAll]);
    FileLines.SaveToFile(configFile);
  finally
    FileLines.Free;
  end;
end;

procedure BackupCfg(Cfg_path : string);
var
  VMname, TimeStamp, BackupDir, BackupCfgFile : string;
  BakList : TStringList;
  SearchRec : TSearchRec;
  Result : Integer;
  i : integer;
begin
  if (nrbackups > 0) and (FileExists(Cfg_path)) then begin
    // make backup
    VMname := ExtractFileNameOnly(Cfg_path);
    DateTimeToString(TimeStamp,'yyyymmdd_hhmmss',Now);
    BackupDir := ExtractFilePath(Cfg_path) + 'bak/';
    if not DirectoryExists(BackupDir) then CreateDir(BackupDir);
    BackupCfgFile := BackupDir+VMname+'.cfg.'+TimeStamp;
    DebugLn('Creating '+BackupCfgFile);
    if not CopyFile(Cfg_path, BackupCfgFile) then
           ShowMessage('Cannot make cfg backup. Please check directory access rights for '+ BackupDir);
  end;
  // cleanup number of backups: cfg file only if more than nrbackups, oldest first
  Result := FindFirst(BackupDir + '*', faAnyFile, SearchRec);
  BakList := TStringList.Create;
  try
    while Result = 0 do
    begin

      if (ExtractFileExt(ExtractFileNameOnly(SearchRec.Name)) = '.cfg') then begin
         BakList.Add(BackupDir+SearchRec.Name);
      end;
      Result := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
  // Sort the list. With the time stamps in the name, the order will be old to new
  BakList.Sort;
  if BakList.Count > nrbackups then begin
    for i := 0 to BakList.Count - nrbackups - 1 do begin
      DeleteFile(BakList[i]);
    end;
  end;
  BakList.Free
end;


procedure PrintVMarr;
var
  VMobject : TVMobject;
  HD : TStorage;
  S : string;
  i: integer;
begin
  // Debug info
  DebugLnEnter('[VMarr]');
  i := 0;
  for VMobject in VMs.VMarr do begin
    with VMobject do begin
      DebugLnEnter([i, ': ', VMname]);
      DebugLn('Cfg_path = ' + Cfg_path);
      DebugLn('Nvr_path = ' + Nvr_path);
      for HD in Storage do begin
          WriteStr(S, HD.StorageEntry);
          DebugLn(S + ' --> ' + HD.Path);
      end;
      DebugLnExit;
    end;
    i += 1;
  end;
  DebugLnExit;
end;

end.

