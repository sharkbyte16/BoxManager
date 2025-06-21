unit VMsystem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Process, Dialogs, FileUtil, LazFileUtils,
  Controls, IniFiles;

const
  APPNAME='BoxManager';
  SLASH = '/';
  DEFAULT_NRBAK = 5;
  DEFAULT_HDBAKSIZE = 100;
  NOHDBAKSIZE = 1001;

type
  TSettings = class
    cfg_dir : string;                    // full path string to BoxManager config dir
    cfg_boxmanager : string;             // full path string to BoxManager.cfg
    sysinfo : string;                    // full path string to sysinfo.txt
    exe_86box : string;                  // full path string 86box executable
    exe_86box_dir : string;              // directory path string 86box executable
    dir_vm : string;                     // directory path string location VM's
    dir_vm_86box : string;               // dir_vm/86box
    exe_86box_present : boolean;         // Do we have the 86box app installed?
    nrbackups : integer;                 // max number of backups to keep
    hdbackupsize : integer;              // max hardisk size to include in backups
    noconfirm : integer;                 // 86Box noconfirm option
    fullscreen : integer;                // 86Box fullscreen option
    other_VM_settings : string;          // Other 86Box options, manual addition
    simulate_dga : integer;              // Tulip Dual Graphics (MDA/CGA) hack
    private
    public
      constructor Create;
      procedure SaveConfig;
  end;

  TStorageEntry = (
      // harddisk entries in 86box machine config:
      hdd_01_fn, hdd_02_fn, hdd_03_fn, hdd_04_fn, hdd_05_fn, hdd_06_fn, hdd_07_fn, hdd_08_fn);

  TStorage = record
    StorageEntry: TStorageEntry;         // use WriteStr(S, StorageEntry) to get string
    Path: string;                        // full path string to image file
    SizeMB: double;                      // hard disk size in MB
  end;

  TStorageArray = array of TStorage;

  TVMobject = class                      // all info for a virtual machine config
    VMname: string;
    Cfg_path: string;
    Nvr_path: string;
    Info_path: string;
    Storage: TStorageArray;
    Tulip_DGA: string;
  private
    procedure GetStorageFromCfg;
  public
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
    procedure NewVM(NewName: string);
    function IndexOfVM(VMname : string): integer;
    procedure ModifyConfigFilePath(const configFile, oldVMname, newVMname: string);
    function IsValidVMName(const VMName: string): boolean;
  end;


var
  Settings : TSettings;
  VMs : TVMs;

implementation

constructor TSettings.Create;
var
  OK, binpathOK : boolean;
  F : Text;
  OpenDialog : TOpenDialog;
  BoxmanagerINI : TIniFile;
begin
  cfg_dir        := GetAppConfigDir(False);
  cfg_boxmanager := cfg_dir + 'BoxManager.cfg';
  sysinfo        := cfg_dir + 'sysinfo.txt';

  // open config file or create
  OK := DirectoryExists(cfg_dir);
  if not OK then OK := CreateDir(cfg_dir);
  if not OK then ShowMessage('Failed to create directory '+cfg_dir);
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

  try
    BoxmanagerINI := TIniFile.Create(cfg_boxmanager);
    // get or set binary 86Box path from config file
    binpathOK := False;
    exe_86box := BoxmanagerINI.ReadString('BoxManager', 'exe_86box', 'not_present');
    if exe_86box = 'not_present' then begin
      binpathOK := False;
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

    // get settings
    if not TryStrToInt(BoxmanagerINI.ReadString('BoxManager', 'nrbackups', IntToStr(DEFAULT_NRBAK)), nrbackups) then
       nrbackups := DEFAULT_NRBAK;
    if nrbackups < 0 then nrbackups := 0;

    if not TryStrToInt(BoxmanagerINI.ReadString('BoxManager', 'hdbackupsize', IntToStr(DEFAULT_HDBAKSIZE)), hdbackupsize) then
       hdbackupsize := DEFAULT_HDBAKSIZE;
    if hdbackupsize < 0 then hdbackupsize := 0;

    if not TryStrToInt(BoxmanagerINI.ReadString('BoxManager', 'noconfirm', '0'), noconfirm) then
       noconfirm := 0;
    if noconfirm < 0 then noconfirm := 0;

    if not TryStrToInt(BoxmanagerINI.ReadString('BoxManager', 'fullscreen', '0'), fullscreen) then
       fullscreen := 0;
    if fullscreen < 0 then fullscreen := 0;

    if not TryStrToInt(BoxmanagerINI.ReadString('BoxManager', 'simulate_dga', '0'), simulate_dga) then
       simulate_dga := 0;
    if simulate_dga < 0 then simulate_dga := 0;

    other_VM_settings := BoxmanagerINI.ReadString('BoxManager', 'other_VM_settings', '');

  finally
    BoxmanagerINI.Free;
  end;

  // refresh config file
  SaveConfig;

  // VM configs dir
  dir_vm         := cfg_dir + 'VMs/';
  // override default div_vm path with command line argument
  if paramCount() > 0 then dir_vm := paramStr(1);
  OK := DirectoryExists(dir_vm);
  if not OK then OK := CreateDir(dir_vm);
  if not OK then ShowMessage('Failed to create directory '+dir_vm);

  // 86Box VM configs dir
  dir_vm_86box   := dir_vm + '86box/';
  if OK then begin
    OK := DirectoryExists(dir_vm_86box);
    if not OK then OK := CreateDir(dir_vm_86box);
    if not OK then ShowMessage('Failed to create directory '+dir_vm_86box);
  end;
end;

procedure TSettings.SaveConfig;
var
  BoxmanagerINI : TIniFile;
begin
  try
    BoxmanagerINI := TIniFile.Create(cfg_boxmanager);
    BoxmanagerINI.WriteString('BoxManager', 'exe_86box', exe_86box);
    BoxmanagerINI.WriteString('BoxManager', 'nrbackups', IntToStr(nrbackups));
    BoxmanagerINI.WriteString('BoxManager', 'hdbackupsize', IntToStr(hdbackupsize));
    BoxmanagerINI.WriteString('BoxManager', 'noconfirm', IntToStr(noconfirm));
    BoxmanagerINI.WriteString('BoxManager', 'fullscreen', IntToStr(fullscreen));
    BoxmanagerINI.WriteString('BoxManager', 'simulate_dga', IntToStr(simulate_dga));
    BoxmanagerINI.WriteString('BoxManager', 'other_VM_settings', other_VM_settings);
  finally
    BoxmanagerINI.Free;
  end;
end;

procedure TVMobject.GetStorageFromCfg;
var
  St : TStorage;
  S, HDPath : String;
  hdd : TStorageEntry;
  F : file;
  INIcfg : TIniFile;
begin
  // ensure we start with an empty Storage array
  SetLength(Storage, 0);
  // then fill from cfg entries
  for hdd in TStorageEntry do begin
    St.StorageEntry := hdd;
    WriteStr(S, St.StorageEntry);
    try
      INIcfg := TIniFile.Create(Cfg_path);
      St.Path := INIcfg.ReadString('Hard disks', S, '');
    finally
      INIcfg.Free;
    end;
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
  PN, FN : String;
  VMobject : TVMobject;
  Machine, NVR, INFO : String;
  i : integer;
  VMINI : TIniFile;
  Sections : Tstrings;
begin
  Settings := TSettings.Create;
  // Look for 86Box VMs
  if Settings.exe_86box_present then begin
    Result := FindFirst(Settings.dir_vm_86box + '*', faDirectory, SearchRec);
    try
      while Result = 0 do
      begin
        if ((SearchRec.Name <> '.') and (SearchRec.Name <> '..')) then begin
          PN := Settings.dir_vm_86box + SearchRec.Name;
          FN := PN + '/'+ SearchRec.Name + '.cfg';
          INFO := PN + '/'+ SearchRec.Name + '.info';
          if FileExists(FN) then begin
            try
              VMINI := TIniFile.Create(FN);
              Machine := VMINI.ReadString('Machine', 'machine', 'machine not found');
            finally
              VMINI.Free;
            end;
            NVR := PN + '/nvr/'+ Machine + '.nvr';
            VMobject := TVMobject.Create;
            with VMobject do begin
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
  for i := 0 to Length(VMarr)-1 do begin
    Sections := TStringList.Create;
    try
      try
        VMINI := TIniFile.Create(VMarr[i].Cfg_path);
        VMINI.ReadSections(Sections);
        if (Sections.IndexOf('Hercules') <> -1)
            and (Sections.IndexOf('IBM CGA') <> -1)
            and ( (VMINI.ReadString('Video', 'gfxcard', 'Unknown') = 'hercules')
                   or (VMINI.ReadString('Video', 'gfxcard', 'Unknown') = 'cga') )
        then begin
          VMINI.WriteString('Tulip DGA', 'tulipdga', 'present');
          VMarr[i].Tulip_DGA := 'present';
        end
        else begin
          VMINI.WriteString('Tulip DGA', 'tulipdga', 'none');
          VMarr[i].Tulip_DGA := 'none';
        end;
      finally
        VMINI.Free;
      end;
    finally
      Sections.Free;
    end;
  end; // Tulip DGA
end;

procedure TVMs.LaunchVM(index: integer);
var
  proc: TProcess;
begin
  if not (index > Length(VMarr)-1) then begin
    with VMarr[index] do begin
      proc := TProcess.Create(nil);
      proc.Executable := Settings.exe_86box;
      proc.Parameters.Add('--config');
      proc.Parameters.Add(Cfg_path);
      if (Settings.noconfirm = 1) then proc.Parameters.Add('--noconfirm');
      if (Settings.fullscreen = 1) then proc.Parameters.Add('--fullscreen');
      proc.Parameters.Add(Settings.other_VM_settings);
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
  INIcfg : TIniFile;
begin
  if (index > Length(VMarr)-1) then begin
    ShowMessage('VM index out of range.');
    Exit;
  end;

  // Validate VM name first
  if not IsValidVMName(NewName) then begin
    ShowMessage('Invalid VM name. Please use only letters, numbers, spaces, and hyphens. Avoid special characters.');
    Exit;
  end;

  // If name OK, proceed
  with VMarr[index] do begin
    OldName := VMname;
    dir_vm := Settings.dir_vm_86box;
    if RenameFile(dir_vm + OldName, dir_vm + NewName) then begin
       if RenameFile(dir_vm + NewName + '/' + OldName + '.cfg', dir_vm + NewName + '/' + NewName + '.cfg') then begin
          Cfg_path := dir_vm + NewName + '/' + NewName + '.cfg';
          ModifyConfigFilePath(Cfg_path, OldName, NewName);
       end;
       try
         INIcfg := TIniFile.Create(Cfg_path);
         machine := INIcfg.ReadString('Machine', 'machine', '');
       finally
         INIcfg.Free;
       end;
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
  SourceName, SourceDir, machine, dir_vm : string;
  NewDir, NewCfg, NewNvr : string;
  InputOK, CopyOK, CfgOK, NvrOK : boolean;
  VMobject : TVMobject;
  INIcfg : TIniFile;
begin
  if (index > Length(VMarr)-1) then begin
    ShowMessage('VM index out of range.');
    Exit;
  end;

  // Validate VM name first
  if not IsValidVMName(NewName) then begin
    ShowMessage('Invalid VM name. Please use only letters, numbers, spaces, and hyphens. Avoid special characters.');
    Exit;
  end;

  // If name OK, proceed
  with VMarr[index] do begin
    // source info
    SourceName := VMname;
    dir_vm := Settings.dir_vm_86box;
    try
      INIcfg := TIniFile.Create(Cfg_path);
      machine := INIcfg.ReadString('Machine', 'machine', '');
    finally
      INIcfg.Free;
    end;
    SourceDir := dir_vm + SourceName + '/';

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

    if InputOK then begin
       CopyOK := CopyDirTree(SourceDir, NewDir);
    end;

    if CopyOK then begin
      CfgOk := RenameFile(NewDir+SourceName+'.cfg', NewDir+NewName+'.cfg');
      NewCfg := NewDir+NewName+'.cfg';
    end;

    if CfgOK then begin
      ModifyConfigFilePath(NewCfg, SourceName, NewName);
      NewNvr := NewDir + 'nvr/' + machine + '.nvr';
      NvrOK := True;
    end;

  end; // with VMarr[index]

  if NvrOK then begin
  // everything copied, now make VMobject
  VMobject := TVMobject.Create;
   with VMobject do begin
     VMname := NewName;
     Cfg_path := NewCfg;
     Nvr_path := NewNvr;
     GetStorageFromCfg;
   end;
   SetLength(VMarr, Length(VMarr)+1);
   VMarr[Length(VMarr)-1] := VMobject;
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
    vm_dir := Settings.dir_vm_86box + VMname + '/';
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
  INIcfg : TIniFile;
begin
  if not (index > Length(VMarr)-1) then begin
    with VMarr[index] do begin
      // first backup cfg
      BackupVM(index);
      // now (re)configure the VM
      try
        INIcfg := TIniFile.Create(Cfg_path);
        machine := INIcfg.ReadString('Machine', 'machine', '');
      finally
        INIcfg.Free;
      end;
      Exe_path := Settings.exe_86box;
      proc := TProcess.Create(nil);
      proc.Executable := Exe_path;
      proc.Parameters.Add('--settings');
      proc.Parameters.Add(Cfg_path);
      proc.Options := proc.Options - [poWaitOnExit];
      proc.Execute;
      proc.Free;
      // update VMobject
      try
        INIcfg := TIniFile.Create(Cfg_path);
        if not (INIcfg.ReadString('Machine', 'machine', '') = machine) then begin
           DeleteFile(Nvr_path);
           Nvr_path := Settings.dir_vm_86box + VMname + '/nvr/' + machine + '.nvr';
           GetStorageFromCfg;
        end;
      finally
        INIcfg.Free;
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
  if (Settings.nrbackups > 0) and FileExistsUTF8(VMarr[index].Cfg_path) then begin
    // backup folder
    DateTimeToString(TimeStamp,'yyyymmdd_hhmmss',Now);
    BAKdir := ExtractFilePath(VMarr[index].Cfg_path) + 'bak/';
    if not DirectoryExists(BAKdir) then CreateDir(BAKdir);
    BackupDir := BAKdir + 'vmbackup_' + TimeStamp + '/';
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
      Assign(HDfile, HDfilename);
      Reset(HDfile);
      HDsizeMB := FileSize(HDfile) div 1048576;
      Close(HDfile);
      if Settings.hdbackupsize > 0 then begin
        if (HDsizeMB <= Settings.hdbackupsize) or (Settings.hdbackupsize = NOHDBAKSIZE) then begin
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
  if BakList.Count > Settings.nrbackups then begin
    for i := 0 to BakList.Count - Settings.nrbackups - 1 do begin
      DeleteDirectory(BakList[i], false);
    end;
  end;
  BakList.Free
end;

procedure TVMs.NewVM(NewName: string);
var
  vm_base_dir, New_vm_dir : string;
  NameOK : boolean;
  VMobject : TVMobject;
begin
  // Validate VM name first
  if not IsValidVMName(NewName) then begin
    ShowMessage('Invalid VM name. Please use only letters, numbers, spaces, and hyphens. Avoid special characters.');
    Exit;
  end;

  // First create VM dir. If it fails, no need to create VMobject
  vm_base_dir := Settings.dir_vm_86box;
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

procedure TVMs.ModifyConfigFilePath(const configFile, oldVMname, newVMname: string);
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

function TVMs.IsValidVMName(const VMName: string): boolean;
var
  i: integer;
  InvalidChars: set of char;
begin
  Result := False;

  // Check if name is empty or too long
  if (Length(VMName) = 0) or (Length(VMName) > 50) then Exit;

  // Define invalid characters for file/directory names
  InvalidChars := ['/', '\', ':', '*', '?', '"', '<', '>', '|', '.'];

  // Check each character
  for i := 1 to Length(VMName) do begin
    if VMName[i] in InvalidChars then Exit;
  end;

  // Check for reserved names
  if (UpperCase(VMName) = 'CON') or (UpperCase(VMName) = 'PRN') or
     (UpperCase(VMName) = 'AUX') or (UpperCase(VMName) = 'NUL') then Exit;

  Result := True;
end;

end.

