library osu_share_hook;

uses
  Windows, SysUtils, Messages,
  MinHook in 'MinHook.pas';

{$R *.res}

var
  OldCreateFileW: function(lpFileName: LPCWSTR; dwDesiredAccess: DWORD;
    dwShareMode: DWORD; lpSecurityAttributes: DWORD;
    dwCreationDisposition: DWORD; dwFlagsAndAttributes: DWORD;
    hTemplateFile: THandle): THandle; stdcall;

  CreateFileWPtr: Pointer;

  disabled: boolean = true;
  mutex: THandle = 0;
  fileMap: THandle = 0;
  mapmemory: Pointer = nil;

const
  filemapsize = $1000;

function NewCreateFileW(lpFileName: LPCWSTR; dwDesiredAccess: DWORD;
  dwShareMode: DWORD; lpSecurityAttributes: DWORD; dwCreationDisposition: DWORD;
  dwFlagsAndAttributes: DWORD; hTemplateFile: THandle): THandle; stdcall;
var
  s, ext: string;
  waitstat: THandle;
  len: integer;
begin
  s := lpFileName;
  ext := ExtractFileExt(s);
  if (ext = '.mp3') or (ext = '.ogg') then
  begin
    len := (Length(s) + 1) * SizeOf(Char);
    if len > filemapsize then
      len := filemapsize;

    if (mutex <> 0) and (Assigned(mapmemory)) then
    begin
      waitstat := WaitForSingleObject(mutex, 100);
      if (waitstat = WAIT_OBJECT_0) or (waitstat = WAIT_ABANDONED_0) then
      begin
        CopyMemory(mapmemory, lpFileName, len);
        ReleaseMutex(mutex);
      end;
    end;
  end;

  Result := OldCreateFileW(lpFileName, dwDesiredAccess, dwShareMode,
    lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes,
    hTemplateFile);
end;

procedure DLLEntryPoint(Reason: DWORD);
begin
  case Reason of
    DLL_PROCESS_ATTACH:
      begin
        mutex := CreateMutex(nil, false, 'osu!mutex');
        fileMap := CreateFileMapping(0, nil, PAGE_READWRITE, 0, filemapsize,
          'osu!filemap');
        if fileMap <> 0 then
          mapmemory := MapViewOfFile(fileMap, FILE_MAP_WRITE or
            FILE_MAP_READ, 0, 0, 0);

        MH_Initialize();
        CreateFileWPtr := GetProcAddress(LoadLibrary('Kernel32.dll'),
          'CreateFileW');
        MH_CreateHook(CreateFileWPtr, @NewCreateFileW, @@OldCreateFileW);
        MH_EnableHook(CreateFileWPtr);
        disabled := false;
      end;
    DLL_PROCESS_DETACH:
      begin
        if not disabled then
        begin
          MH_DisableHook(CreateFileWPtr);
          MH_Uninitialize();
          CloseHandle(mutex);
          if Assigned(mapmemory) then
          begin
            UnmapViewOfFile(mapmemory);
            CloseHandle(fileMap);
          end;
        end;
      end;
  end;
end;

begin
  DllProc := @DLLEntryPoint;
  DLLEntryPoint(DLL_PROCESS_ATTACH);

end.
