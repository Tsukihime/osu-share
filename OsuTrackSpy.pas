unit OsuTrackSpy;

interface

uses
  shellapi,
  Windows,
  tlhelp32,
  SysUtils;

type
  TOsuTrackSpy = class
  private
    FConnected: Boolean;
    FFilePath: string;
    mutex: THandle;
    fileMap: THandle;
    mapmemory: Pointer;
    FHProcess: THandle;
    function GetFilePath: string;
  public
    constructor Create;
    destructor Destroy; override;
    function Connect: Boolean;
    procedure Disconnect;
    property FilePath: string read GetFilePath;
    property Connected: Boolean read FConnected;
  end;

  TInject = packed record
    PushLoadLibraryCommand: BYTE;
    LoadLibraryArg: DWORD;
    CallLoadLibrary: word;
    CallLoadLibraryAddr: DWORD;
    PushExitThread: BYTE;
    ExitThreadArg: DWORD;
    CallExitThread: word;
    CallExitThreadAddr: DWORD;
    AddrLoadLibrary: Pointer;
    AddrExitThread: Pointer;
    LibraryName: array [0 .. MAX_PATH] of Char;
  end;

implementation

{ Внедрение Dll в процесс }
function InjectDll(Process: DWORD; ModulePath: PChar): Boolean;
  function Offset(struct, field: Pointer): cardinal;
  begin
    Result := cardinal(field) - cardinal(struct);
  end;

var
  Memory: Pointer;
  Code: DWORD;
  BytesWritten: DWORD;
  ThreadId: DWORD;
  hThread: DWORD;
  hKernel32: DWORD;
  Inject: TInject;
begin
  Result := false;
  Memory := VirtualAllocEx(Process, nil, sizeof(Inject), MEM_TOP_DOWN or
    MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if Memory = nil then
    Exit;

  Code := DWORD(Memory);
  // инициализация внедряемого кода:
  FillChar(Inject, sizeof(Inject), 0);

  Inject.PushLoadLibraryCommand := $68;
  Inject.LoadLibraryArg := Code + Offset(@Inject, @Inject.LibraryName);
  Inject.CallLoadLibrary := $15FF;
  Inject.CallLoadLibraryAddr := Code + Offset(@Inject, @Inject.AddrLoadLibrary);

  Inject.PushExitThread := $68;
  Inject.ExitThreadArg := 0;
  Inject.CallExitThread := $15FF;
  Inject.CallExitThreadAddr := Code + Offset(@Inject, @Inject.AddrExitThread);

  hKernel32 := GetModuleHandle('kernel32.dll');
  Inject.AddrLoadLibrary := GetProcAddress(hKernel32, 'LoadLibraryW');
  Inject.AddrExitThread := GetProcAddress(hKernel32, 'ExitThread');
  lstrcpy(@Inject.LibraryName, ModulePath);
  // записать машинный код по зарезервированному адресу
  WriteProcessMemory(Process, Memory, @Inject, SIZE_T(sizeof(Inject)),
    SIZE_T(BytesWritten));
  // выполнить машинный код
  hThread := CreateRemoteThread(Process, nil, 0, Memory, nil, 0, ThreadId);
  if hThread = 0 then
    Exit;
  WaitForSingleObject(hThread, INFINITE);
  CloseHandle(hThread);
  VirtualFreeEx(Process, Memory, 0, MEM_RELEASE);
  // надо-надо умываться по утрам и вечерам
  Result := True;
end;

function OpenProcessByName(pName: string): THandle;
var
  hSnap: THandle;
  pe: TProcessEntry32;
begin
  Result := 0;
  pe.dwSize := sizeof(pe);
  hSnap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if Process32First(hSnap, pe) then
    while Process32Next(hSnap, pe) do
      if ExtractFileName(pe.szExeFile) = pName then
      begin
        Result := OpenProcess(PROCESS_ALL_ACCESS, false, pe.th32ProcessID);
        break;
      end;
  CloseHandle(hSnap);
end;

{ TOsuTrackSpy }

function TOsuTrackSpy.Connect: Boolean;
var
  res: Boolean;
  dllpath: string;
begin
  FConnected := false;
  try
    mutex := OpenMutex(SYNCHRONIZE, false, 'osu!mutex');

    if mutex = 0 then
    begin
      dllpath := ExtractFilePath(Paramstr(0)) + 'osu-hook.dll';

      if not FileExists(dllpath) then
        Exit;

      FHProcess := OpenProcessByName('osu!.exe');
      if FHProcess = 0 then
        FHProcess := OpenProcessByName('osu!test.exe');
      if FHProcess = 0 then
        Exit;

      res := InjectDll(FHProcess, PChar(dllpath));

      if not res then
        Exit;
      mutex := OpenMutex(SYNCHRONIZE, false, 'osu!mutex');
    end;

    if mutex <> 0 then
      fileMap := OpenFileMapping(FILE_MAP_READ, false, 'osu!filemap')
    else
      Exit;

    if fileMap <> 0 then
      mapmemory := MapViewOfFile(fileMap, FILE_MAP_READ, 0, 0, 0);

    if mapmemory <> nil then
      FConnected := True;
  finally
    Result := FConnected;
  end;
end;

procedure TOsuTrackSpy.Disconnect;
begin
  if FConnected then
  begin
    FConnected := false;
    UnmapViewOfFile(mapmemory);
    mapmemory := nil;
    CloseHandle(fileMap);
    CloseHandle(mutex);
    CloseHandle(FHProcess);
  end;
end;

constructor TOsuTrackSpy.Create;
begin
  FConnected := false;
  FFilePath := '';
  FHProcess := 0;
end;

destructor TOsuTrackSpy.Destroy;
begin
  Disconnect;
  inherited;
end;

function TOsuTrackSpy.GetFilePath: string;
var
  waitstat, excode: cardinal;
  res: Boolean;
begin
  if FConnected then
  begin
    res := GetExitCodeProcess(FHProcess, excode);
    if (not res) or (excode <> STILL_ACTIVE) then
    begin
      Disconnect;
      Connect;
    end;

    if not Connected then
      Exit;

    waitstat := WaitForSingleObject(mutex, 100);
    if (waitstat = WAIT_OBJECT_0) or (waitstat = WAIT_ABANDONED_0) then
    begin
      try
        FFilePath := PChar(mapmemory);
      except
        on E: Exception do
        begin
          Messagebox(0, PChar(E.ClassName + ': ' + E.Message),
            'Shit happens.', 0);
          Disconnect;
        end;
      end;
      ReleaseMutex(mutex);
    end;
  end;
  Result := FFilePath;
end;

end.
