unit FSChangeMonitor;

interface

uses
  windows, Classes;

type
  TFSChangeMonitor = class
    FOnChangeHappened: TNotifyEvent;
    FThread: THandle;
    FDirPath: string;
    FTerminated: Boolean;
    constructor Create(ADirPath: string);
    destructor Destroy; override;
    property OnChangeHappened: TNotifyEvent read FOnChangeHappened
      write FOnChangeHappened;
  end;

implementation

{ TFSChangeMonitor }

function ThreadRoutine(AMonitor: TFSChangeMonitor): Cardinal; stdcall;
var
  ChangeHandle: THandle;
begin
  Result := 0;
  ChangeHandle := FindFirstChangeNotification(PChar(AMonitor.FDirPath), false,
    FILE_NOTIFY_CHANGE_DIR_NAME);

  if ChangeHandle = INVALID_HANDLE_VALUE then
    exit;

  try
    while not AMonitor.FTerminated do
    begin
      case WaitForSingleObject(ChangeHandle, 300) of
        WAIT_FAILED:
          exit;
        WAIT_OBJECT_0:
          if Assigned(AMonitor.OnChangeHappened) then
            TThread.Synchronize(TThread.CurrentThread,
              procedure
              begin
                AMonitor.OnChangeHappened(AMonitor);
              end);
      end;
      FindNextChangeNotification(ChangeHandle);
    end;
  finally
    FindCloseChangeNotification(ChangeHandle);
  end;
end;

constructor TFSChangeMonitor.Create(ADirPath: string);
var
  tid: Cardinal;
begin
  FOnChangeHappened := nil;
  FDirPath := ADirPath;
  FTerminated := false;
  FThread := CreateThread(nil, 0, @ThreadRoutine, self, 0, tid);
end;

destructor TFSChangeMonitor.Destroy;
begin
  FTerminated := true;
  WaitForSingleObject(FThread, 700);
  CloseHandle(FThread);
  inherited;
end;

end.
