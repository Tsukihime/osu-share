unit NetUtils;

interface

uses
  windows,
  classes,
  sysutils,
  IdHTTP,
  ComObj,
  ActiveX,
  Variants;

type
  TThreadAction = (taNone, taPing, taIP, taForward);

  TPingThread = class
  private
    FIsPingSucces: boolean;
    FOnPing: TNotifyEvent;
    FOnGetIP: TNotifyEvent;
    FOnForwardPort: TNotifyEvent;
    FIP: string;
    FPingIP: string;
    FIntIP: string;
    FPort: Word;
    FName: string;
    FThreadAct: TThreadAction;
    FPortForwarded: boolean;
    procedure DoGetIP;
    procedure DoPing;
    procedure DoForwardPort;
    procedure ThreadStart;
  public
    constructor Create;
    procedure ForwardPort(const AName, AIntIP: string; APort: Word);
    procedure getIP;
    procedure Ping(const IP: string; APort: Word);
    property OnGetIP: TNotifyEvent read FOnGetIP write FOnGetIP;
    property OnPing: TNotifyEvent read FOnPing write FOnPing;
    property OnForwardPort: TNotifyEvent read FOnForwardPort
      write FOnForwardPort;
    property IP: string read FIP write FIP;
    property IsPingSucces: boolean read FIsPingSucces write FIsPingSucces;
  end;

  TMiniUPnP = class
    class function AddPort(const APort: Word;
      const AName, AInternalIP: string): boolean;
    class procedure RemovePort(const APort: Word);
  end;

implementation

{ TPingThread }

constructor TPingThread.Create;
begin
  FThreadAct := taNone;
end;

procedure TPingThread.DoForwardPort;
begin
  if Assigned(FOnForwardPort) then
    FOnForwardPort(self);
end;

procedure TPingThread.DoGetIP;
begin
  if Assigned(FOnGetIP) then
    FOnGetIP(self);
end;

procedure TPingThread.DoPing;
begin
  if Assigned(FOnPing) then
    FOnPing(self);
end;

function ThreadRoutine(PingThread: TPingThread): Cardinal; stdcall;
var
  http: TIdHTTP;
  s: string;
begin
  with PingThread do
    case FThreadAct of
      taIP:
        begin
          http := TIdHTTP.Create(nil);
          try
            try
              FIP := http.Get('http://v4.ipv6-test.com/api/myip.php');
            except
            end;
            TThread.Synchronize(TThread.CurrentThread,
              procedure
              begin
                DoGetIP;
              end);
          finally
            http.Free;
          end;
        end;

      taPing:
        begin
          if FIP <> '' then
          begin
            http := TIdHTTP.Create(nil);
            try
              s := http.Get('http://' + FIP + ':' + inttostr(FPort) + '/ping');
            except
            end;
            http.Free;
            FIsPingSucces := (s = 'pong');
            TThread.Synchronize(TThread.CurrentThread,
              procedure
              begin
                DoPing;
              end);
          end;
        end;

      taForward:
        begin
          FPortForwarded := TMiniUPnP.AddPort(FPort, FName, FIntIP);
          TThread.Synchronize(TThread.CurrentThread,
            procedure
            begin
              DoForwardPort;
            end);
        end;
    end;
end;

procedure TPingThread.ForwardPort(const AName, AIntIP: string; APort: Word);
begin
  FPort := APort;
  FName := AName;
  FIntIP := AIntIP;
  FThreadAct := taForward;
  ThreadStart;
end;

procedure TPingThread.getIP;
begin
  FThreadAct := taIP;
  ThreadStart;
end;

procedure TPingThread.Ping(const IP: string; APort: Word);
begin
  FPort := APort;
  FPingIP := IP;
  FThreadAct := taPing;
  ThreadStart;
end;

procedure TPingThread.ThreadStart;
var
  tid: Cardinal;
begin
  CreateThread(nil, 0, @ThreadRoutine, self, 0, tid);
end;

{ TMiniUPnP }

class function TMiniUPnP.AddPort(const APort: Word;
const AName, AInternalIP: string): boolean;
var
  NAT: Variant;
  Ports: Variant;
begin
  Result := false;
  try
    CoInitialize(nil);
    NAT := CreateOleObject('HNetCfg.NATUPnP');
    Ports := NAT.StaticPortMappingCollection;
    if not VarIsClear(Ports) then
    begin
      Ports.Add(APort, 'TCP', APort, AInternalIP, true, AName);
      Result := true;
    end;
    Ports := 0;
    NAT := 0;
    CoUninitialize;
  except
  end;
end;

class procedure TMiniUPnP.RemovePort(const APort: Word);
var
  NAT: Variant;
  Ports: Variant;
begin
  try
    CoInitialize(nil);
    NAT := CreateOleObject('HNetCfg.NATUPnP');
    Ports := NAT.StaticPortMappingCollection;
    if not VarIsClear(Ports) then
      Ports.Remove(APort, 'TCP');
    Ports := 0;
    NAT := 0;
    CoUninitialize;
  except
  end;
end;

end.
