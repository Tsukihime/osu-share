unit Config;

interface

uses
  windows,
  sysutils,
  inifiles,
  registry,
  winsock,
  NetUtils;

type
  TConfig = class
  private
    ini: TMemIniFile;
    FsectionName: string;
    FOsuSongPath: string;
    FPuushPath: string;
    FTempPath: string;
    FHost: string;
    FPort: Word;
    FIsWhiteIP: boolean;
    FNetThread: TPingThread;
    function getOsuSongPath: string;
    function getPuushPath: string;
    function getTemp: string;
    procedure setOsuSongPath(const Value: string);
    function getHost: string;
    function getPort: Word;
    procedure setHost(const Value: string);
    procedure OnGetIP(Sender: TObject);
    procedure OnPing(Sender: TObject);
    procedure OnForward(Sender: TObject);
    procedure setPort(const Value: Word);
    function getInternalIP: string;
  public
    constructor Create(iniFileName: string; sectionName: string);
    destructor Destroy; override;
    function getValue(name: string): Variant;
    procedure setValue(name: string; Value: Variant);
    property OsuSongPath: string read getOsuSongPath write setOsuSongPath;
    property PuushPath: string read getPuushPath;
    property TempPath: string read getTemp;
    property IsWhiteIP: boolean read FIsWhiteIP;
    property Host: string read getHost write setHost;
    property port: Word read getPort write setPort;
    property InternalIP: string read getInternalIP;
  end;

implementation

{ TConfig }

constructor TConfig.Create(iniFileName: string; sectionName: string);
begin
  ini := TMemIniFile.Create(iniFileName);
  FsectionName := sectionName;
  ini.WriteString(FsectionName, 'beer', 'included');
  ini.WriteString(FsectionName, 'KawaiiLevel', 'over9000');
  ini.WriteString(FsectionName, 'Loli', 'on');

  FOsuSongPath := ini.ReadString(FsectionName, 'OsuSongPath', '');
  FHost := ini.ReadString(FsectionName, 'Host', '');
  FPort := ini.ReadInteger(FsectionName, 'Port', 778);

  ini.WriteString(FsectionName, 'Host', FHost);
  ini.WriteInteger(FsectionName, 'Port', FPort);

  FNetThread := TPingThread.Create;
  FNetThread.OnGetIP := self.OnGetIP;
  FNetThread.OnPing := self.OnPing;
  FNetThread.OnForwardPort := self.OnForward;
  FNetThread.getIP;
  FNetThread.ForwardPort('osu!share', InternalIP, port);
end;

destructor TConfig.Destroy;
begin
  ini.UpdateFile;
  FreeAndNil(ini);
  FreeAndNil(FNetThread);
  TMiniUPnP.RemovePort(port);
  inherited;
end;

function TConfig.getHost: string;
begin
  result := FHost;
end;

function TConfig.getInternalIP: string;
type
  pu_long = ^u_long;
var
  varTWSAData: TWSAData;
  varPHostEnt: PHostEnt;
  varTInAddr: TInAddr;
  namebuf: array [0 .. 255] of AnsiChar;
begin
  if WSAStartup($101, varTWSAData) <> 0 then
    result := ''
  else
  begin
    gethostname(namebuf, sizeof(namebuf));
    varPHostEnt := gethostbyname(namebuf);
    varTInAddr.S_addr := u_long(pu_long(varPHostEnt^.h_addr_list^)^);
    result := inet_ntoa(varTInAddr);
  end;
  WSACleanup;
end;

function TConfig.getOsuSongPath: string;
var
  Reg: TRegistry;
  s: string;
begin
  if FOsuSongPath = '' then
  begin
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CLASSES_ROOT;
      if Reg.OpenKeyReadOnly('osu\DefaultIcon') then
      begin
        s := Reg.ReadString('');
        delete(s, 1, 1);
        FOsuSongPath := ExtractFileDir(s) + '\Songs';
        setValue('OsuSongPath', FOsuSongPath);
      end;
    finally
      Reg.Free;
    end;
  end;
  result := FOsuSongPath;
end;

function TConfig.getPuushPath: string;
var
  Reg: TRegistry;
  s: string;
begin
  if FPuushPath = '' then
  begin
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CLASSES_ROOT;
      if Reg.OpenKeyReadOnly('*\shell\puush\command') then
      begin
        s := Reg.ReadString('');
        FPuushPath := copy(s, 1, pos(' -upload', s) - 1);
      end;
    finally
      Reg.Free;
    end;
  end;
  result := FPuushPath;
end;

function TConfig.getTemp: string;
var
  buf: string;
  len: integer;
begin
  if FTempPath = '' then
  begin
    SetLength(buf, MAX_PATH + 1);
    len := getTempPath(MAX_PATH, PChar(buf));
    SetLength(buf, len);
    FTempPath := buf + FsectionName;
  end;
  result := FTempPath;
end;

function TConfig.getPort: Word;
begin
  result := FPort;
end;

function TConfig.getValue(name: string): Variant;
begin
  result := ini.ReadString(FsectionName, name, '');
end;

procedure TConfig.OnForward(Sender: TObject);
begin
  FNetThread.Ping(Host, port);
end;

procedure TConfig.OnGetIP(Sender: TObject);
begin
  FHost := (Sender as TPingThread).IP;
end;

procedure TConfig.OnPing(Sender: TObject);
begin
  FIsWhiteIP := (Sender as TPingThread).IsPingSucces;
end;

procedure TConfig.setHost(const Value: string);
begin
  FHost := Value;
  setValue('Host', FHost);
end;

procedure TConfig.setOsuSongPath(const Value: string);
begin
  FOsuSongPath := Value;
  setValue('OsuSongPath', FOsuSongPath);
end;

procedure TConfig.setPort(const Value: Word);
begin
  FPort := Value;
  setValue('port', FPort);
end;

procedure TConfig.setValue(name: string; Value: Variant);
begin
  ini.WriteString(FsectionName, name, Value);
end;

end.
