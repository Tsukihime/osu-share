unit Core;

interface

uses forms, IdHTTP, inifiles, registry, messages, classes, sysutils, windows,
  winsock,
  System.Generics.Collections;

type
  TOsuMap = class
  private
    FName: string;
    FPath: string;
    FIsInitialized: boolean;
    FTitle: string;
    FArtist: string;
    FCreator: string;
    FSource: string;
    FTags: string;
    { FTitleU: string;
      FArtistU: string; }
  public
    property Path: string read FPath write FPath;
    property name: string read FName write FName;
    property Title: string read FTitle write FTitle;
    property Artist: string read FArtist write FArtist;
    property Creator: string read FCreator write FCreator;
    property Source: string read FSource write FSource;
    property Tags: string read FTags write FTags;
    property IsInitialized: boolean read FIsInitialized;
    procedure InitMap;
  end;

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
    function getOsuSongPath: string;
    function getPuushPath: string;
    function getTemp: string;
    procedure setOsuSongPath(const Value: string);
    function getHost: string;
    function getPort: Word;
    procedure setHost(const Value: string);
    procedure OnGetIP(Sender: TObject);
    procedure OnPing(Sender: TObject);
    procedure setPort(const Value: Word);
  public
    constructor Create(iniFileName: string; sectionName: string);
    destructor Destroy; override;
    function getValue(name: string): Variant;
    procedure setValue(name: string; Value: Variant);
    procedure ReInitExternalAddres;
    property OsuSongPath: string read getOsuSongPath write setOsuSongPath;
    property PuushPath: string read getPuushPath;
    property TempPath: string read getTemp;
    property IsWhiteIP: boolean read FIsWhiteIP;
    property Host: string read getHost write setHost;
    property port: Word read getPort write setPort;
  end;

  TPingThread = class(TThread)
  private
    FIsPingSucces: boolean;
    FOnPing: TNotifyEvent;
    FOnGetIP: TNotifyEvent;
    FIP: string;
    FPingPort: Word;
    procedure DoGetIP;
    procedure DoPing;
  protected
    procedure Execute; override;
  public
    constructor Create(PingPort: Word);
    procedure Run;
    property OnGetIP: TNotifyEvent read FOnGetIP write FOnGetIP;
    property OnPing: TNotifyEvent read FOnPing write FOnPing;
    property ip: string read FIP write FIP;
    property IsPingSucces: boolean read FIsPingSucces write FIsPingSucces;
  end;

  TUploadStream = class(TMemoryStream)
  private
    FAdress: string;
    FStartTime: TDateTime;
    FOnDestroy: TNotifyEvent;
    function getProgress: double;
    function getSpeed: double;
  public
    constructor Create;
    destructor Destroy; override;
    property Progress: double read getProgress;
    property Address: string read FAdress write FAdress;
    property Speed: double read getSpeed;
    function Read(var Buffer; Count: integer): integer; override;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

  IMapKeeper = interface
    function GetMap(ServReq: string; var MapName: string): TUploadStream;
  end;

procedure DirRemove(Path: string);
procedure GetMapList(Path: string; list: TObjectList<TOsuMap>);
procedure MakeOsz(dir: string; Stream: TStream);
procedure getPage404(Stream: TStream);

implementation

uses zip;

procedure GetMapList(Path: string; list: TObjectList<TOsuMap>);
var
  i: integer;
  SearchRec: TSearchRec;
  map: TOsuMap;
begin
  Path := ExcludeTrailingBackslash(Path);
  try
    i := FindFirst(Path + '\*.*', $FF, SearchRec);
    while (i = 0) and ((SearchRec.name <> '.') or (SearchRec.name <> '..')) do
    begin
      if (SearchRec.name <> '.') and (SearchRec.name <> '..') then
        if (SearchRec.Attr and faDirectory <> 0) then
        begin
          map := TOsuMap.Create;
          map.name := SearchRec.name;
          map.Path := Path + '\' + SearchRec.name;
          list.Add(map);
        end;
      i := FindNext(SearchRec);
    end;
  except
    raise;
  end;
end;

procedure DirRemove(Path: string);
var
  sr: TSearchRec;
begin
  if FindFirst(Path + '\*.*', faAnyFile, sr) = 0 then
  begin
    repeat
      if sr.Attr and faDirectory = 0 then
      begin
        sysutils.DeleteFile(Path + '\' + sr.name);
      end
      else
      begin
        if pos('.', sr.name) <= 0 then
          DirRemove(Path + '\' + sr.name);
      end;
    until FindNext(sr) <> 0;
  end;
  sysutils.FindClose(sr);
  RemoveDirectory(PChar(Path));
end;

procedure MakeOsz(dir: string; Stream: TStream);
  procedure GetFileList(const Path: string; list: TStrings);
  var
    i: integer;
    SearchRec: TSearchRec;
  begin
    try
      i := FindFirst(Path + '\*.*', $FF, SearchRec);
      while (i = 0) and ((SearchRec.name <> '.') or (SearchRec.name <> '..')) do
      begin
        if (SearchRec.name <> '.') and (SearchRec.name <> '..') then
          if (SearchRec.Attr and faDirectory <> 0) then
            GetFileList(Path + '\' + SearchRec.name, list)
          else
            list.Add(Path + '\' + SearchRec.name);
        i := FindNext(SearchRec);
      end;
    except
    end;
  end;

var
  list: TStringList;
  ZipFile: TZipFile;
  i, len: integer;
  FPath: string;
  fs: TFileStream;
begin
  list := TStringList.Create;
  GetFileList(dir, list);
  ZipFile := TZipFile.Create;
  ZipFile.Open(Stream, zmWrite);
  len := Length(dir);
  for i := 0 to list.Count - 1 do
  begin
    FPath := list[i];
    // ZipFile.Add(fname,intpath) не умеет зан€тые файлы
    fs := TFileStream.Create(FPath, fmOpenRead or fmShareDenyWrite);
    FPath := list.Strings[i];
    ZipFile.Add(fs, copy(FPath, len + 2, Length(FPath) - len - 1));
    FreeAndNil(fs);
  end;
  ZipFile.Free;
  list.Free;
end;

procedure getPage404(Stream: TStream);
var
  RS: TResourceStream;
begin
  RS := TResourceStream.Create(HInstance, // your app or DLL instance handle
    '404page', // string containing resource name
    RT_RCDATA);
  try
    Stream.CopyFrom(RS, RS.Size)
  finally
    FreeAndNil(RS);
  end;
end;

{ TOsuMap }

procedure TOsuMap.InitMap;
var
  i: integer;
  SearchRec: TSearchRec;
  _path, s: string;
  slist: TStringList;
begin
  if FIsInitialized then
    exit;
  slist := TStringList.Create;
  try
    _path := ExcludeTrailingBackslash(Path);
    try
      i := FindFirst(_path + '\*.osu', $FF, SearchRec);
      if (i = 0) then
      begin
        slist.LoadFromFile(_path + '\' + SearchRec.name);

        for i := 0 to slist.Count - 1 do
        begin
          s := slist[i];
          { if Pos('TitleUnicode:', s) > 0 then
            FTitleU := UTF8Decode(Copy(s, 14, Length(s) - 13));
            if Pos('ArtistUnicode:', s) > 0 then
            FArtistU := UTF8Decode(Copy(s, 15, Length(s) - 14)); }
          if pos('Title:', s) > 0 then
            FTitle := UTF8ToWideString(copy(s, 7, Length(s) - 6));
          if pos('Artist:', s) > 0 then
            FArtist := UTF8ToWideString(copy(s, 8, Length(s) - 7));
          if pos('Creator:', s) > 0 then
            FCreator := UTF8ToWideString(copy(s, 9, Length(s) - 8));
          if pos('Source:', s) > 0 then
            FSource := UTF8ToWideString(copy(s, 8, Length(s) - 7));
          if pos('Tags:', s) > 0 then
            FTags := UTF8ToWideString(copy(s, 6, Length(s) - 5));
        end;
      end;
    except
      raise;
    end;
  finally
    slist.Free;
    FIsInitialized := true; // в любом случае мен€ем статус
  end;
end;

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
  ReInitExternalAddres;
end;

destructor TConfig.Destroy;
begin
  ini.UpdateFile;
  FreeAndNil(ini);
  inherited;
end;

function TConfig.getHost: string;
begin
  result := FHost;
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

procedure TConfig.OnGetIP(Sender: TObject);
begin
  FHost := (Sender as TPingThread).FIP;
end;

procedure TConfig.OnPing(Sender: TObject);
begin
  FIsWhiteIP := (Sender as TPingThread).IsPingSucces;
end;

procedure TConfig.ReInitExternalAddres;
begin
  with TPingThread.Create(port) do
  begin
    OnGetIP := self.OnGetIP;
    OnPing := self.OnPing;
    Run;
  end;
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

{ TPingThread }

constructor TPingThread.Create(PingPort: Word);
begin
  inherited Create(true);
  FPingPort := PingPort;
  FreeOnTerminate := true;
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

procedure TPingThread.Execute;
var
  http: TIdHTTP;
  s: string;
begin
  try
    http := TIdHTTP.Create(nil);
    try
      FIP := http.Get('http://v4.ipv6-test.com/api/myip.php');
      Synchronize(self, DoGetIP);
      if FIP <> '' then
        try
          s := http.Get('http://' + FIP + ':' + inttostr(FPingPort) + '/ping');
        except
        end;
      FIsPingSucces := (s = 'pong');
      Synchronize(self, DoPing);
    finally
      http.Free;
    end;
  except
  end;
end;

procedure TPingThread.Run;
begin
  Start;
end;

{ TUploadStream }

constructor TUploadStream.Create;
begin
  FStartTime := 0;
end;

destructor TUploadStream.Destroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(self);
  inherited;
end;

function TUploadStream.getProgress: double;
begin
  if (Assigned(self)) and (Size > 0) then
    result := Position * 100 / Size
  else
    result := 100;
end;

function TUploadStream.getSpeed: double;
begin
  if FStartTime = 0 then
    result := 0
  else
    result := Position / 1024 / ((now - FStartTime) * 60 * 60 * 24);
end;

function TUploadStream.Read(var Buffer; Count: integer): integer;
begin
  result := inherited read(Buffer, Count);
  if FStartTime = 0 then
    FStartTime := now;
end;

end.
