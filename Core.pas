unit Core;

interface

uses
  forms,
  messages,
  classes,
  sysutils,
  windows,
  System.Generics.Collections,
  MapServer,
  Config,
  OsuTrackSpy,
  FSChangeMonitor;

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
    FHash: string;
    FBeatmapSetID: Integer;
    procedure setPath(const Value: string);
  public
    property Path: string read FPath write setPath;
    property name: string read FName write FName;
    property Title: string read FTitle write FTitle;
    property Artist: string read FArtist write FArtist;
    property Creator: string read FCreator write FCreator;
    property Source: string read FSource write FSource;
    property Tags: string read FTags write FTags;
    property IsInitialized: boolean read FIsInitialized;
    property Hash: string read FHash write FHash;
    property BeatmapSetID: Integer read FBeatmapSetID write FBeatmapSetID;
    procedure InitMap;
    procedure getOszStream(Stream: TStream);
  end;

  TOsuShareCore = class(TInterfacedPersistent, IMapKeeper)
  private
    FMapServer: TMapServer;
    FConfig: TConfig;
    FMapList: TObjectList<TOsuMap>;
    FTrackSpy: TOsuTrackSpy;
    FFSMonitor: TFSChangeMonitor;
    FOnMapListUpdated: TNotifyEvent;
    procedure FSChangeMthd(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    function GetMap(ServReq: string; var MapName: string): TUploadStream;
    property OnMapListUpdated: TNotifyEvent read FOnMapListUpdated
      write FOnMapListUpdated;
    property MapList: TObjectList<TOsuMap> read FMapList;
    property Config: TConfig read FConfig;
    property MapServer: TMapServer read FMapServer;
    property TrackSpy: TOsuTrackSpy read FTrackSpy;
  end;

procedure DirRemove(Path: string);
procedure GetMapList(Path: string; list: TObjectList<TOsuMap>);

implementation

uses
  zip,
  IdHashMessageDigest,
  idHash;

function MD5(const AText: string): string;
var
  idmd5: TIdHashMessageDigest5;
begin
  idmd5 := TIdHashMessageDigest5.Create;
  try
    result := idmd5.HashStringAsHex(AText);
  finally
    idmd5.Free;
  end;
end;

procedure GetMapList(Path: string; list: TObjectList<TOsuMap>);
var
  i: Integer;
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

{ TOsuMap }

procedure TOsuMap.getOszStream(Stream: TStream);
  procedure GetFileList(const Path: string; list: TStrings);
  var
    i: Integer;
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
  i, len: Integer;
  pth, zippath: string;
  fs: TFileStream;
begin
  list := TStringList.Create;
  GetFileList(FPath, list);
  ZipFile := TZipFile.Create;
  ZipFile.Open(Stream, zmWrite);
  len := Length(FPath);
  for i := 0 to list.Count - 1 do
  begin
    pth := list[i];
    fs := TFileStream.Create(pth, fmOpenRead or fmShareDenyWrite);
    zippath := copy(pth, len + 2, Length(pth) - len - 1);
    ZipFile.Add(fs, zippath);
    FreeAndNil(fs);
  end;
  ZipFile.Free;
  list.Free;
end;

procedure TOsuMap.InitMap;
var
  i: Integer;
  SearchRec: TSearchRec;
  _path, s, bts: string;
  slist: TStringList;
begin
  if FIsInitialized then
    exit;
  FBeatmapSetID := -1;
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
          if pos('BeatmapSetID:', s) > 0 then
            bts := UTF8ToWideString(copy(s, 14, Length(s) - 13));
          TryStrToInt(bts, FBeatmapSetID);
        end;
      end;
    except
      raise;
    end;
  finally
    slist.Free;
    FIsInitialized := true; // в любом случае меняем статус
  end;
end;

procedure TOsuMap.setPath(const Value: string);
begin
  FPath := Value;
  FHash := MD5(FPath);
end;

{ TOsuShareCore }

constructor TOsuShareCore.Create;
begin
  FConfig := TConfig.Create('osu!share.ini', 'osu!share');

  FMapList := TObjectList<TOsuMap>.Create(true);

  FMapServer := TMapServer.Create(self);
  FMapServer.port := FConfig.port;
  FMapServer.Run;

  FTrackSpy := TOsuTrackSpy.Create;
  FTrackSpy.Connect;

  FFSMonitor := TFSChangeMonitor.Create(FConfig.OsuSongPath);
  FFSMonitor.OnChangeHappened := FSChangeMthd;

  CreateDir(FConfig.TempPath);
  FSChangeMthd(self);
end;

destructor TOsuShareCore.Destroy;
begin
  FFSMonitor.Free;

  FTrackSpy.Free;

  FMapServer.Stop;
  FMapServer.Free;

  FMapList.Free;

  DirRemove(FConfig.TempPath);
  FConfig.Free;

  inherited;
end;

procedure TOsuShareCore.FSChangeMthd(Sender: TObject);
begin
  FMapList.Clear;
  GetMapList(FConfig.OsuSongPath, FMapList);
  if Assigned(FOnMapListUpdated) then
    OnMapListUpdated(Sender);
end;

function TOsuShareCore.GetMap(ServReq: string; var MapName: string)
  : TUploadStream;
var
  i: Integer;
  us: TUploadStream;
begin
  result := nil;

  for i := 0 to FMapList.Count - 1 do
    if FMapList[i].Hash = ServReq then
    begin
      us := TUploadStream.Create;
      FMapList[i].getOszStream(us);
      result := us;

      if FMapList[i].IsInitialized then
        MapName := FMapList[i].Artist + ' - ' + FMapList[i].Title
      else
        MapName := FMapList[i].name;
      break;
    end;
end;

end.
