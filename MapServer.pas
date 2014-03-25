unit MapServer;

interface

uses
  IdHTTPServer,
  IdContext,
  IdCustomHTTPServer,
  windows,
  Classes,
  Sysutils,
  System.Generics.Collections;

type
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

  TMapServer = class
  private
    FPort: Word;
    Server: TIdHTTPServer;
    FIsActive: boolean;
    FConnections: integer;
    FUploads: TObjectList<TUploadStream>;
    FMapKeeper: IMapKeeper;
    procedure Disconnect(AContext: TIdContext);
    procedure Connect(AContext: TIdContext);
    procedure CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure UploadFinish(Sender: TObject);
    class procedure getPage404(Stream: TStream);
  public
    constructor Create(const MapKeeper: IMapKeeper);
    destructor Destroy; override;
    procedure Run;
    procedure Stop;
    procedure Restart;
    property Port: Word read FPort write FPort;
    property Connections: integer read FConnections;
    property CurrentUploads: TObjectList<TUploadStream> read FUploads;
  end;

implementation

uses
  IdURI;

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

{ TMapServer }

constructor TMapServer.Create(const MapKeeper: IMapKeeper);
begin
  FMapKeeper := MapKeeper;
  FIsActive := false;
  FConnections := 0;
  FUploads := TObjectList<TUploadStream>.Create(false);
end;

destructor TMapServer.Destroy;
begin
  Stop;
  FUploads.Free;
  FMapKeeper := nil;
  inherited;
end;

procedure TMapServer.CommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Stream: TUploadStream;
  m_stream: TMemoryStream;
  req, MapName: string;
begin
  try
    AResponseInfo.Server := 'osu!share by Vladimir, North, Vodka, Bears';

    req := ARequestInfo.Document;
    delete(req, 1, 1);

    if req = 'ping' then
    begin
      AResponseInfo.ContentType := 'text/plain';
      AResponseInfo.ContentText := 'pong';
      exit;
    end;

    req := StringReplace(req, '.osz', '', [rfReplaceAll]);

    Stream := FMapKeeper.GetMap(req, MapName);

    if Assigned(Stream) then
    begin
      Stream.Address := ARequestInfo.RemoteIP;
      Stream.OnDestroy := UploadFinish;
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          FUploads.Add(Stream);
        end);

      MapName := TIdURI.ParamsEncode(MapName);
      AResponseInfo.ContentType := 'application/octet-stream';
      AResponseInfo.CustomHeaders.Values['Content-disposition'] :=
        Format('attachment; filename="%s.osz";', [MapName]);

      AResponseInfo.ContentText := '';
      AResponseInfo.ContentLength := Stream.Size;
      AResponseInfo.ContentStream := Stream;
      // тут нельзя разрушить stream.Free; его порушит сам серв
    end
    else
    begin
      m_stream := TMemoryStream.Create;
      getPage404(m_stream);
      AResponseInfo.ContentType := 'text/html';
      AResponseInfo.ResponseNo := 404;
      AResponseInfo.ContentLength := m_stream.Size;
      AResponseInfo.ContentStream := m_stream;
      // тут нельзя разрушить m_stream.Free; его порушит сам серв
      exit;
    end;
  except
  end;
end;

procedure TMapServer.Connect(AContext: TIdContext);
begin
  InterlockedIncrement(FConnections);
end;

procedure TMapServer.Disconnect(AContext: TIdContext);
begin
  InterlockedDecrement(FConnections);
end;

class procedure TMapServer.getPage404(Stream: TStream);
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

procedure TMapServer.Restart;
begin
  Stop;
  Run;
end;

procedure TMapServer.Run;
begin
  if not FIsActive then
  begin
    Server := TIdHTTPServer.Create(nil);
    Server.OnConnect := Connect;
    Server.OnDisconnect := Disconnect;
    Server.OnCommandGet := CommandGet;
    Server.DefaultPort := Port;
    Server.ListenQueue := 50;
    Server.Active := true;
    FIsActive := true;
  end;
end;

procedure TMapServer.Stop;
begin
  if FIsActive then
  begin
    Server.Active := false;
    Server.Free;
    FIsActive := false;
    FConnections := 0;
  end;
end;

procedure TMapServer.UploadFinish(Sender: TObject);
begin
  TThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      FUploads.Extract(Sender as TUploadStream);
    end);
end;

end.
