unit MapServer;

interface

uses IdHTTPServer, IdContext, IdCustomHTTPServer, windows, Core, Classes,
  Sysutils, System.Generics.Collections, IdURI;

type
  TMapServer = class
  private
    FPort: Word;
    Server: TIdHTTPServer;
    FIsActive: boolean;
    FConnections: Integer;
    FUploads: TObjectList<TUploadStream>;
    FMapKeeper: IMapKeeper;
    procedure Disconnect(AContext: TIdContext);
    procedure Connect(AContext: TIdContext);
    procedure CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure UploadFinish(Sender: TObject);
  public
    constructor Create(MapKeeper: IMapKeeper);
    destructor Destroy; override;
    procedure Run;
    procedure Stop;
    procedure Restart;
    property Port: Word read FPort write FPort;
    property Connections: Integer read FConnections;
    property CurrentUploads: TObjectList<TUploadStream> read FUploads;
  end;

implementation

{ TMapServer }

constructor TMapServer.Create(MapKeeper: IMapKeeper);
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
  inherited;
end;

procedure TMapServer.CommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  stream: TUploadStream;
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

    stream := FMapKeeper.GetMap(req, MapName);

    if Assigned(stream) then
    begin
      stream.Address := ARequestInfo.RemoteIP;
      stream.OnDestroy := UploadFinish;
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          FUploads.Add(stream);
        end);

      MapName := TIdURI.ParamsEncode(MapName);
      AResponseInfo.ContentType := 'application/octet-stream';
      AResponseInfo.CustomHeaders.Values['Content-disposition'] :=
        Format('attachment; filename="%s.osz";', [MapName]);

      AResponseInfo.ContentText := '';
      AResponseInfo.ContentLength := stream.Size;
      AResponseInfo.ContentStream := stream;
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
