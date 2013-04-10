unit MainForm;

interface

uses
  Windows, Messages, IdBaseComponent, IdComponent, IdCustomTCPServer,
  IdCustomHTTPServer, IdHTTPServer, IdContext, Vcl.Forms, Vcl.Controls,
  Vcl.Buttons, Vcl.ExtCtrls, Vcl.Menus, System.Classes, Vcl.ActnList,
  Vcl.ImgList, Vcl.ComCtrls, Vcl.StdCtrls, VirtualTrees,
  //
  Sysutils, Graphics, registry, clipbrd, System.Generics.Collections, Core;

type
  TListForm = class(TForm)
    VST: TVirtualStringTree;
    icons: TImageList;
    Panel1: TPanel;
    SearchEdit: TEdit;
    ActionList: TActionList;
    Action1: TAction;
    PuushButton: TButton;
    ShareLinkButton: TButton;
    buttonIcons: TImageList;
    TrayIcon: TTrayIcon;
    TrayPopup: TPopupMenu;
    Exit1: TMenuItem;
    BGImageList: TImageList;
    Timer1: TTimer;
    StatusBar1: TStatusBar;
    OptionsButton: TButton;
    IdHTTPServer: TIdHTTPServer;
    ImageList1: TImageList;
    UVST: TVirtualStringTree;
    Splitter1: TSplitter;
    Show1: TMenuItem;
    N1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SearchEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure FormShow(Sender: TObject);
    procedure PuushButtonClick(Sender: TObject);
    procedure ShareLinkButtonClick(Sender: TObject);
    procedure IdHTTPServerCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure OptionsButtonClick(Sender: TObject);
    procedure VSTNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure ActionRefresh(Sender: TObject);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure Timer1Timer(Sender: TObject);
    procedure IdHTTPServerConnect(AContext: TIdContext);
    procedure IdHTTPServerDisconnect(AContext: TIdContext);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure UVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure UVSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure Exit1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TrayIconClick(Sender: TObject);
    procedure Show1Click(Sender: TObject);
  private
    procedure IterateSearchCallback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure IterateSelCallback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure IterateUpdateCallback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure UploadFinish(Sender: TObject);
    procedure IdleEventHandler(Sender: TObject; var Done: Boolean);
  public
    procedure RefreshList;
  end;

var
  ListForm: TListForm;
  MapList: TObjectList<TOsuMap>;
  Uploads: TObjectList<TUploadStream>;
  ConnectedClients: Integer = 0;
  Config: TConfig;
  toClose: Boolean = false;

implementation

{$R *.dfm}

uses Settingsfrm, dialogs;

procedure TListForm.RefreshList;
var
  a, b, c: int64;
begin
  MapList.Clear;
  QueryPerformanceCounter(a);
  QueryPerformanceFrequency(c);
  GetMapList(Config.OsuSongPath, MapList);
  QueryPerformanceCounter(b);

  DbgPrint(format('GetMapList %f sec', [(b - a) / c]));

  VST.Clear;
  VST.RootNodeCount := MapList.Count;

  if Config.IsWhiteIP then
    ShareLinkButton.ImageIndex := 2
  else
    ShareLinkButton.ImageIndex := 1;
  ShareLinkButton.Enabled := false;
  ShareLinkButton.Enabled := true;
end;

procedure TListForm.ActionRefresh(Sender: TObject);
begin
  RefreshList;
end;

procedure TListForm.Exit1Click(Sender: TObject);
begin
  toClose := true;
  Close;
end;

procedure TListForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TrayIcon.Visible := false;
end;

procedure TListForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not toClose then
    Hide;
  CanClose := toClose;
end;

procedure TListForm.FormCreate(Sender: TObject);
var
  a, b, c: int64;
begin
  Application.OnIdle := IdleEventHandler;
  QueryPerformanceFrequency(c);
  QueryPerformanceCounter(a);
  Config := TConfig.Create('osu!share.ini', 'osu!share');
  QueryPerformanceCounter(b);
  DbgPrint(format('TConfig.Create %f sec', [(b - a) / c]));

  MapList := TObjectList<TOsuMap>.Create(true);
  Uploads := TObjectList<TUploadStream>.Create(false);
  DirRemove(Config.TempPath);
  CreateDir(Config.TempPath);
end;

procedure TListForm.FormDestroy(Sender: TObject);
begin
  MapList.Free;
  Uploads.Free;
  DirRemove(Config.TempPath);
  Config.Free;
end;

procedure TListForm.FormShow(Sender: TObject);
var
  ThreadID: cardinal;
begin
  RefreshList;
  OptionsButton.Parent := StatusBar1;
  OptionsButton.Top := 0;
  OptionsButton.Left := 0;
end;

procedure TListForm.OptionsButtonClick(Sender: TObject);
begin
  frmSettings.ShowModal;
end;

procedure TListForm.SearchEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  VST.IterateSubtree(nil, IterateSearchCallback, nil);
end;

procedure TListForm.IterateSearchCallback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer;
  var Abort: Boolean);
var
  CanPass: Boolean;
begin
  CanPass := true;
  if (length(SearchEdit.Text) > 0) then
    CanPass := (pos(AnsiLowerCase(SearchEdit.Text), AnsiLowerCase(MapList[Node.index].name)) > 0);
  Sender.IsVisible[Node] := CanPass;
end;

procedure TListForm.VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  bmp: TBitmap;
  i: Integer;
begin
  i := 0;
  bmp := TBitmap.Create;
  bmp.SetSize(CellRect.Width, CellRect.Height);
  BGImageList.GetBitmap(i, bmp);
  TargetCanvas.StretchDraw(CellRect, bmp);
  bmp.Free;
end;

procedure TListForm.VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  ImageIndex := 0;
end;

procedure TListForm.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  map: TOsuMap;
begin
  map := MapList[Node.index];
  if map.IsInitialized then
  begin
    CellText := map.Artist + ' - ' + map.Title + ' '#13#10'Creator: ' + map.Creator + ' '#13#10'Source: ' + map.Source;
  end
  else
    CellText := map.name;
end;

procedure TListForm.VSTNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  MapList[HitInfo.HitNode.index].InitMap;
  Sender.MultiLine[HitInfo.HitNode] := true;
  Sender.Invalidate;
end;

procedure TListForm.ShareLinkButtonClick(Sender: TObject);
var
  i: Integer;
  s: string;
begin
  i := -1;
  VST.IterateSubtree(nil, IterateSelCallback, @i, [vsSelected]);
  if i = -1 then
    exit;
  if MapList[i].IsInitialized then
    s := MapList[i].Artist + ' - ' + MapList[i].Title
  else
    s := MapList[i].name;

  Clipboard.AsText := format('direct link: [http://%s:%d/%d.osz %s]', [Config.Host, Config.port, i, s]);
  StatusBar1.Panels[3].Text := '"' + s + '" was copied to clipboard';
end;

procedure TListForm.Show1Click(Sender: TObject);
begin
  Show;
end;

procedure TListForm.TrayIconClick(Sender: TObject);
begin
  Show;
end;

procedure TListForm.IterateSelCallback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
begin
  Integer(Data^) := Node.index;
  Abort := true;
end;

procedure TListForm.Timer1Timer(Sender: TObject);
begin
  StatusBar1.Panels[1].Text := format('Connected: %d', [ConnectedClients]);
  UVST.RootNodeCount := Uploads.Count;
  UVST.Invalidate;
end;

procedure TListForm.IterateUpdateCallback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer;
  var Abort: Boolean);
begin
  if (Sender.IsEffectivelyVisible[Node]) and (not MapList[Node.index].IsInitialized) then
  begin
    MapList[Node.index].InitMap;
    Abort := true;
  end;
  Sender.MultiLine[Node] := true;
end;

procedure TListForm.PuushButtonClick(Sender: TObject);
var
  index: Integer;
  stream: TFileStream;
  songname, filepath: string;
begin
  index := -1;
  VST.IterateSubtree(nil, IterateSelCallback, @index, [vsSelected]);
  if index < 0 then
    exit;
  songname := MapList[index].name;
  filepath := Config.TempPath + '/' + MapList[index].name + '.osz';

  if not FileExists(filepath) then
  begin
    stream := TFileStream.Create(filepath, fmCreate);
    MakeOsz(MapList[index].Path, stream);
    stream.Free;
  end;
  try
    WinExec(PAnsiChar(Config.PuushPath + '"' + filepath + '"'), 0);
  except
  end;
end;

procedure TListForm.UVSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  i: Integer;
  bmp: TBitmap;
  r: TRect;
begin
  i := Node.index;
  if (i < Uploads.Count) and (Column = 2) then
  begin
    r := CellRect;
    r.Width := trunc(r.Width * Uploads[i].Progress / 100);
    bmp := TBitmap.Create;
    bmp.SetSize(CellRect.Width, CellRect.Height);
    BGImageList.GetBitmap(1, bmp);
    TargetCanvas.StretchDraw(r, bmp);
    bmp.Free;
  end;
end;

procedure TListForm.UVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  i: Integer;
begin
  CellText := '';
  i := Node.index;
  if i < Uploads.Count then
    case Column of
      0:
        CellText := Uploads[i].Address;
      1:
        CellText := format('%f KB/s', [Uploads[i].Speed]);
      2:
        CellText := format('%f%%', [Uploads[i].Progress]);
    end;
end;

procedure TListForm.IdHTTPServerCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);

  function MyEncodeUrl(Source: string): string;
  var
    i: Integer;
  begin
    result := '';
    for i := 1 to length(Source) do
      if not(Source[i] in ['A' .. 'Z', 'a' .. 'z', '0', '1' .. '9', '-', '_', '~', '.']) then
        result := result + '%' + inttohex(ord(Source[i]), 2)
      else
        result := result + Source[i];
  end;

var
  stream: TUploadStream;
  req: string;
  i: Integer;
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

    stream := TUploadStream.Create;
    stream.Address := ARequestInfo.RemoteIP;
    stream.OnDestroy := UploadFinish;

    TThread.Synchronize(TThread.CurrentThread,
      procedure
      begin
        Uploads.Add(stream);
      end);

    req := StringReplace(req, '.osz', '', [rfReplaceAll]);

    if not TryStrToInt(req, i) or (i < 0) or (i >= MapList.Count) then
    begin
      page404(stream);
      AResponseInfo.ContentType := 'text/html';
      AResponseInfo.ResponseNo := 404;
      AResponseInfo.ContentLength := stream.Size;
      AResponseInfo.ContentStream := stream; // тут нельзя разрушить stream.Free; его порушит сам серв
      exit;
    end;
    MakeOsz(MapList[i].Path, stream);
    AResponseInfo.ContentType := 'application/octet-stream';
    AResponseInfo.CustomHeaders.Values['Content-disposition'] := 'attachment; filename=' +
      '"' + StringReplace(MapList[i].name, ' ', '%20', [rfReplaceAll]) + '.osz";';

    AResponseInfo.ContentText := '';
    AResponseInfo.ContentLength := stream.Size;
    AResponseInfo.ContentStream := stream; // тут нельзя разрушить stream.Free; его порушит сам серв
  except
  end;
end;

procedure TListForm.UploadFinish(Sender: TObject);
begin
  Uploads.Extract((Sender as TUploadStream));
end;

procedure TListForm.IdHTTPServerConnect(AContext: TIdContext);
begin
  InterlockedIncrement(ConnectedClients);
end;

procedure TListForm.IdHTTPServerDisconnect(AContext: TIdContext);
begin
  InterlockedDecrement(ConnectedClients);
end;

procedure TListForm.IdleEventHandler(Sender: TObject; var Done: Boolean);
begin
  VST.IterateSubtree(nil, IterateUpdateCallback, nil, [vsVisible]);
  Done := false;
end;

end.
