unit MainForm;

interface

uses
  Windows, Messages, Vcl.Forms, Vcl.Controls,
  Vcl.Buttons, Vcl.ExtCtrls, Vcl.Menus, System.Classes, Vcl.ActnList,
  Vcl.ImgList, Vcl.ComCtrls, Vcl.StdCtrls, VirtualTrees,
  //
  Sysutils, Graphics, registry, clipbrd, System.Generics.Collections, Core,
  MapServer, ShellApi, OsuTrackSpy;

type
  TOsuShareListForm = class(TForm, IMapKeeper)
    VST: TVirtualStringTree;
    icons: TImageList;
    Panel1: TPanel;
    SearchEdit: TEdit;
    ActionList: TActionList;
    ActRefresh: TAction;
    PuushButton: TButton;
    ShareLinkButton: TButton;
    buttonIcons: TImageList;
    TrayIcon: TTrayIcon;
    TrayPopup: TPopupMenu;
    Exit1: TMenuItem;
    BGImageList: TImageList;
    Timer1: TTimer;
    StatusBar1: TStatusBar;
    ImageList1: TImageList;
    UVST: TVirtualStringTree;
    Splitter1: TSplitter;
    Show1: TMenuItem;
    N1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SearchEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure FormShow(Sender: TObject);
    procedure PuushButtonClick(Sender: TObject);
    procedure ShareLinkButtonClick(Sender: TObject);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure ActionRefresh(Sender: TObject);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure UVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure UVSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure Exit1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TrayIconClick(Sender: TObject);
    procedure Show1Click(Sender: TObject);
    procedure VSTKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FOsuTrackPath: string;
    FOsuMapIndex: Integer;
    procedure IterateSearchCallback(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure IterateSelCallback(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    procedure IterateUpdateCallback(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure IterateFlashCallback(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    procedure IdleEventHandler(Sender: TObject; var Done: Boolean);
  public
    procedure RefreshList;
    function GetMap(ServReq: string; var MapName: string): TUploadStream;
  end;

var
  OsuShareListForm: TOsuShareListForm;
  MapList: TObjectList<TOsuMap>;
  MapServ: TMapServer;
  Config: TConfig;
  TrackSpy: TOsuTrackSpy;
  toClose: Boolean = false;

implementation

{$R *.dfm}

uses dialogs;

procedure TOsuShareListForm.RefreshList;
begin
  MapList.Clear;
  GetMapList(Config.OsuSongPath, MapList);

  VST.Clear;
  VST.RootNodeCount := MapList.Count;

  if Config.IsWhiteIP then
    ShareLinkButton.ImageIndex := 2
  else
    ShareLinkButton.ImageIndex := 1;
  ShareLinkButton.Enabled := false;
  ShareLinkButton.Enabled := true;
end;

procedure TOsuShareListForm.ActionRefresh(Sender: TObject);
begin
  RefreshList;
end;

procedure TOsuShareListForm.Exit1Click(Sender: TObject);
begin
  toClose := true;
  Close;
end;

procedure TOsuShareListForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  TrayIcon.Visible := false;
end;

procedure TOsuShareListForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if not toClose then
    Hide;
  CanClose := toClose;
end;

procedure TOsuShareListForm.FormCreate(Sender: TObject);
begin
  Application.OnIdle := IdleEventHandler;
  Config := TConfig.Create('osu!share.ini', 'osu!share');
  MapList := TObjectList<TOsuMap>.Create(true);
  DirRemove(Config.TempPath);
  CreateDir(Config.TempPath);
  MapServ := TMapServer.Create(self);
  MapServ.Port := Config.Port;
  MapServ.Run;
  FOsuMapIndex := -1;

  TrackSpy := TOsuTrackSpy.Create;
  TrackSpy.Connect;
end;

procedure TOsuShareListForm.FormDestroy(Sender: TObject);
begin
  MapServ.Stop;
  MapServ.Free;
  MapList.Free;
  DirRemove(Config.TempPath);
  Config.Free;
end;

procedure TOsuShareListForm.FormShow(Sender: TObject);
begin
  RefreshList;
end;

function TOsuShareListForm.GetMap(ServReq: string; var MapName: string)
  : TUploadStream;
var
  i: Integer;
  us: TUploadStream;
begin
  Result := nil;
  if TryStrToInt(ServReq, i) or (i < 0) or (i >= MapList.Count) then
  begin
    us := TUploadStream.Create;
    MakeOsz(MapList[i].Path, us);
    Result := us;

    if MapList[i].IsInitialized then
      MapName := MapList[i].Artist + ' - ' + MapList[i].Title
    else
      MapName := MapList[i].name;
  end;
end;

procedure TOsuShareListForm.SearchEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  VST.IterateSubtree(nil, IterateSearchCallback, nil);
end;

procedure TOsuShareListForm.IterateSearchCallback(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  CanPass: Boolean;
begin
  CanPass := true;
  if (length(SearchEdit.Text) > 0) then
    CanPass := (pos(AnsiLowerCase(SearchEdit.Text),
      AnsiLowerCase(MapList[Node.index].name)) > 0);
  Sender.IsVisible[Node] := CanPass;
end;

procedure TOsuShareListForm.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  bmp: TBitmap;
  i: Integer;
begin
  i := 0;
  if Node.index = FOsuMapIndex then
    i := 1;
  bmp := TBitmap.Create;
  bmp.SetSize(CellRect.Width, CellRect.Height);
  BGImageList.GetBitmap(i, bmp);
  TargetCanvas.StretchDraw(CellRect, bmp);
  bmp.Free;
end;

procedure TOsuShareListForm.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  ImageIndex := 0;
end;

procedure TOsuShareListForm.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  map: TOsuMap;
begin
  map := MapList[Node.index];
  if map.IsInitialized then
  begin
    if map.Source = '' then
      CellText := Format('%s - %s [%s]', [map.Artist, map.Title, map.Creator])
    else
      CellText := Format('%s (%s) - %s [%s]', [map.Source, map.Artist,
        map.Title, map.Creator])
  end
  else
    CellText := map.name;
end;

procedure TOsuShareListForm.VSTKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (GetAsyncKeyState(VK_CONTROL) <> 0) and (Key = ord('C')) then
    ShareLinkButtonClick(ShareLinkButton);
end;

procedure TOsuShareListForm.VSTNodeClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
begin
  MapList[HitInfo.HitNode.index].InitMap;
  Sender.MultiLine[HitInfo.HitNode] := true;
  Sender.Invalidate;
end;

procedure TOsuShareListForm.IterateFlashCallback(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
begin
  if Integer(Data) = Node.index then
  begin
    Sender.ScrollIntoView(Node, true, false);
    Sender.Selected[Node] := true;
    Abort := true;
  end;
end;

procedure TOsuShareListForm.ShareLinkButtonClick(Sender: TObject);
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

  Clipboard.AsText := Format('direct link - [http://%s:%d/%d.osz %s]',
    [Config.Host, Config.Port, i, s]);
  StatusBar1.Panels[3].Text := '"' + s + '" was copied to clipboard';
end;

procedure TOsuShareListForm.Show1Click(Sender: TObject);
begin
  Show;
end;

procedure TOsuShareListForm.TrayIconClick(Sender: TObject);
begin
  Show;
end;

procedure TOsuShareListForm.IterateSelCallback(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
begin
  Integer(Data^) := Node.index;
  Abort := true;
end;

procedure TOsuShareListForm.Timer1Timer(Sender: TObject);
var
  s: string;
  i: Integer;
begin
  StatusBar1.Panels[1].Text := Format('Connected: %d', [MapServ.Connections]);
  UVST.RootNodeCount := MapServ.CurrentUploads.Count;
  UVST.Invalidate;

  if TrackSpy.Connected then
  begin
    s := TrackSpy.FilePath;
    if FOsuTrackPath <> s then
    begin
      FOsuTrackPath := s;
      s := ExtractFilePath(FOsuTrackPath);
      Delete(s, length(s), 1);
      for i := 0 to MapList.Count - 1 do
        if s = MapList[i].Path then
        begin
          FOsuMapIndex := i;
          VST.IterateSubtree(nil, IterateFlashCallback, Pointer(i));
          break;
        end;
    end;
  end
  else
    TrackSpy.Connect;
end;

procedure TOsuShareListForm.IterateUpdateCallback(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
begin
  if (Sender.IsEffectivelyVisible[Node]) and
    (not MapList[Node.index].IsInitialized) then
  begin
    MapList[Node.index].InitMap;
    Abort := true;
  end;
  Sender.MultiLine[Node] := true;
end;

procedure TOsuShareListForm.PuushButtonClick(Sender: TObject);
var
  index: Integer;
  stream: TFileStream;
  songname, FilePath: string;
begin
  index := -1;
  VST.IterateSubtree(nil, IterateSelCallback, @index, [vsSelected]);
  if index < 0 then
    exit;
  songname := MapList[index].name;
  FilePath := Config.TempPath + '/' + MapList[index].name + '.osz';

  if not FileExists(FilePath) then
  begin
    stream := TFileStream.Create(FilePath, fmCreate);
    MakeOsz(MapList[index].Path, stream);
    stream.Free;
  end;
  try
    ShellExecute(0, 'open', PChar(Config.PuushPath),
      PChar('-upload "' + FilePath + '"'), '', 0);
  except
  end;
end;

procedure TOsuShareListForm.UVSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  i: Integer;
  bmp: TBitmap;
  r: TRect;
begin
  i := Node.index;
  if (i < MapServ.CurrentUploads.Count) and (Column = 2) then
  begin
    r := CellRect;
    r.Width := trunc(r.Width * MapServ.CurrentUploads[i].Progress / 100);
    bmp := TBitmap.Create;
    bmp.SetSize(CellRect.Width, CellRect.Height);
    BGImageList.GetBitmap(1, bmp);
    TargetCanvas.StretchDraw(r, bmp);
    bmp.Free;
  end;
end;

procedure TOsuShareListForm.UVSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  i: Integer;
begin
  CellText := '';
  i := Node.index;
  if i < MapServ.CurrentUploads.Count then
    case Column of
      0:
        CellText := MapServ.CurrentUploads[i].Address;
      1:
        CellText := Format('%f KB/s', [MapServ.CurrentUploads[i].Speed]);
      2:
        CellText := Format('%f%%', [MapServ.CurrentUploads[i].Progress]);
    end;
end;

procedure TOsuShareListForm.IdleEventHandler(Sender: TObject;
  var Done: Boolean);
begin
  VST.IterateSubtree(nil, IterateUpdateCallback, nil, [vsVisible]);
  Done := true;
end;

end.
