unit MainForm;

interface

uses
  Windows, Messages, Vcl.Forms, Vcl.Controls,
  Vcl.Buttons, Vcl.ExtCtrls, Vcl.Menus, System.Classes, Vcl.ActnList,
  Vcl.ImgList, Vcl.ComCtrls, Vcl.StdCtrls, VirtualTrees,
  //
  Sysutils, Graphics, registry, clipbrd, System.Generics.Collections, Core,
  MapServer, ShellApi, OsuTrackSpy, FSChangeMonitor, Config, NetUtils;

type
  TOsuShareListForm = class(TForm)
    VST: TVirtualStringTree;
    Panel1: TPanel;
    SearchEdit: TEdit;
    ActionList: TActionList;
    ActRefresh: TAction;
    TrayIcon: TTrayIcon;
    TrayPopup: TPopupMenu;
    Exit1: TMenuItem;
    BGImageList: TImageList;
    Timer1: TTimer;
    StatusBar1: TStatusBar;
    StatusImages: TImageList;
    UVST: TVirtualStringTree;
    Splitter1: TSplitter;
    Show1: TMenuItem;
    N1: TMenuItem;
    MapPopup: TPopupMenu;
    CopyLink1: TMenuItem;
    PuushMap1: TMenuItem;
    Icons: TImageList;
    Openbeatmapfolder1: TMenuItem;
    Searchinbloodcat1: TMenuItem;
    GoToCurrentMap: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SearchEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
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
    procedure FormShow(Sender: TObject);
    procedure Openbeatmapfolder1Click(Sender: TObject);
    procedure Searchinbloodcat1Click(Sender: TObject);
    procedure GoToCurrentMapClick(Sender: TObject);
  private
    FOsuTrackPath: string;
    FOsuMapIndex: Integer;
    FCopyMapLinkHotKey: Word;
    FPortStatusIcon: TImage;
    FCore: TOsuShareCore;
    procedure HideUnrelatedItemsSearchCallback(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure GetSelectedItemCallback(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure UpdateMapInfoCallback(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure SelectPlayingMapCallback(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure IdleEventHandler(Sender: TObject; var Done: Boolean);
    procedure WMHotKey(var HTK: TWMHotKey); message WM_HOTKEY;
  end;

var
  OsuShareListForm: TOsuShareListForm;
  toClose: Boolean = false;

implementation

{$R *.dfm}

uses dialogs;

procedure TOsuShareListForm.ActionRefresh(Sender: TObject);
begin
  VST.Clear;
  VST.RootNodeCount := FCore.MapList.Count;
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
  FOsuMapIndex := -1;

  FCore := TOsuShareCore.Create;
  FCore.OnMapListUpdated := ActionRefresh;

  FCopyMapLinkHotKey := GlobalAddAtom('CopyMapLinkHotKey');
  RegisterHotKey(Handle, FCopyMapLinkHotKey, MOD_CONTROL + MOD_NOREPEAT,
    ord('M'));

  FPortStatusIcon := TImage.Create(self);
  with FPortStatusIcon do
  begin
    Parent := StatusBar1;
    AutoSize := true;
    Left := StatusBar1.Panels[0].Width + 2;
    Top := Top + 2;
  end;

  Application.OnIdle := IdleEventHandler;
  Timer1.Enabled := true;
end;

procedure TOsuShareListForm.FormDestroy(Sender: TObject);
begin
  Hide;
  Timer1.Enabled := false;
  UnRegisterHotKey(Handle, FCopyMapLinkHotKey);
  GlobalDeleteAtom(FCopyMapLinkHotKey);
  FCore.Free;
end;

procedure TOsuShareListForm.FormShow(Sender: TObject);
begin
  ActionRefresh(Sender);
end;

procedure TOsuShareListForm.SearchEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  VST.IterateSubtree(nil, HideUnrelatedItemsSearchCallback, nil);
end;

procedure TOsuShareListForm.Searchinbloodcat1Click(Sender: TObject);
var
  index: Integer;
  fpath: string;
begin
  index := -1;
  VST.IterateSubtree(nil, GetSelectedItemCallback, @index, [vsSelected]);
  if index < 0 then
    exit;

  if FCore.MapList[index].BeatmapSetID = 0 then
    with FCore.MapList[index] do
      fpath := Format('http://bloodcat.com/osu/?q=%s %s - %s %s',
        [Source, Artist, Title, Creator])
  else
    fpath := Format('http://bloodcat.com/osu/?q=%d&c=s',
      [FCore.MapList[index].BeatmapSetID]);
  ShellExecute(0, 'open', PChar(fpath), nil, nil, SW_SHOWNORMAL);
end;

procedure TOsuShareListForm.HideUnrelatedItemsSearchCallback
  (Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer;
  var Abort: Boolean);
var
  CanPass: Boolean;
begin
  CanPass := true;
  if (length(SearchEdit.Text) > 0) then
    CanPass := (pos(AnsiLowerCase(SearchEdit.Text),
      AnsiLowerCase(FCore.MapList[Node.index].InfoStr)) > 0);
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
  if not(Kind in [ikNormal, ikSelected]) then // very powerful magic
    ImageIndex := -1
  else
  begin
    // normal code here
    if Node.index = FOsuMapIndex then
      ImageIndex := 7
    else
      ImageIndex := 2;
  end;
end;

procedure TOsuShareListForm.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  CellText := FCore.MapList[Node.index].InfoStr;
end;

procedure TOsuShareListForm.VSTKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (GetAsyncKeyState(VK_CONTROL) <> 0) and (Key = ord('C')) then
    ShareLinkButtonClick(self);
end;

procedure TOsuShareListForm.VSTNodeClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
begin
  FCore.MapList[HitInfo.HitNode.index].InitMap;
  Sender.MultiLine[HitInfo.HitNode] := true;
  Sender.Invalidate;
end;

procedure TOsuShareListForm.WMHotKey(var HTK: TWMHotKey);
begin
  if HTK.HotKey = FCopyMapLinkHotKey then
  begin
    ShareLinkButtonClick(self);
    keybd_event(VK_CONTROL, 0, 0, 0);
    keybd_event(ord('V'), 0, 0, 0);
    keybd_event(ord('V'), 0, KEYEVENTF_KEYUP, 0);
    keybd_event(VK_CONTROL, 0, KEYEVENTF_KEYUP, 0);
  end;
end;

procedure TOsuShareListForm.SelectPlayingMapCallback(Sender: TBaseVirtualTree;
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
  VST.IterateSubtree(nil, GetSelectedItemCallback, @i, [vsSelected]);
  if i = -1 then
    exit;
  if FCore.MapList[i].IsInitialized then
    s := FCore.MapList[i].Artist + ' - ' + FCore.MapList[i].Title
  else
    s := FCore.MapList[i].name;

  Clipboard.AsText := Format('direct link - [http://%s:%d/%s.osz %s]',
    [FCore.Config.Host, FCore.Config.port, FCore.MapList[i].Hash, s]);
  StatusBar1.Panels[2].Text := '"' + s + '" was copied to clipboard';
end;

procedure TOsuShareListForm.Show1Click(Sender: TObject);
begin
  Show;
end;

procedure TOsuShareListForm.TrayIconClick(Sender: TObject);
begin
  Show;
end;

procedure TOsuShareListForm.GetSelectedItemCallback(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
begin
  Integer(Data^) := Node.index;
  Abort := true;
end;

procedure TOsuShareListForm.GoToCurrentMapClick(Sender: TObject);
begin
  VST.IterateSubtree(nil, SelectPlayingMapCallback, Pointer(FOsuMapIndex));
end;

procedure TOsuShareListForm.Timer1Timer(Sender: TObject);
var
  s: string;
  i: Integer;
begin
  StatusBar1.Panels[0].Text := Format('Connected: %d',
    [FCore.MapServer.Connections]);
  UVST.RootNodeCount := FCore.MapServer.CurrentUploads.Count;
  UVST.Invalidate;

  if FCore.TrackSpy.Connected then
  begin
    s := FCore.TrackSpy.FilePath;
    if FOsuTrackPath <> s then
    begin
      FOsuTrackPath := s;
      s := ExtractFilePath(FOsuTrackPath);
      Delete(s, length(s), 1);
      for i := 0 to FCore.MapList.Count - 1 do
        if s = FCore.MapList[i].Path then
        begin
          FOsuMapIndex := i;
          VST.IterateSubtree(nil, SelectPlayingMapCallback,
            Pointer(FOsuMapIndex));
          break;
        end;
    end;
  end
  else
    FCore.TrackSpy.Connect;

  if FCore.Config.IsWhiteIP then
  begin
    Icons.GetIcon(4, FPortStatusIcon.Picture.Icon);
  end
  else
  begin
    Icons.GetIcon(3, FPortStatusIcon.Picture.Icon);
  end;
end;

procedure TOsuShareListForm.UpdateMapInfoCallback(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
begin
  if (Sender.IsEffectivelyVisible[Node]) and
    (not FCore.MapList[Node.index].IsInitialized) then
  begin
    FCore.MapList[Node.index].InitMap;
    Sender.Invalidate;
    Abort := true;
  end;
end;

procedure TOsuShareListForm.Openbeatmapfolder1Click(Sender: TObject);
var
  index: Integer;
  fpath: string;
begin
  index := -1;
  VST.IterateSubtree(nil, GetSelectedItemCallback, @index, [vsSelected]);
  if index < 0 then
    exit;
  fpath := IncludeTrailingBackslash(FCore.MapList[index].Path);
  ShellExecute(self.Handle, 'open', PChar(fpath), '', PChar(fpath),
    SW_SHOWNORMAL);
end;

procedure TOsuShareListForm.PuushButtonClick(Sender: TObject);
var
  index: Integer;
  stream: TFileStream;
  songname, FilePath: string;
begin
  index := -1;
  VST.IterateSubtree(nil, GetSelectedItemCallback, @index, [vsSelected]);
  if index < 0 then
    exit;
  songname := FCore.MapList[index].name;
  FilePath := FCore.Config.TempPath + '/' + FCore.MapList[index].name + '.osz';

  if not FileExists(FilePath) then
  begin
    stream := TFileStream.Create(FilePath, fmCreate);
    FCore.MapList[index].getOszStream(stream);
    stream.Free;
  end;
  try
    ShellExecute(0, 'open', PChar(FCore.Config.PuushPath),
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
  if (i < FCore.MapServer.CurrentUploads.Count) and (Column = 2) then
  begin
    r := CellRect;
    r.Width := trunc(r.Width * FCore.MapServer.CurrentUploads[i]
      .Progress / 100);
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
  if i < FCore.MapServer.CurrentUploads.Count then
    case Column of
      0:
        CellText := FCore.MapServer.CurrentUploads[i].Address;
      1:
        CellText := Format('%f KB/s',
          [FCore.MapServer.CurrentUploads[i].Speed]);
      2:
        CellText := Format('%f%%',
          [FCore.MapServer.CurrentUploads[i].Progress]);
    end;
end;

procedure TOsuShareListForm.IdleEventHandler(Sender: TObject;
  var Done: Boolean);
begin
  VST.IterateSubtree(nil, UpdateMapInfoCallback, nil, [vsVisible]);
  Done := true;
end;

end.
