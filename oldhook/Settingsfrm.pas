unit Settingsfrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, FileCtrl, inifiles;

type
  TfrmSettings = class(TForm)
    IpEdit: TEdit;
    OkButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    PortEdit: TEdit;
    osuDirEdit: TEdit;
    Label3: TLabel;
    BrowseButton: TButton;
    OpenDialog: TOpenDialog;
    AutoButton: TButton;
    procedure BrowseButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AutoButtonClick(Sender: TObject);
  private
    procedure UpdateForm;
  public
    { Public declarations }
  end;

var
  frmSettings: TfrmSettings;

implementation

uses MainForm, helper;

{$R *.dfm}

procedure TfrmSettings.BrowseButtonClick(Sender: TObject);
var
  chosenDirectory: string;
begin
  if SelectDirectory('Выберите каталог osu!', '', chosenDirectory) then
    osuDirEdit.Text := chosenDirectory;
end;

procedure TfrmSettings.AutoButtonClick(Sender: TObject);
begin
  Config.ReInitExternalAddres;
  UpdateForm;
end;

procedure TfrmSettings.OkButtonClick(Sender: TObject);
begin
  Config.OsuSongPath := osuDirEdit.Text;
  if IpEdit.Text <> '' then
    Config.Host := IpEdit.Text;
  OsuShareListForm.RefreshList;
  Close;
end;

procedure TfrmSettings.UpdateForm;
begin
  IpEdit.Text := Config.Host;
  PortEdit.Text := IntToStr(Config.port);
  osuDirEdit.Text := Config.OsuSongPath;
end;

procedure TfrmSettings.FormShow(Sender: TObject);
begin
  UpdateForm;
end;

end.
