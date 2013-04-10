program osu_share;

{$R *.dres}

uses
  windows,
  Forms,
  MainForm in 'MainForm.pas' {ListForm},
  Core in 'Core.pas',
  Settingsfrm in 'Settingsfrm.pas' {frmSettings},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

var
  Mutex: THandle;

begin
  Mutex := CreateMutex(nil, True, 'osu!share');
  if (Mutex <> 0) and (GetLastError <> ERROR_ALREADY_EXISTS) then
  begin
    Application.Initialize;
    TStyleManager.TrySetStyle('Smokey Quartz Kamri');
  Application.Title := 'osu!share';
    Application.CreateForm(TListForm, ListForm);
  Application.CreateForm(TfrmSettings, frmSettings);
  Application.Run;
    if (Mutex <> 0) then
      CloseHandle(Mutex)
  end;

end.
