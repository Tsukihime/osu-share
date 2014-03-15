program osu_share;

{$R *.dres}

uses
  windows,
  Forms,
  MainForm in 'MainForm.pas' {OsuShareListForm},
  Core in 'Core.pas',
  MapServer in 'MapServer.pas',
  OsuTrackSpy in 'OsuTrackSpy.pas';

{$R *.res}

var
  Mutex: THandle;

begin
  Mutex := CreateMutex(nil, True, 'osu!share');
  if (Mutex <> 0) and (GetLastError <> ERROR_ALREADY_EXISTS) then
  begin
    Application.Initialize;
    Application.Title := 'osu!share';
    Application.CreateForm(TOsuShareListForm, OsuShareListForm);
  Application.Run;
    if (Mutex <> 0) then
      CloseHandle(Mutex)
  end;

end.
