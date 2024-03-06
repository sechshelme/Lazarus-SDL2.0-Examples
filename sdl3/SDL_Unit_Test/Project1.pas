program Project1;

uses
  gtk2,
  ctypes,
  SDL3_messagebox,
  SDL3_version,
  SDL3_stdinc,
  SDL3_rect,
  SDL3_init,
  SDL3_timer,
  SDL3_video;

var
  ver: TSDL_Version;
  window: PSDL_Window;

  procedure ShowMessageBox;
  const
    // https://wiki.libsdl.org/SDL3/SDL_ShowMessageBox

    Buttons: array [0..2] of TSDL_MessageBoxButtonData = (
      (flags: 0; buttonID: 0; Text: 'no'),
      (flags: SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT; buttonID: 1; Text: 'yes'),
      (flags: SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT; buttonID: 2; Text: 'cancel'));

    colorSchema: TSDL_MessageBoxColorScheme = (colors: (
      (r: 255; g: 0; b: 0),
      (r: 0; g: 255; b: 0),
      (r: 255; g: 255; b: 0),
      (r: 0; g: 0; b: 255),
      (r: 255; g: 0; b: 255)));

    messageboxdata: TSDL_MessageBoxData = (
      flags: SDL_MESSAGEBOX_INFORMATION;
      window: nil;
      title: 'example message box';
      message: 'select a button';
      numbuttons: length(Buttons);
      Buttons: @Buttons;
      colorScheme: @colorSchema);
  var
    buttonid: longint;
  begin
    SDL_ShowMessageBox(@messageboxdata, @buttonid);
    //            SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, 'Missing file', 'File is missing. Please reinstall the program.', nil);
  end;

begin
  SDL_init(SDL_INIT_VIDEO);

  window := SDL_CreateWindow('SDL3 Window', 320, 200, 0);

  ShowMessageBox;

  SDL_Delay(3000);
  SDL_DestroyWindow(window);

  SDL_VERSION(@ver);
  WriteLn(ver.major, '.', ver.minor, '.', ver.patch);
  SDL_GetVersion(@ver);
  WriteLn(ver.major, '.', ver.minor, '.', ver.patch);
end.
