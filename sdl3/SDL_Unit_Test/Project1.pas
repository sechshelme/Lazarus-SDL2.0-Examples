program Project1;

uses
  gtk2,
  ctypes,
  //  SDL_quit ,
  SDL3_main,
  SDL3_mutex,
  SDL3_scancode,
  SDL3_keycode,
  SDL3_events,
  SDL3_messagebox,
  SDL3_surface,
  SDL3_version,
  SDL3_stdinc,
  SDL3_rect,
  SDL3_assert,
  SDL3_error,
  SDL3_init,
  SDL3_timer,
  SDL3_video;

var
  ver: TSDL_Version;
  window: PSDL_Window;
  e: TSDL_Event;
  quit: boolean = False;
  image, screen: PSDL_Surface;
  dstrect: TSDL_Rect = (x: 100; y: 100; w: 200; h: 200);

  procedure ShowMessageBox;
  const
    // https://wiki.libsdl.org/SDL3/SDL_ShowMessageBox

    Buttons: array [0..3] of TSDL_MessageBoxButtonData = (
      (flags: 0; buttonID: 0; Text: 'no'),
      (flags: SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT; buttonID: 1; Text: 'yes'),
      (flags: SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT; buttonID: 2; Text: 'cancel'),
      (flags: 0; buttonID: 2; Text: 'help'));

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

  procedure ShowAssert;
  var
    Data: TSDL_AssertData;
    i: TSDL_AssertState;
  begin
    i := SDL_ReportAssertion(@Data, 'Ein Schwerwiegender Fehler !', 'xxx.xxx', 100);
    WriteLn(i);
  end;

  procedure ShowError;
  begin
    SDL_SetError('Fehler', 123);
    WriteLn('error');
  end;

begin
  SDL_init(SDL_INIT_VIDEO);

  window := SDL_CreateWindow('SDL3 Window', 320, 200, SDL_WINDOW_RESIZABLE);
  image := SDL_LoadBMP('mauer.bmp');
  screen := SDL_GetWindowSurface(window);

  while not quit do begin
    while SDL_PollEvent(@e) <> 0 do begin
      case e.type_ of
        //        SDL_KEYDOWN: begin
        SDL_EVENT_KEY_DOWN: begin
          case e.key.keysym.sym of

            SDLK_ESCAPE: begin
              //            SDLK_ESCAPE: begin
              WriteLn('down');
              quit := True;
            end;
            SDLK_m: begin
              ShowMessageBox;
            end;
            SDLK_a: begin
              ShowAssert;
            end;
            SDLK_e: begin
              ShowError;
            end;
          end;
        end;
        SDL_EVENT_QUIT: begin
          WriteLn('quit');
          //          SDL_QUITEV: begin
          quit := True;
        end;
      end;
    end;
    SDL_BlitSurface(image, nil, screen, nil);
    SDL_BlitSurface(image, nil, screen, @dstrect);

    SDL_BlitSurfaceScaled(image, nil, screen, nil, SDL_SCALEMODE_LINEAR);

    SDL_UpdateWindowSurface(Window);
  end;


  //  SDL_Delay(3000);
  SDL_DestroyWindow(window);

  SDL_VERSION(@ver);
  WriteLn(ver.major, '.', ver.minor, '.', ver.patch);
  SDL_GetVersion(@ver);
  WriteLn(ver.major, '.', ver.minor, '.', ver.patch);

  SDL_Quit;
end.