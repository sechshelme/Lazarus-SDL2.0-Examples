program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

// https://stackoverflow.com/questions/37978149/sdl1-sdl2-resolution-list-building-with-a-custom-screen-mode-class

uses
  SDL2,
  unixtype;

//Widht: 3440 Height: 1440
//Widht: 2560 Height: 1440
//Widht: 2560 Height: 1080
//Widht: 1920 Height: 1200
//Widht: 1920 Height: 1080
//Widht: 1680 Height: 1050
//Widht: 1600 Height: 1200
//Widht: 1600 Height:  900
//Widht: 1440 Height:  900
//Widht: 1366 Height:  768
//Widht: 1280 Height: 1024
//Widht: 1280 Height:  800
//Widht: 1280 Height:  720
//Widht: 1152 Height:  864
//Widht: 1024 Height:  768
//Widht:  800 Height:  600
//Widht:  720 Height:  480
//Widht:  640 Height:  480

var
  sdlWindow1: PSDL_Window;
  sdlRenderer: PSDL_Renderer;
  sdlsurface: PSDL_Surface;
  r: TSDL_Rect;

  procedure VideoInfo;
  const
    mode: TSDL_DisplayMode = (format: SDL_PIXELFORMAT_UNKNOWN; w: 0; h: 0; refresh_rate: 0; driverdata: nil);
  var
    display_count, VideoDriver_count, mode_count: cint;
    display_index, mode_index, i: integer;
    modes: array of TSDL_DisplayMode;
  begin
    VideoDriver_count := SDL_GetNumVideoDrivers;
    WriteLn('Video Driver Count: ', VideoDriver_count);

    for i := 0 to VideoDriver_count - 1 do begin
      WriteLn(SDL_GetVideoDriver(i));
    end;

    display_count := SDL_GetNumVideoDisplays;
    WriteLn('Display Count: ', display_count);

    for display_index := 0 to display_count - 1 do begin
      SDL_Log(' Display %i: ', [display_index]);
      mode_count := SDL_GetNumDisplayModes(display_index);
      WriteLn('Mode Count: ', mode_count);

      for mode_index := 0 to mode_count - 1 do begin
        if SDL_GetDisplayMode(display_index, mode_index, @mode) = 0 then begin
          SDL_Log(' %i bpp   %i x %i @ %iHz', [SDL_BITSPERPIXEL(mode.format), mode.w, mode.h, mode.refresh_rate]);
        end;
        Insert(mode, modes, 0);
      end;
    end;

    for i := 0 to Length(modes) - 1 do begin
      WriteLn(modes[i].w, 'x', modes[i].h);
    end;
  end;

begin

  //initilization of video subsystem
  if SDL_Init(SDL_INIT_VIDEO) < 0 then begin
    Halt;
  end;

  VideoInfo;


  // full set up
  sdlWindow1 := SDL_CreateWindow('Window1', 50, 50, 500, 500, SDL_WINDOW_SHOWN);
  if sdlWindow1 = nil then begin
    Halt;
  end;
  //
  //  sdlRenderer := SDL_CreateRenderer(sdlWindow1, -1, 0);
  //  if sdlRenderer = nil then begin
  //    Halt;
  //  end;

  sdlsurface := SDL_GetWindowSurface(sdlWindow1);

  // quick set up
  {
  if SDL_CreateWindowAndRenderer(500, 500, SDL_WINDOW_SHOWN, @sdlWindow1, @sdlRenderer) <> 0
    then Halt;
  }
  r.x := 10;
  r.y := 10;
  r.h := 100;
  r.w := 100;


  SDL_FillRect(sdlsurface, @r, $FF);
  SDL_UpdateWindowSurface(sdlWindow1);


  //  SDL_RenderPresent(sdlRenderer);
  // render to window for 2 seconds
  SDL_Delay(2000);

  // clear memory
  SDL_DestroyRenderer(sdlRenderer);
  SDL_DestroyWindow(sdlWindow1);

  //closing SDL2
  SDL_Quit;

end.
