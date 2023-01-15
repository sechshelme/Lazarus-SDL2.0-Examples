program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

// {$UNITPATH ../units/units}

uses
  SDL2;

var
  sdlWindow1: PSDL_Window;
  sdlRenderer: PSDL_Renderer;
  sdlsurface:PSDL_Surface;
  r: TSDL_Rect;

begin

  //initilization of video subsystem
  if SDL_Init(SDL_INIT_VIDEO) < 0 then begin
    Halt;
  end;

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

  sdlsurface:=SDL_GetWindowSurface(sdlWindow1);

  // quick set up
  {
  if SDL_CreateWindowAndRenderer(500, 500, SDL_WINDOW_SHOWN, @sdlWindow1, @sdlRenderer) <> 0
    then Halt;
  }
  r.x:=10;
  r.y:=10;
  r.h:=100;
  r.w:=100;


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
