program Project1;

uses
  gl,
  ctypes,
  SDL3_version,
  SDL3_stdinc,
  SDL3_rect,
  SDL3_init,
  SDL3_timer,
  SDL3_video;

var
  ver: TSDL_Version;
  window: PSDL_Window;

begin
  SDL_init(SDL_INIT_VIDEO);

  window := SDL_CreateWindow('SDL3 Window', 320, 200, 0);
  SDL_Delay(3000);
  SDL_DestroyWindow(window);

  SDL_VERSION(@ver);
  WriteLn(ver.major, '.', ver.minor, '.', ver.patch);
  SDL_GetVersion(@ver);
  WriteLn(ver.major, '.', ver.minor, '.', ver.patch);
end.
