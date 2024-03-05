program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

// {$UNITPATH ../units/units}

uses
  crt,
  gl,
  ctypes,
  SDL3_stdinc,
  SDL3_rect,
  SDL3_init,
  SDL3_timer,
  SDL3_video;

  //const
  //  SDL_LibName = '/usr/local/lib/libSDL';
  //  SDL_LibName = 'libSDL3.so.0';
  //  SDL_LibName = 'libSDL2.so.0';

type
  PSDL_Version = ^TSDL_Version;

  //TSDL_Window=record
  //end;
  //
  //PSDL_Window=^TSDL_Window;

  TSDL_Version = record
    major,           {**< major version *}
    minor,           {**< minor version *}
    patch: cuint8;   {**< update version *}
  end;

  //const
  //  SDL_INIT_VIDEO = $00000020; //,  /**< `SDL_INIT_VIDEO` implies `SDL_INIT_EVENTS` */


  //  procedure SDL_GetVersion(ver: PSDL_Version); cdecl; external SDL_LibName;

  //  function SDL_Init(flags: cuint32): cint; cdecl; external SDL_LibName;
  //  function SDL_CreateWindow(title: PChar; w, h: cint; flags: cuint32): PSDL_Window; cdecl; external SDL_LibName;
  //  function SDL_DestroyWindow(window:PSDL_Window): Pointer; cdecl; external SDL_LibName;
  //  procedure SDL_Delay(ms: cuint32); cdecl; external SDL_LibName;

  procedure TestInRect;
  var
    p: TSDL_Point;
    r, r2: TSDL_Rect;
    i: integer;
  begin
    r.x := 10;
    r.y := 3;
    r.w := 60;
    r.h := 15;
    for i := 0 to 10000 do begin
      p.x := Random(80) + 1;
      p.y := Random(25) + 1;
      if SDL_PointInRect(@p, @r) = SDL_TRUE then begin
        TextAttr := 4;
      end else begin
        TextAttr := 6;
      end;
      GotoXY(p.x, p.y);
      Write('X');
    end;
  end;

var
  ver: TSDL_Version;
  window: PSDL_Window;

begin
  TestInRect;

  SDL_init(SDL_INIT_VIDEO);

  window := SDL_CreateWindow('SDL3 Window', 320, 200, 0);
  SDL_Delay(3000);
  SDL_DestroyWindow(window);

  //  SDL_VERSION(ver);
  //WriteLn(ver.major,' .',ver.minor,'.',ver.patch);
  //  SDL_GetVersion(@ver);
  //  WriteLn(ver.major, '.', ver.minor, '.', ver.patch);
end.
