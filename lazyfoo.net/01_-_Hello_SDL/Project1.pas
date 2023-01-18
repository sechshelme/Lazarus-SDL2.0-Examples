program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

uses
  SDL2;

const
  Screen_Widht = 640;
  Screen_Height = 480;
var
  window: PSDL_Window;
  screenSurface: PSDL_Surface;
  quit: boolean = False;
  e: TSDL_Event;

begin
  if SDL_Init(SDL_INIT_VIDEO) < 0 then begin
    WriteLn('SDL could not initialize! SDL_Error: ', SDL_GetError);
  end else begin
    window := SDL_CreateWindow('SDL Tuorial', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, Screen_Widht, Screen_Height, SDL_WINDOW_SHOWN);
    if window = nil then begin
      WriteLn('Window could not be created! SDL_Error: ', SDL_GetError);
    end else begin
      screenSurface := SDL_GetWindowSurface(window);
      SDL_FillRect(screenSurface, nil, SDL_MapRGB(screenSurface^.format, $FF, $88, $88));
      SDL_UpdateWindowSurface(window);
      while not quit do begin
        while SDL_PollEvent(@e)<>0 do begin
          case e.type_ of
            SDL_QUITEV: begin
              quit := True;
            end;
          end;
        end;
      end;
    end;
  end;
  SDL_DestroyWindow(window);
  SDL_Quit();
end.
