program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

uses
  SDL2;

const
  Screen_Widht = 640;
  Screen_Height = 480;
var
  gWindow: PSDL_Window;
  gscreenSurface: PSDL_Surface;
  gHelloWorld: PSDL_Surface;
  quit: boolean = False;
  e: TSDL_Event;

  function init: boolean;
  var
    sucess: boolean = True;
  begin
    sucess := True;
    if SDL_Init(SDL_INIT_VIDEO) < 0 then begin
      WriteLn('SDL could not initialize! SDL_Error: ', SDL_GetError);
      sucess := False;
    end else begin
      gWindow := SDL_CreateWindow('SDL Tuorial', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, Screen_Widht, Screen_Height, SDL_WINDOW_SHOWN);
      if gWindow = nil then begin
        WriteLn('Window could not be created! SDL_Error: ', SDL_GetError);
        sucess := False;
      end else begin
        gscreenSurface := SDL_GetWindowSurface(gWindow);
      end;
    end;
    Result := sucess;
  end;

  function loadMedia: boolean;
  var
    sucess: boolean = True;
  begin
    gHelloWorld := SDL_LoadBMP('hello_world.bmp');
    if gHelloWorld = nil then begin
      WriteLn('Unable to load image hello_world.bmp SDL Error: ', SDL_GetError);
      sucess := False;
    end;
    Result := sucess;
  end;

  procedure Close;
  begin
    SDL_FreeSurface(gHelloWorld);
    gHelloWorld := nil;
    SDL_DestroyWindow(gWindow);
    gHelloWorld := nil;
    SDL_Quit();
  end;

begin
  if not init then begin
    WriteLn('Failed to initialize');
  end else begin
    if not loadMedia then begin
      WriteLn('Failed to load media');
    end else begin
      SDL_BlitSurface(gHelloWorld, nil, gscreenSurface, nil);
      SDL_UpdateWindowSurface(gWindow);
      while not quit do begin
        while SDL_PollEvent(@e) <> 0 do begin
          case e.type_ of
            SDL_KEYDOWN: begin
              case e.key.keysym.sym of
                SDLK_ESCAPE: begin
                  quit := True;
                end;
              end;
            end;
            SDL_QUITEV: begin
              quit := True;
            end;
          end;
        end;
      end;
    end;
  end;
  Close;
end.
