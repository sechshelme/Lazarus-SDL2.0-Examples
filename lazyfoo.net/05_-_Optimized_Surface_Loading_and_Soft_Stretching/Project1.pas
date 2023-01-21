program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

uses
  SDL2;

const
  Screen_Widht = 640;
  Screen_Height = 480;
var
  gWindow: PSDL_Window;
  gscreenSurface, gStretchedSurface: PSDL_Surface;
  quit: boolean = False;
  e: TSDL_Event;

  stretchRect:TSDL_Rect;

  function loadSurface(path: string): PSDL_Surface;
  var
    loadedSurface, optimizeSurface: PSDL_Surface;
  begin
    loadedSurface := SDL_LoadBMP(PChar(path));
    if loadedSurface = nil then  begin
      WriteLn('Unable to load image ' + path + '! SDL Error: ', SDL_GetError());
    end else begin
      optimizeSurface := SDL_ConvertSurface(loadedSurface, gscreenSurface^.format, 0);
      if optimizeSurface = nil then begin
        WriteLn('Unable to optimize image ', path, ' SDL Error: ', SDL_GetError);
      end;
      SDL_FreeSurface(loadedSurface);
    end;
    Result := optimizeSurface;
  end;

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
    gStretchedSurface := loadSurface('stretch.bmp');
    if gStretchedSurface = nil then begin
      WriteLn('Unable to load image hello_world.bmp SDL Error: ', SDL_GetError);
      sucess := False;
    end;
    Result := sucess;
  end;

  procedure Close;
  begin
    SDL_FreeSurface(gStretchedSurface);
    gStretchedSurface := nil;
    SDL_DestroyWindow(gWindow);
    gWindow := nil;
    SDL_Quit();
  end;

begin
  if not init then begin
    WriteLn('Failed to initialize');
  end else begin
    if not loadMedia then begin
      WriteLn('Failed to load media');
    end else begin
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

        stretchRect.x:=0;
        stretchRect.y:=0;
        stretchRect.w:=Screen_Widht;
        stretchRect.h:=Screen_Height;

        SDL_BlitSurfaceScaled(gStretchedSurface,nil,gscreenSurface,@stretchRect);
        SDL_UpdateWindowSurface(gWindow);
      end;
    end;
  end;
  Close;
end.
