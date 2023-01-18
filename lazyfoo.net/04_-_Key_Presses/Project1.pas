program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

uses
  SDL2;

const
  Screen_Widht = 640;
  Screen_Height = 480;

const
  KEY_PRESS_SURFACE_DEFAULT = 0;
  KEY_PRESS_SURFACE_UP = 1;
  KEY_PRESS_SURFACE_DOWN = 2;
  KEY_PRESS_SURFACE_LEFT = 3;
  KEY_PRESS_SURFACE_RIGHT = 4;
  KEY_PRESS_SURFACE_TOTAL = 5;
var
  gWindow: PSDL_Window;
  gscreenSurface: PSDL_Surface;
  gCurrentSurface: PSDL_Surface;
  quit: boolean = False;
  e: TSDL_Event;
  //  KeyPressesSurface: set of (KEY_PRESS_SURFACE_DEFAULT, KEY_PRESS_SURFACE_UP, KEY_PRESS_SURFACE_DOWN, KEY_PRESS_SURFACE_LEFT, KEY_PRESS_SURFACE_RIGHT, KEY_PRESS_SURFACE_TOTAL);

  gKeyPressSurfaces: array [0..KEY_PRESS_SURFACE_TOTAL - 1] of PSDL_Surface;

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

function loadSurface(path: string): PSDL_Surface;
var
  loadedSurface: PSDL_Surface;
begin
  loadedSurface := SDL_LoadBMP(PChar(path));
  if loadedSurface = nil then  begin
    WriteLn('Unable to load image ' + path + '! SDL Error: ', SDL_GetError());
  end;
  Result := loadedSurface;
end;

  function loadMedia: boolean;
  var
    sucess: boolean = True;
  begin
    gKeyPressSurfaces[KEY_PRESS_SURFACE_DEFAULT] := loadSurface('press.bmp');
    if gKeyPressSurfaces[KEY_PRESS_SURFACE_DEFAULT] = nil then begin
      WriteLn('Failed to load default image !');
      sucess := False;
    end;

    gKeyPressSurfaces[KEY_PRESS_SURFACE_UP] := loadSurface('up.bmp');
    if gKeyPressSurfaces[KEY_PRESS_SURFACE_UP] = nil then begin
      WriteLn('Failed to load up image !');
      sucess := False;
    end;

    gKeyPressSurfaces[KEY_PRESS_SURFACE_DOWN] := loadSurface('down.bmp');
    if gKeyPressSurfaces[KEY_PRESS_SURFACE_DOWN] = nil then begin
      WriteLn('Failed to load down image !');
      sucess := False;
    end;

    gKeyPressSurfaces[KEY_PRESS_SURFACE_LEFT] := loadSurface('left.bmp');
    if gKeyPressSurfaces[KEY_PRESS_SURFACE_LEFT] = nil then begin
      WriteLn('Failed to load left image !');
      sucess := False;
    end;

    gKeyPressSurfaces[KEY_PRESS_SURFACE_RIGHT] := loadSurface('right.bmp');
    if gKeyPressSurfaces[KEY_PRESS_SURFACE_RIGHT] = nil then begin
      WriteLn('Failed to load right image !');
      sucess := False;
    end;

    Result := sucess;
  end;

  procedure Close;
  var
    i: integer;
  begin
    for i := 1 to KEY_PRESS_SURFACE_TOTAL - 1 do begin
      SDL_FreeSurface(gKeyPressSurfaces[i]);
      gKeyPressSurfaces[i] := nil;
    end;
    SDL_DestroyWindow(gWindow);
    gWindow:=nil;
    SDL_Quit();
  end;

begin
  if not init then begin
    WriteLn('Failed to initialize');
  end else begin
    if not loadMedia then begin
      WriteLn('Failed to load media');
    end else begin
      gCurrentSurface := gKeyPressSurfaces[KEY_PRESS_SURFACE_DEFAULT];
      while not quit do begin
        while SDL_PollEvent(@e) <> 0 do begin
          case e.type_ of
            SDL_KEYDOWN: begin
              case e.key.keysym.sym of
                SDLK_ESCAPE: begin
                  quit := True;
                end;
                SDLK_UP: begin
                  gCurrentSurface := gKeyPressSurfaces[KEY_PRESS_SURFACE_UP];
                end;
                SDLK_DOWN: begin
                  gCurrentSurface := gKeyPressSurfaces[KEY_PRESS_SURFACE_DOWN];
                end;
                SDLK_LEFT: begin
                  gCurrentSurface := gKeyPressSurfaces[KEY_PRESS_SURFACE_LEFT];
                end;
                SDLK_RIGHT: begin
                  gCurrentSurface := gKeyPressSurfaces[KEY_PRESS_SURFACE_RIGHT];
                end;
              end;
            end;
            SDL_QUITEV: begin
              quit := True;
            end;
          end;
        end;
        SDL_BlitSurface(gCurrentSurface, nil, gscreenSurface, nil);
        SDL_UpdateWindowSurface(gWindow);
      end;
    end;
  end;
  Close;
end.
