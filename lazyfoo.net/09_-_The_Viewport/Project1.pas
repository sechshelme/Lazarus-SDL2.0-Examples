program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

uses
  sdl2,
  sdl2_image,
  ctypes;

const
  Screen_Widht = 640;
  Screen_Height = 480;
var
  gWindow: PSDL_Window;
  gscreenSurface: PSDL_Surface;
  gTexture: PSDL_Texture;
  gRenderer: PSDL_Renderer;

  quit: boolean = False;
  e: TSDL_Event;

  stretchRect: TSDL_Rect;

  function loadtexture(path: string): PSDL_Texture;
  var
    loadedSurface: PSDL_Surface;
    newTexture: PSDL_Texture;
  begin
    //loadedSurface := IMG_Load(PChar(path));
    //if loadedSurface = nil then  begin
    //  WriteLn('Unable to load image ' + path + '! SDL_image Error: ', IMG_GetError());
    //end else begin
    //  newTexture := SDL_CreateTextureFromSurface(gRenderer, loadedSurface);
    //  if newTexture = nil then begin
    //    WriteLn('Unable to create texturefrom ', path, ' SDL Error: ', SDL_GetError);
    //  end;
    //  SDL_FreeSurface(loadedSurface);
    //end;

    newTexture := IMG_LoadTexture(gRenderer, PChar(path));
    if newTexture = nil then begin
      WriteLn('Unable to create texturefrom ', path, ' SDL Error: ', SDL_GetError);
    end;

    Result := newTexture;
  end;

  function init: boolean;
  var
    sucess: boolean = True;
    imgFlags: cint32 = 0;
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
        gRenderer := SDL_CreateRenderer(gWindow, -1, SDL_RENDERER_ACCELERATED);
        if gRenderer = nil then begin
          WriteLn('Renderer could not be created! SDL Error: ', SDL_GetError);
          sucess := False;
        end else begin
          SDL_SetRenderDrawColor(gRenderer, $FF, $FF, $FF, $FF);

          if (IMG_Init(imgFlags) and imgFlags) <> 0 then begin
            WriteLn('SDL_image could not initialize! SDL_image Error: ', IMG_GetError);
            sucess := False;
          end;

        end;
      end;
    end;
    Result := sucess;
  end;

  function loadMedia: boolean;
  var
    sucess: boolean = True;
  begin
    gTexture := loadtexture('viewport.png');
    if gTexture = nil then begin
      WriteLn('Failed to load texture image !');
      sucess := False;
    end;
    Result := sucess;
  end;

  procedure Close;
  begin
    SDL_DestroyTexture(gTexture);
    gTexture := nil;

    SDL_DestroyRenderer(gRenderer);
    SDL_DestroyWindow(gWindow);
    gWindow := nil;
    gRenderer := nil;

    IMG_Quit;
    SDL_Quit;
  end;

const
  topLeftViewport: TSDL_Rect = (x: 0; y: 0; w: Screen_Widht div 2; h: Screen_Height div 2);
  topRightViewport: TSDL_Rect = (x: Screen_Widht div 2; y: 0; w: Screen_Widht div 2; h: Screen_Height div 2);
  bottomViewport: TSDL_Rect = (x: 0; y: Screen_Height div 2; w: Screen_Widht; h: Screen_Height div 2);

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

        SDL_SetRenderDrawColor(gRenderer, $FF, $FF, $FF, $FF);
        SDL_RenderClear(gRenderer);

        SDL_SetRenderDrawColor(gRenderer, $FF, $00, $00, $FF);
        SDL_RenderSetViewport(gRenderer, @topLeftViewport);
        SDL_RenderCopy(gRenderer, gTexture, nil, nil);
        SDL_RenderDrawRect(gRenderer, nil);

        SDL_SetRenderDrawColor(gRenderer, $00, $FF, $00, $FF);
        SDL_RenderSetViewport(gRenderer, @topRightViewport);
        SDL_RenderCopy(gRenderer, gTexture, nil, nil);
        SDL_RenderDrawRect(gRenderer, nil);

        SDL_SetRenderDrawColor(gRenderer, $00, $00, $FF, $FF);
        SDL_RenderSetViewport(gRenderer, @bottomViewport);
        SDL_RenderCopy(gRenderer, gTexture, nil, nil);
        SDL_RenderDrawRect(gRenderer, nil);

        SDL_RenderPresent(gRenderer);
      end;
    end;
  end;
  Close;
end.
