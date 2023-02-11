program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

uses
  sdl2,
  sdl2_image,
  ctypes,
  LTexture,
  LTimer,
  LWindow;

const
  Screen_Widht = 640;
  Screen_Height = 480;
  Screen_FPS = 60;
  Screen_Tick_Per_Frame = 1000 div Screen_FPS;

var
  gWindow: TLWindow;
  gRenderer: PSDL_Renderer;

  quit: boolean = False;
  e: TSDL_Event;

  gSceneTexture: TLTexture;

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
      if not SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, '1') then begin
        WriteLn('Warning: Linear texture filtering not enabled!');
      end;

      gWindow := TLWindow.Create(Screen_Widht,Screen_Height);
        gRenderer := gWindow.Renderer;
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
    Result := sucess;

    gSceneTexture := TLTexture.Create(gRenderer);
  end;

  function loadMedia: boolean;
  var
    sucess: boolean = True;
  begin
    gSceneTexture.LoadFromFile('window.png');
    if gSceneTexture = nil then begin
      WriteLn('Failed to load dot texture! SDL_ttf Error: ');
      sucess := False;
    end;

    Result := sucess;
  end;

  procedure Close;
  begin
    gSceneTexture.Free;

    SDL_DestroyRenderer(gRenderer);
    gWindow.Free;

    gWindow := nil;
    gRenderer := nil;

    IMG_Quit;
    SDL_Quit;
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
          gWindow.handleEvent(e);
        end;

        SDL_SetRenderDrawColor(gRenderer, $00, $9F, $00, $FF);
        SDL_RenderClear(gRenderer);

        gSceneTexture.Render((gWindow.Width - gSceneTexture.Widht) div 2, (gWindow.Height - gSceneTexture.Height) div 2);

        SDL_RenderPresent(gRenderer);
      end;
    end;
  end;
  Close;
end.
