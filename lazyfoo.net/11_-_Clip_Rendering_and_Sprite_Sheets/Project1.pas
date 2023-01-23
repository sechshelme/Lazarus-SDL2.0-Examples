program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

uses
  sdl2,
  sdl2_image,
  ctypes,
  LTexture;

const
  Screen_Widht = 640;
  Screen_Height = 480;

var
  gWindow: PSDL_Window;
  gRenderer: PSDL_Renderer;

  gSprites: array[0..3] of TSDL_Rect = ((x: 0; y: 0; w: 100; h: 100), (x: 100; y: 0; w: 100; h: 100), (x: 0; y: 100; w: 100; h: 100), (x: 100; y: 100; w: 100; h: 100));

  quit: boolean = False;
  e: TSDL_Event;

  gSpriteSheetTexture: TLTexture;

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
    gSpriteSheetTexture := TLTexture.Create(gRenderer);

    Result := sucess;
  end;

  function loadMedia: boolean;
  var
    sucess: boolean = True;
  begin
    if not gSpriteSheetTexture.LoadFromFile('dots.png') then begin
      WriteLn('Failed to load background texture image !');
      sucess := False;
    end;

    Result := sucess;
  end;

  procedure Close;
  begin
    gSpriteSheetTexture.Free;

    SDL_DestroyRenderer(gRenderer);
    SDL_DestroyWindow(gWindow);
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
        end;

        SDL_SetRenderDrawColor(gRenderer, $FF, $FF, $FF, $FF);
        SDL_RenderClear(gRenderer);

        gSpriteSheetTexture.render(0, 0, @gSprites[0]);
        gSpriteSheetTexture.render(Screen_Widht - gSprites[1].w, 0, @gSprites[1]);
        gSpriteSheetTexture.render(0, Screen_Height - gSprites[2].h, @gSprites[2]);
        gSpriteSheetTexture.render(Screen_Widht - gSprites[3].w, Screen_Height - gSprites[3].h, @gSprites[3]);

        SDL_RenderPresent(gRenderer);
      end;
    end;
  end;
  Close;
end.
