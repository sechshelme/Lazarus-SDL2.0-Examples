program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

uses
  sdl2,
  sdl2_image,
  sdl2_ttf,
  ctypes,
  LTexture;

const
  Screen_Widht = 640;
  Screen_Height = 480;

var
  gWindow: PSDL_Window;
  gRenderer: PSDL_Renderer;

  quit: boolean = False;
  e: TSDL_Event;

  gTimeTextTexture, gPromptTexture: TLTexture;
  textColor: TSDL_Color = (r: 0; g: 0; b: 0; a: 255);

  gFont: PTTF_Font;
  startTime: cuint32;
  timeText: string;

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

          if TTF_Init() = -1 then begin
            WriteLn('SDL_ttf could not initialize! SDL_ttf Error: ', TTF_GetError);
            sucess := False;
          end;

        end;
      end;
    end;
    Result := sucess;

    gTimeTextTexture := TLTexture.Create(gRenderer);
    gPromptTexture := TLTexture.Create(gRenderer);
  end;

  function loadMedia: boolean;
  var
    sucess: boolean = True;
  begin
    gFont := TTF_OpenFont('lazy.ttf', 28);
    if gFont = nil then begin
      WriteLn('Failed to load lazy font! SDL_ttf Error: ', TTF_GetError);
      sucess := False;
    end else begin
      if not gPromptTexture.LoadFromRenderedText(gFont, 'The quick brown fox jumps over the lazy dog', textColor) then begin
        WriteLn('Failed to render text texture !');
        sucess := False;
      end;
    end;

    Result := sucess;
  end;

  procedure Close;
  begin
    gTimeTextTexture.Free;
    gPromptTexture.Free;

    TTF_CloseFont(gFont);
    gFont := nil;


    SDL_DestroyRenderer(gRenderer);
    SDL_DestroyWindow(gWindow);
    gWindow := nil;
    gRenderer := nil;

    TTF_Quit;
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
                SDLK_RETURN: begin
                  startTime := SDL_GetTicks;
                end;
              end;
            end;
            SDL_QUITEV: begin
              quit := True;
            end;
          end;
        end;

        timeText := '';
        WriteStr(timeText, 'Milliseconds since start time ', SDL_GetTicks - startTime);
        if not gTimeTextTexture.LoadFromRenderedText(gFont, timeText, textColor) then begin
          WriteLn('Unable to render time texture !');
        end;

        SDL_SetRenderDrawColor(gRenderer, $FF, $FF, $FF, $FF);
        SDL_RenderClear(gRenderer);

        gPromptTexture.Render((Screen_Widht - gPromptTexture.Widht) div 2, 0);
        gTimeTextTexture.Render((Screen_Widht - gTimeTextTexture.Widht) div 2, (Screen_Height - gTimeTextTexture.Height) div 2);

        SDL_RenderPresent(gRenderer);
      end;
    end;
  end;
  Close;
end.
