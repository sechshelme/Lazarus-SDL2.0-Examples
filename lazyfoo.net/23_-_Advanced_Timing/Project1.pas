program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

uses
  sdl2,
  sdl2_image,
  sdl2_ttf,
  ctypes,
  LTexture,
  LTimer;

const
  Screen_Widht = 640;
  Screen_Height = 480;

var
  gWindow: PSDL_Window;
  gRenderer: PSDL_Renderer;

  quit: boolean = False;
  e: TSDL_Event;

  gPausePromptTexture, gTimeTextTexture, gStartPromptTexture: TLTexture;
  timer: TLTimer;
  textColor: TSDL_Color = (r: 0; g: 0; b: 0; a: 255);

  gFont: PTTF_Font;
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

    gPausePromptTexture := TLTexture.Create(gRenderer);
    gTimeTextTexture := TLTexture.Create(gRenderer);
    gStartPromptTexture := TLTexture.Create(gRenderer);
    timer := TLTimer.Create;
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
      if not gStartPromptTexture.LoadFromRenderedText(gFont, 'Press S to Start or Stop the Timer', textColor) then begin
        WriteLn('Unable to render start/stop prompt texture !');
        sucess := False;
      end;
      if not gPausePromptTexture.LoadFromRenderedText(gFont, 'Press P to Pause or Unpause the Timer', textColor) then begin
        WriteLn('Unable to render pause/unpause prompt texture !');
        sucess := False;
      end;
    end;

    Result := sucess;
  end;

  procedure Close;
  begin
    gTimeTextTexture.Free;
    gPausePromptTexture.Free;
    gStartPromptTexture.Free;
    timer.Free;

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
                SDLK_s: begin
                  if timer.isStarted then begin
                    timer.stop;
                  end else begin
                    timer.start;
                  end;
                end;
                SDLK_p: begin
                  if timer.isPausedd then begin
                    timer.unpause;
                  end else begin
                    timer.pause;
                  end;
                end;
              end;
            end;
            SDL_QUITEV: begin
              quit := True;
            end;
          end;
        end;

        timeText := '';
        WriteStr(timeText, 'Seconds since start time ', timer.getTicks div 1000);
        if not gTimeTextTexture.LoadFromRenderedText(gFont, timeText, textColor) then begin
          WriteLn('Unable to render time texture !');
        end;

        SDL_SetRenderDrawColor(gRenderer, $FF, $FF, $FF, $FF);
        SDL_RenderClear(gRenderer);

        gStartPromptTexture.Render((Screen_Widht - gStartPromptTexture.Widht) div 2, 0);
        gPausePromptTexture.Render((Screen_Widht - gPausePromptTexture.Widht) div 2, gStartPromptTexture.Height);
        gTimeTextTexture.Render((Screen_Widht - gTimeTextTexture.Widht) div 2, (Screen_Height - gTimeTextTexture.Height) div 2);

        SDL_RenderPresent(gRenderer);
      end;
    end;
  end;
  Close;
end.
