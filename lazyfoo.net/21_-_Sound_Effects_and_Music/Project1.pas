program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

uses
  sdl2,
  sdl2_image,
  sdl2_mixer,
  ctypes,
  LTexture;

const
  Screen_Widht = 640;
  Screen_Height = 480;

var
  gWindow: PSDL_Window;
  gRenderer: PSDL_Renderer;

  gMusik: PMix_Music;
  gScratch, gHigh, GMedium, gLow: PMix_Chunk;

  quit: boolean = False;
  e: TSDL_Event;

  gPromptTexture: TLTexture;

  function init: boolean;
  var
    sucess: boolean = True;
    imgFlags: cint32 = 0;
  begin
    sucess := True;
    if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO) < 0 then begin
      WriteLn('SDL could not initialize! SDL_Error: ', SDL_GetError);
      sucess := False;
    end else begin
      if not SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, '1') then begin
        WriteLn('Warning: Linear texture filtering not enabled !');
      end;

      gWindow := SDL_CreateWindow('SDL Tuorial', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, Screen_Widht, Screen_Height, SDL_WINDOW_SHOWN);
      if gWindow = nil then begin
        WriteLn('Window could not be created! SDL_Error: ', SDL_GetError);
        sucess := False;
      end else begin
        gRenderer := SDL_CreateRenderer(gWindow, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);
        if gRenderer = nil then begin
          WriteLn('Renderer could not be created! SDL Error: ', SDL_GetError);
          sucess := False;
        end else begin
          SDL_SetRenderDrawColor(gRenderer, $FF, $FF, $FF, $FF);

          if (IMG_Init(imgFlags) and imgFlags) <> 0 then begin
            WriteLn('SDL_image could not initialize! SDL_image Error: ', IMG_GetError);
            sucess := False;
          end;

          if Mix_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 2048) < 0 then begin
            WriteLn('SDL_mixer could not initialize! SDL_mixer Error: ', Mix_GetError);
            sucess := False;
          end;
        end;
      end;
    end;
    Result := sucess;

    gPromptTexture := TLTexture.Create(gRenderer);
  end;

  function loadMedia: boolean;
  var
    sucess: boolean = True;
  begin
    if not gPromptTexture.LoadFromFile('prompt.png') then begin
      WriteLn('Failed to load prompt texture !');
      sucess := False;
    end;

    //    gMusik:=Mix_LoadMUS('beat.wav');
    gMusik := Mix_LoadMUS('doom.mid');
    if gMusik = nil then begin
      WriteLn('Failed to load beat music! SDL_mixer Error: ', Mix_GetError);
      sucess := False;
    end;

    gScratch := Mix_LoadWAV('scratch.wav');
    if gScratch = nil then begin
      WriteLn('Failed to load scratch sound effect! SDL_mixer Error: ', Mix_GetError);
      sucess := False;
    end;

    gHigh := Mix_LoadWAV('high.wav');
    if gHigh = nil then begin
      WriteLn('Failed to load high sound effect! SDL_mixer Error: ', Mix_GetError);
      sucess := False;
    end;

    GMedium := Mix_LoadWAV('medium.wav');
    if GMedium = nil then begin
      WriteLn('Failed to load medium sound effect! SDL_mixer Error: ', Mix_GetError);
      sucess := False;
    end;

    gLow := Mix_LoadWAV('low.wav');
    if gLow = nil then begin
      WriteLn('Failed to load low sound effect! SDL_mixer Error: ', Mix_GetError);
      sucess := False;
    end;

    Result := sucess;
  end;

  procedure Close;
  begin
    gPromptTexture.Free;

    SDL_DestroyRenderer(gRenderer);
    SDL_DestroyWindow(gWindow);
    gWindow := nil;
    gRenderer := nil;

    Mix_FreeChunk(gScratch);
    Mix_FreeChunk(gHigh);
    Mix_FreeChunk(GMedium);
    Mix_FreeChunk(gLow);

    Mix_FreeMusic(gMusik);
    gMusik := nil;
    Mix_Quit;
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
                SDLK_1: begin
                  Mix_PlayChannel(-1, gHigh, 0);
                end;
                SDLK_2: begin
                  Mix_PlayChannel(-1, GMedium, 0);
                end;
                SDLK_3: begin
                  Mix_PlayChannel(-1, gLow, 0);
                end;
                SDLK_4: begin
                  Mix_PlayChannel(-1, gScratch, 0);
                end;
                SDLK_9: begin
                  if Mix_PlayingMusic = 0 then begin
                    Mix_PlayMusic(gMusik, -1);
                  end else begin
                    if Mix_PausedMusic = 1 then begin
                      Mix_ResumeMusic;
                    end else begin
                      Mix_PausedMusic;
                    end;
                  end;
                end;
                SDLK_0: begin
                  Mix_HaltMusic;
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

        gPromptTexture.Render(0,0);

        SDL_RenderPresent(gRenderer);
      end;
    end;
  end;
  Close;
end.
