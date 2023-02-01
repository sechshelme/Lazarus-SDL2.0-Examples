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

  textColor: TSDL_Color = (r: 0; g: 0; b: 0; a: $FF);

var
  gWindow: PSDL_Window;
  gRenderer: PSDL_Renderer;

  quit: boolean = False;
  e: TSDL_Event;

  gPromptTextTexture, gInputTextTexture: TLTexture;

  gFont: PTTF_Font;
  inputText: string;
  renderText: boolean;

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

    gPromptTextTexture := TLTexture.Create(gRenderer);
    gInputTextTexture := TLTexture.Create(gRenderer);
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
      if not gPromptTextTexture.LoadFromRenderedText(gFont, 'Enter Text:', textColor) then begin
        WriteLn('Failed to render prompt text !');
        sucess := False;
      end;
    end;

    Result := sucess;
  end;

  procedure Close;
  begin
    gPromptTextTexture.Free;
    gInputTextTexture.Free;

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
      inputText := 'Some Text';
      gInputTextTexture.LoadFromRenderedText(gFont,inputText,textColor);
      SDL_StartTextInput;
      while not quit do begin
        renderText := False;

        while SDL_PollEvent(@e) <> 0 do begin
          case e.type_ of
            SDL_KEYDOWN: begin
              case e.key.keysym.sym of
                SDLK_ESCAPE: begin
                  quit := True;
                end;
                SDLK_BACKSPACE: begin
                  if Length(inputText) > 0 then begin
                    Delete(inputText, Length(inputText), 1);
                    renderText := True;
                  end;
                end;
                SDLK_c: begin
                  if (SDL_GetModState and KMOD_CTRL) <> 0 then begin
                    SDL_SetClipboardText(pansichar(inputText));
                  end;
                end;
                SDLK_v: begin
                  if (SDL_GetModState and KMOD_CTRL) <> 0 then begin
                    inputText := SDL_GetClipboardText;
                    renderText := True;
                  end;
                end;
              end;
            end;
            SDL_TEXTINPUT: begin
              if (SDL_GetModState and KMOD_CTRL) = 0 then begin
                inputText += e.Text.Text;
                renderText := True;
              end;
            end;
            SDL_QUITEV: begin
              quit := True;
            end;
          end;
          if renderText then begin
            if inputText <> '' then begin
              gInputTextTexture.LoadFromRenderedText(gFont, inputText, textColor);
            end else begin
              gInputTextTexture.LoadFromRenderedText(gFont, ' ', textColor);
            end;
          end;
        end;

        SDL_SetRenderDrawColor(gRenderer, $FF, $FF, $FF, $FF);
        SDL_RenderClear(gRenderer);

        gPromptTextTexture.Render((Screen_Widht - gPromptTextTexture.Widht) div 2, 0);
        gInputTextTexture.Render((Screen_Widht - gInputTextTexture.Widht) div 2, (Screen_Height - gPromptTextTexture.Height));

        SDL_RenderPresent(gRenderer);
      end;
      SDL_StopTextInput;
    end;
  end;
  Close;
end.
