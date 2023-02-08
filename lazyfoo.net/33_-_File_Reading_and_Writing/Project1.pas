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

  Total_Date = 10;

  textColor: TSDL_Color = (r: $80; g: $80; b: $80; a: $FF);
  highlightColor: TSDL_Color = (r: $FF; g: $FF; b: 0; a: $FF);

var
  gWindow: PSDL_Window;
  gRenderer: PSDL_Renderer;

  quit: boolean = False;
  e: TSDL_Event;

  gPromptTextTexture: TLTexture;
  gDataTextures: array [0..Total_Date - 1] of TLTexture;
  gData: array [0..Total_Date - 1] of integer;

  gFont: PTTF_Font;
  inputText: string;
  currentData: integer = 0;
  s: string;
  i: integer;

  function init: boolean;
  var
    sucess: boolean = True;
    imgFlags: cint32 = 0;
    i: integer;
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
    for i := 0 to Length(gDataTextures) - 1 do begin
      gDataTextures[i] := TLTexture.Create(gRenderer);
    end;
  end;

  function loadMedia: boolean;
  var
    sucess: boolean = True;
    datei: PSDL_RWops;
    i: integer;
    s: string;
  begin
    gFont := TTF_OpenFont('lazy.ttf', 28);
    if gFont = nil then begin
      WriteLn('Failed to load lazy font! SDL_ttf Error: ', TTF_GetError);
      sucess := False;
    end else begin
      if not gPromptTextTexture.LoadFromRenderedText(gFont, 'Enter Data:', textColor) then begin
        WriteLn('Failed to render prompt text !');
        sucess := False;
      end;
    end;

    datei := SDL_RWFromFile('nums.bin', 'r+b');
    if datei = nil then begin
      WriteLn('Warning: Unable to open file! SDL Error: ', SDL_GetError);

      datei := SDL_RWFromFile('nums.bin', 'w+b');
      if datei <> nil then begin
        WriteLn('New file created!');
        for i := 0 to Length(gData) - 1 do begin
          gData[i] := i;
          SDL_RWwrite(datei, @gData[i], SizeOf(integer), 1);
        end;
        SDL_RWclose(datei);
      end else begin
        WriteLn('Error: Unable to create file! SDL Error: ', SDL_GetError);
        sucess := False;
      end;
    end else begin
      WriteLn('Reading file...!');
      for i := 0 to Length(gData) - 1 do begin
        gData[i] := 0;
        SDL_RWread(datei, @gData[i], SizeOf(integer), 1);
      end;
      SDL_RWclose(datei);
    end;

    str(gData[0], s);
    gDataTextures[0].LoadFromRenderedText(gFont, s, highlightColor);
    for i := 1 to Length(gData) - 1 do begin
      str(gData[i], s);
      gDataTextures[i].LoadFromRenderedText(gFont, s, textColor);
    end;

    Result := sucess;
  end;

  procedure Close;
  var
    i: integer;
    datei: PSDL_RWops;
  begin
    datei := SDL_RWFromFile('nums.bin', 'w+b');
    if datei <> nil then  begin
      for i := 0 to Length(gData) - 1 do begin
        SDL_RWwrite(datei, @gData[i], SizeOf(integer), 1);
      end;
      SDL_RWclose(datei);
    end else begin
      WriteLn('Error: Unable to save file! ', SDL_GetError);
    end;

    gPromptTextTexture.Free;
    for i := 0 to Length(gDataTextures) - 1 do begin
      gDataTextures[i].Free;
    end;

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
      while not quit do begin
        while SDL_PollEvent(@e) <> 0 do begin
          case e.type_ of
            SDL_KEYDOWN: begin
              case e.key.keysym.sym of
                SDLK_UP: begin
                  str(gData[currentData], s);
                  gDataTextures[currentData].LoadFromRenderedText(gFont, s, textColor);
                  Dec(currentData);
                  if currentData < 0 then begin
                    currentData := Total_Date - 1;
                  end;
                  str(gData[currentData], s);
                  gDataTextures[currentData].LoadFromRenderedText(gFont, s, highlightColor);
                end;
                SDLK_DOWN: begin
                  str(gData[currentData], s);
                  gDataTextures[currentData].LoadFromRenderedText(gFont, s, textColor);
                  Inc(currentData);
                  if currentData >= Total_Date then begin
                    currentData := 0;
                  end;
                  str(gData[currentData], s);
                  gDataTextures[currentData].LoadFromRenderedText(gFont, s, highlightColor);
                end;
                SDLK_LEFT: begin
                  Dec(gData[currentData]);
                  str(gData[currentData], s);
                  gDataTextures[currentData].LoadFromRenderedText(gFont, s, highlightColor);
                end;
                SDLK_RIGHT: begin
                  Inc(gData[currentData]);
                  str(gData[currentData], s);
                  gDataTextures[currentData].LoadFromRenderedText(gFont, s, highlightColor);
                end;
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

        SDL_SetRenderDrawColor(gRenderer, $40, $00, $00, $FF);
        SDL_RenderClear(gRenderer);

        gPromptTextTexture.Render((Screen_Widht - gPromptTextTexture.Widht) div 2, 0);

        for i := 0 to Length(gDataTextures) - 1 do begin
          gDataTextures[i].Render((Screen_Widht - gDataTextures[i].Widht) div 2, gPromptTextTexture.Height * 2 + gDataTextures[0].Height * i);
        end;

        SDL_RenderPresent(gRenderer);
      end;
    end;
  end;
  Close;
end.
