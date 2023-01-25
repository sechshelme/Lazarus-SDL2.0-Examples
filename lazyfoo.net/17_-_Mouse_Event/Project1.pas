program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

uses
  sdl2,
  sdl2_image,
  ctypes,
  LTexture,
  LMouse;

const
  Screen_Widht = 640;
  Screen_Height = 480;

var
  gWindow: PSDL_Window;
  gRenderer: PSDL_Renderer;
  gButtonSpriteSheetTexture: TLTexture;

  quit: boolean = False;
  e: TSDL_Event;

  gButton: array[0..3] of TLButton;
  i: integer;

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
    i: integer;
  begin
    gButtonSpriteSheetTexture := TLTexture.Create(gRenderer);

    if not gButtonSpriteSheetTexture.LoadFromFile('button.png') then begin
      WriteLn('Failed to load button sprite texture! ');
    end else begin
      for i := 0 to Length(gButton) - 1 do begin
        gButton[i] := TLButton.Create(gButtonSpriteSheetTexture);
      end;
      gButton[0].SetPosition(0, 0);
      gButton[1].SetPosition(Screen_Widht - BUTTON_WIDTH, 0);
      gButton[2].SetPosition(0, Screen_Height - BUTTON_HEIGHT);
      gButton[3].SetPosition(Screen_Widht - BUTTON_WIDTH, Screen_Height - BUTTON_HEIGHT);
    end;

    Result := sucess;
  end;

  procedure Close;
  var
    i: integer;
  begin
    gButtonSpriteSheetTexture.Free;
    for i := 0 to Length(gButton) - 1 do begin
      gButton[i].Free;
    end;

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
          for i := 0 to Length(gButton) - 1 do begin
            gButton[i].HandleEvent(@e);
          end;
        end;

        SDL_SetRenderDrawColor(gRenderer, $FF, $FF, $FF, $FF);
        SDL_RenderClear(gRenderer);

        for i := 0 to Length(gButton) - 1 do begin
          gButton[i].Renderer;
        end;

        SDL_RenderPresent(gRenderer);
      end;
    end;
  end;
  Close;
end.
