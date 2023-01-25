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

  quit: boolean = False;
  e: TSDL_Event;

  gPressTexture, gUpTexture, gDownTexture, gLeftTexture, gRightTexture, currentTexture: TLTexture;

  currentKeyStates: pcuint8;

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

    gPressTexture := TLTexture.Create(gRenderer);
    gLeftTexture := TLTexture.Create(gRenderer);
    gRightTexture := TLTexture.Create(gRenderer);
    gUpTexture := TLTexture.Create(gRenderer);
    gDownTexture := TLTexture.Create(gRenderer);
  end;

  function loadMedia: boolean;
  var
    sucess: boolean = True;
  begin
    if not gPressTexture.LoadFromFile('press.png') then begin
      WriteLn('Failed to load fwalking press texture !');
      sucess := False;
    end;
    if not gLeftTexture.LoadFromFile('left.png') then begin
      WriteLn('Failed to load fwalking left texture !');
      sucess := False;
    end;
    if not gRightTexture.LoadFromFile('right.png') then begin
      WriteLn('Failed to load fwalking right texture !');
      sucess := False;
    end;
    if not gUpTexture.LoadFromFile('up.png') then begin
      WriteLn('Failed to load fwalking up texture !');
      sucess := False;
    end;
    if not gDownTexture.LoadFromFile('down.png') then begin
      WriteLn('Failed to load fwalking down texture !');
      sucess := False;
    end;

    Result := sucess;
  end;

  procedure Close;
  begin
    gPressTexture.Free;
    gLeftTexture.Free;
    gRightTexture.Free;
    gUpTexture.Free;
    gDownTexture.Free;

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

        currentKeyStates := SDL_GetKeyboardState(nil);
        if currentKeyStates[SDL_SCANCODE_UP] <> 0 then begin
          currentTexture := gUpTexture;
        end else if currentKeyStates[SDL_SCANCODE_DOWN] <> 0 then begin
          currentTexture := gDownTexture;
        end else if currentKeyStates[SDL_SCANCODE_LEFT] <> 0 then begin
          currentTexture := gLeftTexture;
        end else if currentKeyStates[SDL_SCANCODE_RIGHT] <> 0 then begin
          currentTexture := gRightTexture;
        end else begin
          currentTexture := gPressTexture;
        end;

        SDL_SetRenderDrawColor(gRenderer, $FF, $FF, $FF, $FF);
        SDL_RenderClear(gRenderer);

        currentTexture.Render(0, 0);

        SDL_RenderPresent(gRenderer);
      end;
    end;
  end;
  Close;
end.
