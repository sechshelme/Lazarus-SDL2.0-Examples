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
  JOYSTICK_DEAD_ZONE = 2000;

  function arctan2(y, x: single): single;
  begin
    if (x = 0) then begin
      if y = 0 then begin
        Result := 0.0;
      end else if y > 0 then begin
        Result := pi / 2;
      end else if y < 0 then begin
        Result := -pi / 2;
      end;
    end else begin
      Result := ArcTan(y / x);
    end;

    if x < 0.0 then begin
      Result := Result + pi;
    end;
    if Result > pi then begin
      Result := Result - 2 * pi;
    end;
  end;

var
  gWindow: PSDL_Window;
  gRenderer: PSDL_Renderer;

  quit: boolean = False;
  e: TSDL_Event;

  gArrowTexture: TLTexture;
  gGameController: PSDL_Joystick = nil;

  joystichAngele: single = 0.0;
  xDir, yDir: integer;

  function init: boolean;
  var
    sucess: boolean = True;
    imgFlags: cint32 = 0;
  begin
    sucess := True;
    if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_JOYSTICK) < 0 then begin
      WriteLn('SDL could not initialize! SDL_Error: ', SDL_GetError);
      sucess := False;
    end else begin
      if not SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, '1') then begin
        WriteLn('Warning: Linear texture filtering not enabled !');
      end;

      if SDL_NumJoysticks < 1 then begin
        WriteLn('Warning: No joysticks connected!');
      end else begin
        gGameController := SDL_JoystickOpen(0);
        if gGameController = nil then begin
          WriteLn('Warning: Unable to open game controller! SDL Error: ', SDL_GetError);
        end;
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

        end;
      end;
    end;
    Result := sucess;

    gArrowTexture := TLTexture.Create(gRenderer);
  end;

  function loadMedia: boolean;
  var
    sucess: boolean = True;
  begin
    if not gArrowTexture.LoadFromFile('arrow.png') then begin
      WriteLn('Failed to load fwalking animation texture !');
      sucess := False;
    end;

    Result := sucess;
  end;

  procedure Close;
  begin
    gArrowTexture.Free;

    SDL_JoystickClose(gGameController);
    gGameController := nil;

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
                SDLK_a: begin
                  joystichAngele -= 60;
                end;
                SDLK_d: begin
                  joystichAngele += 60;
                end;
              end;
            end;
            SDL_JOYBUTTONDOWN: begin
              WriteLn('button:', e.jbutton.button);
            end;
            SDL_JOYAXISMOTION: begin
              if e.jaxis.which = 0 then begin
                case e.jaxis.axis of
                  0: begin
                    if e.jaxis.Value < -JOYSTICK_DEAD_ZONE then begin
                      xDir := -1;
                      xDir := e.jaxis.Value;
                    end else if e.jaxis.Value > JOYSTICK_DEAD_ZONE then begin
                      xDir := 1;
                      xDir := e.jaxis.Value;
                    end else begin
                      xDir := 0;
                    end;
                  end;
                  1: begin
                    if e.jaxis.Value < -JOYSTICK_DEAD_ZONE then begin
                      yDir := -1;
                      yDir := e.jaxis.Value;
                    end else if e.jaxis.Value > JOYSTICK_DEAD_ZONE then begin
                      yDir := 1;
                      yDir := e.jaxis.Value;
                    end else begin
                      yDir := 0;
                    end;
                  end;
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

        if (xDir = 0) and (yDir = 0) then begin
          joystichAngele := 0.0;
        end else begin
          joystichAngele := ArcTan2(yDir, xDir) * (180 / Pi);
        end;

        gArrowTexture.Render((Screen_Widht - gArrowTexture.Widht) div 2, (Screen_Height - gArrowTexture.Height) div 2, nil, joystichAngele);

        SDL_RenderPresent(gRenderer);
      end;
    end;
  end;
  Close;
end.
