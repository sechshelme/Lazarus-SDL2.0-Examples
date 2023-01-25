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

  gArrowTexture: TLTexture;

  gGameController: PSDL_GameController = nil;
  gJoystick: PSDL_Joystick = nil;
  gJoyHaptic: PSDL_Haptic = nil;

  function init: boolean;
  var
    sucess: boolean = True;
    imgFlags: cint32 = 0;
  begin
    sucess := True;
    if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_JOYSTICK or SDL_INIT_HAPTIC or SDL_INIT_GAMECONTROLLER) < 0 then begin
      WriteLn('SDL could not initialize! SDL_Error: ', SDL_GetError);
      sucess := False;
    end else begin
      if not SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, '1') then begin
        WriteLn('Warning: Linear texture filtering not enabled !');
      end;

      if SDL_NumJoysticks < 1 then begin
        WriteLn('Warning: No joysticks connected!');
      end else begin
        if not SDL_IsGameController(0) then begin
          WriteLn('Warning: Joystick is not game controller interface compatible! SDL Error: ', SDL_GetError);
        end else begin
          gGameController := SDL_GameControllerOpen(0);
          if not SDL_GameControllerHasRumble(gGameController) then begin
            WriteLn('Warning: Game controller does not have rumble! SDL Error: ', SDL_GetError);
          end;
        end;

        if gGameController = nil then begin
          gJoystick := SDL_JoystickOpen(0);
          if gJoystick = nil then begin
            WriteLn('Warning: Unable to open joystick controller! SDL Error: ', SDL_GetError);
          end else begin
            if SDL_JoystickIsHaptic(gJoystick) = 0 then begin
              WriteLn('Warning: Controller does not support haptics! SDL Error: ', SDL_GetError);
            end else begin
              gJoyHaptic := SDL_HapticOpenFromJoystick(gJoystick);
              if gJoyHaptic = nil then begin
                WriteLn('Warning: Unable to get joystick haptics! SDL Error: ', SDL_GetError);
              end else begin
                if SDL_HapticRumbleInit(gJoyHaptic) < 0 then begin
                  WriteLn('Warning: Unable to initialize haptic rumble! SDL Error: ', SDL_GetError);
                end;
              end;
            end;
          end;
        end;
      end;

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

    gArrowTexture := TLTexture.Create(gRenderer);
  end;

  function loadMedia: boolean;
  var
    sucess: boolean = True;
  begin
    if not gArrowTexture.LoadFromFile('splash.png') then begin
      WriteLn('Failed to load fwalking animation texture !');
      sucess := False;
    end;

    Result := sucess;
  end;

  procedure Close;
  begin
    gArrowTexture.Free;

    if gGameController <> nil then  begin
      SDL_GameControllerClose(gGameController);
      gGameController := nil;
    end;

    if gJoystick <> nil then  begin
      SDL_JoystickClose(gJoystick);
      gJoystick := nil;
    end;

    if gJoyHaptic <> nil then  begin
      SDL_HapticClose(gJoyHaptic);
      gJoyHaptic := nil;
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
              WriteLn('key');
              case e.key.keysym.sym of
                SDLK_ESCAPE: begin
                  quit := True;
                end;
              end;
            end;
            SDL_JOYBUTTONDOWN: begin
                WriteLn('button:', e.jbutton.button);
              if gGameController <> nil then begin
                if SDL_GameControllerRumble(gGameController, $FFFF * 3 div 4, $FFFF * 3 div 4, 500) <> 0 then begin
                  WriteLn('Warning: Unable to play game contoller rumble! %s\n", SDL_GetError');
                end;
              end else if gJoyHaptic <> nil then begin
                if SDL_HapticRumblePlay(gJoyHaptic, 0.75, 500) <> 0 then begin
                  WriteLn('Warning: Unable to play haptic rumble! ', SDL_GetError);
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

        gArrowTexture.Render(0,0);

        SDL_RenderPresent(gRenderer);
      end;
    end;
  end;
  Close;
end.
