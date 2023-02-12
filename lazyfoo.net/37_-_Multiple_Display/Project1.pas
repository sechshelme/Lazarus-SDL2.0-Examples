program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

uses
  sdl2,
  sdl2_image,
  ctypes,
  LTexture,
  LTimer,
  LWindow;

const
  Screen_Widht = 640;
  Screen_Height = 480;

  Total_Windows = 3;

  Screen_FPS = 60;

var
  gWindow: array[0..Total_Windows - 1] of TLWindow;

  quit: boolean = False;
  e: TSDL_Event;
  i: integer;
  allWindowsClosed: boolean;
  r: TSDL_Rect;

  function init: boolean;
  var
    sucess: boolean = True;
    i: integer;
  begin
    sucess := True;
    if SDL_Init(SDL_INIT_VIDEO) < 0 then begin
      WriteLn('SDL could not initialize! SDL_Error: ', SDL_GetError);
      sucess := False;
    end else begin
      if not SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, '1') then begin
        WriteLn('Warning: Linear texture filtering not enabled!');
      end;

      gTotalDisplays := SDL_GetNumVideoDisplays;
      if gTotalDisplays < 2 then begin
        WriteLn('Warning: Only one display connected!');
      end else WriteLn('Num Display: ', gTotalDisplays);

      SetLength(gDisplayBounds, gTotalDisplays);
      for i := 0 to Length(gDisplayBounds) - 1 do begin
        SDL_GetDisplayBounds(i, @gDisplayBounds[i]);
      end;

      gWindow[0] := TLWindow.Create(Screen_Widht, Screen_Height);
    end;
    Result := sucess;
  end;

  procedure Close;
  var
    i: integer;
  begin
    for i := 0 to Length(gWindow) - 1 do begin
      gWindow[i].Free;
    end;
    SDL_Quit;
  end;

begin
  if not init then begin
    WriteLn('Failed to initialize');
  end else begin
    for i := 1 to Length(gWindow) - 1 do begin
      gWindow[i] := TLWindow.Create(Screen_Widht, Screen_Height);
    end;

    while not quit do begin
      while SDL_PollEvent(@e) <> 0 do begin
        case e.type_ of
          SDL_KEYDOWN: begin
            case e.key.keysym.sym of
              SDLK_ESCAPE: begin
                quit := True;
              end;
              SDLK_1: begin
                gWindow[0].focus;
              end;
              SDLK_2: begin
                gWindow[1].focus;
              end;
              SDLK_3: begin
                gWindow[2].focus;
              end;
            end;
          end;
          SDL_QUITEV: begin
            quit := True;
          end;
        end;
        for i := 0 to Length(gWindow) - 1 do begin
          gWindow[i].handleEvent(e);
        end;
      end;

      for i := 0 to Length(gWindow) - 1 do begin
        with gWindow[i] do begin
          r.x := 10;
          r.y := 10;
          r.w := Width - 20;
          r.h := Height - 20;
          SDL_SetRenderDrawColor(Renderer, $FF, $F shl (i * 3), $FF, $FF);
          SDL_RenderFillRect(Renderer, @r);

          SDL_RenderPresent(Renderer);
        end;
      end;

      allWindowsClosed := True;
      for i := 1 to Length(gWindow) - 1 do begin
        if gWindow[i].Shown then begin
          allWindowsClosed := False;
        end;
      end;

      if allWindowsClosed then begin
        quit := True;
      end;
    end;
  end;
  Close;
end.
