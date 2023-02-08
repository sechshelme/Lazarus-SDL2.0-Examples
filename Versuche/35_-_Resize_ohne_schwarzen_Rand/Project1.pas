program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

uses
  sdl2,
  sdl2_image,
  ctypes;

const
  Screen_Widht = 640;
  Screen_Height = 480;
  Screen_FPS = 60;
  Screen_Tick_Per_Frame = 1000 div Screen_FPS;

var
  gWindow: PSDL_Window;
  gRenderer: PSDL_Renderer;

  quit: boolean = False;
  e: TSDL_Event;
  FWidth: integer = Screen_Widht;
  FHeight: integer = Screen_Height;
  rect: TSDL_Rect;

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
      if not SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, '1') then begin
        WriteLn('Warning: Linear texture filtering not enabled!');
      end;
      gWindow := SDL_CreateWindow('SDL Tutorial', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, Screen_Widht, Screen_Height, SDL_WINDOW_SHOWN or SDL_WINDOW_RESIZABLE);
      if gWindow = nil then begin
        WriteLn('Window could not be created! SDL Error: ', SDL_GetError);
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

  procedure Close;
  begin
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
          SDL_WINDOWEVENT: begin
            case e.window.event of
              SDL_WINDOWEVENT_SIZE_CHANGED: begin
                FWidth := e.window.data1;
                FHeight := e.window.data2;
              end;
            end;
          end;
        end;
      end;

//      SDL_SetRenderDrawColor(gRenderer, $00, $9F, $00, $FF);
      SDL_SetRenderDrawColor(gRenderer, $FF, $FF, $FF, $FF);
      SDL_RenderClear(gRenderer);

      rect.x := 100;
      rect.y := 100;
      rect.w := FWidth - 200;
      rect.h := FHeight - 200;
      SDL_SetRenderDrawColor(gRenderer, $80, $80, $00, $FF);
      SDL_RenderFillRect(gRenderer, @rect);

      SDL_RenderPresent(gRenderer);
    end;
  end;
  Close;
end.
