program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

uses
  sdl2,
  sdl2_image,
  ctypes;

const
  Screen_Widht = 640;
  Screen_Height = 480;
var
  gWindow: PSDL_Window;
  gRenderer: PSDL_Renderer;

  quit: boolean = False;
  e: TSDL_Event;


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
      gWindow := SDL_CreateWindow('SDL Tutorial', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, Screen_Widht, Screen_Height, SDL_WINDOW_SHOWN);
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

  procedure Close;
  begin
    SDL_DestroyRenderer(gRenderer);
    SDL_DestroyWindow(gWindow);
    gWindow := nil;
    gRenderer := nil;

    IMG_Quit;
    SDL_Quit;
  end;

const
  fillRect: TSDL_Rect = (x: Screen_Widht div 4; y: Screen_Height div 4; w: Screen_Widht div 2; h: Screen_Height div 2);
  outlineRect: TSDL_Rect = (x: Screen_Widht div 6; y: Screen_Height div 6; w: Screen_Widht * 2 div 3; h: Screen_Height * 2 div 3);
var
  i: integer;

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
        end;
      end;

      SDL_SetRenderDrawColor(gRenderer, $FF, $FF, $FF, $FF);
      SDL_RenderClear(gRenderer);

      SDL_SetRenderDrawColor(gRenderer, $FF, $00, $00, $FF);
      SDL_RenderFillRect(gRenderer, @fillRect);

      SDL_SetRenderDrawColor(gRenderer, $00, $FF, $00, $FF);
      SDL_RenderDrawRect(gRenderer, @outlineRect);

      SDL_SetRenderDrawColor(gRenderer, $00, $00, $FF, $FF);
      SDL_RenderDrawLine(gRenderer, 0, Screen_Height div 2, Screen_Widht, Screen_Height div 2);

      SDL_SetRenderDrawColor(gRenderer, $FF, $FF, $00, $FF);
      i := 0;
      while i < Screen_Height do begin
        SDL_RenderDrawPoint(gRenderer, Screen_Widht div 2, i);
        Inc(i, 4);
      end;

      SDL_RenderPresent(gRenderer);
    end;
  end;
  Close;
end.
