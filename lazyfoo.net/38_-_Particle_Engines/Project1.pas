program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

uses
  sdl2,
  sdl2_image,
  ctypes,
  LTexture,
  LTimer,
  LDot, LParticle;

const
  Screen_Widht = 640;
  Screen_Height = 480;
  Screen_FPS = 300;
  Screen_Tick_Per_Frame = 1000 div Screen_FPS;

var
  gWindow: PSDL_Window;
  gRenderer: PSDL_Renderer;

  quit: boolean = False;
  e: TSDL_Event;

  gDotTexture: TLTexture;
  fpsTimer: TLTimer;
  myDot: TLDot;

  frameTicks: uint32;

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

    gDotTexture := TLTexture.Create(gRenderer);
    fpsTimer := TLTimer.Create;
    myDot := TLDot.Create(gRenderer,Screen_Widht, Screen_Height);
  end;

  procedure Close;
  begin
    gDotTexture.Free;
    fpsTimer.Free;
    myDot.Free;

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
//      fpsTimer.start;
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
          myDot.HandleEvent(e);
        end;

        myDot.move;

        SDL_SetRenderDrawColor(gRenderer, $00, $9F, $00, $FF);
        SDL_RenderClear(gRenderer);

        myDot.render;

        SDL_RenderPresent(gRenderer);

        frameTicks := fpsTimer.getTicks;
        if frameTicks < Screen_Tick_Per_Frame then begin
          SDL_Delay(Screen_Tick_Per_Frame - frameTicks);
        end;
      end;
  end;
  Close;
end.
